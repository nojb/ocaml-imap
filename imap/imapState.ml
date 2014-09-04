open ImapTypes

type selection_info = {
  sel_perm_flags : flag_perm list;
  sel_perm : [ `READ_ONLY | `READ_WRITE ];
  sel_uidnext : Uid.t;
  sel_uidvalidity : Uid.t;
  sel_first_unseen : Seq.t;
  sel_flags : flag list;
  sel_exists : int option;
  sel_recent : int option;
  sel_uidnotsticky : bool;
  sel_highestmodseq : Modseq.t
}

type response_info = {
  rsp_alert : string;
  rsp_parse : string;
  rsp_badcharset : string list;
  rsp_trycreate : bool;
  rsp_mailbox_list : mailbox_list list;
  (* rsp_mailbox_lsub : mailbox_list list; *)
  rsp_search_results : Uint32.t list;
  rsp_search_results_modseq : Modseq.t;
  rsp_status : mailbox_data_status;
  rsp_expunged : Seq.t list;
  rsp_fetch_list : msg_att list;
  rsp_appenduid : Uid.t * Uid.t;
  rsp_copyuid : Uid.t * Uid_set.t * Uid_set.t;
  rsp_compressionactive : bool;
  rsp_id : (string * string) list;
  rsp_modified : Uint32_set.t;
  (* rsp_namespace : namespace list * namespace list * namespace list; *)
  rsp_enabled : capability list;
  rsp_other : string * string
}

type state = {
  rsp_info : response_info;
  sel_info : selection_info;
  cap_info : capability list
}

let fresh_response_info = {
  rsp_alert = "";
  rsp_parse = "";
  rsp_badcharset = [];
  rsp_trycreate = false;
  rsp_mailbox_list = [];
  (* rsp_mailbox_lsub = []; *)
  rsp_search_results = [];
  rsp_search_results_modseq = Modseq.zero;
  rsp_status = {st_mailbox = ""; st_info_list = []};
  rsp_expunged = [];
  rsp_fetch_list = [];
  rsp_appenduid = (Uid.zero, Uid.zero);
  rsp_copyuid = (Uid.zero, Uid_set.empty, Uid_set.empty);
  rsp_compressionactive = false;
  rsp_id = [];
  rsp_modified = Uint32_set.empty;
  (* rsp_namespace = ([], [], []); *)
  rsp_enabled = [];
  rsp_other = ("", "")
}

let fresh_selection_info = {
  sel_perm_flags = [];
  sel_perm = `READ_ONLY; (* FIXME *)
  sel_uidnext = Uid.zero;
  sel_uidvalidity = Uid.zero;
  sel_first_unseen = Seq.zero;
  sel_flags = [];
  sel_exists = None;
  sel_recent = None;
  sel_uidnotsticky = false;
  sel_highestmodseq = Modseq.zero
}

let resp_text_store s {rsp_code; rsp_text} =
  match rsp_code with
    RESP_TEXT_CODE_ALERT ->
      {s with rsp_info = {s.rsp_info with rsp_alert = rsp_text}}
  | RESP_TEXT_CODE_BADCHARSET csets ->
      {s with rsp_info = {s.rsp_info with rsp_badcharset = csets}}
  | RESP_TEXT_CODE_CAPABILITY_DATA caps ->
      {s with cap_info = caps}
  | RESP_TEXT_CODE_PARSE ->
      {s with rsp_info = {s.rsp_info with rsp_parse = rsp_text}}
  | RESP_TEXT_CODE_PERMANENTFLAGS flags ->
      {s with sel_info = {s.sel_info with sel_perm_flags = flags}}
  | RESP_TEXT_CODE_READ_ONLY ->
      {s with sel_info = {s.sel_info with sel_perm = `READ_ONLY}}
  | RESP_TEXT_CODE_READ_WRITE ->
      {s with sel_info = {s.sel_info with sel_perm = `READ_WRITE}}
  | RESP_TEXT_CODE_TRYCREATE ->
      {s with rsp_info = {s.rsp_info with rsp_trycreate = true}}
  | RESP_TEXT_CODE_UIDNEXT uid ->
      {s with sel_info = {s.sel_info with sel_uidnext = uid}}
  | RESP_TEXT_CODE_UIDVALIDITY uid ->
      {s with sel_info = {s.sel_info with sel_uidvalidity = uid}}
  | RESP_TEXT_CODE_UNSEEN unseen ->
      {s with sel_info = {s.sel_info with sel_first_unseen = unseen}}
  | RESP_TEXT_CODE_APPENDUID (uidvalidity, uid) ->
      {s with rsp_info = {s.rsp_info with rsp_appenduid = (uidvalidity, uid)}}
  | RESP_TEXT_CODE_COPYUID (uidvalidity, src_uids, dst_uids) ->
      {s with rsp_info = {s.rsp_info with rsp_copyuid = (uidvalidity, src_uids, dst_uids)}}
  | RESP_TEXT_CODE_UIDNOTSTICKY ->
      {s with sel_info = {s.sel_info with sel_uidnotsticky = true}}
  | RESP_TEXT_CODE_COMPRESSIONACTIVE ->
      {s with rsp_info = {s.rsp_info with rsp_compressionactive = true}}
  | RESP_TEXT_CODE_HIGHESTMODSEQ modseq ->
      {s with sel_info = {s.sel_info with sel_highestmodseq = modseq}}
  | RESP_TEXT_CODE_NOMODSEQ ->
      {s with sel_info = {s.sel_info with sel_highestmodseq = Modseq.zero}}
  | RESP_TEXT_CODE_MODIFIED set ->
      {s with rsp_info = {s.rsp_info with rsp_modified = set}}
  | RESP_TEXT_CODE_OTHER other ->
      {s with rsp_info = {s.rsp_info with rsp_other = other}}
  | RESP_TEXT_CODE_NONE ->
      s

let mailbox_data_store s =
  function
    MAILBOX_DATA_FLAGS flags ->
      {s with sel_info = {s.sel_info with sel_flags = flags}}
  | MAILBOX_DATA_LIST mb ->
      {s with rsp_info =
                {s.rsp_info with rsp_mailbox_list =
                                   s.rsp_info.rsp_mailbox_list @ [mb]}}
  | MAILBOX_DATA_LSUB mb ->
      {s with rsp_info =
                {s.rsp_info with rsp_mailbox_list =
                                   s.rsp_info.rsp_mailbox_list @ [mb]}}
  (* rsp_mailbox_lsub = s.rsp_info.rsp_mailbox_lsub @ [mb] } *)
  | MAILBOX_DATA_SEARCH (results, modseq) ->
      {s with rsp_info = {s.rsp_info with
                          rsp_search_results = s.rsp_info.rsp_search_results @ results;
                          rsp_search_results_modseq =
                            let max x y = if Modseq.compare x y <= 0 then y else x in
                            max modseq s.rsp_info.rsp_search_results_modseq}}
  | MAILBOX_DATA_STATUS status ->
      {s with rsp_info = {s.rsp_info with rsp_status = status}}
  | MAILBOX_DATA_EXISTS n ->
      {s with sel_info = {s.sel_info with sel_exists = Some n}}
  | MAILBOX_DATA_RECENT n ->
      {s with sel_info = {s.sel_info with sel_recent = Some n}}

let message_data_store s =
  function
    MESSAGE_DATA_EXPUNGE n ->
      let s =
        {s with rsp_info = {s.rsp_info with rsp_expunged = s.rsp_info.rsp_expunged @ [n]}}
      in
      begin match s.sel_info.sel_exists with
        | Some n ->
            {s with sel_info = {s.sel_info with sel_exists = Some (n-1)}}
        | None ->
            s
      end
  | MESSAGE_DATA_FETCH att ->
      {s with rsp_info = {s.rsp_info with rsp_fetch_list = s.rsp_info.rsp_fetch_list @ [att]}}

let resp_cond_state_store s {rsp_text = r} =
  resp_text_store s r

let resp_cond_bye_store s r =
  resp_text_store s r

let response_data_store s =
  function
    RESP_DATA_COND_STATE r ->
      resp_cond_state_store s r
  | RESP_DATA_COND_BYE r ->
      resp_cond_bye_store s r
  | RESP_DATA_MAILBOX_DATA r ->
      mailbox_data_store s r
  | RESP_DATA_MESSAGE_DATA r ->
      message_data_store s r
  | RESP_DATA_CAPABILITY_DATA cap_info ->
      {s with cap_info}
  (* | `ID params -> *)
  (*   {s with rsp_info = {s.rsp_info with rsp_id = params}} *)
  (* | `NAMESPACE (pers, other, shared) -> *)
  (*   {s with rsp_info = {s.rsp_info with rsp_namespace = pers, other, shared}} *)
  (* | `ENABLED caps -> *)
  (*   {s with rsp_info = {s.rsp_info with rsp_enabled = caps}} *)

let response_tagged_store s {rsp_cond_state = r} =
  resp_cond_state_store s r

let response_fatal_store s r =
  resp_cond_bye_store s r

let resp_cond_auth_store s {rsp_text = r} =
  resp_text_store s r

let greeting_store s =
  function
    GREETING_RESP_COND_AUTH r ->
      resp_cond_auth_store s r
  | GREETING_RESP_COND_BYE r ->
      resp_cond_bye_store s r

let text_of_response_done =
  function
    RESP_DONE_TAGGED {rsp_cond_state = {rsp_text}}
  | RESP_DONE_FATAL rsp_text ->
    rsp_text

let response_done_store s =
  function
    RESP_DONE_TAGGED r ->
      response_tagged_store s r
  | RESP_DONE_FATAL r ->
      response_fatal_store s r

let cont_req_or_resp_data_store s =
  function
    RESP_CONT_REQ r ->
      s
  | RESP_CONT_DATA r ->
      response_data_store s r

let response_store s {rsp_cont_req_or_resp_data_list; rsp_resp_done} =
  response_done_store
    (List.fold_left cont_req_or_resp_data_store s rsp_cont_req_or_resp_data_list)
    rsp_resp_done

let has_capability_name s name =
  let name = String.uppercase name in
  List.exists
    (function CAPABILITY_NAME x -> String.uppercase x = name
            | CAPABILITY_AUTH_TYPE _ -> false)
    s.cap_info

let has_uidplus s =
  has_capability_name s "UIDPLUS"

let has_compress_deflate s =
  has_capability_name s "COMPRESS=DEFLATE"

let has_id s =
  has_capability_name s "ID"

let has_condstore s =
  has_capability_name s "CONDSTORE"

let has_x_gm_ext_1 s =
  has_capability_name s "X-GM-EXT-1"

let has_namespace s =
  has_capability_name s "NAMESPACE"

let has_enable s =
  has_capability_name s "ENABLE"
