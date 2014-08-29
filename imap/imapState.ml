open ImapTypes
open ImapUint

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
  rsp_fetch_list : (Seq.t * msg_att list) list;
  rsp_appenduid : Uid.t * Uid.t;
  rsp_copyuid : Uid.t * Uid_set.t * Uid_set.t;
  rsp_compressionactive : bool;
  rsp_id : (string * string) list;
  rsp_modified : Uint32_set.t;
  rsp_namespace : namespace list * namespace list * namespace list;
  rsp_enabled : capability list;
  rsp_other : string * string
}

type state = {
  imap_response : string;
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
  rsp_namespace = ([], [], []);
  rsp_enabled = [];
  rsp_other = ("", "")
}

let fresh_selection_info = {
  sel_perm_flags = [];
  sel_perm = `READ_ONLY;
  sel_uidnext = Uid.zero;
  sel_uidvalidity = Uid.zero;
  sel_first_unseen = Seq.zero;
  sel_flags = [];
  sel_exists = None;
  sel_recent = None;
  sel_uidnotsticky = false;
  sel_highestmodseq = Modseq.zero
}

let resp_text_store s (c, text) =
  match c with
  | `ALERT ->
    {s with rsp_info = {s.rsp_info with rsp_alert = text}}
  | `BADCHARSET csets ->
    {s with rsp_info = {s.rsp_info with rsp_badcharset = csets}}
  | `CAPABILITY caps ->
    {s with cap_info = caps}
  | `PARSE ->
    {s with rsp_info = {s.rsp_info with rsp_parse = text}}
  | `PERMANENTFLAGS flags ->
    {s with sel_info = {s.sel_info with sel_perm_flags = flags}}
  | `READ_ONLY ->
    {s with sel_info = {s.sel_info with sel_perm = `READ_ONLY}}
  | `READ_WRITE ->
    {s with sel_info = {s.sel_info with sel_perm = `READ_WRITE}}
  | `TRYCREATE ->
    {s with rsp_info = {s.rsp_info with rsp_trycreate = true}}
  | `UIDNEXT uid ->
    {s with sel_info = {s.sel_info with sel_uidnext = uid}}
  | `UIDVALIDITY uid ->
    {s with sel_info = {s.sel_info with sel_uidvalidity = uid}}
  | `UNSEEN unseen ->
    {s with sel_info = {s.sel_info with sel_first_unseen = unseen}}
  | `APPENDUID (uidvalidity, uid) ->
    {s with rsp_info = {s.rsp_info with rsp_appenduid = (uidvalidity, uid)}}
  | `COPYUID (uidvalidity, src_uids, dst_uids) ->
    {s with rsp_info = {s.rsp_info with rsp_copyuid = (uidvalidity, src_uids, dst_uids)}}
  | `UIDNOTSTICKY ->
    {s with sel_info = {s.sel_info with sel_uidnotsticky = true}}
  | `COMPRESSIONACTIVE ->
    {s with rsp_info = {s.rsp_info with rsp_compressionactive = true}}
  | `HIGHESTMODSEQ modseq ->
    {s with sel_info = {s.sel_info with sel_highestmodseq = modseq}}
  | `NOMODSEQ ->
    {s with sel_info = {s.sel_info with sel_highestmodseq = Modseq.zero}}
  | `MODIFIED set ->
    {s with rsp_info = {s.rsp_info with rsp_modified = set}}
  | `OTHER other ->
    {s with rsp_info = {s.rsp_info with rsp_other = other}}
  | `NONE ->
    s

let mailbox_data_store s = function
  | `FLAGS flags ->
    {s with sel_info = {s.sel_info with sel_flags = flags}}
  | `LIST mb ->
    {s with rsp_info = {s.rsp_info with
                        rsp_mailbox_list = s.rsp_info.rsp_mailbox_list @ [mb]}}
  | `LSUB mb ->
    {s with rsp_info = {s.rsp_info with
                        rsp_mailbox_list = s.rsp_info.rsp_mailbox_list @ [mb]}}
  (* rsp_mailbox_lsub = s.rsp_info.rsp_mailbox_lsub @ [mb] } *)
  | `SEARCH (results, modseq) ->
    {s with rsp_info = {s.rsp_info with
                        rsp_search_results = s.rsp_info.rsp_search_results @ results;
                        rsp_search_results_modseq =
                          Modseq.max modseq s.rsp_info.rsp_search_results_modseq}}
  | `STATUS status ->
    {s with rsp_info = {s.rsp_info with rsp_status = status}}
  | `EXISTS n ->
    {s with sel_info = {s.sel_info with sel_exists = Some n}}
  | `RECENT n ->
    {s with sel_info = {s.sel_info with sel_recent = Some n}}

let message_data_store s = function
  | `EXPUNGE n ->
    let s =
      {s with rsp_info = {s.rsp_info with rsp_expunged = s.rsp_info.rsp_expunged @ [n]}}
    in
    begin match s.sel_info.sel_exists with
      | Some n ->
        {s with sel_info = {s.sel_info with sel_exists = Some (n-1)}}
      | None ->
        s
    end
  | `FETCH att ->
    {s with rsp_info = {s.rsp_info with rsp_fetch_list = s.rsp_info.rsp_fetch_list @ [att]}}

let resp_cond_state_store s = function
  | `OK rt | `NO rt | `BAD rt ->
    resp_text_store s rt

let resp_cond_bye_store s = function
  | `BYE rt ->
    resp_text_store s rt

let response_data_store s = function
  | #ImapResponse.resp_cond_state as resp ->
    resp_cond_state_store s resp
  | #ImapResponse.resp_cond_bye as resp ->
    resp_cond_bye_store s resp
  | #ImapResponse.mailbox_data as resp ->
    mailbox_data_store s resp
  | #ImapResponse.message_data as resp ->
    message_data_store s resp
  | `CAPABILITY caps ->
    {s with cap_info = caps}
  | `ID params ->
    {s with rsp_info = {s.rsp_info with rsp_id = params}}
  | `NAMESPACE (pers, other, shared) ->
    {s with rsp_info = {s.rsp_info with rsp_namespace = pers, other, shared}}
  | `ENABLED caps ->
    {s with rsp_info = {s.rsp_info with rsp_enabled = caps}}

let response_tagged_store s (_, rcs) =
  resp_cond_state_store s rcs

let response_fatal_store s r =
  resp_cond_bye_store s r

let text_of_response_done = function
  | `TAGGED (_, `OK (_, txt))
  | `TAGGED (_, `BAD (_, txt))
  | `TAGGED (_, `NO (_, txt))
  | `BYE (_, txt) ->
    txt

let response_done_store s resp =
  let s = {s with imap_response = text_of_response_done resp} in
  match resp with
  | `TAGGED tagged ->
    response_tagged_store s tagged
  | #ImapResponse.response_fatal as resp ->
    response_fatal_store s resp

let resp_data_or_resp_done_store s =
  function
  | #ImapResponse.response_data as resp ->
    response_data_store s resp
  | #ImapResponse.response_done as resp ->
    response_done_store s resp

let resp_cond_auth_store s = function
  | `OK rt | `PREAUTH rt ->
    resp_text_store s rt

let greetings_store s = function
  | #ImapResponse.resp_cond_auth as resp ->
    resp_cond_auth_store s resp
  | #ImapResponse.resp_cond_bye as resp ->
    resp_cond_bye_store s resp

let cont_req_or_resp_data_or_resp_done_store s = function
  | `CONT_REQ _ -> s
  | #ImapResponse.response_data | #ImapResponse.response_done as resp ->
    resp_data_or_resp_done_store s resp

let has_capability_name s name =
  List.exists (function
      | `NAME x -> ImapUtils.compare_ci x name = 0
      | `AUTH_TYPE _ -> false) s.cap_info

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

let last_response s =
  s.imap_response
