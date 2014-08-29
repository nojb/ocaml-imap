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

val fresh_selection_info : selection_info
  
val fresh_response_info : response_info

val greetings_store : state -> ImapResponse.greeting -> state
  
val resp_data_or_resp_done_store : state -> [ImapResponse.response_data | ImapResponse.response_done] -> state

val cont_req_or_resp_data_or_resp_done_store : state -> ImapResponse.cont_req_or_resp_data_or_resp_done -> state

val has_uidplus : state -> bool
(** Whether the IMAP server supports the UIDPLUS extension. *)

val has_compress_deflate : state -> bool
(** Whether the IMAP server supports the COMPRESS=DEFLATE extension. *)

val has_id : state -> bool
(** Whether the IMAP server supports the ID extension. *)

val has_condstore : state -> bool
(** Whether the IMAP server supports the CONDSTORE extension. *)

val has_x_gm_ext_1 : state -> bool
(** Whether the IMAP server supports the X-GM-EXT-1 extension (probably only
    Google supports this). *)

val has_namespace : state -> bool
(** Whether the IMAP server supports the NAMESPACE extension. *)

val has_enable : state -> bool
(** Whether the IMAP server supports the ENABLE extension. *)

val last_response : state -> string
(** The descriptive text of the last tagged response (or the last BYE
    (untagged) response from the server. *)

