(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** IMAP queries and response data *)

type extension_data = ..

(** {2 Message flags} *)
                
type flag =
    FLAG_ANSWERED
  (** [\Answered] flag *)
  | FLAG_FLAGGED
  (** [\Flagged] flag *)
  | FLAG_DELETED
  (** [\Deleted] flag *)
  | FLAG_SEEN
  (** [\Seen] flag *)
  | FLAG_DRAFT
  (** [\Draft] flag *)
  | FLAG_KEYWORD of string
  (** keyword flag *)
  | FLAG_EXTENSION of string
  (* (\** \extension flag *\) ] with sexp *)

type flag_fetch =
    FLAG_FETCH_RECENT
  | FLAG_FETCH_OTHER of flag

(** Flags returned with a PERMANENTFLAG response code *)
type flag_perm =
    FLAG_PERM_FLAG of flag
  | FLAG_PERM_ALL

(** {2 SEARCH command} *)

type day_month_year =
  int * int * int

(** Search keys *)
type search_key =
    SEARCH_KEY_ALL
  (** All messages in the mailbox. *)
  | SEARCH_KEY_ANSWERED
  (** Messages with the [\Answered] flag set. *)
  | SEARCH_KEY_BCC of string
  (** Messages that contain the specified string in the envelope's [BCC] field. *)
  | SEARCH_KEY_BEFORE of day_month_year
  (** Messages whose internal date (disregarding time and timezone) is earlier
      than the specified date. *)
  | SEARCH_KEY_BODY of string
  (** Messages that contain the specified string in the body of the message. *)
  | SEARCH_KEY_CC of string
  (** Messages that contain the specified string in the envelope's [CC] field. *)
  | SEARCH_KEY_DELETED
  (** Messages with the [\Deleted] flag set. *)
  | SEARCH_KEY_FLAGGED
  (** Messages with the [\Flagged] flag set. *)
  | SEARCH_KEY_FROM of string
  (** Messages that contain the specified string in the envelope's [FROM]
      field. *)
  | SEARCH_KEY_KEYWORD of string
  (** Messages with the specified keyword flag set. *)
  | SEARCH_KEY_NEW
  (** Messages that have the \Recent flag set but not the \Seen flag.  This is
      functionally equivalent to [`AND (`RECENT, `UNSEEN)]. *)
  | SEARCH_KEY_OLD
  (** Messages that do not have the [\Recent] flag set.  This is functionally
      equivalent to [`NOT `RECENT] (as opposed to [`NOT `NEW]). *)
  | SEARCH_KEY_ON of day_month_year
  (** Messages whose internal date (disregarding time and timezone) is within
      the specified date. *)
  | SEARCH_KEY_RECENT
  (** Messages that have the [\Recent] flag set. *)
  | SEARCH_KEY_SEEN
  (** Messages that have the [\Seen] flag set. *)
  | SEARCH_KEY_SINCE of day_month_year
  (** Messages whose internal date (disregarding time and timezone) is within or
      later than the specified date. *)
  | SEARCH_KEY_SUBJECT of string
  (** Messages that contain the specified string in the envelope's [SUBJECT]
      field. *)
  | SEARCH_KEY_TEXT of string
  (** Messages that contain the specified string in the header or body of the
     message. *)
  | SEARCH_KEY_TO of string
  (** Messages that contain the specified string in the envelope's [TO]
      field. *)
  | SEARCH_KEY_UNANSWERED
  (** Messages that do not have the [\Answered] flag set. *)
  | SEARCH_KEY_UNDELETED
  (** Messages that do not have the [\Deleted] flag set. *)
  | SEARCH_KEY_UNFLAGGED
  (** Messages that do not have the [\Flagged] flag set. *)
  | SEARCH_KEY_UNKEYWORD of string
  (** Messages that do not have the specified keyword flag set. *)
  | SEARCH_KEY_UNSEEN
  (** Messages that do not have the [\Seen] flag set. *)
  | SEARCH_KEY_DRAFT
  (** Messages with the [\Draft] flag set. *)
  | SEARCH_KEY_HEADER of string * string
  (** Messages that have a header with the specified field-name (as defined in
      [RFC-2822]) and that contains the specified string in the text of the header
      (what comes after the colon). *)
  | SEARCH_KEY_LARGER of int
  (** Messages with an [RFC-2822] size larger than the specified number of
      bytes. *)
  | SEARCH_KEY_NOT of search_key
  (** Messages that do not match the specified search key. *)
  | SEARCH_KEY_OR of search_key * search_key
  (** Messages that match either search key. *)
  | SEARCH_KEY_SENTBEFORE of day_month_year
  (** Messages whose [Date:] header (disregarding time and timezone) is earlier
      than the specified date. *)
  | SEARCH_KEY_SENTON of day_month_year
  (** Messages whose [Date:] header (disregarding time and timezone) is within
      the specified date. *)
  | SEARCH_KEY_SENTSINCE of day_month_year
  (** Messages whose [Date:] header (disregarding time and timezone) is within
      or later than the specified date. *)
  | SEARCH_KEY_SMALLER of int
  (** Messages with an [RFC-2822] size smaller than the specified number of
      bytes. *)
  | SEARCH_KEY_UID of ImapSet.Uint32.t
  (** Messages with unique identifiers corresponding to the specified unique
      identifier set. *)
  | SEARCH_KEY_UNDRAFT
  (** Messages that do not have the [\Draft] flag set. *)
  | SEARCH_KEY_INSET of ImapSet.Uint32.t
  (** Messages with message sequence numbers corresponding to the specified
      message sequence number set. *)
  | SEARCH_KEY_AND of search_key * search_key
  (** Messages that match both search keys. *)
  | SEARCH_KEY_MODSEQ of (flag * [`Shared | `Priv | `All]) option * Uint64.t
  | SEARCH_KEY_XGMRAW of string
  (** Messages that satisfy Gmail search expression. *)
  | SEARCH_KEY_XGMMSGID of Uint64.t
  (** Message with given Gmail Message ID. *)
  | SEARCH_KEY_XGMTHRID of Uint64.t
  (** Messages with given Gmail Thread ID. *)
  (* | `X_GM_LABELS of string *)
  (* (\** Messages with given Gmail labels. *\) ] *)

(** {2 FETCH command} *)

(** {3 Queries} *)

type section_msgtext =
    SECTION_MSGTEXT_HEADER
  | SECTION_MSGTEXT_HEADER_FIELDS of string list
  | SECTION_MSGTEXT_HEADER_FIELDS_NOT of string list
  | SECTION_MSGTEXT_TEXT

type section_part =
  int list

type section_text =
    SECTION_TEXT_MSGTEXT of section_msgtext
  | SECTION_TEXT_MIME

type section_spec =
    SECTION_SPEC_SECTION_MSGTEXT of section_msgtext
  | SECTION_SPEC_SECTION_PART of section_part * section_text option

type section =
  section_spec option

(** FETCH queries *)
type fetch_att =
    FETCH_ATT_ENVELOPE
  (** MIME envelope information *)
  | FETCH_ATT_INTERNALDATE
  (** The message date kept by the server *)
  | FETCH_ATT_RFC822_HEADER
  (** The message header *)
  | FETCH_ATT_RFC822_TEXT
  (** The message text part *)
  | FETCH_ATT_RFC822_SIZE
  (** The size of the message content *)
  | FETCH_ATT_RFC822
  (** The message content (header and body) *)
  | FETCH_ATT_BODY
  (** The MIME description of the message *)
  | FETCH_ATT_BODY_SECTION of section * (int * int) option
  (** A MIME part content *)
  | FETCH_ATT_BODY_PEEK_SECTION of section * (int * int) option
  (** Like [`BODYSECTION], but does not set the [`Seen] flag. *)
  | FETCH_ATT_BODYSTRUCTURE
  (** The MIME description of the message with additional information *)
  | FETCH_ATT_UID
  (** Unique identification number. *)
  | FETCH_ATT_FLAGS
  (** Message flags *)
  (* | `MODSEQ *)
  (* (\** Modification sequence number.  This requires support for the CONDSTORE *)
  (*     extension. *\) *)
  (* | `X_GM_MSGID *)
  (* (\** Gmail message ID. *\) *)
  (* | `X_GM_THRID *)
  (* (\** Gmail thread ID. *\) *)
  (* | `X_GM_LABELS *)
(* (\** Gmail labels *\) ] with sexp *)

type fetch_type =
    FETCH_TYPE_ALL
  | FETCH_TYPE_FULL
  | FETCH_TYPE_FAST
  | FETCH_TYPE_FETCH_ATT of fetch_att
  | FETCH_TYPE_FETCH_ATT_LIST of fetch_att list

(** {MIME} *)

type address = {
  ad_personal_name : string;
  ad_source_route : string;
  ad_mailbox_name : string;
  ad_host_name : string
}

type envelope = {
  env_date : string;
  env_subject : string;
  env_from : address list;
  env_sender : address list;
  env_reply_to : address list;
  env_to : address list;
  env_cc : address list;
  env_bcc : address list;
  env_in_reply_to : string;
  env_message_id : string
}

type body_fld_param =
  (string * string) list

type body_fld_dsp = {
  dsp_type : string;
  dsp_attributes : body_fld_param
}

type body_fld_lang =
    BODY_FLD_LANG_SINGLE of string option
  | BODY_FLD_LANG_LIST of string list

type media_basic_type =
    MEDIA_BASIC_APPLICATION
  | MEDIA_BASIC_AUDIO
  | MEDIA_BASIC_IMAGE
  | MEDIA_BASIC_MESSAGE
  | MEDIA_BASIC_VIDEO
  | MEDIA_BASIC_OTHER of string

type media_basic = {
  med_basic_type : media_basic_type;
  med_basic_subtype : string
}

type body_fld_enc =
    BODY_FLD_ENC_7BIT
  | BODY_FLD_ENC_8BIT
  | BODY_FLD_ENC_BINARY
  | BODY_FLD_ENC_BASE64
  | BODY_FLD_ENC_QUOTED_PRINTABLE
  | BODY_FLD_ENC_OTHER of string

type body_extension =
    BODY_EXTENSION_LIST of body_extension list
  | BODY_EXTENSION_NUMBER of Uint32.t
  | BODY_EXTENSION_NSTRING of string option

and body_fields = {
  bd_parameter : body_fld_param;
  bd_id : string option;
  bd_description : string option;
  bd_encoding : body_fld_enc;
  bd_size : int
}

and body_type_msg = {
  bd_fields : body_fields;
  bd_envelope : envelope;
  bd_body : body
}

and body_type_text = {
  bd_media_text : string;
  bd_fields : body_fields;
  bd_lines : int
}

and body_type_basic = {
  bd_media_basic : media_basic;
  bd_fields : body_fields
}

and body_type_1part_data =
    BODY_TYPE_1PART_BASIC of body_type_basic
  | BODY_TYPE_1PART_MSG of body_type_msg
  | BODY_TYPE_1PART_TEXT of body_type_text

and body_ext_mpart = {
  bd_parameter : body_fld_param;
  bd_disposition : body_fld_dsp option;
  bd_language : body_fld_lang option;
  bd_loc : string option;
  bd_extension_list : body_extension list
}

and body_ext_1part = {
  bd_md5 : string option;
  bd_disposition : body_fld_dsp option;
  bd_language : body_fld_lang option;
  bd_loc : string option;
  bd_extension_list : body_extension list
}

and body_type_1part = {
  bd_data : body_type_1part_data;
  bd_ext_1part : body_ext_1part
}

and body_type_mpart = {
  bd_list : body list;
  bd_media_subtype : string;
  bd_ext_mpart : body_ext_mpart
}

and body =
    BODY_1PART of body_type_1part
  | BODY_MPART of body_type_mpart

(** {3 Responses} *)
  
type date_time = {
  dt_day : int;
  dt_month : int;
  dt_year : int;
  dt_hour : int;
  dt_min : int;
  dt_sec : int;
  dt_zone : int
}

type msg_att_body_section = {
  sec_section : section;
  sec_origin_octet : int;
  sec_body_part : string;
  sec_length : int
}

type msg_att_static =
    MSG_ATT_ENVELOPE of envelope
  | MSG_ATT_INTERNALDATE of date_time
  | MSG_ATT_RFC822 of string
  | MSG_ATT_RFC822_HEADER of string
  | MSG_ATT_RFC822_TEXT of string
  | MSG_ATT_RFC822_SIZE of int
  | MSG_ATT_BODY of body
  | MSG_ATT_BODYSTRUCTURE of body
  | MSG_ATT_BODY_SECTION of msg_att_body_section
  | MSG_ATT_UID of Uint32.t
  (* | MSG_ATT_X_GM_MSGID of Gmsgid.t *)
  (* | MSG_ATT_X_GM_THRID of Gthrid.t *)

type msg_att_dynamic =
  flag_fetch list
  (* [ `FLAGS of flag list *)
  (* | `MODSEQ of Modseq.t *)
  (* | `X_GM_LABELS of string list ] with sexp *)
  
type msg_att_item =
    MSG_ATT_ITEM_DYNAMIC of msg_att_dynamic
  | MSG_ATT_ITEM_STATIC of msg_att_static
  | MSG_ATT_ITEM_EXTENSION of extension_data

type msg_att =
  msg_att_item list * Uint32.t

(** {2 STORE command} *)

type store_att_flags_sign =
    STORE_ATT_FLAGS_SET
  | STORE_ATT_FLAGS_ADD
  | STORE_ATT_FLAGS_REMOVE

type store_att_flags = {
  fl_sign : store_att_flags_sign;
  fl_silent : bool;
  fl_flag_list : flag list
}

(* type store_att = *)
(*   [ `FLAGS of flag list *)
(*   (\** Message flags *\) *)
(*   | `FLAGS_SILENT of flag list *)
(*   (\** Silent message flags (server will not respond with the updated list of flags). *\) *)
(*   | `X_GM_LABELS of string list *)
(*   (\** Gmail labels *\) *)
(*   | `X_GM_LABELS_SILENT of string list *)
(*   (\** Silent Gmail labels (server will not respond with the updated list of labels). *\) ] with sexp *)

(** {2 STATUS command} *)

(** STATUS queries: the different mailbox status attributes that can be fetched
    via {!Imap.status}. *)
type status_att =
    STATUS_ATT_MESSAGES
  (** Number of messages in the mailbox. *)
  | STATUS_ATT_RECENT
  (** Number of new messages in the mailbox. *)
  | STATUS_ATT_UIDNEXT
  (** The probable unique identification number of the next message to arrive. *)
  | STATUS_ATT_UIDVALIDITY
  (** The UID validity value of the mailbox. *)
  | STATUS_ATT_UNSEEN
  (** The number of unseen messages in the mailbox. *)
  | STATUS_ATT_HIGHESTMODSEQ
  (** The highest modification sequence of the mailbox.  This requires support
      for the CONDSTORE extension. *)

(** STATUS replies. See {!status_att}. *)
type status_info =
    STATUS_ATT_MESSAGES of int
  | STATUS_ATT_RECENT of int
  | STATUS_ATT_UIDNEXT of Uint32.t
  | STATUS_ATT_UIDVALIDITY of Uint32.t
  | STATUS_ATT_UNSEEN of int
  | STATUS_ATT_HIGHESTMODSEQ of Uint64.t
  | STATUS_ATT_EXTENSION of extension_data

(** {2 LIST/LSUB commands} *)

type mailbox_data_status = {
  st_mailbox : string;
  st_info_list : status_info list
}

(** The type of mailbox single flags *)
type mbx_list_sflag =
    MBX_LIST_SFLAG_NOSELECT
  | MBX_LIST_SFLAG_MARKED
  | MBX_LIST_SFLAG_UNMARKED

(** The type of mailbox other flags *)
type mbx_list_oflag =
    MBX_LIST_OFLAG_NOINFERIORS
  | MBX_LIST_OFLAG_EXT of string

(** Mailbox flag *)
type mbx_list_flags = {
  mbf_sflag : mbx_list_sflag option;
  (** Mailbox single flag *)
  mbf_oflags : mbx_list_oflag list
  (** List of "mailbox other flag" *)
}

(** List of mailbox flags *)
type mailbox_list = {
  mb_flag : mbx_list_flags;
  (** List of mailbox flags *)
  mb_delimiter : char;
  (** Delimiter of the mailbox path, '\000' if not present *)
  mb_name : string
  (** Name of the mailbox *)
}

(** {2 CAPABILITY command} *)

type capability =
    CAPABILITY_AUTH_TYPE of string
  | CAPABILITY_NAME of string

(** List of capabilities *)
type capability_data =
  capability list

(** {2 Response codes} *)
  
type resp_text_code =
    RESP_TEXT_CODE_ALERT
  | RESP_TEXT_CODE_BADCHARSET of string list
  | RESP_TEXT_CODE_CAPABILITY_DATA of capability_data
  | RESP_TEXT_CODE_PARSE
  | RESP_TEXT_CODE_PERMANENTFLAGS of flag_perm list
  | RESP_TEXT_CODE_READ_ONLY
  | RESP_TEXT_CODE_READ_WRITE
  | RESP_TEXT_CODE_TRYCREATE
  | RESP_TEXT_CODE_UIDNEXT of Uint32.t
  | RESP_TEXT_CODE_UIDVALIDITY of Uint32.t
  | RESP_TEXT_CODE_UNSEEN of Uint32.t
  (* | RESP_TEXT_CODE_APPENDUID of Uid.t * Uid.t *)
  (* | RESP_TEXT_CODE_COPYUID of Uid.t * Uid_set.t * Uid_set.t *)
  (* | RESP_TEXT_CODE_UIDNOTSTICKY *)
  (* | RESP_TEXT_CODE_COMPRESSIONACTIVE *)
  (* | RESP_TEXT_CODE_HIGHESTMODSEQ of Modseq.t *)
  (* | RESP_TEXT_CODE_NOMODSEQ *)
  (* | RESP_TEXT_CODE_MODIFIED of Uint32_set.t *)
  | RESP_TEXT_CODE_EXTENSION of extension_data
  | RESP_TEXT_CODE_OTHER of (string * string)
  | RESP_TEXT_CODE_NONE
  
(** response code, human readable text *)
type resp_text = {
  rsp_code : resp_text_code;
  rsp_text : string
}

(** {2 Untagged responses} *)

type 'resp_type resp_cond = {
  rsp_type : 'resp_type;
  rsp_text : resp_text
}

(** Authentication condition responses *)
type resp_cond_auth_type =
    RESP_COND_AUTH_OK
  | RESP_COND_AUTH_PREAUTH

type resp_cond_auth =
  resp_cond_auth_type resp_cond

(** BYE response *)
type resp_cond_bye =
  resp_text

type response_fatal =
  resp_cond_bye

(** Condition state responses *)
type resp_cond_state_type =
    RESP_COND_STATE_OK
  | RESP_COND_STATE_NO
  | RESP_COND_STATE_BAD

type resp_cond_state =
  resp_cond_state_type resp_cond

(** Message information *)
type message_data =
    MESSAGE_DATA_EXPUNGE of Uint32.t
  | MESSAGE_DATA_FETCH of msg_att
                            
(** Mailbox information *)
type mailbox_data =
    MAILBOX_DATA_FLAGS of flag list
  | MAILBOX_DATA_LIST of mailbox_list
  | MAILBOX_DATA_LSUB of mailbox_list
  | MAILBOX_DATA_SEARCH of Uint32.t list
  | MAILBOX_DATA_STATUS of mailbox_data_status
  | MAILBOX_DATA_EXISTS of int
  | MAILBOX_DATA_RECENT of int
  | MAILBOX_DATA_EXTENSION_DATA of extension_data

(** Untagged response *)
type response_data =
    RESP_DATA_COND_STATE of resp_cond_state
  | RESP_DATA_COND_BYE of resp_cond_bye
  | RESP_DATA_MAILBOX_DATA of mailbox_data
  | RESP_DATA_MESSAGE_DATA of message_data
  | RESP_DATA_CAPABILITY_DATA of capability_data
  | RESP_DATA_EXTENSION_DATA of extension_data

(** {2 Tagged responses} *)
  
type response_tagged = {
  rsp_tag : string;
  rsp_cond_state : resp_cond_state
}

(** Ending response *)
type response_done =
    RESP_DONE_TAGGED of response_tagged
  | RESP_DONE_FATAL of response_fatal

type continue_req =
    CONTINUE_REQ_TEXT of resp_text
  | CONTINUE_REQ_BASE64 of string

type cont_req_or_resp_data =
    RESP_CONT_REQ of continue_req
  | RESP_CONT_DATA of response_data

type response = {
  rsp_cont_req_or_resp_data_list : cont_req_or_resp_data list;
  rsp_resp_done : response_done
}

(** {2 Greeting response} *)

type greeting =
    GREETING_RESP_COND_AUTH of resp_cond_auth
  | GREETING_RESP_COND_BYE of resp_cond_bye

type mailbox_perm =
    MAILBOX_READONLY
  | MAILBOX_READWRITE

type selection_info = {
  sel_perm_flags : flag_perm list;
  sel_perm : mailbox_perm;
  sel_uidnext : Uint32.t;
  sel_uidvalidity : Uint32.t;
  sel_first_unseen : Uint32.t;
  sel_flags : flag list;
  sel_exists : int option;
  sel_recent : int option;
  sel_unseen : int
}

type response_info = {
  rsp_alert : string;
  rsp_parse : string;
  rsp_badcharset : string list;
  rsp_trycreate : bool;
  rsp_mailbox_list : mailbox_list list;
  rsp_mailbox_lsub : mailbox_list list;
  rsp_search_results : Uint32.t list;
  rsp_status : mailbox_data_status;
  rsp_expunged : Uint32.t list;
  rsp_fetch_list : msg_att list;
  rsp_extension_list : extension_data list;
  rsp_other : string * string
}

type state = {
  rsp_info : response_info;
  sel_info : selection_info;
  cap_info : capability list;
  imap_response : string;
  current_tag : string option;
  next_tag : int
}

type 'a rope =
    Atom of 'a
  | Empty
  | Append of 'a rope * 'a rope

type send_atom =
    Raw of string
  | Cont_req

type sender =
  send_atom rope

type input =
    End
  | More

type 'a result =
    Ok of 'a * int
  | Fail of int
  | Need of int * (input -> 'a result)

type 'a parser =
  Buffer.t -> int -> 'a result  

type 'a command = {
  cmd_sender : sender;
  cmd_parser : response parser;
  cmd_handler : state -> 'a
}

type extended_parser =
    EXTENDED_PARSER_RESPONSE_DATA
  | EXTENDED_PARSER_RESP_TEXT_CODE
  | EXTENDED_PARSER_MAILBOX_DATA
  | EXTENDED_PARSER_FETCH_DATA
  | EXTENDED_PARSER_STATUS_ATT

type extension = {
  ext_parser : extended_parser -> extension_data parser;
  ext_printer : (extension_data -> (Format.formatter -> unit) option)
}
