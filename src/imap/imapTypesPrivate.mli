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

open ImapTypes

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
  (* | RESP_TEXT_CODE_COMPRESSIONACTIVE *)
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

type 'a parse_result =
    Ok of 'a * int
  | Fail of int
  | Need of (input -> 'a parse_result)

type 'a parser =
  Buffer.t -> int -> 'a parse_result
