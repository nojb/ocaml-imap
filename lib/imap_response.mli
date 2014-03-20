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

(** IMAP responses *)

open Sexplib.Std
open Imap_types
open Imap_uint

(** List of capabilities *)
type capability_data =
  [ `CAPABILITY of capability list ] with sexp

(** {2 Response codes} *)
  
type resp_text_code =
  [ `ALERT
  | `BADCHARSET of string list
  (** SEARCH response, list of character sets supported by the server *)
  | capability_data
  (** List of capabilities *)
  | `PARSE
  | `PERMANENTFLAGS of flag_perm list
  (** List of flags that can be changed permanently on the messages of the
      mailbox *)
  | `READ_ONLY
  (** READONLY response *)
  | `READ_WRITE
  (** READWRITE response *)
  | `TRYCREATE
  (** TRYCREATE response *)
  | `UIDNEXT of uint32
  (** Probable UID of next message to arrive. *)
  | `UIDVALIDITY of uint32
  (** UID validity value *)
  | `UNSEEN of uint32
  (** Sequence number of the first message without the [\Seen] flag *)
  | `APPENDUID of uint32 * uint32
  (** APPEND response code.  Requires support for the UIDPLUS extension. *)
  | `COPYUID of uint32 * Imap_set.t * Imap_set.t
  (** COPY response code.  Requires support for the UIDPLUS extension. *)
  | `UIDNOTSTICKY
  (** UIDs are not persistent.  Requires support for the UIDPLUS extension. *)
  | `COMPRESSIONACTIVE
  (** Compression is being used.  Requires support for the COMPRESS=DEFLATE extension. *)
  | `HIGHESTMODSEQ of uint64
  (** Highest modification sequence.  Requires support for the CONDSTORE extension. *)
  | `NOMODSEQ
  | `MODIFIED of Imap_set.t
  | `OTHER of string * string
  (** other type of response *)
  | `NONE
  (** No response code *) ] with sexp

(** response code, human readable text *)
type resp_text =
  resp_text_code * string with sexp

(** {2 Untagged responses} *)

(** Authentication condition responses *)
type resp_cond_auth =
  [ `OK of resp_text
  (** Authentication is needed *)
  | `PREAUTH of resp_text
  (** Authentication is not needed *) ] with sexp

(** BYE response *)
type resp_cond_bye =
  [ `BYE of resp_text ] with sexp

type response_fatal =
  resp_cond_bye with sexp

(** Condition state responses *)
type resp_cond_state =
  [ `OK of resp_text
  | `NO of resp_text
  | `BAD of resp_text ] with sexp

(** Message information *)
type message_data =
  [ `EXPUNGE of uint32
  | `FETCH of msg_att ] with sexp

(** Mailbox information *)
type mailbox_data =
  [ `FLAGS of flag list
  (** Flags that are applicable to this mailbox. *)
  | `LIST of mailbox_list
  (** LIST response, list of matching mailboxes. *)
  | `LSUB of mailbox_list
  (** LSUB response, list of matching mailboxes. *)
  | `SEARCH of uint32 list * uint64
  (** SEARCH or UID SEARCH response, list of sequence numbers (or UIDs) of
      matching messages, and, if available, highest modification sequence number
      of the corresponding messages.  This requires support for the CONDSTORE
      extension. *)
  | `STATUS of mailbox_data_status
  (** STATUS response, list of attribytes requested. *)
  | `EXISTS of int
  (** Number of messages in the mailbox *)
  | `RECENT of int
  (** Number of new messages in the mailbox *) ] with sexp

type id_response =
  [ `ID of (string * string) list ] with sexp

type namespace_response =
  [ `NAMESPACE of namespace list * namespace list * namespace list ] with sexp

type enable_response =
  [ `ENABLED of capability list ] with sexp

(** Untagged response *)
type response_data =
  [ resp_cond_state
  (** Condition state response *)
  | resp_cond_bye
  (** BYE response (server is about to close the connection) *)
  | mailbox_data
  (** Mailbox information *)
  | message_data
  (** Message information *)
  | capability_data
  (** Capability information *)
  | id_response
  (** ID response *)
  | namespace_response
  (** NAMESPACE response *)
  | enable_response
  (** ENABLE response *) ] with sexp

(** {2 Tagged responses} *)
  
type response_tagged =
  string * resp_cond_state with sexp

(** Ending response *)
type response_done =
  [ `TAGGED of response_tagged
  (** Tagged response *)
  | response_fatal
  (** Fatal error response *) ] with sexp

(** {2 Greeting response} *)
  
type greeting =
  [ resp_cond_auth
  (** If connection is accepted *)
  | resp_cond_bye
  (** If connection is refused *) ] with sexp

type continue_req =
  [ `CONT_REQ of [ `TEXT of resp_text | `BASE64 of string ] ] with sexp

type cont_req_or_resp_data_or_resp_done =
  [ continue_req
  | response_data
  | response_done ] with sexp

(** {2 Response parsers} *)

val greeting : greeting Imap_parser.t

val continue_req : [> continue_req] Imap_parser.t

val response_data : [> response_data] Imap_parser.t

val response_done : [> response_done] Imap_parser.t

val resp_data_or_resp_done : [response_data | response_done] Imap_parser.t

val cont_req_or_resp_data_or_resp_done : cont_req_or_resp_data_or_resp_done Imap_parser.t
