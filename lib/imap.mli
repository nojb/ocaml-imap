(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

(** Non-blocking IMAP4 protocol codec

    [Imap] is a non-blocking codec to {{!section:decode}decode} and
    {{!section:encode}encode} the {{:https://tools.ietf.org/html/rfc3501}IMAP4}
    protocol, together with some {{!section:extensions}extensions}.  It can
    process input without blocking on IO.

    Consult the {{!examples}examples} of use.

    {3 References}
    {ul
    {- M. Crispin
    {e {{:https://tools.ietf.org/html/rfc3501}INTERNET MESSAGE ACCESS PROTOCOL - VERSION 4rev1}, 2003}}
    {- A. Melnikov, S. Hole
    {e {{:https://tools.ietf.org/html/rfc4551}IMAP Extension for Conditional STORE Operation or Quick Flag Changes Resynchronization}, 2006}}
    {- A. Melnikov, D. Cridland, C. Wilson
    {e {{:https://tools.ietf.org/html/rfc5162}IMAP4 Extensions for Quick Mailbox Resynchronization}, 2008}}
    {- A. Gulbrandsen, Ed., A. Melnikov, Ed.
    {e {{:https://tools.ietf.org/html/rfc5161}The IMAP ENABLE Extension}, 2008}}
    {- B. Leiba, J. Nicolson
    {e {{:https://tools.ietf.org/html/rfc6154}IMAP LIST Extension for Special-Use Mailboxes}, 2011}}
    {- M. Crispin
    {e {{:https://tools.ietf.org/html/rfc4315}Internet Message Access Protocol (IMAP) - UIDPLUS extension}, 2005}}}
*)

module MUTF7 : sig

  (** {1 Modified UTF-7}

      A mail-safe (7-bit) encoding of Unicode.  It is mainly used to transmit
      mailbox names and flags/labels.  The necessary encoding/decoding is
      automatically performed by the library, and it is exposed here only in
      case it is useful for some other purposes. *)

  val encode : string -> string
  (** [encode s] encoded a string into modified UTF-7.  The input string is
      assumed to be valid UTF-8.  If any malformed unicode characters are found they
      are replaced by the Unicode replacement character [U+FFFD]. *)

  val decode : string -> string
  (** [decode s] decodes a modified UTF-7 string into a UTF-8 encoded string. *)
end

(** {1:decode Decode} *)

type address =
  { ad_name : string;
    ad_adl : string;
    ad_mailbox : string;
    ad_host : string }

(** Message envelope information *)
type envelope =
  { env_date : string;
    env_subject : string;
    env_from : address list;
    env_sender : address list;
    env_reply_to : address list;
    env_to : address list;
    env_cc : address list;
    env_bcc : address list;
    env_in_reply_to : string;
    env_message_id : string }

(** Unsigned 32-bit numbers are used for message UID and Sequence numbers. *)
type uint32 = Uint32.t

(** Unsigned 64-bit numbers are used for: modification sequence numbers and
    Gmail message and thread IDs. *)
type uint64 = Uint64.t

(** List of message numbers (UID or Sequence) as a list of intervals. *)
type set = (uint32 * uint32) list

(** List of standard capabilites. *)
type capability =
  [ `Acl
  | `Binary
  | `Catenate
  | `Children
  | `Compress_deflate
  | `Condstore
  | `Enable
  | `Idle
  | `Id
  | `Literal_plus
  | `Multi_append
  | `Namespace
  | `Qresync
  | `Quote
  | `Sort
  | `Start_tls
  | `Uid_plus
  | `Unselect
  | `Xlist
  | `Auth of [ `Anonymous
             | `Login
             | `Plain ]
  | `Xoauth2
  | `Gmail
  | `Other of string ]

type fields =
  { fld_params : (string * string) list;
    fld_id : string option;
    fld_desc : string option;
    fld_enc : string;
    fld_octets : int }

(** Message body *)
type body =
  [ `Text of string * fields * int
  | `Message of fields * envelope * body * int
  | `Basic of string * string * fields
  | `Multiple of body list * string ]

(** Message flags *)
type flag =
  [ `Answered
  | `Flagged
  | `Deleted
  | `Seen
  | `Draft
  | `Keyword of string
  | `Extension of string ]

(** MIME part *)
type section =
  [ `Header
  | `Header_fields of string list
  | `Header_fields_not of string list
  | `Text
  | `Mime
  | `Part of int * section
  | `All ]

(** Message attributes. *)
type msg_att =
  [ `Flags of [ flag | `Recent ] list
  (** Flags. *)

  | `Envelope of envelope
  (** Envelope information. *)

  | `Internal_date of string
  (** The internal date of the message. *)

  | `Rfc822 of string option
  (** The raw message contents (header & body). *)

  | `Rfc822_header of string option
  (** The message header. *)

  | `Rfc822_text of string option
  (** The message body. *)

  | `Rfc822_size of int
  (** The size of the message. *)

  | `Body of body
  (** TODO *)

  | `Body_structure of body
  (** TODO *)

  | `Body_section of section * int option * string option
  (** A message MIME part, starting offset, part data. *)

  | `Uid of uint32
  (** UID.  Typically constant between sessions, but needs to be re-requested if
      the mailbox [UIDVALIDITY] value changes. *)

  | `Modseq of uint64
  (** The modification sequence number of this message.  Requires [CONDSTORE]. *)

  | `Gm_msgid of uint64
  (** Gmail message ID. *)

  | `Gm_thrid of uint64
  (** Gmail thread ID. *)

  | `Gm_labels of string list
  (** Gmail labels. *) ]

(** Response codes *)
type code =
  [ `Alert
  (** An alert that should be presented to the user as such. *)

  | `Bad_charset of string list
  (** A [SEARCH] command failed because the requested charset is not supported
      by the server, followed by a list of supported charsets. *)

  | `Capability of capability list
  (** List of capabilities supported by the server. *)

  | `Parse
  (** An error occurred during the parsing of MIME information of a message in
      the current mailbox. *)

  | `Permanent_flags of [ flag | `All ] list
  (** List of message flags that can be changed permanently in the current
      mailbox.  The [`All] flag means that means that it is possible to create new
      keyword flags by issuing appropiate [STORE] commands. *)

  | `Read_only
  (** The mailbox is read-only, no changes are permitted. *)

  | `Read_write
  (** The mailbox access is, or has been changed to, read-write. *)

  | `Try_create
  (** An [APPEND] or [COPY] command is failing because the destination mailbox
      does not exist.  Suggests issuing a [CREATE] command to create it. *)

  | `Uid_next of uint32
  (** The next UID of the current mailbox. *)

  | `Uid_validity of uint32
  (** The [UIDVALIDITY] value of the current mailbox. *)

  | `Unseen of uint32
  (** The Sequence number of the next unseen message in the current mailbox. *)

  | `Other of string * string option
  (** Another response code. *)

  | `Closed
  (** Signals that the current mailbox has been closed.  It is sent when closing
      a mailbox implictly as a consequence of selecting a different mailbox.
      Requires [QRESYNC]. *)

  | `Highest_modseq of uint64
  (** The highest modification sequence number of all messages in the current
      mailbox.  Requires [CONDSTORE]. *)

  | `No_modseq
  (** The server does not support the persistent storage of modification
      sequence numbers.  Requires [CONDSTORE]. *)

  | `Modified of set
  (** The message numbers (UID or Sequence) of all the messages that failed the
      [UNCHANGEDSINCE] test used int he last issued [STORE] or [UID STORE]
      command.  Requires [CONDSTORE]. *)

  | `Append_uid of uint32 * uint32
  (** Sent in response to an [APPEND] command, contains the [UIDVALIDITY] of
      the destination mailbox, followed by the [UID] of the newly appended message.
      Requires [UIDPLUS]. *)

  | `Copy_uid of uint32 * set * set
  (** Sent in response to a [COPY] command, contains the [UIDVALIDITY] of the
      destination mailbox, followed by the set of UIDs of the source messages,
      and the set of UIDs of the destination messages (in the same order).
      Requires [UIDPLUS]. *)

  | `Uid_not_sticky
  (** The current mailbox does not support persistent UIDs, i.e., [UIDVALIDITY]
      will be different each time the mailbox is selected.  Requires
      [UIDPLUS]. *)

  | `Compression_active
  (** Compression has been activated. *)

  | `Use_attr
  (** A [CREATE] command failed due to the special-use attribute requested. *)

  | `None
  (** No response code was sent. *) ]

(** Mailbox flags *)
type mbx_flag =
  [ `Noselect
  | `Marked
  | `Unmarked
  | `Noinferiors
  | `HasChildren
  | `HasNoChildren
  | `All
  | `Archive
  | `Drafts
  | `Flagged
  | `Junk
  | `Sent
  | `Trash
  | `Extension of string ]

(** Mailbox status items. *)
type mbx_status =
  [ `Messages of int
  (** Number of messages in the mailbox. *)

  | `Recent of int
  (** Number of messages in the mailbox with the [\Recent] flag. *)

  | `Uid_next of uint32
  (** The expected UID of the next message to be added to this mailbox. *)

  | `Uid_validity of uint32
  (** The UID VALIDITY value of this mailbox. *)

  | `Unseen of uint32
  (** The Sequence number of the first message in the mailbox that has not been
      seen. *)

  | `Highest_modseq of uint64
  (** The highest modification sequence number of all the messages in the
      mailbox.  This is only sent back if [CONDSTORE] is enabled. *) ]

type state =
  [ `Ok of code * string
  | `No of code * string
  | `Bad of code * string ]

(** Server Responses.  Sending a command to the server will typically result in
    a sequence of [response] items being sent back, ending in a [`Tagged]
    response or possibly [`Bye]. *)
type response =
  [ state

  | `Bye of code * string
  (** [BYE] message sent by the server just before closing the connection. *)

  | `Flags of flag list
  (** List of labels are valid for the messages contained within. *)

  | `List of mbx_flag list * char option * string
  (** [LIST] response: mailbox flags, character used as path delimiter
      (optional), and mailbox name. *)

  | `Lsub of mbx_flag list * char option * string
  (** [LSUB] response, same information as [LIST]. *)

  | `Search of uint32 list * uint64 option
  (** [SEARCH] or [UID SEARCH] response: list of message numbers (UID or
      Sequence), and optionally the highest modification sequence number of the
      returned list of messages if [CONDSTORE] is enabled. *)

  | `Status of string * mbx_status list
  (** [STATUS] response: mailbox name, list of status items. *)

  | `Exists of int
  (** Number of messages in the currently selected mailbox.  This response may
      be sent unrequested as part of a response to any command to inform the
      client of new messages. *)

  | `Recent of int
  (** Number of messages with the [\Recent] flag. *)

  | `Expunge of uint32
  (** Inform that a message has been permanently deleted from the current
      mailbox.  The message number is always a Sequence number. *)

  | `Fetch of uint32 * msg_att list
  (** [FETCH] or [UID FETCH] response: message number (UID of Sequence), list of
      attributes requested. *)

  | `Capability of capability list
  (** List of capabilities supported by the server. *)

  | `Preauth of code * string
  (** The server is informing that the session as been Pre-Authorized and no
      authentication is necessary.  This should not occur in normal
      circumstances. *)

  | `Cont of string
  (** Continuation request: the server is waiting for client data.  The client
      must wait for this message before sending literal data. *)

  | `Tagged of string * state
  (** Tagged response: tag, result status. *)

  | `Vanished of set
  (** List of UIDs of messages that have been expunged from the current
      mailbox.  Requires [QRESYNC]. *)

  | `Vanished_earlier of set
  (** Same as [`Vanished], but sent only in response to a [FETCH (VANISHED)] or
      [SELECT/EXAMINE (QRESYNC)] command.  Requires [QRESYNC]. *)

  | `Enabled of capability list
  (** [ENABLE] response: list of capabilities enabled. *) ]

type error =
  [ `Expected_char of char
  | `Expected_string of string
  | `Unexpected_char of char
  | `Unexpected_string of string
  | `Illegal_char of char
  | `Illegal_range ]

val pp_response : Format.formatter -> response -> unit
(** Pretty print a server response. *)

val pp_error : Format.formatter -> error -> unit
(** Pretty print a decoding error. *)

type decoder

val decoder : string -> decoder

val decode : decoder -> [ `Ok of response | `Await | `Error of error ]

(** {1:encode Encode} *)

(** Keys used for [SEARCH] command. *)
type search_key =
  [ `All
  | `Answered
  | `Bcc of string
  | `Before of int * int * int
  | `Body of string
  | `Cc of string
  | `Deleted
  | `Flagged
  | `From of string
  | `Keyword of string
  | `New
  | `Old
  | `On of int * int * int
  | `Recent
  | `Seen
  | `Since of int * int * int
  | `Subject of string
  | `Text of string
  | `To of string
  | `Unanswered
  | `Undeleted
  | `Unflagged
  | `Unkeyword of string
  | `Unseen
  | `Draft
  | `Header of string * string
  | `Larger of int
  | `Not of search_key
  | `Or of search_key * search_key
  | `Sent_before of int * int * int
  | `Sent_on of int * int * int
  | `Sent_since of int * int * int
  | `Smaller of int
  | `Uid of set
  | `Undraft
  | `In_set of set
  | `And of search_key * search_key
  | `Modseq of (flag * [ `Priv | `Shared | `All ]) option * uint64
  | `Gm_raw of string
  | `Gm_msgid of uint64
  | `Gm_thrid of uint64
  | `Gm_labels of string list ]

type fetch_att =
  [ `Envelope
  | `Internal_date
  | `Rfc822_header
  | `Rfc822_text
  | `Rfc822_size
  | `Rfc822
  | `Body
  | `Body_section of section * (int * int) option
  | `Body_peek_section of section * (int * int) option
  | `Body_structure
  | `Uid
  | `Flags ]

type status_att =
  [ `Messages
  | `Recent
  | `Uid_next
  | `Uid_validity
  | `Unseen
  | `Highest_modseq ]

type fragment

val capability          : fragment
(** The [CAPABILITY] command. *)

val noop                : fragment
(** The [NOOP] command. *)

val logout              : fragment
(** The [LOGOUT] command. *)

val login               : string -> string -> fragment
(** [login u p] is the [LOGIN] command with user [u] and password [p]. *)

val create              : string -> fragment
(** [create m] is the [CREATE] command. *)

val rename              : string -> string -> fragment
val subscribe           : string -> fragment
val unsubscribe         : string -> fragment
val list                : string -> string -> fragment
val lsub                : string -> string -> fragment
val status              : string -> status_att list -> fragment
val copy                : set -> string -> fragment
val uid_copy            : set -> string -> fragment
val check               : fragment
val close               : fragment
val expunge             : fragment
val search              : search_key -> fragment
val uid_search          : search_key -> fragment
val select_condstore    : string -> fragment
val select              : string -> fragment
val examine_condstore   : string -> fragment
val examine             : string -> fragment
val enable              : capability list -> fragment

type fetch_spec = [ `All | `Fast | `Full | `Att of fetch_att list ]

val fetch_changed       : set -> [< fetch_spec] -> changed:uint64 -> fragment
val fetch_vanished      : set -> [< fetch_spec] -> changed:uint64 -> fragment
val uid_fetch_changed   : set -> [< fetch_spec] -> changed:uint64 -> fragment
val uid_fetch_vanished  : set -> [< fetch_spec] -> changed:uint64 -> fragment
val fetch               : set -> [< fetch_spec] -> fragment
val uid_fetch           : set -> [< fetch_spec] -> fragment

type store_spec = [ `Add | `Set | `Remove ]

val store               : set -> ?silent:bool -> [< store_spec] -> flag list -> fragment
val uid_store           : set -> ?silent:bool -> [< store_spec] -> flag list -> fragment
val store_unchanged     : set -> ?silent:bool -> uint64 -> [< store_spec] -> flag list -> fragment
val uid_store_unchanged : set -> ?silent:bool -> uint64 -> [< store_spec] -> flag list -> fragment
val store_labels        : set -> ?silent:bool -> [< store_spec] -> string list -> fragment
val uid_store_labels    : set -> ?silent:bool -> [< store_spec] -> string list -> fragment
