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

type date = { day : int; month : int ; year : int }

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

val string_of_capability : capability -> string

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

  | `Uid of Uint32.t
  (** UID.  Typically constant between sessions, but needs to be re-requested if
      the mailbox [UIDVALIDITY] value changes. *)

  | `Modseq of Uint64.t
  (** The modification sequence number of this message.  Requires [CONDSTORE]. *)

  | `Gm_msgid of Uint64.t
  (** Gmail message ID. *)

  | `Gm_thrid of Uint64.t
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

  | `Uid_next of Uint32.t
  (** The next UID of the current mailbox. *)

  | `Uid_validity of Uint32.t
  (** The [UIDVALIDITY] value of the current mailbox. *)

  | `Unseen of Uint32.t
  (** The Sequence number of the next unseen message in the current mailbox. *)

  | `Other of string * string option
  (** Another response code. *)

  | `Closed
  (** Signals that the current mailbox has been closed.  It is sent when closing
      a mailbox implictly as a consequence of selecting a different mailbox.
      Requires [QRESYNC]. *)

  | `Highest_modseq of Uint64.t
  (** The highest modification sequence number of all messages in the current
      mailbox.  Requires [CONDSTORE]. *)

  | `No_modseq
  (** The server does not support the persistent storage of modification
      sequence numbers.  Requires [CONDSTORE]. *)

  | `Modified of (Uint32.t * Uint32.t) list
  (** The message numbers (UID or Sequence) of all the messages that failed the
      [UNCHANGEDSINCE] test used int he last issued [STORE] or [UID STORE]
      command.  Requires [CONDSTORE]. *)

  | `Append_uid of Uint32.t * Uint32.t
  (** Sent in response to an [APPEND] command, contains the [UIDVALIDITY] of
      the destination mailbox, followed by the [UID] of the newly appended message.
      Requires [UIDPLUS]. *)

  | `Copy_uid of Uint32.t * (Uint32.t * Uint32.t) list * (Uint32.t * Uint32.t) list
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

  | `Uid_next of Uint32.t
  (** The expected UID of the next message to be added to this mailbox. *)

  | `Uid_validity of Uint32.t
  (** The UID VALIDITY value of this mailbox. *)

  | `Unseen of Uint32.t
  (** The Sequence number of the first message in the mailbox that has not been
      seen. *)

  | `Highest_modseq of Uint64.t
  (** The highest modification sequence number of all the messages in the
      mailbox.  This is only sent back if [CONDSTORE] is enabled. *) ]

type state =
  [ `Ok of code * string
  | `No of code * string
  | `Bad of code * string ]

(** Server data responses. *)
type untagged =
  [ state
  (** Untagged status response. *)

  | `Flags of flag list
  (** List of labels are valid for the messages contained within. *)

  | `List of mbx_flag list * char option * string
  (** [LIST] response: mailbox flags, character used as path delimiter
      (optional), and mailbox name. *)

  | `Lsub of mbx_flag list * char option * string
  (** [LSUB] response, same information as [LIST]. *)

  | `Search of Uint32.t list * Uint64.t option
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

  | `Expunge of Uint32.t
  (** Inform that a message has been permanently deleted from the current
      mailbox.  The message number is always a Sequence number. *)

  | `Fetch of Uint32.t * msg_att list
  (** [FETCH] or [UID FETCH] response: message number (UID of Sequence), list of
      attributes requested. *)

  | `Capability of capability list
  (** List of capabilities supported by the server. *)

  | `Vanished of (Uint32.t * Uint32.t) list
  (** List of UIDs of messages that have been expunged from the current
      mailbox.  Requires [QRESYNC]. *)

  | `Vanished_earlier of (Uint32.t * Uint32.t) list
  (** Same as [`Vanished], but sent only in response to a [FETCH (VANISHED)] or
      [SELECT/EXAMINE (QRESYNC)] command.  Requires [QRESYNC]. *)

  | `Enabled of capability list
  (** [ENABLE] response: list of capabilities enabled. *) ]

(** Server responses.  Sending a command will typically result in a
    sequence of [untagged] items being sent back, ending in a [`Tagged]
    response or possibly [`Bye]. *)
type response =
  [ untagged
  (** Untagged data response. *)

  | `Bye of code * string
  (** The server will close the connection immediately. *)

  | `Preauth of code * string
  (** The session as been Pre-Authorized and no authentication is necessary.
      This should not occur in normal circumstances. *)

  | `Cont of string
  (** Continuation request: the server is waiting for client data.  The client
      must wait for this message before sending literal data. *)

  | `Tagged of string * state ]
(** Tagged response: tag, result status. *)

val pp_response : Format.formatter -> response -> unit
(** Pretty print a server response. *)

(** Keys used for [SEARCH] command. *)
type search_key =
  [  `Seq of (Uint32.t * Uint32.t) list
  (** Messages with message sequence numbers corresponding to the specified
      message sequence number set. *)

  | `All
  (** All messages in the mailbox; the default initial key for ANDing. *)

  | `Answered
  (** Messages with the {v \Answered v} flag set. *)

  | `Bcc of string
  (** Messages that contain the specified string in the envelope structure's
      "BCC" field. *)

  | `Before of date
  (** Messages whose internal date (disregarding time and timezone) is earlier
      than the specified date. *)

  | `Body of string
  (** Messages that contain the specified string in the body of the message. *)

  | `Cc of string
  (** Messages that contain the specified string in the envelope structure's
      "CC" field. *)

  | `Deleted
  (** Messages with the {v \Deleted v} flag set. *)

  | `Draft
  (** Messages with the {v \Draft v} flag set. *)

  | `Flagged
  (** Messages with the {v \Flagged v} flag set. *)

  | `From of string
  (** Messages that contain the specified string in the envelope structure's
      "FROM" field. *)

  | `Header of string * string
  (** Messages that have a header with the specified field-name and that
      contains the specified string in the text of the header (what comes after
      the colon).  If the string to search is zero-length, this matches all
      messages that have a header line with the specified field-name regardless
      of the contents. *)

  | `Keyword of string
  (** Messages with the specified keyword flag set. *)

  | `Larger of int
  (** Messages with a size larger than the specified number of octets. *)

  | `New
  (** Messages that have the {v \Recent v} flag set but not the {v \Seen v}
      flag.  This is functionally equivalent to [`And (`Recent, `Unseen)]. *)

  | `Not of search_key
  (** Messages that do not match the specified search key. *)

  | `Old
  (** Messages that do not have the {v \Recent v} flag set.  This is
      functionally equivalent to [`Not `Recent] (as opposed to [`Not `New]). *)

  | `On of date
  (** Messages whose internal date (disregarding time and timezone) is within
      the specified date.  *)

  | `Or of search_key * search_key
  (** Messages that match either search key. *)

  | `Recent
  (** Messages that have the {v \Recent v} flag set. *)

  | `Seen
  (** Messages that have the {v \Seen v} flag set. *)

  | `Sent_before of date
  (** Messages whose "Date:" header (disregarding time and timezone) is earlier
      than the specified date. *)

  | `Sent_on of date
  (** Messages whose "Date:" header (disregarding time and timezone) is within
      the specified date. *)

  | `Sent_since of date
  (** Messages whose "Date:" header (disregarding time and timezone) is within
      or later than the specified date.  *)

  | `Since of date
  (** Messages whose internal date (disregarding time and timezone) is within or
      later than the specified date.  *)

  | `Smaller of int
  (** Messages with a size smaller than the specified number of octets. *)

  | `Subject of string
  (** Messages that contain the specified string in the envelope structure's
      "SUBJECT" field. *)

  | `Text of string
  (** Messages that contain the specified string in the header or body of the
      message. *)

  | `To of string
  (** Messages that contain the specified string in the envelope structure's
      "TO" field. *)

  | `Uid of (Uint32.t * Uint32.t) list
  (** Messages with unique identifiers corresponding to the specified unique
      identifier set.  Sequence set ranges are permitted. *)

  | `Unanswered
  (** Messages that do not have the {v \Answered v} flag set. *)

  | `Undeleted
  (** Messages that do not have the {v \Deleted v} flag set. *)

  | `Undraft
  (** Messages that do not have the {v \Draft v} flag set. *)

  | `Unflagged
  (** Messages that do not have the {v \Flagged v} flag set. *)

  | `Unkeyword of string
  (** Messages that do not have the specified keyword flag set. *)

  | `Unseen
  (** Messages that do not have the {v \Seen v} flag set. *)

  | `And of search_key * search_key
  | `Modseq of (flag * [ `Priv | `Shared | `All ]) option * Uint64.t
  | `Gm_raw of string
  | `Gm_msgid of Uint64.t
  | `Gm_thrid of Uint64.t
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

type command =
  [ `Login of string * string
  (** The [LOGIN] command to authenticate with the server.  Note that this
      should only be used on a connection that is protected by TLS or similar
      protocol, as the login and password are sent in plain. *)

  | `Capability
  (** The [CAPABILITY] command.  Returns the list of capabilities supported by
      the server.  Note that this list can change when passing from a
      non-authenticated to an authenticated state. *)

  | `Create of string
  (** The [CREATE] command used to create a new mailbox. *)

  | `Rename of string * string
  (** The [RENAME] command renames an existing mailbox. *)

  | `Logout
  (** The [LOGOUT] command used to gracefully terminate a session. *)

  | `Noop
  (** The [NOOP] command used to keep the connection alive. *)

  | `Subscribe of string
  | `Unsubscribe of string
  | `List of string * string
  | `Lsub of string * string
  | `Status of string * status_att list
  | `Copy of [ `Uid | `Seq ] * (Uint32.t * Uint32.t) list * string
  | `Check
  | `Close
  (** The [CLOSE] command, used to close for access the currently selected
      mailbox. *)

  | `Expunge
  | `Search of [ `Uid | `Seq ] * search_key
  | `Select of [ `Condstore | `Plain ] * string
  | `Examine of [ `Condstore | `Plain ] * string
  | `Enable of capability list
  | `Fetch of [ `Uid | `Seq ] * (Uint32.t * Uint32.t) list *
              [ `All | `Fast | `Full | `List of fetch_att list ] *
              [ `Changed_since of Uint64.t | `Changed_since_vanished of Uint64.t | `All ]
  | `Store of [ `Uid | `Seq ] * (Uint32.t * Uint32.t) list * [ `Silent | `Loud ] *
              [ `Unchanged_since of Uint64.t | `All ] *
              [ `Add | `Set | `Remove ] * [ `Flags of flag list | `Labels of string list ] ]

(** {1 Commands} *)

type error =
  [ `Incorrect_tag of string * string
  | `Decode_error of
      [ `Expected_char of char
      | `Expected_string of string
      | `Unexpected_char of char
      | `Unexpected_string of string
      | `Illegal_char of char
      | `Illegal_range
      | `Unexpected_eoi ]
  | `Unexpected_cont
  | `Not_running
  | `Bad
  | `Bye
  | `No ]

type connection
type src = [ `String of string | `Channel of in_channel | `Manual ]
type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

val connection : [< src] -> [< dst] -> connection

module Manual : sig
  val src : connection -> string -> int -> int -> unit
  val dst : connection -> string -> int -> int -> unit
  val dst_rem : connection -> int
end

val run : connection -> [ `Cmd of command | `Await ] ->
  [ `Untagged of untagged | `Ok | `Error of error | `Await_src | `Await_dst ]

(* val search              : search_key -> command *)
(* val uid_search          : search_key -> command *)
(* val enable              : capability list -> command *)
