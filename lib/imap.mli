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

type uint32 = Uint32.t
type uint64 = Uint64.t

(** {1 Responses} *)

type date = { day : int; month : int ; year : int }
type time = { hours : int; minutes : int; seconds : int; zone : int }

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

(** MIME message types *)
type mime =
  [ `Text of string * fields * int
  | `Message of fields * envelope * mime * int
  | `Basic of string * string * fields
  | `Multiple of mime list * string ]

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

(** Message attributes returned in the {{:#TYPEuntagged}untagged} [`Fetch]
    response to the {!fetch} command. *)
type msg_att =
  [ `Flags of [ flag | `Recent ] list
  (** A parenthesized list of flags that are set for this message. *)

  | `Envelope of envelope
  (** A list that describes the envelope structure of a message.  This is
      computed by the server by parsing the [RFC-2822] header into the component
      parts, defaulting various fields as necessary. *)

  | `Internal_date of date * time
  (** A string representing the internal date of the message. *)

  | `Rfc822 of string option
  (** Equivalent to [BODY[]]. *)

  | `Rfc822_header of string option
  (** The message header. *)

  | `Rfc822_text of string option
  (** The message body. *)

  | `Rfc822_size of int
  (** The [RFC-2822] size of the message. *)

  | `Body of mime
  (** A form of [BODYSTRUCTURE] without extension data. *)

  | `Body_structure of mime
  (** A parenthesized list that describes the [MIME-IMB] body structure of a
      message.  This is computed by the server by parsing the [MIME-IMB] header
      fields, defaulting various fields as necessary.  *)

  | `Body_section of section * int option * string option
  (** A message MIME part, starting offset, part data. *)

  | `Uid of Uint32.t
  (** The unique identifier of the message. *)

  | `Modseq of Uint64.t
  (** The modification sequence number of this message.  Requires [CONDSTORE]. *)

  | `Gm_msgid of Uint64.t
  (** Gmail message ID. *)

  | `Gm_thrid of Uint64.t
  (** Gmail thread ID. *)

  | `Gm_labels of string list
  (** Gmail labels. *) ]

(** Response codes. *)
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

(** Mailbox status items returned in the {{:#TYPEuntagged}untagged} [`Status]
    response to the [status] command. *)
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
  (** The [`Flags] response occurs as a result of a {!select} or {!examine}
      command.  Contains the list of flag that are applicable for this
      mailbox. *)

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
  (** The [`Exists] response reports the number of messages in the mailbox.
      This response occurs as a result of a {!select} or {!examine} command, and
      if the size of the mailbox changes (e.g., new messages). *)

  | `Recent of int
  (** The [`Recent] response reports the number of messages with the
      {v \Recent v} flag set.  This response occurs as a result of a {!select} or
      {!examine} command, and if the size of the mailbox changes (e.g., new
      messages). *)

  | `Expunge of Uint32.t
  (** The [`Expunge] response reports that the specified message sequence number
      has been permanently removed from the mailbox.  The message sequence
      number for each successive message in the mailbox is immediately
      decremented by 1, and this decrement is reflected in message sequence
      numbers in subsequent responses (including other untagged [`Expunge]
      responses).

      The [`Expunge] response also decrements the number of messages in the
      mailbox; it is not necessary to send an [`Exists] response with the new
      value.

      As a result of the immediate decrement rule, message sequence numbers that
      appear in a set of successive [`Expunge] responses depend upon whether the
      messages are removed starting from lower numbers to higher numbers, or
      from higher numbers to lower numbers.  For example, if the last 5 messages
      in a 9-message mailbox are expunged, a "lower to higher" server will send
      five untagged EXPUNGE responses for message sequence number 5, whereas a
      "higher to lower server" will send successive untagged [`Expunge]
      responses for message sequence numbers 9, 8, 7, 6, and 5. *)

  | `Fetch of Uint32.t * msg_att list
  (** The [`Fetch] response returns data about a message to the client.  It
      contains the message number and the list of data items and their values.
      This response occurs as the result of a {!fetch} or {!store} command, as
      well as by unilateral server decision (e.g., flag updates). *)

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

  | `Tagged of string * state
  (** Tagged response: tag, result status. *) ]

val pp_response : Format.formatter -> response -> unit
(** Pretty print a server response. *)

(** {1 Commands} *)

(** Search keys. See {!search} command. *)
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
  (** Messages that satisfy both search criteria. *)

  | `Modseq of Uint64.t
  (** Messages that have equal or greater modification sequence numbers. *)

  | `Gm_raw of string
  | `Gm_msgid of Uint64.t
  (** Messages with a given Gmail Message ID. *)

  | `Gm_thrid of Uint64.t
  (** Messages with a given Gmail Thread ID. *)

  | `Gm_labels of string list
  (** Messages with given Gmail labels. *) ]

(** Message attributes.  See {!fetch} command. *)
type fetch_att =
  [ `Envelope
  (** The envelope structure of the message.  This is computed by the server by
      parsing the header into the component parts, defaulting various fields as
      necessary. *)

  | `Internal_date
  (** The internal date of the message. *)

  | `Rfc822_header
  (** Functionally equivalent to [BODY.PEEK[HEADER]], differing in the syntax of
       the resulting untagged [FETCH] data ([RFC822.HEADER] is returned). *)

  | `Rfc822_text
  (** Functionally equivalent to [BODY[TEXT]], differing in the syntax of the
      resulting untagged [FETCH] data ([RFC822.TEXT] is returned). *)

  | `Rfc822_size
  (** The size of the message. *)

  | `Rfc822
  (** Functionally equivalent to [BODY[]], differing in the syntax of the
      resulting untagged [FETCH] data ([RFC822] is returned). *)

  | `Body
  (** Non-extensible form of [BODYSTRUCTURE]. *)

  | `Body_section of [ `Peek | `Look ] * section * (int * int) option
  (** The text of a particular body section.  The [`Peek] flag is an alternate
      form that does not implicitly set the {v \Seen v} flag. *)

  | `Body_structure
  (** The MIME body structure of the message.  This is computed by the
      server by parsing the MIME header fields in the [RFC-2822] header
      and MIME headers. *)

  | `Uid
  (** The unique identifier for the message. *)

  | `Flags
  (** The flags that are set for this message. *) ]

(** Mailbox status items.  See {!status} command. *)
type status_att =
  [ `Messages
  (** The number of messages in the mailbox. *)

  | `Recent
  (** The number of messages with the {v \Recent v} flag set. *)

  | `Uid_next
  (** The next unique identifier value of the mailbox. *)

  | `Uid_validity
  (** The unique identifier validity value of the mailbox. *)

  | `Unseen
  (** The number of messages which do not have the {v \Seen v} flag set. *)

  | `Highest_modseq ]

type command

val login : string -> string -> command
(** [login user pass] identifies the client to the server and carries the
    plaintext password authenticating this [user] with password [pass].  A
    server MAY include a [`Capability] response {{:#TYPEcode}code} in the tagged
    [`Ok] response to a successful [login] command in order to send capabilities
    automatically. *)

val capability : command
(** [capability] returns the list of capabilities supported by the server.  The
    server MUST send a single {{:#TYPEuntagged}untagged} [`Capability] response
    with "IMAP4rev1" as one of the listed capabilities before the (tagged) [`Ok]
    response.  See the type describing the possible
    {{:#TYPEcapability}capabilities}. *)

val create : string -> command
(** [create m] creates a mailbox named [m].  An [`Ok] response is returned only
    if a new mailbox with that name has been created.  It is an error to attempt
    to create "INBOX" or a mailbox with a name that refers to an extant mailbox.
    Any error in creation will return a tagged [`No] response. *)

val rename : string -> string -> command
(** [rename oldname newname] command changes the name of a mailbox from
    [oldname] to [newname].  A tagged [`Ok] response is returned only if the
    mailbox has been renamed.  It is an error to attempt to rename from a
    mailbox name that does not exist or to a mailbox name that already exists.
    Any error in renaming will return a tagged [`No] response. *)

val logout : command
(** [logout] gracefully terminates a session.  The server MUST send a [`Bye]
    {{:#TYPEuntagged}untagged} response before the (tagged) [`Ok] response. *)

val noop : command
(** [noop] does nothing.  Since any command can return a status update as
    untagged data, the [noop] command can be used as a periodic poll for new
    messages or message status updates during a period of inactivity (this is
    the preferred method to do this). *)

val subscribe : string -> command
(** The [SUBSCRIBE] command adds the specified mailbox name to the server's
    set of "active" or "subscribed" mailboxes as returned by the [LSUB]
    command. *)

val unsubscribe : string -> command
(** The [UNSUBSCRIBE] command removes the specified mailbox name from the
    server's set of "active" or "subscribed" mailboxes as returned by the
    [LSUB] command. *)

val list : string -> string -> command
(** The [LIST] command returns a subset of names from the complete set of all
    names available to the client. *)

val lsub : string -> string -> command
(** The [LSUB] command returns a subset of names from the set of names that
    the user has declared as being "active" or "subscribed". *)

val status : string -> status_att list -> command
(** [status] requests the status of the indicated mailbox.  An
    {{:#TYPEuntagged}untagged} [`Status] response is returned with the requested
    information.  See the type describing {{:#TYPEmbx_status}status items} which
    are returned. *)

val copy : ?uid:bool -> (Uint32.t * Uint32.t) list -> string -> command

val check : command
(** [check] requests a checkpoint of the currently selected mailbox.  A
    checkpoint refers to any implementation-dependent housekeeping associated
    with the mailbox. *)

val close : command
(** [close] permanently removes all messages that have the {v \Deleted v} flag
    set from the currently selected mailbox, and returns to the authenticated
    state from the selected state. *)

val expunge : command
(** [expunge] permanently removes all messages that have the {v \Deleted v} flag
    set from the currently selected mailbox.  Before returning an [`Ok] to the
    client, an {{:#TYPEuntagged}untagged} [`Expunge] response is sent for each
    message that is removed. *)

val search : ?uid:bool -> search_key -> command
(** [search uid sk] searches the mailbox for messages that match the given
    searching criteria.  If [uid] is [true] (the default), then the matching
    messages' unique identification numbers are returned.  Otherwise, their
    sequence numbers are.  The {{:#TYPEuntagged}untagged} [`Search] response
    from the server contains a listing of message numbers corresponding to those
    messages that match the searching criteria. *)

val select : ?condstore:bool -> string -> command
(** [select condstore m] selects the mailbox [m] so that its messages can be
    accessed.  If [condstore] (default value [false]) is [true], then the server
    will return the [`Modseq] data item in all subsequent
    {{:#TYPEuntagged}untagged} [`Fetch] responses. *)

val examine : ?condstore:bool -> string -> command
(** [examine condstore m] is identical to [select condstore m] and returns the
    same output; however, the selected mailbox is identified as read-only. *)

type set = (uint32 * uint32) list

val fetch      : ?uid:bool -> ?changed:uint64 -> ?vanished:bool -> set -> fetch_att list -> command
val fetch_fast : ?uid:bool -> ?changed:uint64 -> ?vanished:bool -> set -> command
val fetch_full : ?uid:bool -> ?changed:uint64 -> ?vanished:bool -> set -> command
val fetch_all  : ?uid:bool -> ?changed:uint64 -> ?vanished:bool -> set -> command
(** The [FETCH] command retrieves data associated with a message in the
    mailbox. *)

val store_add_flags     : ?uid:bool -> ?silent:bool -> ?unchanged:uint64 -> set -> flag list -> command
val store_set_flags     : ?uid:bool -> ?silent:bool -> ?unchanged:uint64 -> set -> flag list -> command
val store_remove_flags  : ?uid:bool -> ?silent:bool -> ?unchanged:uint64 -> set -> flag list -> command
val store_add_labels    : ?uid:bool -> ?silent:bool -> ?unchanged:uint64 -> set -> string list -> command
val store_set_labels    : ?uid:bool -> ?silent:bool -> ?unchanged:uint64 -> set -> string list -> command
val store_remove_labels : ?uid:bool -> ?silent:bool -> ?unchanged:uint64 -> set -> string list -> command
(** The [STORE] command alters data associated with a message in the mailbox. *)

val enable : capability list -> command

(** {1 Authenticators} *)

type authenticator =
  { name : string;
    step : string -> [ `Ok of [ `Last | `More ] * string | `Error of string ] }

val plain : string -> string -> authenticator
(** [plain user pass] authenticates via [PLAIN] mechanism using username [user]
    and password [pass]. *)

val xoauth2 : string -> string -> authenticator
(** [xoauth2 user token] authenticates via [XOAUTH2] mechanishm user username
    [user] and access token [token].  The access token should be obtained
    independently. *)

(** {1 Running commands} *)

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
  | `Bad_greeting
  | `Auth_error of string
  | `Bad
  | `Bye
  | `No ]

val pp_error : Format.formatter -> error -> unit

type connection
type src = [ `String of string | `Channel of in_channel | `Manual ]
type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

val connection : [< src] -> [< dst] -> connection

module Manual : sig
  val src : connection -> string -> int -> int -> unit
  val dst : connection -> string -> int -> int -> unit
  val dst_rem : connection -> int
end

val run : connection ->
  [ `Cmd of command | `Await | `Idle | `Idle_done | `Authenticate of authenticator ] ->
  [ `Untagged of untagged | `Ok | `Error of error | `Await_src | `Await_dst ]

(** {1:limitations Limitations}

    - no body structure extensions
    - "simple" search modseq criteria *)
