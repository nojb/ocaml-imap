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

(** Non-blocking IMAP4 protocol codec.

    [Imap] is a non-blocking codec to {{!section:decode}decode} and
    {{!section:encode}encode} the full
    {{:https://tools.ietf.org/html/rfc3501}IMAP4} protocol, together with some
    {{!section:extensions}extensions}.  It can process input without blocking on
    IO.

    Consult the {{!ex}examples} of use.

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

(** {1 Types} *)

type uint32 = Uint32.t
type uint64 = Uint64.t

type date = { day : int; month : int ; year : int }
type time = { hours : int; minutes : int; seconds : int; zone : int }

(** Message flags *)
type flag =
  [ `Answered
  | `Flagged
  | `Deleted
  | `Seen
  | `Draft
  | `Keyword of string
  | `Extension of string ]

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

(** {1 Responses} *)

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

(** MIME part *)
type section =
  [ `Header
  | `Header_fields of string list
  | `Header_fields_not of string list
  | `Text
  | `Mime
  | `Part of int * section
  | `All ]

(** Message attributes returned in the untagged [`Fetch]
    {{:#TYPEuntagged}response} to the {!fetch} command. *)
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

  | `Uid of uint32
  (** The unique identifier of the message. *)

  | `Modseq of uint64
  (** The modification sequence number of this message.  Requires [CONDSTORE]. *)

  | `Gm_msgid of uint64
  (** Gmail message ID. *)

  | `Gm_thrid of uint64
  (** Gmail thread ID. *)

  | `Gm_labels of string list
  (** Gmail labels. *) ]

(** {2 Response codes.}

    Status responses are [`Ok], [`No], [`Bad], [`Preauth] and [`Bye].  [`Ok],
    [`No], and [`Bad] can be tagged or untagged.  [`Preauth] and [`Bye] are
    always untagged.

    Status responses MAY include an OPTIONAL "response code".  The response code
    contains additional information or status codes for client software beyond
    the [`Ok], [`No], [`Bad], and are defined when there is a specific action
    that a client can take based upon the additional information.

    The last response code is cached by the library and can be accessed using
    {!last_code}. *)

type code_type =
  [ `Alert
  (** A special alert that MUST be presented to the user in a fashion that calls
      the user's attention to the message. *)

  | `Bad_charset of string list
  (** A {!search} command failed because the given charset is not supported by
      this implementation.  Contains the list the charsets that are supported by
      the server. *)

  | `Capability of capability list
  (** The list of capabilities supported by the server.  This can appear in the
      initial [`Ok] or [`Preauth] response to transmit an initial capabilities
      list.  This makes it unnecessary for a client to send a separate
      {!capability} command if it recognizes this response. *)

  | `Parse
  (** An error occurred in parsing the headers of a message in the mailbox. *)

  | `Permanent_flags of [ flag | `All ] list
  (** The list of flags the client can change permanently.  Any flags that are
      in the untagged [`Flags] {{:#TYPEuntagged}response}, but not in the
      [`Permanent_flags] list, can not be set permanently.  If the client
      attempts to {!store} a flag that is not in the [`Permanent_flags] list,
      the server will either ignore the change or store the state change for the
      remainder of the current session only.  The [`Permanent_flags] list can
      also include the special flag [`All], which indicates that it is possible
      to create new keywords by attempting to store those flags in the
      mailbox. *)

  | `Read_only
  (** The mailbox is selected read-only, or its access while selected has
      changed from read-write to read-only. *)

  | `Read_write
  (** The mailbox is selected read-write, or its access while selected has
      changed from read-only to read-write. *)

  | `Try_create
  (** An {!append} or {!copy} command is failing because the target mailbox does
      not exist (as opposed to some other reason).  This is a hint to the client
      that the operation can succeed if the mailbox is first created by the
      {!create} command. *)

  | `Uid_next of uint32
  (** The next unique identifier value.  Refer to {{:fdfads}???} for more
      information. *)

  | `Uid_validity of uint32
  (** The unique identifier validity value.  Refer to {{::fdfdsa}???} for more
      information. *)

  | `Unseen of uint32
  (** The number of the first message without the [`Seen] flag set. *)

  | `Other of string * string option
  (** Another response code. *)

  | `Closed
  (** Signals that the current mailbox has been closed.  It is sent when closing
      a mailbox implictly as a consequence of selecting a different mailbox.
      Requires [QRESYNC]. *)

  | `Highest_modseq of uint64
  (** The highest mod-sequence value of all messages in the mailbox.  Returned
      in an untagged [`Ok] {{:#TYPEuntagged}response} to the {!select} and
      {!examine} commands. *)

  | `No_modseq
  (** A server that doesn't support the persistent storage of mod-sequences for
      the mailbox MUST send this code in an untagged [`Ok]
      {{:#TYPEuntagged}response} to every successful {!select} or {!examine}
      command. *)

  | `Modified of (uint32 * uint32) list
  (** The [`Modified] response code includes the message set or set of UIDs of
      the most recent {!sotre} command of all messages that failed the
      [UNCHANGESINCE] test. *)

  | `Append_uid of uint32 * uint32
  (** Contains the [`Uid_validity] of the destination mailbox and the
      [`Uid] assigned to the appended message in the destination mailbox.

      This response code is returned in a tagged [`Ok] response to the {!append}
      command. *)

  | `Copy_uid of uint32 * (uint32 * uint32) list * (uint32 * uint32) list
  (** Sent in response to a [COPY] command, contains the [UIDVALIDITY] of the
      destination mailbox, followed by the set of UIDs of the source messages,
      and the set of UIDs of the destination messages (in the same order).
      Requires [UIDPLUS].

      This response code is returned in a tagged [`Ok] response to the {!copy}
      command. *)

  | `Uid_not_sticky
  (** The selected mailbox is supported by a mail store that does not support
      persistent UIDs; that is, [`Uid_validity] will be different each time the
      mailbox is selected.  Consequently, {!append} or {!copy} to this mailbox
      will not return an [`Append_uid] or [`Copy_uid] response code.

      This response code is returned in an untagged [`No] response to the
      {!select} command. *)

  | `Compression_active
  (** Compression has been activated. *)

  | `Use_attr
  (** A [CREATE] command failed due to the special-use attribute requested. *)

  | `None
  (** No response code was sent. *) ]

type code = code_type * string

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

(** Mailbox status items returned in the untagged [`Status]
    {{:#TYPEuntagged}response} to the [status] command. *)
type mbx_status =
  [ `Messages of int
  (** Number of messages in the mailbox. *)

  | `Recent of int
  (** Number of messages in the mailbox with the [`Recent] flag. *)

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
  [ `Ok of code
  | `No of code
  | `Bad of code ]

(** Server data responses. *)
type untagged =
  [ state
  (** Untagged status response. *)

  | `Bye of code

  | `Flags of flag list
  (** The [`Flags] response occurs as a result of a {!select} or {!examine}
      command.  Contains the list of flag that are applicable for this
      mailbox. *)

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
  (** The [`Exists] response reports the number of messages in the mailbox.
      This response occurs as a result of a {!select} or {!examine} command, and
      if the size of the mailbox changes (e.g., new messages). *)

  | `Recent of int
  (** The [`Recent] response reports the number of messages with the [`Recent]
      flag set.  This response occurs as a result of a {!select} or {!examine}
      command, and if the size of the mailbox changes (e.g., new messages). *)

  | `Expunge of uint32
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

  | `Fetch of uint32 * msg_att list
  (** The [`Fetch] response returns data about a message to the client.  It
      contains the message number and the list of data items and their values.
      This response occurs as the result of a {!fetch} or {!store} command, as
      well as by unilateral server decision (e.g., flag updates). *)

  | `Capability of capability list
  (** List of capabilities supported by the server. *)

  | `Vanished of (uint32 * uint32) list
  (** List of UIDs of messages that have been expunged from the current
      mailbox.  Requires [QRESYNC]. *)

  | `Vanished_earlier of (uint32 * uint32) list
  (** Same as [`Vanished], but sent only in response to a [FETCH (VANISHED)] or
      [SELECT/EXAMINE (QRESYNC)] command.  Requires [QRESYNC]. *)

  | `Enabled of capability list
  (** List of capabilities enabled after issuing {!enable} command. *) ]

(** Server responses.  Sending a command will typically result in a
    sequence of [untagged] items being sent back, ending in a [`Tagged]
    response or possibly [`Bye]. *)
type response =
  [ untagged
  (** Untagged data response. *)

  | `Preauth of code
  (** The session as been Pre-Authorized and no authentication is necessary.
      This should not occur in normal circumstances. *)

  | `Cont of string
  (** Continuation request: the server is waiting for client data.  The client
      must wait for this message before sending literal data. *)

  | `Tagged of string * state
  (** Tagged response: tag, result status. *) ]

val pp_response : Format.formatter -> [< response] -> unit
(** Pretty print a server response. *)

(** {1 Commands} *)

(** Search keys. See {!search} command. *)
type search_key =
  [  `Seq of (uint32 * uint32) list
  (** Messages with message sequence numbers corresponding to the specified
      message sequence number set. *)

  | `All
  (** All messages in the mailbox; the default initial key for ANDing. *)

  | `Answered
  (** Messages with the [`Answered] flag set. *)

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
  (** Messages with the [`Deleted] {{:#TYPEflag}flag} set. *)

  | `Draft
  (** Messages with the [`Draft] {{:#TYPEflag}flag} set. *)

  | `Flagged
  (** Messages with the [`Flagged] {{:#TYPEflag}flag} set. *)

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
  (** Messages that have the [`Recent] {{:#TYPEflag}flag} set but not the
      [`Seen] {{:#TYPEflag}flag}.  This is functionally equivalent to
      [`And (`Recent, `Unseen)]. *)

  | `Not of search_key
  (** Messages that do not match the specified search key. *)

  | `Old
  (** Messages that do not have the [`Recent] {{:#TYPEflag}flag} set.  This is
      functionally equivalent to [`Not `Recent] (as opposed to [`Not `New]). *)

  | `On of date
  (** Messages whose internal date (disregarding time and timezone) is within
      the specified date.  *)

  | `Or of search_key * search_key
  (** Messages that match either search key. *)

  | `Recent
  (** Messages that have the [`Recent] {{:#TYPEflag}flag} set. *)

  | `Seen
  (** Messages that have the [`Seen] {{:#TYPEflag}flag} set. *)

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

  | `Uid of (uint32 * uint32) list
  (** Messages with unique identifiers corresponding to the specified unique
      identifier set.  Sequence set ranges are permitted. *)

  | `Unanswered
  (** Messages that do not have the [`Answered] {{:#TYPEflag}flag} set. *)

  | `Undeleted
  (** Messages that do not have the [`Deleted] {{:#TYPEflag}flag} set. *)

  | `Undraft
  (** Messages that do not have the [`Draft] {{:#TYPeflag}flag} set. *)

  | `Unflagged
  (** Messages that do not have the [`Flagged] {{:#TYPEflag}flag} set. *)

  | `Unkeyword of string
  (** Messages that do not have the specified keyword {{:#TYPEflag}flag} set. *)

  | `Unseen
  (** Messages that do not have the [`Seen] {{:#TYPEflag}flag} set. *)

  | `And of search_key * search_key
  (** Messages that satisfy both search criteria. *)

  | `Modseq of uint64
  (** Messages that have equal or greater modification sequence numbers. *)

  | `Gm_raw of string
  (** TOOD *)

  | `Gm_msgid of uint64
  (** Messages with a given Gmail Message ID. *)

  | `Gm_thrid of uint64
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
  (** Functionally equivalent to [`Body_section (`Peek, `Header, None)],
      differing in the syntax of the resulting untagged [`Fetch]
      {{:#TYPEuntagged}data} ([`Rfc822_header] is returned). *)

  | `Rfc822_text
  (** Functionally equivalent to [`Body_section (`Look, `Text, None)], differing
      in the syntax of the resulting untagged [`Fetch] {{:#TYPEuntagged}data}
      ([`Rfc822_text] is returned). *)

  | `Rfc822_size
  (** The size of the message. *)

  | `Rfc822
  (** Functionally equivalent to [`Body_section (`Look, `All, None)], differing
      in the syntax of the resulting untagged [`Fetch] {{:#TYPEuntagged}data}
      ([`Rfc822] is returned). *)

  | `Body
  (** Non-extensible form of [`Body_structure]. *)

  | `Body_section of [ `Peek | `Look ] * section * (int * int) option
  (** The text of a particular body section.  The [`Peek] flag is an alternate
      form that does not implicitly set the [`Seen] {{:#TYPEflag}flag}. *)

  | `Body_structure
  (** The MIME body structure of the message.  This is computed by the
      server by parsing the MIME header fields in the [RFC-2822] header
      and MIME headers. *)

  | `Uid
  (** The unique identifier for the message. *)

  | `Flags
  (** The {{:#TYPEflag}flags} that are set for this message. *) ]

(** Mailbox status items.  See {!status} command. *)
type status_att =
  [ `Messages
  (** The number of messages in the mailbox. *)

  | `Recent
  (** The number of messages with the [`Recent] {{:#TYPEflag}flag} set. *)

  | `Uid_next
  (** The next unique identifier value of the mailbox. *)

  | `Uid_validity
  (** The unique identifier validity value of the mailbox. *)

  | `Unseen
  (** The number of messages which do not have the [`Seen] {{:#TYPEflag}flag}
      set. *)

  | `Highest_modseq
  (** TODO *) ]

type command

val login : string -> string -> command
(** [login user pass] identifies the client to the server and carries the
    plaintext password authenticating this [user] with password [pass].  A
    server MAY include a [`Capability] response {{:#TYPEcode}code} in the tagged
    [`Ok] response to a successful [login] command in order to send capabilities
    automatically. *)

val capability : command
(** [capability] returns the list of capabilities supported by the server.  The
    server MUST send a single untagged [`Capability] {{:#TYPEuntagged}response}
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
(** [logout] gracefully terminates a session.  The server MUST send an untagged
    [`Bye] {{:#TYPEuntagged}response} before the (tagged) [`Ok] response. *)

val noop : command
(** [noop] does nothing.  Since any command can return a status update as
    untagged data, the [noop] command can be used as a periodic poll for new
    messages or message status updates during a period of inactivity (this is
    the preferred method to do this). *)

val subscribe : string -> command
(** [subscribe m] adds the mailbox [m] to the server's set of "active" or
    "subscribed" mailboxes as returned by the {!lsub} command. *)

val unsubscribe : string -> command
(** [unsubcribe m] removes the mailbox [m] from the server's set of "active" or
    "subscribed" mailboxes as returned by the {!lsub} command. *)

val list : ?ref:string -> string -> command
(** [list ref m] returns a subset of names from the complete set of all names
    available to the client.  Zero or more untagged [`List]
    {{:#TYPEuntagged}replies} are returned, containing the name attributes,
    hierarchy delimiter.  The optional argument [ref] is the name of a mailbox
    or a level of mailbox hierarchy, and indicates the context in which the
    mailbox name is interpreted.*)

val lsub : ?ref:string -> string -> command
(** [lsub ref m] is identical to {!list}, except that it returns a subset of
    names from the set of names that the user has declared as being "active" or
    "subscribed". *)

val status : string -> status_att list -> command
(** [status] requests {{:#TYPEmbx_status}status information} of the indicated
    mailbox.  An untagged [`Status] {{:#TYPEuntagged}response} is returned with
    the requested information. *)

val copy : ?uid:bool -> (uint32 * uint32) list -> string -> command

val check : command
(** [check] requests a checkpoint of the currently selected mailbox.  A
    checkpoint refers to any implementation-dependent housekeeping associated
    with the mailbox. *)

val close : command
(** [close] permanently removes all messages that have the [`Deleted]
    {{:#TYPEflag}flag} set from the currently selected mailbox, and returns to
    the authenticated state from the selected state. *)

val expunge : command
(** [expunge] permanently removes all messages that have the [`Deleted]
    {{:#TYPEflag}flag} set from the currently selected mailbox.  Before
    returning an [`Ok] to the client, an untagged [`Expunge]
    {{:#TYPEuntagged}response} is sent for each message that is removed. *)

val search : ?uid:bool -> search_key -> command
(** [search uid sk] searches the mailbox for messages that match the given
    searching criteria.  If [uid] is [true] (the default), then the matching
    messages' unique identification numbers are returned.  Otherwise, their
    sequence numbers are.  The untagged [`Search] {{:#TYPEuntagged}response}
    from the server contains a listing of message numbers corresponding to those
    messages that match the searching criteria. *)

val select : ?condstore:bool -> string -> command
(** [select condstore m] selects the mailbox [m] so that its messages can be
    accessed.  If [condstore] (default value [false]) is [true], then the server
    will return the [`Modseq] data item in all subsequent untagged [`Fetch]
    {{:#TYPEuntagged}responses}. *)

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

(** {1 Authenticators}

    These are used to implement SASL authentication. *)

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

(** {1 Running and Processing Commands} *)

type error =
  [ `Incorrect_tag of string * string
  | `Decode_error of
      [ `Expected_char of char * int
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
  | `No ]

val pp_error : Format.formatter -> error -> unit

type connection

type src = [ `String of string | `Channel of in_channel | `Manual ]
type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

val connection : [< src] -> [< dst] -> connection

module Manual : sig
  (** {2 Performing IO} *)
  val src : connection -> string -> int -> int -> unit
  val dst : connection -> string -> int -> int -> unit
  val dst_rem : connection -> int
end

val run : connection ->
  [ `Cmd of command | `Await | `Idle | `Idle_done | `Authenticate of authenticator ] ->
  [ `Untagged of untagged | `Ok | `Error of error | `Await_src | `Await_dst ]

val last_code : connection -> code

(** {1:limitations Limitations}

    - no body structure extensions
    - "simple" search modseq criteria *)

(** {1:ex Example: checking for new mail}

    [wait_mail host port user pass mbox] logs into the IMAP server
    [host] on port [port] over SSL, authenticates user [user] with password
    [pass] and watches mailbox [mbox] until a new message arrives.  When a
    new message arrives, it outputs the sender's name and stops.

    In order to detect new messages, we record the next UID value of the
    [INBOX] mailbox as soon as we open it.  Then we wait for the server to
    alert us of activity in this mailbox using the [IDLE] command.  And we look
    for messages with UIDs larger than this one using the [SEARCH] command.

{[
let run sock i o c v =
  let rec write_fully s off len =
    if len > 0 then
      let rc = Ssl.write sock s off len in
      write_fully s (off + rc) (len - rc)
  in
  let rec loop = function
    | `Await_src ->
        let rc = Ssl.read sock i 0 (Bytes.length i) in
        if !debug_flag then Format.eprintf ">>> %d\n%s>>>\n%!" rc (String.sub i 0 rc);
        Imap.Manual.src c i 0 rc;
        loop (Imap.run c `Await)
    | `Await_dst ->
        let rc = Bytes.length o - Imap.Manual.dst_rem c in
        write_fully o 0 rc;
        if !debug_flag then Format.eprintf "<<< %d\n%s<<<\n%!" rc (String.sub o 0 rc);
        Imap.Manual.dst c o 0 (Bytes.length o);
        loop (Imap.run c `Await)
    | `Untagged _ as r -> r
    | `Ok -> `Ok
    | `Error e ->
        Format.eprintf "@[IMAP Error: %a@]@." Imap.pp_error e;
        failwith "imap error"
  in
  loop (Imap.run c v)

let () =
  Ssl.init ()

let wait_mail host port user pass mbox =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let he = Unix.gethostbyname host in
  Unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port));
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  let sock = Ssl.embed_socket fd ctx in
  Ssl.connect sock;
  let c = Imap.connection `Manual `Manual in
  let i = Bytes.create io_buffer_size in
  let o = Bytes.create io_buffer_size in
  Imap.Manual.dst c o 0 (Bytes.length o);
  match run sock i o c `Await with
  | `Ok ->
      let rec logout = function
        | `Untagged _ -> logout (run sock i o c `Await)
        | `Ok -> ()
      in
      let rec idle uidn = function
        | `Untagged (`Exists _) -> idle_done uidn (run sock i o c (`Idle_done))
        | `Untagged _ -> idle uidn (run sock i o c `Await) | `Ok -> idle_done uidn `Ok
      and idle_done uidn = function
        | `Untagged _ -> idle_done uidn (run sock i o c `Await)
        | `Ok ->
            search uidn Uint32.zero
              (run sock i o c (`Cmd (Imap.search ~uid:true (`Uid [uidn, Uint32.max_int]))))
      and search uidn n = function
        | `Untagged (`Search (n :: _, _)) -> search uidn n (run sock i o c `Await)
        | `Untagged _ -> search uidn n (run sock i o c `Await)
        | `Ok ->
            (* n = 0 means that we couldn't find the message after all. *)
            if n = Uint32.zero then idle uidn (run sock i o c `Idle) else
            let cmd = Imap.fetch ~uid:true ~changed:Uint64.one [n, n] [`Envelope] in
            fetch uidn n None (run sock i o c (`Cmd cmd))
      and fetch uidn n name = function
        | `Untagged (`Fetch (_, att)) ->
            let name =
              List.fold_left
                (fun name att -> match att with
                   | `Envelope e ->
                       begin match e.Imap.env_from with
                       | [] -> name
                       | ad :: _ -> Some ad.Imap.ad_name
                       end
                   | _ -> name) name att
            in
            fetch uidn n name (run sock i o c `Await)
        | `Untagged _ -> fetch uidn n name (run sock i o c `Await)
        | `Ok ->
            let name = match name with None -> "<unnamed>" | Some name -> name in
            Format.printf "New mail from %s, better go and check it out!\n%!" name;
            logout (run sock i o c (`Cmd Imap.logout))
      in
      let rec select uidn = function
        | `Untagged (`Ok (`Uid_next uidn, _)) -> select uidn (run sock i o c `Await)
        | `Untagged _ -> select uidn (run sock i o c `Await)
        | `Ok -> idle uidn (run sock i o c `Idle)
      in
      let rec login = function
        | `Untagged _ -> login (run sock i o c `Await)
        | `Ok -> select Uint32.zero (run sock i o c (`Cmd (Imap.select mbox)))
      in
      login (run sock i o c (`Cmd (Imap.login user pass)))
  | `Untagged _ -> assert false
]}
*)
