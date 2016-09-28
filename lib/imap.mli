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

    [Imap] is a non-blocking codec to encode and decode the full
    {{:https://tools.ietf.org/html/rfc3501}IMAP4} protocol, together with some
    {{!section:limitations}extensions}.  It can process input without blocking
    on IO and is completely independent of any particular buffering and/or IO
    strategry (concurrent, like {{:https://github.com/ocsigen/lwt}Lwt} or
    {{:https://github.com/janestreet/async}Async}, sequential, etc.).

    Most users should begin by looking at the {{!ex}examples}, the types and
    functions describing {{!type:connection}connections},
    {{!section:commands}commands}, and the {!run} function.

    See the {{!ex}examples} of use.

    {3 References}
    {ul
    {- M. Crispin
    {e {{:https://tools.ietf.org/html/rfc3501}INTERNET MESSAGE ACCESS PROTOCOL - VERSION 4rev1}, 2003}}}
*)

(** {1 Common types for IMAP} *)

(** Unsigned 32-bit integers are used for: message sequence numbers, unique
    identification numbers (UIDs). *)
type uint32 = Uint32.t

(** Unsigned 64-bit integers are used for: mod-sequence numbers, Gmail message
    and thread IDs. *)
type uint64 = Uint64.t

module Modseq : module type of Uint64
module UID : module type of Uint32
module Seq : module type of Uint32

type _ uid_or_seq =
  | UID : UID.t uid_or_seq
  | Seq : Seq.t uid_or_seq

type date = { day : int; month : int ; year : int }
type time = { hours : int; minutes : int; seconds : int; zone : int }

module Capability : sig
  (** List of standard capabilites.  These are returned by the {!capability}
      command, in status {{!code}codes} and can be enabled by the {!enable} command. *)
  type capability =
    | ACL
    | BINARY
    | CATENATE
    | CHILDREN
    | COMPRESS_DEFLATE
    | CONDSTORE
    | ENABLE
    | IDLE
    | ID
    | LITERALPLUS
    | MULTIAPPEND
    | NAMESPACE
    | QRESYNC
    | QUOTE
    | SORT
    | STARTTLS
    | UIDPLUS
    | UNSELECT
    | XLIST
    | AUTH_ANONYMOUS
    | AUTH_LOGIN
    | AUTH_PLAIN
    | XOAUTH2
    | X_GM_EXT_1
    | OTHER of string
end

module Envelope : sig
  (** {3 Envelope information}

      It is returned when fetching the [`Envelope] message
      {{!fetch_query}attribute} using the {!fetch} command.

      If [val a : address], then the expression
      {[
        Printf.printf "\"%s\" <%s@%s>" a.ad_name a.ad_mailbox a.ad_host
      ]}
      will output [a] in the usual format ["name" <user\@host.com>]. *)

  type address =
    {
      ad_name: string;
      ad_adl: string;
      ad_mailbox: string;
      ad_host: string;
    }

  val pp_address: Format.formatter -> address -> unit

  type envelope =
    {
      env_date: string;
      env_subject: string;
      env_from: address list;
      env_sender: address list;
      env_reply_to: address list;
      env_to: address list;
      env_cc: address list;
      env_bcc: address list;
      env_in_reply_to: string;
      env_message_id: string;
    }

  val pp_envelope: Format.formatter -> envelope -> unit
end

module MIME : sig
  (** {3 MIME message structure}

      The following types describe the bodies of
      {{:http://en.wikipedia.org/wiki/MIME}MIME} emails.  The IMAP server can
      parse the MIME structure of the messages and return individual parts.  This
      saves the client from having to do that parsing itself.  See
      {ul
      {- {{:http://tools.ietf.org/html/rfc2045}RFC 2045: Format of Internet Message Bodies}}
      {- {{:http://tools.ietf.org/html/rfc2046}RFC 2046: Media Types}}}
      and related RFCs for details. *)

  (** Basic fields of a MIME body part.  See
      {{:https://tools.ietf.org/html/rfc2045}RFC 2045} for more details. *)
  type fields =
    { fld_params : (string * string) list;      (* Part parameters *)
      fld_id : string option;                   (* Optional part ID *)
      fld_desc : string option;                 (* Optional content description *)
      fld_enc : string;                         (* Content transfer encoding *)
      fld_octets : int }                        (* Size in bytes *)

  val pp_fields : Format.formatter -> fields -> unit

  (** MIME content types

      The message MIME content type can be retrieved using {!fetch} with
      [`Body_structure].  Similarly, individual MIME parts can be retrieved using
      {!fetch} with an appropriate [`Body_section] message attribute.

      In IMAP, MIME media types are described as follows:

      - [`Text (s, f, n)] corresponds to the MIME type ["TEXT/" ^ s].  Common
        examples of the subtype [s] are ["HTML"] and ["PLAIN"].  [f] contains
        general part information (see {!fields}), and [n] is the number of text
        lines of the part.

      - [`Message (f, e, m, n)] corresponds to the MIME type ["MESSAGE/RFC822"],
        used to enclose a complete message within a message. [f] contains general
        part information (see {!fields}), [e] is the envelope information of the
        encapsulated message, [m] is the MIME structure of the encapsulated
        message, and [n] is the number of lines in the encapsulated message.

      - [`Basic (t, s, f)] corresponds to a (non-multipart) MIME type [t ^ "/" ^
        s].  Common examples of the type [t] are ["APPLICATION"], ["AUDIO"],
        ["IMAGE"], ["MESSAGE"], ["VIDEO"].  [f] contains general part information
        (see {!fields}).

      - [`Multipart (p, s)] corresponds to the MIME type ["MULTIPART/" ^ s].  [p]
        is the lists of MIME subparts. *)
  type mime =
    | Text of string * fields * int
    | Message of fields * Envelope.envelope * mime * int
    | Basic of string * string * fields
    | Multipart of mime list * string

  val pp_mime : Format.formatter -> mime -> unit
end

module Msg : sig
  (** Message flags.  The underlying string of [`Extension s] is ['\\' ^ s], while
      [`Keyword s] is [s]. *)
  type flag =
    [ `Answered
    | `Flagged
    | `Deleted
    | `Seen
    | `Draft
    | `Keyword of string
    | `Extension of string ]

  (** Message number (either sequence or UIDs) sets, as a union of intervals.  It
      is the responsability of the client to make any necessary validity check, as
      none is performed by the library.

      For example, the IMAP sequence set [1,2:3] is represented by [[(1, 1); (2, 3)]]. *)
  type set = (uint32 * uint32) list

  val pp_set: Format.formatter -> set -> unit

  (** The [section] type is used to specify which part(s) of a message should be
      retrieved when using {!fetch} with [`Body_section].  See
      {{:https://tools.ietf.org/html/rfc3501#section-6.4.5}RFC 3501 6.4.5} for
      more details.

      For more on RFC 2822 headers, see
      {{:https://tools.ietf.org/html/rfc2822#section-2.2}RFC 2822, 2.2}.  For more
      on MIME headers, see {{:https://tools.ietf.org/html/rfc2045#section-3}RFC
      2045, 3}. *)
  type section =
    | HEADER                             (* Full RFC 2822 message headers *)
    | HEADER_FIELDS of string list       (* Header fields matching one of the given field names *)
    | HEADER_FIELDS_NOT of string list   (* Header fields not matching any of the given field names *)
    | TEXT                               (* The text body of this part, omitting RFC2822 headers *)
    | MIME                               (* The MIME headers of this part *)
    | Part of int * section              (* Subpart *)
    | All                              (* The whole message *)

  val pp_section : Format.formatter -> section -> unit

  (** {3 Fetch responses}

      Message attributes returned in the untagged [`Fetch] {{!untagged}response}
      to the {!fetch} command. *)

  type msg_att =
    | FLAGS of [ flag | `Recent ] list
    (** A parenthesized list of flags that are set for this message. *)

    | ENVELOPE of Envelope.envelope
    (** A list that describes the envelope structure of a message.  This is
        computed by the server by parsing the [RFC-2822] header into the component
        parts, defaulting various fields as necessary. *)

    | INTERNALDATE of date * time
    (** A string representing the internal date of the message. *)

    | RFC822 of string option
    (** Equivalent to [BODY[]]. *)

    | RFC822_HEADER of string option
    (** The message header. *)

    | RFC822_TEXT of string option
    (** The message body. *)

    | RFC822_SIZE of int
    (** The [RFC-2822] size of the message. *)

    | BODY of MIME.mime
    (** A form of [BODYSTRUCTURE] without extension data. *)

    | BODYSTRUCTURE of MIME.mime
    (** A parenthesized list that describes the [MIME-IMB] body structure of a
        message.  This is computed by the server by parsing the [MIME-IMB] header
        fields, defaulting various fields as necessary.  *)

    | BODY_SECTION of section * int option * string option
    (** A message MIME part, starting offset, part data. *)

    | UID of uint32
    (** The unique identifier of the message. *)

    | MODSEQ of uint64
    (** The modification sequence number of this message.  Requires [CONDSTORE]. *)

    | X_GM_MSGID of uint64
    (** Gmail message ID. *)

    | X_GM_THRID of uint64
    (** Gmail thread ID. *)

    | X_GM_LABELS of string list
      (** Gmail labels. *)

  module Request : sig
    (** Message attributes that can be requested using the {!fetch} command. *)

    type t

    val envelope: t
    (** The envelope structure of the message.  This is computed by the server
        by parsing the header into the component parts, defaulting various
        fields as necessary. *)

    val internaldate: t
    (** The internal date of the message. *)

    val rfc822_header: t
    (** Functionally equivalent to [`Body_section (`Peek, `Header, None)],
        differing in the syntax of the resulting untagged [`Fetch]
        {{!untagged}data} ([`Rfc822_header] is returned). *)

    val rfc822_text: t
    (** Functionally equivalent to [`Body_section (`Look, `Text, None)],
        differing in the syntax of the resulting untagged [`Fetch]
        {{!untagged}data} ([`Rfc822_text] is returned). *)

    val rfc822_size: t
    (** The size of the message. *)

    val rfc822: t
    (** Functionally equivalent to [`Body_section (`Look, `All, None)],
        differing in the syntax of the resulting untagged [`Fetch]
        {{!untagged}data} ([`Rfc822] is returned). *)

    val body: t
    (** Non-extensible form of [`Body_structure]. *)

    val body_section: [ `Peek | `Look ] -> section -> (int * int) option -> t
    (** The text of a particular body section.  The [`Peek] flag is an alternate
        form that does not implicitly set the [`Seen] {!flag}. *)

    val bodystructure: t
    (** The MIME body structure of the message.  This is computed by the server
        by parsing the MIME header fields in the [RFC-2822] header and MIME
        headers. *)

    val uid: t
    (** The unique identifier for the message. *)

    val flags: t
    (** The {{!flag}flags} that are set for this message. *)
  end

  val pp: Format.formatter -> msg_att -> unit
end

module Code : sig
  (** {3 Response codes}

      Status responses are [`Ok], [`No], [`Bad], [`Preauth] and [`Bye].  [`Ok],
      [`No], and [`Bad] can be tagged or untagged.  [`Preauth] and [`Bye] are
      always untagged.

      Status responses may include a "response code".  The response code contains
      additional information or status codes for client software beyond the [`Ok],
      [`No], [`Bad], and are defined when there is a specific action that a client
      can take based upon the additional information. *)

  type code =
    | ALERT
    (** A special alert that MUST be presented to the user in a fashion that calls
        the user's attention to the message. *)
    | BADCHARSET of string list
    (** A {!search} command failed because the given charset is not supported by
        this implementation.  Contains the list the charsets that are supported by
        the server. *)
    | CAPABILITY of Capability.capability list
    (** The list of capabilities supported by the server.  This can appear in the
        initial [`Ok] or [`Preauth] response to transmit an initial capabilities
        list.  This makes it unnecessary for a client to send a separate
        {!capability} command if it recognizes this response. *)
    | PARSE
    (** An error occurred in parsing the headers of a message in the mailbox. *)
    | PERMANENTFLAGS of [ Msg.flag | `All ] list
    (** The list of flags the client can change permanently.  Any flags that are
        in the untagged [`Flags] {{!untagged}response}, but not in the
        [`Permanent_flags] list, can not be set permanently.  If the client
        attempts to {!store} a flag that is not in the [`Permanent_flags] list,
        the server will either ignore the change or store the state change for the
        remainder of the current session only.  The [`Permanent_flags] list can
        also include the special flag [`All], which indicates that it is possible
        to create new keywords by attempting to store those flags in the
        mailbox. *)
    | READ_ONLY
    (** The mailbox is selected read-only, or its access while selected has
        changed from read-write to read-only. *)
    | READ_WRITE
    (** The mailbox is selected read-write, or its access while selected has
        changed from read-only to read-write. *)
    | TRYCREATE
    (** An {!append} or {!copy} command is failing because the target mailbox does
        not exist (as opposed to some other reason).  This is a hint to the client
        that the operation can succeed if the mailbox is first created by the
        {!create} command. *)
    | UIDNEXT of uint32
    (** The next unique identifier value.  Refer to {{:fdfads}???} for more
        information. *)
    | UIDVALIDITY of uint32
    (** The unique identifier validity value.  Refer to {{::fdfdsa}???} for more
        information. *)
    | UNSEEN of uint32
    (** The number of the first message without the [`Seen] flag set. *)
    | OTHER of string * string option
    (** Another response code. *)
    | CLOSED
    (** Signals that the current mailbox has been closed.  It is sent when closing
        a mailbox implictly as a consequence of selecting a different mailbox.
        Requires [QRESYNC]. *)
    | HIGHESTMODSEQ of uint64
    (** The highest mod-sequence value of all messages in the mailbox.  Returned
        in an untagged [`Ok] {{!untagged}response} to the {!select} and
        {!examine} commands. *)
    | NOMODSEQ
    (** A server that doesn't support the persistent storage of mod-sequences for
        the mailbox MUST send this code in an untagged [`Ok]
        {{!untagged}response} to every successful {!select} or {!examine}
        command. *)
    | MODIFIED of (uint32 * uint32) list
    (** The [`Modified] response code includes the message set or set of UIDs of
        the most recent {!sotre} command of all messages that failed the
        [UNCHANGESINCE] test. *)
    | APPENDUID of uint32 * uint32
    (** Contains the [`Uid_validity] of the destination mailbox and the
        [`Uid] assigned to the appended message in the destination mailbox.

        This response code is returned in a tagged [`Ok] response to the {!append}
        command. *)
    | COPYUID of uint32 * (uint32 * uint32) list * (uint32 * uint32) list
    (** Sent in response to a [COPY] command, contains the [UIDVALIDITY] of the
        destination mailbox, followed by the set of UIDs of the source messages,
        and the set of UIDs of the destination messages (in the same order).
        Requires [UIDPLUS].

        This response code is returned in a tagged [`Ok] response to the {!copy}
        command. *)
    | UIDNOTSTICKY
    (** The selected mailbox is supported by a mail store that does not support
        persistent UIDs; that is, [`Uid_validity] will be different each time the
        mailbox is selected.  Consequently, {!append} or {!copy} to this mailbox
        will not return an [`Append_uid] or [`Copy_uid] response code.

        This response code is returned in an untagged [`No] response to the
        {!select} command. *)
    | COMPRESSIONACTIVE
    (** Compression has been activated.  Requires the [COMPRESS=DEFLATE]
        capability. *)
    | USEATTR
    (** A {!create} command failed due to the special-use attribute requested. *)

  val pp: Format.formatter -> code -> unit
end

module Mailbox : sig
  (** {3 Mailbox flags}

      Returned by the {!list} or {!lsub} commands and also in some
      status {{!code}codes}. *)

  type mbx_flag =
    | Noselect
    | Marked
    | Unmarked
    | Noinferiors
    | HasChildren
    | HasNoChildren
    | All
    | Archive
    | Drafts
    | Flagged
    | Junk
    | Sent
    | Trash
    | Extension of string

  (** {3 Mailbox status responses}

      Mailbox status items returned in the untagged [`Status]
      {{!untagged}response} to the {!status} command. *)

  type status_response =
    | MESSAGES of int
    (** Number of messages in the mailbox. *)

    | RECENT of int
    (** Number of messages in the mailbox with the [`Recent] flag. *)

    | UIDNEXT of UID.t
    (** The expected UID of the next message to be added to this mailbox. *)

    | UIDVALIDITY of UID.t
    (** The UID VALIDITY value of this mailbox. *)

    | UNSEEN of uint32
    (** The Sequence number of the first message in the mailbox that has not been
        seen. *)

    | HIGHESTMODSEQ of Modseq.t
      (** The highest modification sequence number of all the messages in the
          mailbox.  This is only sent back if [CONDSTORE] is enabled. *)

  module Request : sig
    (** Mailbox attibutes that can be requested with the {!status} command. *)

    type t

    val messages: t
    (** The number of messages in the mailbox. *)

    val recent: t
    (** The number of messages with the [`Recent] {!flag} set. *)

    val uidnext: t
    (** The next unique identifier value of the mailbox. *)

    val uidvalidity: t
    (** The unique identifier validity value of the mailbox. *)

    val unseen: t
    (** The number of messages which do not have the [`Seen] {!flag}
        set. *)

    val highestmodseq: t
    (** TODO *)
  end
end

(** {e Extended} message numbers sets, as a union of intervals.  The second
    component of an interval being [None] means that it should take the largest
    possible value appearing in the mailbox (this is denoted ['*'] in the IMAP
    protocol).

    For example, the IMAP extended set [1,2:3,4:*] will be represented by
    [[(1, Some 1); (2, Some 3); (4, None)]]. *)
type eset = (uint32 * uint32 option) list

module Search : sig
  (** Search keys. See {!search} command. *)
  type key

  val all: key
  (** All messages in the mailbox; the default initial key for ANDing. *)

  val seq: eset -> key
  (** Messages with message sequence numbers corresponding to the specified
      message sequence number set. *)

  val answered: key
  (** Messages with the [`Answered] flag set. *)

  val bcc: string -> key
  (** Messages that contain the specified string in the envelope structure's
      "BCC" field. *)

  val before: date -> key
  (** Messages whose internal date (disregarding time and timezone) is earlier
      than the specified date. *)

  val body: string -> key
  (** Messages that contain the specified string in the body of the message. *)

  val cc: string -> key
  (** Messages that contain the specified string in the envelope structure's
      "CC" field. *)

  val deleted: key
  (** Messages with the [`Deleted] {!flag} set. *)

  val draft: key
  (** Messages with the [`Draft] {!flag} set. *)

  val flagged: key
  (** Messages with the [`Flagged] {!flag} set. *)

  val from: string -> key
  (** Messages that contain the specified string in the envelope structure's
      "FROM" field. *)

  val header: string -> string -> key
  (** Messages that have a header with the specified field-name and that
      contains the specified string in the text of the header (what comes after
      the colon).  If the string to search is zero-length, this matches all
      messages that have a header line with the specified field-name regardless
      of the contents. *)

  val keyword: string -> key
  (** Messages with the specified keyword {!flag} set. *)

  val larger: int -> key
  (** Messages with a size larger than the specified number of octets. *)

  val new_: key
  (** Messages that have the [`Recent] {!flag} set but not the [`Seen] {!flag}.
      This is functionally equivalent to [`And (`Recent, `Unseen)]. *)

  val not: key -> key
  (** Messages that do not match the specified search key. *)

  val old: key
  (** Messages that do not have the [`Recent] {!flag} set.  This is
      functionally equivalent to [`Not `Recent] (as opposed to [`Not `New]). *)

  val on: date -> key
  (** Messages whose internal date (disregarding time and timezone) is within
      the specified date.  *)

  val (||): key -> key -> key
  (** Messages that match either search key. *)

  val recent: key
  (** Messages that have the [`Recent] {!flag} set. *)

  val seen: key
  (** Messages that have the [`Seen] {!flag} set. *)

  val sent_before: date -> key
  (** Messages whose "Date:" header (disregarding time and timezone) is earlier
      than the specified date. *)

  val sent_on: date -> key
  (** Messages whose "Date:" header (disregarding time and timezone) is within
      the specified date. *)

  val sent_since: date -> key
  (** Messages whose "Date:" header (disregarding time and timezone) is within
      or later than the specified date.  *)

  val since: date -> key
  (** Messages whose internal date (disregarding time and timezone) is within or
      later than the specified date.  *)

  val smaller: int -> key
  (** Messages with a size smaller than the specified number of octets. *)

  val subject: string -> key
  (** Messages that contain the specified string in the envelope structure's
      "SUBJECT" field. *)

  val text: string -> key
  (** Messages that contain the specified string in the header or body of the
      message. *)

  val to_: string -> key
  (** Messages that contain the specified string in the envelope structure's
      "TO" field. *)

  val uid: eset -> key
  (** Messages with unique identifiers corresponding to the specified unique
      identifier set.  Sequence set ranges are permitted. *)

  val unanswered: key
  (** Messages that do not have the [`Answered] {!flag} set. *)

  val undeleted: key
  (** Messages that do not have the [`Deleted] {!flag} set. *)

  val undraft: key
  (** Messages that do not have the [`Draft] {!flag} set. *)

  val unflagged: key
  (** Messages that do not have the [`Flagged] {!flag} set. *)

  val unkeyword: string -> key
  (** Messages that do not have the specified keyword {!flag} set. *)

  val unseen: key
  (** Messages that do not have the [`Seen] {!flag} set. *)

  val (&&): key -> key -> key
  (** Messages that satisfy both search criteria. *)

  val modseq: Modseq.t -> key
  (** Messages that have equal or greater modification sequence numbers. *)

  val x_gm_raw: string -> key
  (** TOOD *)

  val x_gm_msgid: uint64 -> key
  (** Messages with a given Gmail Message ID. *)

  val x_gm_thrid: uint64 -> key
  (** Messages with a given Gmail Thread ID. *)

  val x_gm_labels: string list -> key
  (** Messages with given Gmail labels. *)
end

module Auth : sig
  (** {1 Authenticators}

      These are used to implement SASL authentication. SASL authentication is
      initiated by the {!authenticate} command and typically would occur right
      after receiving the server greeting.

      The authentication protocol exchange consists of a series of server
      challenges and client responses that are specific to the authentication
      mechanism.  If [a] is the authenticator being used, [a.step] will be called
      with each of the server's challenges.  The return value of [a.step] can
      signal an error or give the corresponding response.

      [step] functions do {e not} have to perform base64-encoding and decoding, as
      this is handled automatically by the library.

      The implementation of particular SASL authenticaton methods is outside the
      scope of this library and should be provided independently. Only [PLAIN] and
      [XOAUTH2] are provided as way of example. *)

  type authenticator =
    { name : string;
      step : string -> [ `Ok of string | `Error of string ] }

  val plain : string -> string -> authenticator
  (** [plain user pass] authenticates via [PLAIN] mechanism using username [user]
      and password [pass]. *)

  val xoauth2 : string -> string -> authenticator
  (** [xoauth2 user token] authenticates via [XOAUTH2] mechanishm user username
      [user] and access token [token].  The access token should be obtained
      independently. *)
end

(** {1:commands Commands}

    These are the available commands that can be sent to an IMAP server.  Not
    all commands will be supported in every IMAP server as they will depend on
    certain capabilities being enabled.  A command is executed using {!run}.
    It is a programmer error to try to run more than one command at the same
    time.  If this is required, more than one connection should be opened to the
    server.

    Some commands have a [?uid] optional argument.  When [true] it means to use
    the variant of the command that uses UIDs (instead of sequence numbers).
    See for example {!copy}, {!search}, {!fetch} and {!store_add_flags}. *)

type 'a command
(** The type of client commands.  They are executed using {!run}. *)

val login: string -> string -> unit command
(** [login user pass] identifies the client to the server and carries the
    plaintext password authenticating this [user] with password [pass].  A
    server MAY include a [`Capability] response {{!code}code} in the tagged
    [`Ok] response to a successful [login] command in order to send capabilities
    automatically. *)

val capability: Capability.capability list command
(** [capability] returns the list of capabilities supported by the server.  The
    server must send a single untagged [`Capability] {{!untagged}response}
    with "IMAP4rev1" as one of the listed capabilities before the (tagged) [`Ok]
    response.  See the type describing the possible
    {{!capability}capabilities}. *)

val create: string -> unit command
(** [create m] creates a mailbox named [m].  An [`Ok] response is returned only
    if a new mailbox with that name has been created.  It is an error to attempt
    to create "INBOX" or a mailbox with a name that refers to an existent mailbox.
    Any error in creation will return a tagged [`No] response. *)

val delete: string -> unit command
(** [delete m] deletes a mailbox named [m].  An [`Ok] response is returned only
    if the mailbox with that name has been deleted.
    Any error in deletion will return a tagged [`No] response. *)

val rename: string -> string -> unit command
(** [rename oldname newname] command changes the name of a mailbox from
    [oldname] to [newname].  A tagged [`Ok] response is returned only if the
    mailbox has been renamed.  It is an error to attempt to rename from a
    mailbox name that does not exist or to a mailbox name that already exists.
    Any error in renaming will return a tagged [`No] response. *)

val logout: unit command
(** [logout] gracefully terminates a session.  The server MUST send an untagged
    [`Bye] {{!untagged}response} before the (tagged) [`Ok] response. *)

val noop: unit command
(** [noop] does nothing.  Since any command can return a status update as
    untagged data, the [noop] command can be used as a periodic poll for new
    messages or message status updates during a period of inactivity (this is
    the preferred method to do this). *)

val subscribe: string -> unit command
(** [subscribe m] adds the mailbox [m] to the server's set of "active" or
    "subscribed" mailboxes as returned by the {!lsub} command. *)

val unsubscribe: string -> unit command
(** [unsubcribe m] removes the mailbox [m] from the server's set of "active" or
    "subscribed" mailboxes as returned by the {!lsub} command. *)

val list: ?ref:string -> string -> (Mailbox.mbx_flag list * char option * string) list command
(** [list ref m] returns a subset of names from the complete set of all names
    available to the client.  Zero or more untagged [`List]
    {{!untagged}replies} are returned, containing the name attributes,
    hierarchy delimiter.  The optional argument [ref] is the name of a mailbox
    or a level of mailbox hierarchy, and indicates the context in which the
    mailbox name is interpreted.*)

val lsub: ?ref:string -> string -> (Mailbox.mbx_flag list * char option * string) list command
(** [lsub ref m] is identical to {!list}, except that it returns a subset of
    names from the set of names that the user has declared as being "active" or
    "subscribed". *)

val status: string -> Mailbox.Request.t list -> (string * Mailbox.status_response list) command
(** [status] requests {{!status_query}status information} of the indicated
    mailbox.  An untagged [`Status] {{!untagged}response} is returned with
    the requested information. *)

val copy: ?uid:bool -> eset -> string -> unit command
(** [copy uid set m] copies the messages in [set] to the end of the specified
    mailbox [m].  [set] is understood as a set of message UIDs if [uid] is
    [true] (the default) or sequence numbers if [uid] is [false]. *)

val check: unit command
(** [check] requests a checkpoint of the currently selected mailbox.  A
    checkpoint refers to any implementation-dependent housekeeping associated
    with the mailbox. *)

val close: unit command
(** [close] permanently removes all messages that have the [`Deleted]
    {!flag} set from the currently selected mailbox, and returns to
    the authenticated state from the selected state. *)

val expunge: uint32 list command
(** [expunge] permanently removes all messages that have the [`Deleted]
    {!flag} set from the currently selected mailbox.  Before
    returning an [`Ok] to the client, an untagged [`Expunge]
    {{!untagged}response} is sent for each message that is removed. *)

val uid_search: Search.key -> (UID.t list * Modseq.t option) command
val search: Search.key -> (Seq.t list * Modseq.t option) command
(** [search uid sk] searches the mailbox for messages that match the given
    searching criteria.  If [uid] is [true] (the default), then the matching
    messages' unique identification numbers are returned.  Otherwise, their
    sequence numbers are.  The untagged [`Search] {{!untagged}response}
    from the server contains a listing of message numbers corresponding to those
    messages that match the searching criteria. *)

type _ condstore_flag =
  | Condstore: uint64 condstore_flag
  | No_condstore: unit condstore_flag

val select: 'a condstore_flag -> string -> 'a command
(** [select condstore m] selects the mailbox [m] so that its messages can be
    accessed.  If [condstore] (default value [false]) is [true], then the server
    will return the [`Modseq] data item in all subsequent untagged [`Fetch]
    {{!untagged}responses}. *)

val examine: 'a condstore_flag -> string -> 'a command
(** [examine condstore m] is identical to [select condstore m] and returns the
    same output; however, the selected mailbox is identified as read-only. *)

val append: string -> ?flags:Msg.flag list -> string -> unit command
(** [append m flags id data] appends [data] as a new message to the end of the
    mailbox [m].  This argument should be in the format of an [RFC-2822]
    message.

    If a flag list is specified, the flags should be set in the resulting
    message; otherwise, the flag list of the resulting message is set to empty
    by default.  In either case, the [`Recent] flag is also set. *)

(** {2 Fetch commands}

    The IMAP [FETCH] command is used to retrieve the data associated to a
    message (or a set of messages).  Messages are identified either by their
    sequence number (i.e., their position in the mailbox), or by their unique
    identification number (UID).

    For those servers that support the [CONDSTORE] extension, one can pass a
    mod-sequence value [?changed] to restrict the set of affected messages to
    those that are not yet known by the client.  One can further use the
    [?vanished] argument to learn of recently expunged messages.  It is a
    programmer error to set [?vanished] to [true] but not to pass a value for
    [?changed]. *)

val fetch: 'a uid_or_seq -> ?changed:uint64 -> ?vanished:bool -> eset -> Msg.Request.t list -> ('a * Msg.msg_att list) list command
(** [fetch uid changed vanished set att] retrieves data associated with the
    message set [set] in the current mailbox.  [set] is interpeted as being a
    set of UIDs or sequence numbers depending on whether [uid] is [true] (the
    default) or [false].  Specifying a [?changed] argument will further reduce
    the set of returned messages to those whose [CHANGEDSINCE] mod-sequence
    value is at least the passed value (requires the [CONDSTORE] extension).
    The [vanished] optional parameter specifies whether one wants to receive
    [`Vanished] responses as well. *)

val fetch_all: 'a uid_or_seq -> ?changed:uint64 -> ?vanished:bool -> eset -> ('a * Msg.msg_att list) list command
(** [fetch_all uid changed vanished set] is equivalent to [fetch uid changed vanished set a], where
    [a = [`Flags; `Internal_date; `Rfc822_size; `Envelope]]. *)

val fetch_fast: 'a uid_or_seq -> ?changed:uint64 -> ?vanished:bool -> eset -> ('a * Msg.msg_att list) list command
(** [fetch_fast uid changed vanished set] is equivalent to [fetch uid changed vanished set a], where
    [a = [`Flags; `Internal_date; `Rfc822_size]]. *)

val fetch_full: 'a uid_or_seq -> ?changed:uint64 -> ?vanished:bool -> eset -> ('a * Msg.msg_att list) list command
(** [fetch_full u c v s] is equivalent to [fetch u c v s a] where
    [a = [`Flags; `Internal_date; `Rfc822_size; `Envelope; `Body]]. *)

(** {2 Store commands} *)

val store_add_flags: 'a uid_or_seq -> ?silent:bool -> ?unchanged:uint64 -> eset -> Msg.flag list -> ('a * Msg.msg_att list) list command
(** [store_add_flags uid silent unchanged set flags] adds flags [flags] to the
    message set [set].  [set] is interpreter as being a set of UIDs or sequence
    numbers depending on whether [uid] is [true] (the default) or [false].  The
    server will return the updated flags for the affected messages in untagged
    [`Fetch] {{!untagged}responses} depending on whether [silent] is [true] (the
    default) or [false].  Specifying a [?unchanged] argument will further reduce
    the set of affected messages to those whose [UNCHANGEDSINCE] mod-sequence
    value is at least the passed value (requires the [CONDSTORE] extension). *)

val store_set_flags: 'a uid_or_seq -> ?silent:bool -> ?unchanged:uint64 -> eset -> Msg.flag list -> ('a * Msg.msg_att list) list command
(** [store_set_flags] is like {!store_add_flags} but replaces the set of flags
    instead of adding to it. *)

val store_remove_flags: 'a uid_or_seq -> ?silent:bool -> ?unchanged:uint64 -> eset -> Msg.flag list -> ('a * Msg.msg_att list) list command
(** [store_remove_flags] is like {!store_add_flags} but removes flags instead of
    adding them. *)

val store_add_labels: 'a uid_or_seq -> ?silent:bool -> ?unchanged:uint64 -> eset -> string list -> ('a * Msg.msg_att list) list command
(** [store_add_labels] is like {!store_add_flags} but adds
    {{:https://developers.google.com/gmail/imap_extensions}Gmail} {e labels}
    instead of regular flags. *)

val store_set_labels: 'a uid_or_seq -> ?silent:bool -> ?unchanged:uint64 -> eset -> string list -> ('a * Msg.msg_att list) list command
(** [store_set_labels] is like {!store_add_labels} but replaces the set of
    labels instead of adding to it. *)

val store_remove_labels: 'a uid_or_seq -> ?silent:bool -> ?unchanged:uint64 -> eset -> string list -> ('a * Msg.msg_att list) list command
(** [store_remove_labels] is like {!store_add_labels} but removes labels instead
    of adding them. *)

val enable: Capability.capability list -> Capability.capability list command

val authenticate: Auth.authenticator -> unit command
(** [authenticate a] indicates a [SASL] authentication mechanism to the server.
    If the server supports the requested authentication mechanism, it performs
    an authentication protocol exchange to authenticate and identify the client.
    See {!authenticator} for details on the interface with particular [SASL]
    mechanisms. *)

val idle: unit -> unit command * (unit -> unit)
(** [idle ()] is a pair [(c, stop)].  [c] starts an IDLE command.  When this
    command is executing the client will receive a stream of incoming untagged
    {{!untagged}responses} until [IDLE] ends.  IDLE can end by server decision
    of can be stopped by the client by forcing [stop].  If [stop] is forced
    after [IDLE] ended, then it is a no-op.

    See the relevent {{:https://tools.ietf.org/html/rfc2177}RFC} and the
    {{!ex}examples} for more details. *)

(** {1 Running commands and receiving responses} *)

module Error : sig
  type error =
    | Incorrect_tag of string * string
    (** The server response tag does not have a matching message tag.  The
        connection should be closed. *)

    | Decode_error of string * int
    (** Decoding error. It contains the reason, the curren tinput buffer and the
        current position.  The connection should be closed after this.  In some
        cases it might be possible to continue fater a decoding error, but this is
        not yet implemented. *)

    | Unexpected_cont
    (** A continuation request '+' is received from the server at an unexpected
        time.  The connection should be closed after seeing this error, as there is no
        safe way to continue. *)

    | Bad_greeting
    (** The server did not send a valid greeting message.  The connection should
        be closed. *)

    | Auth_error of string
    (** An client-side SASL authentication error ocurred.  This error can only
        appear when using the SASL-based {!authenticate} command.  The error is
        communicated to the server and the server responds with a [BAD] response.
        Thus, after receiving this error the client should pass [`Await] to {!run}
        until [`Error `Bad] is received, and then take appropiate action. *)

    | Bad of Code.code * string
    (** The server could not parse the request. *)

    | No of Code.code * string
    (** The server could not perform the requested action. *)

  val pp: Format.formatter -> error -> unit
end

(** {3 Connections}

    {{!connection}Connections} manage the encoder and decoder states and keeps
    track of message tags. *)

type 'a progress

type 'a result =
  | Ok of 'a
  | Error of Error.error
  | Send of string * 'a progress
  | Refill of 'a progress

val continue: 'a progress -> 'a result
val feed: 'a progress -> string -> int -> int -> 'a result

type state
(** The type for connections. *)

val client: state

(** [connection ()] creates a new connection object.  The connection should be
    supplied with input and output buffers as necessary using {!src} and {!dst}.
    Other that that, it is completely independent of any particular connection
    and/or IO mechanism.

    After creation, the connection should be run with [`Await] until [`Ok] to
    process the server greeting and start issuing commands.

    See the {{!ex}examples}. *)

(** {3 I/O interface}

    The following functions are used to provide input and/or output buffers to
    {!run} when it returns [`Await_src] and/or [`Await_dst]. *)

(** {3 Executing commands}

    The {!run} function encodes the connection state machine.  A connection [c]
    can be in three states: {e awaiting the server greeting}, {e executing a
    command}, or {e awaiting commands}.

    After {{!val:connection}creation}, [c] is {e awaiting the server greeting.}
    {!run} must be passed [`Await] until getting back [`Ok] which signals that
    the greeting has been received and [c] is now {e awaiting commands.}  A
    command can then be initiated by passing [`Cmd].  After a command is
    initiated, [c] will be {e executing a command} and {!run} will return a
    sequence of [`Await_src] and/or [`Await_dst] values interspaced with
    [`Untagged] values, and finally ending with [`Ok].  The client should call
    {!run} with [`Await] to step through this process (possibly after providing
    more input and output storage using {!src} and {!dst}).

    After returning [`Ok], [c] is again {e awaiting commands} and the process can be
    reinitiated.

    It is an error to execute a command if [c] is not {e awaiting commands.}

    See the {{!ex}examples.} *)

val run: state -> 'a command -> 'a progress
(** [run c v] performs [v] on the connection [c].  The meaning of the different values
    of [v] is:
    {ul
    {- [`Cmd c]: execute the {!command} [c].}
    {- [`Await]: perform periodic IO processing to complete the execution of currently
       executing command.}}

    The value of [run c v] is:
    {ul
    {- [`Untagged u] if an untagged {{!untagged}response} has been received from the
       server.}
    {- [`Ok] if no command is in progress and the server is ready to receive a new
       command from the client.}
    {- [`Error e] if an error ocurred during the processing of the current command.
       If the {!error} is not fatal, then the client can continue using this connection.
       Otherwise, the connection should be discarded immediately.}
    {- [`Await_src] if the connection is awaiting for more input.  The client must use
       {!src} to provide a new buffer and then call {!run} with [`Await].}
    {- [`Await_dst] if the connection needs more output storage.  The client must use
       {!dst} to provide a new buffer and then call {!run} with [`Await].}} *)

(** {1:limitations Supported extensions and limitations}

    The following extensions are supported:
    - {{:https://tools.ietf.org/html/rfc4551}CONDSTORE}
    - {{:https://tools.ietf.org/html/rfc5162}QRESYNC}
    - {{:http://tools.ietf.org/html/rfc5161}ENABLE}
    - {{:https://tools.ietf.org/html/rfc4315}UIDPLUS}
    - {{:https://tools.ietf.org/html/rfc6154}SPECIAL-USE}
    - {{:https://tools.ietf.org/html/rfc2177}IDLE}
    - {{:https://developers.google.com/gmail/imap_extensions}X-GM-EXT-1}

    {3 Limitations}

    Error handling is very simplistic.  In particular there is no error recovery
    from simple parsing errors.  Some form of this could be added if
    required. *)

(** {1:ex Example: checking for new mail}

    [wait_mail host port user pass mbox] logs into the IMAP server
    [host] on port [port] over {{:https://github.com/savonet/ocaml-ssl}SSL},
    authenticates user [user] with password [pass] and watches mailbox [mbox] until a
    new message arrives.  When a new message arrives, it outputs the sender's name
    and address and stops.

    See the
    {{:https://github.com/nojb/ocaml-imap/blob/master/test/wait_mail.ml}wait_mail.ml}
    file for a more complete version of this example.

    Setting [debug_flag := true] will output all the data exchanged with the
    server which can be quite instructive.

{[
let debug_flag = ref false
]}

    The function [run] takes care of managing I/O.

{[
  let run sock i o c v : [ `Untagged of Imap.untagged | `Ok ] =
    let rec write_fully s off len =
      if len > 0 then
        let rc = Ssl.write sock s off len in
        write_fully s (off + rc) (len - rc)
    in
    let rec loop = function
      | `Await_src ->
          let rc = Ssl.read sock i 0 (Bytes.length i) in
          if !debug_flag then Format.eprintf ">>> %d\n%s>>>\n%!" rc (String.sub i 0 rc);
          Imap.src c i 0 rc;
          loop (Imap.run c `Await)
      | `Await_dst ->
          let rc = Bytes.length o - Imap.dst_rem c in
          write_fully o 0 rc;
          if !debug_flag then Format.eprintf "<<< %d\n%s<<<\n%!" rc (String.sub o 0 rc);
          Imap.dst c o 0 (Bytes.length o);
          loop (Imap.run c `Await)
      | `Untagged _ as r -> r
      | `Ok _ -> `Ok
      | `Error e ->
          Format.eprintf "@[IMAP Error: %a@]@." Imap.pp_error e;
          failwith "imap error"
    in
    loop (Imap.run c v)
]}

    In order to detect new messages, we record the next UID value of the
    [INBOX] mailbox as soon as we open it.  Then we wait for the server to
    alert us of activity in this mailbox using the {{!run}idle} command.  And we look
    for messages with UIDs larger than this one using the {!search} command.

{[
  let () = Ssl.init ()

  let wait_mail host port user pass mbox =
    let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let he = Unix.gethostbyname host in
    Unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port));
    let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
    let sock = Ssl.embed_socket fd ctx in
    Ssl.connect sock;
    let c = Imap.connection () in
    let i = Bytes.create io_buffer_size in
    let o = Bytes.create io_buffer_size in
    Imap.dst c o 0 (Bytes.length o);
    match run sock i o c `Await with
    | `Ok ->
        let rec logout = function
          | `Untagged _ -> logout (run sock i o c `Await)
          | `Ok -> ()
        in
        let rec idle stop uidn = function
          | `Untagged (`Exists _) -> Lazy.force stop; idle stop uidn (run sock i o c `Await)
          | `Untagged _ -> idle stop uidn (run sock i o c `Await)
          | `Ok ->
              search uidn Uint32.zero
                (run sock i o c (`Cmd (Imap.search ~uid:true (`Uid [uidn, None]))))
        and search uidn n = function
          | `Untagged (`Search (n :: _, _)) -> search uidn n (run sock i o c `Await)
          | `Untagged _ -> search uidn n (run sock i o c `Await)
          | `Ok ->
              if n = Uint32.zero then
                let cmd, stop = Imap.idle () in
                idle stop uidn (run sock i o c (`Cmd cmd))
              else
              let cmd = Imap.fetch ~uid:true ~changed:Uint64.one [n, Some n] [`Envelope] in
              fetch uidn n None (run sock i o c (`Cmd cmd))
        and fetch uidn n name = function
          | `Untagged (`Fetch (_, att)) ->
              let name =
                List.fold_left
                  (fun name att -> match att with
                     | `Envelope e ->
                         begin match e.Imap.env_from with
                         | [] -> name
                         | ad :: _ ->
                             Some (Printf.sprintf "\"%s\" <%s@%s>"
                                     ad.Imap.ad_name ad.Imap.ad_mailbox ad.Imap.ad_host)
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
          | `Ok ->
              let cmd, stop = Imap.idle () in
              idle stop uidn (run sock i o c (`Cmd cmd))
        in
        let rec login = function
          | `Untagged _ -> login (run sock i o c `Await)
          | `Ok -> select Uint32.zero (run sock i o c (`Cmd (Imap.examine mbox)))
        in
        login (run sock i o c (`Cmd (Imap.login user pass)))
    | `Untagged _ -> assert false
]}
*)
