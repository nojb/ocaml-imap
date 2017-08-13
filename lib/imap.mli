(* The MIT License (MIT)

   Copyright (c) 2015-2017 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

    {1:limitations Supported extensions and limitations}

    The following extensions are supported:
    - {{:https://tools.ietf.org/html/rfc4551}CONDSTORE}
    - {{:https://tools.ietf.org/html/rfc5162}QRESYNC}
    - {{:http://tools.ietf.org/html/rfc5161}ENABLE}
    - {{:https://tools.ietf.org/html/rfc4315}UIDPLUS}
    - {{:https://tools.ietf.org/html/rfc6154}SPECIAL-USE}
    - {{:https://tools.ietf.org/html/rfc2177}IDLE}
    - {{:https://developers.google.com/gmail/imap_extensions}X-GM-EXT-1} *)

(** {1 Common types for IMAP} *)

module type NUMBER = sig
  type t

  val zero: t
  val of_int: int -> t
  val compare: t -> t -> int
end

module Modseq : NUMBER
(** Unsigned 64-bit integers are used for: mod-sequence numbers, Gmail message
    and thread IDs. *)

module Uid : NUMBER
module Seq : NUMBER
(** Unsigned 32-bit integers are used for: message sequence numbers, unique
    identification numbers (UIDs). *)

module type NUMBER_SET = sig
  type elt
  type t [@@deriving sexp]
  val empty: t
  val singleton: elt -> t
  val union: t -> t -> t
  val add: elt -> t -> t
  val interval: elt -> elt -> t
  val of_list: elt list -> t
end

module SeqSet : NUMBER_SET with type elt := Seq.t
module UidSet : NUMBER_SET with type elt := Uid.t

type date =
  {
    day: int;
    month: int;
    year: int;
  } [@@deriving sexp]

type time =
  {
    hours: int;
    minutes: int;
    seconds: int;
    zone: int;
  } [@@deriving sexp]

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
  | LITERALMINUS
  | UTF8_ACCEPT
  | UTF8_ONLY
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
  | OTHER of string [@@deriving sexp]

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
  } [@@deriving sexp]

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
  } [@@deriving sexp]

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
    {
      fld_params: (string * string) list;      (* Part parameters *)
      fld_id: string option;                   (* Optional part ID *)
      fld_desc: string option;                 (* Optional content description *)
      fld_enc: string;                         (* Content transfer encoding *)
      fld_octets: int;           (* Size in bytes *)
    } [@@deriving sexp]

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
    | Message of fields * envelope * mime * int
    | Basic of string * string * fields
    | Multipart of mime list * string [@@deriving sexp]
end

module Flag : sig
  type flag =
    | Answered
    | Flagged
    | Deleted
    | Seen
    | Draft
    | Keyword of string
    | Extension of string
    | Recent
    | Any [@@deriving sexp]
end

module Fetch : sig
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
    | All [@@deriving sexp]              (* The whole message *)

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

  val body_section: peek:bool -> section -> (int * int) option -> t
  (** The text of a particular body section.  The [peek] flag is an alternate
      form that does not implicitly set the [`Seen] {!flag}. *)

  val bodystructure: t
  (** The MIME body structure of the message.  This is computed by the server
      by parsing the MIME header fields in the [RFC-2822] header and MIME
      headers. *)

  val uid: t
  (** The unique identifier for the message. *)

  val flags: t
  (** The {{!flag}flags} that are set for this message. *)

  val all: t list
  val fast: t list
  val full: t list

  val x_gm_msgid: t
  val x_gm_thrid: t
  val x_gm_labels: t
  (** [fetch_all uid changed vanished set] is equivalent to [fetch uid changed vanished set a], where
      [a = [`Flags; `Internal_date; `Rfc822_size; `Envelope]]. *)

  (** [fetch_fast uid changed vanished set] is equivalent to [fetch uid changed vanished set a], where
      [a = [`Flags; `Internal_date; `Rfc822_size]]. *)

  (** [fetch_full u c v s] is equivalent to [fetch u c v s a] where
      [a = [`Flags; `Internal_date; `Rfc822_size; `Envelope; `Body]]. *)

  type response =
    {
      flags: Flag.flag list option;
      envelope: envelope option;
      internaldate: (date * time) option;
      rfc822: string option;
      rfc822_header: string option;
      rfc822_text: string option;
      rfc822_size: int option;
      body: MIME.mime option;
      bodystructure: MIME.mime option;
      body_section: (section * int option * string option) option;
      uid: Uid.t option;
      modseq: Modseq.t option;
      x_gm_msgid: Modseq.t option;
      x_gm_thrid: Modseq.t option;
      x_gm_labels: string list option;
    } [@@deriving sexp]
end

module MbxFlag : sig
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
    | Extension of string [@@deriving sexp]
end

(** {3 Mailbox status} *)

module Status : sig
  (** Mailbox attibutes that can be requested with the {!status} command. *)
  type mbx_att_request =
    | MESSAGES
    (** The number of messages in the mailbox. *)
    | RECENT
    (** The number of messages with the [`Recent] {!flag} set. *)
    | UIDNEXT
    (** The next unique identifier value of the mailbox. *)
    | UIDVALIDITY
    (** The unique identifier validity value of the mailbox. *)
    | UNSEEN
    (** The number of messages which do not have the [`Seen] {!flag} set. *)
    | HIGHESTMODSEQ [@@deriving sexp]
    (** TODO *)

  (** Mailbox status items returned in the untagged [`Status] {{!untagged}response}
      to the {!status} command. *)
  type response =
    {
      messages: int option;
      recent: int option;
      uidnext: Uid.t option;
      uidvalidity: Uid.t option;
      unseen: Seq.t option;
      highestmodseq: Modseq.t option;
    } [@@deriving sexp]
end

(** Message search *)

module Search : sig
  (** Search keys. See {!search} command. *)
  type key [@@deriving sexp]

  val all: key
  (** All messages in the mailbox; the default initial key for ANDing. *)

  val seq: SeqSet.t -> key
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

  val uid: UidSet.t -> key
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

  val x_gm_msgid: Modseq.t -> key
  (** Messages with a given Gmail Message ID. *)

  val x_gm_thrid: Modseq.t -> key
  (** Messages with a given Gmail Thread ID. *)

  val x_gm_labels: string list -> key
  (** Messages with given Gmail labels. *)
end

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
  <
    name: string;
    step: string -> (string, string) result;
  >

val plain: string -> string -> authenticator
(** [plain user pass] authenticates via [PLAIN] mechanism using username
    [user] and password [pass]. *)

val xoauth2: string -> string -> authenticator
(** [xoauth2 user token] authenticates via [XOAUTH2] mechanishm user username
    [user] and access token [token].  The access token should be obtained
    independently. *)

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

(** {3 Connections}

    {{!connection}Connections} manage the encoder and decoder states and keeps
    track of message tags. *)

type error =
  | Incorrect_tag of string * string (** An unknown response tag was received. *)
  | Decode_error of string * int
  (** Decoding error. It contains the reason, the curren tinput buffer and the
      current position.  The connection should be closed after this.  In some
      cases it might be possible to continue fater a decoding error, but this is
      not yet implemented. *)
  | Unexpected_cont (** A continuation request '+' is received at an unexpected time. *)
  | Bad_greeting (** The server did not send a valid greeting message. *)
  | Auth_error of string (** A SASL authentication error ocurred. *)
  | No of string
  | Bad of string

type t
(** The type for connections. *)

val connect: string -> string -> string -> string -> t Lwt.t
(** [connect server username password mailbox] *)

val capability: t -> capability list Lwt.t
(** [capability] returns the list of {{!capability}capabilities} supported by
    the server. *)

val create: t -> string -> unit Lwt.t
(** [create m] creates a mailbox named [m].  An [`Ok] response is returned only
    if a new mailbox with that name has been created.  It is an error to attempt
    to create "INBOX" or a mailbox with a name that refers to an existent mailbox.
    Any error in creation will return a tagged [`No] response. *)

val delete: t -> string -> unit Lwt.t
(** [delete m] deletes a mailbox named [m].  An [`Ok] response is returned only
    if the mailbox with that name has been deleted.
    Any error in deletion will return a tagged [`No] response. *)

val rename: t -> string -> string -> unit Lwt.t
(** [rename oldname newname] command changes the name of a mailbox from
    [oldname] to [newname].  A tagged [`Ok] response is returned only if the
    mailbox has been renamed.  It is an error to attempt to rename from a
    mailbox name that does not exist or to a mailbox name that already exists.
    Any error in renaming will return a tagged [`No] response. *)

val logout: t -> unit Lwt.t
(** [logout] gracefully terminates a session.  The server MUST send an untagged
    [`Bye] {{!untagged}response} before the (tagged) [`Ok] response. *)

val noop: t -> unit Lwt.t
(** [noop] does nothing.  Since any command can return a status update as
    untagged data, the [noop] command can be used as a periodic poll for new
    messages or message status updates during a period of inactivity. *)

val subscribe: t -> string -> unit Lwt.t
(** [subscribe m] adds the mailbox [m] to the server's set of "active" or
    "subscribed" mailboxes as returned by the {!lsub} command. *)

val unsubscribe: t -> string -> unit Lwt.t
(** [unsubcribe m] removes the mailbox [m] from the server's set of "active" or
    "subscribed" mailboxes as returned by the {!lsub} command. *)

val list: t -> ?ref:string -> string -> (MbxFlag.mbx_flag list * char option * string) list Lwt.t
(** [list ref m] returns a subset of names from the complete set of all names
    available to the client.  Zero or more untagged [`List]
    {{!untagged}replies} are returned, containing the name attributes,
    hierarchy delimiter.  The optional argument [ref] is the name of a mailbox
    or a level of mailbox hierarchy, and indicates the context in which the
    mailbox name is interpreted.*)

val lsub: t -> ?ref:string -> string -> (MbxFlag.mbx_flag list * char option * string) list Lwt.t
(** [lsub ref m] is identical to {!list}, except that it returns a subset of
    names from the set of names that the user has declared as being "active" or
    "subscribed". *)

val status: t -> string -> Status.mbx_att_request list -> Status.response Lwt.t
(** [status] requests {{!status_query}status information} of the indicated
    mailbox.  An untagged [`Status] {{!untagged}response} is returned with
    the requested information. *)

val copy: t -> SeqSet.t -> string -> unit Lwt.t
(** [copy uid set m] copies the messages with sequence number in [set] to the
    mailbox [m].  *)

val uid_copy: t -> UidSet.t -> string -> unit Lwt.t
(** Like {!copy}, but identifies messages by UID. *)

val check: t -> unit Lwt.t
(** [check] requests a checkpoint of the currently selected mailbox.  A
    checkpoint refers to any implementation-dependent housekeeping associated
    with the mailbox. *)

val close: t -> unit Lwt.t
(** [close] permanently removes all messages that have the [`Deleted]
    {!flag} set from the currently selected mailbox, and returns to
    the authenticated state from the selected state. *)

val expunge: t -> Seq.t list Lwt.t
(** [expunge] permanently removes all messages that have the [`Deleted]
    {!flag} set from the currently selected mailbox.  Before
    returning an [`Ok] to the client, an untagged [`Expunge]
    {{!untagged}response} is sent for each message that is removed. *)

val uid_search: t -> Search.key -> (Uid.t list * Modseq.t option) Lwt.t
(** [uid_search conn key] returns the set of UIDs of messages satisfying the
    criteria [key]. *)

val search: t -> Search.key -> (Seq.t list * Modseq.t option) Lwt.t
(** Like {!uid_search}, but the sequence numbers of matching messages is
    returned instead. *)

val select: t -> ?read_only:bool -> string -> unit Lwt.t
(** [select conn m] selects the mailbox [m] for access. *)

val condstore_select: t -> ?read_only:bool -> string -> Modseq.t Lwt.t
(** Like {!select}, but retruns the modification sequence number in all
    subsequent fetch requests. *)

val append: t -> string -> ?flags:Flag.flag list -> string -> unit Lwt.t
(** [append m flags id data] appends [data] as a new message to the end of the
    mailbox [m].  This argument should be in the format of an [RFC-2822]
    message.

    If a flag list is specified, the flags should be set in the resulting
    message; otherwise, the flag list of the resulting message is set to empty
    by default.  In either case, the [`Recent] flag is also set. *)

val fetch: t -> ?changed:Modseq.t -> ?vanished:bool -> SeqSet.t -> Fetch.t list -> Fetch.response Lwt.t
(** [fetch uid ?changed ?vanished set att] retrieves data associated with
    messages with sequence number in [set].

    If the [?changed] argument is passed, only those messages with
    [CHANGEDSINCE] mod-sequence value at least the passed value are affected
    (requires the [CONDSTORE] extension).

    The [vanished] optional parameter specifies whether one wants to receive
    [`Vanished] responses as well. *)

val uid_fetch: t -> ?changed:Modseq.t -> ?vanished:bool -> UidSet.t -> Fetch.t list -> Fetch.response Lwt.t
(** Like {!fetch}, but identifies messages by UID. *)

type store_mode =
  [`Add | `Remove | `Set]

type store_kind =
  [`Flags of Flag.flag list | `Labels of string list]

val store: t -> ?silent:bool -> ?unchanged:Modseq.t -> store_mode -> SeqSet.t -> store_kind -> Fetch.response Lwt.t
(** [store_add_flags uid ?silent ?unchanged set flags] adds flags [flags] to the
    messages with sequence number in [set].

    If [?silent] is present, the updated flags for the affected messages is
    returned.

    If [?unchanged] is present, then only those messages with [UNCHANGEDSINCE]
    mod-sequence value at least the passed value are affected. *)

val uid_store: t -> ?silent:bool -> ?unchanged:Modseq.t -> store_mode -> UidSet.t -> store_kind -> Fetch.response Lwt.t
(** Like {!add_flags}, but identifies messages by UID. *)

val enable: t -> capability list -> capability list Lwt.t
