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

    [Imap] is a Lwt-compatible client library for the
    {{:https://tools.ietf.org/html/rfc3501}IMAP4} protocol, together with some
    {{!section:limitations}extensions}.

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
(** Modification sequence numbers. *)

module Uid : NUMBER
(** Message unique identification numbers. *)

module Seq : NUMBER
(** Message sequence numbers. *)

type _ num_kind =
  | Uid : Uid.t num_kind
  | Seq : Seq.t num_kind

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
    command, in status {{!Code.code}codes} and can be enabled by the {!enable} command. *)
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

    Returned when fetching the {{!Fetch.envelope}envelope message attribute}
    using the {!fetch} command. *)

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
      retrieved when using {!fetch} with {!body_section}.  See
      {{:https://tools.ietf.org/html/rfc3501#section-6.4.5}RFC 3501 6.4.5} for
      more details.

      For more on RFC 2822 headers, see
      {{:https://tools.ietf.org/html/rfc2822#section-2.2}RFC 2822, 2.2}.  For more
      on MIME headers, see {{:https://tools.ietf.org/html/rfc2045#section-3}RFC
      2045, 3}. *)
  type section =
    | HEADER                             (* Full RFC 2822 message headers *)
    | HEADER_FIELDS of string list       (* Header fields matching one of the given field names *)
    | HEADER_FIELDS_NOT of string list   (* Header fields not matching the given field names *)
    | TEXT                               (* The text body of this part, omitting RFC2822 headers *)
    | MIME                               (* The MIME headers of this part *)
    | Part of int * section              (* Subpart *)
    | All [@@deriving sexp]              (* The whole message *)

  type t
  (** Message attributes that can be requested using the {!fetch} command. *)

  val envelope: t
  (** The envelope structure of the message. *)

  val internaldate: t
  (** The internal date of the message. *)

  val rfc822_header: t
  (** Equivalent to [body_section ~peek:true HEADER], but returning a
      rfc822_header response. *)

  val rfc822_text: t
  (** Equivalent to [body_section ~peek:false TEXT], but returning a
      rfc822_text response. *)

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
      form that does not implicitly set the [Seen] {!flag}. *)

  val bodystructure: t
  (** The MIME body structure of the message.  This is computed by the server
      by parsing the MIME header fields in the [RFC-2822] header and MIME
      headers. *)

  val uid: t
  (** The unique identifier for the message. *)

  val flags: t
  (** The {{!flag}flags} that are set for this message. *)

  val all: t list
  (** Equivalent to [[flags; internaldate; rfc822_size; envelope]]. *)

  val fast: t list
  (** Equivalent to [[flags; internaldate; rfc822_size]]. *)

  val full: t list
  (** Equivalent to [[flags; internaldate; rfc822_size; envelope; body]]. *)

  val x_gm_msgid: t
  val x_gm_thrid: t
  val x_gm_labels: t

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

  (** Mailbox status items returned by {!status} command. *)
  type response =
    {
      messages: int option;
      recent: int option;
      uidnext: Uid.t option;
      uidvalidity: Uid.t option;
      unseen: int option;
      highestmodseq: Modseq.t option;
    } [@@deriving sexp]
end

(** Message search *)

module Search : sig
  type key [@@deriving sexp]

  val all: key
  (** All messages in the mailbox. *)

  val seq: Seq.t list -> key
  (** Messages with message sequence number in the given set. *)

  val answered: key
  (** Messages with the [Answered] flag set. *)

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
  (** Messages with the [Deleted] {!flag} set. *)

  val draft: key
  (** Messages with the [Draft] {!flag} set. *)

  val flagged: key
  (** Messages with the [Flagged] {!flag} set. *)

  val from: string -> key
  (** Messages with FROM field containing given string. *)

  val header: string -> string -> key
  (** Messages with headers with the specified field-name and that
      contains the specified string in the text of the header (what comes after
      the colon). *)

  val keyword: string -> key
  (** Messages with the specified [Keyword] {!flag} set. *)

  val larger: int -> key
  (** Messages with size at least the given number of bytes. *)

  val new_: key
  (** Messages that have the [Recent] {!flag} set but not the [Seen] {!flag}. *)

  val not: key -> key
  (** Negation of search criteria. *)

  val old: key
  (** Messages that do not have the [Recent] {!flag} set. *)

  val on: date -> key
  (** Messages whose internal date (disregarding time and timezone) is within
      the specified date.  *)

  val (||): key -> key -> key
  (** OR of search criteria. *)

  val recent: key
  (** Messages that have the [Recent] {!flag} set. *)

  val seen: key
  (** Messages that have the [Seen] {!flag} set. *)

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

  val uid: Uid.t list -> key
  (** Messages with UID in the given set. *)

  val unanswered: key
  (** Messages that do not have the [Answered] {!flag} set. *)

  val undeleted: key
  (** Messages that do not have the [Deleted] {!flag} set. *)

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
  (** Gmail search string *)

  val x_gm_msgid: Modseq.t -> key
  (** Messages with a given Gmail Message ID. *)

  val x_gm_thrid: Modseq.t -> key
  (** Messages with a given Gmail Thread ID. *)

  val x_gm_labels: string list -> key
  (** Messages with given Gmail labels. *)
end

(** {3 Connections} *)

type error =
  | Incorrect_tag of string * string (** An unknown response tag was received. *)
  | Decode_error of string * string list * string (** Decoding error. *)
  | Unexpected_cont (** A continuation request '+' is received at an unexpected time. *)
  | Bad_greeting (** The server did not send a valid greeting message. *)
  | Auth_error of string (** A SASL authentication error ocurred. *)
  | No of string (** Server replied NO. *)
  | Bad of string (** Server replied BAD. *)

type t
(** The type for connections. *)

val uidnext: t -> Uid.t option

val messages: t -> int option

val recent: t -> int option

val unseen: t -> int option

val uidvalidity: t -> Uid.t option

val connect: string -> string -> string -> string -> t Lwt.t
(** [connect server username password mailbox]. *)

val disconnect: t -> unit Lwt.t
(** Disconnect. *)

(** {1 Commands} *)

val poll: t -> unit Lwt.t

val stop_poll: t -> unit

val create: t -> string -> unit Lwt.t
(** [create imap name] creates a mailbox named [name]. *)

val delete: t -> string -> unit Lwt.t
(** [delete imap name] deletes mailbox [name]. *)

val rename: t -> string -> string -> unit Lwt.t
(** [rename imap oldname newname] renames mailbox [oldname] to [newname]. *)

val noop: t -> unit Lwt.t
(** [noop imap] does nothing.  Since any command can return a status update as
    untagged data, the [noop] command can be used as a periodic poll for new
    messages or message status updates during a period of inactivity. *)

val list: t -> ?ref:string -> string -> (MbxFlag.mbx_flag list * char option * string) list Lwt.t
(** [list imap ref m] returns the list of mailboxes with names matching
    [ref]. *)

val status: t -> string -> Status.mbx_att_request list -> Status.response Lwt.t
(** [status imap mbox items] requests status [items] for mailbox [mbox]. *)

val copy: t -> 'a num_kind -> 'a list -> string -> unit Lwt.t
(** [copy imap nums mbox] copies messages with sequence number in [nums] to
    mailbox [mbox]. *)

val expunge: t -> Seq.t list Lwt.t
(** [expunge imap] permanently removes all messages that have the [Deleted]
    {!flag} set from the currently selected mailbox. *)

val search: t -> 'a num_kind -> Search.key -> ('a list * Modseq.t option) Lwt.t
(** [uid_search imap key] returns the set of UIDs of messages satisfying the
    criteria [key]. *)

val select: t -> ?read_only:bool -> string -> unit Lwt.t
(** [select imap m] selects the mailbox [m] for access. *)

val condstore_select: t -> ?read_only:bool -> string -> Modseq.t Lwt.t
(** Like {!select}, but returns the modification sequence number in all
    subsequent fetch requests. *)

val append: t -> string -> ?flags:Flag.flag list -> string -> unit Lwt.t
(** [append imap mbox flags id data] appends [data] as a new message to the end
    of the mailbox [mbox]. An optional flag list can be passed using the [flags]
    argument. *)

val fetch: t -> ?changed_since:Modseq.t -> 'a num_kind -> 'a list -> Fetch.t list -> Fetch.response Lwt.t
(** [fetch imap uid ?changed_since set att] retrieves data associated with
    messages with sequence number in [set].

    If the [?changed_since] argument is passed, only those messages with
    [CHANGEDSINCE] mod-sequence value at least the passed value are affected
    (requires the [CONDSTORE] extension). *)

type store_mode =
  [ `Add
  | `Remove
  | `Set ]

type store_kind =
  [ `Flags of Flag.flag list
  | `Labels of string list ]

val store: t -> ?unchanged_since:Modseq.t -> store_mode -> 'a num_kind -> 'a list -> store_kind -> Fetch.response Lwt.t
(** [store imap ?unchanged_since mode nums kind] modifies [kind] according to
    [mode] for those message with sequence number in [nums].

    If [?unchanged_since] is present, then only those messages with [UNCHANGEDSINCE]
    mod-sequence value at least the passed value are affected. *)
