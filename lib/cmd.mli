(* The MIT License (MIT)

   Copyright (c) 2015-2018 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

open Common
open Response

type state
(** The type of IMAP connections. Values of type [t] keep track of the implicit
    state in IMAP connections: number of unseen messages, total number of
    messages in the current mailbox, etc. Note that this state can be updated
    during the execution of almost any IMAP command. *)

val messages : state -> int option
(** Returns the number of messages in the selected mailbox. The server can
    update this count during most any interaction.

    This operation does not communicate with the server. It merely reports the
    result of previous communication. *)

val recent : state -> int option
(** Returns the number of "recent" messages in the currently selected mailbox,
    as most recently reported by the server. The server can update this count
    during most any interaction.

    This operation does not communicate with the server. It merely reports the
    result of previous communication. *)

val flags : state -> flag list option

val uidnext : state -> int32 option
(** Returns the predicted next uid for a message in the currently selected
    mailbox, as most recently reported by the server. The server can update this
    count during most any interaction. Old IMAP servers might not report this
    value, in which case the result is [None].

    This operation does not communicate with the server. It merely reports the
    result of previous communication. *)

val uidvalidity : state -> int32 option
(** Returns an id number that changes when all uids become invalid. The server
    cannot update this number during a session. Old IMAP servers might not
    report this value, in which case the result is [None].

    This operation does not communicate with the server. It merely reports the
    result of previous communication. *)

val unseen : state -> int option
(** Returns the number of "unseen" messages in the currently selected mailbox,
    as most recently reported by the server. The server can update this count
    during most any interaction. Old IMAP servers might not report this value,
    in which case the result is [None].

    This operation does not communicate with the server. It merely reports the
    result of previous communication. *)

val highestmodseq : state -> int64 option

type ('a, 'b) cmd
(** The type of IMAP commands which return a response of type ['a]. *)

val initial : state

type ('a, 'b) step =
  | Send of string * ('a, 'b) step
  | Wait of (response -> ('a, 'b) step)
  | Partial of state * 'a * ('a, 'b) step
  | Done of state * 'b
  | Error of string

val run : state -> ('a, 'b) cmd -> state * ('a, 'b) step

(** {2 IMAP Commands} *)

val login : string -> string -> (unit, unit) cmd

val logout : (unit, unit) cmd

(* val poll : unit -> (unit -> unit) * (unit, unit) cmd *)

val create : string -> (unit, unit) cmd
(** Creates mailbox. (It must not exist already.) *)

val delete : string -> (unit, unit) cmd
(** [delete imap name] deletes mailbox [name]. *)

val rename : string -> string -> (unit, unit) cmd
(** [rename imap oldname newname] renames mailbox [oldname] to [newname]. *)

val noop : (unit, unit) cmd
(** [noop imap] sends a "no-op" message to the server, typically to keep the
    session alive. As for many commands, the server may report message-state
    updates or expunges, which are recorded in [imap].  *)

val list :
  ?ref:string ->
  string ->
  (unit, (mailbox_flag list * char option * string) list) cmd
(** [list imap ref m] returns the list of mailboxes with names matching
    [ref]. *)

module Status : sig
  type 'a t

  val messages : int t
  (** Number of messages. *)

  val recent : int t
  (** Number of recent messages. *)

  val uidnext : int32 t
  (** Uid for next received message. *)

  val uidvalidity : int32 t
  (** Id that changes when uids are modified. *)

  val unseen : int t
  (** Number of unseen messages. *)

  val highestmodseq : int64 t

  val pair : 'a t -> 'b t -> ('a * 'b) t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

val status : string -> 'a Status.t -> (unit, 'a option) cmd
(** Requests information about a mailbox from the server, typically not the
    currently selected mailbox. *)

val copy : seq list -> string -> (unit, unit) cmd
(** Copies the specified messages from the currently selected mailbox to the
    specified mailbox.

    Pending expunges must be handled before calling this function; see
    imap-get-expunges. *)

val uid_copy : uid list -> string -> (unit, unit) cmd

val expunge : (unit, unit) cmd
(** Purges every message currently marked with the [Deleted] flag from the
    mailbox. *)

val uid_expunge : uid list -> (unit, unit) cmd
(** Purges the messages with the given uids which are {em also} marked with the
    [Deleted] flag from the mailbox. *)

module Search : sig
  type t
  (** Message search *)

  val all : t
  (** All messages in the mailbox. *)

  val seq : seq list -> t
  (** Messages with message sequence number in the given set. *)

  val answered : t
  (** Messages with the [Answered] flag set. *)

  val bcc : string -> t
  (** Messages that contain the specified string in the envelope structure's
    "BCC" field. *)

  val before : date -> t
  (** Messages whose internal date (disregarding time and timezone) is earlier
    than the specified date. *)

  val body : string -> t
  (** Messages that contain the specified string in the body of the message. *)

  val cc : string -> t
  (** Messages that contain the specified string in the envelope structure's
    "CC" field. *)

  val deleted : t
  (** Messages with the [Deleted] {!flag} set. *)

  val draft : t
  (** Messages with the [Draft] {!flag} set. *)

  val flagged : t
  (** Messages with the [Flagged] {!flag} set. *)

  val from : string -> t
  (** Messages with FROM field containing given string. *)

  val header : string -> string -> t
  (** Messages with headers with the specified field-name and that
    contains the specified string in the text of the header (what comes after
    the colon). *)

  val keyword : string -> t
  (** Messages with the specified [Keyword] {!flag} set. *)

  val larger : int -> t
  (** Messages with size at least the given number of bytes. *)

  val new_ : t
  (** Messages that have the [Recent] {!flag} set but not the [Seen] {!flag}. *)

  val not : t -> t
  (** Negation of search criteria. *)

  val old : t
  (** Messages that do not have the [Recent] {!flag} set. *)

  val on : date -> t
  (** Messages whose internal date (disregarding time and timezone) is within
      the specified date.  *)

  val ( || ) : t -> t -> t
  (** OR of search criteria. *)

  val recent : t
  (** Messages that have the [Recent] {!flag} set. *)

  val seen : t
  (** Messages that have the [Seen] {!flag} set. *)

  val sent_before : date -> t
  (** Messages whose "Date:" header (disregarding time and timezone) is earlier
      than the specified date. *)

  val sent_on : date -> t
  (** Messages whose "Date:" header (disregarding time and timezone) is within
      the specified date. *)

  val sent_since : date -> t
  (** Messages whose "Date:" header (disregarding time and timezone) is within
      or later than the specified date.  *)

  val since : date -> t
  (** Messages whose internal date (disregarding time and timezone) is within or
      later than the specified date.  *)

  val smaller : int -> t
  (** Messages with a size smaller than the specified number of octets. *)

  val subject : string -> t
  (** Messages that contain the specified string in the envelope structure's
      "SUBJECT" field. *)

  val text : string -> t
  (** Messages that contain the specified string in the header or body of the
      message. *)

  val to_ : string -> t
  (** Messages that contain the specified string in the envelope structure's
      "TO" field. *)

  val uid : uid list -> t
  (** Messages with UID in the given set. *)

  val unanswered : t
  (** Messages that do not have the [Answered] {!flag} set. *)

  val undeleted : t
  (** Messages that do not have the [Deleted] {!flag} set. *)

  val undraft : t
  (** Messages that do not have the [`Draft] {!flag} set. *)

  val unflagged : t
  (** Messages that do not have the [`Flagged] {!flag} set. *)

  val unkeyword : string -> t
  (** Messages that do not have the specified tword {!flag} set. *)

  val unseen : t
  (** Messages that do not have the [`Seen] {!flag} set. *)

  val ( && ) : t -> t -> t
  (** Messages that satisfy both search criteria. *)

  val modseq : modseq -> t
  (** Messages that have equal or greater modification sequence numbers. *)

  val x_gm_raw : string -> t
  (** Gmail search string *)

  val x_gm_msgid : modseq -> t
  (** Messages with a given Gmail Message ID. *)

  val x_gm_thrid : modseq -> t
  (** Messages with a given Gmail Thread ID. *)

  val x_gm_labels : string list -> t
  (** Messages with given Gmail labels. *)
end

val search : Search.t -> (unit, seq list * modseq option) cmd
(** [uid_search imap key] returns the set of UIDs of messages satisfying the
    criteria [key]. *)

val uid_search : Search.t -> (unit, uid list * modseq option) cmd

val examine : string -> (unit, unit) cmd
(** [select imap m] selects the mailbox [m] for access. *)

val select : string -> (unit, unit) cmd
(** [select imap m] selects the mailbox [m] for access. *)

val append :
  string ->
  ?flags:flag list ->
  ?internaldate:string ->
  string ->
  (unit, unit) cmd
(** Adds a new message (containing message) to the given mailbox. *)

module Fetch : sig
  type 'a t

  val flags : flag list t

  val envelope : envelope t

  val internaldate : string t

  val uid : uid t

  val x_gm_msgid : int64 t

  val x_gm_thrid : int64 t

  val x_gm_labels : string list t

  val rfc822 : string t

  val rfc822_text : string t

  val rfc822_header : string t

  val rfc822_size : int t

  val body : mime t

  val bodystructure : mime t

  val modseq : int64 t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val pair : 'a t -> 'b t -> ('a * 'b) t
end

val fetch : ?changed_since:modseq -> seq list -> 'a Fetch.t -> ('a, unit) cmd
(** [fetch imap uid ?changed_since set att] retrieves data associated with
    messages with sequence number in [set].

    If the [?changed_since] argument is passed, only those messages with
    [CHANGEDSINCE] mod-sequence value at least the passed value are affected
    (requires the [CONDSTORE] extension). *)

val uid_fetch :
  ?changed_since:modseq -> uid list -> 'a Fetch.t -> ('a, unit) cmd

type store_mode = [ `Add | `Remove | `Set ]

type _ store_kind = Flags : flag store_kind | Labels : string store_kind

val store :
  ?unchanged_since:modseq ->
  store_mode ->
  seq list ->
  'a store_kind ->
  'a list ->
  (unit, unit) cmd
(** [store imap ?unchanged_since mode nums kind] modifies [kind] according to
    [mode] for those message with sequence number in [nums].

    If [?unchanged_since] is present, then only those messages with [UNCHANGEDSINCE]
    mod-sequence value at least the passed value are affected. *)

val uid_store :
  ?unchanged_since:modseq ->
  store_mode ->
  uid list ->
  'a store_kind ->
  'a list ->
  (unit, unit) cmd
