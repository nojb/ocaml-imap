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

(** {1 Commands} *)

type 'a cmd

val encode :
  string -> 'a cmd -> ([ `Next of string * 'x | `Wait of 'x | `End ] as 'x)

val process : 'a cmd -> untagged -> ('a cmd, string) result

val finish : 'a cmd -> 'a

(* val poll: t -> unit Lwt.t *)

(* val stop_poll: t -> unit *)

val login : string -> string -> unit cmd

val logout : unit cmd

val create : string -> unit cmd
(** [create imap name] creates a mailbox named [name]. *)

val delete : string -> unit cmd
(** [delete imap name] deletes mailbox [name]. *)

val rename : string -> string -> unit cmd
(** [rename imap oldname newname] renames mailbox [oldname] to [newname]. *)

val noop : unit cmd
(** [noop imap] does nothing.  Since any command can return a status update as
    untagged data, the [noop] command can be used as a periodic poll for new
    messages or message status updates during a period of inactivity. *)

val list :
  ?ref:string -> string -> (mailbox_flag list * char option * string) list cmd
(** [list imap ref m] returns the list of mailboxes with names matching
    [ref]. *)

module Status : sig
  type 'a t

  val messages : int t

  val recent : int t

  val uidnext : int32 t

  val uidvalidity : int32 t

  val unseen : int t

  val highestmodseq : int64 t

  val pair : 'a t -> 'b t -> ('a * 'b) t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

val status : string -> 'a Status.t -> 'a option cmd
(** [status imap mbox items] requests status [items] for mailbox [mbox]. *)

val copy : seq list -> string -> unit cmd
(** [copy imap nums mbox] copies messages with sequence number in [nums] to
    mailbox [mbox]. *)

val uid_copy : uid list -> string -> unit cmd

val expunge : unit cmd
(** [expunge imap] permanently removes all messages that have the [Deleted]
    {!flag} set from the currently selected mailbox. *)

val uid_expunge : uid list -> unit cmd
(** Requires [UIDPLUS] extension. *)

module Search : sig
  type key
  (** Message search *)

  val all : key
  (** All messages in the mailbox. *)

  val seq : seq list -> key
  (** Messages with message sequence number in the given set. *)

  val answered : key
  (** Messages with the [Answered] flag set. *)

  val bcc : string -> key
  (** Messages that contain the specified string in the envelope structure's
    "BCC" field. *)

  val before : Date.t -> key
  (** Messages whose internal date (disregarding time and timezone) is earlier
    than the specified date. *)

  val body : string -> key
  (** Messages that contain the specified string in the body of the message. *)

  val cc : string -> key
  (** Messages that contain the specified string in the envelope structure's
    "CC" field. *)

  val deleted : key
  (** Messages with the [Deleted] {!flag} set. *)

  val draft : key
  (** Messages with the [Draft] {!flag} set. *)

  val flagged : key
  (** Messages with the [Flagged] {!flag} set. *)

  val from : string -> key
  (** Messages with FROM field containing given string. *)

  val header : string -> string -> key
  (** Messages with headers with the specified field-name and that
    contains the specified string in the text of the header (what comes after
    the colon). *)

  val keyword : string -> key
  (** Messages with the specified [Keyword] {!flag} set. *)

  val larger : int -> key
  (** Messages with size at least the given number of bytes. *)

  val new_ : key
  (** Messages that have the [Recent] {!flag} set but not the [Seen] {!flag}. *)

  val not : key -> key
  (** Negation of search criteria. *)

  val old : key
  (** Messages that do not have the [Recent] {!flag} set. *)

  val on : Date.t -> key
  (** Messages whose internal date (disregarding time and timezone) is within
    the specified date.  *)

  val ( || ) : key -> key -> key
  (** OR of search criteria. *)

  val recent : key
  (** Messages that have the [Recent] {!flag} set. *)

  val seen : key
  (** Messages that have the [Seen] {!flag} set. *)

  val sent_before : Date.t -> key
  (** Messages whose "Date:" header (disregarding time and timezone) is earlier
    than the specified date. *)

  val sent_on : Date.t -> key
  (** Messages whose "Date:" header (disregarding time and timezone) is within
    the specified date. *)

  val sent_since : Date.t -> key
  (** Messages whose "Date:" header (disregarding time and timezone) is within
    or later than the specified date.  *)

  val since : Date.t -> key
  (** Messages whose internal date (disregarding time and timezone) is within or
    later than the specified date.  *)

  val smaller : int -> key
  (** Messages with a size smaller than the specified number of octets. *)

  val subject : string -> key
  (** Messages that contain the specified string in the envelope structure's
    "SUBJECT" field. *)

  val text : string -> key
  (** Messages that contain the specified string in the header or body of the
    message. *)

  val to_ : string -> key
  (** Messages that contain the specified string in the envelope structure's
    "TO" field. *)

  val uid : uid list -> key
  (** Messages with UID in the given set. *)

  val unanswered : key
  (** Messages that do not have the [Answered] {!flag} set. *)

  val undeleted : key
  (** Messages that do not have the [Deleted] {!flag} set. *)

  val undraft : key
  (** Messages that do not have the [`Draft] {!flag} set. *)

  val unflagged : key
  (** Messages that do not have the [`Flagged] {!flag} set. *)

  val unkeyword : string -> key
  (** Messages that do not have the specified keyword {!flag} set. *)

  val unseen : key
  (** Messages that do not have the [`Seen] {!flag} set. *)

  val ( && ) : key -> key -> key
  (** Messages that satisfy both search criteria. *)

  val modseq : modseq -> key
  (** Messages that have equal or greater modification sequence numbers. *)

  val x_gm_raw : string -> key
  (** Gmail search string *)

  val x_gm_msgid : modseq -> key
  (** Messages with a given Gmail Message ID. *)

  val x_gm_thrid : modseq -> key
  (** Messages with a given Gmail Thread ID. *)

  val x_gm_labels : string list -> key
  (** Messages with given Gmail labels. *)
end

val search : Search.key -> (seq list * modseq option) cmd
(** [uid_search imap key] returns the set of UIDs of messages satisfying the
    criteria [key]. *)

val uid_search : Search.key -> (uid list * modseq option) cmd

val examine : string -> unit cmd
(** [select imap m] selects the mailbox [m] for access. *)

val select : string -> unit cmd
(** [select imap m] selects the mailbox [m] for access. *)

val append :
  string -> ?flags:flag list -> ?internaldate:string -> string -> unit cmd
(** [append imap mbox flags id data] appends [data] as a new message to the end
    of the mailbox [mbox]. An optional flag list can be passed using the [flags]
    argument. *)

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

val fetch :
  ?changed_since:modseq -> seq list -> 'a Fetch.t -> ('a -> unit) -> unit cmd
(** [fetch imap uid ?changed_since set att] retrieves data associated with
    messages with sequence number in [set].

    If the [?changed_since] argument is passed, only those messages with
    [CHANGEDSINCE] mod-sequence value at least the passed value are affected
    (requires the [CONDSTORE] extension). *)

val uid_fetch :
  ?changed_since:modseq -> uid list -> 'a Fetch.t -> ('a -> unit) -> unit cmd

type store_mode = [ `Add | `Remove | `Set ]

type _ store_kind = Flags : flag store_kind | Labels : string store_kind

val store :
  ?unchanged_since:modseq ->
  store_mode ->
  seq list ->
  'a store_kind ->
  'a list ->
  unit cmd
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
  unit cmd

module L : sig
  val is_complete : Bytes.t -> int -> int option
end
