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

type t
(** The type for connections. *)

val init: t

module Cmd : sig
  type 'a t

  val login: string -> string -> unit t

  val create: string -> unit t
  (** [create imap name] creates a mailbox named [name]. *)

  val delete: string -> unit t
  (** [delete imap name] deletes mailbox [name]. *)

  val rename: string -> string -> unit t
  (** [rename imap oldname newname] renames mailbox [oldname] to [newname]. *)

  val noop: unit t
  (** [noop imap] does nothing.  Since any command can return a status update as
      untagged data, the [noop] command can be used as a periodic poll for new
      messages or message status updates during a period of inactivity. *)

  val list: ?ref:string -> string -> (MailboxFlag.t list * char option * string) list t
  (** [list imap ref m] returns the list of mailboxes with names matching
      [ref]. *)

  val status: string -> 'a Status.t -> 'a option t
  (** [status imap mbox items] requests status [items] for mailbox [mbox]. *)

  val copy: seq list -> string -> unit t
  (** [copy imap nums mbox] copies messages with sequence number in [nums] to
      mailbox [mbox]. *)

  val uid_copy: uid list -> string -> unit t

  val expunge: unit t
  (** [expunge imap] permanently removes all messages that have the [Deleted]
      {!flag} set from the currently selected mailbox. *)

  val uid_expunge: uid list -> unit t
  (** Requires [UIDPLUS] extension. *)

  val search: Search.key -> (seq list * modseq option) t
  (** [uid_search imap key] returns the set of UIDs of messages satisfying the
      criteria [key]. *)

  val uid_search: Search.key -> (uid list * modseq option) t

  val examine: string -> unit t
  (** [select imap m] selects the mailbox [m] for access. *)

  val select: string -> unit t
  (** [select imap m] selects the mailbox [m] for access. *)

  val append: string -> ?flags:Flag.t list -> ?internaldate:string -> string -> unit t
  (** [append imap mbox flags id data] appends [data] as a new message to the end
      of the mailbox [mbox]. An optional flag list can be passed using the [flags]
      argument. *)

  val fetch: ?changed_since:modseq -> seq list -> 'a Fetch.t -> ('a -> unit) -> unit t
  (** [fetch imap uid ?changed_since set att] retrieves data associated with
      messages with sequence number in [set].

      If the [?changed_since] argument is passed, only those messages with
      [CHANGEDSINCE] mod-sequence value at least the passed value are affected
      (requires the [CONDSTORE] extension). *)

  val uid_fetch: ?changed_since:modseq -> uid list -> 'a Fetch.t -> ('a -> unit) -> unit t

  type store_mode =
    [ `Add
    | `Remove
    | `Set ]

  type store_kind =
    [ `Flags of Flag.t list
    | `Labels of string list ]

  val store: ?unchanged_since:modseq -> store_mode -> seq list -> store_kind -> unit t
  (** [store imap ?unchanged_since mode nums kind] modifies [kind] according to
      [mode] for those message with sequence number in [nums].

      If [?unchanged_since] is present, then only those messages with [UNCHANGEDSINCE]
      mod-sequence value at least the passed value are affected. *)

  val uid_store: ?unchanged_since:modseq -> store_mode -> uid list -> store_kind -> unit t
end

module Run : sig
  type state

  type 'a t

  type 'a r =
    [ `More of int option * 'a t
    | `Run of string * 'a r
    | `Done of ('a, string) result * state ]

  val step: 'a t -> string -> 'a r
end with type state := t

val run: t -> 'a Cmd.t -> 'a Run.r
