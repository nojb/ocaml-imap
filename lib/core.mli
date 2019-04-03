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

type error =
  | Incorrect_tag of string * string
  | Decode_error of string * int
  | Unexpected_cont
  | Bad_greeting
  | Auth_error of string
  | Server_error of string

exception Error of error

type t
(** The type for connections. *)

val connect: host:string -> port:int -> username:string -> password:string -> t Lwt.t
(** [connect server username password mailbox]. *)

val disconnect: t -> unit Lwt.t
(** Disconnect. *)

(** {1 Commands} *)

(* val poll: t -> unit Lwt.t *)

(* val stop_poll: t -> unit *)

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

val list: t -> ?ref:string -> string -> (MailboxFlag.t list * char option * string) list Lwt.t
(** [list imap ref m] returns the list of mailboxes with names matching
    [ref]. *)

val status: t -> string -> Status.Request.t list -> Status.Response.t Lwt.t
(** [status imap mbox items] requests status [items] for mailbox [mbox]. *)

val copy: t -> seq list -> string -> unit Lwt.t
(** [copy imap nums mbox] copies messages with sequence number in [nums] to
    mailbox [mbox]. *)

val uid_copy: t -> uid list -> string -> unit Lwt.t

val expunge: t -> unit Lwt.t
(** [expunge imap] permanently removes all messages that have the [Deleted]
    {!flag} set from the currently selected mailbox. *)

val uid_expunge: t -> uid list -> unit Lwt.t
(** Requires [UIDPLUS] extension. *)

val search: t -> Search.key -> (seq list * modseq option) Lwt.t
(** [uid_search imap key] returns the set of UIDs of messages satisfying the
    criteria [key]. *)

val uid_search: t -> Search.key -> (uid list * modseq option) Lwt.t

val examine: t -> string -> unit Lwt.t
(** [select imap m] selects the mailbox [m] for access. *)

val select: t -> string -> unit Lwt.t
(** [select imap m] selects the mailbox [m] for access. *)

val append: t -> string -> ?flags:Flag.t list -> ?internaldate:string -> string -> unit Lwt.t
(** [append imap mbox flags id data] appends [data] as a new message to the end
    of the mailbox [mbox]. An optional flag list can be passed using the [flags]
    argument. *)

val fetch: t -> ?changed_since:modseq -> seq list -> 'a Fetch.t -> ('a -> unit) -> unit Lwt.t
(** [fetch imap uid ?changed_since set att] retrieves data associated with
    messages with sequence number in [set].

    If the [?changed_since] argument is passed, only those messages with
    [CHANGEDSINCE] mod-sequence value at least the passed value are affected
    (requires the [CONDSTORE] extension). *)

val uid_fetch: t -> ?changed_since:modseq -> uid list -> 'a Fetch.t -> ('a -> unit) -> unit Lwt.t

type store_mode =
  [ `Add
  | `Remove
  | `Set ]

type store_kind =
  [ `Flags of Flag.t list
  | `Labels of string list ]

val store: t -> ?unchanged_since:modseq -> store_mode -> seq list -> store_kind -> unit Lwt.t
(** [store imap ?unchanged_since mode nums kind] modifies [kind] according to
    [mode] for those message with sequence number in [nums].

    If [?unchanged_since] is present, then only those messages with [UNCHANGEDSINCE]
    mod-sequence value at least the passed value are affected. *)

val uid_store: t -> ?unchanged_since:modseq -> store_mode -> uid list -> store_kind -> unit Lwt.t
