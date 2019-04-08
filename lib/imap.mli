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

type ('a, 'b) cmd = private
  {
    format: Encoder.t;
    default: 'a;
    process: 'a -> Response.Untagged.t -> 'a;
    finish: 'a -> 'b;
  }

val run: t -> ('a, 'b) cmd -> 'b Lwt.t

(* val poll: t -> unit Lwt.t *)

(* val stop_poll: t -> unit *)

val login: string -> string -> (unit, unit) cmd
val logout: (unit, unit) cmd

val create: string -> (unit, unit) cmd
(** [create imap name] creates a mailbox named [name]. *)

val delete: string -> (unit, unit) cmd
(** [delete imap name] deletes mailbox [name]. *)

val rename: string -> string -> (unit, unit) cmd
(** [rename imap oldname newname] renames mailbox [oldname] to [newname]. *)

val noop: (unit, unit) cmd
(** [noop imap] does nothing.  Since any command can return a status update as
    untagged data, the [noop] command can be used as a periodic poll for new
    messages or message status updates during a period of inactivity. *)

val list: ?ref:string -> string -> ((MailboxFlag.t list * char option * string) list, (MailboxFlag.t list * char option * string) list) cmd
(** [list imap ref m] returns the list of mailboxes with names matching
    [ref]. *)

val status: string -> 'a Status.t -> ('a option, 'a option) cmd
(** [status imap mbox items] requests status [items] for mailbox [mbox]. *)

val copy: seq list -> string -> (unit, unit) cmd
(** [copy imap nums mbox] copies messages with sequence number in [nums] to
    mailbox [mbox]. *)

val uid_copy: uid list -> string -> (unit, unit) cmd

val expunge: (unit, unit) cmd
(** [expunge imap] permanently removes all messages that have the [Deleted]
    {!flag} set from the currently selected mailbox. *)

val uid_expunge: uid list -> (unit, unit) cmd
(** Requires [UIDPLUS] extension. *)

val search: Search.key -> (seq list list * modseq option, seq list * modseq option) cmd
(** [uid_search imap key] returns the set of UIDs of messages satisfying the
    criteria [key]. *)

val uid_search: Search.key -> (uid list list * modseq option, uid list * modseq option) cmd

val examine: string -> (unit, unit) cmd
(** [select imap m] selects the mailbox [m] for access. *)

val select: string -> (unit, unit) cmd
(** [select imap m] selects the mailbox [m] for access. *)

val append: string -> ?flags:Flag.t list -> ?internaldate:string -> string -> (unit, unit) cmd
(** [append imap mbox flags id data] appends [data] as a new message to the end
    of the mailbox [mbox]. An optional flag list can be passed using the [flags]
    argument. *)

val fetch: ?changed_since:modseq -> seq list -> 'a Fetch.t -> ('a -> unit) -> (unit, unit) cmd
(** [fetch imap uid ?changed_since set att] retrieves data associated with
    messages with sequence number in [set].

    If the [?changed_since] argument is passed, only those messages with
    [CHANGEDSINCE] mod-sequence value at least the passed value are affected
    (requires the [CONDSTORE] extension). *)

val uid_fetch: ?changed_since:modseq -> uid list -> 'a Fetch.t -> ('a -> unit) -> (unit, unit) cmd

type store_mode =
  [ `Add
  | `Remove
  | `Set ]

type store_kind =
  [ `Flags of Flag.t list
  | `Labels of string list ]

val store: ?unchanged_since:modseq -> store_mode -> seq list -> store_kind -> (unit, unit) cmd
(** [store imap ?unchanged_since mode nums kind] modifies [kind] according to
    [mode] for those message with sequence number in [nums].

    If [?unchanged_since] is present, then only those messages with [UNCHANGEDSINCE]
    mod-sequence value at least the passed value are affected. *)

val uid_store: ?unchanged_since:modseq -> store_mode -> uid list -> store_kind -> (unit, unit) cmd

module Parser   = Parser
module Encoder  = Encoder
module Response = Response
