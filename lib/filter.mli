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

module Message : sig
  type t

  val uid: t -> int32

  val fetch_labels: t -> string list Lwt.t
end

module Message_set : sig
  type t

  val count: t -> int Lwt.t
  val uids: t -> Message.t list Lwt.t

  val unseen: t -> t
  val answered: t -> t
  val new_: t -> t
  (* val smaller: int -> t -> t *)
  (* val subject: string -> t -> t *)

  val mark_seen: t -> unit Lwt.t
  val mark_unseen: t -> unit Lwt.t

  val fetch_labels: t -> Message.t Lwt_stream.t
  val add_labels: t -> string list -> unit Lwt.t
  val remove_labels: t -> string list -> unit Lwt.t
  val set_labels: t -> string list -> unit Lwt.t

  val add_flags: t -> Flag.t list -> unit Lwt.t
  val remove_flags: t -> Flag.t list -> unit Lwt.t
  val set_flags: t -> Flag.t list -> unit Lwt.t

  val delete: t -> unit Lwt.t

  val union: t -> t -> t
  val inter: t -> t -> t

  (* val fetch: t -> string Lwt_stream.t *)
end

module Mailbox : sig
  type t

  val name: t -> string
  val all: t -> Message_set.t
  val rename: t -> string -> unit Lwt.t
  val delete: t -> unit Lwt.t

  (* val copy_to: t -> Message_set.t -> unit Lwt.t *)
  (* val move_to: t -> Message_set.t -> unit Lwt.t *)
end

module Account : sig
  type t

  val create: username:string -> password:string -> host:string -> port:int -> unit -> t
  val inbox: t -> Mailbox.t
  val select: t -> string -> Mailbox.t
  val all: t -> Mailbox.t list Lwt.t
end
