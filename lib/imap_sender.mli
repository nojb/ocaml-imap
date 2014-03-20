(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

(** Output routines *)

open Imap_uint

type output = string -> unit Lwt.t
type t = output -> [ `Ok | `Cont_req of t ] Lwt.t
val (@>) : t -> t -> t
val space : t
val crlf : t
val char : char -> t
val int : int -> t
val uint32 : uint32 -> t
val uint64 : uint64 -> t
val raw : string -> t
val null : t
val separated : t -> ('a -> t) -> 'a list -> t
val list : ('a -> t) -> 'a list -> t
val separated_pair : ('a -> t) -> t -> ('b -> t) -> ('a * 'b) -> t
val string : string -> t
val nil : t
val nstring : string option -> t
val literal : string -> t
val mailbox : string -> t
val date_time : float -> t
val message_set : Imap_set.t -> t
val flag : Imap_types.flag -> t
val fetch_att : Imap_types.fetch_att -> t
val search_key : Imap_types.search_key -> t
val status_att : Imap_types.status_att -> t
val store_att : Imap_types.store_att -> t
