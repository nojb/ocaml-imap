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

open Types
open Core

val fetch_att_modseq : fetch_att
val search_modseq : ?charset:string -> search_key -> (Uint32.t list * Uint64.t) command
val uid_search_modseq : ?charset:string -> search_key -> (Uint32.t list * Uint64.t) command
val search : ?charset:string -> search_key -> Uint32.t list command
val uid_search : ?charset:string -> search_key -> Uint32.t list command
val select : string -> unit command
val select_condstore : string -> Uint64.t command
val examine : string -> unit command
val examine_condstore : string -> Uint64.t command
val fetch : ImapSet.t -> fetch_att list -> msg_att list command
val uid_fetch : ImapSet.t -> fetch_att list -> msg_att list command
val fetch_changedsince : ImapSet.t -> Uint64.t -> fetch_att list -> msg_att list command
val uid_fetch_changedsince : ImapSet.t -> Uint64.t -> fetch_att list -> msg_att list command
val store : ImapSet.t -> store_att_flags -> unit command
val uid_store : ImapSet.t -> store_att_flags -> unit command
val store_unchangedsince : ImapSet.t -> Uint64.t -> store_att_flags -> ImapSet.t command
val uid_store_unchangedsince : ImapSet.t -> Uint64.t -> store_att_flags -> ImapSet.t command
