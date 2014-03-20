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

(** Sets of message numbers

    The numbers are either "sequence numbers" or "unique identification
    numbers".  The sets are in the "formal" sense.  That is, the order and the
    reptitions matter.  The elements are kept in the order they are given. *)

open Sexplib.Std
open Imap_uint

type t = (uint32 * uint32) list with sexp

(** The type of element of the index set.  [n] means the number [n] if [n > 0],
    and it means [*] if [n = 0]. *)
val empty : t
(** The empty index set. *)

val all : t

val singleton : uint32 -> t

val interval : uint32 * uint32 -> t

val from : uint32 -> t
(** [from n] forms the half-interval starting from [n] (inclusive).  It
    corresponds to the expression "n:*" in the IMAP protocol. *)

val add : uint32 -> t -> t
(** [add n s] adds a single number [n] to the set [s]. *)

val add_interval : uint32 * uint32 -> t -> t
(** [add_interval n m s] adds the whole interval between numbers [n] and [m] to
    the index set [s] (including both [n] and [m]). *)

val union : t -> t -> t
(** [union s1 s2] forms the union of the [s1] and [s2]. *)

val mem : t -> uint32 -> bool
val mem_zero : t -> bool

val iter : (uint32 -> unit) -> t -> unit

val iter2 : (uint32 -> uint32 -> unit) -> t -> t -> unit

val fold : (uint32 -> 'a -> 'a) -> t -> 'a -> 'a

val fold_intervals : (uint32 * uint32 -> 'a -> 'a) -> t -> 'a -> 'a
