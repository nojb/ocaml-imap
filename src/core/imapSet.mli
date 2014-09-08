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
    reptitions matter.  The elements are kept in the order they are given.

    The elements of the set are unsigned 32-bit integers.  This is the data type
    that represents both IMAP sequence numbers and unique identification
    numbers, except for zero.  Zero will be interpreted as being IMAP '*', the
    highest possible number of the messages in the mailbox.

    Briefly, an expression such as 1:3,4,5:12,18:* will be represented by the OCaml
    list [[(1, 3); (4, 4); (5, 12); (18, 0)]]. *)

type num = Uint32.t

(** The type of sets, a formal, ordered, union of intervals. *)
type t = (num * num) list

val empty : t
(** The empty set. *)

val all : t
(** The interval of all positive numbers. *)

val single : num -> t
(** The interval consisting of a single number. *)

val single_int : int -> t

val interval : num -> num -> t
(** The set consisting of a single interval. *)

val from : num -> t
(** The half-interval starting from [n] (inclusive).  It corresponds to the
    IMAP expression "n:*". *)

val add : num -> t -> t
(** [add n s] adds a single number [n] to the set [s]. *)

val add_interval : num -> num -> t -> t
(** [add_interval n m s] adds the whole interval between numbers [n] and [m]
    (including both [n] and [m]) to the set [s]. *)

val union : t -> t -> t
(** [union s1 s2] forms the union of the [s1] and [s2]. *)
