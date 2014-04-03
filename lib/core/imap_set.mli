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

open Sexplib.Std
(* open Imap_uint *)

module type S = sig
  type t with sexp
  val succ : t -> t
  val zero : t
  val is_zero : t -> bool
  val compare : t -> t -> int
  val of_int : int -> t
end

module Make (N : S) : sig
  (** The type of sets, a formal, ordered, union of intervals. *)
  type t = (N.t * N.t) list with sexp

  val empty : t
  (** The empty set. *)

  val all : t
  (** The interval of all positive numbers. *)

  val single : N.t -> t
  (** The interval consisting of a single number. *)

  val single' : int -> t

  val interval : N.t * N.t -> t
  (** The set consisting of a single interval. *)

  val from : N.t -> t
  (** The half-interval starting from [n] (inclusive).  It corresponds to the
      IMAP expression "n:*". *)

  val add : N.t -> t -> t
  (** [add n s] adds a single number [n] to the set [s]. *)

  val add_interval : N.t * N.t -> t -> t
  (** [add_interval n m s] adds the whole interval between numbers [n] and [m]
      (including both [n] and [m]) to the set [s]. *)

  val union : t -> t -> t
  (** [union s1 s2] forms the union of the [s1] and [s2]. *)

  val mem : t -> N.t -> bool
  (** Whether an element belongs to the set. *)

  val mem_zero : t -> bool
  (** Whether zero belongs to the set. *)

  val iter : (N.t -> unit) -> t -> unit
  (** [iter f s] computes [f n1; f n2; ...], where [n1, n2, ...] is an enumeration
      of the elements of the set [s], in the order that they appear in the
      underlying list. *)

  val iter2 : (N.t -> N.t -> unit) -> t -> t -> unit
  (** [iter2 f s1 s2] computes [f n1 m1; f n2 m2; ...], where [n1, n2, ...] and
      [m1, m2, ...] are enumerations of the elements of the sets [s1] and [s2],
      in the order that they appear in the underlying lists. *)

  val fold : (N.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s x] computes [... (f n2 (f n1 x))] where [n1, n2, ...] is an
      anumeration of the elements of [s] in the order that they appear in the
      underlying list. *)

  val fold_intervals : (N.t * N.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_intervals f s x] is equivalent to [List.fold_right f s x]. *)
end
