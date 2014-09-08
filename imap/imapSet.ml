(* The MIT License (MIT) *)

(* Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com> *)

(* Permission is hereby granted, free of charge, to any person obtaining a copy *)
(* of this software and associated documentation files (the "Software"), to deal *)
(* in the Software without restriction, including without limitation the rights *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell *)
(* copies of the Software, and to permit persons to whom the Software is *)
(* furnished to do so, subject to the following conditions: *)

(* The above copyright notice and this permission notice shall be included in *)
(* all copies or substantial portions of the Software. *)

(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE *)
(* SOFTWARE. *)

module type NUM = sig
  type t
  val succ : t -> t
  val zero : t
  val compare : t -> t -> int
  val of_int : int -> t
end

module type S = sig
  type num
    
  (** The type of sets, a formal, ordered, union of intervals. *)
  type t = (num * num) list

  val empty : t
  (** The empty set. *)

  val all : t
  (** The interval of all positive numbers. *)

  val single : num -> t
  (** The interval consisting of a single number. *)

  val single' : int -> t

  val interval : num * num -> t
  (** The set consisting of a single interval. *)

  val from : num -> t
  (** The half-interval starting from [n] (inclusive).  It corresponds to the
      IMAP expression "n:*". *)

  val add : num -> t -> t
  (** [add n s] adds a single number [n] to the set [s]. *)

  val add_interval : num * num -> t -> t
  (** [add_interval n m s] adds the whole interval between numbers [n] and [m]
      (including both [n] and [m]) to the set [s]. *)

  val union : t -> t -> t
  (** [union s1 s2] forms the union of the [s1] and [s2]. *)

  val mem : t -> num -> bool
  (** Whether an element belongs to the set. *)

  val mem_zero : t -> bool
  (** Whether zero belongs to the set. *)

  val iter : (num -> unit) -> t -> unit
  (** [iter f s] computes [f n1; f n2; ...], where [n1, n2, ...] is an enumeration
      of the elements of the set [s], in the order that they appear in the
      underlying list. *)

  val iter2 : (num -> num -> unit) -> t -> t -> unit
  (** [iter2 f s1 s2] computes [f n1 m1; f n2 m2; ...], where [n1, n2, ...] and
      [m1, m2, ...] are enumerations of the elements of the sets [s1] and [s2],
      in the order that they appear in the underlying lists. *)

  val fold : (num -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s x] computes [... (f n2 (f n1 x))] where [n1, n2, ...] is an
      anumeration of the elements of [s] in the order that they appear in the
      underlying list. *)

  val fold_intervals : (num * num -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_intervals f s x] is equivalent to [List.fold_right f s x]. *)
end

module Make (N : NUM) = struct
  type num = N.t
  type t = (N.t * N.t) list

  let empty =
    []

  let all =
    [(N.succ N.zero, N.zero)]

  let single n =
    [(n, n)]

  let single' n =
    single (N.of_int n)

  let interval (n, m) =
    [(n, m)]

  let from n =
    (n, N.zero) :: []

  let add n set =
    (n, n) :: set

  let compare_elt x y =
    if N.compare N.zero x = 0 then
      if N.compare N.zero y = 0 then
        0
      else
        1
    else if N.compare N.zero y = 0 then
      -1
    else
      N.compare x y

  let add_interval (n, m) set =
    let n, m =
      if compare_elt n m <= 0 then n, m
      else m, n
    in
    (n, m) :: set

  let union s1 s2 =
    List.fold_right (fun r s2 -> r :: s2) s1 s2

  let mem s x =
    List.exists (fun (m, n) -> compare_elt x m >= 0 && compare_elt x n <= 0) s

  let mem_zero s =
    mem s N.zero

  let rec iter f = function
    | [] -> ()
    | (l, r) :: set ->
        if N.compare N.zero r = 0 then failwith "ImapSet.iter";
        let rec loop l =
          if N.compare l r > 0 then
            iter f set
          else
            begin
              f l;
              loop (N.succ l)
            end
        in
        loop l

  let rec iter2 f s1 s2 =
    match s1, s2 with
    | [], [] -> ()
    | (l1, r1) :: s1, (l2, r2) :: s2 ->
      if N.compare r1 N.zero = 0 || N.compare r2 N.zero = 0 then
        failwith "ImapSet.iter2";
      let rec loop l1 l2 =
        if N.compare l1 r1 > 0 then
          if N.compare l2 r2 > 0 then
            iter2 f s1 s2
          else
            iter2 f s1 ((l2, r2) :: s2)
        else
        if N.compare l2 r2 > 0 then
          iter2 f ((l1, r1) :: s1) s2
        else begin
          f l1 l2;
          loop (N.succ l1) (N.succ l2)
        end
      in
      loop l1 l2
    | _ ->
      failwith "ImapSet.iter2"

  let rec fold f s x =
    let rec loop =
      function
        [] -> x
      | (l, r) :: s ->
          if N.compare N.zero l = 0 then failwith "ImapSet.fold";
          let rec loop l x =
            if N.compare l r > 0 then
              fold f s x
            else
              loop (N.succ l) (f l x)
          in
          loop l x
    in
    loop s

  let rec fold_intervals f s x =
    match s with
    | [] -> x
    | r :: s ->
      fold_intervals f s (f r x)

  (* let min_elt n m = *)
  (*   if compare_elt n m >= 0 then m else n *)

  (* let max_elt n m = *)
  (*   if compare_elt n m >= 0 then n else m *)

  (* let pred_elt n = *)
  (*   if Int.compare n Int.zero = 0 then Int.zero *)
  (*   else Int.pred n *)

  (* let succ_elt n = *)
  (*   if Int.compare n Int.zero = 0 then Int.zero *)
  (*   else Int.succ n *)

  (* let rec merge (n, m) = function *)
  (*   |  [] -> [n, m] *)
  (*   |  (l, r) :: set -> *)
  (*     if compare_elt m (pred_elt l) < 0 then *)
  (*       (n, m) :: merge (l, r) set *)
  (*     else if compare_elt n (succ_elt r) > 0 then *)
  (*       (l, r) :: merge (n, m) set *)
  (*     else (\* m >= l-1 && n <= r+1 *\) *)
  (*       merge (min_elt n l, max_elt r m) set *)

  (* let optimise set = *)
  (*   let set = List.sort (fun (n, _) (l, _) -> compare n l) set in *)
  (*   match set with *)
  (*   | [] -> [] *)
  (*   | (n, m) :: set -> *)
  (*     merge (n, m) set *)
end

module Uint32 = Make (Uint32)
