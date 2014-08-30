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

(** Combinator parsers on strings *)

open ImapUint

type (+'a, 'state) t

(** {2 Monadic operations} *)
  
val bind : ('a, 'state) t -> ('a -> ('b, 'state) t) -> ('b, 'state) t
val return : 'a -> ('a, _) t
val fail : ('a, _) t
val update : ('state -> 'a * 'state) -> ('a, 'state) t

(** {2 Infix operators} *)

val (>>=) : ('a, 'state) t -> ('a -> ('b, 'state) t) -> ('b, 'state) t
val (>>) : (_, 'state) t -> ('b, 'state) t -> ('b, 'state) t

(** {2 Combinators} *)

val alt : ('a, 'state) t list -> ('a, 'state) t
(** [choices ps] applies all the parsers in the list [ps] until one succeeds, or
    fails if none succeeds. *)

val opt : ('a, 'state) t -> ('a option, 'state) t
(** [option p] tries to apply the parser [p].  If [p] fails, then it returns
    [None], otherwise it returns [Some x], where [x] is the value returned by
    [p]. *)
    
val lopt : ('a list, 'state) t -> ('a list, 'state) t
(** [loption p] tries to apply the parser [p].  If [p] fails, then it
    returns [[]], otherwise it returns the value returned by [p]. *)
    
val rep1 : ('a, 'state) t -> ('a list, 'state) t
(** [nonempty_list p] parses one or more occurences of [p], and returns the list
    of returned values. *)
    
val rep : ('a, 'state) t -> ('a list, 'state) t
(** [list p] parses zero or more occurences of [p], and returns the list of
    returned values. *)

val sep : (_, 'state) t -> ('a, 'state) t -> ('a list, 'state) t

val sep1 : (_, 'state) t -> ('a, 'state) t -> ('a list, 'state) t
    
val sopt : (string, 'state) t -> (string, 'state) t
(** [soption p] tries to apply [p].  If [p] fails, it returns [""].  Otherwise,
    it returns the value returned by [p]. *)
    
val fix : (unit -> ('a, 'state) t) -> ('a, 'state) t

(** {2 Token parsers} *)

val char : char -> (char, _) t
    
val quoted_char : (char, string list) t
(** Parses an IMAP QUOTED-CHAR, surrounded by double quotes. *)
    
val number : (Uint32.t, string list) t
(* (\** Parses an unsigned 32-bit integer represented by a sequence of digits. *)
(*     Fails if the number cannot be represented with 32-bits. *\) *)
    
val number' : (int, string list) t
(** Like {!number}, but with OCaml [int]'s instead of [uint32]. *)
    
val string : string -> (string, _) t
(** [string s] parses exactly the string [s]. *)
    
val string_ci : string -> (string, _) t
(** [string s] parses exactly the string [s] in a case-insensitive manner. *)
    
val imap_string : (string, string list) t
(** Parses an IMAP [string] terminal. *)
        
val nstring : (string option, string list) t
(** Tries to apply [imap_string] or [nil].  If [imap_string] returns [x], then
    it returns [Some x].  If [nil] succeeds, then it returns [None]. *)

val nstring' : (string, string list) t
(** Like {!nstring} but returns [x] if [imap_string] returns [x] and [""] if
    [nil] suceeds. *)
    
val astring : (string, string list) t
(** Parses the IMAP terminal [astring]. *)
    
val nz_number : (Uint32.t, string list) t
(** Parses an unsigned 32-bit integers represented by a sequence of digits
    starting with a non-zero digit.  Fails if the number cannot be represented with
    32-bits. *)
    
val nz_number' : (int, string list) t
(** Like {!nz_number} but with OCaml [int]'s instead of [uint32]. *)
    
val matches : Str.regexp -> (string, _) t
(** [matches re] parses the longest string that matches the regular expression
    [re] and returns this string. *)
    
val text : (string, string list) t
(** Parses IMAP terminal [text]. *)
    
val atom : (string, string list) t
(** Parses IMAP terminal [atom]. *)

val nil : (string, string list) t
    
val digit : (int, string list) t
(** Parses a decimal digit. *)
    
val digits2 : (int, string list) t
(** Parses two decimal digits. *)
    
val digits4 : (int, string list) t
(** Parses four decimal digits. *)

val string_of_length : int -> (string, 'state) t

val parse : ('a, 'state) t -> string -> 'state -> [ `Ok of 'a | `Fail of int | `Exn of exn ]
(** [parse p s] tries to parse [s] with [p].  If [p] succeeds and returns [x],
    [parse p s] returns [`Ok x].  If [p] fails, it returns [Fail i], where [i] is
    the approximate location in [s] where the failure occurred.  If an exception
    [exn] is thrown during parsing, then it returns [Exn exn]. *)
