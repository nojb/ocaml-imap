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

open Imap_uint

type +'a t

(** {2 Monadic operations} *)
  
val bind : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> 'a t
val fail : 'a t

(** {2 Infix operators} *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>|=) : 'a t -> ('a -> 'b) -> 'b t
val (>|) : 'a t -> 'b -> 'b t
val (>>) : 'a t -> 'b t -> 'b t
val (<|>) : 'a t -> 'a t -> 'a t

(** {2 Combinators} *)

val choice : 'a t -> 'a t -> 'a t
(** [choice p q] applies [p].  If it fails, it applies [q]. *)
    
val choices : 'a t list -> 'a t
(** [choices ps] applies all the parsers in the list [ps] until one succeeds, or
    fails if none succeeds. *)

val option : 'a t -> 'a option t
(** [option p] tries to apply the parser [p].  If [p] fails, then it returns
    [None], otherwise it returns [Some x], where [x] is the value returned by
    [p]. *)
    
val loption : 'a list t -> 'a list t
(** [loption p] tries to apply the parser [p].  If [p] fails, then it
    returns [[]], otherwise it returns the value returned by [p]. *)
    
val delimited : 'a t -> 'b t -> 'c t -> 'b t
(** [delimited l p r] applies parsers [l], [p], [r] (in that order) and returns
    the value returned by [p]. *)
    
val separated_nonempty_list : 'a t -> 'b t -> 'b list t
(** [separated_nonempty_list sep p] parses one or more occurences of [p],
    separated by [sep].  Returns the list of values returned by [p]. *)
    
val separated_pair : 'a t -> 'b t -> 'c t -> ('a * 'c) t
(** [separated_pair l sep r] parses [l], [sep], [r] (in that order). Returns the
    pair of values returned by [l] and [r]. *)
    
val separated_list : 'a t -> 'b t -> 'b list t
(** [separated_list sep p] parses zero or more occurences of [p], separated by
    [sep].  Retruns the list of values returned by [p]. *)
    
val nonempty_list : 'a t -> 'a list t
(** [nonempty_list p] parses one or more occurences of [p], and returns the list
    of returned values. *)
    
val list : 'a t -> 'a list t
(** [list p] parses zero or more occurences of [p], and returns the list of
    returned values. *)
    
val terminated : 'a t -> 'b t -> 'a t
(** [terminated p r] parses [p], then [r] and returns the value returned
    by [p]. *)
    
val noption : 'a t -> 'a option t
(** [noption p] tries to apply [p] or [nil].  If [p] succeeds, then it returns
    [Some x], where [x] is the value returned by [p].  If [nil] succeeds,
    returns [None]. *)
    
val soption : string t -> string t
(** [soption p] tries to apply [p].  If [p] fails, it returns [""].  Otherwise,
    it returns the value returned by [p]. *)
    
val boption : 'a t -> bool t
(** [boption p] tries to apply [p].  If [p] succeeds then it returns [true],
    otherwise [false]. *)

val fix : (unit -> 'a t) -> 'a t
(** [fix f] is equivalent to the parser [f ()].  It is used to define mutually
    recursive parsers. *)

(** {2 Token parsers} *)

val dot : char t
(** Parses '.' *)
    
val quoted_char : char t
(** Parses an IMAP QUOTED-CHAR, surrounded by double quotes. *)
    
val number : Uint32.t t
(** Parses an unsigned 32-bit integer represented by a sequence of digits.
    Fails if the number cannot be represented with 32-bits. *)
    
val number' : int t
(** Like {!number}, but with OCaml [int]'s instead of [uint32]. *)
    
val string : string -> string t
(** [string s] parses exactly the string [s]. *)
    
val string_ci : string -> string t
(** [string s] parses exactly the string [s] in a case-insensitive manner. *)
    
val space : char t
(** Parses [' ']. *)
    
val imap_string : string t
(** Parses an IMAP [string] terminal. *)
    
val dquote : char t
(** Parses ['"']. *)
    
val lpar : char t
(** Parses ['(']. *)
    
val rpar : char t
(** Parses [')']. *)
    
val bslash : char t
(** Parses ['\\']. *)
    
val nil : string t
(** [nil] is equivalent to [string_ci "NIL"]. *)
    
val nstring : string option t
(** Tries to apply [imap_string] or [nil].  If [imap_string] returns [x], then
    it returns [Some x].  If [nil] succeeds, then it returns [None]. *)

val nstring' : string t
(** Like {!nstring} but returns [x] if [imap_string] returns [x] and [""] if
    [nil] suceeds. *)
    
val astring : string t
(** Parses the IMAP terminal [astring]. *)
    
val nz_number : Uint32.t t
(** Parses an unsigned 32-bit integers represented by a sequence of digits
    starting with a non-zero digit.  Fails if the number cannot be represented with
    32-bits. *)
    
val nz_number' : int t
(** Like {!nz_number} but with OCaml [int]'s instead of [uint32]. *)
    
val matches : Str.regexp -> string t
(** [matches re] parses the longest string that matches the regular expression
    [re] and returns this string. *)
    
val labra : char t
(** Parses ['<']. *)
    
val rabra : char t
(** Parses ['>']. *)
    
val lbra : char t
(** Parses ['\[']. *)
    
val rbra : char t
(** Parses ['\]']. *)
    
val text : string t
(** Parses IMAP terminal [text]. *)
    
val atom : string t
(** Parses IMAP terminal [atom]. *)
    
val digit : int t
(** Parses a decimal digit. *)
    
val digits2 : int t
(** Parses two decimal digits. *)
    
val digits4 : int t
(** Parses four decimal digits. *)
    
val string_of_length : int -> string t
(** Parses an string of exactly a given length. *)
    
val colon : char t
(** Parses [':']. *)
    
val comma : char t
(** Parses [',']. *)
    
val plus : char t
(** Parses ['+']. *)
    
val minus : char t
(** Parses ['-']. *)
    
val dash : char t
(** Parses ['-']. *)
    
val crlf : string t
(** Parses ["\r\n"]. *)
    
val star : char t
(** Parses ['*']. *)
    
val parse : 'a t -> string -> [ `Ok of 'a | `Fail of int | `Exn of exn ]
(** [parse p s] tries to parse [s] with [p].  If [p] succeeds and returns [x],
    [parse p s] returns [`Ok x].  If [p] fails, it returns [Fail i], where [i] is
    the approximate location in [s] where the failure occurred.  If an exception
    [exn] is thrown during parsing, then it returns [Exn exn]. *)
