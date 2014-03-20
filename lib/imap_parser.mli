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

(** Combinator parser *)

open Imap_uint

type +'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> 'a t
val fail : 'a t
    
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>|=) : 'a t -> ('a -> 'b) -> 'b t
val (>|) : 'a t -> 'b -> 'b t
val (>>) : 'a t -> 'b t -> 'b t
val (<|>) : 'a t -> 'a t -> 'a t
    
val choice : 'a t -> 'a t -> 'a t
val choices : 'a t list -> 'a t

val option : 'a t -> 'a option t
val loption : 'a list t -> 'a list t
val delimited : 'a t -> 'b t -> 'c t -> 'b t
val separated_nonempty_list : 'a t -> 'b t -> 'b list t
val separated_pair : 'a t -> 'b t -> 'c t -> ('a * 'c) t
val separated_list : 'a t -> 'b t -> 'b list t
val nonempty_list : 'a t -> 'a list t
val list : 'a t -> 'a list t
val terminated : 'a t -> 'b t -> 'a t
val noption : 'a t -> 'a option t
val soption : string t -> string t
val boption : 'a t -> bool t

val dot : char t
val quoted_char : char t
val number : uint32 t
val number' : int t
val fix : (unit -> 'a t) -> 'a t
val string : string -> string t
val string_ci : string -> string t
val space : char t
val imap_string : string t
val dquote : char t
val lpar : char t
val rpar : char t
val bslash : char t
val nil : string t
val nstring : string t
val astring : string t
val nz_number : uint32 t
val nz_number' : int t
val matches : Str.regexp -> string t
val labra : char t
val rabra : char t
val lbra : char t
val rbra : char t
val text : string t
val atom : string t
val digit : int t
val digits2 : int t
val digits4 : int t
val string_of_length : int -> string t
val colon : char t
val comma : char t
val plus : char t
val minus : char t
val dash : char t
val crlf : string t
val star : char t
    
val parse : 'a t -> string -> [ `Ok of 'a | `Fail of int | `Exn of exn ]
