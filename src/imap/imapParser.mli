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

(** CPS Combinator parsers on strings *)

open ImapTypes
open ImapTypesPrivate
  
type 'a t =
  'a parser

val bind : 'a t -> ('a -> 'b t) -> 'b t
val alt : 'a t -> 'a t -> 'a t
val altn : 'a t list -> 'a t
val ret : 'a -> 'a t
val fail : _ t
val (>>) : _ t -> 'b t -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val app : ('a -> 'b) -> 'a t -> 'b t
val opt : 'a t -> 'a -> 'a t
val some : 'a t -> 'a option t
val rep : 'a t -> 'a list t
val rep1 : 'a t -> 'a list t
val sep : _ t -> 'a t -> 'a list t
val sep1 : _ t -> 'a t -> 'a list t
val delay : ('a -> 'b t) -> 'a -> 'b t
    
val char : char -> char t
val str : string -> string t (* case-insensitive *)
val accum : (char -> bool) -> string t
val crlf : unit t
val number : Uint32.t t
val number' : int t
val literal : string t
val imap_string : string t
val astring : string t
val atom : string t
val text : string t
val nstring : string option t
val nstring' : string t
val nz_number : Uint32.t t
val nz_number' : int t
val nil : unit t
val eof : unit t
val base64 : string t
val test : 'a t -> string -> 'a

val uint64 : Uint64.t t

val uniqueid : Uint32.t t

val sequence_set : ImapSet.t t

val capability : capability t

val greeting : greeting t

val response_data : response_data t

val response_done : response_done t

val continue_req : continue_req t

val cont_req_or_resp_data : cont_req_or_resp_data t

val response : response t
