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

module type S = sig
  type 'a t
      
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
      
  type mutex
  val create_mutex : unit -> mutex
  val is_locked : mutex -> bool
  val with_lock : mutex -> (unit -> 'a t) -> 'a t

  type input
  type output

  val read_line : input -> string t
  val read_exactly : input -> int -> string t
  val write : output -> string -> unit t
  val flush : output -> unit t
      
  val connect : int -> string -> (input * output) t
  val disconnect : (input * output) -> unit t
      
  val connect_ssl : [ `TLSv1 | `SSLv23 | `SSLv3 ] -> ?ca_file : string ->
    int -> string -> (input * output) t
  
  val starttls : [ `TLSv1 | `SSLv23 | `SSLv3 ] -> ?ca_file : string ->
    input * output -> (input * output) t
      
  val compress : input * output -> input * output
end
