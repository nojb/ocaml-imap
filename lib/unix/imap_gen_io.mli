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

(** Low-level I/O *)

module type S = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module Make (IO : S) : sig
  type input
    
  type 'a output

  val null : unit output

  val close_in : input -> unit IO.t

  val close_out : 'a output -> 'a IO.t

  val flush : 'a output -> unit IO.t

  val read_char_opt : input -> char option IO.t

  val read_line : input -> string IO.t

  val read_into_exactly : input -> string -> int -> int -> unit IO.t

  val write : 'a output -> string -> unit IO.t

  val write_from_exactly : 'a output -> string -> int -> int -> unit IO.t

  val buffered_input : ?buffer_size : int -> input -> input

  val buffered_output : ?buffer_size : int -> 'a output -> 'a output

  val inflate_input : ?buffer_size : int -> input -> input

  val deflate_output : ?buffer_size : int -> 'a output -> 'a output

  val string_input : string -> input
    
  val string_output : unit -> string output

  val unsafe_write : 'a output -> string -> int -> int -> int IO.t
      
  val unsafe_read : input -> string -> int -> int -> int IO.t

  val create_out :
    write:(string -> int -> int -> int IO.t)->
    close:(unit -> 'a IO.t) -> flush:(unit -> unit IO.t) -> 'a output
                                       
  val create_in :
    read:(string -> int -> int -> int IO.t) -> close:(unit -> unit IO.t) -> input
end
