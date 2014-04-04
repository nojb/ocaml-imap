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

(** Unsigned integers *)

open Sexplib
open Sexplib.Conv

module Uint32 = struct
  include Uint32

  let max n m =
    if compare n m <= 0 then m else n

  let equal n m =
    compare n m = 0

  let is_zero n =
    equal n zero

  let sexp_of_t x =
    Sexplib.Sexp.Atom (to_string x)

  let t_of_sexp sexp =
    match sexp with
    | Sexplib.Sexp.Atom str ->
      (try of_string str
       with exc -> of_sexp_error ("Uint32.t_of_sexp: " ^
                                  (Sexp.to_string_hum (sexp_of_exn exc))) sexp)
    | _ as sexp ->
      of_sexp_error "Uint32.t_of_sexp: atom needed" sexp

  let printer fmt n =
    Format.fprintf fmt "%s" (to_string n)
end

module Uint32_set = Imap_set.Make (Uint32)

module Uint64 = struct
  include Uint64

  let sexp_of_t x =
    Sexplib.Sexp.Atom (to_string x)

  let t_of_sexp sexp =
    match sexp with
    | Sexplib.Sexp.Atom str ->
      (try of_string str
       with exc -> of_sexp_error ("Uint64.t_of_sexp: " ^
                                  (Sexp.to_string_hum (sexp_of_exn exc))) sexp)
    | _ as sexp ->
      of_sexp_error "Uint64.t_of_sexp: atom needed" sexp

  let max x y =
    if compare x y >= 0 then x else y

  let equal n m =
    compare n m = 0

  let is_zero n =
    equal n zero

  let printer fmt n =
    Format.fprintf fmt "%s" (to_string n)
end
