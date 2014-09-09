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

open Types
open TypesPrivate

type 'a result =
    ControlOk of 'a * state * int
  | ControlFail of error
  | ControlNeed of int * (input -> 'a result)
  | ControlFlush of string * 'a result

type 'a control

val flush : unit control

val bind : 'a control -> ('a -> 'b control) -> 'b control

val fail : error -> _ control

val liftP : 'a parser -> 'a control

val send : string -> unit control

val ret : 'a -> 'a control

val gets : (state -> 'a) -> 'a control

val modify : (state -> state) -> unit control

val get : state control

val put : state -> unit control

val catch : 'a control -> (error -> 'a control) -> 'a control

val (>>=) : 'a control -> ('a -> 'b control) -> 'b control

val (>>) : _ control -> 'a control -> 'a control

val run : 'a control -> state -> Buffer.t -> int -> 'a result
