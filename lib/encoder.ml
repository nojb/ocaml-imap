(* The MIT License (MIT)

   Copyright (c) 2015-2018 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

open Sexplib.Std

type t =
  | Cat of t * t
  | Literal of string
  | Raw of string [@@deriving sexp]

let rec is_empty = function
  | Cat (f, g) -> is_empty f && is_empty g
  | Literal _ -> false
  | Raw "" -> true
  | Raw _ -> false

let raw s =
  Raw s

let empty =
  raw ""

let (&) f g =
  if is_empty f then g
  else if is_empty g then f
  else Cat (f, g)

let (++) f g =
  f & raw " " & g

let literal s =
  Literal s

let str s =
  let literal_chars = function
    | '\x80' .. '\xFF' | '\r' | '\n' -> true
    | _ -> false
  in
  let quoted_chars = function
    | '(' | ')' | '{' | ' ' | '\x00' .. '\x1F' | '\x7F'
    | '%' | '*' | '\"' | '\\' -> true
    | _ -> false
  in
  let needs f s =
    let rec loop i = i < String.length s && (f s.[i] || loop (i+1)) in
    loop 0
  in
  if s = "" then
    raw "\"\""
  else if needs literal_chars s then
    literal s
  else if needs quoted_chars s then
    raw (Printf.sprintf "\"%s\"" s)
  else
    raw s

let p f =
  Cat (Raw "(", Cat (f, Raw ")"))

let mutf7 s =
  raw (Mutf7.encode s)

let int n =
  raw (string_of_int n)

let uint64 m =
  raw (Printf.sprintf "%Lu" m)

let label l =
  raw (Mutf7.encode l)

let list ?(sep = ' ') f l =
  let rec loop = function
    | [] -> empty
    | [x] -> f x
    | x :: xs -> Cat (f x, Cat (Raw (String.make 1 sep), loop xs))
  in
  loop l

let plist ?sep f l =
  Cat (Raw "(", Cat (list ?sep f l, Raw ")"))

let eset s =
  let elt = function 0l -> "*" | n -> Printf.sprintf "%lu" n in
  let f = function
    | (lo, hi) when lo = hi -> raw (elt lo)
    | (lo, hi) -> raw (Printf.sprintf "%s:%s" (elt lo) (elt hi))
  in
  list ~sep:',' f s
