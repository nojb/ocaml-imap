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

type s =
  | Wait
  | Crlf
  | Raw of string

type t =
  s list -> s list

let raw s k =
  Raw s :: k

let empty k =
  k

let char c =
  raw (String.make 1 c)

let (&) f g k =
  f (g k)

let (++) f g =
  f & char ' ' & g

let wait k =
  Wait :: k

let crlf k =
  Crlf :: k

let literal s =
  char '{' & raw (string_of_int (String.length s)) & char '}' & crlf & wait & raw s

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
  char '(' & f & char ')'

let mutf7 s =
  str (Mutf7.encode s)

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
    | x :: xs -> f x & char sep & loop xs
  in
  loop l

let plist ?sep f l =
  char '(' & list ?sep f l & char ')'

let eset s =
  let elt = function 0l -> "*" | n -> Printf.sprintf "%lu" n in
  let f = function
    | (lo, hi) when lo = hi -> raw (elt lo)
    | (lo, hi) -> raw (Printf.sprintf "%s:%s" (elt lo) (elt hi))
  in
  list ~sep:',' f s
