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

open ImapUint

type ('a, 'state) result =
  | Ok of 'a * 'state * int
  | Fail of int

type ('a, 'state) t =
  string -> 'state -> int -> ('a, 'state) result

let bind p f s st i =
  match p s st i with
  | Ok (x, st, j) -> (f x) s st j
  | Fail i -> Fail i

let return x _ st i =
  Ok (x, st, i)

let fail _ _ i =
  Fail i

let update f _ st i =
  let x, st = f st in Ok (x, st, i)

let (>>=) = bind

let (>>) p q =
  fun s st i ->
    match p s st i with
    | Ok (_, st, j) -> q s st j
    | Fail i -> Fail i

let alt ps =
  fun s st i ->
    let rec loop m = function
      | [] -> Fail m
      | p :: ps ->
        match p s st i with
        | Ok (x, st, j) -> Ok (x, st, j)
        | Fail i -> loop (max i m) ps
    in
    loop i ps

let fix f s st i =
  (f ()) s st i

let char c s st i =
  if i >= String.length s || s.[i] <> c then
    Fail i
  else
    Ok (c, st, i+1)

let any_char s st i =
  if i >= String.length s then
    Fail i
  else
    Ok (s.[i], st, i+1)

let string u s st i =
  if i + String.length u > String.length s then
    Fail (String.length s)
  else
    let rec loop j =
      if j >= String.length u then
        Ok (u, st, i+j)
      else if u.[j] = s.[i+j] then
        loop (j+1)
      else
        Fail (i+j)
    in
    loop 0

let string_ci u s st i =
  if i + String.length u > String.length s then
    Fail (String.length s)
  else
    let rec loop j =
      if j >= String.length u then
        Ok (u, st, i+j)
      else if Char.uppercase u.[j] = Char.uppercase s.[i+j] then
        loop (j+1)
      else
        Fail (i+j)
    in
    loop 0

let opt p =
  alt [(p >>= fun x -> return (Some x)); return None]

let lopt p =
  alt [p; return []]

let rec rep p =
  alt [rep1 p;return []]

and rep1 p =
  p >>= fun x -> rep p >>= fun xs -> return (x :: xs)

let sep1 s p =
  p >>= fun x -> rep (s >> p) >>= fun xs -> return (x :: xs)

let sep s p =
  alt [sep1 s p; return []]

let matches re =
  fun s st i ->
    if Str.string_match re s i then
      let ms = Str.matched_string s in
      Ok (ms, st, i + String.length ms)
    else
      Fail i

let digit =
  any_char >>= function
  | '0' .. '9' as c -> return (Char.code c - Char.code '0')
  | _ -> fail

let crlf = string "\r\n"

let nil = string_ci "NIL"

let dot s st i = char '.' s st i

let colon = char ':'

let comma = char ','

let sopt p =
  alt [p; return ""]

(*
number          = 1*DIGIT
                    ; Unsigned 32-bit integer
                    ; (0 <= n < 4,294,967,296)
*)
let number =
  let number_re = Str.regexp "[0-9]+" in
  matches number_re >>= fun s ->
  try return (Uint32.of_string s) with _ -> fail

let number' =
  let number_re = Str.regexp "[0-9]+" in
  matches number_re >>= fun s ->
  try return (int_of_string s) with _ -> fail

let quoted s st i =
  let b = Buffer.create 16 in
  let l = String.length s in
  if i >= l || s.[i] <> '\"' then
    Fail i
  else
    let rec loop j =
      if j >= l then
        Fail j
      else if s.[j] = '\"' then
        Ok (Buffer.contents b, st, j+1)
      else match s.[j] with
        | '\r' | '\n' | '\"' ->
            Fail j
        | '\\' ->
            if j+1 >= l then
              Fail l
            else
              begin
                match s.[j+1] with
                | '\\' | '\"' as c ->
                    Buffer.add_char b c; loop (j+2)
                | _ ->
                    Fail (j+1)
              end
        | '\x01' .. '\x7f' as c ->
            Buffer.add_char b c; loop (j+1)
        | _ ->
            Fail j
    in
    loop (i+1)

(*
literal         = "{" number "}" CRLF *CHAR8
                    ; Number represents the number of CHAR8s
*)
let literal =
  char '{' >> number' >>= fun len -> char '}' >> string "\r\n" >>
  update (fun st -> List.hd st, List.tl st) >>= fun lit ->
  assert (len = String.length lit);
  return lit

(*
string          = quoted / literal
*)
let imap_string =
  alt [quoted; literal]

(*
astring         = 1*ASTRING-CHAR / string
*)
let astring =
  let astring_re =
    Str.regexp "[^\x80-\xff(){ \x00-\x1f\x7f%*\\\"]+"
  in
  alt [matches astring_re; imap_string]

(*
atom            = 1*ATOM-CHAR

ATOM-CHAR       = <any CHAR except atom-specials>

atom-specials   = "(" / ")" / "{" / SP / CTL / list-wildcards /
                  quoted-specials / resp-specials

quoted-specials = DQUOTE / "\\"

resp-specials   = "]"

list-wildcards  = "%" / "*"
*)
let atom =
  let atom_re =
    Str.regexp "[^]\x80-\xff(){ \x00-\x1f\x7f%*\\\"]+"
  in
  matches atom_re

(*
TEXT-CHAR       = <any CHAR except CR and LF>

text            = 1*TEXT-CHAR
*)
let text =
  let text_re =
    Str.regexp "[^\r\n\x80-\xff]+"
  in
  matches text_re

(*
nz-number       = digit-nz *DIGIT
                    ; Non-zero unsigned 32-bit integer
                    ; (0 < n < 4,294,967,296)
*)
let nz_number =
  let nz_number_re = Str.regexp "[1-9][0-9]*" in
  matches nz_number_re >>= fun s -> try return (Uint32.of_string s) with _ -> fail

let nz_number' =
  let nz_number_re = Str.regexp "[1-9][0-9]*" in
  matches nz_number_re >>= fun s -> try return (int_of_string s) with _ -> fail

let nstring =
  alt [(imap_string >>= fun s -> return (Some s)); (nil >> return None)]

let nstring' =
  nstring >>= function
  | None -> return ""
  | Some s -> return s

let digits2 =
  let digits2_re = Str.regexp "[0-9][0-9]" in
  matches digits2_re >>= fun s -> return (int_of_string s)

let digits4 =
  let digits4_re = Str.regexp "[0-9][0-9][0-9][0-9]" in
  matches digits4_re >>= fun s -> return (int_of_string s)

let quoted_char =
  fun s st i ->
    let l = String.length s in
    if i + 2 >= l || s.[i] <> '\"' then Fail i
    else match s.[i+1] with
      | '\r' | '\n' | '\"' -> Fail (i+1)
      | '\\' ->
        if i+3 >= l || s.[i+3] <> '\"' then Fail (min l (i+3))
        else begin match s.[i+2] with
          | '\\' | '\"' as c -> Ok (c, st, i+4)
          | _ -> Fail (i+2)
        end
      | '\x01' .. '\x7f' as c ->
        if s.[i+2] = '\"' then Ok (c, st, i+3) else Fail (i+2)
      | _ -> Fail (i+1)

let string_of_length n s st i =
  if i + n > String.length s then
    Fail (String.length s)
  else
    Ok (String.sub s i n, st, i+n)

let parse p s st =
  try
    match p s st 0 with
    | Ok (x, _, j) ->
      if j < String.length s then
        Printf.eprintf "warning: incomplete parse: len: %d off: %d s: %S\n%!"
          (String.length s) j s;
      `Ok x
    | Fail i -> `Fail i
  with
  | exn -> `Exn exn
