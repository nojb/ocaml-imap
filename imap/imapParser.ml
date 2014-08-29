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

type 'a result =
  | Ok of 'a * int
  | Fail of int

type 'a t =
  string -> int -> 'a result

let bind p f =
  fun s i ->
    match p s i with
    | Ok (x, j) -> (f x) s j
    | Fail i -> Fail i

let return x = fun _ i -> Ok (x, i)

let fail = fun _ i -> Fail i

let (>>=) = bind

let (>|=) p f =
  fun s i ->
    match p s i with
    | Ok (x, j) -> Ok (f x, j)
    | Fail i -> Fail i

let (>>) p q =
  fun s i ->
    match p s i with
    | Ok (_, j) -> q s j
    | Fail i -> Fail i

let (>|) p x =
  fun s i ->
    match p s i with
    | Ok (_, j) -> Ok (x, j)
    | Fail i -> Fail i

let choice p q =
  fun s i ->
    match p s i with
    | Ok (x, j) -> Ok (x, j)
    | Fail j ->
      match q s i with
      | Ok (x, j) -> Ok (x, j)
      | Fail k -> Fail (max j k)

let (<|>) = choice

let choices ps =
  fun s i ->
    let rec loop m = function
      | [] -> Fail m
      | p :: ps ->
        match p s i with
        | Ok (x, j) -> Ok (x, j)
        | Fail i -> loop (max i m) ps
    in
    loop i ps

let fix f =
  fun s i ->
    (f ()) s i

let char c =
  fun s i ->
    if i >= String.length s || s.[i] <> c then Fail i
    else Ok (c, i+1)

let any_char =
  fun s i ->
    if i >= String.length s then Fail i
    else Ok (s.[i], i+1)

let string_of_length n =
  fun s i ->
    if i + n > String.length s then Fail (String.length s)
    else Ok (String.sub s i n, i+n)

let string u s i =
  if i + String.length u > String.length s then Fail (String.length s)
  else
    let rec loop j =
      if j >= String.length u then Ok (u, i+j)
      else if u.[j] = s.[i+j] then loop (j+1)
      else Fail (i+j)
    in
    loop 0

let string_ci u =
  fun s i ->
    if i + String.length u > String.length s then Fail (String.length s)
    else
      let rec loop j =
        if j >= String.length u then Ok (u, i+j)
        else if Char.uppercase u.[j] = Char.uppercase s.[i+j] then loop (j+1)
        else Fail (i+j)
      in
      loop 0

let option p =
  (p >|= fun x -> Some x) <|> return None

let loption p =
  p <|> return []

let boption p =
  (p >| true) <|> return false

let delimited l p r =
  l >> p >>= fun x -> r >| x

let terminated p r =
  p >>= fun x -> r >| x

let preceded l p =
  l >> p

let rec list p =
  nonempty_list p <|> return []

and nonempty_list p =
  p >>= fun x -> list p >|= fun xs -> x :: xs

let rec separated_nonempty_list sep p =
  p >>= fun x -> loption (sep >> separated_nonempty_list sep p) >|= fun xs -> (x :: xs)

let separated_list sep p =
  separated_nonempty_list sep p <|> return []

let separated_pair p sep q =
  p >>= fun x -> sep >> q >|= fun y -> (x, y)

let pair p q =
  p >>= fun x -> q >|= fun y -> (x, y)

let matches re =
  fun s i ->
    if Str.string_match re s i then
      let ms = Str.matched_string s in
      Ok (ms, i + String.length ms)
    else
      Fail i

let digit =
  any_char >>= function
  | '0' .. '9' as c -> return (Char.code c - Char.code '0')
  | _ -> fail

let space = char ' '

let lpar = char '('

let rpar = char ')'

let bslash = char '\\'

let dquote = char '"'

let star = char '*'

let plus = char '+'

let dash = char '-'

let minus = char '-'

let lcbra = char '{'

let rcbra = char '}'

let lbra = char '['

let rbra = char ']'

let labra = char '<'

let rabra = char '>'

let crlf = string "\r\n"

let nil = string_ci "NIL"

let dot = char '.'

let colon = char ':'

let comma = char ','

let noption p =
  (p >|= fun x -> Some x) <|> (nil >| None)

let soption p =
  p <|> return ""

(*
number          = 1*DIGIT
                    ; Unsigned 32-bit integer
                    ; (0 <= n < 4,294,967,296)
*)
let number =
  let number_re = Str.regexp "[0-9]+" in
  matches number_re >>= fun s -> try return (Uint32.of_string s) with _ -> fail

let number' =
  let number_re = Str.regexp "[0-9]+" in
  matches number_re >>= fun s -> try return (int_of_string s) with _ -> fail

let quoted =
  fun s i ->
    let b = Buffer.create 16 in
    let l = String.length s in
    if i >= l || s.[i] <> '"' then Fail i
    else
      let rec loop j =
        if j >= l then Fail j
        else if s.[j] = '"' then Ok (Buffer.contents b, j+1)
        else match s.[j] with
          | '\r' | '\n' | '"' -> Fail j
          | '\\' ->
            if j+1 >= l then Fail l
            else begin match s.[j+1] with
              | '\\' | '"' as c -> Buffer.add_char b c; loop (j+2)
              | _ -> Fail (j+1)
            end
          | '\x01' .. '\x7f' as c -> Buffer.add_char b c; loop (j+1)
          | _ -> Fail j
      in
      loop (i+1)

(*
literal         = "{" number "}" CRLF *CHAR8
                    ; Number represents the number of CHAR8s
*)
let literal =
  delimited lcbra number' (rcbra >> crlf) >>= string_of_length

(*
string          = quoted / literal
*)
let imap_string : string t =
  quoted <|> literal

(*
astring         = 1*ASTRING-CHAR / string
*)
let astring =
  let astring_re =
    Str.regexp "[^\x80-\xff(){ \x00-\x1f\x7f%*\\\"]+"
  in
  matches astring_re <|> imap_string

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
  (imap_string >|= fun s -> Some s) <|> (nil >| None)

let nstring' =
  nstring >|= function
  | None -> ""
  | Some s -> s

let digits2 =
  let digits2_re = Str.regexp "[0-9][0-9]" in
  matches digits2_re >|= int_of_string

let digits4 =
  let digits4_re = Str.regexp "[0-9][0-9][0-9][0-9]" in
  matches digits4_re >|= int_of_string

let quoted_char =
  fun s i ->
    let l = String.length s in
    if i + 2 >= l || s.[i] <> '"' then Fail i
    else match s.[i+1] with
      | '\r' | '\n' | '"' -> Fail (i+1)
      | '\\' ->
        if i+3 >= l || s.[i+3] <> '"' then Fail (min l (i+3))
        else begin match s.[i+2] with
          | '\\' | '"' as c -> Ok (c, i+4)
          | _ -> Fail (i+2)
        end
      | '\x01' .. '\x7f' as c ->
        if s.[i+2] = '"' then Ok (c, i+3) else Fail (i+2)
      | _ -> Fail (i+1)

let parse p s =
  try
    match p s 0 with
    | Ok (x, j) ->
      if j < String.length s then
        Printf.eprintf "warning: incomplete parse: len: %d off: %d s: %S\n%!"
          (String.length s) j s;
      `Ok x
    | Fail i -> `Fail i
  with
  | exn -> `Exn exn
