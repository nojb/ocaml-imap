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

(* open ImapUint *)

type input =
    End
  | More

type 'a result =
    Ok of 'a * int
  | Fail of int
  | Need of int * (input -> 'a result)

type 'a t =
  Buffer.t -> int -> 'a result

let bind p f b i =
  let rec loop =
    function
      Ok (x, i) -> f x b i
    | Fail _ as x -> x
    | Need (n, k) -> Need (n, fun inp -> loop (k inp))
  in
  loop (p b i)

let (>>=) = bind

let alt p q b i =
  let rec loop =
    function
      Ok _ as x -> x
    | Fail _ -> q b i
    | Need (n, k) -> Need (n, fun inp -> loop (k inp))
  in
  loop (p b i)

let ret x _ i =
  Ok (x, i)

let fail _ i =
  Fail i

let altn ps =
  List.fold_right alt ps fail

let (>>) p q =
  p >>= fun _ -> q

let app f p =
  p >>= fun x -> ret (f x)

let opt p x =
  alt p (ret x)

let some p =
  app (fun x -> Some x) p

let none p =
  app (fun _ -> None) p

let rec rep p =
  alt (rep1 p) (ret [])

and rep1 p =
  p >>= fun x -> rep p >>= fun xs -> ret (x :: xs)

let sep1 s p =
  p >>= fun x -> rep (s >> p) >>= fun xs -> ret (x :: xs)

let sep s p =
  alt (sep1 s p) (ret [])

let rec char c b i =
  if i >= Buffer.length b then
    Need (1, function End -> Fail i | More -> char c b i)
  else if Buffer.nth b i = c then
    Ok (c, i + 1)
  else
    Fail i

let rec any_char b i =
  if i >= Buffer.length b then
    Need (1, function End -> Fail i | More -> any_char b i)
  else
    Ok (Buffer.nth b i, i + 1)

let str u b i =
  let len = String.length u in
  let rec str i =
    let left = Buffer.length b - i in
    if len <= left then
      if String.uppercase (Buffer.sub b i len) = String.uppercase u then
        Ok (u, i + len)
      else
        Fail i
    else
      Need (len - left, function End -> Fail i | More -> str i)
  in
  str i

let rec accum f b i =
  let rec loop j =
    if j >= Buffer.length b then
      Need (1, function End when i = j -> Fail j
                      | End -> Ok (Buffer.sub b i (j - i), j)
                      | More -> loop j)
    else if f (Buffer.nth b j) then
      loop (j+1)
    else if j = i then
      Fail j
    else
      Ok (Buffer.sub b i (j - i), j)
  in
  loop i

let crlf =
  str "\r\n" >> ret ()

(*
number          = 1*DIGIT
                    ; Unsigned 32-bit integer
                    ; (0 <= n < 4,294,967,296)
*)
let number =
  app Uint32.of_string (accum (function '0' .. '9' -> true | _ -> false))

let rec eof b i =
  if i >= Buffer.length b then
    Need (0, function End -> Ok ((), i) | More -> eof b i)
  else
    Fail i

let number' =
  app int_of_string (accum (function '0' .. '9' -> true | _ -> false))

let rec quoted b i =
  let bb = Buffer.create 0 in
  let rec loop i =
    if i >= Buffer.length b then
      Need (1, function End -> Fail i | More -> loop i)
    else
      match Buffer.nth b i with
        '\"' -> Ok (Buffer.contents bb, i+1)
      | '\r' | '\n' -> Fail i
      | '\\' ->
          let rec loop1 i =
            if i >= Buffer.length b then
              Need (2, function End -> Fail i | More -> loop1 i)
            else
              match Buffer.nth b i with
                '\"' | '\\' as c ->
                  Buffer.add_char bb c;
                  loop (i+1)
              | _ ->
                  Fail i
          in
          loop1 (i+1)
      | '\x01' .. '\x7f' as c ->
          Buffer.add_char bb c;
          loop (i+1)
      | _ ->
          Fail i
  in
  if i >= Buffer.length b then
    Need (2, function End -> Fail i | More -> quoted b i)
  else if Buffer.nth b i = '\"' then
    loop (i+1)
  else
    Fail i

(*
literal         = "{" number "}" CRLF *CHAR8
                    ; Number represents the number of CHAR8s
*)
let literal =
  let rec lit len b i =
    let left = Buffer.length b - i in
    if len <= left then
      Ok (Buffer.sub b i len, i + len)
    else
      Need (len - left, function End -> Fail i | More -> lit len b i)
  in
  char '{' >> number' >>= fun len -> char '}' >> str "\r\n" >> lit len

(*
string          = quoted / literal
*)
let imap_string =
  alt quoted literal

(*
astring         = 1*ASTRING-CHAR / string
*)
let is_astring_char =
  function
    '\x80' .. '\xff' | '(' | ')'
  | '{' | ' ' | '\x00' .. '\x1f' | '\x7f'
  | '%' | '*' | '\\' | '\"' -> false
  | _ -> true

let astring =
  alt (accum is_astring_char) imap_string

(*
atom            = 1*ATOM-CHAR

ATOM-CHAR       = <any CHAR except atom-specials>

atom-specials   = "(" / ")" / "{" / SP / CTL / list-wildcards /
                  quoted-specials / resp-specials

quoted-specials = DQUOTE / "\\"

resp-specials   = "]"

list-wildcards  = "%" / "*"
*)
let is_atom_char =
  function
    '\x80' .. '\xff'
  | '(' | ')' | '{' | ' ' | '\x00' .. '\x1f'
  | '\x7f' | '%' | '*' | '\\' | '\"' -> false
  | _ -> true

let atom =
  accum is_atom_char

(*
TEXT-CHAR       = <any CHAR except CR and LF>

text            = 1*TEXT-CHAR
*)
let is_text_char =
  function
    '\r' | '\n' | '\x80' .. '\xff' -> false
  | _ -> true

let text =
  accum is_text_char

(*
nz-number       = digit-nz *DIGIT
                    ; Non-zero unsigned 32-bit integer
                    ; (0 < n < 4,294,967,296)
*)
let is_nz_number_digit () =
  let first = ref true in
  function
    '0' -> not !first
  | '1' .. '9' -> true
  | _ -> false
  
let nz_number =
  app Uint32.of_string (accum (is_nz_number_digit ()))
    
let nz_number' =
  app int_of_string (accum (is_nz_number_digit ()))

let nil =
  str "nil" >> ret ()

let nstring =
  alt (some imap_string) (none nil)

let nstring' =
  app (function Some s -> s | None -> "") nstring

let rec digit b i =
  if i >= Buffer.length b then
    Need (1, function End -> Fail i | More -> digit b i)
  else
    match Buffer.nth b i with
      '0' .. '9' as c ->
        Ok (Char.code c - Char.code '0', i + 1)
    | _ ->
        Fail i

let digits2 =
  digit >>= fun n -> digit >>= fun m -> ret (n * 10 + m)

let digits4 =
  digit >>= fun n -> digit >>= fun m ->
  digit >>= fun r -> digit >>= fun s ->
  ret (n * 1000 + m * 100 + r * 10 + s)

let rec quoted_char =
  let rec aux b i =
    if i >= Buffer.length b then
      Need (1, function End -> Fail i | More -> aux b i)
    else
      match Buffer.nth b i with
        '\r' | '\n' | '\"' -> Fail i
      | '\\' ->
          let rec loop1 i =
            if i >= Buffer.length b then
              Need (1, function End -> Fail i | More -> loop1 i)
            else
              match Buffer.nth b i with
                '\\' | '\"' as c -> Ok (c, i + 1)
              | _ -> Fail i
          in
          loop1 (i + 1)
      | '\x01' .. '\x7f' as c ->
          Ok (c, i + 1)
      | _ ->
          Fail i
  in
  char '\"' >> aux >>= fun c -> char '\"' >> ret c
          
let rec string_of_length n b i =
  let rec loop i =
    let left = Buffer.length b - i in
    if n > left then
      Need (n - left, function End -> Fail (i + left) | More -> loop i)
    else
      Ok (Buffer.sub b i n, i + n)
  in
  loop i

let is_base64_char =
  function
    'a' .. 'z' | 'A' .. 'Z'
  | '0' .. '9' | '+' | '-' -> true
  | _ -> false

let base64_char =
  any_char >>= fun c -> if is_base64_char c then ret c else fail

let repn n p f =
  let rec loop i =
    if i = 0 then
      ret ()
    else
      p >>= fun x -> f x; loop (i-1)
  in
  loop n

let base64 =
  let b = Buffer.create 0 in
  let rec loop () =
    altn [
      (repn 4 base64_char (Buffer.add_char b) >>= loop);
      (repn 2 base64_char (Buffer.add_char b) >> str "==" >> ret ());
      (repn 3 base64_char (Buffer.add_char b) >> str "=" >> ret ())
    ]
  in
  loop () >>= fun () -> ret (Buffer.contents b)

let test p s =
  let b = Buffer.create 0 in
  let rec loop i =
    function
      Ok (x, _) -> x
    | Fail _ -> failwith "parsing error"
    | Need (_, k) ->
        if i >= String.length s then
          loop i (k End)
        else
          begin
            Buffer.add_char b s.[i];
            loop (i+1) (k More)
          end
  in
  loop 0 (p b 0)
