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

let recode ?nln ?encoding out_encoding src dst =
  let rec loop d e =
    match Uutf.decode d with
    | `Uchar _ as u ->
        ignore (Uutf.encode e u);
        loop d e
    | `End -> ignore (Uutf.encode e `End)
    | `Malformed _ ->
        ignore (Uutf.encode e (`Uchar Uutf.u_rep));
        loop d e
    | `Await -> assert false
  in
  let d = Uutf.decoder ?nln ?encoding src in
  let e = Uutf.encoder out_encoding dst in
  loop d e

let replace s ch1 ch2 =
  String.init (String.length s) (fun i -> if s.[i] = ch1 then ch2 else s.[i])

let encode s =
  let b = Buffer.create 0 in
  let rec a i =
    if i >= String.length s then ()
    else
      match s.[i] with
      | '&' ->
          Buffer.add_string b "&-";
          a (i + 1)
      | '\x20' .. '\x7E' as c ->
          Buffer.add_char b c;
          a (i + 1)
      | _ ->
          Buffer.add_char b '&';
          u i
  and u i =
    let upto j =
      let str = String.sub s i (j - i) and buf = Buffer.create 32 in
      recode ~encoding:`UTF_8 `UTF_16BE (`String str) (`Buffer buf);
      match Base64.encode ~pad:false (Buffer.contents buf) with
      | Ok str ->
          let str = replace str '/' ',' in
          Buffer.add_string b str;
          Buffer.add_char b '-'
      | Error (`Msg s) -> failwith s
    in
    let rec loop i =
      if i >= String.length s then upto i
      else
        match s.[i] with
        | '\x20' .. '\x7E' ->
            upto i;
            a i
        | _ -> loop (i + 1)
    in
    loop i
  in
  a 0;
  Buffer.contents b

let decode s =
  let b = Buffer.create 32 in
  let rec a i =
    if i >= String.length s then ()
    else
      match s.[i] with
      | '&' ->
          if i + 1 < String.length s && s.[i] = '-' then (
            Buffer.add_char b '&';
            a (i + 2) )
          else u (i + 1)
      | _ as c ->
          Buffer.add_char b c;
          a (i + 1)
  and u i =
    let start = i in
    let rec loop i =
      if i >= String.length s then invalid_arg "unterminated base64 input"
      else
        match s.[i] with
        | '-' ->
            let str = String.sub s start (i - start) in
            let str = replace str ',' '/' in
            ( match Base64.decode str with
            (* FIXME do we need to pad it with "===" ? *)
            | Ok str ->
                recode ~encoding:`UTF_16BE `UTF_8 (`String str) (`Buffer b)
            | Error (`Msg s) -> failwith s );
            a (i + 1)
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | ',' -> loop (i + 1)
        | _ -> invalid_arg "unexpected character"
    in
    loop i
  in
  a 0;
  Buffer.contents b
