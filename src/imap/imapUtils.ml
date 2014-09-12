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

let base64_encode s =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact_pad ()) s

let base64_encode_nopad s =
  Cryptokit.transform_string (Cryptokit.Base64.encode_compact ()) s

let base64_decode s =
  Cryptokit.transform_string (Cryptokit.Base64.decode ()) s

(* let zlib_compress s = *)
(*   Cryptokit.transform_string (Cryptokit.Zlib.compress ()) s *)

(* let zlib_uncompress s = *)
(*   Cryptokit.transform_string (Cryptokit.Zlib.uncompress ()) s *)

let recode ?nln ?encoding out_encoding src dst =
  let rec loop d e = match Uutf.decode d with 
    | `Uchar _ as u -> ignore (Uutf.encode e u); loop d e 
    | `End -> ignore (Uutf.encode e `End)
    | `Malformed _ -> ignore (Uutf.encode e (`Uchar Uutf.u_rep)); loop d e 
    | `Await -> assert false
  in
  let d = Uutf.decoder ?nln ?encoding (`String src) in 
  let e = Uutf.encoder out_encoding (`Buffer dst) in
  loop d e
    
let replace s ch1 ch2 =
  for i = 0 to String.length s - 1 do
    if s.[i] = ch1 then s.[i] <- ch2
  done

let encode_mutf7 s =
  let b = Buffer.create 0 in
  let rec tr_ascii i =
    if i >= String.length s then
      ()
    else match s.[i] with
      | '&' ->
          Buffer.add_string b "&-";
          tr_ascii (i + 1)
      | '\x20' .. '\x7e' as c ->
          Buffer.add_char b c;
          tr_ascii (i + 1)
      | _ ->
          Buffer.add_char b '&';
          tr_uni i
  and tr_uni i =
    let add_until j =
      let str = String.sub s i (j - i) in
      (* Printf.eprintf "recoding: %S\n%!" str; *)
      let buf = Buffer.create 0 in
      recode ~encoding:`UTF_8 `UTF_16BE str buf;
      let str = Buffer.contents buf in
      let str = base64_encode_nopad str in
      (* Printf.eprintf "result: %S\n%!" str; *)
      replace str '/' ',';
      Buffer.add_string b str;
      Buffer.add_char b '-'
    in
    let rec loop j =
      if j >= String.length s then
        add_until j
      else match s.[j] with
        | '\x20' .. '\x7e' ->
            add_until j;
            tr_ascii j
        | _ ->
            loop (j + 1)
    in
    loop i
  in
  tr_ascii 0;
  Buffer.contents b

let decode_mutf7 s =
  let b = Buffer.create 0 in
  let rec tr_ascii i =
    if i >= String.length s then
      ()
    else match s.[i] with
      | '&' ->
          if i+1 < String.length s && s.[i] = '-' then
            begin
              Buffer.add_char b '&';
              tr_ascii (i + 2)
            end
          else
            tr_uni (i + 1)
      | _ as c ->
          Buffer.add_char b c;
          tr_ascii (i + 1)
  and tr_uni i =
    let decode_until j =
      let str = String.sub s i (j - i) in
      (* Printf.eprintf "decoding: i=%d j=%d str=%S\n%!" i j str; *)
      replace str ',' '/';
      let str = base64_decode str in (* FIXME do we need to pad it with "===" ? *)
      recode ~encoding:`UTF_16BE `UTF_8 str b
    in
    let rec loop i =
      if i >= String.length s then
        begin
          Printf.eprintf "warning: decode_mutf7: unterminated base64 input; ignoring.\n%!";
          decode_until i;
          tr_ascii i
        end
      else match s.[i] with
        | '-' ->
            decode_until i;
            tr_ascii (i + 1)
        | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | ',' ->
            loop (i+1)
        | _ as c ->
            Printf.eprintf "warning: decode_mutf7: unexpected character %C; ignoring.\n%!" c;
            loop (i + 1)
    in
    loop i
  in
  tr_ascii 0;
  Buffer.contents b
