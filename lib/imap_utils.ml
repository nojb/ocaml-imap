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

let encode_utf16be s =
  match UCoreLib.CharEncoding.encode_text
          UCoreLib.CharEncoding.utf16be (UCoreLib.Text.of_string s) with
  | `Success s -> s
  | `Error -> invalid_arg "encode_utf16be"

let recode_utf16be s =
  UCoreLib.CharEncoding.decode_string
    UCoreLib.CharEncoding.utf16be s |> UCoreLib.Text.to_string

let repl x y s =
  for i = 0 to (String.length s - 1) do
    if s.[i] = x then s.[i] <- y
  done;
  s

let encode_mutf7 =
  let re = Str.regexp "&\\|[^\x20-\x7e]+" in
  Str.global_substitute re (fun s ->
      let s = Str.matched_string s in
      if s = "&" then "&-"
      else
        let base64 = base64_encode_nopad (encode_utf16be s) in
        "&" ^ repl '/' ',' base64 ^ "-")

let decode_mutf7 =
  let re = Str.regexp "&\\([^-]+\\)?-" in
  Str.global_substitute re (fun s ->
      let s' = Str.matched_string s in
      if s' = "&-" then "&"
      else
        let s = Str.matched_group 1 s in
        base64_decode (repl ',' '/' s ^ "===") |> recode_utf16be)

let rec option_map f = function
  | [] -> []
  | x :: xs ->
    match f x with
    | Some v -> v :: option_map f xs
    | None -> option_map f xs
