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

let compare_ci s1 s2 =
  compare (String.uppercase s1) (String.uppercase s2)

(* The default logger echoes both input and output to [stderr].  In order to
   make them look a little nicer, new lines are preceded by "S: " or "C: "
   depending on the case.  Here a new line means either "\r\n" or "\n". *)
let log =
  let read_st = ref `NL in
  let write_st = ref `NL in
  fun origin str ->
    let len = String.length str in
    let header = match origin with
      | `Client -> "C: "
      | `Server -> "S: "
    in
    let rec loop st off =
      if off >= len then
        st
      else
        let header = match st with
          | `NL -> header
          | `CR -> "\r"
          | `Other -> ""
        in
        try
          let idx = String.index_from str off '\n' in
          if idx = off && (match st with `CR -> true | _ -> false) then begin
            prerr_newline ();
            loop `NL (off + 1)
          end else begin
            let hascr = idx > 0 && str.[idx-1] = '\r' in
            let str' = String.sub str off (if hascr then idx-off-1 else idx-off) in
            prerr_string header;
            prerr_string str';
            prerr_newline ();
            loop `NL (idx + 1)
          end
        with
        | Not_found ->
          let hascr = len > 0 && str.[len-1] = '\r' in
          let str' = String.sub str off (if hascr then len-off-1 else len-off) in
          prerr_string header;
          prerr_string str';
          if hascr then `CR else `Other
    in
    match origin with
    | `Client ->
      write_st := loop !write_st 0
    | `Server ->
      read_st := loop !read_st 0
