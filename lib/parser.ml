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
open Response

exception Error of string * int

type buffer =
  {
    read_line: (string -> unit) -> unit;
    mutable line: string;
    mutable pos: int;
  }

let is_eol buf =
  buf.pos >= String.length buf.line

let curr buf =
  if buf.pos >= String.length buf.line then
    '\000'
  else
    buf.line.[buf.pos]

let next buf =
  buf.pos <- buf.pos + 1

let error buf =
  raise (Error (buf.line, buf.pos))

let char c buf =
  if curr buf = c then next buf else error buf

let take_while1 f buf =
  let pos0 = buf.pos in
  let pos = ref pos0 in
  while !pos < String.length buf.line && f buf.line.[!pos] do
    incr pos
  done;
  if pos0 = !pos then error buf;
  buf.pos <- !pos;
  String.sub buf.line pos0 (!pos - pos0)

let is_atom_char = function
  | '(' | ')' | '{' | ' '
  | '\x00' .. '\x1F' | '\x7F'
  | '%' | '*' | '"' | '\\' | ']' -> false
  | '\x01' .. '\x7F' -> true
  | _ -> false

let atom =
  take_while1 is_atom_char

let is_astring_char c =
  is_atom_char c || c = ']'

let is_text_char = function
  | '\r' | '\n' -> false
  | '\x01' .. '\x7F' -> true
  | _ -> false

let text buf =
  take_while1 is_text_char buf

let is_text_other_char c =
  is_text_char c && (c <> ']')

let text_1 =
  take_while1 is_text_other_char

let resp_text_code buf k =
  let open Code in
  let k code = char ']' buf; char ' ' buf; k code in
  char '[' buf;
  match atom buf with
  | "ALERT" ->
      k ALERT
  (* | "BADCHARSET" ->
   *     many (sp *> astring) >>| (fun l -> BADCHARSET l)
   * | "CAPABILITY" ->
   *     many (sp *> capability) >>| (fun l -> CAPABILITY l) *)
  | "PARSE" ->
      k PARSE
  (* | "PERMANENTFLAGS" ->
   *     sp *> psep_by sp flag_perm >>| (fun l -> PERMANENTFLAGS l) *)
  | "READ-ONLY" ->
      k READ_ONLY
  | "READ-WRITE" ->
      k READ_WRITE
  | "TRYCREATE" ->
      k TRYCREATE
  (* | "UIDNEXT" ->
   *     sp *> nz_number >>| (fun n -> UIDNEXT n)
   * | "UIDVALIDITY" ->
   *     sp *> nz_number >>| (fun n -> UIDVALIDITY n)
   * | "UNSEEN" ->
   *     sp *> nz_number >>| (fun n -> UNSEEN n) *)
  | "CLOSED" ->
      k CLOSED
  (* | "HIGHESTMODSEQ" ->
   *     sp *> mod_sequence_value >>| (fun n -> HIGHESTMODSEQ n) *)
  | "NOMODSEQ" ->
      k NOMODSEQ
  (* | "MODIFIED" ->
   *     sp *> set >>| (fun l -> MODIFIED l)
   * | "APPENDUID" ->
   *     sp *> pair sp nz_number append_uid >>| (fun (n, uid) -> APPENDUID (n, uid))
   * | "COPYUID" ->
   *     sp *> triple sp nz_number set set >>| (fun (n, s1, s2) -> COPYUID (n, s1, s2)) *)
  | "UIDNOTSTICKY" ->
      k UIDNOTSTICKY
  | "COMPRESSIONACTIVE" ->
      k COMPRESSIONACTIVE
  | "USEATTR" ->
      k USEATTR
  | a ->
      let x = if curr buf = ' ' then Some (text_1 buf) else None in
      k (OTHER (a, x))

let resp_text buf k =
  let code buf k =
    match curr buf with
    | '[' -> resp_text_code buf (fun code -> k (Some code))
    | _ -> k None
  in
  code buf (fun code -> k code (text buf))

let is_tag_char = function
  | '+' -> false
  | c -> is_astring_char c

let tag =
  take_while1 is_tag_char

let resp_cond_state buf k =
  let open Response.State in
  match atom buf with
  | "OK" ->
      char ' ' buf;
      resp_text buf (fun code text -> k (OK (code, text)))
  | "NO" ->
      char ' ' buf;
      resp_text buf (fun code text -> k (NO (code, text)))
  | "BAD" ->
      char ' ' buf;
      resp_text buf (fun code text -> k (BAD (code, text)))
  | _ ->
      error buf

let response buf k =
  match curr buf with
  | '+' ->
      next buf;
      if curr buf = ' ' then next buf;
      if not (is_eol buf) then resp_text buf (fun _ x -> k (Cont x))
      else k (Cont "")
  | '*' ->
      error buf
  | _ ->
      let tag = tag buf in
      char ' ' buf;
      resp_cond_state buf (fun state -> k (Tagged (tag, state)))

let parse s =
  let buf =
    { read_line = (fun _k -> assert false);
      line = s;
      pos = 0 }
  in
  let result = ref (Cont "") in
  match response buf (fun u -> result := u) with
  | () ->
      !result |> Response.sexp_of_t |> Sexplib.Sexp.to_string_hum |> print_endline
  | exception Error (line, pos) ->
      Printf.eprintf "Parsing error:\n%s\n%s^\n" line (String.make pos ' ')

let%expect_test _ =
  let tests =
    [
      "+ YGgGCSqGSIb3EgECAgIAb1kwV6ADAgEFoQMCAQ+iSzBJoAMC";
      "+ YDMGCSqGSIb3EgECAgIBAAD/////6jcyG4GE3KkTzBeBiVHe";
      "+";
      "+ Ready for literal data";
      "+ Ready for additional command text";
      "abcd OK CAPABILITY completed";
      "efgh OK STARTLS completed";
      "ijkl OK CAPABILITY completed";
      "a002 OK NOOP completed";
      "a047 OK NOOP completed";
      "A023 OK LOGOUT completed";
      "a001 OK CAPABILITY completed";
      "a002 OK Begin TLS negotiation now";
      "a003 OK CAPABILITY completed";
      "a004 OK LOGIN completed";
      "A001 OK GSSAPI authentication successful";
      "a001 OK LOGIN completed";
      "A142 OK [READ-WRITE] SELECT completed";
    ]
  in
  List.iter parse tests;
  [%expect {| |}]
