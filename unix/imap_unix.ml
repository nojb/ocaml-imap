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

open Imap

module L = struct
  type state = Begin | Int of int | Cr of int | Lf of int

  let is_complete s len =
    assert (len <= Bytes.length s);
    let rec loop state i =
      if i >= len then None
      else
        match (state, Bytes.get s i) with
        | Begin, '{' -> loop (Int 0) (i + 1)
        | Int n, ('0' .. '9' as c) ->
            loop (Int ((10 * n) + Char.code c - Char.code '0')) (i + 1)
        | Int n, '}' -> loop (Cr n) (i + 1)
        | Begin, '\r' -> loop (Lf (-1)) (i + 1)
        | Cr n, '\r' -> loop (Lf n) (i + 1)
        | Lf -1, '\n' -> Some (i + 1)
        | Lf n, '\n' -> loop Begin (i + 1 + n)
        | _ -> loop Begin (i + 1)
    in
    loop Begin 0
end

type t = {
  mutable imap : Cmd.state;
  sock : Ssl.socket;
  mutable tag : int;
  mutable buf : Bytes.t;
  mutable len : int;
}

let parse t =
  let rec loop () =
    match L.is_complete t.buf t.len with
    | Some pos ->
        let s = Bytes.sub_string t.buf 0 pos in
        t.len <- t.len - pos;
        Bytes.blit t.buf pos t.buf 0 t.len;
        Printf.eprintf "S: %s\n%!" (String.sub s 0 (String.length s - 2));
        s
    | None ->
        let n = Ssl.read t.sock t.buf t.len (Bytes.length t.buf - t.len) in
        t.len <- t.len + n;
        loop ()
  in
  let s = loop () in
  match Parser.response s 0 with
  | Ok x -> x
  | Error _ -> failwith "parsing error"

let really f ofs len =
  let rec loop ofs len =
    if len <= 0 then ()
    else
      let n = f ofs len in
      loop (ofs + n) (len - n)
  in
  loop ofs len

let run t cmd notify =
  let rec loop t imap = function
    | Cmd.Send (s, next) ->
        let b = Bytes.unsafe_of_string s in
        Printf.eprintf "C: %s\n%!" (Bytes.sub_string b 0 (Bytes.length b - 2));
        really (Ssl.write t.sock b) 0 (Bytes.length b);
        loop t imap next
    | Wait next -> loop t imap (next (parse t))
    | Partial (imap, x, next) ->
        notify x;
        loop t imap next
    | Done (imap, x) -> (imap, x)
    | Error s -> failwith s
  in
  let imap, state = Cmd.run t.imap cmd in
  let imap, x = loop t imap state in
  t.imap <- imap;
  x

let ssl_init = Lazy.from_fun Ssl.init

let connect ?(port = 993) host =
  Lazy.force ssl_init;
  let sock =
    let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
    let he = Unix.gethostbyname host in
    let sa = Unix.ADDR_INET (he.Unix.h_addr_list.(0), port) in
    Ssl.open_connection_with_context ctx sa
  in
  let t =
    { imap = Cmd.initial; sock; tag = 1; buf = Bytes.create 4096; len = 0 }
  in
  match parse t with
  | Response.Untagged _ -> t
  | Tagged _ | Cont _ -> failwith "unexpected response"

let disconnect t = Ssl.shutdown_connection t.sock
