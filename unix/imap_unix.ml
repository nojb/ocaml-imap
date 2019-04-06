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

let ssl_init =
  Lazy.from_fun Ssl.init

type t =
  {
    sock: Ssl.socket;
    mutable imap: Imap.t;
  }

let connect ?(port = 993) host =
  Lazy.force ssl_init;
  let sock =
    let he = Unix.gethostbyname host in
    let saddr = Unix.ADDR_INET (he.Unix.h_addr_list.(0), port) in
    let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
    Ssl.open_connection_with_context ctx saddr
  in
  {sock; imap = Imap.init}

let disconnect t =
  Ssl.shutdown_connection t.sock

let fully f off len =
  let rec loop off len =
    if len <= 0 then ()
    else
      let len1 = f off len in
      loop (off + len1) (len - len1)
  in
  loop off len

let read_chunk _ = ""
let read_exactly _ _ = ""

let run t cmd =
  let rec loop = function
    | `More (None, imap) ->
        let s = read_chunk t.sock in
        loop (Imap.Run.step imap s)
    | `Exact (Some n, imap) ->
        let s = read_exactly t.sock n in
        loop (Imap.Run.step imap s)
    | `Run (s, cont) ->
        fully (Ssl.write_substring t.sock s) 0 (String.length s);
        loop cont
    | `Done (r, imap) ->
        t.imap <- imap;
        r
  in
  loop (Imap.run t.imap cmd)
