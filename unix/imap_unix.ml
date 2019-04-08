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

type t =
  {
    sock: Ssl.socket;
    mutable tag: int;
  }

module L : sig
  type t = string
  val is_complete: t -> int option
end = struct
  type t = string

  type state =
    | Begin
    | Int of int
    | Cr of int
    | Lf of int

  let is_complete s =
    let rec loop state i =
      if i >= String.length s then
        None
      else begin
        match state, s.[i] with
        | Begin, '{' ->
            loop (Int 0) (i+1)
        | Int n, ('0'..'9' as c) ->
            loop (Int (10 * n + Char.code c - Char.code '0')) (i+1)
        | Int n, '}' ->
            loop (Cr n) (i+1)
        | Begin, '\r' ->
            loop (Lf 0) (i+1)
        | Cr n, '\r' ->
            loop (Lf n) (i+1)
        | Lf 0, '\n' ->
            Some (i+1)
        | Lf n, '\n' ->
            loop Begin (i+1+n)
        | _ ->
            loop Begin i
      end
    in
    loop Begin 0
end

let parse {sock; buf; len; _} =
  let rec loop () =
    let n = Ssl.read sock buf len (Bytes.length buf - len) in
    match L.is_complete buf with
    | Some (n, pos) ->
        let line = Buffer.sub buf 0 pos in
        Bytes.blit buf pos buf 0 (len - pos);
        line
    | None ->
        loop ()
  in
  let t, u = Lwt.wait () in
  loop () >>= fun s ->
  let buf = {Parser.s; p = 0} in
  Parser.response buf (function
      | Ok x ->
          Lwt.wakeup u x
      | Pervasives.Error (s, pos) ->
          Lwt.wakeup_exn u (Error (Decode_error (s, pos)))
    );
  t

let rec send imap r process res =
  match r with
  | Encoder.End ->
      Lwt.return res
  | Wait r ->
      let rec loop res =
        parse imap >>= function
        | Response.Cont _ ->
            send imap r process res
        | Untagged u ->
            loop (process res u)
        | Tagged _ ->
            Lwt.fail (Failure "not expected")
      in
      Lwt_io.flush imap.oc >>= fun () -> loop res
  | Crlf r ->
      Lwt_io.write imap.oc "\r\n" >>= fun () ->
      send imap r process res
  | Raw (s, r) ->
      Lwt_io.write imap.oc s >>= fun () ->
      send imap r process res

let send imap r process res =
  let r = r Encoder.End in
  (* Printf.eprintf "%s\n%!" (Sexplib.Sexp.to_string_hum (Encoder.sexp_of_s r)); *)
  send imap r process res >>= fun res ->
  Lwt_io.flush imap.oc >>= fun () ->
  Lwt.return res

let wrap_process f res = function
  | Response.Untagged.State (NO (_, s) | BAD (_, s)) ->
      raise (Error (Server_error s))
  | u ->
      f res u

let run t {Imap.format; default; process; finish} =
  let process = wrap_process process in
  let tag = tag imap in
  let r = Encoder.(raw tag ++ format & crlf) in
  let rec loop res =
    parse imap >>= function
    | Response.Cont _ ->
        Lwt.fail_with "unexpected"
    | Untagged u ->
        loop (process res u)
    | Tagged (_, (NO (_code, s) | BAD (_code, s))) ->
        Lwt.fail (Error (Server_error s))
    | Tagged (_, OK _) ->
        imap.tag <- imap.tag + 1;
        Lwt.return res
  in
  send imap r process default >>= loop >|= finish

let ssl_init =
  Lazy.from_fun Ssl.init

let connect ?(port = 993) host =
  Lazy.force ssl_init;
  let sock =
    let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
    let he = Unix.gethostbyname host in
    let sa = Lwt_unix.ADDR_INET (he.Unix.h_addr_list.(0), port) in
    Ssl.open_connection_with_context ctx sa
  in
  let t = {sock; tag = 1} in
  parse imap >>= function
  | Response.Untagged _ ->
      run imap (login username password) >|= fun () -> imap
  | Tagged _ | Cont _ ->
      failwith "unexpected response"

let disconnect t =
  run t Imap.logout;
  Ssl.shutdown_connection t.sock
