(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 Nicolas Ojeda Bar <n.oje.bar@gmail.com>            *)

open Lwt.Infix

type connection =
  {
    mutable state: Imap.state;
    mutex: Lwt_mutex.t;
    sock: Lwt_ssl.socket;
    buf: bytes;
  }

let complete f b o l =
  let rec loop o l =
    if l <= 0 then
      Lwt.return_unit
    else
      f b o l >>= function
      | 0 -> Lwt.fail End_of_file
      | n -> loop (o + n) (l - n)
  in
  loop o l

let run c cmd =
  let open Imap in
  let perform () =
    let rec loop = function
      | Ok res -> Lwt.return res
      | Error _ -> Lwt.fail (Failure "error")
      | Send (s, p) ->
          complete (Lwt_ssl.write c.sock) s 0 (String.length s) >>= fun () ->
          Printf.eprintf "<<< %d\n%s\n<<<\n%!" (String.length s) s;
          loop (continue p)
      | Refill p ->
          Lwt_ssl.read c.sock c.buf 0 (String.length c.buf) >>= fun n ->
          Printf.eprintf ">>> %d\n%s\n>>>\n%!" n (String.sub c.buf 0 n);
          loop (feed p c.buf 0 n)
    in
    loop (continue (run c.state cmd))
  in
  Lwt_mutex.with_lock c.mutex perform

let connect ?(port = 993) host =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  Lwt_unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >>= fun () ->
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  Lwt_ssl.ssl_connect fd ctx >>= fun sock ->
  let state = Imap.client in
  Lwt.return {state; mutex = Lwt_mutex.create (); sock; buf = Bytes.create 512}
