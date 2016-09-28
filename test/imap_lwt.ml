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

let rec perform sock buf = function
  | Imap.Ok (res, state) ->
      Lwt.return (res, state)
  | Imap.Error _ ->
      Lwt.fail (Failure "error")
  | Imap.Send (s, p) ->
      complete (Lwt_ssl.write sock) s 0 (String.length s) >>= fun () ->
      Printf.eprintf "<<< %d\n%s\n<<<\n%!" (String.length s) s;
      perform sock buf (Imap.continue p)
  | Imap.Refill p ->
      Lwt_ssl.read sock buf 0 (String.length buf) >>= fun n ->
      Printf.eprintf ">>> %d\n%s\n>>>\n%!" n (String.sub buf 0 n);
      perform sock buf (Imap.feed p buf 0 n)

let run c cmd =
  Lwt_mutex.with_lock c.mutex
    (fun () ->
       perform c.sock c.buf (Imap.run c.state cmd) >>= fun (res, state) ->
       c.state <- state;
       Lwt.return res
    )

let connect ?(port = 993) host =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  Lwt_unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >>= fun () ->
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  Lwt_ssl.ssl_connect fd ctx >>= fun sock ->
  let buf = Bytes.create 512 in
  perform sock buf Imap.initiate >>= fun ((), state) ->
  Printf.eprintf "Got greetings!\n%!";
  Lwt.return {state; mutex = Lwt_mutex.create (); sock; buf}

let () =
  try
    Lwt_main.run (connect "imap.gmail.com" >>= fun _ -> Lwt.return_unit)
  with e ->
    let s = match e with Failure s -> s | e -> Printexc.to_string e in
    Printf.eprintf ">> Failure: %s\n%!" s;
    exit 2
