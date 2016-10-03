(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 Nicolas Ojeda Bar <n.oje.bar@gmail.com>            *)

open Lwt.Infix

type connection =
  {
    mutable session: Imap.session;
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
  | Imap.No (s, state) | Imap.Bad (s, state) ->
      Lwt.fail (Failure s)
  | Imap.Error _ ->
      Printf.eprintf "Error\n%!";
      Lwt.fail (Failure "error")
  | Imap.Send (s, p) ->
      complete (Lwt_ssl.write sock) s 0 (String.length s) >>= fun () ->
      Printf.eprintf "<<< %d\n%s<<<\n%!" (String.length s) s;
      perform sock buf (Imap.continue p)
  | Imap.Refill p ->
      Printf.eprintf "Refill\n%!";
      Lwt_ssl.read sock buf 0 (String.length buf) >>= fun n ->
      Printf.eprintf ">>> %d\n%s>>>\n%!" n (String.sub buf 0 n);
      perform sock buf Imap.(continue (feed p buf 0 n))

let run c cmd =
  Lwt_mutex.with_lock c.mutex
    (fun () ->
       perform c.sock c.buf (Imap.run c.session cmd) >>= fun (res, session) ->
       c.session <- session;
       Lwt.return res
    )

let connect port host username password =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  Printf.eprintf "gethostbyname: %s => %s\n%!" host (Unix.string_of_inet_addr he.Unix.h_addr_list.(0));
  Lwt_unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >>= fun () ->
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  Lwt_ssl.ssl_connect fd ctx >>= fun sock ->
  let buf = Bytes.create 512 in
  perform sock buf Imap.(initiate (Auth.plain username password)) >>= fun ((), session) ->
  Lwt.return {session; mutex = Lwt_mutex.create (); sock; buf}

let () =
  Ssl.init ()
