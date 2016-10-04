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

let run f c =
  Lwt_mutex.with_lock c.mutex
    (fun () ->
       perform c.sock c.buf (f c.session) >>= fun (res, session) ->
       c.session <- session;
       Lwt.return res
    )

let run0 f c =
  run (fun ss -> f ss) c

let run1 f c a =
  run (fun ss -> f ss a) c

let run2 f c a b =
  run (fun ss -> f ss a b) c

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

let login = run2 Imap.login
let capability = run0 Imap.capability
let create = run1 Imap.create
let delete = run1 Imap.delete
let rename = run2 Imap.rename
let logout = run0 Imap.logout
let noop = run0 Imap.noop
let subscribe = run1 Imap.subscribe
let unsubscribe = run1 Imap.unsubscribe
let list c ?ref m =
  run (fun ss -> Imap.list ss ?ref m) c
let lsub c ?ref m =
  run (fun ss -> Imap.lsub ss ?ref m) c
let status = run2 Imap.status
let copy = run2 Imap.copy
let uid_copy = run2 Imap.uid_copy
let check = run0 Imap.check
let close = run0 Imap.close
let expunge = run0 Imap.expunge
let uid_search = run1 Imap.uid_search
let search = run1 Imap.search
let select = run1 Imap.select
let condstore_select = run1 Imap.condstore_select
let examine = run1 Imap.examine
let condstore_examine = run1 Imap.condstore_examine
let append c m ?flags s =
  run (fun ss -> Imap.append ss m ?flags s) c
let fetch c ?changed ?vanished set req =
  run (fun ss -> Imap.fetch ss ?changed ?vanished set req) c
let uid_fetch c ?changed ?vanished set req =
  run (fun ss -> Imap.uid_fetch ss ?changed ?vanished set req) c
let add_flags c ?silent ?unchanged set flags =
  run (fun ss -> Imap.add_flags ss ?silent ?unchanged set flags) c
let set_flags c ?silent ?unchanged set flags =
  run (fun ss -> Imap.set_flags ss ?silent ?unchanged set flags) c
let remove_flags c ?silent ?unchanged set flags =
  run (fun ss -> Imap.remove_flags ss ?silent ?unchanged set flags) c
let uid_add_flags c ?silent ?unchanged set flags =
  run (fun ss -> Imap.uid_add_flags ss ?silent ?unchanged set flags) c
let uid_set_flags c ?silent ?unchanged set flags =
  run (fun ss -> Imap.uid_set_flags ss ?silent ?unchanged set flags) c
let uid_remove_flags c ?silent ?unchanged set flags =
  run (fun ss -> Imap.uid_remove_flags ss ?silent ?unchanged set flags) c
let add_labels c ?silent ?unchanged set labels =
  run (fun ss -> Imap.add_labels ss ?silent ?unchanged set labels) c
let set_labels c ?silent ?unchanged set labels =
  run (fun ss -> Imap.set_labels ss ?silent ?unchanged set labels) c
let remove_labels c ?silent ?unchanged set labels =
  run (fun ss -> Imap.remove_labels ss ?silent ?unchanged set labels) c
let uid_add_labels c ?silent ?unchanged set labels =
  run (fun ss -> Imap.uid_add_labels ss ?silent ?unchanged set labels) c
let uid_set_labels c ?silent ?unchanged set labels =
  run (fun ss -> Imap.uid_set_labels ss ?silent ?unchanged set labels) c
let uid_remove_labels c ?silent ?unchanged set labels =
  run (fun ss -> Imap.uid_remove_labels ss ?silent ?unchanged set labels) c
let enable = run1 Imap.enable
