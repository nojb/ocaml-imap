(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 Nicolas Ojeda Bar <n.oje.bar@gmail.com>            *)

type connection =
  {
    mutable session: Imap.session;
    sock: Ssl.socket;
    buf: bytes;
  }

let complete f b o l =
  let rec loop o l =
    if l > 0 then
      match f b o l with
      | 0 -> raise End_of_file
      | n -> loop (o + n) (l - n)
  in
  loop o l

let rec perform sock buf = function
  | Imap.Ok (res, state) ->
      (res, state)
  | Imap.No (s, _) | Imap.Bad (s, _) ->
      failwith s
  | Imap.Error _ ->
      Printf.eprintf "Error\n%!";
      failwith "error"
  | Imap.Send (s, p) ->
      complete (Ssl.write sock) s 0 (String.length s);
      Printf.eprintf "<<< %d\n%s<<<\n%!" (String.length s) s;
      perform sock buf (Imap.continue p)
  | Imap.Refill p ->
      (* Printf.eprintf "Refill\n%!"; *)
      let n = Ssl.read sock buf 0 (String.length buf) in
      Printf.eprintf ">>> %d\n%s>>>\n%!" n (String.sub buf 0 n);
      perform sock buf Imap.(continue (feed p buf 0 n))

(* let run c cmd = *)
(*   let res, session = perform c.sock c.buf (Imap.run c.session cmd) in *)
(*   c.session <- session; *)
(*   res *)

let idle c =
  let res, session = perform c.sock c.buf (Imap.idle c.session) in
  c.session <- session;
  res

let connect port host username password =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let he = Unix.gethostbyname host in
  Printf.eprintf "gethostbyname: %s => %s\n%!" host (Unix.string_of_inet_addr he.Unix.h_addr_list.(0));
  Unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port));
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  let sock = Ssl.embed_socket fd ctx in
  Ssl.connect sock;
  let buf = Bytes.create 512 in
  let (), session =
    perform sock buf Imap.(initiate (Auth.plain username password))
  in
  {session; sock; buf}

let () =
  Ssl.init ()

let run f c =
  let res, session = perform c.sock c.buf (f c.session) in
  c.session <- session;
  res

let run0 f c =
  run (fun ss -> f ss) c

let run1 f c a =
  run (fun ss -> f ss a) c

let run2 f c a b =
  run (fun ss -> f ss a b) c

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
