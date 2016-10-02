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

let run c cmd =
  let res, session = perform c.sock c.buf (Imap.run c.session cmd) in
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
