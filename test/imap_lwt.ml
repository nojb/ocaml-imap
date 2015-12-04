(* this code is in the public domain *)

open Lwt.Infix

type connection =
  {
    c : Imap.connection;
    sock : Lwt_ssl.socket;
  }

let rec process c : Imap.result -> _ = function
  | `Read (s, i, l, k) ->
      Lwt_ssl.read c.sock s i l >>= fun n ->
      process c (k n)
  | `Write (o, pos, len, k) ->
      Lwt_ssl.write c.sock o pos len >>= fun n ->
      process c (k n)
  | `Untagged _ as r ->
      (* LTerm.eprintl "Untagged" >>= fun () -> *)
      Lwt.return r
  | `Ok _ ->
      Lwt.return `Ok
  | `Error e ->
      Imap.pp_error Format.str_formatter e;
      Lwt.fail (Failure (Format.flush_str_formatter ()))

let run c cmd =
  let next = ref (fun () -> Imap.run c.c cmd) in
  Lwt_stream.from (fun () ->
      process c (!next ()) >>= function
      | `Untagged (u, k) ->
          next := k;
          Lwt.return (Some u)
      | `Ok ->
          Lwt.return None
    )

let connect ?(port = 993) host =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  Lwt_unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >>= fun () ->
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  Lwt_ssl.ssl_connect fd ctx >>= fun sock ->
  let c, v = Imap.connection () in
  let c =
    {
      c;
      sock;
    }
  in
  let rec handle = function
    | `Untagged (_, k) -> process c (k ()) >>= handle
    | `Ok -> Lwt.return_unit
  in
  process c v >>= handle >>= fun () -> Lwt.return c
