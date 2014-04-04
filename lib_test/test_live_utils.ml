let test_host = "imap.gmail.com"
let test_user = "imaplibtest@gmail.com"
let test_password = "hardpassword"

let simple_message = "\
Date: Mon, 7 Feb 1994 21:52:25 -0800 (PST)\r\n\
From: Fred Foobar <foobar@Blurdybloop.COM>\r\n\
Subject: afternoon meeting\r\n\
To: mooch@owatagu.siam.edu\r\n\
Message-Id: <B27397-0100000@Blurdybloop.COM>\r\n\
MIME-Version: 1.0\r\n\
Content-Type: TEXT/PLAIN; CHARSET=US-ASCII\r\n\r\n\
Hello Joe, do you think we can meet at 3:30 tomorrow?\r\n"


let test_folder = "ocaml-imap-test"

open Imap.Imap_types

module Imap = Imap_lwt

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

exception Skip of string

let delete_if_exists imap mbox =
  Imap.list imap "" (mbox ^ "*") >>= fun mbs ->
  Lwt_list.iter_s (fun mb -> Imap.delete imap mb.mb_name) mbs

let set_up_test f =
  let imap = Imap.make () in
  Imap.connect_ssl imap test_host
  >>= begin function
    | `Needsauth -> Imap.login imap test_user test_password
    | `Preauth -> Lwt.return ()
  end
  >>= fun () ->
  delete_if_exists imap test_folder
  >>= fun () ->
  Imap.create imap test_folder
  >>= fun () ->
  Lwt.finalize
    (fun () -> f imap)
    (fun () -> Imap.logout imap)

let get_delimiter imap =
  Imap.list imap "" "inbox"
  >>= fun mbs ->
  assert (List.length mbs > 0);
  match (List.hd mbs).mb_delimiter with
  | '\000' -> invalid_arg "get_delimiter"
  | c -> Lwt.return c

let run f ctxt =
  try Lwt_main.run (f ctxt)
  with
  | Skip s -> OUnit2.skip_if true s
  | exn -> raise exn
