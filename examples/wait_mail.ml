(* this code is in the public domain *)

(** {1:ex Example: checking for new mail}

    [wait_mail host port user pass mbox] logs into the IMAP server
    [host] on port [port] over {{:https://github.com/savonet/ocaml-ssl}SSL},
    authenticates user [user] with password [pass] and watches mailbox [mbox] until a
    new message arrives.  When a new message arrives, it outputs the sender's name
    and address and stops.

    See the
    {{:https://github.com/nojb/ocaml-imap/blob/master/test/wait_mail.ml}wait_mail.ml}
    file for a more complete version of this example.

    Setting [debug_flag := true] will output all the data exchanged with the
    server which can be quite instructive. *)

open Lwt.Infix

let wait_mail server port username password mailbox =
  Imap.connect server username password mailbox >>= fun imap ->
  let uidnext = Imap.uidnext imap in
  Imap.idle imap >>= fun () ->
  Imap.uid_search imap (uidnext) >>= function
  | n :: _ ->
      Imap.fetch imap n `Envelope >>= fun e ->
      Lwt_io.eprintlf "..." >>= fun () ->
  (* | `Envelope e -> *)
  (*     begin match e.Imap.env_from with *)
  (*     | [] -> name *)
  (*     | ad :: _ -> *)
  (*         Some (Printf.sprintf "\"%s\" <%s@%s>" *)
  (*                 ad.Imap.ad_name ad.Imap.ad_mailbox ad.Imap.ad_host) *)
  (*     end *)
      Imap.disconnect imap
  | [] ->
      assert false

open Cmdliner

let server =
  let doc = Arg.info ~docv:"SERVER" ~doc:"Server hostname" [] in
  Arg.(required & pos 0 (some string) None & doc)

let port =
  let doc = Arg.info ~docv:"PORT" ~doc:"Server port" ["port"; "p"] in
  Arg.(value & opt int 993 & doc)

let username =
  let doc = Arg.info ~docv:"USERNAME" ~doc:"Username" [] in
  Arg.(required & pos 1 (some string) None & doc)

let password =
  let doc = Arg.info ~docv:"PASSWORD" ~doc:"Password" [] in
  Arg.(required & pos 2 (some string) None & doc)

let mailbox =
  let doc = Arg.info ~docv:"MAILBOX" ~doc:"Mailbox to watch" [] in
  Arg.(required & pos 3 (some string) None & doc)

(* let debug = *)
(*   let doc = Arg.info ~doc:"Show debug info" ["debug"; "d"] in *)
(*   Arg.(value & flag doc) *)

let main =
  Term.(pure wait_mail $ server $ port $ username $ password $ mailbox),
  Term.info "wait_mail"

let () =
  match Term.eval ~catch:true main with
  | `Ok _ | `Version | `Help -> ()
  | `Error _ -> exit 1
