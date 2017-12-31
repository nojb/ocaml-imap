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
module C = Imap_cmdliner

let wait_mail opts =
  C.connect opts >>= fun imap ->
  let rec loop () =
    let uidnext =
      match Imap.uidnext imap with
      | Some n -> n
      | None -> failwith "Could not determine UIDNEXT"
    in
    Imap.poll imap >>= fun () ->
    Imap.uid_search imap Imap.Search.(unseen && uid [uidnext]) >>= function
    | (n :: _), _ ->
        Lwt_stream.to_list (Imap.uid_fetch imap [n] [Imap.Fetch.envelope]) >>= begin function
        | [_, {envelope = Some {Imap.env_from = {Imap.ad_name; ad_mailbox; ad_host; _} :: _; _}; _}; _] ->
            Lwt_io.printlf "New mail! (from \"%s\" <%s@%s>)" ad_name ad_mailbox ad_host >>= fun () ->
            Imap.disconnect imap
        | _ ->
            loop ()
        end
    | [], _ ->
        loop ()
  in
  loop ()

let wait_mail opts =
  Lwt_main.run (wait_mail opts)

open Cmdliner

(* let debug = *)
(*   let doc = Arg.info ~doc:"Show debug info" ["debug"; "d"] in *)
(*   Arg.(value & flag doc) *)

let main =
  Term.(pure wait_mail $ C.client),
  Term.info "wait_mail"

let () =
  match Term.eval ~catch:true main with
  | `Ok _ | `Version | `Help -> ()
  | `Error _ -> exit 1
