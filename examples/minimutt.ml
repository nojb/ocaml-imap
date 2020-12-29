(* this code is in the public domain *)

(** Mini mutt *)

open Lwt.Infix

let () = Printexc.record_backtrace true

let sync server ?port username password mailbox =
  Lwt_unix.mkdir mailbox 0o700 >>= fun () ->
  Lwt_unix.chdir mailbox >>= fun () ->
  Imap.connect server ?port username password ~read_only:true mailbox
  >>= fun imap ->
  Imap.uid_search imap Imap.Search.all >>= fun (l, _) ->
  Lwt_list.iter_s
    (fun (uid : Imap.uid) ->
      let filename = Int32.to_string (uid :> int32) in
      Lwt_unix.openfile filename [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600
      >>= fun fd ->
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
      Lwt_stream.to_list
        Imap.(uid_fetch imap [ uid ] Fetch.[ Request.body_section () ])
      >>= function
      | [
          (_, { Imap.Fetch.Response.body_section = [ (([], None), Some s) ]; _ });
        ] ->
          Lwt_io.write oc s >>= fun () ->
          Lwt_io.close oc >>= fun () ->
          Lwt_io.eprintlf "OK writing #%s" filename
      | _ ->
          Lwt_io.eprintlf "Error while retrieving #%s" filename >>= fun () ->
          Lwt_io.close oc)
    l

let sync server port username password mailbox =
  Lwt_main.run (sync server ?port username password mailbox)

open Cmdliner

let server =
  let doc = Arg.info ~docv:"SERVER" ~doc:"Server hostname" [] in
  Arg.(required & pos 0 (some string) None & doc)

let port =
  let doc = Arg.info ~docv:"PORT" ~doc:"Server port" [ "port"; "p" ] in
  Arg.(value & opt (some int) None & doc)

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
  ( Term.(pure sync $ server $ port $ username $ password $ mailbox),
    Term.info "minimutt" )

let () =
  match Term.eval ~catch:true main with
  | `Ok _ | `Version | `Help -> ()
  | `Error _ -> exit 1
