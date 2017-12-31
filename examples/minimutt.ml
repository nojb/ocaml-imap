(* this code is in the public domain *)

(** Mini mutt *)

open Lwt.Infix
module C = Imap_cmdliner

let () =
  Printexc.record_backtrace true

let sync opts = 
  Lwt_unix.mkdir opts.C.mailbox 0o700 >>= fun () ->
  Lwt_unix.chdir opts.C.mailbox >>= fun () ->
  C.connect opts >>= fun imap ->
  Imap.uid_search imap Imap.Search.all >>= fun (l, _) ->
  Lwt_list.iter_s (fun (uid : Imap.uid) ->
      let filename = Int32.to_string (uid :> int32) in
      Lwt_unix.openfile filename [O_WRONLY; O_CREAT; O_TRUNC] 0o600 >>= fun fd ->
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
      Lwt_stream.to_list Imap.(uid_fetch imap [uid] Fetch.[body_section ()]) >>= function
      | [_, {Imap.Fetch.body_section = [([], None), Some s]; _}] ->
          Lwt_io.write oc s >>= fun () -> Lwt_io.close oc >>= fun () ->
          Lwt_io.eprintlf "OK writing #%s" filename
      | _ ->
          Lwt_io.eprintlf "Error while retrieving #%s" filename >>= fun () ->
          Lwt_io.close oc
    ) l

let sync opts =
  Lwt_main.run (sync opts)

open Cmdliner

(* let debug = *)
(*   let doc = Arg.info ~doc:"Show debug info" ["debug"; "d"] in *)
(*   Arg.(value & flag doc) *)

let main =
  Term.(pure sync $ C.client),
  Term.info "minimutt"

let () =
  match Term.eval ~catch:true main with
  | `Ok _ | `Version | `Help -> ()
  | `Error _ -> exit 1
