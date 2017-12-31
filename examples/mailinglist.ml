(* this code is in the public domain *)

(** List all the mailing lists in a folder *)

open Lwt.Infix
module C = Imap_cmdliner

let () =
  Printexc.record_backtrace true

let list opts =
  Imap_cmdliner.connect ~read_only:true opts >>= fun imap ->
  Imap.uid_search imap Imap.Search.all >>= fun (l, _) ->
  let h = Hashtbl.create 17 in
  Lwt_list.iter_s (fun (uid : Imap.uid) ->
      Imap.(uid_fetch imap [uid] Fetch.[body_section ~section:(header_fields ["list-id"]) ()]) |>
      Lwt_stream.iter (function
      | _, Imap.Fetch.{body_section = [_, Some b]; _} -> Hashtbl.replace h b ()
      | _ -> ())
    ) l >>= fun () ->
  (* TODO: process the list-id header via MrMime *)
  Hashtbl.iter (fun k () -> Printf.printf "%s\n" k) h;
  Lwt.return_unit

let list opts = Lwt_main.run (list opts)

open Cmdliner

(* let debug = *)
(*   let doc = Arg.info ~doc:"Show debug info" ["debug"; "d"] in *)
(*   Arg.(value & flag doc) *)

let main =
  Term.(pure list $ C.client),
  Term.info "minimutt"

let () =
  match Term.eval ~catch:true main with
  | `Ok _ | `Version | `Help -> ()
  | `Error _ -> exit 1
