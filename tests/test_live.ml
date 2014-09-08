open OUnit2

open Imap.Imap_types

open Test_live_utils

let test_capability ctxt =
  set_up_test (fun imap ->
      Imap.capability imap >|= fun caps ->
      assert_bool "IMAP4rev1 capability"
        (List.exists (function
             | `NAME cap -> String.lowercase cap = "imap4rev1"
             | _ -> false) caps))

let test_noop ctxt =
  set_up_test (fun imap -> Imap.noop imap)

let test_id ctxt =
  set_up_test (fun imap ->
      if not (Imap.has_id imap) then Lwt.fail (Skip "server does not support ID")
      else
        Imap.id imap [] >|= fun _ -> ())

let test_compress ctxt =
  set_up_test (fun imap ->
      if not (Imap.has_compress_deflate imap) then
        Lwt.fail (Skip "server does not support COMPRESS=DEFLATE")
      else
        Imap.compress imap >>= fun () ->
        Imap.noop imap)

let test_select ctxt =
  set_up_test (fun imap ->
      Imap.select imap test_folder >|= fun () ->
      assert_bool "open read-write" ((Imap.selection_info imap).sel_perm = `READ_WRITE))
      (* Lwt.catch (fun () -> *)
      (*     Imap.select imap "does not exist" >>= fun () -> *)
      (*     Lwt.fail (Failure "assert false")) *)
      (*   (function Imap.BAD -> Lwt.return () | exn -> Lwt.fail exn)) *)

let test_examine ctxt =
  set_up_test (fun imap ->
      Imap.examine imap test_folder >|= fun () ->
      assert_bool "open read-write" ((Imap.selection_info imap).sel_perm = `READ_ONLY))

let test_create ctxt =
  set_up_test (fun imap ->
      get_delimiter imap >>= fun delim ->
      let subname = Printf.sprintf "%s%csubfolder" test_folder delim in
      Imap.create imap subname >>= fun () ->
      Imap.select imap subname)

let test_delete ctxt =
  set_up_test (fun imap ->
      Imap.delete imap test_folder >>= fun () ->
      Lwt.catch (fun () ->
          Imap.select imap test_folder >>= fun () ->
          Lwt.fail (Failure "assert false"))
        (function Imap.NO -> Lwt.return () | exn -> Lwt.fail exn))

let test_rename ctxt =
  set_up_test (fun imap ->
      let newname = test_folder ^ "-renamed" in
      Imap.rename imap test_folder newname >>= fun () ->
      Imap.select imap newname >>= fun () ->
      Lwt.catch (fun () ->
          Imap.select imap test_folder >>= fun () ->
          Lwt.fail (Failure "assert false"))
        (function Imap.NO -> Lwt.return () | exn -> Lwt.fail exn))

let test_subscribe ctxt =
  set_up_test (fun imap ->
      Imap.subscribe imap test_folder)

let test_unsubscribe ctxt =
  set_up_test (fun imap ->
      Imap.unsubscribe imap test_folder)

let test_list ctxt =
  set_up_test (fun imap ->
      Imap.list imap "" "%" >|= fun mblist ->
      assert_bool "test_folder is listed"
        (List.exists (fun mb -> mb.mb_name = test_folder) mblist))

let test_lsub ctxt =
  set_up_test (fun imap ->
      Imap.lsub imap "" "%" >|= fun mblist ->
      assert_bool "inbox is subscribed"
        (List.exists (fun mb -> mb.mb_name = "INBOX") mblist))

let test_status ctxt =
  set_up_test (fun imap ->
      Imap.status imap "inbox" [`MESSAGES; `UNSEEN; `UIDVALIDITY] >>= fun _ ->
      Lwt.return ())

let test_append ctxt =
  set_up_test (fun imap ->
      Imap.append imap test_folder simple_message >>= fun _ ->
      Lwt.return ())

let test_check ctxt =
  set_up_test (fun imap ->
      Imap.select imap test_folder >>= fun () ->
      Imap.check imap)

let test_close ctxt =
  set_up_test (fun imap ->
      Imap.select imap test_folder >>= fun () ->
      Imap.close imap >>= fun () ->
      Lwt.catch (fun () ->
          Imap.close imap >>= fun () ->
          Lwt.fail (Failure "assert false"))
        (function Imap.NO | Imap.BAD -> Lwt.return () | exn -> Lwt.fail exn))

let test_expunge ctxt =
  set_up_test (fun imap ->
      Imap.select imap test_folder >>= fun () ->
      Imap.expunge imap)

let test_uid_expunge ctxt =
  Lwt.fail (Skip "yet to be done")
  
let suite =
  "test_live" >::: [
    "test_noop" >:: run test_noop;
    "test_id" >:: run test_id;
    "test_compress" >:: run test_compress;
    "test_select" >:: run test_select;
    "test_examine" >:: run test_examine;
    "test_create" >:: run test_create;
    "test_delete" >:: run test_delete;
    "test_rename" >:: run test_rename;
    "test_subscribe" >:: run test_subscribe;
    "test_unsubscribe" >:: run test_unsubscribe;
    "test_list" >:: run test_list;
    "test_lsub" >:: run test_lsub;
    "test_status" >:: run test_status;
    "test_append" >:: run test_append;
    "test_check" >:: run test_check;
    "test_close" >:: run test_close;
    "test_expunge" >:: run test_expunge;
    "test_uid_expunge" >:: run test_uid_expunge
  ]

let () =
  run_test_tt_main suite
