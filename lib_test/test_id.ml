open OUnit2
open Test_live_utils

let test_id ctxt =
  set_up_test (fun imap ->
      if not (Imap.has_id imap) then Lwt.fail (Skip "server does not support ID")
      else
        Imap.id imap [] >|= fun _ -> ())

let suite =
  "test_id" >:: run test_id

let _ =
  run_test_tt_main suite
