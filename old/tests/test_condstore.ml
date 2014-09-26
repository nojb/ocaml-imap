open OUnit2
open Imap
open Imap_types
open Imap_uint

let examples = [
  "* OK [HIGHESTMODSEQ 715194045007]", `OK (`HIGHESTMODSEQ (Modseq.of_string "715194045007"), "");
  "* OK [NOMODSEQ] Sorry, this mailbox format doesn't support modsequences",
  `OK (`NOMODSEQ, "Sorry, this mailbox format doesn't support modsequences");
  "* 4 FETCH (UID 8 MODSEQ (12121130956))",
  `FETCH (Seq.of_int 4, [`UID (Uid.of_int 8); `MODSEQ (Modseq.of_string "12121130956")]);
  "d105 OK [MODIFIED 7,9] Conditional STORE failed",
  `TAGGED ("d105", `OK (`MODIFIED (Uint32_set.(union (single' 7) (single' 9))),
                        "Conditional STORE failed"));
  "* SEARCH 2 5 6 7 11 12 18 19 20 23 (MODSEQ 917162500)",
  `SEARCH (List.map Uint32.of_int [2; 5; 6; 7; 11; 12; 18; 19; 20; 23],
           (Modseq.of_string "917162500"));
  "* STATUS blurdybloop (MESSAGES 231 HIGHESTMODSEQ 7011231777)",
  `STATUS {st_mailbox = "blurdybloop";
           st_info_list = [`MESSAGES 231; `HIGHESTMODSEQ (Modseq.of_string "7011231777")]}
]

let test_parser ~ctxt s check =
  match Parser.parse Response.cont_req_or_resp_data_or_resp_done s with
  | `Ok x ->
    let pp fmt x =
      Sexplib.Sexp.pp_hum fmt (Response.sexp_of_cont_req_or_resp_data_or_resp_done x)
    in
    assert_equal ~ctxt ~msg:s ~pp_diff:(fun fmt (x, check) ->
        Format.fprintf fmt "@[EXPECTED: @[%a@]@\nGOT: @[%a@]@]@."
          pp x pp check) x check
  | `Fail i -> failwith (Printf.sprintf "test_parser: %S near %d" s i)
  | `Exn exn -> raise exn

let test_responses =
  List.map (fun (ex, check) ->
      test_case (fun ctxt -> test_parser ~ctxt (ex ^ "\r\n") check))
    examples

open Test_live_utils

let test_select_condstore ctxt =
  set_up_test begin fun imap ->
    Imap.select_condstore imap "inbox" >>= fun modseq ->
    assert_bool "modseq is zero" (Modseq.compare modseq Modseq.zero <> 0);
    Lwt.return ()
  end

let test_examine_condstore ctxt =
  set_up_test begin fun imap ->
    Imap.examine_condstore imap "inbox" >>= fun modseq ->
    assert_bool "modseq is zero" (Modseq.compare modseq Modseq.zero <> 0);
    Lwt.return ()
  end

let test_fetch_changedsince_aux cmd set ctxt =
  set_up_test begin fun imap ->
    Imap.examine_condstore imap "inbox" >>= fun modseq ->
    let s = cmd imap set modseq [`UID; `FLAGS] in
    Lwt_stream.is_empty s >|= fun b ->
    assert_bool "fetch_changedsince should be empty" b
  end

let test_fetch_changedsince ctxt =
  test_fetch_changedsince_aux Imap.fetch_changedsince Seq_set.all ctxt

let test_uid_fetch_changedsince ctxt =
  test_fetch_changedsince_aux Imap.uid_fetch_changedsince Uid_set.all ctxt

let suite =
  "test_condstore" >:::
  [
    "test_parser" >::: test_responses;
    "test_select_condstore" >:: run test_select_condstore;
    "test_fetch_changesince" >:: run test_fetch_changedsince;
    "test_uid_fetch_changesince" >:: run test_uid_fetch_changedsince
  ]

let () =
  run_test_tt_main suite
