open OUnit2
  
(* open Test_live_util *)
open Imap
open Imap_types

let example_responses = [
  "* NAMESPACE NIL NIL NIL",
  `NAMESPACE ([], [], []);
  "* NAMESPACE ((\"\" \"/\")) NIL NIL",
  `NAMESPACE ([{ns_prefix = ""; ns_delimiter = '/'; ns_extensions = []}], [], []);
  "* NAMESPACE NIL NIL ((\"\" \".\"))",
  `NAMESPACE ([], [], [{ns_prefix = ""; ns_delimiter = '.'; ns_extensions = []}]);
  "* NAMESPACE ((\"\" \"/\")) NIL ((\"Public Folders/\" \"/\"))",
  `NAMESPACE ([{ns_prefix = ""; ns_delimiter = '/'; ns_extensions = []}],
              [], [{ns_prefix = "Public Folders/"; ns_delimiter = '/'; ns_extensions = []}]);
  "* NAMESPACE ((\"\" \"/\")) ((\"~\" \"/\")) ((\"#shared/\" \"/\")(\"#public/\" \"/\")(\"#ftp/\" \"/\")(\"#news.\" \".\"))",
  `NAMESPACE ([{ns_prefix = ""; ns_delimiter = '/'; ns_extensions = []}],
              [{ns_prefix = "~"; ns_delimiter = '/'; ns_extensions = []}],
              [{ns_prefix = "#shared/"; ns_delimiter = '/'; ns_extensions = []};
               {ns_prefix = "#public/"; ns_delimiter = '/'; ns_extensions = []};
               {ns_prefix = "#ftp/"; ns_delimiter = '/'; ns_extensions = []};
               {ns_prefix = "#news."; ns_delimiter = '.'; ns_extensions = []}]);
  "* NAMESPACE ((\"INBOX.\" \".\")) NIL NIL",
  `NAMESPACE ([{ns_prefix = "INBOX."; ns_delimiter = '.'; ns_extensions = []}], [], [])
]

let test_parser ~ctxt s check =
  match Parser.(parse Response.(continue_req <|> response_data <|> response_done)) s with
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
    example_responses

let suite =
  "test_condstore" >:::
  [
    "test_parser" >::: test_responses
  ]

let () =
  run_test_tt_main suite
  
