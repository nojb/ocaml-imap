open OUnit2
open Sexplib.Std
open Imap
open Imap_types
open Imap_uint

let example_responses =
  [ ("* CAPABILITY IMAP4rev1 STARTTLS AUTH=GSSAPI",
     `Capability ["IMAP4rev1"; "STARTTLS"; "AUTH=GSSAPI"]);
    ("abcd OK CAPABILITY completed",
     `Tagged ("abcd", `Ok (`None, "CAPABILITY completed")));
    ("* CAPABILITY IMAP4rev1 AUTH=GSSAPI AUTH=PLAIN",
     `Capability ["IMAP4rev1"; "AUTH=GSSAPI"; "AUTH=PLAIN"]);
    ("* 22 EXPUNGE", `Expunge 22);
    ("* 23 EXISTS", `Exists 23);
    ("* 3 RECENT", `Recent 3);
    ("* 14 FETCH (FLAGS (\\Seen \\Deleted))",
     `Fetch (14, [`Flags [`Seen; `Deleted]]));
    ("* BYE IMAP4rev1 Server logging out",
     `Bye (`None, "IMAP4rev1 Server logging out"));
    ("* CAPABILITY IMAP4rev1 STARTTLS LOGINDISABLED",
     `Capability ["IMAP4rev1"; "STARTTLS"; "LOGINDISABLED"]);
    ("+ ", `Cont "");
    ("* OK [UNSEEN 12] Message 12 is first unseen",
     `Ok (`Unseen 12, "Message 12 is first unseen"));
    ("* OK [UIDVALIDITY 3857529045] UIDs valid",
     `Ok (`Uid_validity 3857529045, "UIDs valid"));
    ("* OK [UIDNEXT 4392] Predicted next UID",
     `Ok (`Uid_next 4392, "Predicted next UID"));
    ("* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)",
     `Flags [`Answered; `Flagged; `Deleted; `Seen; `Draft]);
    ("* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited",
     `Ok (`Permanent_flags [`Deleted; `Seen; `All], "Limited"));
    ("A142 OK [READ-WRITE] SELECT completed",
     `Tagged ("A142", `Ok (`Read_write, "SELECT completed")));
    ("* OK [PERMANENTFLAGS ()] No permanent flags permitted",
     `Ok (`Permanent_flags [], "No permanent flags permitted"));
    ("A932 OK [READ-ONLY] EXAMINE completed",
     `Tagged ("A932", `Ok (`Read_only, "EXAMINE completed")));
    ("* LIST (\\Noselect) \"/\" foo",
     `List ([`Noselect], Some '/', "foo"));
    ("* LIST () \"/\" foo/bar", `List ([], Some '/', "foo/bar"));
    ("A684 NO Name \"foo\" has inferior hierarchical names",
     `Tagged ("A684", `No (`None, "Name \"foo\" has inferior hierarchical names")));
    ("* LIST () \".\" foo", `List ([], Some '.', "foo"));
    ("* LIST () \".\" foo.bar", `List ([], Some '.', "foo.bar"));
    ("* LIST (\\Noselect) \".\" foo", `List ([`Noselect], Some '.', "foo"));
    ("* LIST () \".\" INBOX", `List ([], Some '.', "INBOX"));
    ("* LIST () \".\" INBOX.bar", `List ([], Some '.', "INBOX.bar"));
    ("* LIST (\\Noselect) \"/\" \"\"", `List ([`Noselect], Some '/', ""));
    ("* LIST (\\Noselect) \".\" #news.",
     `List ([`Noselect], Some '.', "#news."));
    ("* LIST (\\Noselect) \"/\" ~/Mail/foo",
     `List ([`Noselect], Some '/', "~/Mail/foo"));
    ("* LSUB () \".\" #news.comp.mail.mime",
     `Lsub ([], Some '.', "#news.comp.mail.mime"));
    ("* LSUB (\\NoSelect) \".\" #news.comp.mail",
     `Lsub ([`Noselect], Some '.', "#news.comp.mail"));
    ("* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)",
     `Status ("blurdybloop", [`Messages 231; `Uid_next 44292]));
    ("+ Ready for literal data", `Cont "Ready for literal data");
    ("* SEARCH 2 84 882", `Search [2; 84; 882]);
    ("* SEARCH", `Search []);
    ("* 2 FETCH (FLAGS (\\Deleted \\Seen))",
     `Fetch (2, [`Flags [`Deleted; `Seen]]));
    ("* 23 FETCH (FLAGS (\\Seen) UID 4827313)",
     `Fetch (23, [`Flags [`Seen]; `Uid 4827313]));
    ("* 23 FETCH (FLAGS (\\Seen) RFC822.SIZE 44827)",
     `Fetch (23, [`Flags [`Seen]; `Rrfc822_size 44827]))
  ]

let test_parser ~ctxt p sexp_of_t s check =
  match Parser.parse p s with
  | `Ok x ->
    let pp fmt x =
      Sexplib.Sexp.pp_hum fmt (sexp_of_t x)
    in
    assert_equal ~ctxt ~msg:s ~pp_diff:(fun fmt (x, check) ->
        Format.fprintf fmt "@[EXPECTED: @[%a@]@\nGOT: @[%a@]@]@."
          pp x pp check) x check
  | `Fail i -> failwith (Printf.sprintf "test_parser: %S near %d" s i)
  | `Exn exn -> raise exn

let test_responses =
  List.map (fun (ex, check) ->
      test_case (fun ctxt ->
          test_parser ~ctxt Response.cont_req_or_resp_data_or_resp_done
            Response.sexp_of_cont_req_or_resp_data_or_resp_done (ex ^ "\r\n") check))
    example_responses

let test_envelope ctxt =
  let open Mime in
  let ex =
    "(\"Wed, 17 Jul 1996 02:23:25 -0700 (PDT)\" \
     \"IMAP4rev1 WG mtg summary and minutes\" \
     ((\"Terry Gray\" NIL \"gray\" \"cac.washington.edu\")) \
     ((\"Terry Gray\" NIL \"gray\" \"cac.washington.edu\")) \
     ((\"Terry Gray\" NIL \"gray\" \"cac.washington.edu\")) \
     ((NIL NIL \"imap\" \"cac.washington.edu\")) \
     ((NIL NIL \"minutes\" \"CNRI.Reston.VA.US\") \
     (\"John Klensin\" NIL \"KLENSIN\" \"MIT.EDU\")) NIL NIL \
     \"<B27397-0100000@cac.washington.edu>\")"
  in
  let tgray =
    { ad_name = "Terry Gray";
      ad_adl = "";
      ad_mailbox = "gray";
      ad_host = "cac.washington.edu" }
  in
  let imap =
    { ad_name = "";
      ad_adl = "";
      ad_mailbox = "imap";
      ad_host = "cac.washington.edu" }
  in
  let minutes =
    { ad_name = "";
      ad_adl = "";
      ad_mailbox = "minutes";
      ad_host = "CNRI.Reston.VA.US" }
  in
  let jklensin =
    { ad_name = "John Klensin";
      ad_adl = "";
      ad_mailbox = "KLENSIN";
      ad_host = "MIT.EDU" }
  in
  let check =
    { env_date = "Wed, 17 Jul 1996 02:23:25 -0700 (PDT)";
      env_subject = "IMAP4rev1 WG mtg summary and minutes";
      env_from = [tgray]; env_sender = [tgray]; env_reply_to = [tgray];
      env_to = [imap]; env_cc = [minutes; jklensin]; env_bcc = [];
      env_in_reply_to = ""; env_message_id = "<B27397-0100000@cac.washington.edu>" }
  in
  test_parser ~ctxt envelope sexp_of_envelope ex check

let test_body ctxt =
  let open Mime in
  let ex =
    "(\"TEXT\" \"PLAIN\" (\"CHARSET\" \"US-ASCII\") NIL NIL \"7BIT\" 3028 92)"
  in
  let check =
    Text
      { bd_param = ["CHARSET", "US-ASCII"];
        bd_id = None;
        bd_desc = None;
        bd_enc = `BIT7;
        bd_octets = 3028;
        bd_other = { text_subtype = "PLAIN"; text_lines = 92 };
        bd_ext = { ext_md5 = None; ext_dsp = None; ext_lang = []; ext_exts = [] } }
  in
  test_parser ~ctxt body sexp_of_body ex check

let suite =
  "test_parser" >:::
  [
    "test_responses" >::: test_responses;
    "test_envelope" >:: test_envelope;
    "test_body" >:: test_body
  ]

let () =
  run_test_tt_main suite
