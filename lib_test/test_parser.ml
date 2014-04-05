open OUnit2
open Sexplib.Std
open Imap
open Imap_types
open Imap_uint
  
let example_responses =
  [ ("* CAPABILITY IMAP4rev1 STARTTLS AUTH=GSSAPI",
     `CAPABILITY [`NAME "IMAP4rev1"; `NAME "STARTTLS"; `AUTH_TYPE "GSSAPI"]);
    ("abcd OK CAPABILITY completed",
     `TAGGED ("abcd", `OK (`NONE, "CAPABILITY completed")));
    ("* CAPABILITY IMAP4rev1 AUTH=GSSAPI AUTH=PLAIN",
     `CAPABILITY [`NAME "IMAP4rev1"; `AUTH_TYPE "GSSAPI"; `AUTH_TYPE "PLAIN"]);
    ("* 22 EXPUNGE", `EXPUNGE (Seq.of_int 22));
    ("* 23 EXISTS", `EXISTS 23);
    ("* 3 RECENT", `RECENT 3);
    ("* 14 FETCH (FLAGS (\\Seen \\Deleted))",
     `FETCH (Seq.of_int 14, [`FLAGS [`Seen; `Deleted]]));
    ("* BYE IMAP4rev1 Server logging out",
     `BYE (`NONE, "IMAP4rev1 Server logging out"));
    ("* CAPABILITY IMAP4rev1 STARTTLS LOGINDISABLED",
     `CAPABILITY [`NAME "IMAP4rev1"; `NAME "STARTTLS"; `NAME "LOGINDISABLED"]);
    ("+ ", `CONT_REQ (`BASE64 ""));
    ("* OK [UNSEEN 12] Message 12 is first unseen",
     `OK (`UNSEEN (Seq.of_int 12), "Message 12 is first unseen"));
    ("* OK [UIDVALIDITY 3857529045] UIDs valid",
     `OK (`UIDVALIDITY (Uid.of_int 3857529045), "UIDs valid"));
    ("* OK [UIDNEXT 4392] Predicted next UID",
     `OK (`UIDNEXT (Uid.of_int 4392), "Predicted next UID"));
    ("* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)",
     `FLAGS [`Answered; `Flagged; `Deleted; `Seen; `Draft]);
    ("* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited",
     `OK (`PERMANENTFLAGS [`Deleted; `Seen; `All], "Limited"));
    ("A142 OK [READ-WRITE] SELECT completed",
     `TAGGED ("A142", `OK (`READ_WRITE, "SELECT completed")));
    ("* OK [PERMANENTFLAGS ()] No permanent flags permitted",
     `OK (`PERMANENTFLAGS [], "No permanent flags permitted"));
    ("A932 OK [READ-ONLY] EXAMINE completed",
     `TAGGED ("A932", `OK (`READ_ONLY, "EXAMINE completed")));
    ("* LIST (\\Noselect) \"/\" foo",
     `LIST {mb_flag = {mbf_sflag = Some `Noselect; mbf_oflags=[]};
            mb_delimiter = '/';
            mb_name = "foo"});
    ("* LIST () \"/\" foo/bar",
     `LIST {mb_flag = {mbf_sflag = None; mbf_oflags = []};
            mb_delimiter = '/';
            mb_name = "foo/bar"});
    ("A684 NO Name \"foo\" has inferior hierarchical names",
     `TAGGED ("A684", `NO (`NONE, "Name \"foo\" has inferior hierarchical names")));
    ("* LIST () \".\" foo",
     `LIST {mb_flag = {mbf_sflag = None; mbf_oflags = []};
            mb_delimiter = '.';
            mb_name = "foo"});
    ("* LIST () \".\" foo.bar",
     `LIST {mb_flag = {mbf_sflag = None; mbf_oflags = []};
            mb_delimiter = '.';
            mb_name = "foo.bar"});
    ("* LIST (\\Noselect) \".\" foo",
     `LIST {mb_flag = {mbf_sflag = Some `Noselect; mbf_oflags = []};
            mb_delimiter = '.';
            mb_name = "foo"});
    ("* LIST () \".\" INBOX",
     `LIST {mb_flag = {mbf_sflag = None; mbf_oflags = []};
            mb_delimiter = '.';
            mb_name = "INBOX"});
    ("* LIST () \".\" INBOX.bar",
     `LIST {mb_flag = {mbf_sflag = None; mbf_oflags = []};
            mb_delimiter = '.';
            mb_name = "INBOX.bar"});
    ("* LIST (\\Noselect) \"/\" \"\"",
     `LIST {mb_flag = {mbf_sflag = Some `Noselect; mbf_oflags = []};
            mb_delimiter = '/';
            mb_name = ""});
    ("* LIST (\\Noselect) \".\" #news.",
     `LIST {mb_flag = {mbf_sflag = Some `Noselect; mbf_oflags = []};
            mb_delimiter = '.';
            mb_name = "#news."});
    ("* LIST (\\Noselect) \"/\" ~/Mail/foo",
     `LIST {mb_flag = {mbf_sflag = Some `Noselect; mbf_oflags = []};
            mb_delimiter = '/';
            mb_name = "~/Mail/foo"});
    ("* LSUB () \".\" #news.comp.mail.mime",
     `LSUB {mb_flag = {mbf_sflag = None; mbf_oflags = []};
            mb_delimiter = '.';
            mb_name = "#news.comp.mail.mime"});
    ("* LSUB (\\NoSelect) \".\" #news.comp.mail",
     `LSUB {mb_flag = {mbf_sflag = Some `Noselect; mbf_oflags = []};
            mb_delimiter = '.';
            mb_name = "#news.comp.mail"});
    ("* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)",
     `STATUS {st_mailbox = "blurdybloop";
              st_info_list = [`MESSAGES 231; `UIDNEXT (Uid.of_int 44292)]});
    ("+ Ready for literal data",
     `CONT_REQ (`TEXT (`NONE, "Ready for literal data")));
    ("* SEARCH 2 84 882",
     `SEARCH (List.map Uint32.of_int [2; 84; 882], Modseq.zero));
    ("* SEARCH", `SEARCH ([], Modseq.zero));
    ("* 2 FETCH (FLAGS (\\Deleted \\Seen))",
     `FETCH (Seq.of_int 2, [`FLAGS [`Deleted; `Seen]]));
    ("* 23 FETCH (FLAGS (\\Seen) UID 4827313)",
     `FETCH (Seq.of_int 23, [`FLAGS [`Seen]; `UID (Uid.of_int 4827313)]));
    ("* 23 FETCH (FLAGS (\\Seen) RFC822.SIZE 44827)",
     `FETCH (Seq.of_int 23, [`FLAGS [`Seen]; `RFC822_SIZE 44827]))
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
    { addr_name = "Terry Gray";
      addr_adl = "";
      addr_mailbox = "gray";
      addr_host = "cac.washington.edu" }
  in
  let imap =
    { addr_name = "";
      addr_adl = "";
      addr_mailbox = "imap";
      addr_host = "cac.washington.edu" }
  in
  let minutes =
    { addr_name = "";
      addr_adl = "";
      addr_mailbox = "minutes";
      addr_host = "CNRI.Reston.VA.US" }
  in
  let jklensin =
    { addr_name = "John Klensin";
      addr_adl = "";
      addr_mailbox = "KLENSIN";
      addr_host = "MIT.EDU" }
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
