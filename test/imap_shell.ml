(* this code is in the public domain *)

let io_buffer_size = 65 * 1024

let (>>=) = Lwt.(>>=)

let () = Ssl.init ()

let () =
  if (try Sys.getenv "IMAP_DEBUG" <> "0" with Not_found -> false) then
    Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Debug

type connection =
  {
    c : Imap.connection;
    sock : Lwt_ssl.socket;
  }

let rec process c : Imap.result -> _ = function
  | `Read (s, i, l, k) ->
      (* LTerm.eprintl "Read" >>= fun () -> *)
      Lwt_ssl.read c.sock s i l >>= fun n ->
      Lwt_log.debug_f ">>> %d\n%s>>>%!" n (String.sub s i n) >>= fun () ->
      process c (k n)
  | `Write (o, pos, len, k) ->
      (* LTerm.eprintl "Write" >>= fun () -> *)
      Lwt_ssl.write c.sock o pos len >>= fun n ->
      Lwt_log.debug_f "<<< %d\n%s<<<%!" n (String.sub o pos n) >>= fun () ->
      process c (k n)
  | `Untagged _ as r ->
      (* LTerm.eprintl "Untagged" >>= fun () -> *)
      Lwt.return r
  | `Ok _ ->
      Lwt.return `Ok
  (* | `Error (`Decode_error (_, i, n)) as e -> *)
  (*     (\* LTerm.eprintlf "Error: %s\n%s^\n" i (String.make (7 + n) ' ') >>= fun () -> *\) *)
  (*     Lwt.return e *)
  | `Error e ->
      (* LTerm.eprintlf "Error: %s" (Format.flush_str_formatter ()) >>= fun () -> *)
      Imap.pp_error Format.str_formatter e;
      Lwt.fail (Failure (Format.flush_str_formatter ()))

let run c cmd =
  process c (Imap.run c.c cmd)

let rec handle c h = function
  | `Untagged (u, k) ->
      Format.printf "%a@." Imap.pp_response u;
      h u >>= fun () -> process c (k ()) >>= handle c h
  | `Ok ->
      Lwt.return_unit

let handle_unit c r =
  handle c (fun _ -> Lwt.return_unit) r

let do_connect host port =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  Lwt_unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >>= fun () ->
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  Lwt_ssl.ssl_connect fd ctx >>= fun sock ->
  let c, v = Imap.connection () in
  let c =
    {
      c;
      sock;
    }
  in
  process c v >>= handle_unit c >>= fun () -> Lwt.return c

open Cmdliner

let uint64 =
  let f s = try `Ok (Uint64.of_string s) with _ -> `Error "uint64" in
  let g ppf x = Format.fprintf ppf "%s" (Uint64.to_string x) in
  f, g

let uint32 =
  let f s = try `Ok (Uint32.of_string s) with _ -> `Error "uint32" in
  let g ppf x = Format.fprintf ppf "%s" (Uint32.to_string x) in
  f, g

let docs = "COMMON IMAP OPTIONS"

let host =
  let doc = Arg.info ~docs ~docv:"HOST" ~doc:"Server hostname." ["h"; "host"] in
  Arg.(required & opt (some string) None & doc)

let port =
  let doc = Arg.info ~docs ~docv:"PORT" ~doc:"Port number." ["port"] in
  Arg.(value & opt int 993 & doc)

let user =
  let doc = Arg.info ~docs ~docv:"USERNAME" ~doc:"Username." ["u"; "user"] in
  Arg.(required & opt (some string) None & doc)

let password =
  let doc = Arg.info ~docs ~docv:"PASSWORD" ~doc:"Password." ["p"; "password"] in
  Arg.(required & opt (some string) None & doc)

let mailbox =
  let doc = Arg.info ~docs ~docv:"MAILBOX" ~doc:"Mailbox name (UTF-8 encoded)." ["mailbox"] in
  Arg.(value & opt string "INBOX" & doc)

let pos_mailbox i =
  let doc = Arg.info ~docs ~docv:"MAILBOX" ~doc:"Mailbox name (UTF-8 encoded)." [] in
  Arg.(required & pos i (some string) None & doc)

let docs = "IMAP OPTIONS"

let condstore =
  let doc = Arg.info ~docs ~doc:"Use CONDSTORE." ["condstore"] in
  Arg.(value & flag & doc)

let status_att =
  let att =
    [
      "messages", `Messages, "Number of messages in the mailbox";
      "recent", `Recent, "Number of recent messages";
      "uidnext", `Uid_next, "Next UID value";
      "uidvalidity", `Uid_validity, "UID validity value";
      "unseen", `Unseen, "Number of unseen messages";
      "highestmodseq", `Highest_modseq, "Highest modification sequence number";
    ]
  in
  let rec loop = function
    | (n, a, d) :: rem ->
        let t = Arg.(value & flag & info ~docs ~docv:(String.uppercase n) ~doc:d [n]) in
        Term.(pure (fun x rest -> if x then a :: rest else rest) $ t $ loop rem)
    | [] ->
        Term.(pure [])
  in
  loop att

let range =
  let f s =
    try Scanf.sscanf s "%s@:%s" (fun a b -> `Ok (Uint32.of_string a, Some (Uint32.of_string b))) with
    | _ -> try let a = Uint32.of_string s in `Ok (a, Some a) with _ -> `Error "range"
  in
  let g ppf = function
    | (lo, Some hi) ->
        if lo = hi then Format.pp_print_string ppf (Uint32.to_string lo) else
        Format.fprintf ppf "%s:%s" (Uint32.to_string lo) (Uint32.to_string hi)
    | (lo, None) ->
        Format.fprintf ppf "%s:*" (Uint32.to_string lo)
  in
  f, g

let set =
  let doc = Arg.info ~docv:"SET" ~doc:"Set of message numbers." [] in
  Arg.(required & pos 0 (some (list range)) None & doc)

let uid =
  let doc = Arg.info ~docs ~docv:"UID" ~doc:"Use UIDs instead of sequence numbers." ["uid"] in
  Arg.(value & flag doc)

let changed_since =
  let doc = Arg.info ~docs ~docv:"CHANGEDSINCE" ~doc:"Modification sequence of ..." ["changed-since"] in
  Arg.(value & opt (some uint64) None & doc)

let vanished =
  let doc = Arg.info ~docs ~doc:"Report VANISHED messages." ["vanished"] in
  Arg.(value & flag doc)

let fetch_att =
  let att =
    [ "envelope", `Envelope, "Envelope information";
      "internaldate", `Internal_date, "Internal date";
      "rfc822", `Rfc822, "Full text (header & body)";
      "rfc822-header", `Rfc822_header, "Header text";
      "rfc822-text", `Rfc822_text, "Body text";
      "rfc822-size", `Rfc822_size, "Size";
      "body", `Body, "? ? ?";
      "bodystructure", `Body_structure, "Body structure";
      "uid", `Uid, "UID";
      "flags", `Flags, "Flags" ]
  in
  let rec loop = function
    | (n, a, d) :: rem ->
        let t = Arg.(value & flag & info ~docs ~docv:(String.uppercase n) ~doc:d [n]) in
        Term.(pure (fun x rest -> if x then a :: rest else rest) $ t $ loop rem)
    | [] ->
        Term.(pure [])
  in
  loop att

let store_mode =
  let doc = Arg.info ~doc:"Add, Set or Remove flags" [] in
  Arg.(required & pos 0 (some (enum ["add", `Add; "set", `Set; "remove", `Remove])) None & doc)

let silent =
  let doc = Arg.info ~docs ~doc:"Whether to be silent after flag changes" ["silent"] in
  Arg.(value & flag doc)

let unchanged_since =
  let doc = Arg.info ~docs ~docv:"UNCHANGEDSINCE" ~doc:"Unchanged since TODO" ["unchanged-since"] in
  Arg.(value & opt (some uint64) None & doc)

let flags =
  let flags =
    [ "answered", `Answered;
      "flagged", `Flagged;
      "deleted", `Deleted;
      "seen", `Seen;
      "draft", `Draft ]
  in
  let rec loop = function
    | (n, a) :: rem ->
        let arg = Arg.(value & flag & info ~docs ~doc:n [n]) in
        Term.(pure (fun x rem -> if x then a :: rem else rem) $ arg $ loop rem)
    | [] ->
        let doc = Arg.info ~docs ~docv:"FLAG" ~doc:"Flag to store" ["flag"] in
        Term.(pure (List.map (fun x -> `Keyword x)) $ Arg.(value & opt_all string [] & doc))
  in
  loop flags

let labels =
  let doc = Arg.info ~docs ~docv:"LABEL" ~doc:"Label to store" ["label"] in
  Arg.(value & opt_all string [] & doc)

let list_reference =
  let doc = Arg.info ~docs ~docv:"REFERENCE" ~doc:"List reference" ["reference"] in
  Arg.(value & opt string "" & doc)

let list_wildcard =
  let doc = Arg.info ~docs ~docv:"WILDCARD" ~doc:"Mailbox wildcard" [] in
  Arg.(value & pos 0 string "*" & doc)

let search_key =
  let keys =
    [
      "all", `All;
      "answered", `Answered;
      "deleted", `Deleted;
      "draft", `Draft;
      "flagged", `Flagged;
      "new", `New;
      "old", `Old;
      "recent", `Recent;
      "seen", `Seen;
      "unanswered", `Unanswered;
      "undeleted", `Undeleted;
      "undraft", `Undraft;
      "unflagged", `Unflagged;
      "unseen", `Unseen;
    ]
  in
  let bcc = Arg.(value & opt (some string) None & info ["bcc"]) in
  let rec loop = function
    | (n, a) :: rem ->
        let arg = Arg.(value & flag & info ~docs ~doc:n [n]) in
        Term.(pure (fun x rem -> if x then `And (a, rem) else rem) $ arg $ loop rem)
    | [] ->
        Term.pure `All
  in
  loop keys

let capabilities =
  let caps =
    [
      "acl", `Acl;
      "binary", `Binary;
      "catenate", `Catenate;
      "children", `Children;
      "compress-deflate", `Compress_deflate;
      "condstore", `Condstore;
      "enable", `Enable;
      "idle", `Idle;
      "id", `Id;
      "literal-plus", `Literal_plus;
      "multi-append", `Multi_append;
      "namespace", `Namespace;
      "qresync", `Qresync;
      "quote", `Quote;
      "sort", `Sort;
      "start-tls", `Start_tls;
      "uid-plus", `Uid_plus;
      "unselect", `Unselect;
      "xlist", `Xlist;
      "auth-anonymous", `Auth `Anonymous;
      "auth-login", `Auth `Login;
      "auth-plain", `Auth `Plain;
      "xoauth2", `Xoauth2;
      "gmail", `Gmail;
    ]
  in
  let rec loop = function
    | (n, a) :: rem ->
        let arg = Arg.(value & flag & info ~docs ~doc:n [n]) in
        Term.(pure (fun x rem -> if x then a :: rem else rem) $ arg $ loop rem)
    | [] ->
        let doc = Arg.info ~docs ~docv:"CAPABILITY" ~doc:"Capability to enable" ["cap"] in
        Term.(pure (List.map (fun x -> `Other x)) $ Arg.(value & opt_all string [] & doc))
  in
  loop caps

(* CONNECT *)
let connect_doc = "Connecto to an IMAPS server."
let connect =
  let doc = connect_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,connect) command tries to establish a connction to an IMAP server over TLS."
    ]
  in
  let connect host port =
    do_connect host port >>= fun _ -> Lwt.return_unit
  in
  Term.(pure connect $ host $ port), Term.info "connect" ~doc ~man

(* CAPABILITY *)
let capability_doc = "Query the capabilities of an IMAP server."
let capability =
  let doc = capability_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,capability) command queries the capabilities of the IMAP \
          server by sending a $(b,CAPABILITY) command."
    ]
  in
  let capability host port user password =
    do_connect host port >>= fun c ->
    run c Imap.capability >>=
    handle_unit c
  in
  Term.(pure capability $ host $ port $ user $ password), Term.info "capability" ~doc ~man

(* LOGIN *)
let login_doc = "Login to an IMAP server."
let login =
  let doc = login_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,login) command sends login credentials to the IMAP server."
    ]
  in
  let login host port user pass =
    do_connect host port >>= fun c ->
    run c (Imap.login user pass) >>=
    handle_unit c
  in
  Term.(pure login $ host $ port $ user $ password), Term.info "login" ~doc ~man

(* LOGOUT *)
let logout_doc = "Logout from an IMAP server."
let logout =
  let doc = logout_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,logout) command logs out from an IMAP server."
    ]
  in
  let logout host port =
    do_connect host port >>= fun c ->
    run c Imap.logout >>=
    handle_unit c
  in
  Term.(pure logout $ host $ port), Term.info "logout" ~doc ~man

(* NOOP *)
let noop_doc = "Sends a NOOP command to the IMAP server."
let noop =
  let doc = noop_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,noop) command pings the IMAP server.  \
          It is useful to keep a connection from closing due to inactivity."
    ]
  in
  let noop host port =
    do_connect host port >>= fun c ->
    run c Imap.noop >>=
    handle_unit c
  in
  Term.(pure noop $ host $ port), Term.info "noop" ~doc ~man

(* AUTHENTICATE *)
let authenticate_doc = "Authenticate with an IMAP server."
let authenticate =
  let doc = authenticate_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,authenticate) command authenticates the user with the IMAP server."
    ]
  in
  let authenticate host port user pass =
    do_connect host port >>= fun c ->
    run c (Imap.authenticate (Imap.plain user pass)) >>=
    handle_unit c
  in
  Term.(pure authenticate $ host $ port $ user $ password), Term.info "authenticate" ~doc ~man

(* SUBSCRIBE *)
let subscribe_doc = "Subscribe to a mailbox. FIXME"
let subscribe =
  let doc = subscribe_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,subscribe) command subscribes the client to a mailbox."
    ]
  in
  let subscribe host port user password m =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.subscribe m) >>=
    handle_unit c
  in
  Term.(pure subscribe $ host $ port $ user $ password $ mailbox), Term.info "subscribe" ~doc ~man

(* UNSUBSCRIBE *)
let unsubscribe_doc = "Unsubscribes from a mailbox. FIXME"
let unsubscribe =
  let doc = unsubscribe_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,unsubscribe) command unsubscribes the client to a mailbox."
    ]
  in
  let unsubscribe host port user password m =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.unsubscribe m) >>=
    handle_unit c
  in
  Term.(pure unsubscribe $ host $ port $ user $ password $ mailbox),
  Term.info "unsubscribe" ~doc ~man

(* LIST *)
let list_doc = "List mailboxes."
let list =
  let doc = list_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,list) command shows a list of available mailboxes matching a certain patter."
    ]
  in
  let list host port user password m =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.list m) >>=
    handle_unit c
  in
  Term.(pure list $ host $ port $ user $ password $ list_wildcard),
  Term.info "list" ~doc ~man

(* LSUB *)
let lsub_doc = "List subscribed mailboxes."
let lsub =
  let doc = lsub_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,lsub) command shows a list of subscribed mailboxes matching a certain patter."
    ]
  in
  let lsub host port user password m =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.list m) >>=
    handle_unit c
  in
  Term.(pure lsub $ host $ port $ user $ password $ list_wildcard),
  Term.info "lsub" ~doc ~man

(* SELECT *)
let select_doc = "Select a mailbox for further manipulation."
let select =
  let doc = select_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,select) command opens a mailbox in read-write mode in order \
          to inspect and change its contents."
    ]
  in
  let select host port user password condstore m =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.select ~condstore m) >>=
    handle_unit c
  in
  Term.(pure select $ host $ port $ user $ password $ condstore $ mailbox), Term.info "select" ~doc ~man

(* EXAMINE *)
let examine_doc = "Open a mailbox (READ-ONLY)."
let examine =
  let doc = examine_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,examine) command opens a mailbox in read-only mode in order \
          to inspect its contents."
    ]
  in
  let examine host port user password condstore m =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.examine ~condstore m) >>=
    handle_unit c
  in
  Term.(pure examine $ host $ port $ user $ password $ condstore $ mailbox),
  Term.info "examine" ~doc ~man

(* CREATE *)
let create_doc = "Create a new mailbox."
let create =
  let doc = create_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,create) command creates a new mailbox."
  ] in
  let create host port user password m =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.create m) >>=
    handle_unit c
  in
  Term.(pure create $ host $ port $ user $ password $ mailbox),
  Term.info "create" ~doc ~man

(* RENAME *)
let rename_doc = "Rename an existing mailbox."
let rename =
  let doc = rename_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,rename) command renames an existing mailbox."
    ]
  in
  let rename host port user password oldm newm =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.rename oldm newm) >>=
    handle_unit c
  in
  Term.(pure rename $ host $ port $ user $ password $ pos_mailbox 0 $ pos_mailbox 1), Term.info "rename" ~doc ~man

(* STATUS *)
let status_doc = "Query mailbox information."
let status =
  let doc = status_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,status) commands queries the IMAP server for mailbox \
          information (number of messages, etc.)."
    ]
  in
  let status host port user password m atts =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.status m atts) >>=
    handle_unit c
  in
  Term.(pure status $ host $ port $ user $ password $ mailbox $ status_att),
  Term.info "status" ~doc ~man

(* CLOSE *)
let close_doc = "Closes the currently selected mailbox."
let close =
  let doc = close_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,close) command closes the currently selected mailbox."
    ]
  in
  let close host port user password m =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.examine m) >>=
    handle_unit c >>= fun () ->
    run c Imap.close >>=
    handle_unit c
  in
  Term.(pure close $ host $ port $ user $ password $ mailbox),
  Term.info "close" ~doc ~man

(* FETCH *)
let fetch_doc = "Fetch message attributes."
let fetch =
  let doc = fetch_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The $(b,fetch) command retrieves message properties."
    ]
  in
  let fetch host port user password mailbox set uid att changed vanished =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.examine mailbox) >>=
    handle_unit c >>= fun () ->
    run c (Imap.fetch ~uid ?changed ~vanished set att) >>=
    handle_unit c
  in
  Term.(pure fetch $ host $ port $ user $ password $ mailbox $ set $ uid $ fetch_att $ changed_since $ vanished), Term.info "fetch" ~doc ~man

(* STORE *)
let store_doc = "Modify message flags & labels."
let store =
  let doc = store_doc in
  let man =
    [
      `S "DESCRIPTION";
      `P "The command $(b,store) modifies the flags and labels associated \
          with a given set of messages."
    ]
  in
  let store host port user password mailbox mode set uid silent unchanged flags labels =
    let f_fl, f_lb = match mode with
      | `Add -> Imap.store_add_flags, Imap.store_add_labels
      | `Set -> Imap.store_set_flags, Imap.store_set_labels
      | `Remove -> Imap.store_remove_flags, Imap.store_remove_labels
    in
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.select mailbox) >>=
    handle_unit c >>= fun () ->
    begin
      if List.length flags > 0 then
        run c (f_fl ~uid ~silent ?unchanged set flags) >>= handle_unit c
      else
        Lwt.return_unit
    end >>= fun () ->
    begin
      if List.length labels > 0 then
        run c (f_lb ~uid ~silent ?unchanged set labels) >>= handle_unit c
      else
        Lwt.return_unit
    end
  in
  Term.(pure store $ host $ port $ user $ password $ mailbox $ store_mode $ set $ uid $ silent $ unchanged_since $ flags $ labels),
  Term.info "store" ~doc ~man

(* SEARCH *)
let search_doc = "Search for message numbers of messages satisfying some criteria."
let search =
  let doc = search_doc in
  let search host port user password mailbox uid key =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.examine mailbox) >>=
    handle_unit c >>= fun () ->
    run c (Imap.search ~uid key) >>=
    handle_unit c
  in
  Term.(pure search $ host $ port $ user $ password $ mailbox $ uid $ search_key),
  Term.info "search" ~doc

(* ENABLE *)
let enable_doc = "Enable server capabilities."
let enable =
  let doc = enable_doc in
  let enable host port user password caps =
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.enable caps) >>=
    handle_unit c
  in
  Term.(pure enable $ host $ port $ user $ password $ capabilities),
  Term.info "enable" ~doc

let forever =
  let doc = Arg.info ["forver"] in
  Arg.(value & flag doc)

(* IDLE *)
let idle_doc = "IDLE"
let idle =
  let doc = idle_doc in
  let idle host port user password mailbox forever =
    let h stop _ = if not forever then Lwt.wrap1 stop () else Lwt.return_unit in
    do_connect host port >>= fun c ->
    run c (Imap.login user password) >>=
    handle_unit c >>= fun () ->
    run c (Imap.examine mailbox) >>=
    handle_unit c >>= fun () ->
    let cmd, stop = Imap.idle () in
    run c cmd >>=
    handle c (h stop)
  in
  Term.(pure idle $ host $ port $ user $ password $ mailbox $ forever), Term.info "idle" ~doc

let commands =
  [
    connect;
    capability;
    login;
    logout;
    noop;
    authenticate;
    subscribe;
    unsubscribe;
    list;
    lsub;
    select;
    examine;
    create;
    rename;
    status;
    close;
    fetch;
    store;
    search;
    enable;
    idle;
  ]

let main_command =
  Term.(ret (const (`Help (`Pager, None)))), Term.info "imap_shell"

let () =
  match Term.eval_choice main_command commands with
  | `Ok t ->
      Lwt_main.run t
  | _ ->
      ()
