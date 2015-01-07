(*
 * imap_shell.ml
 * --------
 * Copyright : (c) 2015, Nicolas Ojeda Bar <n.oje.bar@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of ocaml-imap
 *)

let io_buffer_size = 65 * 1024

let (>>=) = Lwt.(>>=)

let () = Ssl.init ()

type connection =
  { i : string;
    o : string;
    mutable c : Imap.connection;
    mutable sock : Lwt_ssl.socket option }

let run c v =
  let rec write_fully sock s off len =
    if len > 0 then
      Lwt_ssl.write sock s off len >>= fun rc ->
      write_fully sock s (off + rc) (len - rc)
    else
    Lwt.return_unit
  in
  let sock = match c.sock with None -> invalid_arg "not connected" | Some sock -> sock in
  let rec loop = function
    | `Await_src ->
        (* Printf.eprintf "Await_src\n%!"; *)
        Lwt_ssl.read sock c.i 0 (Bytes.length c.i) >>= fun rc ->
        LTerm.eprintlf ">>> %d\n%s>>>%!" rc (String.sub c.i 0 rc) >>= fun () ->
        Imap.Manual.src c.c c.i 0 rc;
        loop (Imap.run c.c `Await)
    | `Await_dst ->
        (* Printf.eprintf "Await_dst\n%!"; *)
        let rc = Bytes.length c.o - Imap.Manual.dst_rem c.c in
        write_fully sock c.o 0 rc >>= fun () ->
        LTerm.eprintlf "<<< %d\n%s<<<%!" rc (String.sub c.o 0 rc) >>= fun () ->
        Imap.Manual.dst c.c c.o 0 (Bytes.length c.o);
        loop (Imap.run c.c `Await)
    | `Untagged _ as r -> Lwt.return r
    | `Ok -> Lwt.return `Ok
    | `Error _ as e -> Lwt.return e
  in
  loop (Imap.run c.c v)

let connect c host port =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  Lwt_unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >>= fun () ->
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  Lwt_ssl.ssl_connect fd ctx >>= fun sock ->
  c.sock <- Some sock;
  run c `Await

let g =
  let s =
    { i = Bytes.create io_buffer_size;
      o = Bytes.create io_buffer_size;
      c = Imap.connection `Manual `Manual;
      sock = None }
  in
  Imap.Manual.dst s.c s.o 0 (Bytes.length s.o);
  s

open Cmdliner

let uint64 =
  let f s = try `Ok (Uint64.of_string s) with _ -> `Error "uint64" in
  let g ppf x = Format.fprintf ppf "%s" (Uint64.to_string x) in
  f, g

let uint32 =
  let f s = try `Ok (Uint32.of_string s) with _ -> `Error "uint32" in
  let g ppf x = Format.fprintf ppf "%s" (Uint32.to_string x) in
  f, g

let docs = "IMAP OPTIONS"

let host =
  let doc = Arg.info ~docs ~docv:"HOST" ~doc:"Server hostname." [] in
  Arg.(required & pos 0 (some string) None & doc)

let port =
  let doc = Arg.info ~docs ~docv:"PORT" ~doc:"Port number." ["p"; "port"] in
  Arg.(value & opt int 993 & doc)

let user =
  let doc = Arg.info ~docv:"USERNAME" ~doc:"Username." [] in
  Arg.(required & pos 0 (some string) None & doc)

let password =
  let doc = Arg.info ~docv:"PASSWORD" ~doc:"Password." [] in
  Arg.(required & pos 1 (some string) None & doc)

let condstore =
  let doc = Arg.info ~docs ~doc:"Use CONDSTORE." ["condstore"] in
  Arg.(value & flag doc)

let mailbox =
  let doc = Arg.info ~docv:"MAILBOX" ~doc:"Mailbox name (UTF-8 encoded)." [] in
  Arg.(required & pos 0 (some string) None & doc)

let status_att =
  let att =
    [ "messages", `Messages, "Number of messages in the mailbox";
      "recent", `Recent, "Number of recent messages";
      "uidnext", `Uid_next, "Next UID value";
      "uidvalidity", `Uid_validity, "UID validity value";
      "unseen", `Unseen, "Number of unseen messages";
      "highestmodseq", `Highest_modseq, "Highest modification sequence number" ]
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
    try Scanf.sscanf s "%s@:%s" (fun a b -> `Ok (Uint32.of_string a, Uint32.of_string b)) with
    | _ -> try let a = Uint32.of_string s in `Ok (a, a) with _ -> `Error "range"
  in
  let g ppf (lo, hi) =
    if lo = hi then Format.pp_print_string ppf (Uint32.to_string lo) else
    Format.fprintf ppf "%s:%s" (Uint32.to_string lo) (Uint32.to_string hi)
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
    [ "all", `All;
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
      "unseen", `Unseen ]
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
    [ "acl", `Acl;
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
      "gmail", `Gmail ]
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
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,connect) command tries to establish a connction to an IMAP server over TLS."
  ] in
  let connect host port =
    connect g host port >>= function
    | `Ok -> LTerm.printl "OK"
    | _ -> Lwt.return_unit
  in
  Term.(pure connect $ host $ port), Term.info "connect" ~doc ~man

let rec handle h = function
  | `Untagged u ->
      h u >>= fun () ->
      run g `Await >>= handle h
  | `Ok -> LTerm.printl "OK"
  | `Error e ->
      Imap.pp_error Format.str_formatter e;
      LTerm.printlf "ERROR: %s" (Format.flush_str_formatter ())

let handle_unit r = handle (fun _ -> Lwt.return_unit) r

(* CAPABILITY *)
let capability_doc = "Query the capabilities of an IMAP server."
let capability =
  let doc = capability_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,capability) command queries the capabilities of the IMAP \
        server by sending a $(b,CAPABILITY) command."
  ] in
  let capability () =
    let h = function
      | `Capability _ as u ->
          Imap.pp_response Format.str_formatter u;
          LTerm.printl (Format.flush_str_formatter ())
      | _ -> Lwt.return_unit
    in
    run g (`Cmd `Capability) >>= handle h
  in
  Term.(pure capability $ pure ()), Term.info "capability" ~doc ~man

(* LOGIN *)
let login_doc = "Login to an IMAP server."
let login =
  let doc = login_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,login) command sends login credentials to the IMAP server."
  ] in
  let login user pass = run g (`Cmd (`Login (user, pass))) >>= handle_unit in
  Term.(pure login $ user $ password), Term.info "login" ~doc

(* LOGOUT *)
let logout_doc = "Logout from an IMAP server."
let logout =
  let doc = logout_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,logout) command logs out from an IMAP server."
  ] in
  let logout () = run g (`Cmd `Logout) >>= handle_unit in
  Term.(pure logout $ pure ()), Term.info "logout" ~doc

(* NOOP *)
let noop_doc = "Sends a NOOP command to the IMAP server."
let noop =
  let doc = noop_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,noop) command pings the IMAP server.  \
        It is useful to keep a connection from closing due to inactivity."
  ] in
  let noop () = run g (`Cmd `Noop) >>= handle_unit in
  Term.(pure noop $ pure ()), Term.info "noop" ~doc ~man

(* AUTHENTICATE *)
let authenticate_doc = "Authenticate with an IMAP server."
let authenticate =
  let doc = authenticate_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,authenticate) command authenticates the user with the IMAP server."
  ] in
  let authenticate user pass =
    assert false
  in
  Term.(pure authenticate $ user $ password), Term.info "authenticate" ~doc ~man

(* SUBSCRIBE *)
let subscribe_doc = "Subscribe to a mailbox. FIXME"
let subscribe =
  let doc = subscribe_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,subscribe) command subscribes the client to a mailbox."
  ] in
  let subscribe m = run g (`Cmd (`Subscribe m)) >>= handle_unit in
  Term.(pure subscribe $ mailbox), Term.info "subscribe" ~doc ~man

(* UNSUBSCRIBE *)
let unsubscribe_doc = "Unsubscribes from a mailbox. FIXME"
let unsubscribe =
  let doc = unsubscribe_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,unsubscribe) command unsubscribes the client to a mailbox."
  ] in
  let unsubscribe m = run g (`Cmd (`Unsubscribe m)) >>= handle_unit in
  Term.(pure unsubscribe $ mailbox), Term.info "unsubscribe" ~doc ~man

(* LIST *)
let list_doc = "List mailboxes."
let list =
  let doc = list_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,list) command shows a list of available mailboxes matching a certain patter."
  ] in
  let list m p =
    let h = function
      | `List _ as u ->
          Imap.pp_response Format.str_formatter u;
          LTerm.printl (Format.flush_str_formatter ())
      | _ -> Lwt.return_unit
    in
    run g (`Cmd (`List (m, p))) >>= handle h
  in
  Term.(pure list $ list_reference $ list_wildcard), Term.info "list" ~doc ~man

(* LSUB *)
let lsub_doc = "List subscribed mailboxes."
let lsub =
  let doc = lsub_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,lsub) command shows a list of subscribed mailboxes matching a certain patter."
  ] in
  let lsub m p =
    let h = function
      | `Lsub _ as u ->
          Imap.pp_response Format.str_formatter u;
          LTerm.printl (Format.flush_str_formatter ())
      | _ -> Lwt.return_unit
    in
    run g (`Cmd (`Lsub (m, p))) >>= handle h
  in
  Term.(pure lsub $ list_reference $ list_wildcard), Term.info "lsub" ~doc ~man

(* SELECT *)
let select_doc = "Select a mailbox for further manipulation."
let select =
  let doc = select_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,select) command opens a mailbox in read-write mode in order \
        to inspect and change its contents."
  ] in
  let select condstore m =
    let mode = if condstore then `Condstore else `Plain in
    run g (`Cmd (`Select (mode, m))) >>= handle_unit
  in
  Term.(pure select $ condstore $ mailbox), Term.info "select" ~doc ~man

(* EXAMINE *)
let examine_doc = "Open a mailbox (READ-ONLY)."
let examine =
  let doc = examine_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,examine) command opens a mailbox in read-only mode in order \
        to inspect its contents."
  ] in
  let examine condstore m =
    let mode = if condstore then `Condstore else `Plain in
    run g (`Cmd (`Examine (mode, m))) >>= handle_unit
  in
  Term.(pure examine $ condstore $ mailbox), Term.info "examine" ~doc ~man

(* CREATE *)
let create_doc = "Create a new mailbox."
let create =
  let doc = create_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,create) command creates a new mailbox."
  ] in
  let create m =
    run g (`Cmd (`Create m)) >>= handle_unit
  in
  Term.(pure create $ mailbox), Term.info "create" ~doc ~man

(* RENAME *)
let rename_doc = "Rename an existing mailbox."
let rename =
  let doc = rename_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,rename) command renames an existing mailbox."
  ] in
  let rename oldm newm = run g (`Cmd (`Rename (oldm, newm))) >>= handle_unit in
  Term.(pure rename $ mailbox $ mailbox), Term.info "rename" ~doc ~man

(* STATUS *)
let status_doc = "Query mailbox information."
let status =
  let doc = status_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,status) commands queries the IMAP server for mailbox \
        information (number of messages, etc.)."
  ] in
  let status m atts =
    let h = function
      | `Status _ as u ->
          Imap.pp_response Format.str_formatter u;
          LTerm.printl (Format.flush_str_formatter ())
      | _ -> Lwt.return_unit
    in
    run g (`Cmd (`Status (m, atts))) >>= handle h
  in
  Term.(pure status $ mailbox $ status_att), Term.info "status" ~doc ~man

(* CLOSE *)
let close_doc = "Closes the currently selected mailbox."
let close =
  let doc = close_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,close) command closes the currently selected mailbox."
  ] in
  let close () = run g (`Cmd `Close) >>= handle_unit in
  Term.(pure close $ pure ()), Term.info "close" ~doc ~man

(* FETCH *)
let fetch_doc = "Fetch message attributes."
let fetch =
  let doc = fetch_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,fetch) command retrieves message properties."
  ] in
  let fetch set uid att changed_since vanished =
    let h = function
      | `Fetch _ as u ->
          Imap.pp_response Format.str_formatter u;
          LTerm.printl (Format.flush_str_formatter ())
      | _ -> Lwt.return_unit
    in
    let uid = if uid then `Uid else `Seq in
    let changed_since = match changed_since, vanished with
      | None, _ -> `All
      | Some m, false -> `Changed_since m
      | Some m, true -> `Changed_since_vanished m
    in
    run g (`Cmd (`Fetch (uid, set, `List att, changed_since))) >>= handle h
  in
  Term.(pure fetch $ set $ uid $ fetch_att $ changed_since $ vanished), Term.info "fetch" ~doc ~man

(* STORE *)
let store_doc = "Modify message flags & labels."
let store =
  let doc = store_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The command $(b,store) modifies the flags and labels associated \
        with a given set of messages."
  ] in
  let store mode set uid silent unchanged_since flags labels =
    let uid = if uid then `Uid else `Seq in
    let silent = if silent then `Silent else `Loud in
    let unchanged_since = match unchanged_since with
      | None -> `All
      | Some m -> `Unchanged_since m
    in
    begin
      if List.length flags > 0
      then run g (`Cmd (`Store (uid, set, silent, unchanged_since, mode, `Flags flags))) >>= handle_unit else
      Lwt.return_unit
    end >>= fun () ->
    begin
      if List.length labels > 0
      then run g (`Cmd (`Store (uid, set, silent, unchanged_since, mode, `Labels labels))) >>= handle_unit else
      Lwt.return_unit
    end
  in
  Term.(pure store $ store_mode $ set $ uid $ silent $ unchanged_since $ flags $ labels),
  Term.info "store" ~doc ~man

(* SEARCH *)
let search_doc = "Search for message numbers of messages satsifying some criteria."
let search =
  let doc = search_doc in
  let search uid key =
    let uid = if uid then `Uid else `Seq in
    let h = function
      | `Search _ as u ->
          Imap.pp_response Format.str_formatter u;
          LTerm.printl (Format.flush_str_formatter ())
      | _ ->
          Lwt.return_unit
    in
    run g (`Cmd (`Search (uid, key))) >>= handle h
  in
  Term.(pure search $ uid $ search_key), Term.info "search" ~doc

(* ENABLE *)
let enable_doc = "Enable server capabilities."
let enable =
  let doc = enable_doc in
  let enable caps =
    let h = function
      | `Enabled _ as u ->
          Imap.pp_response Format.str_formatter u;
          LTerm.printl (Format.flush_str_formatter ())
      | _ -> Lwt.return_unit
    in
    run g (`Cmd (`Enable caps)) >>= handle h
  in
  Term.(pure enable $ capabilities), Term.info "enable" ~doc

(* IDLE *)
let idle_doc = "IDLE"
let idle =
  let doc = idle_doc in
  let idle () =
    let h _ = run g `Idle_done >>= handle_unit in
    run g `Idle >>= handle h
  in
  Term.(pure idle $ pure ()), Term.info "idle" ~doc

let commands =
  [ connect;
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
    idle ]

(* A mini shell *)

open CamomileLibraryDyn.Camomile
open React
open Lwt
open LTerm_style
open LTerm_text
open LTerm_geom

(* +-----------------------------------------------------------------+
   | Prompt creation                                                 |
   +-----------------------------------------------------------------+ *)

(* The function [make_prompt] creates the prompt. Parameters are:

   - size: the current size of the terminal.
   - exit_code: the exit code of the last executed command.
   - time: the current time. *)
let make_prompt size exit_code time =
  let tm = Unix.localtime time in
  let code = string_of_int exit_code in

  (* Replace the home directory by "~" in the current path. *)
  let path = Sys.getcwd () in
  let path =
    try
      let home = Sys.getenv "HOME" in
      if Zed_utf8.starts_with path home then
        Zed_utf8.replace path 0 (Zed_utf8.length home) "~"
      else
        path
    with Not_found ->
      path
  in

  (* Shorten the path if it is too large for the size of the
     terminal. *)
  let path_len = Zed_utf8.length path in
  let size_for_path = size.cols - 24 - Zed_utf8.length code in
  let path =
    if path_len > size_for_path then
      if size_for_path >= 2 then
        ".." ^ Zed_utf8.after path (path_len - size_for_path + 2)
      else
        path
    else
      path
  in

  eval [
    B_bold true;

    B_fg lcyan;
    S"─( ";
    B_fg lmagenta; S(Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec); E_fg;
    S" )─< ";
    B_fg lyellow; S path; E_fg;
    S" >─";
    S(Zed_utf8.make
        (size.cols - 24 - Zed_utf8.length code - Zed_utf8.length path)
        (UChar.of_int 0x2500));
    S"[ ";
    B_fg(if exit_code = 0 then lwhite else lred); S code; E_fg;
    S" ]─";
    E_fg;
    S"\n";

    B_fg lred; S(try Sys.getenv "USER" with Not_found -> ""); E_fg;
    B_fg lgreen; S"@"; E_fg;
    B_fg lblue; S(Unix.gethostname ()); E_fg;
    B_fg lgreen; S" $ "; E_fg;

    E_bold;
  ]

(* +-----------------------------------------------------------------+
   | Listing binaries of the path for completion                     |
   +-----------------------------------------------------------------+ *)

module String_set = Set.Make(String)

let space_re = Str.regexp "[ \t]+"
let split s = Str.split space_re s

(* +-----------------------------------------------------------------+
   | Customization of the read-line engine                           |
   +-----------------------------------------------------------------+ *)

(* Signal updated every second with the current time. *)
let time =
  let time, set_time = S.create (Unix.time ()) in
  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> set_time (Unix.time ())));
  time

class read_line ~term ~history = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method show_box = false

  (* initializer *)
    (* self#set_prompt (S.l2 (fun size time -> make_prompt size 345 time) self#size time) *)
end

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let make_formatter term =
  let m = Lwt_mutex.create () in
  let open Format in
  let out_string s i l = ignore (Lwt_mutex.with_lock m (fun () -> LTerm.fprint term (String.sub s i l))) in
  let out_flush () = ignore (Lwt_mutex.with_lock m (fun () -> LTerm.flush term)) in
  make_formatter out_string out_flush

let rec loop term history exit_code =
  let std_formatter = make_formatter term in
  match_lwt
    try_lwt
      lwt command = (new read_line ~term ~history:(LTerm_history.contents history))#run in
      return (Some command)
    with Sys.Break ->
      return None
  with
    | Some command ->
        lwt status =
          let s = Array.of_list (split command) in
          if Array.length s > 0 then
            let t = List.find (fun (_, i) -> Term.name i = s.(0)) commands in
            let r = Term.eval ~help:std_formatter ~err:std_formatter ~argv:s t in
            match r with
            | `Version | `Help | `Error _ -> Lwt.return (Unix.WEXITED 1)
            | `Ok v -> v >>= fun _ -> Lwt.return (Unix.WEXITED 0)
          else
          Lwt.return (Unix.WEXITED 0)
          (* try_lwt *)
          (*   Lwt_process.exec (Lwt_process.shell command) *)
          (* with Unix.Unix_error (Unix.ENOENT, _, _) -> *)
          (*   lwt () = LTerm.fprintls term (eval [B_fg lred; S "command not found"]) in *)
          (*   return (Unix.WEXITED 127) *)
        in
        LTerm_history.add history command;
        loop
          term
          history
          (match status with
             | Unix.WEXITED code -> code
             | Unix.WSIGNALED code -> code
             | Unix.WSTOPPED code -> code)
    | None ->
        loop term history 130

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let history_file = Filename.concat (Sys.getenv "HOME") "/.imapsh_history"

lwt () =
  lwt () = LTerm_inputrc.load () in
  let hist = LTerm_history.create [] in
  try_lwt
    lwt () = LTerm_history.load hist history_file in
    lwt term = Lazy.force LTerm.stdout in
    loop term hist 0
  with LTerm_read_line.Interrupt ->
    LTerm_history.save hist ~perm:0o600 history_file
