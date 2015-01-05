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

type s =
  { i : string;
    o : string;
    mutable c : Imap.connection;
    mutable sock : Lwt_ssl.socket option }

type 'a result = [ `Untagged of Imap.untagged | `Ok of 'a | `Error of Imap.error ]

let rec run s r =
  let rec write_fully sock s off len =
    if len > 0 then
      Lwt_ssl.write sock s off len >>= fun rc ->
      write_fully sock s (off + rc) (len - rc)
    else
    Lwt.return_unit
  in
  let sock = match s.sock with None -> invalid_arg "not connected" | Some sock -> sock in
  let rec loop = function
    | `Await_src ->
        (* Printf.eprintf "Await_src\n%!"; *)
        Lwt_ssl.read sock s.i 0 (Bytes.length s.i) >>= fun rc ->
        LTerm.eprintlf ">>> %d\n%s>>>%!" rc (String.sub s.i 0 rc) >>= fun () ->
        Imap.Manual.src s.c s.i 0 rc;
        loop (Imap.run s.c r)
    | `Await_dst ->
        (* Printf.eprintf "Await_dst\n%!"; *)
        let rc = Bytes.length s.o - Imap.Manual.dst_rem s.c in
        write_fully sock s.o 0 rc >>= fun () ->
        LTerm.eprintlf "<<< %d\n%s<<<%!" rc (String.sub s.o 0 rc) >>= fun () ->
        Imap.Manual.dst s.c s.o 0 (Bytes.length s.o);
        loop (Imap.run s.c r)
    | `Untagged _ as r -> Lwt.return r
    | `Ok x -> Lwt.return (`Ok x)
    | `Error _ as e -> Lwt.return e
  in
  loop (Imap.run s.c r)

let connect s host port =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  Lwt_unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >>= fun () ->
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  Lwt_ssl.ssl_connect fd ctx >>= fun sock ->
  s.sock <- Some sock;
  let r = Imap.connect s.c in
  run s r

let capability s =
  run s (Imap.capability s.c)
  (* step s (Imap.capability s.c) *)

(* let login s user pass = *)
  (* step s (Imap.login user pass s.c) *)

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

let host =
  let doc = Arg.info ~docv:"HOST" ~doc:"Server hostname." [] in
  Arg.(required & pos 0 (some string) None & doc)

let port =
  let doc = Arg.info ~docv:"PORT" ~doc:"Port number." ["p"; "port"] in
  Arg.(value & opt int 993 & doc)

let user =
  let doc = Arg.info ~docv:"USERNAME" ~doc:"Username." [] in
  Arg.(required & pos 0 (some string) None & doc)

let password =
  let doc = Arg.info ~docv:"PASSWORD" ~doc:"Password." [] in
  Arg.(required & pos 1 (some string) None & doc)

let mailbox =
  let doc = Arg.info ~docv:"MAILBOX" ~doc:"Mailbox (UTF-8 encoded)." [] in
  Arg.(required & pos 0 (some string) None & doc)

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
    | `Ok `Needs_auth -> LTerm.printl "OK, needs authorization."
    | `Ok `Pre_auth -> LTerm.printl "OK, already authorized."
    | _ -> Lwt.return_unit
  in
  Term.(pure connect $ host $ port), Term.info "connect" ~doc ~man

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
    let r = Imap.capability g.c in
    let rec loop = function
      | `Untagged _ -> run g r >>= loop
      | `Ok caps ->
          LTerm.printl (String.concat ", " (List.map Imap.string_of_capability caps)) >>= fun () ->
          LTerm.printl "OK"
      | _ ->
          Lwt.return_unit
    in
    run g r >>= loop
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
  let login user pass =
    let r = Imap.login user pass g.c in
    let rec loop = function
      | `Untagged _ -> run g r >>= loop
      | `Ok () ->
          LTerm.printl "OK"
      | _ ->
          Lwt.return_unit
    in
    run g r >>= loop
  in
  Term.(pure login $ user $ password), Term.info "login" ~doc ~man

(* NOOP *)
let noop_doc = "Sends a NOOP command to the IMAP server."
let noop =
  let doc = noop_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,noop) command pings the IMAP server.  \
        It is useful to keep a connection from closing due to inactivity."
  ] in
  let noop () =
    let r = Imap.noop g.c in
    let rec loop = function
      | `Untagged _ -> run g r >>= loop
      | `Ok () -> LTerm.printl "OK"
      | _ -> Lwt.return_unit
    in
    run g r >>= loop
  in
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

(* SELECT *)
let select_doc = "Select a mailbox for further manipulation."
let select =
  let doc = select_doc in
  let man = [
    `S "DESCRIPTION";
    `P "The $(b,select) command opens a mailbox in read-write mode in order \
        to inspect and change its contents."
  ] in
  let select m =
    let r = Imap.select m g.c in
    let rec loop = function
      | `Untagged _ -> run g r >>= loop
      | `Ok () -> LTerm.printl "OK"
      | _ -> Lwt.return_unit
    in
    run g r >>= loop
  in
  Term.(pure select $ mailbox), Term.info "select" ~doc ~man

let commands = [
  connect;
  capability;
  login;
  noop;
  authenticate;
  select
]

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

  initializer
    self#set_prompt (S.l2 (fun size time -> make_prompt size 345 time) self#size time)
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

lwt () =
  lwt () = LTerm_inputrc.load () in
  try_lwt
    lwt term = Lazy.force LTerm.stdout in
    loop term (LTerm_history.create []) 0
  with LTerm_read_line.Interrupt ->
    return ()
