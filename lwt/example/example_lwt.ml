open Imap_lwt
open Lwt.Infix

let () =
  Printexc.record_backtrace true

let username = ref ""
let password = ref ""
let host = ref "imap.gmail.com"
let port = ref 993

let spec =
  let open Arg in
  [
    "-username", Set_string username, " Username";
    "-password", Set_string password, " Password";
    "-host", Set_string host, " IMAP Host";
    "-port", Set_int port, " IMAP Port";
  ]

let usage_msg =
  Printf.sprintf "Usage: %s <options>" Sys.argv.(0)

let main () =
  Arg.(parse (align spec) ignore usage_msg);
  if !username = "" || !password = "" then raise (Arg.Bad "Missing username or password");
  connect !port !host !username !password  >>= fun _ ->
  Lwt.return_unit

let () =
  try
    Lwt_main.run (main ())
  with
  | Arg.Bad s ->
      Printf.eprintf "%s: %s\n%!" Sys.argv.(0) s;
      Arg.(usage (align spec) usage_msg);
      exit 2
  | e ->
      let s = match e with Failure s -> s | e -> Printexc.to_string e in
      Printf.eprintf ">> Failure: %s\n%!" s;
      Printexc.print_backtrace stderr;
      exit 2
