open Imap
open Types

let _ =
  try
    Printf.printf "Login: %!";
    let user = read_line () in
    Printf.printf "Password: %!";
    let pass = read_line () in
    let s = ImapUnix.create_session "imap.gmail.com" in
    let _ = ImapUnix.connect s in
    let _ = ImapUnix.send_command s Commands.noop in
    let _ = ImapUnix.send_command s Commands.capability in
    let _ = ImapUnix.send_command s (Commands.authenticate (Auth.plain user pass)) in
    let _ = ImapUnix.send_command s (Enable.enable [CAPABILITY_NAME "UIDPLUS"]) in
    let _ = ImapUnix.send_command s (Condstore.select "INBOX") in
    let _ = ImapUnix.send_command s (Condstore.search_modseq SEARCH_KEY_ALL) in
    let _ = ImapUnix.send_command s
        (Commands.fetch (ImapSet.single_int 1) [FETCH_ATT_ENVELOPE; Xgmlabels.fetch_att_xgmlabels]) in
    let _ = ImapUnix.send_command s (Id.id_basic "ocaml-imap" "0.1") in
    let _ = ImapUnix.send_command s Idle.idle in
    let _ = ImapUnix.send_command s (Commands.logout) in
    prerr_endline "OK!"
  with
    ImapUnix.Error err ->
      Printf.eprintf "Error: %s\n%!" (Core.string_of_error err)
