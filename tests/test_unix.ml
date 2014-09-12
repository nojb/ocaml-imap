open ImapTypes

let _ =
  try
    Printf.printf "Login: %!";
    let user = read_line () in
    Printf.printf "Password: %!";
    let pass = read_line () in
    let s = ImapUnix.create_session "imap.gmail.com" in
    let _ = ImapUnix.connect s in
    let _ = ImapUnix.send_command s ImapCommands.noop in
    let _ = ImapUnix.send_command s ImapCommands.capability in
    let _ = ImapUnix.send_command s (ImapCommands.authenticate (ImapAuth.plain user pass)) in
    let _ = ImapUnix.send_command s (ImapEnable.enable [CAPABILITY_NAME "UIDPLUS"]) in
    let _ = ImapUnix.send_command s (ImapCondstore.select "INBOX") in
    let _ = ImapUnix.send_command s (ImapCondstore.search_modseq SEARCH_KEY_ALL) in
    let _ = ImapUnix.send_command s
        (ImapCommands.fetch (ImapSet.single_int 1) [FETCH_ATT_ENVELOPE; ImapXgmlabels.fetch_att_xgmlabels]) in
    let _ = ImapUnix.send_command s (ImapId.id_basic "ocaml-imap" "0.1") in
    let _ = ImapUnix.send_command s ImapIdle.idle in
    let _ = ImapUnix.send_command s (ImapCommands.logout) in
    prerr_endline "OK!"
  with
    ImapUnix.Error err ->
      Printf.eprintf "Error: %s\n%!" (ImapCore.string_of_error err)
