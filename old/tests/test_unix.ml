open ImapSession
  
let _ =
  try
    Printf.printf "Login: %!";
    let user = read_line () in
    Printf.printf "Password: %!";
    let pass = read_line () in
    let s = ImapUnix.create_session "imap.gmail.com" user pass in
    let _ = ImapUnix.connect s in
    (* let _ = ImapUnix.run s ImapCommands.noop in *)
    (* let _ = ImapUnix.run s ImapCommands.capability in *)
    let _ = ImapUnix.run s (login) in
    let _ = ImapUnix.run s (enable_feature "UIDPLUS") in
    let _ = ImapUnix.run s (select "INBOX") in
    (* let _ = ImapUnix.run s (ImapCondstore.search_modseq SEARCH_KEY_ALL) in *)
    (* let _ = ImapUnix.run s *)
        (* (ImapCommands.fetch (ImapSet.single_int 1) [FETCH_ATT_ENVELOPE; ImapXgmlabels.fetch_att_xgmlabels]) in *)
    (* let _ = ImapUnix.run s (ImapId.id_basic "ocaml-imap" "0.1") in *)
    (* let _ = ImapUnix.run s ImapIdle.idle in *)
    (* let _ = ImapUnix.run s (ImapCommands.logout) in *)
    prerr_endline "OK!"
  with
    ImapUnix.Error err ->
      failwith "error!"
      (* Printf.eprintf "Error: %s\n%!" (ImapSession.string_of_error err) *)
