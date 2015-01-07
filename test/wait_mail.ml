(* this code is in the public domain *)

let io_buffer_size = 4096

let debug_flag = ref false

let run sock i o c v =
  let rec write_fully s off len =
    if len > 0 then
      let rc = Ssl.write sock s off len in
      write_fully s (off + rc) (len - rc)
  in
  let rec loop = function
    | `Await_src ->
        let rc = Ssl.read sock i 0 (Bytes.length i) in
        if !debug_flag then Format.eprintf ">>> %d\n%s>>>\n%!" rc (String.sub i 0 rc);
        Imap.Manual.src c i 0 rc;
        loop (Imap.run c `Await)
    | `Await_dst ->
        let rc = Bytes.length o - Imap.Manual.dst_rem c in
        write_fully o 0 rc;
        if !debug_flag then Format.eprintf "<<< %d\n%s<<<\n%!" rc (String.sub o 0 rc);
        Imap.Manual.dst c o 0 (Bytes.length o);
        loop (Imap.run c `Await)
    | `Untagged _ as r -> r
    | `Ok -> `Ok
    | `Error e ->
        Format.eprintf "@[IMAP Error: %a@]@." Imap.pp_error e;
        failwith "imap error"
  in
  loop (Imap.run c v)

let () =
  Ssl.init ()

let wait_mail host port user pass mbox =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let he = Unix.gethostbyname host in
  Unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port));
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  let sock = Ssl.embed_socket fd ctx in
  Ssl.connect sock;
  let c = Imap.connection `Manual `Manual in
  let i = Bytes.create io_buffer_size in
  let o = Bytes.create io_buffer_size in
  Imap.Manual.dst c o 0 (Bytes.length o);
  match run sock i o c `Await with
  | `Ok ->
      let rec logout = function
        | `Untagged _ -> logout (run sock i o c `Await)
        | `Ok -> ()
      in
      let rec idle uidn = function
        | `Untagged (`Exists _) -> idle_done uidn (run sock i o c (`Idle_done))
        | `Untagged _ -> idle uidn (run sock i o c `Await) | `Ok -> idle_done uidn `Ok
      and idle_done uidn = function
        | `Untagged _ -> idle_done uidn (run sock i o c `Await)
        | `Ok ->
            search uidn Uint32.zero
              (run sock i o c (`Cmd (Imap.search ~uid:true (`Uid [uidn, Uint32.max_int]))))
      and search uidn n = function
        | `Untagged (`Search (n :: _, _)) -> search uidn n (run sock i o c `Await)
        | `Untagged _ -> search uidn n (run sock i o c `Await)
        | `Ok ->
            if n = Uint32.zero then idle uidn (run sock i o c `Idle) else
            let cmd = Imap.fetch ~uid:true ~changed:Uint64.one [n, n] [`Envelope] in
            fetch uidn n None (run sock i o c (`Cmd cmd))
      and fetch uidn n name = function
        | `Untagged (`Fetch (_, att)) ->
            let name =
              List.fold_left
                (fun name att -> match att with
                   | `Envelope e ->
                       begin match e.Imap.env_from with
                       | [] -> name
                       | ad :: _ -> Some ad.Imap.ad_name
                       end
                   | _ -> name) name att
            in
            fetch uidn n name (run sock i o c `Await)
        | `Untagged _ -> fetch uidn n name (run sock i o c `Await)
        | `Ok ->
            let name = match name with None -> "<unnamed>" | Some name -> name in
            Format.printf "New mail from %s, better go and check it out!\n%!" name;
            logout (run sock i o c (`Cmd Imap.logout))
      in
      let rec select uidn = function
        | `Untagged (`Ok (`Uid_next uidn, _)) -> select uidn (run sock i o c `Await)
        | `Untagged _ -> select uidn (run sock i o c `Await)
        | `Ok -> idle uidn (run sock i o c `Idle)
      in
      let rec login = function
        | `Untagged _ -> login (run sock i o c `Await)
        | `Ok -> select Uint32.zero (run sock i o c (`Cmd (Imap.select mbox)))
      in
      login (run sock i o c (`Cmd (Imap.login user pass)))
  | `Untagged _ -> assert false

open Cmdliner

let host =
  let doc = Arg.info ~docv:"HOST" ~doc:"Server hostname" [] in
  Arg.(required & pos 0 (some string) None & doc)

let port =
  let doc = Arg.info ~docv:"PORT" ~doc:"Server port" ["port"; "p"] in
  Arg.(value & opt int 993 & doc)

let user =
  let doc = Arg.info ~docv:"USER" ~doc:"Username" [] in
  Arg.(required & pos 1 (some string) None & doc)

let pass =
  let doc = Arg.info ~docv:"PASSWORD" ~doc:"Password" [] in
  Arg.(required & pos 2 (some string) None & doc)

let mbox =
  let doc = Arg.info ~docv:"MAILBOX" ~doc:"Mailbox to watch" [] in
  Arg.(required & pos 3 (some string) None & doc)

let debug =
  let doc = Arg.info ~doc:"Show debug info" ["debug"; "d"] in
  Arg.(value & flag doc)

let main =
  let main d = debug_flag := d; wait_mail in
  Term.(pure main $ debug $ host $ port $ user $ pass $ mbox),
  Term.info "wait_mail"

let () =
  match Term.eval ~catch:true main with
  | `Ok _ | `Version | `Help -> ()
  | `Error _ -> exit 1
