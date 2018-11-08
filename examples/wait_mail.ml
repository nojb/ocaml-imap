(* this code is in the public domain *)

(** {1:ex Example: checking for new mail}

    [wait_mail host port user pass mbox] logs into the IMAP server
    [host] on port [port] over {{:https://github.com/savonet/ocaml-ssl}SSL},
    authenticates user [user] with password [pass] and watches mailbox [mbox] until a
    new message arrives.  When a new message arrives, it outputs the sender's name
    and address and stops.

    See the
    {{:https://github.com/nojb/ocaml-imap/blob/master/test/wait_mail.ml}wait_mail.ml}
    file for a more complete version of this example.

    Setting [debug_flag := true] will output all the data exchanged with the
    server which can be quite instructive. *)

open Lwt.Infix

let get =
  let re = Re.(compile (str "\r\n")) in
  fun header s ->
    let header = String.lowercase_ascii header in
    let l = Re.split re s in
    let l =
      List.map (fun s ->
          match String.index s ':' with
          | i ->
              String.lowercase_ascii (String.trim (String.sub s 0 i)),
              String.trim (String.sub s (i+1) (String.length s - (i+1)))
          | exception Not_found ->
              String.trim s, ""
        ) l
    in
    List.assoc_opt header l

module M = Map.Make (struct type t = int32 let compare = Int32.compare end)

let wait_mail host ?port username password mailbox =
  Imap.connect ~host ?port ~username ~password >>= fun imap ->
  Imap.examine imap mailbox >>= fun () ->
  Imap.uid_search imap Imap.Search.all >>= fun (uids, _) ->
  (* Lwt_list.iter_s (fun uid -> Lwt_io.printlf "%ld" uid) lst >>= fun () -> *)
  let strm =
    let mime = Imap.MIME.Section.HEADER_FIELDS ["Message-Id"] in
    Imap.uid_fetch imap uids
      [
        Imap.Fetch.Request.body_section ~peek:true ~section:([], Some mime) ();
        Imap.Fetch.Request.rfc822_size;
        Imap.Fetch.Request.internaldate;
        Imap.Fetch.Request.flags;
      ]
  in
  Lwt_stream.fold_s (fun ((seq : Imap.seq), resp) map ->
      match resp with
      | {Imap.Fetch.Response.body_section = ([] | [_, ""]); _} ->
          Lwt_io.printlf "missing data for seq %ld" (seq :> int32) >>= fun () ->
          Lwt.return map
      | {Imap.Fetch.Response.body_section = [_, s];
         uid;
         rfc822_size = Some size;
         internaldate;
         flags; _} ->
          Lwt_io.printl  "---------------------------------------------------" >>= fun () ->
          Lwt_io.printlf "          Seq: %ld" (seq :> int32) >>= fun () ->
          Lwt_io.printlf "          UID: %ld" uid >>= fun () ->
          Lwt_io.printlf "   Message-Id: %s"
            (match get "Message-Id" s with None -> "<none>" | Some s -> s) >>= fun () ->
          Lwt_io.printlf "         Size: %d" size >>= fun () ->
          Lwt_io.printlf "Internal Date: %S" internaldate >>= fun () ->
          Lwt_io.printlf "        Flags: %s"
            (String.concat ", " (List.map Imap.Flag.to_string flags)) >>= fun () ->
          let sign =
            match get "Message-Id" s with
            | None -> String.concat "\000" [internaldate; string_of_int size]
            | Some mid -> mid
          in
          let digest = Digest.to_hex (Digest.string sign) in
          Lwt.return (M.add uid digest map)
      | _ ->
          Lwt_io.printlf "missing data for seq %ld" (seq :> int32) >>= fun () ->
          Lwt.return map
    ) strm M.empty
  >>= fun map ->
  let strm = Imap.uid_fetch imap uids [Imap.Fetch.Request.rfc822] in
  Lwt_stream.iter_s (fun (_seq, {Imap.Fetch.Response.uid; rfc822; _}) ->
      let digest = M.find uid map in
      Lwt_io.open_file ~mode:Lwt_io.output (Filename.concat "tmp" digest) >>= fun oc ->
      Lwt_io.fprint oc rfc822 >>= fun () ->
      Lwt_io.close oc >>= fun () ->
      Lwt_io.printlf "Message %s saved." digest
    ) strm
  >>= fun () ->
  Imap.disconnect imap
  (* let rec loop () =
   *   let uidnext =
   *     match Imap.uidnext imap with
   *     | Some n -> n
   *     | None -> failwith "Could not determine UIDNEXT"
   *   in
   *   Imap.poll imap >>= fun () ->
   *   Imap.uid_search imap Imap.Search.(unseen && uid [uidnext]) >>= function
   *   | (n :: _), _ ->
   *       Lwt_stream.to_list (Imap.uid_fetch imap [n] [Imap.Fetch.Request.envelope]) >>= begin function
   *       | [_, {envelope = Some {Imap.Envelope.env_from = {Imap.Envelope.Address.ad_name; ad_mailbox; ad_host; _} :: _; _}; _}; _] ->
   *           Lwt_io.printlf "New mail! (from \"%s\" <%s@%s>)" ad_name ad_mailbox ad_host >>= fun () ->
   *           Imap.disconnect imap
   *       | _ ->
   *           loop ()
   *       end
   *   | [], _ ->
   *       loop ()
   * in
   * loop () *)

