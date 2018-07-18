open Lwt.Infix

type imap_account =
  {
    host: string;
    port: int option;
    username: string;
    password: string;
  }

class message acc mailbox uid =
  let {host; port; username; password} = acc in
  object
    method fetch_headers =
      Core.connect ~host ?port ~username ~password >>= fun imap ->
      Core.examine imap mailbox >>= fun () ->
      let strm = Core.uid_fetch imap [uid] [Fetch.Request.rfc822_header] in
      Lwt_stream.next strm >|= fun (_, {Fetch.Response.rfc822_header; _}) ->
      let parts = Re.split Re.(compile (str "\r\n")) rfc822_header in
      let parts = List.filter (function "" -> false | _ -> true) parts in
      let rec loop acc curr = function
        | s :: rest ->
            if s.[0] = '\t' || s.[0] = ' ' then
              loop acc (curr ^ " " ^ String.trim s) rest
            else
              loop (curr :: acc) (String.trim s) rest
        | [] ->
            if curr <> "" then
              List.rev (curr :: acc)
            else
              List.rev acc
      in
      let parts = loop [] "" parts in
      let parts = List.filter (function "" -> false | _ -> true) parts in
      (* List.iter (fun s -> Printf.eprintf "%S\n%!" s) parts; *)
      List.map (fun part ->
          let i =
            match String.index part ':' with
            | i -> i
            | exception Not_found -> String.length part
          in
          String.trim (String.sub part 0 i),
          String.trim (String.sub part (i+1) (String.length part - i - 1))
        ) parts

    method fetch_body =
      Core.connect ~host ?port ~username ~password >>= fun imap ->
      Core.examine imap mailbox >>= fun () ->
      let strm = Core.uid_fetch imap [uid] [Fetch.Request.rfc822] in
      Lwt_stream.next strm >>= fun (_, {Fetch.Response.rfc822_text; _}) ->
      Lwt.return rfc822_text
  end

class message_set acc mailbox query =
  let {host; port; username; password} = acc in
  object
    method count =
      Core.connect ~host ?port ~username ~password >>= fun imap ->
      Core.examine imap mailbox >>= fun () ->
      Core.uid_search imap query >|= fun (uids, _) ->
      List.length uids

    method get uid =
      new message acc mailbox uid
      (* Core.connect ~host ?port ~username ~password >>= fun imap ->
       * Core.examine imap mailbox >>= fun () ->
       * Core.search imap (Search.uid [uid]) >>= function
       * | (_ :: _), _ ->
       *     Lwt.return (new message acc mailbox uid)
       * | [], _ ->
       *     Lwt.fail (Failure "no such message") *)

    method uids =
      Core.connect ~host ?port ~username ~password >>= fun imap ->
      Core.examine imap mailbox >>= fun () ->
      Core.uid_search imap query >|= fun (uids, _) ->
      uids

    method contain_from s =
      new message_set acc mailbox Search.(query && from s)

    method is_unseen =
      new message_set acc mailbox Search.(query && unseen)
  end

class mailbox acc mailbox =
  object
    inherit message_set acc mailbox Search.all
    method name = mailbox
  end

class account ~host ?port ~username ~password () =
  let acc = {host; port; username; password} in
  object
    method inbox = new mailbox acc "INBOX"
    method list_all =
      let mk (_, _, name) = new mailbox acc name in
      Core.connect ~host ?port ~username ~password >>= fun imap ->
      Core.list imap "%" >|= List.map mk
  end
