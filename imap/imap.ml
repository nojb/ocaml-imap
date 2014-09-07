(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open ImapUtils
open ImapTypes

let fresh_response_info = {
  rsp_alert = "";
  rsp_parse = "";
  rsp_badcharset = [];
  rsp_trycreate = false;
  rsp_mailbox_list = [];
  rsp_mailbox_lsub = [];
  rsp_search_results = [];
  rsp_status = {st_mailbox = ""; st_info_list = []};
  rsp_expunged = [];
  rsp_fetch_list = [];
  rsp_extension_list = [];
  rsp_other = ("", "")
}

let fresh_selection_info = {
  sel_perm_flags = [];
  sel_perm = MAILBOX_READONLY;
  sel_uidnext = Uint32.zero;
  sel_uidvalidity = Uint32.zero;
  sel_first_unseen = Uint32.zero;
  sel_flags = [];
  sel_exists = None;
  sel_recent = None;
  sel_unseen = 0
}

let resp_text_store s {rsp_code; rsp_text} =
  match rsp_code with
    RESP_TEXT_CODE_ALERT ->
      {s with rsp_info = {s.rsp_info with rsp_alert = rsp_text}}
  | RESP_TEXT_CODE_BADCHARSET csets ->
      {s with rsp_info = {s.rsp_info with rsp_badcharset = csets}}
  | RESP_TEXT_CODE_CAPABILITY_DATA caps ->
      {s with cap_info = caps}
  | RESP_TEXT_CODE_PARSE ->
      {s with rsp_info = {s.rsp_info with rsp_parse = rsp_text}}
  | RESP_TEXT_CODE_PERMANENTFLAGS flags ->
      {s with sel_info = {s.sel_info with sel_perm_flags = flags}}
  | RESP_TEXT_CODE_READ_ONLY ->
      {s with sel_info = {s.sel_info with sel_perm = MAILBOX_READONLY}}
  | RESP_TEXT_CODE_READ_WRITE ->
      {s with sel_info = {s.sel_info with sel_perm = MAILBOX_READWRITE}}
  | RESP_TEXT_CODE_TRYCREATE ->
      {s with rsp_info = {s.rsp_info with rsp_trycreate = true}}
  | RESP_TEXT_CODE_UIDNEXT uid ->
      {s with sel_info = {s.sel_info with sel_uidnext = uid}}
  | RESP_TEXT_CODE_UIDVALIDITY uid ->
      {s with sel_info = {s.sel_info with sel_uidvalidity = uid}}
  | RESP_TEXT_CODE_UNSEEN unseen ->
      {s with sel_info = {s.sel_info with sel_first_unseen = unseen}}
  (* | RESP_TEXT_CODE_APPENDUID (uidvalidity, uid) -> *)
      (* {s with rsp_info = {s.rsp_info with rsp_appenduid = (uidvalidity, uid)}} *)
  (* | RESP_TEXT_CODE_COPYUID (uidvalidity, src_uids, dst_uids) -> *)
      (* {s with rsp_info = {s.rsp_info with rsp_copyuid = (uidvalidity, src_uids, dst_uids)}} *)
  (* | RESP_TEXT_CODE_UIDNOTSTICKY -> *)
      (* {s with sel_info = {s.sel_info with sel_uidnotsticky = true}} *)
  (* | RESP_TEXT_CODE_COMPRESSIONACTIVE -> *)
      (* {s with rsp_info = {s.rsp_info with rsp_compressionactive = true}} *)
  (* | RESP_TEXT_CODE_HIGHESTMODSEQ modseq -> *)
      (* {s with sel_info = {s.sel_info with sel_highestmodseq = modseq}} *)
  (* | RESP_TEXT_CODE_NOMODSEQ -> *)
      (* {s with sel_info = {s.sel_info with sel_highestmodseq = Modseq.zero}} *)
  (* | RESP_TEXT_CODE_MODIFIED set -> *)
  (* {s with rsp_info = {s.rsp_info with rsp_modified = set}} *)
  | RESP_TEXT_CODE_EXTENSION e ->
      ImapExtension.extension_data_store s e
  | RESP_TEXT_CODE_OTHER other ->
      {s with rsp_info = {s.rsp_info with rsp_other = other}}
  | RESP_TEXT_CODE_NONE ->
      s

let mailbox_data_store s =
  function
    MAILBOX_DATA_FLAGS flags ->
      {s with sel_info = {s.sel_info with sel_flags = flags}}
  | MAILBOX_DATA_LIST mb ->
      {s with rsp_info =
                {s.rsp_info with rsp_mailbox_list =
                                   s.rsp_info.rsp_mailbox_list @ [mb]}}
  | MAILBOX_DATA_LSUB mb ->
      {s with rsp_info =
                {s.rsp_info with rsp_mailbox_list =
                                   s.rsp_info.rsp_mailbox_list @ [mb]}}
  (* rsp_mailbox_lsub = s.rsp_info.rsp_mailbox_lsub @ [mb] } *)
  | MAILBOX_DATA_SEARCH results ->
      {s with rsp_info = {s.rsp_info with
                          rsp_search_results = s.rsp_info.rsp_search_results @ results}}
                          (* rsp_search_results_modseq = *)
                            (* let max x y = if Modseq.compare x y <= 0 then y else x in *)
                            (* max modseq s.rsp_info.rsp_search_results_modseq}} *)
  | MAILBOX_DATA_STATUS status ->
      {s with rsp_info = {s.rsp_info with rsp_status = status}}
  | MAILBOX_DATA_EXISTS n ->
      {s with sel_info = {s.sel_info with sel_exists = Some n}}
  | MAILBOX_DATA_RECENT n ->
      {s with sel_info = {s.sel_info with sel_recent = Some n}}
  | MAILBOX_DATA_EXTENSION_DATA e ->
      ImapExtension.extension_data_store s e

let message_data_store s =
  function
    MESSAGE_DATA_EXPUNGE n ->
      let s =
        {s with rsp_info = {s.rsp_info with rsp_expunged = s.rsp_info.rsp_expunged @ [n]}}
      in
      begin match s.sel_info.sel_exists with
        | Some n ->
            {s with sel_info = {s.sel_info with sel_exists = Some (n-1)}}
        | None ->
            s
      end
  | MESSAGE_DATA_FETCH att ->
      {s with rsp_info = {s.rsp_info with rsp_fetch_list = s.rsp_info.rsp_fetch_list @ [att]}}

let resp_cond_state_store s {rsp_text = r} =
  resp_text_store s r

let resp_cond_bye_store s r =
  resp_text_store s r

let response_data_store s =
  function
    RESP_DATA_COND_STATE r ->
      resp_cond_state_store s r
  | RESP_DATA_COND_BYE r ->
      resp_cond_bye_store s r
  | RESP_DATA_MAILBOX_DATA r ->
      mailbox_data_store s r
  | RESP_DATA_MESSAGE_DATA r ->
      message_data_store s r
  | RESP_DATA_CAPABILITY_DATA cap_info ->
      {s with cap_info}
  | RESP_DATA_EXTENSION_DATA e ->
      ImapExtension.extension_data_store s e
  (* | `ID params -> *)
  (*   {s with rsp_info = {s.rsp_info with rsp_id = params}} *)
  (* | `NAMESPACE (pers, other, shared) -> *)
  (*   {s with rsp_info = {s.rsp_info with rsp_namespace = pers, other, shared}} *)
  (* | `ENABLED caps -> *)
  (*   {s with rsp_info = {s.rsp_info with rsp_enabled = caps}} *)

let response_tagged_store s {rsp_cond_state = r} =
  resp_cond_state_store s r

let response_fatal_store s r =
  resp_cond_bye_store s r

let resp_cond_auth_store s {rsp_text = r} =
  resp_text_store s r

let greeting_store s =
  function
    GREETING_RESP_COND_AUTH r ->
      resp_cond_auth_store s r
  | GREETING_RESP_COND_BYE r ->
      resp_cond_bye_store s r

let text_of_response_done =
  function
    RESP_DONE_TAGGED {rsp_cond_state = {rsp_text}}
  | RESP_DONE_FATAL rsp_text ->
    rsp_text

let response_done_store s =
  function
    RESP_DONE_TAGGED r ->
      response_tagged_store s r
  | RESP_DONE_FATAL r ->
      response_fatal_store s r

let cont_req_or_resp_data_store s =
  function
    RESP_CONT_REQ r ->
      s
  | RESP_CONT_DATA r ->
      response_data_store s r

let response_store s {rsp_cont_req_or_resp_data_list; rsp_resp_done} =
  response_done_store
    (List.fold_left cont_req_or_resp_data_store s rsp_cont_req_or_resp_data_list)
    rsp_resp_done

let debug =
  try let s = Sys.getenv "IMAP_DEBUG" in ref (s <> "0")
  with Not_found -> ref false

(* exception NO *)
(* exception BAD *)
(* exception BYE *)
(* exception Parse_error of string * int *)
(* exception Bad_tag *)
(* exception Io_error of exn *)
(* exception Auth_error of exn *)

let fresh_state = {
  rsp_info = fresh_response_info;
  sel_info = fresh_selection_info;
  cap_info = [];
  imap_response = "";
  current_tag = None;
  next_tag = 0
}

(* let run_parser session p = *)
(*   let rec loop = *)
(*     function *)
(*       ImapParser.Ok (x, i) -> *)
(*         session.i <- i; *)
(*         IO.return x *)
(*     | ImapParser.Fail i -> *)
(*         IO.fail (Parse_error (Buffer.contents ci.buffer, i)) *)
(*     | ImapParser.Need (len, k) -> *)
(*         let ic, _  = ci.chan in *)
(*         IO.read 65536 ic >>= *)
(*         begin *)
(*           function *)
(*             "" -> *)
(*               loop (k ImapParser.End) *)
(*           | _ as data -> *)
(*               Buffer.add_string ci.buffer data; *)
(*               loop (k ImapParser.More) *)
(*         end *)
(*   in *)
(*   loop (p ci.buffer ci.i) *)

let get_response ci tag =
  assert false
(*   ci.state <- {ci.state with rsp_info = fresh_response_info}; *)
(*   let rec loop () = *)
(*     read_resp_data_or_resp_done ci >>= *)
(*     function *)
(*       `BYE _ -> *)
(*         ci.disconnect () >>= fun () -> *)
(*         IO.fail BYE *)
(*     | #ImapResponse.response_data -> *)
(*         loop () *)
(*     | `TAGGED (tag', `OK rt) -> *)
(*         if tag <> tag' then IO.fail Bad_tag *)
(*         else IO.return rt *)
(*     | `TAGGED (_, `BAD rt) -> *)
(*         IO.fail BAD *)
(*     | `TAGGED (_, `NO rt) -> *)
(*         IO.fail NO *)
(*   in *)
(*   loop () *)

let get_idle_response ci tag f stop =
  assert false
(*   ci.state <- {ci.state with rsp_info = fresh_response_info}; *)
(*   let rec loop () = *)
(*     read_cont_req_or_resp_data_or_resp_done ci >>= *)
(*     function *)
(*       `BYE _ -> *)
(*         ci.disconnect () >>= fun () -> *)
(*         IO.fail BYE *)
(*     | #ImapResponse.response_data -> *)
(*         begin *)
(*           match f () with *)
(*             `Continue -> loop () *)
(*           | `Stop -> stop (); loop () *)
(*         end *)
(*     | `TAGGED (tag', `OK _) -> *)
(*         if tag <> tag' then IO.fail Bad_tag *)
(*         else IO.return () *)
(*     | `TAGGED (_, `BAD rt) -> *)
(*         IO.fail BAD *)
(*     | `TAGGED (_, `NO rt) -> *)
(*         IO.fail NO *)
(*     | `CONT_REQ _ -> *)
(*         loop () *)
(*   in *)
(*   loop () *)

let get_auth_response step ci tag =
  assert false
(*   ci.state <- {ci.state with rsp_info = fresh_response_info}; *)
(*   let rec loop needs_more = *)
(*     read_cont_req_or_resp_data_or_resp_done ci >>= *)
(*     function *)
(*       `BYE _ -> *)
(*         ci.disconnect () >>= fun () -> *)
(*         IO.fail BYE *)
(*     | #ImapResponse.response_data -> *)
(*         loop needs_more *)
(*     | `TAGGED (tag', `OK _) -> *)
(*         begin *)
(*           if needs_more then step "" else IO.return `OK end >>= begin *)
(*           function *)
(*             `OK -> *)
(*               if tag <> tag' then IO.fail Bad_tag *)
(*               else IO.return () *)
(*           | `NEEDS_MORE -> *)
(*               IO.fail (Auth_error (Failure "Insufficient data for SASL authentication")) *)
(*         end *)
(*     | `TAGGED (_, `BAD rt) -> *)
(*         IO.fail BAD *)
(*     | `TAGGED (_, `NO rt) -> *)
(*         IO.fail NO *)
(*     | `CONT_REQ data -> *)
(*         let data = *)
(*           match data with *)
(*             `BASE64 data -> data *)
(*           | `TEXT _ -> "" *)
(*         in *)
(*         step data >>= *)
(*         function *)
(*           `OK -> loop false *)
(*         | `NEEDS_MORE -> loop true *)
(*   in *)
(*   loop true *)

let get_continuation_request ci =
  assert false
(* read ci ImapResponse.continue_req (fun _ _ -> ()) >|= function (`CONT_REQ x) -> x *)

(* let make () = *)
(*   { conn_state = Disconnected } *)

(* let connect' s chan = *)
(*   let disconnect' chan = *)
(*     s.conn_state <- Disconnected; *)
(*     IO.catch (fun () -> IO.disconnect chan) (fun _ -> IO.return ()) *)
(*   in *)
(*   match s.conn_state with *)
(*     Disconnected -> *)
(*       let ci = { *)
(*         chan; next_tag = 1; *)
(*         buffer = Buffer.create 0; *)
(*         i = 0; *)
(*         imap_response = ""; *)
(*         imap_tag = ""; *)
(*         state = { *)
(*           rsp_info = fresh_response_info; *)
(*           sel_info = fresh_selection_info; *)
(*           cap_info = [] *)
(*         }; *)
(*         compress_deflate = false; *)
(*         send_lock = IO.create_mutex (); *)
(*         disconnect = (fun () -> disconnect' chan) *)
(*       } *)
(*       in *)
(*       s.conn_state <- Connected ci; *)
(*       parse_greeting ci *)
(*   | _ -> *)
(*       IO.fail (Failure "Imap.connect: already connected") *)

(* let connect s ?(port=143) host = *)
(*   IO.connect port host >>= connect' s *)

(* let connect_ssl s ?(version=`TLSv1) ?ca_file ?(port=993) host = *)
(*   IO.connect_ssl version ?ca_file port host >>= connect' s *)

(* let disconnect s = *)
(*   match s.conn_state with *)
(*     Disconnected -> () *)
(*   | Connected ci -> *)
(*       let (_ : unit IO.t) = ci.disconnect () in *)
(*       () *)

let next_tag s =
  let tag = s.next_tag in
  let next_tag = tag + 1 in
  let tag = string_of_int tag in
  tag, {s with current_tag = Some tag; next_tag}
 
open Control

let greeting =
  liftP ImapParser.greeting >>= fun g ->
  ImapPrint.greeting_print Format.err_formatter g;
  modify (fun s -> greeting_store s g) >>
  match g with
    GREETING_RESP_COND_BYE r ->
      modify (fun s -> {s with imap_response = r.rsp_text}) >>
      fail Bye
  | GREETING_RESP_COND_AUTH r ->
      modify (fun s -> {s with imap_response = r.rsp_text.rsp_text}) >>
      ret r.rsp_type

let handle_response r =
  ImapPrint.response_print Format.err_formatter r;
  let imap_response =
    match r.rsp_resp_done with
      RESP_DONE_TAGGED {rsp_cond_state = {rsp_text = {rsp_text = s}}}
    | RESP_DONE_FATAL {rsp_text = s} -> s
  in
  modify (fun s -> {s with imap_response}) >>
  gets (fun s -> s.current_tag) >>= fun tag ->
  let bad_tag t = match tag with Some tag -> tag <> t | None -> true in
  match r.rsp_resp_done with
    RESP_DONE_TAGGED {rsp_tag} when bad_tag rsp_tag ->
      fail BadTag
  | RESP_DONE_TAGGED {rsp_cond_state = {rsp_type = RESP_COND_STATE_BAD}} ->
      fail Bad
  | RESP_DONE_TAGGED {rsp_cond_state = {rsp_type = RESP_COND_STATE_NO}} ->
      fail No
  | RESP_DONE_TAGGED {rsp_cond_state = {rsp_type = RESP_COND_STATE_OK}} ->
      ret ()
  | RESP_DONE_FATAL _ ->
      fail Bye

let std_command sender handler tag =
  send tag >>
  send " " >>
  sender >>
  send "\r\n" >>
  flush >>
  liftP ImapParser.response >>= fun r ->
  modify (fun s -> response_store s r) >>
  handle_response r >>
  gets handler

let capability =
  std_command (ImapWriter.raw "CAPABILITY") (fun s -> s.cap_info)

(* let capability session = *)
(*   let ci = connection_info session in *)
(*   let cmd = S.raw "CAPABILITY" in *)
(*   let tag = generate_tag ci in *)
(*   let f = S.(raw tag ++ space ++ cmd ++ crlf) in *)
(*   run_sender ci f >>= fun () -> *)
(*   parse_response ci >>= fun _ -> *)
(*   IO.return ci.state.cap_info *)
(* (\* let aux () = send_command ci cmd >|= fun () -> ci.state.cap_info in *\) *)
(* (\* IO.with_lock ci.send_lock aux *\) *)

let noop =
  std_command (ImapWriter.raw "NOOP") (fun _ -> ())

(* let noop s = *)
(*   let ci = connection_info s in *)
(*   let cmd = S.raw "NOOP" in *)
(*   let aux () = send_command ci cmd in *)
(*   IO.with_lock ci.send_lock aux *)

(* let logout s = *)
(*   let ci = connection_info s in *)
(*   let cmd = S.raw "LOGOUT" in *)
(*   let aux () = *)
(*     IO.catch *)
(*       (fun () -> send_command ci cmd) *)
(*       (function *)
(*         | BYE -> ci.disconnect () *)
(*         | exn -> IO.fail exn) *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

(* let id s params = *)
(*   let ci = connection_info s in *)
(*   let cmd = match params with *)
(*     | [] -> S.(raw "ID" ++ space ++ nil) *)
(*     | _ -> S.(raw "ID" ++ space ++ list (separated_pair string space string) params) *)
(*   in *)
(*   let aux () = *)
(*     send_command ci cmd >>= fun () -> *)
(*     IO.return ci.state.rsp_info.rsp_id *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

(* let enable s caps = *)
(*   let ci = connection_info s in *)
(*   let string_of_capability = *)
(*     function *)
(*       CAPABILITY_AUTH_TYPE s -> "AUTH=" ^ s *)
(*     | CAPABILITY_NAME s -> s *)
(*   in *)
(*   let cmd = *)
(*     S.(raw "ENABLE " ++ List.fold_right *)
(*          (fun cap rest -> raw (string_of_capability cap) ++ rest) caps null) *)
(*   in *)
(*   let aux () = *)
(*     send_command ci cmd >>= fun () -> *)
(*     IO.return ci.state.rsp_info.rsp_enabled *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

(* let starttls ?(version = `TLSv1) ?ca_file s = *)
(*   let ci = connection_info s in *)
(*   let cmd = S.raw "STARTTLS" in *)
(*   let aux () = *)
(*     if ci.compress_deflate then *)
(*       IO.fail (Failure "starttls: compression active") *)
(*     else *)
(*       send_command ci cmd >>= fun () -> *)
(*       IO.starttls version ?ca_file ci.chan >>= begin fun chan -> *)
(*         ci.chan <- chan; *)
(*         ci.state <- {ci.state with cap_info = []}; (\* See 6.2.1 in RFC 3501 *\) *)
(*         IO.return () *)
(*       end *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

(* let authenticate s auth = *)
(*   let ci = connection_info s in *)
(*   let cmd = S.(raw "AUTHENTICATE" ++ space ++ string auth.ImapAuth.name) in *)
(*   let step data = *)
(*     let data = ImapUtils.base64_decode data in *)
(*     begin *)
(*       try IO.return (auth.ImapAuth.step data) *)
(*       with e -> IO.fail (Auth_error e) *)
(*     end >>= fun (rc, data) -> *)
(*     let data = ImapUtils.base64_encode data in *)
(*     run_sender ci S.(raw data ++ crlf) >>= fun () -> *)
(*     IO.return rc *)
(*   in *)
(*   let aux () = *)
(*     send_command' ci cmd >>= get_auth_response step ci *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

let login user pass =
  std_command
    (ImapWriter.(raw "LOGIN" >> char ' ' >> string user >> char ' ' >> string pass))
    (fun _ -> ())
  
(* let login s user pass = *)
(*   let ci = connection_info s in *)
(*   let cmd = S.(raw "LOGIN" ++ space ++ string user ++ space ++ string pass) in *)
(*   let aux () = send_command ci cmd in *)
(*   IO.with_lock ci.send_lock aux *)

(* let compress s = *)
(*   let ci = connection_info s in *)
(*   let cmd = S.raw "COMPRESS DEFLATE" in *)
(*   let aux () = *)
(*     send_command ci cmd >>= fun () -> *)
(*     let chan = IO.compress ci.chan in *)
(*     ci.chan <- chan;       *)
(*     ci.compress_deflate <- true; *)
(*     IO.return () *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

(* let select_aux s cmd ?use_condstore:(use_condstore=false) mbox = *)
(*   let ci = connection_info s in *)
(*   let send_condstore = *)
(*     if use_condstore then S.(space ++ raw "(CONDSTORE)") else S.null *)
(*   in *)
(*   let cmd = S.(raw cmd ++ space ++ mailbox mbox ++ send_condstore) in *)
(*   let aux () = *)
(*     ci.state <- {ci.state with sel_info = fresh_selection_info}; *)
(*     send_command ci cmd >|= fun () -> ci.state.sel_info.sel_highestmodseq *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

(* let select_condstore s mbox = *)
(*   select_aux s "SELECT" ~use_condstore:true mbox *)

(* let select s mbox = *)
(*   select_aux s "SELECT" ~use_condstore:false mbox >>= fun _ -> *)
(*   IO.return () *)

(* let examine_condstore s mbox = *)
(*   select_aux s "EXAMINE" ~use_condstore:true mbox *)

(* let examine s mbox = *)
(*   select_aux s "EXAMINE" ~use_condstore:false mbox >>= fun _ -> *)
(*   IO.return () *)

let create mbox =
  std_command
    (ImapWriter.(raw "CREATE" >> char ' ' >> mailbox mbox))
    (fun _ -> ())

let delete mbox =
  std_command
    (ImapWriter.(raw "DELETE" >> char ' ' >> mailbox mbox))
    (fun _ -> ())

let rename oldbox newbox =
  std_command
    (ImapWriter.(raw "RENAME" >> char ' ' >> mailbox oldbox >> char ' ' >> mailbox newbox))
    (fun _ -> ())

let subscribe mbox =
  std_command
    (ImapWriter.(raw "SUBSCRIBE" >> char ' ' >> mailbox mbox))
    (fun _ -> ())

let unsubscribe mbox =
  std_command
    (ImapWriter.(raw "UNSUBCRIBE" >> char ' ' >> mailbox mbox))
    (fun _ -> ())

let list mbox list_mb =
  std_command
    (ImapWriter.(raw "LIST" >> char ' ' >> mailbox mbox >> char ' ' >> mailbox list_mb))
    (fun s -> s.rsp_info.rsp_mailbox_list)

let lsub mbox list_mb =
  std_command
    (ImapWriter.(raw "LSUB" >> char ' ' >> mailbox mbox >> char ' ' >> mailbox list_mb))
    (fun s -> s.rsp_info.rsp_mailbox_list)

let status mbox attrs =
  std_command
    (ImapWriter.(raw "STATUS" >> char ' ' >> mailbox mbox >> char ' ' >> list status_att attrs))
    (fun s -> s.rsp_info.rsp_status)

(* let append_uidplus s mbox ?flags ?date data = *)
(*   let ci = connection_info s in *)
(*   let flags = match flags with *)
(*     | None | Some [] -> S.null *)
(*     | Some flags -> S.(list flag flags ++ space) *)
(*   in *)
(*   let date = match date with *)
(*     | None -> S.null *)
(*     | Some dt -> S.(date_time dt ++ space) *)
(*   in *)
(*   let cmd = *)
(*     S.(raw "APPEND" ++ space ++ mailbox mbox ++ space ++ flags ++ date ++ literal data) *)
(*   in *)
(*   let aux () = *)
(*     send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_appenduid *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

(* let append s mbox ?flags ?date data = *)
(*   append_uidplus s mbox ?flags ?date data >>= fun _ -> *)
(*   IO.return () *)

(* let idle s f = *)
(*   let ci = connection_info s in *)
(*   let cmd = S.raw "IDLE" in *)
(*   let idling = ref false in *)
(*   let stop () = *)
(*     if !idling then begin *)
(*       idling := false; *)
(*       ignore (IO.catch *)
(*                 (fun () -> run_sender ci S.(raw "DONE" ++ crlf)) *)
(*                 (fun _ -> IO.return ())) *)
(*     end *)
(*   in *)
(*   let aux () = *)
(*     ci.state <- {ci.state with sel_info = {ci.state.sel_info with sel_exists = None; sel_recent = None}}; *)
(*     send_command' ci cmd >>= fun tag -> *)
(*     idling := true; *)
(*     get_idle_response ci tag f stop *)
(*   in *)
(*   IO.with_lock ci.send_lock aux, stop *)

(* let namespace s = *)
(*   assert false *)
(* (\* let ci = connection_info s in *\) *)
(* (\* let cmd = S.raw "NAMESPACE" in *\) *)
(* (\* let aux () = *\) *)
(* (\*   send_command ci cmd >>= fun () -> *\) *)
(* (\*   IO.return ci.state.rsp_info.rsp_namespace *\) *)
(* (\* in *\) *)
(* (\* IO.with_lock ci.send_lock aux *\) *)

let check =
  std_command (ImapWriter.(raw "CHECK")) (fun _ -> ())

let close =
  std_command (ImapWriter.(raw "CLOSE")) (fun _ -> ())

let expunge =
  std_command (ImapWriter.(raw "EXPUNGE")) (fun _ -> ())

(* let uid_expunge s set = *)
(*   assert (not (Uid_set.mem_zero set)); *)
(*   let ci = connection_info s in *)
(*   let cmd = S.(raw "UID EXPUNGE" ++ space ++ message_set (uid_set_to_uint32_set set)) in *)
(*   let aux () = send_command ci cmd in *)
(*   IO.with_lock ci.send_lock aux *)

(* let search_aux s cmd ?charset query = *)
(*   let ci = connection_info s in *)
(*   let charset_opt = match charset with *)
(*     | None -> S.null *)
(*     | Some charset -> S.(string charset ++ space) *)
(*   in *)
(*   let cmd = S.(raw cmd ++ space ++ charset_opt ++ search_key query) in *)
(*   let aux () = *)
(*     send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_search_results *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

(* let search s ?charset query = *)
(*   search_aux s "SEARCH" ?charset query >|= uint32_list_to_seq_list *)

(* let uid_search s ?charset query = *)
(*   search_aux s "UID SEARCH" ?charset query >|= uint32_list_to_uid_list *)

(* type msg_att_handler = *)
(*     Seq.t * msg_att list -> unit *)

(* let fetch_aux cmd s set changedsince attrs = *)
(*   let ci = connection_info s in *)
(*   let changedsince = *)
(*     match changedsince with *)
(*       None -> *)
(*         S.null *)
(*     | Some modseq -> *)
(*         S.(space ++ raw "(CHANGEDSINCE " ++ raw (Modseq.to_string modseq) ++ raw ")") *)
(*   in *)
(*   let cmd = *)
(*     S.(raw cmd ++ space ++ message_set set ++ space ++ list fetch_att attrs ++ changedsince) *)
(*   in *)
(*   let aux () = *)
(*     send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_fetch_list *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

(* let fetch_changedsince s set modseq atts = *)
(*   fetch_aux "FETCH" s (seq_set_to_uint32_set set) (Some modseq) atts *)

(* let fetch s set attrs = *)
(*   fetch_aux "FETCH" s (seq_set_to_uint32_set set) None attrs *)

(* let uid_fetch_changedsince s set modseq atts = *)
(*   fetch_aux "UID FETCH" s (uid_set_to_uint32_set set) (Some modseq) atts *)

(* let uid_fetch s set attrs = *)
(*   fetch_aux "UID FETCH" s (uid_set_to_uint32_set set) None attrs *)

(* let store_aux cmd s set unchangedsince mode att = *)
(*   assert false *)
(* (\* let ci = connection_info s in *\) *)
(* (\* let unchangedsince = match unchangedsince with *\) *)
(* (\*   | None -> S.null *\) *)
(* (\*   | Some modseq -> S.(raw "(UNCHANGEDSINCE " ++ raw (Modseq.to_string modseq) ++ raw ") ") *\) *)
(* (\* in *\) *)
(* (\* let mode = match mode with *\) *)
(* (\*   | `Add -> S.raw "+" *\) *)
(* (\*   | `Set -> S.null *\) *)
(* (\*   | `Remove -> S.raw "-" *\) *)
(* (\* in *\) *)
(* (\* let cmd = *\) *)
(* (\*   S.(raw cmd ++ space ++ message_set set ++ space ++ unchangedsince ++ mode ++ store_att att) *\) *)
(* (\* in *\) *)
(* (\* let aux () = send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_modified in *\) *)
(* (\* IO.with_lock ci.send_lock aux *\) *)

(* let store s set mode flags = *)
(*   store_aux "STORE" s (seq_set_to_uint32_set set) None mode flags >>= fun _ -> *)
(*   IO.return () *)

(* let uid_store s set mode flags = *)
(*   store_aux "UID STORE" s (uid_set_to_uint32_set set) None mode flags >>= fun _ -> *)
(*   IO.return () *)

(* let store_unchangedsince s set unchangedsince mode flags = *)
(*   store_aux "STORE" s (seq_set_to_uint32_set set) (Some unchangedsince) mode flags >|= *)
(*   uint32_set_to_seq_set *)

(* let uid_store_unchangedsince s set unchangedsince mode flags = *)
(*   store_aux "UID STORE" s (uid_set_to_uint32_set set) (Some unchangedsince) mode flags >|= *)
(*   uint32_set_to_uid_set *)

(* let copy_aux cmd s set destbox = *)
(*   let ci = connection_info s in *)
(*   let cmd = S.(raw cmd ++ space ++ message_set set ++ space ++ mailbox destbox) in *)
(*   let aux () = send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_copyuid in *)
(*   IO.with_lock ci.send_lock aux *)

(* let copy s set destbox = *)
(*   copy_aux "COPY" s (seq_set_to_uint32_set set) destbox >>= fun _ -> *)
(*   IO.return () *)

(* let uidplus_copy s (set : Seq_set.t) destbox = *)
(*   copy_aux "COPY" s (seq_set_to_uint32_set set) destbox *)

(* let uid_copy s set destbox = *)
(*   copy_aux "UID COPY" s (uid_set_to_uint32_set set) destbox >>= fun _ -> *)
(*   IO.return () *)

(* let uidplus_uid_copy s set destbox = *)
(*   copy_aux "UID COPY" s (uid_set_to_uint32_set set) destbox *)

(* let state s = *)
(*   let ci = connection_info s in *)
(*   ci.state *)

(* let is_busy s = *)
(*   let ci = connection_info s in *)
(*   IO.is_locked ci.send_lock *)
