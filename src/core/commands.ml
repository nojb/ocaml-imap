(* The MIT License (MIT) *)

(* Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com> *)

(* Permission is hereby granted, free of charge, to any person obtaining a copy *)
(* of this software and associated documentation files (the "Software"), to deal *)
(* in the Software without restriction, including without limitation the rights *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell *)
(* copies of the Software, and to permit persons to whom the Software is *)
(* furnished to do so, subject to the following conditions: *)

(* The above copyright notice and this permission notice shall be included in *)
(* all copies or substantial portions of the Software. *)

(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE *)
(* SOFTWARE. *)

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
  | RESP_TEXT_CODE_EXTENSION e ->
      Extension.extension_data_store s e
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
                                   s.rsp_info.rsp_mailbox_lsub @ [mb]}}
  | MAILBOX_DATA_SEARCH results ->
      {s with rsp_info = {s.rsp_info with
                          rsp_search_results = s.rsp_info.rsp_search_results @ results}}
  | MAILBOX_DATA_STATUS status ->
      {s with rsp_info = {s.rsp_info with rsp_status = status}}
  | MAILBOX_DATA_EXISTS n ->
      {s with sel_info = {s.sel_info with sel_exists = Some n}}
  | MAILBOX_DATA_RECENT n ->
      {s with sel_info = {s.sel_info with sel_recent = Some n}}
  | MAILBOX_DATA_EXTENSION_DATA e ->
      Extension.extension_data_store s e

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
      Extension.extension_data_store s e
  (* | `NAMESPACE (pers, other, shared) -> *)
  (*   {s with rsp_info = {s.rsp_info with rsp_namespace = pers, other, shared}} *)

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

let fresh_state = {
  rsp_info = fresh_response_info;
  sel_info = fresh_selection_info;
  cap_info = [];
  imap_response = "";
  current_tag = None;
  next_tag = 0
}

let next_tag s =
  let tag = s.next_tag in
  let next_tag = tag + 1 in
  let tag = string_of_int tag in
  tag, {s with current_tag = Some tag; next_tag}
 
open Control

let greeting =
  liftP Parser.greeting >>= fun g ->
  Print.greeting_print Format.err_formatter g;
  modify (fun s -> greeting_store s g) >>
  match g with
    GREETING_RESP_COND_BYE r ->
      modify (fun s -> {s with imap_response = r.rsp_text}) >>
      fail Bye
  | GREETING_RESP_COND_AUTH r ->
      modify (fun s -> {s with imap_response = r.rsp_text.rsp_text}) >>
      ret r.rsp_type

let handle_response r =
  Print.response_print Format.err_formatter r;
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

let std_command sender handler =
  let cmd =
    send " " >>
    sender >>
    send "\r\n" >>
    flush >>
    liftP Parser.response >>= fun r ->
    modify (fun s -> response_store s r) >>
    handle_response r >>
    gets handler
  in
  fun tag -> send tag >> cmd

let capability =
  std_command (Sender.raw "CAPABILITY") (fun s -> s.cap_info)

let noop =
  std_command (Sender.raw "NOOP") (fun _ -> ())

let logout =
  fun tag ->
    catch (std_command (Sender.raw "LOGOUT") (fun _ -> ()) tag) (function Bye -> ret () | _ as e -> fail e)

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

let authenticate auth tag =
  let step data =
    let step data = try ret (auth.Auth.step data) with _ -> fail Auth_error in
    step (Utils.base64_decode data) >>= fun (rc, data) ->
    let data = Utils.base64_encode data in
    Sender.(raw data >> crlf) >>
    flush >>
    ret rc
  in
  let auth_sender tag =
    let open Sender in
    raw tag >> char ' ' >> raw "AUTHENTICATE" >> char ' ' >> string auth.Auth.name >> crlf
  in
  auth_sender tag >>
  flush >>
  let rec loop needs_more =
    liftP
      Parser.(alt
                (continue_req >>= fun data -> ret (`More data))
                (response >>= fun r -> ret (`Done r))) >>= function
      `More data ->
        let data = match data with CONTINUE_REQ_BASE64 data -> data | CONTINUE_REQ_TEXT _ -> "" in
        step data >>= fun needs_more ->
        loop (match needs_more with `OK -> false | `NEEDS_MORE -> true)
    | `Done r ->
        if needs_more then
          step "" >>= function
            `OK -> ret r
          | `NEEDS_MORE -> fail Auth_error
        else
          ret r
  in
  loop true >>= fun r ->
  modify (fun s -> response_store s r) >>
  handle_response r

let login user pass =
  std_command
    (Sender.(raw "LOGIN" >> char ' ' >> string user >> char ' ' >> string pass))
    (fun _ -> ())
  
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

let create mbox =
  std_command
    (Sender.(raw "CREATE" >> char ' ' >> mailbox mbox))
    (fun _ -> ())

let delete mbox =
  std_command
    (Sender.(raw "DELETE" >> char ' ' >> mailbox mbox))
    (fun _ -> ())

let rename oldbox newbox =
  std_command
    (Sender.(raw "RENAME" >> char ' ' >> mailbox oldbox >> char ' ' >> mailbox newbox))
    (fun _ -> ())

let subscribe mbox =
  std_command
    (Sender.(raw "SUBSCRIBE" >> char ' ' >> mailbox mbox))
    (fun _ -> ())

let unsubscribe mbox =
  std_command
    (Sender.(raw "UNSUBCRIBE" >> char ' ' >> mailbox mbox))
    (fun _ -> ())

let list mbox list_mb =
  std_command
    (Sender.(raw "LIST" >> char ' ' >> mailbox mbox >> char ' ' >> mailbox list_mb))
    (fun s -> s.rsp_info.rsp_mailbox_list)

let lsub mbox list_mb =
  std_command
    (Sender.(raw "LSUB" >> char ' ' >> mailbox mbox >> char ' ' >> mailbox list_mb))
    (fun s -> s.rsp_info.rsp_mailbox_list)

let status mbox attrs =
  std_command
    (Sender.(raw "STATUS" >> char ' ' >> mailbox mbox >> char ' ' >> list status_att attrs))
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
  std_command (Sender.(raw "CHECK")) (fun _ -> ())

let close =
  std_command (Sender.(raw "CLOSE")) (fun _ -> ())

let expunge =
  std_command (Sender.(raw "EXPUNGE")) (fun _ -> ())
