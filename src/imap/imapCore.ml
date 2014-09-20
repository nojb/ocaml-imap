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

open ImapTypes
open ImapTypesPrivate
open ImapControl

let string_of_error = function
  | Bad -> "server did not understand request"
  | BadTag -> "incorrect tag in reply"
  | No -> "server denied request"
  | Bye -> "server terminated the connection"
  | ParseError (context, i) -> Printf.sprintf "parser error near char %i of (...) %S (...)" i context
  | Auth_error -> "authentication error"

type 'a command = ('a, state, error) control

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
  rsp_other = ("", None)
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
  (* | RESP_TEXT_CODE_COMPRESSIONACTIVE -> *)
      (* {s with rsp_info = {s.rsp_info with rsp_compressionactive = true}} *)
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
  let s = {s with rsp_info = fresh_response_info} in
  (* FIXME : check if this does not conflict with the implementation of IDLE -
     it seems that the response_data_store there gets discarded ? *)
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
  next_tag = 0;
  out_buf = [];
  in_buf = Buffer.create 0;
  in_pos = 0
}

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
      match r.rsp_type with
        RESP_COND_AUTH_OK -> ret `NeedsAuth
      | RESP_COND_AUTH_PREAUTH -> ret `PreAuth

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

let send_tag =
  gets (fun s -> s.next_tag) >>= fun tag ->
  let ctag = string_of_int tag in
  modify (fun s -> {s with current_tag = Some ctag; next_tag = tag+1}) >>
  send ctag

let std_command sender =
  send_tag >>
  send " " >>
  sender >>
  send "\r\n" >>
  flush >>
  liftP ImapParser.response >>= fun r ->
  modify (fun s -> response_store s r) >>
  handle_response r
