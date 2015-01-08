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
open ImapControl

type state =
  { mutable rsp_alert : string;
    mutable rsp_parse : string;
    mutable rsp_badcharset : string list;
    mutable rsp_trycreate : bool;
    mutable rsp_other : string * string option;
    mutable sel_perm_flags : flag_perm list;
    mutable sel_perm : [ `Read_write | `Read_only ];
    mutable sel_uidnext : uint32;
    mutable sel_uidvalidity : uint32;
    mutable sel_first_unseen : uint32;
    mutable sel_flags : flag list;
    mutable sel_exists : int option;
    mutable sel_recent : int option;
    mutable sel_unseen : int;
    mutable caps : string list;
    mutable imap_response : string;
    mutable current_tag : string option;
    mutable next_tag : int }

type error =
  [ `Bad
  | `Bad_tag
  | `No
  | `Bye
  | `Parse_error of string * int
  | `Auth_error
  | `Extension_error ]

let string_of_error = function
  | `Bad -> "server did not understand request"
  | `Bad_tag -> "incorrect tag in reply"
  | `No -> "server denied request"
  | `Bye -> "server terminated the connection"
  | `Parse_error (context, i) -> Printf.sprintf "parser error near char %i of (...) %S (...)" i context
  | `Auth_error -> "authentication error"
  | `Extension_error -> "extension error"

type 'a command = ('a, state, error) control

(* let extension_data_store st k d = *)
(*   {st with rsp_info = *)
(*              {st.rsp_info with rsp_extension_list = st.rsp_info.rsp_extension_list @ [EXTENSION_DATA (k, d)]}} *)

let store_code s c t =
  match c with
  | `Alert              -> s.rsp_alert <- t
  | `Bad_charset cs     -> s.rsp_badcharset <- cs
  | `Capability cs      -> s.caps <- cs (* CHECK *)
  | `Parse              -> s.rsp_parse <- t
  | `Permanent_flags fl -> s.sel_perm_flags <- fl
  | `Read_only          -> s.sel_perm <- `Read_only
  | `Read_write         -> s.sel_perm <- `Read_write
  | `Try_create         -> s.try_create <- true
  | `Uid_next n         -> s.sel_uidnext <- n
  | `Uid_validity n     -> s.sel_uidvalidity <- n
  | `Unseen n           -> s.sel_first_unseen <- n
  | `Other (a, s)       -> s.rsp_other <- (a, s)
  | _                   -> ()

let store_untagged s = function
  | `Exists n -> s.sel_exists <- n
  | `Recent n -> s.sel_recent <- n
  | _         -> ()

let debug =
  try let s = Sys.getenv "IMAP_DEBUG" in ref (s <> "0")
  with Not_found -> ref false

let create_state () =
  { rsp_alert = "";
    rsp_parse = "";
    rsp_badcharset = [];
    rsp_trycreate = false;
    rsp_other = ("", None);
    sel_perm_flags = [];
    sel_perm = MAILBOX_READONLY;
    sel_uidnext = Uint32.zero;
    sel_uidvalidity = Uint32.zero;
    sel_first_unseen = Uint32.zero;
    sel_flags = [];
    sel_exists = None;
    sel_recent = None;
    sel_unseen = 0;
    cap_info = [];
    r = "";
    current_tag = None;
    next_tag = 0;
    d = Imap_parser.decoder () }

let greeting k s =
  liftP ImapParser.greeting >>= fun g ->
  (* ImapPrint.greeting_print Format.err_formatter g; *)
  modify (fun s -> greeting_store s g) >>
  match g with
  | GREETING_RESP_COND_BYE r ->
      modify (fun s -> {s with imap_response = r.rsp_text}) >>
      fail Bye
  | GREETING_RESP_COND_AUTH r ->
      modify (fun s -> {s with imap_response = r.rsp_text.rsp_text}) >>
      match r.rsp_type with
      | RESP_COND_AUTH_OK -> ret `NeedsAuth
      | RESP_COND_AUTH_PREAUTH -> ret `PreAuth

let handle_response r =
  (* ImapPrint.response_print Format.err_formatter r; *)
  let imap_response = match r.rsp_resp_done with
    | RESP_DONE_TAGGED {rsp_cond_state = {rsp_text = {rsp_text = s}}}
    | RESP_DONE_FATAL {rsp_text = s} -> s
  in
  modify (fun s -> {s with imap_response}) >>
  gets (fun s -> s.current_tag) >>= fun tag ->
  let bad_tag t = match tag with Some tag -> tag <> t | None -> true in
  match r.rsp_resp_done with
  | `Tagged (_, `Bad _) ->
  | `Tagged (_, `No _) ->
  | `Bye _ ->

  | RESP_DONE_TAGGED {rsp_tag} when bad_tag rsp_tag ->
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
