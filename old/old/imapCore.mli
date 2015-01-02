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

open ImapControl

type uint32 = Uint32.t

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
  | `BadTag
  | `No
  | `Bye
  | `ParseError of string * int
  | `Auth_error
  | `ExtensionError ]

val string_of_error : error -> string

type 'a command = ('a, state, error) control

val response_data_store : state -> response_data -> state

val greeting_store : state -> greeting -> state

val cont_req_or_resp_data_store : state -> cont_req_or_resp_data -> state

val response_store : state -> response -> state

val handle_response : response -> unit command

(* val debug : bool ref *)

val send_tag : unit command

(** {2 IMAP sessions} *)

val fresh_selection_info : selection_info

val fresh_state : state

val greeting : [ `NeedsAuth | `PreAuth ] command

val std_command : unit command -> unit command
