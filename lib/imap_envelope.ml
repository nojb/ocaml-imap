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

open Sexplib.Std

type address = {
  addr_name : string;
  addr_adl : string;
  addr_mailbox : string;
  addr_host : string
} with sexp

type envelope = {
  env_date : string;
  env_subject : string;
  env_from : address list;
  env_sender : address list;
  env_reply_to : address list;
  env_to : address list;
  env_cc : address list;
  env_bcc : address list;
  env_in_reply_to : string;
  env_message_id : string
} with sexp

type t = envelope with sexp

open Imap_parser

let address =
  delimited lpar
    (nstring' >>= fun addr_name -> space >>
     nstring' >>= fun addr_adl -> space >>
     nstring' >>= fun addr_mailbox -> space >>
     nstring' >|= fun addr_host ->
     { addr_name; addr_adl; addr_mailbox; addr_host })
    rpar

let address_list =
  delimited lpar (separated_nonempty_list space address) rpar <|> (nil >| [])

let envelope =
  delimited lpar
    (nstring' >>= fun env_date -> space >>
     nstring' >>= fun env_subject -> space >>
     address_list >>= fun env_from -> space >>
     address_list >>= fun env_sender -> space >>
     address_list >>= fun env_reply_to -> space >>
     address_list >>= fun env_to -> space >>
     address_list >>= fun env_cc -> space >>
     address_list >>= fun env_bcc -> space >>
     nstring' >>= fun env_in_reply_to -> space >>
     nstring' >|= fun env_message_id ->
     { env_date; env_subject; env_from; env_sender;
       env_reply_to; env_to; env_cc; env_bcc; env_in_reply_to;
       env_message_id })
    rpar
