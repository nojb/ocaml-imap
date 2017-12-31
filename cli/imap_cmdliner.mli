(* The MIT License (MIT)
  
   Copyright (c) 2015-2017 Nicolas Ojeda Bar <n.oje.bar@gmail.com>
   Copyright (c) 2017 Anil Madhavapeddy <anil@recoil.org>

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

(** Cmdliner terms to quickly assemble IMAP command-line interfaces *)

(** {2 IMAP client command line terms} *)

type args = {
  server: string;
  port: int option;
  username: string;
  password: string;
  tls: bool;
  mailbox: string;
}
(** Arguments to make an IMAP client connection. *)

val client : args Cmdliner.Term.t
(** [client] is a {!Cmdliner} term that exposes CLI flags
    for establishing an outgoing IMAP client connection. *)

val connect : ?read_only:bool -> args -> Imap.t Lwt.t
(** [connect] will invoke {!Imap.connect} with the arguments
    passed on the command-line. *)

(** {2 Individual Terms}
   These can be used individually if the above {!client} term
   needs to be customised. *)

val server : string Cmdliner.Term.t
val port : int option Cmdliner.Term.t
val username : string Cmdliner.Term.t
val password : string Cmdliner.Term.t
val no_tls : bool Cmdliner.Term.t
val mailbox : string Cmdliner.Term.t

