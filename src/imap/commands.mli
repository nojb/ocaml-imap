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

(** IMAP protocol interface *)

open Types
open Core
open Control
  
(* (\** {2 Commands valid in any state} *\) *)

val capability : capability list command

val noop : unit command

val logout : unit command
(** Logs out from the server by sending a {b LOGOUT} command. *)

(* (\** {2 Commands valid in {b Non-authenticated} state} *\) *)

(* val starttls : ?version : [ `TLSv1 | `SSLv23 | `SSLv3 ] -> ?ca_file : string -> *)
(*   session -> unit IO.t *)
(* (\** Start a TLS session using the given protocol.  If [?ca_file] is given, *)
(*     then it must point to a certificate in PEM format and will be used to *)
(*     validate the server identity. *\) *)

val authenticate : Auth.t -> unit command

val login : string -> string -> unit command

(* (\** {2 Commands valid in {b Authenticated} or {b Selected} state} *\) *)

(* val compress : session -> unit IO.t *)
(* (\** Enables compression. *)

(*     This command requires the COMPRESS=DEFLATE extension. *\) *)

val examine : string -> unit command

val select : string -> unit command

val create : string -> unit command

val delete : string -> unit command

val rename : string -> string -> unit command

val subscribe : string -> unit command

val unsubscribe : string -> unit command

val list : string -> string -> mailbox_list list command

val lsub : string -> string -> mailbox_list list command

val status : string -> status_att list -> mailbox_data_status command

val append : string -> ?flags:flag list -> ?date_time:float -> string -> unit command

(* (\* val namespace : session -> (namespace list * namespace list * namespace list) IO.t *\) *)
(* (\** Returns the three types of namespaces (personal, other users, shared) in the server. *)

(*     This command requires the NAMESPACE extension. *\) *)

(* (\** {2 Commands valid only in {b Selected} state} *\) *)

val check : unit command

val close : unit command

val expunge : unit command

val fetch : ImapSet.t -> fetch_att list -> msg_att list command

val uid_fetch : ImapSet.t -> fetch_att list -> msg_att list command

val copy : ImapSet.t -> string -> unit command
    
val uid_copy : ImapSet.t -> string -> unit command
