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

open ImapTypes
open ImapCore
open ImapControl
  
(* (\** {2 Commands valid in any state} *\) *)

module Condstore : sig
  type condstore_resptextcode =
      CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ of Uint64.t
    | CONDSTORE_RESPTEXTCODE_NOMODSEQ
    | CONDSTORE_RESPTEXTCODE_MODIFIED of ImapSet.t

  type extension_data +=
       CONDSTORE_FETCH_DATA_MODSEQ of Uint64.t
     | CONDSTORE_RESP_TEXT_CODE of condstore_resptextcode
     | CONDSTORE_SEARCH_DATA of Uint32.t list * Uint64.t
     | CONDSTORE_STATUS_INFO_HIGHESTMODSEQ of Uint64.t

  val fetch_att_modseq : fetch_att
    
  val search_modseq : ?charset:string -> search_key -> (Uint32.t list * Uint64.t) command
      
  val uid_search_modseq : ?charset:string -> search_key -> (Uint32.t list * Uint64.t) command
      
  val search : ?charset:string -> search_key -> Uint32.t list command
      
  val uid_search : ?charset:string -> search_key -> Uint32.t list command
      
  val select : string -> unit command
      
  val select_condstore : string -> Uint64.t command
      
  val examine : string -> unit command
      
  val examine_condstore : string -> Uint64.t command
      
  val fetch : ImapSet.t -> fetch_type -> msg_att list command
      
  val uid_fetch : ImapSet.t -> fetch_type -> msg_att list command
      
  val fetch_changedsince : ImapSet.t -> Uint64.t -> fetch_type -> msg_att list command
      
  val uid_fetch_changedsince : ImapSet.t -> Uint64.t -> fetch_type -> msg_att list command
      
  val store : ImapSet.t -> store_att_flags -> unit command
      
  val uid_store : ImapSet.t -> store_att_flags -> unit command
      
  val store_unchangedsince : ImapSet.t -> Uint64.t -> store_att_flags -> ImapSet.t command
      
  val uid_store_unchangedsince : ImapSet.t -> Uint64.t -> store_att_flags -> ImapSet.t command
end

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

val authenticate : ImapAuth.t -> unit command

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

val search : ?charset:string -> search_key -> Uint32.t list command

val uid_search : ?charset:string -> search_key -> Uint32.t list command

(* (\* val namespace : session -> (namespace list * namespace list * namespace list) IO.t *\) *)
(* (\** Returns the three types of namespaces (personal, other users, shared) in the server. *)

(*     This command requires the NAMESPACE extension. *\) *)

(* (\** {2 Commands valid only in {b Selected} state} *\) *)

val check : unit command

val close : unit command

val expunge : unit command

val fetch : ImapSet.t -> fetch_type -> msg_att list command

val uid_fetch : ImapSet.t -> fetch_type -> msg_att list command

val store : ImapSet.t -> store_att_flags -> unit command

val uid_store : ImapSet.t -> store_att_flags -> unit command

val copy : ImapSet.t -> string -> unit command
    
val uid_copy : ImapSet.t -> string -> unit command

module Enable : sig
  val enable : capability list -> capability list command
end

module Id : sig
  val id : (string * string option) list -> (string * string option) list command
  val id_basic : string -> string -> (string option * string option) command
end
