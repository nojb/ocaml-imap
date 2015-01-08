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

module QResync : sig
  type qresync_vanished =
    { qr_earlier : bool;
      qr_known_uids : ImapSet.t }
  val fetch_qresync_vanished :
    ImapSet.t -> fetch_type -> Uint64.t -> bool -> (msg_att list * qresync_vanished) command
  val uid_fetch_qresync_vanished :
    ImapSet.t -> fetch_type -> Uint64.t -> bool -> (msg_att list * qresync_vanished) command
  val fetch_qresync :
    ImapSet.t -> fetch_type -> Uint64.t -> (msg_att list * qresync_vanished) command
  val uid_fetch_qresync :
    ImapSet.t -> fetch_type -> Uint64.t -> (msg_att list * qresync_vanished) command
end

module Condstore : sig
  type msg_att_extension +=
       MSG_ATT_MODSEQ of Uint64.t

  type resp_text_code_extension +=
       RESP_TEXT_CODE_HIGHESTMODSEQ of Uint64.t
     | RESP_TEXT_CODE_NOMODSEQ
     | RESP_TEXT_CODE_MODIFIED of ImapSet.t

  type status_info_extension +=
       STATUS_ATT_HIGHESTMODSEQ of Uint64.t
  
  type mailbox_data_extension +=
       MAILBOX_DATA_SEARCH of Uint32.t list * Uint64.t

  val fetch_att_modseq : fetch_att
    
  val search_modseq : ?charset:string -> search_key -> (Uint32.t list * Uint64.t) command
      
  val uid_search_modseq : ?charset:string -> search_key -> (Uint32.t list * Uint64.t) command
      
  val search : ?charset:string -> search_key -> Uint32.t list command
      
  val uid_search : ?charset:string -> search_key -> Uint32.t list command
      
  val select : string -> unit command
      
  val select_condstore : string -> Uint64.t command
      
  val examine : string -> unit command
      
  val examine_condstore : string -> Uint64.t command
      
  val fetch_changedsince : ImapSet.t -> Uint64.t -> fetch_type -> msg_att list command
      
  val uid_fetch_changedsince : ImapSet.t -> Uint64.t -> fetch_type -> msg_att list command
      
  val store_unchangedsince : ImapSet.t -> Uint64.t -> store_att_flags -> ImapSet.t command
      
  val uid_store_unchangedsince : ImapSet.t -> Uint64.t -> store_att_flags -> ImapSet.t command
end

val capability : capability list command

val noop : unit command

val logout : unit command

val starttls : unit command

val authenticate : ImapAuth.t -> unit command

val login : string -> string -> unit command

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

module Uidplus : sig
  val uid_expunge : ImapSet.t -> unit command
  val uidplus_copy : ImapSet.t -> string -> (Uint32.t * ImapSet.t * ImapSet.t) command
  val uidplus_uid_copy : ImapSet.t -> string -> (Uint32.t * ImapSet.t * ImapSet.t) command
  val uidplus_append : string -> ?flags:flag list -> ?date_time:float -> string -> (Uint32.t * Uint32.t) command
end

module XGmExt1 : sig
  type msg_att_extension +=
       MSG_ATT_XGMMSGID of Uint64.t
     | MSG_ATT_XGMTHRID of Uint64.t
     | MSG_ATT_XGMLABELS of string list

  val fetch_att_xgmmsgid : fetch_att

  val fetch_att_xgmthrid : fetch_att

  val fetch_att_xgmlabels : fetch_att

  val uid_store_xgmlabels : ImapSet.t -> store_att_flags_sign -> bool -> string list -> unit command

  val store_xgmlabels : ImapSet.t -> store_att_flags_sign -> bool -> string list -> unit command
end

module Namespace : sig
  type namespace_extension =
    string * string list
  type namespace_info =
    { ns_prefix : string;
      ns_delimiter : char option;
      ns_extensions : namespace_extension list }
  type namespace_item =
    namespace_info list
  type namespace_data =
    { ns_personal : namespace_item;
      ns_other : namespace_item;
      ns_shared : namespace_item }
  val namespace : namespace_data command
end

module Compress : sig
  val compress : unit command
end

module Idle : sig
  val idle_done : unit command
  val idle_start : unit command
end
