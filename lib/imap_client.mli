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

(** Higher-level IMAP interface *)

open Sexplib.Std
open Sexplib.Conv
open Imap_uint

module Identity : sig
  type t = {
    vendor : string;
    name : string;
    version : string;
    info : (string * string) list
  }
end

module Capability : sig
  type t =
    | ACL
    | Binary
    | Catenate
    | Children
    | Compress_deflate
    | Condstore
    | Enable
    | Idle
    | Id
    | Literal_plus
    | Multi_append
    | Namespace
    | Qresync
    | Quota
    | Sort
    | Start_tls
    | Thread_ordered_subject
    | Thread_references
    | Uid_plus
    | Unselect
    | Xlist
    | Auth_anonymous
    | Auth_cram_md5
    | Auth_digest_md5
    | Auth_external
    | Auth_gssapi
    | Auth_kerberos_v4
    | Auth_login
    | Auth_ntlm
    | Auth_otp
    | Auth_plain
    | Auth_skey
    | Auth_srp
    | Auth_xoauth2
    | Gmail
end

module Error : sig
  type t =
    | Connection
    | TLS_not_available
    | Parse
    | Certificate
    | Authentication
    | Gmail_IMAP_not_enabled
    | Gmail_exceeded_bandwidth_limit
    | Gmail_too_many_simulatenous_connections
    | MobileMe_moved
    | Yahoo_unavailable
    | Non_existent_folder
    | Rename
    | Delete
    | Create
    | Subscribe
    | Append
    | Copy
    | Expunge
    | Fetch
    | Idle
    | Identity
    | Namespace
    | Store
    | Capability
    | Start_tls_not_available
    | Send_message_illegal_attachment
    | Storage_limit
    | Send_message_not_allowed
    | Needs_connect_to_webmail
    | Send_message
    | Authentication_required
    | Fetch_message_list
    | Delete_message
    | Invalid_account
    | File
    | Compression
    | No_sender
    | No_recipient
    | Noop
end

exception Error of Error.t * string

module Part : sig
  type encoding =
    | Seven_bits
    | Eight_bits
    | Binary
    | Base64
    | Quoted_printable
    | Other
  
  type t = unit
end

module Message : sig
  type flag =
    | Seen
    | Answered
    | Flagged
    | Deleted
    | Draft
    | MDNSent
    | Forwarded
    | SubmitPending
    | Submitted

  type request =
    | Uid
    | Flags
    | Headers
    | Structure
    | Internal_date
    | Full_headers
    | Header_subject
    | Gmail_labels
    | Gmail_message_id
    | Gmail_thread_id
    | Extra_headers
    | Size

  type request_type =
    | By_uids
    | By_sequence

  type store_request_kind =
    | Add
    | Set
    | Remove
  
  type t = {
    uid : uint32;
    size : int;
    flags : flag list;
    modseq : uint64;
    gmail_labels : string list;
    gmail_message_id : uint64;
    gmail_thread_id : uint64
  }
end

module Folder : sig
  type status = {
    unseen_count : int;
    message_count : int;
    recent_count : int;
    uid_next : uint32;
    uid_validity : uint32;
    highestmodseq : uint64
  }

  type flag =
    | Marked
    | Unmarked
    | NoSelect
    | NoInferiors
    | Inbox
    | SentMail
    | Starred (* = Flagged *)
    | AllMail (* = All *)
    | Trash
    | Drafts
    | Spam (* = Junk *)
    | Important
    | Archive

  type t = {
    path : string;
    delimiter : char;
    flags : flag list
  }
end

type connection_type =
  | Clear
  | Start_tls
  | Type_tls

type auth_type =
  | Sasl_none
  | Sasl_cram_md5
  | Sasl_plain
  | Sasl_gssapi
  | Sasl_digest_md5
  | Sasl_login
  | Xoauth2
  | Xoauth2_outlook

type t

val create : unit -> t

val connect : t -> unit Lwt.t

val disconnect : t -> unit

val connect_if_needed : t -> unit Lwt.t

val login : t -> unit Lwt.t

val login_if_needed : t -> unit Lwt.t

val folder_status : t -> string -> Folder.status Lwt.t

val fetch_all_folders : t -> Folder.t list Lwt.t

val select : t -> string -> unit Lwt.t

val select_if_needed : t -> string -> unit Lwt.t

val rename_folder : t -> string -> string -> unit Lwt.t
    
val delete_folder : t -> string -> unit Lwt.t

val expunge : t -> string -> unit Lwt.t
    
