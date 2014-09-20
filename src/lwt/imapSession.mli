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

type session

type folder_flag =
    Marked
  | Unmarked
  | NoSelect
  | NoInferiors
  | Inbox
  | SentMail
  | Starred
  | AllMail
  | Trash
  | Drafts
  | Spam
  | Important
  | Archive

type message_flag =
    Seen
  | Answered
  | Flagged
  | Deleted
  | Draft
  | MDNSent
  | Forwarded
  | SubmitPending
  | Submitted

type messages_request_kind =
    Uid
  | Flags
  | Headers
  | Structure
  | InternalDate
  | FullHeaders
  | HeaderSubject
  | GmailLabels
  | GmailMessageID
  | GmailThreadID
  | ExtraHeaders of string list
  | Size

type fetch_request_type =
    UID
  | Sequence

type flags_request_kind =
    Add
  | Remove
  | Set

type workaround =
    Gmail
  | Yahoo
  | Exchange2003

type auth_capability =
    Anonymous
  | CRAMMD5
  | DigestMD5
  | External
  | GSSAPI
  | KerberosV4
  | Login
  | NTLM
  | OTP
  | Plain
  | SKey
  | SRP

type capability =
    ACL
  | Binary
  | Catenate
  | Children
  | CompressDeflate
  | Condstore
  | Enable
  | Idle
  | Id
  | LiteralPlus
  | MultiAppend
  | Namespace
  | QResync
  | Quote
  | Sort
  | StartTLS
  | ThreadORderedSubject
  | ThreadReferences
  | UIDPlus
  | Unselect
  | XList
  | Auth of auth_capability
  | XOAuth2
  | Gmail

type encoding =
    Bit7
  | Bit8
  | Binary
  | Base64
  | QuotedPrintable
  | Other
  | UUEncode

type error =
    Connection
  | TLSNotAvailable
  | Parse
  | Certificate
  | Authentication
  | GmailIMAPNotEnabled
  | GmailExceededBandwidthLimit
  | GmailTooManySimultaneousConnections
  | MobileMeMoved
  | YahooUnavailable
  | NonExistantFolder
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
  | StartTLSNotAvailable
  | SendMessageIllegalAttachment
  | StorageLimit
  | SendMessageNotAllowed
  | NeedsConnectToWebmail
  | SendMessage
  | AuthenticationRequired
  | FetchMessageList
  | DeleteMessage
  | InvalidAccount
  | File
  | Compression
  | NoSender
  | NoRecipient
  | Noop

type folder_status = {
  unseen_count : int;
  message_count : int;
  recent_count : int;
  uid_next : Uint32.t;
  uid_validity : Uint32.t;
  highest_mod_seq_value : Uint64.t
}

type folder =
  { path : string;
    delimiter : char option;
    flags : folder_flag list }

type address = {
  display_name : string;
  mailbox : string;
}

type header = {
  message_id : string;
  references : string list;
  in_reply_to : string list;
  sender : address;
  from : address;
  to_ : address list;
  cc : address list;
  bcc : address list;
  reply_to : address list;
  subject : string
}

type message = {
  uid : Uint32.t;
  size : int;
  mod_seq_value : Uint64.t;
  gmail_labels : string list;
  gmail_message_id : Uint64.t;
  gmail_thread_id : Uint64.t;
  flags : message_flag list;
  internal_date : float
}

exception Error of error

val disconnect :
  session -> unit Lwt.t
    (** Disconnects from the server. *)
      
val folder_status :
  session ->
  folder:string -> folder_status Lwt.t
    (** Returns folder status info (like UIDNext, Unseen, ...) *)

val noop :
  session -> unit Lwt.t
    (** Performs a No-Op operation on the IMAP server. *)

val fetch_all_folders :
  session -> folder list Lwt.t
    (** Gets all folders. *)

val rename_folder :
  session ->
  folder:string ->
  new_name:string -> unit Lwt.t
    (** Renames a folder. *)
  
val delete_folder :
  session ->
  folder:string -> unit Lwt.t
    (** Deletes a folder. *)
  
val create_folder :
  session ->
  folder:string -> unit Lwt.t
    (** Creates a new folder. *)

val expunge :
  session ->
  folder:string -> unit Lwt.t
    (** Expunges (deletes trashed messages) a folder. *)
  
val fetch_message_by_uid :
  session ->
  folder:string ->
  uid:Uint32.t -> string Lwt.t
    (** Fetch the raw contents of a message given its UID. *)
  
val fetch_number_uid_mapping :
  session ->
  folder:string ->
  from_uid:Uint32.t ->
  to_uid:Uint32.t -> (Uint32.t, Uint32.t) Hashtbl.t Lwt.t

val add_labels :
  session ->
  folder:string ->
  uids:ImapSet.t ->
  labels:string list -> unit Lwt.t
    (** Add Gmail labels. *)

val remove_labels :
  session ->
  folder:string ->
  uids:ImapSet.t ->
  labels:string list -> unit Lwt.t
    (** Remove Gmail labels. *)

val set_labels :
  session ->
  folder:string ->
  uids:ImapSet.t ->
  labels:string list -> unit Lwt.t
    (** Sets Gmail labels. *)
                        
val capability :
  session -> capability list Lwt.t
    (** Requests capabilities of the server. *)
    
val enable_feature :
  session -> string -> bool Lwt.t

val uid_next : session -> Uint32.t

val uid_validity : session -> Uint32.t

val mod_sequence_value : session -> Uint64.t
