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

val fresh_session : string -> string -> session

type folder_flag =
  [ `None
  | `Marked
  | `Unmarked
  | `NoSelect
  | `NoInferiors
  | `Inbox
  | `SentMail
  | `Starred
  | `AllMail
  | `Trash
  | `Drafts
  | `Spam
  | `Important
  | `Archive ]

type message_flag =
  [ `None
  | `Seen
  | `Answered
  | `Flagged
  | `Deleted
  | `Draft
  | `MDNSent
  | `Forwarded
  | `SubmitPending
  | `Submitted ]

type messages_request_kind =
  [ `Uid
  | `Flags
  | `Headers
  | `Structure
  | `InternalDate
  | `FullHeaders
  | `HeaderSubject
  | `GmailLabels
  | `GmailMessageID
  | `GmailThreadID
  | `ExtraHeaders of string list
  | `Size ]

type fetch_request_type =
  [ `UID
  | `Sequence ]

type flags_request_kind =
  [ `Add
  | `Remove
  | `Set ]

type workaround =
  [ `Gmail
  | `Yahoo
  | `Exchange2003 ]

type auth_capability =
  [ `Anonymous
  | `CRAMMD5
  | `DigestMD5
  | `External
  | `GSSAPI
  | `KerberosV4
  | `Login
  | `NTLM
  | `OTP
  | `Plain
  | `SKey
  | `SRP ]

type capability =
  [ `ACL
  | `Binary
  | `Catenate
  | `Children
  | `CompressDeflate
  | `Condstore
  | `Enable
  | `Idle
  | `Id
  | `LiteralPlus
  | `MultiAppend
  | `Namespace
  | `QResync
  | `Quote
  | `Sort
  | `StartTLS
  | `ThreadORderedSubject
  | `ThreadReferences
  | `UIDPlus
  | `Unselect
  | `XList
  | `Auth of auth_capability
  | `XOAuth2
  | `Gmail ]

type encoding =
  [ `Bit7
  | `Bit8
  | `Binary
  | `Base64
  | `QuotedPrintable
  | `Other
  | `UUEncode ]

type error =
  [ `None
  | `Connection
  | `TLSNotAvailable
  | `Parse
  | `Certificate
  | `Authentication
  | `GmailIMAPNotEnabled
  | `GmailExceededBandwidthLimit
  | `GmailTooManySimultaneousConnections
  | `MobileMeMoved
  | `YahooUnavailable
  | `NonExistantFolder
  | `Rename
  | `Delete
  | `Create
  | `Subscribe
  | `Append
  | `Copy
  | `Expunge
  | `Fetch
  | `Idle
  | `Identity
  | `Namespace
  | `Store
  | `Capability
  | `StartTLSNotAvailable
  | `SendMessageIllegalAttachment
  | `StorageLimit
  | `SendMessageNotAllowed
  | `NeedsConnectToWebmail
  | `SendMessage
  | `AuthenticationRequired
  | `FetchMessageList
  | `DeleteMessage
  | `InvalidAccount
  | `File
  | `Compression
  | `NoSender
  | `NoRecipient
  | `Noop
  | `State ]

type folder_status = {
  unseen_count : int;
  message_count : int;
  recent_count : int;
  uid_next : Uint32.t;
  uid_validity : Uint32.t;
  highest_mod_seq_value : Uint64.t
}

type 'a control = ('a, session, error) ImapControl.control

val connect : [ `NeedsAuth | `PreAuth ] control

val login : unit control

val select : string -> unit control

val folder_status : string -> folder_status control

val enable_feature : string -> bool control
