(* The MIT License (MIT)

   Copyright (c) 2015-2018 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

type capability =
  | IMAP4rev1
  | ACL
  | BINARY
  | CATENATE
  | CHILDREN
  | COMPRESS_DEFLATE
  | CONDSTORE
  | ESEARCH
  | ENABLE
  | IDLE
  | ID
  | LITERALPLUS
  | LITERALMINUS
  | UTF8_ACCEPT
  | UTF8_ONLY
  | MULTIAPPEND
  | NAMESPACE
  | QRESYNC
  | QUOTE
  | SORT
  | STARTTLS
  | UIDPLUS
  | UNSELECT
  | XLIST
  | AUTH_ANONYMOUS
  | AUTH_LOGIN
  | AUTH_PLAIN
  | XOAUTH2
  | X_GM_EXT_1
  | OTHER of string

type code =
  | ALERT
  | BADCHARSET of string list
  | CAPABILITY of capability list
  | PARSE
  | PERMANENTFLAGS of Flag.t list
  | READ_ONLY
  | READ_WRITE
  | TRYCREATE
  | UIDNEXT of int32
  | UIDVALIDITY of int32
  | UNSEEN of int32
  | OTHER of string * string option
  | CLOSED
  | HIGHESTMODSEQ of int64
  | NOMODSEQ
  | MODIFIED of (int32 * int32) list
  | APPENDUID of int32 * int32
  | COPYUID of int32 * (int32 * int32) list * (int32 * int32) list
  | UIDNOTSTICKY
  | COMPRESSIONACTIVE
  | USEATTR

type status = OK | NO | BAD

type state = { code : code option; message : string; status : status }

type untagged =
  | State of state
  | BYE of { code : code option; message : string }
  | PREAUTH of code option * string
  | FLAGS of Flag.t list
  | LIST of MailboxFlag.t list * char option * string
  | LSUB of MailboxFlag.t list * char option * string
  | SEARCH of int32 list * int64 option
  | STATUS of string * Status.MailboxAttribute.t list
  | EXISTS of int
  | RECENT of int
  | EXPUNGE of int32
  | FETCH of int32 * Fetch.MessageAttribute.t list
  | CAPABILITY of capability list
  | VANISHED of Uint32.Set.t
  | VANISHED_EARLIER of Uint32.Set.t
  | ENABLED of capability list

type t =
  | Untagged of untagged
  | Cont of string
  | Tagged of { tag : string; state : state }
