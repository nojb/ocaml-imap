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

open Sexplib.Std
open Common

open Encoder

type key = t [@@deriving sexp]

let all = raw "ALL"
let seq s = eset (Uint32.Set.of_list s)
let answered = raw "ANSWERED"
let bcc s = raw "BCC" ++ str s
let before t = raw "BEFORE" ++ Fetch.Date.encode t
let body s = raw "BODY" ++ str s
let cc s = raw "CC" ++ str s
let deleted = raw "DELETED"
let draft = raw "DRAFT"
let flagged = raw "FLAGGED"
let from s = raw "FROM" ++ str s
let header s1 s2 = raw "HEADER" ++ str s1 ++ str s2
let keyword s = raw "KEYWORD" ++ str s
let larger n = raw "LARGER" ++ int n
let new_ = raw "NEW"
let not k = raw "NOT" ++ p k
let old = raw "OLD"
let on t = raw "ON" ++ Fetch.Date.encode t
let (||) k1 k2 = raw "OR" ++ p k1 ++ p k2
let recent = raw "RECENT"
let seen = raw "SEEN"
let sent_before t = raw "SENTBEFORE" ++ Fetch.Date.encode t
let sent_on t = raw "SENTON" ++ Fetch.Date.encode t
let sent_since t = raw "SENTSINCE" ++ Fetch.Date.encode t
let since t = raw "SINCE" ++ Fetch.Date.encode t
let smaller n = raw "SMALLER" ++ int n
let subject s = raw "SUBJECT" ++ str s
let text s = raw "TEXT" ++ str s
let to_ s = raw "TO" ++ str s
let uid s = raw "UID" ++ eset (Uint32.Set.of_list s)
let unanswered = raw "UNANSWERED"
let undeleted = raw "UNDELETED"
let undraft = raw "UNDRAFT"
let unflagged = raw "UNFLAGGED"
let unkeyword s = raw "UNKEYWORD" ++ str s
let unseen = raw "UNSEEN"
let (&&) k1 k2 = p k1 ++ p k2
let modseq n = raw "MODSEQ" ++ uint64 n
let x_gm_raw s = raw "X-GM-RAW" ++ str s
let x_gm_msgid n = raw "X-GM-MSGID" ++ uint64 n
let x_gm_thrid n = raw "X-GM-THRID" ++ uint64 n
let x_gm_labels l = raw "X-GM-LABELS" ++ list str l
