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

module MailboxAttribute = struct
  type t =
    | MESSAGES of int
    | RECENT of int
    | UIDNEXT of int32
    | UIDVALIDITY of int32
    | UNSEEN of int
    | HIGHESTMODSEQ of int64 [@@deriving sexp]
end

module Request = struct
  open Encoder

  type nonrec t = t

  let messages = raw "MESSAGES"

  let recent = raw "RECENT"

  let uidnext = raw "UIDNEXT"

  let uidvalidity = raw "UIDVALIDITY"

  let unseen = raw "UNSEEN"

  let highestmodseq = raw "HIGHESTMODSEQ"
end

type modseq = int64 [@@deriving sexp]

type uid = int32 [@@deriving sexp]

type seq = int32 [@@deriving sexp]

module Response = struct
  type t =
    {
      messages: int option;
      recent: int option;
      uidnext: uid option;
      uidvalidity: uid option;
      unseen: int option;
      highestmodseq: modseq option;
    } [@@deriving sexp]

  let default =
    {
      messages = None;
      recent = None;
      uidnext = None;
      uidvalidity = None;
      unseen = None;
      highestmodseq = None;
    }
end
