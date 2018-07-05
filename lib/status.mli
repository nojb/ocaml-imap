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

(** {3 Mailbox status} *)

module Request : sig
  type t = Encoder.rope
  (** Mailbox attibutes that can be requested with the {!status} command. *)

  val messages: t
  (** The number of messages in the mailbox. *)

  val recent: t
  (** The number of messages with the [Recent] {!flag} set. *)

  val uidnext: t
  (** The next unique identifier value of the mailbox. *)

  val uidvalidity: t
  (** The unique identifier validity value of the mailbox. *)

  val unseen: t
  (** The number of messages which do not have the [Seen] {!flag} set. *)

  val highestmodseq: t
  (** TODO *)
end

type modseq = int64 [@@deriving sexp]

type uid = int32 [@@deriving sexp]

type seq = int32 [@@deriving sexp]

module Response : sig
  (** Mailbox status items returned by {!status} command. *)
  type t =
    {
      messages: int option;
      recent: int option;
      uidnext: uid option;
      uidvalidity: uid option;
      unseen: int option;
      highestmodseq: modseq option;
    } [@@deriving sexp]

  val default: t
end
