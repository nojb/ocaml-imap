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

(** Message search *)

type modseq = int64 [@@deriving sexp]

type uid = int32 [@@deriving sexp]

type seq = int32 [@@deriving sexp]

type key = Encoder.t [@@deriving sexp]

val all: key
(** All messages in the mailbox. *)

val seq: seq list -> key
(** Messages with message sequence number in the given set. *)

val answered: key
(** Messages with the [Answered] flag set. *)

val bcc: string -> key
(** Messages that contain the specified string in the envelope structure's
    "BCC" field. *)

val before: Fetch.Date.t -> key
(** Messages whose internal date (disregarding time and timezone) is earlier
    than the specified date. *)

val body: string -> key
(** Messages that contain the specified string in the body of the message. *)

val cc: string -> key
(** Messages that contain the specified string in the envelope structure's
    "CC" field. *)

val deleted: key
(** Messages with the [Deleted] {!flag} set. *)

val draft: key
(** Messages with the [Draft] {!flag} set. *)

val flagged: key
(** Messages with the [Flagged] {!flag} set. *)

val from: string -> key
(** Messages with FROM field containing given string. *)

val header: string -> string -> key
(** Messages with headers with the specified field-name and that
    contains the specified string in the text of the header (what comes after
    the colon). *)

val keyword: string -> key
(** Messages with the specified [Keyword] {!flag} set. *)

val larger: int -> key
(** Messages with size at least the given number of bytes. *)

val new_: key
(** Messages that have the [Recent] {!flag} set but not the [Seen] {!flag}. *)

val not: key -> key
(** Negation of search criteria. *)

val old: key
(** Messages that do not have the [Recent] {!flag} set. *)

val on: Fetch.Date.t -> key
(** Messages whose internal date (disregarding time and timezone) is within
    the specified date.  *)

val (||): key -> key -> key
(** OR of search criteria. *)

val recent: key
(** Messages that have the [Recent] {!flag} set. *)

val seen: key
(** Messages that have the [Seen] {!flag} set. *)

val sent_before: Fetch.Date.t -> key
(** Messages whose "Date:" header (disregarding time and timezone) is earlier
    than the specified date. *)

val sent_on: Fetch.Date.t -> key
(** Messages whose "Date:" header (disregarding time and timezone) is within
    the specified date. *)

val sent_since: Fetch.Date.t -> key
(** Messages whose "Date:" header (disregarding time and timezone) is within
    or later than the specified date.  *)

val since: Fetch.Date.t -> key
(** Messages whose internal date (disregarding time and timezone) is within or
    later than the specified date.  *)

val smaller: int -> key
(** Messages with a size smaller than the specified number of octets. *)

val subject: string -> key
(** Messages that contain the specified string in the envelope structure's
    "SUBJECT" field. *)

val text: string -> key
(** Messages that contain the specified string in the header or body of the
    message. *)

val to_: string -> key
(** Messages that contain the specified string in the envelope structure's
    "TO" field. *)

val uid: uid list -> key
(** Messages with UID in the given set. *)

val unanswered: key
(** Messages that do not have the [Answered] {!flag} set. *)

val undeleted: key
(** Messages that do not have the [Deleted] {!flag} set. *)

val undraft: key
(** Messages that do not have the [`Draft] {!flag} set. *)

val unflagged: key
(** Messages that do not have the [`Flagged] {!flag} set. *)

val unkeyword: string -> key
(** Messages that do not have the specified keyword {!flag} set. *)

val unseen: key
(** Messages that do not have the [`Seen] {!flag} set. *)

val (&&): key -> key -> key
(** Messages that satisfy both search criteria. *)

val modseq: modseq -> key
(** Messages that have equal or greater modification sequence numbers. *)

val x_gm_raw: string -> key
(** Gmail search string *)

val x_gm_msgid: modseq -> key
(** Messages with a given Gmail Message ID. *)

val x_gm_thrid: modseq -> key
(** Messages with a given Gmail Thread ID. *)

val x_gm_labels: string list -> key
(** Messages with given Gmail labels. *)
