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

module Date = struct
  type t =
    {
      day: int;
      month: int;
      year: int;
    } [@@deriving sexp]

  let to_string {day; month; year} =
    let months =
      [|
        "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
        "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec";
      |]
    in
    Printf.sprintf "%2d-%s-%4d" day months.(month) year

  let encode d =
    Encoder.raw (to_string d)
end

module Time = struct
  type t =
    {
      hours: int;
      minutes: int;
      seconds: int;
      zone: int;
    } [@@deriving sexp]

  let to_string {hours; minutes; seconds; zone} =
    Printf.sprintf "%02d:%02d:%02d %c%04d" hours minutes seconds
      (if zone >= 0 then '+' else '-') (abs zone)
end

module MessageAttribute = struct
  type t =
    | FLAGS of Flag.t list
    | ENVELOPE of Envelope.t
    | INTERNALDATE of Date.t * Time.t
    (* | RFC822 of string option *)
    (* | RFC822_HEADER of string option *)
    (* | RFC822_TEXT of string option *)
    | RFC822_SIZE of int
    | BODY of MIME.Response.t
    | BODYSTRUCTURE of MIME.Response.t
    | BODY_SECTION of MIME.Section.t * string option
    | UID of int32
    | MODSEQ of int64
    | X_GM_MSGID of int64
    | X_GM_THRID of int64
    | X_GM_LABELS of string list [@@deriving sexp]
end

module Request = struct
  open Encoder

  type nonrec t = t

  let envelope = raw "ENVELOPE"
  let internaldate = raw "INTERNALDATE"
  let rfc822_size = raw "RFC822.SIZE"
  let body = raw "BODY"
  let body_section ?(peek = true) ?section:(sec = [], None) () =
    raw (if peek then "BODY.PEEK" else "BODY") & raw "[" & MIME.Request.encode sec & raw "]"
  let bodystructure = raw "BODYSTRUCTURE"
  let uid = raw "UID"
  let flags = raw "FLAGS"

  let all = [flags; internaldate; rfc822_size; envelope]
  let fast = [flags; internaldate; rfc822_size]
  let full = [flags; internaldate; rfc822_size; envelope; body]

  let x_gm_msgid = raw "X-GM-MSGID"
  let x_gm_thrid = raw "X-GM-THRID"
  let x_gm_labels = raw "X-GM-LABELS"
end

type modseq = int64 [@@deriving sexp]

type uid = int32 [@@deriving sexp]

type seq = int32 [@@deriving sexp]

module Response = struct
  type t =
    {
      flags: Flag.t list;
      envelope: Envelope.t option;
      internaldate: (Date.t * Time.t) option;
      rfc822_size: int option;
      body: MIME.Response.t option;
      bodystructure: MIME.Response.t option;
      body_section: (MIME.Section.t * string) list;
      uid: uid option;
      modseq: modseq option;
      x_gm_msgid: modseq option;
      x_gm_thrid: modseq option;
      x_gm_labels: string list option;
    } [@@deriving sexp]

  let default =
    {
      flags = [];
      envelope = None;
      internaldate = None;
      rfc822_size = None;
      body = None;
      bodystructure = None;
      body_section = [];
      uid = None;
      modseq = None;
      x_gm_msgid = None;
      x_gm_thrid = None;
      x_gm_labels = None;
    }
end
