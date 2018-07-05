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

module Request = struct
  type section_msgtext = Response.section_msgtext =
    | HEADER
    | HEADER_FIELDS of string list
    | HEADER_FIELDS_NOT of string list
    | TEXT
    | MIME [@@deriving sexp]

  type section =
    int list * section_msgtext option [@@deriving sexp]

  open Encoder

  type t = rope

  let section_msgtext = function
    | HEADER -> raw "HEADER"
    | HEADER_FIELDS l -> raw "HEADER.FIELDS" ++ plist str l
    | HEADER_FIELDS_NOT l -> raw "HEADER.FIELDS.NOT" ++ plist str l
    | TEXT -> raw "TEXT"
    | MIME -> raw "MIME"

  let section_enc (nl, sec) =
    let sec = match sec with None -> empty | Some sec -> section_msgtext sec in
    match nl with
    | [] ->
        sec
    | _ :: _ ->
        list ~sep:'.' int nl & raw "." & sec

  let header ?(part = []) () = part, Some HEADER
  let header_fields ?(part = []) l = part, Some (HEADER_FIELDS l)
  let header_fields_not ?(part = []) l = part, Some (HEADER_FIELDS_NOT l)
  let text ?(part = []) () = part, Some TEXT
  let part ~part () = part, None
  let mime ~part () = part, Some MIME

  let envelope = raw "ENVELOPE"
  let internaldate = raw "INTERNALDATE"
  let rfc822_size = raw "RFC822.SIZE"
  let body = raw "BODY"
  let body_section ?(peek = true) ?section:(sec = [], None) () =
    raw (if peek then "BODY.PEEK" else "BODY") & raw "[" & section_enc sec & raw "]"
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
      flags: Flag.t list option;
      envelope: Envelope.t option;
      internaldate: (Response.Date.t * Response.Time.t) option;
      rfc822_size: int option;
      body: MIME.t option;
      bodystructure: MIME.t option;
      body_section: (Response.section * string option) list;
      uid: uid option;
      modseq: modseq option;
      x_gm_msgid: modseq option;
      x_gm_thrid: modseq option;
      x_gm_labels: string list option;
    } [@@deriving sexp]

  let default =
    {
      flags = None;
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
