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

(** {3 MIME message structure}

    The following types describe the bodies of
    {{:http://en.wikipedia.org/wiki/MIME}MIME} emails.  The IMAP server can
    parse the MIME structure of the messages and return individual parts.  This
    saves the client from having to do that parsing itself.  See
    {ul
    {- {{:http://tools.ietf.org/html/rfc2045}RFC 2045: Format of Internet Message Bodies}}
    {- {{:http://tools.ietf.org/html/rfc2046}RFC 2046: Media Types}}}
    and related RFCs for details. *)

(** Basic fields of a MIME body part.  See
    {{:https://tools.ietf.org/html/rfc2045}RFC 2045} for more details. *)
type fields =
  {
    fld_params: (string * string) list;      (* Part parameters *)
    fld_id: string option;                   (* Optional part ID *)
    fld_desc: string option;                 (* Optional content description *)
    fld_enc: string;                         (* Content transfer encoding *)
    fld_octets: int;           (* Size in bytes *)
  } [@@deriving sexp]

type body_extension =
  | List of body_extension list
  | Number of int32
  | String of string [@@deriving sexp]

type part_extension =
  {
    ext_dsp: (string * (string * string) list) option;
    ext_lang: string list;
    ext_loc: string option;
    ext_ext: body_extension list;
  } [@@deriving sexp]

(** MIME content types

    The message MIME content type can be retrieved using {!fetch} with
    [`Body_structure].  Similarly, individual MIME parts can be retrieved using
    {!fetch} with an appropriate [`Body_section] message attribute.

    In IMAP, MIME media types are described as follows:

    - [`Text (s, f, n)] corresponds to the MIME type ["TEXT/" ^ s].  Common
      examples of the subtype [s] are ["HTML"] and ["PLAIN"].  [f] contains
      general part information (see {!fields}), and [n] is the number of text
      lines of the part.

    - [`Message (f, e, m, n)] corresponds to the MIME type ["MESSAGE/RFC822"],
      used to enclose a complete message within a message. [f] contains general
      part information (see {!fields}), [e] is the envelope information of the
      encapsulated message, [m] is the MIME structure of the encapsulated
      message, and [n] is the number of lines in the encapsulated message.

    - [`Basic (t, s, f)] corresponds to a (non-multipart) MIME type [t ^ "/" ^
      s].  Common examples of the type [t] are ["APPLICATION"], ["AUDIO"],
      ["IMAGE"], ["MESSAGE"], ["VIDEO"].  [f] contains general part information
      (see {!fields}).

    - [`Multipart (p, s)] corresponds to the MIME type ["MULTIPART/" ^ s].  [p]
      is the lists of MIME subparts. *)
type t =
  | Text of string * fields * int
  | Message of fields * Envelope.t * t * int
  | Basic of string * string * fields
  | Multipart of t list * string [@@deriving sexp]
