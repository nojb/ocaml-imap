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

(** MIME body structure *)

open Sexplib.Std
open Sexplib.Conv
open Imap_uint
  
(** MIME media types *)
type media_basic =
  [ `APPLICATION
  (** application/xxx *)
  | `AUDIO
  (** audio/xxx *)
  | `IMAGE
  (** image/xxx *)
  | `MESSAGE
  (** message/xxx *)
  | `VIDEO
  (** video/xxx *)
  | `OTHER of string
  (** for all other cases *) ] with sexp

(** Values of the [Content-Transfer-Encoding] header *)
type encoding =
  [ `BIT7
  | `BIT8
  | `BINARY
  | `BASE64
  | `QUOTED_PRINTABLE
  | `OTHER of string ] with sexp

type extension =
  | List of extension list
  | Number of uint32
  | String of string with sexp

type exts = {
  ext_md5 : Digest.t option sexp_opaque;
  ext_dsp : (string * (string * string) list) option;
  ext_lang : string list;
  ext_exts : extension list
} with sexp

type mexts = {
  mext_param : (string * string) list;
  mext_dsp : (string * (string * string) list) option;
  mext_lang : string list;
  mext_exts : extension list
} with sexp

type basic = {
  basic_type : string;
  basic_subtype : string;
} with sexp

type text = {
  text_subtype : string;
  text_lines : int
} with sexp

type message = {
  message_envelope : Imap_envelope.t;
  message_body : t;
  message_lines : int
}

and 'a single_part = {
  bd_param : (string * string) list;
  bd_id : string;
  bd_desc : string;
  bd_enc : encoding;
  bd_octets : int;
  bd_other : 'a;
  bd_ext : exts
}

and multi_part = {
  bd_subtype : string;
  bd_parts : t list;
  bd_mexts : mexts
}

(** The type of MIME body parsed by IMAP server *)
and t =
  | Basic of basic single_part
  | Text of text single_part
  | Message of message single_part
  (** Single parts *)
  | Multi_part of multi_part with sexp
  (** Multi part *)

val body : t Imap_parser.t
