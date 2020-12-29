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

open Common

type 'a t

val flags : Flag.t list t

val envelope : Envelope.t t

val internaldate : string t

val uid : uid t

val x_gm_msgid : int64 t

val x_gm_thrid : int64 t

val x_gm_labels : string list t

val rfc822 : string t

val rfc822_text : string t

val rfc822_header : string t

val rfc822_size : int t

val body : MIME.Response.t t

val bodystructure : MIME.Response.t t

val modseq : int64 t

val map : ('a -> 'b) -> 'a t -> 'b t

val pair : 'a t -> 'b t -> ('a * 'b) t

val encode : _ t -> Encoder.t

val matches : 'a t -> Response.message_attribute list -> 'a option
