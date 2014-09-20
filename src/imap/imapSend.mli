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

(** Output routines *)

open ImapTypes
open ImapControl
  
type t = (unit, state, error) control

val space : t
(** Sends [' ']. *)

val crlf : t
(** Sends ["\r\n"]. *)

val char : char -> t
(** Sends a character. *)

val int : int -> t
(** Sends an OCaml int. *)

val raw : string -> t
(** [raw s] sends the string [s]. *)

val null : t
(** Does not send anything. *)

val nil : t
(** Sends ["NIL"]. *)

val separated : t -> ('a -> t) -> 'a list -> t
(** [separated sep f xs] is equivalent to [f x1 @> sep @> f x2 @> ... @> f xn]
    if [xs] is the list [[x1; ...; xn]]. *)

val list : ('a -> t) -> 'a list -> t
(** [list f xs] outputs "(a1 a2 ... an)" if [ai = f xi] and [xs] is the list
    [[x1; ...; xn]]. *)

val separated_pair : ('a -> t) -> t -> ('b -> t) -> ('a * 'b) -> t
(** [separated_pair f sep g (x, y)] is equivalent to [f x @> sep @> g y]. *)

val string : string -> t
(** Sends an IMAP [string] terminal. *)

val nstring : string option -> t
(** [nstring (Some s)] sends an IMAP [string] terminal [s].  [nstring None]
    sends ["NIL"]. *)

val literal : string -> t
(** Sends an IMAP literal. *)

val mailbox : string -> t
(** Sends an IMAP mailbox name.  It assumes the argument is encoded with UTF8
    and translates it to modified UTF-7. *)

val date_time : float -> t
(** Sends an IMAP [date-time]. *)

val message_set : ImapSet.t -> t
(** Sends an IMAP [sequence-set]. *)

val flag : flag -> t
(** Sends an IMAP flag. *)

val fetch_att : fetch_att -> t
(** Sends an IMAP fetch attribute. *)

val fetch_type : fetch_type -> t

val search_key_need_to_send_charset : search_key -> bool

val search_key : search_key -> t
(** Sends an IMAP fetch query. *)

val status_att : status_att -> t
(** Sends an IMAP status attribute. *)

val store_att_flags : store_att_flags -> t
(** Sends an IMAP store attribute. *)
