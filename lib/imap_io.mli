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

(** Buffered I/O
    
    This module adds buffering to the low-level streams of [Imap_io_low]. *)

(** The type of buffered i/o channels. *)
type t

val of_low : ?buffer_size:int -> Imap_io_low.t -> t
(** [of_low ?buffer_size low] creates a new buffered i/o-channel with underlying
    low-level i/o object [low] and buffer size [?buffer_size]. *)

val get_low : t -> Imap_io_low.t
(** [get_low io] returns the underlying low-level i/o object of [io]. *)
                     
val set_low : t -> Imap_io_low.t -> unit
(** [set_low io low] changes the underlying low_level i/o object of [io].  This
    does not cause any input output to occur. *)

(** {2 Reading} *)

val read_char : t -> char Lwt.t
(** [read_char io] reads one character from the stream [io].  Raises
    [End_of_file] if at end of file. *)
    
val read_char_opt : t -> char option Lwt.t
(** Like {!read_char}, but returns [None] instead of raising [End_of_file]. *)
    
val read_into : t -> string -> int -> int -> int Lwt.t
(** [read_into io buf off len] tries to read len character from [io] and write
    them to [buf], starting at position [off].  Returns the number of character read
    (0 at end of file). *)
    
val read_into_exactly : t -> string -> int -> int -> unit Lwt.t
(** [read_into_exactly io buf off len] tries to read exactly [len] character
    from [io] and copies them into [buf] starting at offset [off].  Raises
    [End_of_file] if less than [len] characters are available. *)
    
val read : ?count:int -> t -> string Lwt.t
(** [read ?count io] reads at most [?count] characters from [io]. *)
    
val read_line : t -> string Lwt.t
(** [read_line io] reads a line from [io].  The line can be terminated with
    either "\n" or "\r\n" and this is not included in the returned string. *)

(** {2 Writing} *)

val flush : t -> unit Lwt.t
(** [flush io] writes out all the bytes in the buffer. *)
    
val write_char : t -> char -> unit Lwt.t
(** [write_char io ch] writes character [ch] into [io]. *)
    
val write_from : t -> string -> int -> int -> int Lwt.t
(** [write_from io buf off len] tries to write [len] characters from [buf],
    starting at position [off] into [io].  Returns the number of characters actually
    written. *)
    
val write_from_exactly : t -> string -> int -> int -> unit Lwt.t
(** [write_from_exactly io buf off len] tries to write exactly [len] characters
    from [buf], starting at position [off] into [io]. *)
    
val write : t -> string -> unit Lwt.t
(** [write io s] writes the string [s] into [io]. *)

val close : t -> unit Lwt.t
(** [close io] flushes and closes the stream [io]. *)
   
