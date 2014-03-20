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

(** Low-level I/O *)

type log_type =
  | Log_read
  (** The logging was triggered by a read operation *)
  | Log_write
  (** The logging was triggered by a write operation *)

type logger = log_type -> string -> unit Lwt.t

type t

val null : t
(** The null i/o channel- everything written to it disappears and nothing can be
    read from it. *)
  
val read  : t -> string -> int -> int -> int Lwt.t
(** [read io buf off len] tries to read [len] characters into [buf], starting at
    position [off] from [io].  Returns the number of characters actually read (0 at
    end of file). *)
  
val write : t -> string -> int -> int -> int Lwt.t
(** [write io buf off len] tries to write [len] characters from [buf], starting
    at position [off], into [io].  Returns the number of characters actually
    written. *)
    
val close : t -> unit Lwt.t
(** [close io] closes [io]. *)

val get_logger : t -> logger option
(** Returns the current logger object (if any). *)
    
val set_logger : t -> logger option -> unit
(** Sets the current logger object. *)

val get_fd : t -> Lwt_unix.file_descr option
(** Returns the underlying Unix file descriptor (if any). *)

val of_fd : Lwt_unix.file_descr -> t
(** Returns a new i/o object that will read and write from a Unix file descriptor. *)

val open_ssl : Ssl.context -> Lwt_unix.file_descr -> t * (unit -> unit Lwt.t)
(** Embeds a Unix file descriptor into a SSL socket and returns a pair [(io,
    connect)]. The channel [io] that can be used to read and write on this
    socket.  The function [connect] should be used before reading and writing
    [io] to negotiate the SSL connection. *)

val compress : t -> t
(** [compress io] returns a new i/o channel that performs on-the-fly DEFLATE compression. *)

val default_logger : logger
(** The default logger.  It logs everything to [stderr] with suitable formatting. *)
