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

module type UNIX = sig
  module IO : IO.T
  type file_descr
  val unix_file_descr : file_descr -> Unix.file_descr
  val socket : Unix.socket_domain -> Unix.socket_type -> int -> file_descr
  val connect : file_descr -> Unix.sockaddr -> unit IO.t
  val gethostbyname : string -> Unix.host_entry IO.t
  val read : file_descr -> string -> int -> int -> int IO.t
  val write : file_descr -> string -> int -> int -> int IO.t
  val close : file_descr -> unit IO.t
  val check_descriptor : file_descr -> unit
  exception Retry_read
  exception Retry_write
  type io_event =
    | Read
    | Write
  val register_action : io_event -> file_descr -> (unit -> 'a) -> 'a IO.t
  val prerr_string : string -> unit IO.t
end

module Make (U : UNIX) : sig
  module IO : IO.T
  type log_type =
    | Log_read
    (** The logging was triggered by a read operation *)
    | Log_write
    (** The logging was triggered by a write operation *)

  type logger = log_type -> string -> unit IO.t

  type t

  val null : t
  (** The null i/o channel- everything written to it disappears and nothing can be
      read from it. *)

  val read  : t -> string -> int -> int -> int IO.t
  (** [read io buf off len] tries to read [len] characters into [buf], starting at
      position [off] from [io].  Returns the number of characters actually read (0 at
      end of file). *)

  val write : t -> string -> int -> int -> int IO.t
  (** [write io buf off len] tries to write [len] characters from [buf], starting
      at position [off], into [io].  Returns the number of characters actually
      written. *)

  val close : t -> unit IO.t
  (** [close io] closes [io]. *)

  val get_logger : t -> logger option
  (** Returns the current logger object (if any). *)

  val set_logger : t -> logger option -> unit
  (** Sets the current logger object. *)

  val get_fd : t -> U.file_descr option
  (** Returns the underlying Unix file descriptor (if any). *)

  val of_fd : U.file_descr -> t
  (** Returns a new i/o object that will read and write from a Unix file descriptor. *)

  val open_socket : unit -> t * (?port : int -> string -> unit IO.t)
  (** Returns a pair [(io, connect)], where [io] is a new i/o object that will
      read and write from a Unix tcp socket.  The function [connect] should be
      called before trying to do any reading or writing in order to connect the
      socket to the given port and host. *)

  val open_ssl : ?ssl_context:Ssl.context -> unit -> t * (?port : int -> string -> unit IO.t)
  (** Returns a pair [(io, connect)].  [io] is a new i/o object that will read and
      write from a Unix tcp SSL socket.  The function [connect] should be used
      before reading and writing [io] to negotiate the SSL connection on the given
      port and host. *)

  val open_tls : ?ssl_context:Ssl.context -> U.file_descr -> t * (unit -> unit IO.t)
  (** Returns a pair [(io, connect)].  [io] is a new i/o object that will read and
      write from a SSL connection over the given socket.  The function [connect]
      should be used before reading and writing [io] to negotiate the SSL
      connection. *)

  val compress : t -> t
  (** [compress io] returns a new i/o channel that performs on-the-fly DEFLATE compression. *)
  
  val default_logger : logger
  (** The default logger.  It logs everything to [stderr] with suitable formatting. *)
end with module IO = U.IO
