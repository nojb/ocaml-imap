(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 Nicolas Ojeda Bar <n.oje.bar@gmail.com>            *)

type connection

val connect: int -> string -> connection Lwt.t

val run: connection -> 'a Imap.command -> 'a Lwt.t
