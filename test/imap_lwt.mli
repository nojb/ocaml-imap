(* this code is in the public domain *)

type connection

val connect : ?port:int -> string -> connection Lwt.t
val run : connection -> Imap.command -> Imap.untagged Lwt_stream.t
