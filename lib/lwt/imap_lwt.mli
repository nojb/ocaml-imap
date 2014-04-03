include Imap.S with type 'a IO.t = 'a Lwt.t

val connect_simple : session -> ?port : int -> string -> [ `Needsauth | `Preauth ] Lwt.t
(* val compress : session -> unit Lwt.t *)
(* val starttls : ?ssl_context:Ssl.context -> session -> unit Lwt.t *)
