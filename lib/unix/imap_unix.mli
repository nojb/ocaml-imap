include Imap.S with type 'a IO.t = 'a

val connect_simple : session -> ?port : int -> string -> [ `Needsauth | `Preauth ]
val compress : session -> unit
val starttls : ?ssl_context:Ssl.context -> session -> unit
