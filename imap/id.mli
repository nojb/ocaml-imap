open ImapTypes
  
val id : (string * string option) list -> (string * string option) list command
val id_basic : string -> string -> (string option * string option) command
