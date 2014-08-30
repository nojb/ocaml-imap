type reader

val create : unit -> reader
val read : reader -> [ `Ok of string * string list | `Await | `End ]
val src : reader -> [ `Data of string | `End ] -> unit
