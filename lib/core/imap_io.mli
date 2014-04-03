module type S = sig
  type 'a t
      
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
      
  type mutex
  val create_mutex : unit -> mutex
  val is_locked : mutex -> bool
  val with_lock : mutex -> (unit -> 'a t) -> 'a t
    
  type input
  type output

  val read_line : input -> string t
  val read_exactly : input -> int -> string t
  val write : output -> string -> unit t
  val flush : output -> unit t

  val starttls : [ `TLSv1 | `SSLv23 | `SSLv3 ] -> ?ca_file : string ->
    input * output -> (input * output) t
  val compress : input * output -> input * output
end
