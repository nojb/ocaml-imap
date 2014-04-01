open Core.Std
open Async.Std

include Imap.S with type 'a IO.t = ('a, exn) Result.t Deferred.t

val connect_simple : session -> ?port : int -> string -> [`Needsauth | `Preauth] IO.t
