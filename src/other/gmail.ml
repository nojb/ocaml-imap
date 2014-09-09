open Imap
open Types
open ImapUnix

let gmail_server = "imap.gmail.com"

module Condition = struct
  type t =
    search_key

  let from email =
    SEARCH_KEY_FROM email

  let (||) c1 c2 =
    SEARCH_KEY_OR (c1, c2)

  let (&&) c1 c2 =
    SEARCH_KEY_AND (c1, c2)

  let label lab =
    SEARCH_KEY_XGMLABEL lab
end

type condition = Condition.t

(* class message session gid envelope = object *)
(*   method delete = *)
(*     ImapUnix.send_command session (Command.) *)
(* end *)

class type message_set = object
  method count : int
end

class gmail user pass = object
  val session = ImapUnix.create_session gmail_server
  method search c =
    let results = ImapUnix.send_command session (Commands.search c) in
    ()
  method inbox =
    search (Condition.label "Inbox")
  initializer
    ImapUnix.send_command session (Commands.login user pass)
end

(* type condition *)

(* val after : string -> condition *)
(* val *)
  
(* let _ = *)
(*   let gmail = new gmail "n.oje.bar@gmail.com" "..." in *)
(*   gmail # inbox # count all; *)
(*   gmail # inbox # count (after "2010-02-20" && before "34234234" || from "myfriend@gmail.com"); *)
(*   gmail # inbox # search (after "2010-02-20") # count; *)
(*   gmail # label "Urgent" # count; *)
(*   gmail # inbox # for_each (fun email -> email # mark spam); *)
(*   gmail # inbox # search (from "ihateyou@gmail.com") # for_each (fun email -> email #delete); *)
(*   gmail # inbox # search (gid 32414124124) # label "faxes" *)



(* module Gmail = struct *)
(*   type condition = *)
(*     | From of string *)
(*     | And of condition * condition *)
(*     | 0 *)
(*   val from : string -> condition *)
    
(*   val (&) : condition -> condition -> condition *)
(*   val (|) : condition -> condition -> condition *)

(*   class type mailbox = object *)
(*     method search : condition -> message_set *)
(*     method name : string *)
(*     method rename : string *)
(*     inherit message_set *)
(*   end *)

(*   class type message_set = object *)
(*     method count : int *)
(*     method for_each : (message -> unit) -> unit *)
(*     method delete : unit *)
(*     method label : string -> unit *)
(*     method unlabel : string -> unit *)
(*     method move : string -> unit *)
(*   end *)

(*   class type message = object *)
(*     method mark : marking -> unit *)
(*     method delete : unit *)
(*     method id : int64 *)
(*     method rfc822 : string *)
(*     method full : string *)
(*   end *)

(*   class gmail : string -> string -> *)
(*     object *)
(*       method inbox : mailbox *)
(*       method all : mailbox *)
(*       method trash : mailbox *)
(*       method label : string -> mailbox *)
(*       method for_each : (mailbox -> unit) -> unit *)
(*       method logout : unit *)
(*     end *)
(* end *)

(* (new gmail) # for_each (fun mbox -> mbox # count *)

(*                            gmail # inbox # search (from "n.oje.bar@gmail.com" & before "1003-40-12") # delete *)
(*                            gmail # inbox # label "old" *)
(*                            gmail # inbox # search (from "n.oje.bar@gmail.com") # label "old" *)
(*                            gmail #  *)
