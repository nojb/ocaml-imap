open Imap
open Types
open ImapUnix

let gmail_server = "imap.gmail.com"

class message g mb uid = object (self)
  method uid = uid
  method flag flg =
    g # in_mailbox mb
      (fun () -> g # imap # uid_store uid `Add [flg])
  method unflag flg =
    g # in_mailbox mb
      (fun () -> g # imap # uid_store uid `Remove [flg])
end

class mailbox g name = object (self)
  method emails c =
    List.map (fun uid -> new message g self uid) (g # imap # uid_search c)
end

module Condition = struct
  type t =
    search_key

  let from email =
    SEARCH_KEY_FROM email

  let ( ||| ) c1 c2 =
    SEARCH_KEY_OR (c1, c2)

  let ( &&& ) c1 c2 =
    SEARCH_KEY_AND (c1, c2)

  let label lab =
    SEARCH_KEY_XGMLABELS lab
end

type condition = Condition.t

  (* let fetch_data = *)
  (*   ImapUnix.send_command session *)
  (*     (Commands.uid_fetch (ImapSet.of_list results) [Xgmmsgid.fetch_att_xgmmsgid]) *)
  (* in *)
  (* let extract_gid (atts, _) = *)
  (*   let rec loop = *)
  (*     function *)
  (*       [] -> *)
  (*         None *)
  (*     | MSG_ATT_ITEM_EXTENSION (Xgmmsgid.XGMMSGID_MSGID n) :: _ -> *)
  (*         Some n *)
  (*     | _ :: rest -> *)
  (*         loop rest *)
  (*   in *)
  (*   loop atts *)
  (* in *)
  (* let gids = *)
  (*   List.fold_right (fun atts l -> match extract_gid atts with Some gid -> gid :: l | None -> l) fetch_data [] *)
(* in *)

module Cache : sig
  type 'a t
  val create : (unit -> 'a) -> 'a t
  val get : 'a t -> 'a
end = struct
  type 'a t =
    'a option ref * (unit -> 'a)
  let create f =
    ref (None, f)
  let get c =
    match !c with
      None, f -> let x = f (); in c := x; x
    | Some x, _ -> x
end

type message_data = {
  msg_env : envelope;
  msg_size : int;
  msg_labels : string list
}

let fetch_message_data session uid =
  let fetch_data =
    ImapUnix.send_command session
      (Command.uid_fetch
         (ImapSet.single uid)
         [Xgmlabels.fetch_att_xgmlabels; FETCH_ATT_ENVELOPE; FETCH_ATT_RFC822_SIZE])
  in
  let (atts, _) = fetch_data in
  let rec loop msg_env msg_size msg_labels =
    function
      [] ->
        {msg_env; msg_size; msg_labels}
    | FETCH_ATT_ENVELOPE msg_env :: rest ->
        loop msg_env msg_size msg_labels rest
    | FETCH_ATT_RFC822_SIZE msg_size :: rest ->
        loop msg_env msg_size msg_labels rest
    | _ :: rest ->
        loop msg_env msg_size msg_labels rest
  in
  loop {} 0 []

class message session uid = object
  val data : message_data Cache.t = Cache.create (fun () -> fetch_message_data session uid)

  method size = (Cache.get data).msg_size
  method labels = (Cache.get data).msg_labels
  method subject = (Cache.get data).msg_env.env_subject
end

let rec fetch session cond =
  ImapUnix.send_command session (Commands.uid_search cond)

class message_set session cond uids = object (self)
  method count =
    List.length uids
  method search c =
    let c = Condition.(cond &&& c) in
    new fetch_message_set session c
  method inbox =
    let c = Condition.label "Inbox" in
    new fetch_message_set session c
end
  
and fetch_message_set session c =
  message_set session c (fetch session c)

class gmail user pass =
  let session = ImapUnix.create_session gmail_server in
  let _ = ImapUnix.send_command session (Commands.login user pass) in
  let () = ImapUnix.send_command session (Commands.select "[Gmail]/All Mail") in
  fetch_message_set session (Condition.label "Inbox")

let _ =
  let g = new gmail "n.oje.bar@gmail.com" "1983.Urmi.Nico" in
  let n = g # count in
  Printf.eprintf "Inbox count: %i\n%!" n

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
