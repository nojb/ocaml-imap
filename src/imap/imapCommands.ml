(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open ImapTypes
open ImapTypesPrivate
open ImapCore
open ImapControl

let capability =
  std_command (ImapSend.raw "CAPABILITY") >> gets (fun s -> s.cap_info)

let noop =
  std_command (ImapSend.raw "NOOP")

let logout =
  catch (std_command (ImapSend.raw "LOGOUT")) (function Bye -> ret () | _ as e -> fail e)

(* let starttls ?(version = `TLSv1) ?ca_file s = *)
(*   let ci = connection_info s in *)
(*   let cmd = S.raw "STARTTLS" in *)
(*   let aux () = *)
(*     if ci.compress_deflate then *)
(*       IO.fail (Failure "starttls: compression active") *)
(*     else *)
(*       send_command ci cmd >>= fun () -> *)
(*       IO.starttls version ?ca_file ci.chan >>= begin fun chan -> *)
(*         ci.chan <- chan; *)
(*         ci.state <- {ci.state with cap_info = []}; (\* See 6.2.1 in RFC 3501 *\) *)
(*         IO.return () *)
(*       end *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

let authenticate auth =
  let step data =
    let step data = try ret (auth.ImapAuth.step data) with _ -> fail Auth_error in
    step (ImapUtils.base64_decode data) >>= fun (rc, data) ->
    let data = ImapUtils.base64_encode data in
    ImapSend.(raw data >> crlf) >>
    flush >>
    ret rc
  in
  let auth_sender =
    let open ImapSend in
    next_tag >>= fun tag ->
    raw tag >> char ' ' >> raw "AUTHENTICATE" >> char ' ' >> string auth.ImapAuth.name >> crlf
  in
  auth_sender >>
  flush >>
  let rec loop needs_more =
    let p =
      let open ImapParser in
      alt (continue_req >>= fun data -> ret (`More data)) (response >>= fun r -> ret (`Done r))
    in
    liftP p >>= function
    | `More data ->
        let data = match data with CONTINUE_REQ_BASE64 data -> data | CONTINUE_REQ_TEXT _ -> "" in
        step data >>= fun needs_more ->
        loop (match needs_more with `OK -> false | `NEEDS_MORE -> true)
    | `Done r ->
        if needs_more then
          step "" >>= function
          | `OK -> ret r
          | `NEEDS_MORE -> fail Auth_error
        else
          ret r
  in
  loop true >>= fun r ->
  modify (fun s -> response_store s r) >>
  handle_response r

let login user pass =
  std_command
    (ImapSend.(raw "LOGIN" >> char ' ' >> string user >> char ' ' >> string pass))
  
(* let compress s = *)
(*   let ci = connection_info s in *)
(*   let cmd = S.raw "COMPRESS DEFLATE" in *)
(*   let aux () = *)
(*     send_command ci cmd >>= fun () -> *)
(*     let chan = IO.compress ci.chan in *)
(*     ci.chan <- chan;       *)
(*     ci.compress_deflate <- true; *)
(*     IO.return () *)
(*   in *)
(*   IO.with_lock ci.send_lock aux *)

let examine mbox =
  ImapCondstore.examine mbox

let select mbox =
  ImapCondstore.select mbox

let create mbox =
  std_command
    (ImapSend.(raw "CREATE" >> char ' ' >> mailbox mbox))
 
let delete mbox =
  std_command
    (ImapSend.(raw "DELETE" >> char ' ' >> mailbox mbox))
 
let rename oldbox newbox =
  std_command
    (ImapSend.(raw "RENAME" >> char ' ' >> mailbox oldbox >> char ' ' >> mailbox newbox))
 
let subscribe mbox =
  std_command
    (ImapSend.(raw "SUBSCRIBE" >> char ' ' >> mailbox mbox))
 
let unsubscribe mbox =
  std_command
    (ImapSend.(raw "UNSUBCRIBE" >> char ' ' >> mailbox mbox))
 
let list mbox list_mb =
  std_command (ImapSend.(raw "LIST" >> char ' ' >> mailbox mbox >> char ' ' >> mailbox list_mb)) >>
  gets (fun s -> s.rsp_info.rsp_mailbox_list)

let lsub mbox list_mb =
  std_command
    (ImapSend.(raw "LSUB" >> char ' ' >> mailbox mbox >> char ' ' >> mailbox list_mb)) >>
  gets (fun s -> s.rsp_info.rsp_mailbox_list)

let status mbox attrs =
  std_command
    (ImapSend.(raw "STATUS" >> char ' ' >> mailbox mbox >> char ' ' >> list status_att attrs)) >>
  gets (fun s -> s.rsp_info.rsp_status)

let copy_aux cmd set destbox =
  let sender =
    let open ImapSend in
    raw cmd >> char ' ' >> message_set set >> char ' ' >> mailbox destbox
  in
  ImapCore.std_command sender

let copy set destbox =
  copy_aux "COPY" set destbox

let uid_copy set destbox =
  copy_aux "UID COPY" set destbox

let append mbox ?flags ?date_time:dt data =
  let sender =
    let open ImapSend in
    let flags = match flags with None | Some [] -> ret () | Some flags -> list flag flags >> char ' ' in
    let date = match dt with None -> ret () | Some dt -> date_time dt >> char ' ' in
    raw "APPEND" >> space >> mailbox mbox >> char ' ' >> flags >> date >> literal data
  in
  std_command sender

let search =
  ImapCondstore.search

let uid_search =
  ImapCondstore.uid_search

(* let namespace s = *)
(*   assert false *)
(* (\* let ci = connection_info s in *\) *)
(* (\* let cmd = S.raw "NAMESPACE" in *\) *)
(* (\* let aux () = *\) *)
(* (\*   send_command ci cmd >>= fun () -> *\) *)
(* (\*   IO.return ci.state.rsp_info.rsp_namespace *\) *)
(* (\* in *\) *)
(* (\* IO.with_lock ci.send_lock aux *\) *)

let check =
  std_command (ImapSend.(raw "CHECK"))

let close =
  std_command (ImapSend.(raw "CLOSE"))

let expunge =
  std_command (ImapSend.(raw "EXPUNGE"))

let fetch =
  ImapCondstore.fetch

let uid_fetch =
  ImapCondstore.uid_fetch
