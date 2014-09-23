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

module Condstore = struct
  open ImapExtension
  (* resp-text-code   =/ "HIGHESTMODSEQ" SP mod-sequence-value / *)
  (*                     "NOMODSEQ" / *)
  (*                     "MODIFIED" SP set *)
  type condstore_resptextcode =
      CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ of Uint64.t
    | CONDSTORE_RESPTEXTCODE_NOMODSEQ
    | CONDSTORE_RESPTEXTCODE_MODIFIED of ImapSet.t
       
  type extension_data +=
       CONDSTORE_FETCH_DATA_MODSEQ of Uint64.t
     | CONDSTORE_RESP_TEXT_CODE of condstore_resptextcode
     | CONDSTORE_SEARCH_DATA of Uint32.t list * Uint64.t
     | CONDSTORE_STATUS_INFO_HIGHESTMODSEQ of Uint64.t

  let fetch_att_modseq = FETCH_ATT_EXTENSION "MODSEQ"

  let condstore_printer =
    let open Format in
    let condstore_resptextcode_print ppf = function
        CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ n ->
          fprintf ppf "(highest-mod-seq %s)" (Uint64.to_string n)
      | CONDSTORE_RESPTEXTCODE_NOMODSEQ ->
          fprintf ppf "(no-mod-seq)"
      | CONDSTORE_RESPTEXTCODE_MODIFIED ns ->
          fprintf ppf "(modified@ ?)"
  in
  function
    CONDSTORE_FETCH_DATA_MODSEQ n ->
      Some (fun ppf -> fprintf ppf "(mod-seq %s)" (Uint64.to_string n))
  | CONDSTORE_RESP_TEXT_CODE r ->
      Some (fun ppf -> condstore_resptextcode_print ppf r)
  | CONDSTORE_SEARCH_DATA (ns, n) ->
      let loop ppf = function
          [] -> ()
        | [x] -> fprintf ppf "%s" (Uint32.to_string x)
        | x :: xs ->
            fprintf ppf "%s" (Uint32.to_string x);
            List.iter (fun x -> fprintf ppf "@ %s" (Uint32.to_string x)) xs
      in
      Some (fun ppf -> fprintf ppf "@[<2>(%a@ (mod-seq %s))@]" loop ns (Uint64.to_string n))
  | CONDSTORE_STATUS_INFO_HIGHESTMODSEQ n ->
      Some (fun ppf -> fprintf ppf "(highest-mod-seq %s)" (Uint64.to_string n))
  | _ ->
      None

  (* [RFC 4551]
  mod-sequence-value  = 1*DIGIT
                            ;; Positive unsigned 64-bit integer
                            ;; (mod-sequence)
                            ;; (1 <= n < 18,446,744,073,709,551,615)
  *)

  (*
  search-sort-mod-seq = "(" "MODSEQ" SP mod-sequence-value ")"
  *)

  (*
  permsg-modsequence  = mod-sequence-value
                            ;; per message mod-sequence
  *)
  let condstore_parse =
    let open ImapParser in
    let mod_sequence_value =
      accum (function '0' .. '9' -> true | _ -> false) >>= fun s ->
      try ret (Uint64.of_string s) with _ -> fail
    in
    let search_sort_mod_seq =
      char '(' >> str "MODSEQ" >> char ' ' >> mod_sequence_value >>= fun x -> char ')' >> ret x
    in
    let permsg_modsequence = mod_sequence_value in
    let condstore_resptextcode =
      let highestmodseq =
        str "HIGHESTMODSEQ" >> char ' ' >> mod_sequence_value >>= fun modseq ->
        ret (CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ modseq)
      in
      let nomodseq = str "NOMODSEQ" >> ret CONDSTORE_RESPTEXTCODE_NOMODSEQ in
      let modified =
        str "MODIFIED" >> char ' ' >> sequence_set >>= fun set ->
        ret (CONDSTORE_RESPTEXTCODE_MODIFIED set)
      in
      altn [ highestmodseq; nomodseq; modified ]
    in
    function
      EXTENDED_PARSER_RESP_TEXT_CODE ->
        condstore_resptextcode >>= fun r ->
        ret (CONDSTORE_RESP_TEXT_CODE r)
    | EXTENDED_PARSER_FETCH_DATA ->
(* fetch-mod-resp      = "MODSEQ" SP "(" permsg-modsequence ")" *)
        str "MODSEQ" >> char ' ' >> char '(' >> permsg_modsequence >>= fun m -> char ')' >>
        ret (CONDSTORE_FETCH_DATA_MODSEQ m)
(* status-att          =/ "HIGHESTMODSEQ" *)
(*                           ;; extends non-terminal defined in RFC 3501. *)
    | EXTENDED_PARSER_STATUS_ATT ->
(* msg-att-dynamic     =/ fetch-mod-resp *)
        str "HIGHESTMODSEQ" >> char ' ' >> mod_sequence_value >>= fun n ->
        ret (CONDSTORE_STATUS_INFO_HIGHESTMODSEQ n)
    | EXTENDED_PARSER_MAILBOX_DATA ->
(* mailbox-data        =/ "SEARCH" [1*(SP nz-number) SP *)
(*                           search-sort-mod-seq] *)
        str "SEARCH" >> char ' ' >>
        opt (rep1 (char ' ' >> nz_number) >>= fun ns ->
             char ' ' >> search_sort_mod_seq >>= fun m -> ret (ns, m)) ([], Uint64.zero) >>=
        fun (ns, m) ->
        ret (CONDSTORE_SEARCH_DATA (ns, m))
    | _ ->
        fail

  let search_modseq_aux cmd ?charset key =
    let sender =
      let open ImapSend in
      raw cmd >>
      begin
        match charset with
          Some charset when search_key_need_to_send_charset key ->
            char ' ' >> string charset
        | _ ->
            ret ()
      end >>
      char ' ' >>
      search_key key
    in
    let cmd_handler s =
      let rec loop = function
          [] ->
            let res = s.rsp_info.rsp_search_results in
            (* modify (fun s -> {s with rsp_info = {s.rsp_info with rsp_search_results = []}}) >> *)
          (* ret (res, Uint64.zero) *)
          (* FIXME *)
          (res, Uint64.zero)
        | CONDSTORE_SEARCH_DATA (res, m) :: _ ->
          (* ret (res, m) *)
            (res, m)
        | _ :: rest ->
            loop rest
      in
      loop s.rsp_info.rsp_extension_list
    in
    ImapCore.std_command sender >> gets cmd_handler

  let search_modseq ?charset key =
    search_modseq_aux "SEARCH" ?charset key

  let uid_search_modseq ?charset key =
    search_modseq_aux "UID SEARCH" ?charset key

  let search ?charset key =
    search_modseq_aux "SEARCH" ?charset key >>= fun (res, _) -> ret res

  let uid_search ?charset key =
    search_modseq_aux "UID SEARCH" ?charset key >>= fun (res, _) -> ret res

  let select_condstore_optional mb cmd use_condstore =
    let cmd_sender =
      let open ImapSend in
      raw cmd >> char ' ' >> mailbox mb >> if use_condstore then raw " (CONDSTORE)" else ret ()
    in
    let cmd_handler s =
      let rec loop = function
          [] ->
            Uint64.zero
        | CONDSTORE_RESP_TEXT_CODE (CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ m) :: _ ->
            m
        | CONDSTORE_RESP_TEXT_CODE CONDSTORE_RESPTEXTCODE_NOMODSEQ :: _ ->
            Uint64.zero
        | _ :: rest ->
            loop rest
      in
      loop s.rsp_info.rsp_extension_list
    in
    modify (fun s -> {s with sel_info = ImapCore.fresh_selection_info}) >>
    ImapCore.std_command cmd_sender >> gets cmd_handler

  let select_condstore mb =
    select_condstore_optional mb "SELECT" true
      
  let select mb =
    select_condstore_optional mb "SELECT" false >> ret ()

  let examine_condstore mb =
    select_condstore_optional mb "EXAMINE" true

  let examine mb =
    select_condstore_optional mb "EXAMINE" false >> ret ()

  let fetch_aux cmd set changedsince attrs =
    let cmd =
      let open ImapSend in
      let changedsince = match changedsince with
          None ->
            ret ()
        | Some modseq ->
            char ' ' >> raw "(CHANGEDSINCE " >> raw (Uint64.to_string modseq) >> char ')'
      in
      raw cmd >> char ' ' >> message_set set >> char ' ' >> fetch_type attrs >> changedsince
    in
    let cmd_handler s = s.rsp_info.rsp_fetch_list in
    ImapCore.std_command cmd >> gets cmd_handler

  let fetch set attrs =
    fetch_aux "FETCH" set None attrs
      
  let uid_fetch set attrs =
    fetch_aux "UID FETCH" set None attrs

  let fetch_changedsince set modseq attrs =
    fetch_aux "FETCH" set (Some modseq) attrs

  let uid_fetch_changedsince set modseq attrs =
    fetch_aux "UID FETCH" set (Some modseq) attrs

  let store_aux cmd set unchangedsince flags =
    let sender =
      let open ImapSend in
      let unchangedsince = match unchangedsince with
          None -> ret ()
        | Some modseq -> raw "(UNCHANGEDSINCE " >> raw (Uint64.to_string modseq) >> raw ") "
      in
      raw cmd >> char ' ' >> message_set set >> char ' ' >> unchangedsince >> store_att_flags flags
    in
    let handler s =
      let rec loop = function
          [] -> ImapSet.empty
        | CONDSTORE_RESP_TEXT_CODE (CONDSTORE_RESPTEXTCODE_MODIFIED uids) :: _ ->
            uids
        | _ :: rest ->
            loop rest
    in
    loop s.rsp_info.rsp_extension_list
  in
  ImapCore.std_command sender >> gets handler

  let store set flags =
    store_aux "STORE" set None flags >> ret ()

  let uid_store set flags =
    store_aux "UID STORE" set None flags >> ret ()

  let store_unchangedsince set unchangedsince flags =
    store_aux "STORE" set (Some unchangedsince) flags

  let uid_store_unchangedsince set unchangedsince flags =
    store_aux "UID STORE" set (Some unchangedsince) flags
    
  let _ =
    register_extension {ext_parser = condstore_parse; ext_printer = condstore_printer}
end

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
    send_tag >> char ' ' >> raw "AUTHENTICATE" >> char ' ' >> string auth.ImapAuth.name >> crlf
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
  Condstore.examine mbox

let select mbox =
  Condstore.select mbox

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
  Condstore.search

let uid_search =
  Condstore.uid_search

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
  Condstore.fetch

let uid_fetch =
  Condstore.uid_fetch

let store =
  Condstore.store

let uid_store =
  Condstore.uid_store
