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
open ImapCore
open ImapControl

module Condstore = struct
  (* open ImapExtension *)
  (* resp-text-code   =/ "HIGHESTMODSEQ" SP mod-sequence-value / *)
  (*                     "NOMODSEQ" / *)
  (*                     "MODIFIED" SP set *)
  type msg_att_extension +=
       CONDSTORE_FETCH_DATA_MODSEQ of Uint64.t

  type resp_text_code_extension +=
       CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ of Uint64.t
     | CONDSTORE_RESPTEXTCODE_NOMODSEQ
     | CONDSTORE_RESPTEXTCODE_MODIFIED of ImapSet.t

  type status_info_extension +=
       CONDSTORE_STATUS_INFO_HIGHESTMODSEQ of Uint64.t
  
  type mailbox_data_extension +=
       CONDSTORE_SEARCH_DATA of Uint32.t list * Uint64.t
     
  let fetch_att_modseq = FETCH_ATT_EXTENSION "MODSEQ"

  let condstore_printer : type a. a extension_kind -> a -> (Format.formatter -> unit) option =
    let open Format in
    function
      FETCH_DATA ->
        begin
          function
            CONDSTORE_FETCH_DATA_MODSEQ n ->
              Some (fun ppf -> fprintf ppf "(mod-seq %s)" (Uint64.to_string n))
          | _ ->
              None
        end
    | RESP_TEXT_CODE ->
        begin
          function
            CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ n ->
              Some (fun ppf -> fprintf ppf "(highest-mod-seq %s)" (Uint64.to_string n))
          | CONDSTORE_RESPTEXTCODE_NOMODSEQ ->
              Some (fun ppf -> fprintf ppf "(no-mod-seq)")
          | CONDSTORE_RESPTEXTCODE_MODIFIED ns ->
              Some (fun ppf -> fprintf ppf "(modified@ ?)")
          | _ ->
              None
        end
    | MAILBOX_DATA ->
        begin
          function
            CONDSTORE_SEARCH_DATA (ns, n) ->
              let loop ppf = function
                  [] -> ()
                | [x] -> fprintf ppf "%s" (Uint32.to_string x)
                | x :: xs ->
                    fprintf ppf "%s" (Uint32.to_string x);
                    List.iter (fun x -> fprintf ppf "@ %s" (Uint32.to_string x)) xs
              in
              Some (fun ppf -> fprintf ppf "@[<2>(%a@ (mod-seq %s))@]" loop ns (Uint64.to_string n))
          | _ ->
              None
        end
    | STATUS_ATT ->
        begin
          function
            CONDSTORE_STATUS_INFO_HIGHESTMODSEQ n ->
              Some (fun ppf -> fprintf ppf "(highest-mod-seq %s)" (Uint64.to_string n))
          | _ ->
              None
        end
    | _ ->
        function _ -> None

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
  let condstore_parse : type a. a extension_kind -> a ImapParser.t = fun kind ->
    let open ImapParser in
    let mod_sequence_value =
      accum (function '0' .. '9' -> true | _ -> false) >>= fun s ->
      try ret (Uint64.of_string s) with _ -> fail
    in
    let search_sort_mod_seq =
      char '(' >> str "MODSEQ" >> char ' ' >> mod_sequence_value >>= fun x -> char ')' >> ret x
    in
    let permsg_modsequence = mod_sequence_value in
    let highestmodseq =
      str "HIGHESTMODSEQ" >> char ' ' >> mod_sequence_value >>= fun modseq ->
      ret (CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ modseq)
    in
    let nomodseq = str "NOMODSEQ" >> ret CONDSTORE_RESPTEXTCODE_NOMODSEQ in
    let modified =
      str "MODIFIED" >> char ' ' >> sequence_set >>= fun set ->
      ret (CONDSTORE_RESPTEXTCODE_MODIFIED set)
    in
    match kind with
      RESP_TEXT_CODE ->
        altn [ highestmodseq; nomodseq; modified ]
    | FETCH_DATA ->
(* fetch-mod-resp      = "MODSEQ" SP "(" permsg-modsequence ")" *)
        str "MODSEQ" >> char ' ' >> char '(' >> permsg_modsequence >>= fun m -> char ')' >>
        ret (CONDSTORE_FETCH_DATA_MODSEQ m)
(* status-att          =/ "HIGHESTMODSEQ" *)
(*                           ;; extends non-terminal defined in RFC 3501. *)
    | STATUS_ATT ->
(* msg-att-dynamic     =/ fetch-mod-resp *)
        str "HIGHESTMODSEQ" >> char ' ' >> mod_sequence_value >>= fun n ->
        ret (CONDSTORE_STATUS_INFO_HIGHESTMODSEQ n)
    | MAILBOX_DATA ->
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
        | EXTENSION_DATA (MAILBOX_DATA, CONDSTORE_SEARCH_DATA (res, m)) :: _ ->
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
        | EXTENSION_DATA (RESP_TEXT_CODE, CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ m) :: _ ->
            m
        | EXTENSION_DATA (RESP_TEXT_CODE, CONDSTORE_RESPTEXTCODE_NOMODSEQ) :: _ ->
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
        | EXTENSION_DATA (RESP_TEXT_CODE, CONDSTORE_RESPTEXTCODE_MODIFIED uids) :: _ ->
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
    ImapPrint.(register_printer {print = condstore_printer});
    ImapParser.(register_parser {parse = condstore_parse})
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

module Enable = struct
  type response_data_extension +=
       EXTENSION_ENABLE of capability list

(*
response-data =/ "*" SP enable-data CRLF
enable-data   = "ENABLED" *(SP capability)
*)
  let enable_parser : type a. a extension_kind -> a ImapParser.t =
    let open ImapParser in
    function
      RESPONSE_DATA ->
        str "ENABLED" >>
        rep (char ' ' >> capability) >>= fun caps ->
        ret (EXTENSION_ENABLE caps)
    | _ ->
        fail

  let enable_printer : type a. a extension_kind -> a -> _ option =
    let open Format in
    let open ImapPrint in
    function
      RESPONSE_DATA ->
        begin
          function
            EXTENSION_ENABLE caps ->
              let p ppf = List.iter (fun x -> fprintf ppf "@ %a" capability_print x) in
              Some (fun ppf -> fprintf ppf "@[<2>(enabled%a)@]" p caps)
          | _ ->
              None
        end
    | _ ->
        function _ -> None

  let enable_sender caps =
    let open ImapSend in
    let send_capability =
      function
        CAPABILITY_AUTH_TYPE t ->
          raw "AUTH=" >> raw t
      | CAPABILITY_NAME t ->
          raw t
    in
    raw "ENABLE" >> char ' ' >> separated (char ' ') send_capability caps

  let enable_handler s =
    let rec loop =
      function
        [] -> []
      | EXTENSION_DATA (RESPONSE_DATA, EXTENSION_ENABLE caps) :: _ -> caps
      | _ :: rest -> loop rest
    in
    loop s.rsp_info.rsp_extension_list

  let enable caps =
    std_command (enable_sender caps) >> gets enable_handler

  let _ =
    ImapParser.(register_parser {parse = enable_parser});
    ImapPrint.(register_printer {print = enable_printer})
end

module Id = struct
  type response_data_extension +=
       ID_PARAMS of (string * string option) list

  let id_printer : type a. a extension_kind -> a -> _ option =
    let open Format in
    function
      RESPONSE_DATA ->
        begin
          function
            ID_PARAMS params ->
              let p ppf =
                List.iter (function (k, None) -> fprintf ppf "@ (%s nil)" k
                                  | (k, Some v) -> fprintf ppf "@ (%s %S)" k v)
              in
              Some (fun ppf -> fprintf ppf "@[<2>(id%a)@]" p params)
          | _ ->
              None
        end
    | _ ->
        function _ -> None

  let id_sender params =
    let open ImapSend in
    let param_sender =
      function
        (k, None) ->
          string k >> char ' ' >> nil
      | (k, Some v) ->
          string k >> char ' ' >> string v
    in
    raw "ID" >> char ' ' >>
    match params with
      [] -> nil
    | xs -> list param_sender params

  let id_parser : type a. a extension_kind -> a ImapParser.t = fun kind ->
    let open ImapParser in
    let id_params =
      astring >>= fun k ->
      char ' ' >>
      nstring >>= fun v ->
      ret (k, v)
    in
    match kind with
      RESPONSE_DATA ->
        str "ID" >> char ' ' >>
        char '(' >> sep (char ' ') id_params >>= fun params_list -> char ')' >>
        ret (ID_PARAMS params_list)
    | _ ->
        fail

  let id_handler s =
    let rec loop =
      function
        [] -> []
      | EXTENSION_DATA (RESPONSE_DATA, ID_PARAMS params) :: _ ->
          params
      | _ :: rest ->
          loop rest
    in
    loop s.rsp_info.rsp_extension_list

  let id params =
    std_command (id_sender params) >> gets id_handler

  let id_basic name version =
    let get l n =
      try
        snd (List.find (fun (k, _) -> String.lowercase k = String.lowercase n) l)
      with
        Not_found -> None
    in
    let cmd = id ["name", Some name; "version", Some version] in
    cmd >>= fun params ->
    ret (get params "name", get params "version")

  let _ =
    ImapPrint.(register_printer {print = id_printer});
    ImapParser.(register_parser {parse = id_parser})
end

module Uidplus = struct
  (* resp-code-apnd  = "APPENDUID" SP nz-number SP append-uid *)

  (* resp-code-copy  = "COPYUID" SP nz-number SP uid-set SP uid-set *)

  (* resp-text-code  =/ resp-code-apnd / resp-code-copy / "UIDNOTSTICKY" *)
  (*                   ; incorporated before the expansion rule of *)
  (*                   ;  atom [SP 1*<any TEXT-CHAR except "]">] *)
  (*                   ; that appears in [IMAP] *)

(*
uid-set         = (uniqueid / uid-range) *("," uid-set)
*)
(*
uid-range       = (uniqueid ":" uniqueid)
                  ; two uniqueid values and all values
                  ; between these two regards of order.
                  ; Example: 2:4 and 4:2 are equivalent.
*)

  type resp_text_code_extension +=
       UIDPLUS_RESP_CODE_APND of Uint32.t * ImapSet.t
     | UIDPLUS_RESP_CODE_COPY of Uint32.t * ImapSet.t * ImapSet.t
     | UIDPLUS_RESP_CODE_UIDNOTSTICKY

  let uidplus_printer : type a. a extension_kind -> a -> _ option =
    let open Format in
    function
      RESP_TEXT_CODE ->
        begin
          function
            UIDPLUS_RESP_CODE_APND (uid1, uid2) ->
              Some (fun ppf -> fprintf ppf "@[<2>(uidplus-append %s ?)@]" (Uint32.to_string uid1)) (* FIXME *)
          | UIDPLUS_RESP_CODE_COPY (uid, uidset1, uidset2) ->
              Some (fun ppf -> fprintf ppf "@[<2>(uidplus-copy %s ?)@]" (Uint32.to_string uid))
          | UIDPLUS_RESP_CODE_UIDNOTSTICKY ->
              Some (fun ppf -> fprintf ppf "(uid-not-sticky)")
          | _ ->
              None
        end
    | _ ->
        function _ -> None

  let uidplus_parser : type a. a extension_kind -> a ImapParser.t = fun kind ->
    let open ImapParser in
    let uid_range =
      uniqueid >>= fun x ->
      char ':' >>
      uniqueid >>= fun y ->
      ret (x, y)
    in
    let uid_set =
      let elem =
        alt
          (uniqueid >>= fun id -> ret (ImapSet.single id))
          (uid_range >>= fun (x, y) -> ret (ImapSet.interval x y))
      in
      elem >>= fun x ->
      rep (char ',' >> elem) >>= fun xs ->
      ret (List.fold_left ImapSet.union x xs)
    in
    let resp_code_apnd =
      str "APPENDUID" >>
      char ' ' >>
      nz_number >>= fun uid ->
      char ' ' >>
      nz_number >>= fun uid2 ->
      ret (UIDPLUS_RESP_CODE_APND (uid, ImapSet.single uid2))
    in
    let resp_code_copy =
      str "COPYUID" >>
      char ' ' >>
      nz_number >>= fun uidvalidity ->
      char ' ' >>
      uid_set >>= fun src_uids ->
      char ' ' >>
      uid_set >>= fun dst_uids ->
      ret (UIDPLUS_RESP_CODE_COPY (uidvalidity, src_uids, dst_uids))
    in
    let resp_code_uidnotsticky =
      str "UIDNOTSTICKY" >> ret UIDPLUS_RESP_CODE_UIDNOTSTICKY
    in
    match kind with
      RESP_TEXT_CODE ->
        altn [ resp_code_apnd; resp_code_copy; resp_code_uidnotsticky ]
    | _ ->
        fail

  let uid_expunge set =
    let sender =
      let open ImapSend in
      raw "UID EXPUNGE" >> char ' ' >> message_set set
    in
    ImapCore.std_command sender

  let extract_copy_uid s =
    let rec loop =
      function
        [] ->
          Uint32.zero, ImapSet.empty, ImapSet.empty
      | EXTENSION_DATA (RESP_TEXT_CODE, UIDPLUS_RESP_CODE_COPY (uid, src, dst)) :: _ ->
          uid, src, dst
      | _ :: rest ->
          loop rest
    in
    loop s.rsp_info.rsp_extension_list

  let uidplus_copy set destbox =
    copy set destbox >> gets extract_copy_uid

  let uidplus_uid_copy set destbox =
    uid_copy set destbox >> gets extract_copy_uid

  let extract_apnd_uid s =
    let rec loop =
      function
        [] ->
          Uint32.zero, ImapSet.empty
      | EXTENSION_DATA (RESP_TEXT_CODE, UIDPLUS_RESP_CODE_APND (uid, set)) :: _ ->
          uid, set
      | _ :: rest ->
          loop rest
    in
    loop s.rsp_info.rsp_extension_list

  let extract_apnd_single_uid s =
    let uid, set = extract_apnd_uid s in
    match set with
      [] ->
        uid, Uint32.zero
    | (first, _) :: _ ->
        uid, first

  let uidplus_append mailbox ?flags ?date_time msg =
    append mailbox ?flags ?date_time msg >> gets extract_apnd_single_uid

  let _ =
    ImapParser.(register_parser {parse = uidplus_parser});
    ImapPrint.(register_printer {print = uidplus_printer})
end

module Xgmmsgid = struct
  type msg_att_extension +=
       XGMMSGID_MSGID of Uint64.t

  let fetch_att_xgmmsgid = FETCH_ATT_EXTENSION "X-GM-MSGID"

  let xgmmsgid_printer : type a. a extension_kind -> a -> _ option =
    let open Format in
    function
      FETCH_DATA ->
        begin
          function
            XGMMSGID_MSGID id ->
              Some (fun ppf -> fprintf ppf "(x-gm-msgid %s)" (Uint64.to_string id))
          | _ ->
              None
        end
    | _ ->
        function _ -> None

  let xgmmsgid_parser : type a. a extension_kind -> a ImapParser.t = fun kind ->
    let open ImapParser in
    match kind with
      FETCH_DATA ->
        str "X-GM-MSGID" >> char ' ' >> uint64 >>= fun n ->
        ret (XGMMSGID_MSGID n)
    | _ ->
        fail

  let _ =
    ImapPrint.(register_printer {print = xgmmsgid_printer});
    ImapParser.(register_parser {parse = xgmmsgid_parser})
end

module Xgmlabels = struct
  type msg_att_extension +=
       XGMLABELS_XGMLABELS of string list

  let fetch_att_xgmlabels =
    FETCH_ATT_EXTENSION "X-GM-LABELS"

  let xgmlabels_printer : type a. a extension_kind -> a -> _ option =
    let open Format in
    function
      FETCH_DATA ->
        begin
          function
            XGMLABELS_XGMLABELS labels ->
              let p ppf = List.iter (fun x -> fprintf ppf "@ %S" x) in
              Some (fun ppf -> fprintf ppf "@[<2>(x-gm-labels%a)@]" p labels)
          | _ ->
              None
        end
    | _ ->
        function _ -> None

  let xgmlabels_parser : type a. a extension_kind -> a ImapParser.t = fun kind ->
    let open ImapParser in
    match kind with
      FETCH_DATA ->
        str "X-GM-LABELS" >> char ' ' >>
        char '(' >> sep (char ' ') astring >>= fun labels -> char ')' >>
        ret (XGMLABELS_XGMLABELS labels)
    | _ ->
        fail

  let store_xgmlabels_aux cmd set fl_sign fl_silent labels =
    let sender =
      let open ImapSend in
      send cmd >> char ' ' >> message_set set >> char ' ' >>
      begin
        match fl_sign with
          STORE_ATT_FLAGS_SET -> ret ()
        | STORE_ATT_FLAGS_ADD -> char '+'
        | STORE_ATT_FLAGS_REMOVE -> char '-'
      end
      >> send "X-GM-LABELS" >> (if fl_silent then send ".SILENT" else ret ()) >>
      char ' ' >>
      list string labels
    in
    ImapCore.std_command sender

  let store_xgmlabels =
    store_xgmlabels_aux "STORE"

  let uid_store_xgmlabels =
    store_xgmlabels_aux "UID STORE"

  let _ =
    ImapPrint.(register_printer {print = xgmlabels_printer});
    ImapParser.(register_parser {parse = xgmlabels_parser})
end

module Namespace = struct
  type namespace_extension =
    string * string list
  type namespace_info =
    { ns_prefix : string;
      ns_delimiter : char option;
      ns_extensions : namespace_extension list }
  type namespace_item =
    namespace_info list
  type namespace_data =
    { ns_personal : namespace_item;
      ns_other : namespace_item;
      ns_shared : namespace_item }
  type response_data_extension +=
       NAMESPACE of namespace_data

(*
Namespace_Response_Extension = SP string SP "(" string *(SP string) ")"
*)
  
(*
Namespace = nil / "(" 1*( "(" string SP  (<"> QUOTED_CHAR <"> /
      nil) *(Namespace_Response_Extension) ")" ) ")"
*)

(*
Namespace_Response = "*" SP "NAMESPACE" SP Namespace SP Namespace SP
      Namespace

      ; The first Namespace is the Personal Namespace(s)
      ; The second Namespace is the Other Users' Namespace(s)
      ; The third Namespace is the Shared Namespace(s)
*)

  let parse : type a. a extension_kind -> a ImapParser.t = fun kind ->
    let open ImapParser in
    let namespace_extension =
      char ' ' >> imap_string >>= fun k -> char ' ' >> char '(' >>
      sep1 (char ' ') imap_string >>= fun vs -> char ')' >>
      ret (k, vs)
    in
    let namespace_item =
      char '(' >>
      rep1 (char '(' >> imap_string >>= fun ns_prefix ->
            char ' ' >> alt (some quoted_char) (nil >> ret None) >>= fun ns_delimiter ->
            rep namespace_extension >>= fun ns_extensions ->
            char ')' >>
            ret {ns_prefix; ns_delimiter; ns_extensions}) >>= fun r ->
      char ')' >>
      ret r
    in
    match kind with
      RESPONSE_DATA ->
        str "NAMESPACE" >>
        char ' ' >> namespace_item >>= fun ns_personal ->
        char ' ' >> namespace_item >>= fun ns_other ->
        char ' ' >> namespace_item >>= fun ns_shared ->
        ret (NAMESPACE {ns_personal; ns_other; ns_shared})
    | _ ->
        fail
  let extract st =
    let rec loop = function
        [] ->
          fail ExtensionError
      | EXTENSION_DATA (RESPONSE_DATA, NAMESPACE ns) :: _ ->
          ret ns
      | _ :: rest ->
          loop rest
    in
    loop st.rsp_info.rsp_extension_list
  let namespace = std_command (send "NAMESPACE") >> get >>= extract
  let _ =
    ImapParser.(register_parser {parse})
end
