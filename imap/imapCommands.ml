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

(*
capability          =/ "QRESYNC"

select-param        =  "QRESYNC" SP "(" uidvalidity SP
                       mod-sequence-value [SP known-uids]
                       [SP seq-match-data] ")"
;; conforms to the generic select-param
;; syntax defined in [IMAPABNF]

seq-match-data      =  "(" known-sequence-set SP known-uid-set ")"

uidvalidity         =  nz-number

known-uids          =  sequence-set
;; sequence of UIDs, "*" is not allowed

known-sequence-set  =  sequence-set
;; set of message numbers corresponding to
;; the UIDs in known-uid-set, in ascending order.
;; * is not allowed.

known-uid-set       =  sequence-set
;; set of UIDs corresponding to the messages in
;; known-sequence-set, in ascending order.
;; * is not allowed.

message-data        =/ expunged-resp

expunged-resp       =  "VANISHED" [SP "(EARLIER)"] SP known-uids

rexpunges-fetch-mod =  "VANISHED"
;; VANISHED UID FETCH modifier conforms
;; to the fetch-modifier syntax
;; defined in [IMAPABNF].  It is only
;; allowed in the UID FETCH command.

resp-text-code      =/ "CLOSED"
*)

module QResync = struct
  (* This are handled by the Condstore extension, below *)
  type resp_text_code_extension +=
       RESP_TEXT_CODE_HIGHESTMODSEQ of Uint64.t
     | RESP_TEXT_CODE_NOMODSEQ
     | RESP_TEXT_CODE_MODIFIED of ImapSet.t

  type resp_text_code_extension +=
       RESP_TEXT_CODE_CLOSED

  type qresync_vanished =
    { qr_earlier : bool;
      qr_known_uids : ImapSet.t }

  type response_data_extension +=
       RESP_DATA_VANISHED of qresync_vanished

  let parse : type a. a extension_kind -> a ImapParser.t = fun kind ->
    let open ImapParser in
    match kind with
      RESPONSE_DATA ->
        str "VANISHED" >> char ' ' >>
        alt (str "(EARLIER) " >> ret true) (ret false) >>= fun qr_earlier ->
        sequence_set >>= fun qr_known_uids -> ret (RESP_DATA_VANISHED {qr_earlier; qr_known_uids})
    | RESP_TEXT_CODE ->
        str "CLOSED" >> ret RESP_TEXT_CODE_CLOSED
    | _ ->
        fail

  let _ =
    ImapParser.(register_parser {parse})

  let send_select_qresync mb uidvalidity modseq ?known_uids ?seq_match_data () =
    let open ImapSend in
    let send_seq_match_data (x, y) = char '(' >> message_set x >> char ' ' >> message_set y >> char ')' in
    send "SELECT" >> char ' ' >> mailbox mb >> char ' ' >>
    send "QRESYNC" >> char ' ' >> char '(' >>
    send (Uint32.to_string uidvalidity) >> char ' ' >>
    send (Uint64.to_string modseq) >>
    opt message_set known_uids >>
    opt send_seq_match_data seq_match_data >> char ')'

  let get_vanished st =
    let rec loop = function
        [] ->
          {qr_earlier = false; qr_known_uids = ImapSet.empty}
      | EXTENSION_DATA (RESPONSE_DATA, RESP_DATA_VANISHED qr) :: _ ->
          qr
      | _ :: rest ->
          loop rest
    in
    loop st.rsp_info.rsp_extension_list

  let get_mod_sequence_value st =
    let rec loop = function
        [] ->
          Uint64.zero
      | EXTENSION_DATA (RESP_TEXT_CODE, RESP_TEXT_CODE_HIGHESTMODSEQ n) :: _ ->
          n
      | EXTENSION_DATA (RESP_TEXT_CODE, RESP_TEXT_CODE_NOMODSEQ) :: _ ->
          Uint64.zero
      | _ :: rest ->
          loop rest
    in
    loop st.rsp_info.rsp_extension_list

  let select_qresync mb uidvalidity modseq ?known_uids ?seq_match_data () =
    std_command (send_select_qresync mb uidvalidity modseq ?known_uids ?seq_match_data ()) >>
    gets get_vanished >>= fun vanished ->
    gets get_mod_sequence_value >>= fun mod_sequence_value ->
    gets (fun st -> st.rsp_info.rsp_fetch_list) >>= fun result ->
    ret (result, mod_sequence_value, vanished)

  let send_fetch cmd set ?changedsince ft =
    let open ImapSend in
    let send_changedsince modseq =
      char ' ' >> send "(CHANGEDSINCE " >> send (Uint64.to_string modseq) >> char ')'
    in
    raw cmd >> char ' ' >> message_set set >> char ' ' >>
    fetch_type ft >> opt send_changedsince changedsince

  let send_fetch_param mod_sequence vanished =
    let open ImapSend in
    if mod_sequence = Uint64.zero then
      ret ()
    else
      char ' ' >> char '(' >> send "CHANGEDSINCE" >>
      char ' ' >> send (Uint64.to_string mod_sequence) >>
      (if vanished then char ' ' >> send "VANISHED" else ret ()) >>
      char ')'

  let fetch_qresync_vanished_aux cmd set fetch_type mod_sequence vanished =
    std_command (send_fetch cmd set fetch_type >> send_fetch_param mod_sequence vanished) >>
    gets get_vanished >>= fun vanished ->
    gets (fun st -> st.rsp_info.rsp_fetch_list) >>= fun result ->
    ret (result, vanished)

  let fetch_qresync_vanished =
    fetch_qresync_vanished_aux "FETCH"

  let uid_fetch_qresync_vanished =
    fetch_qresync_vanished_aux "UID FETCH"

  let fetch_qresync set fetch_type mod_sequence =
    fetch_qresync_vanished_aux "FETCH" set fetch_type mod_sequence true

  let uid_fetch_qresync set fetch_type mod_sequence =
    fetch_qresync_vanished_aux "UID FETCH" set fetch_type mod_sequence true
end

(*
capability          =/ "CONDSTORE"

status-att          =/ "HIGHESTMODSEQ"
                       ;; extends non-terminal defined in RFC 3501.

status-att-val      =/ "HIGHESTMODSEQ" SP mod-sequence-valzer
                       ;; extends non-terminal defined in [IMAPABNF].
                       ;; Value 0 denotes that the mailbox doesn't
                       ;; support persistent mod-sequences
                       ;; as described in Section 3.1.2

store-modifier      =/ "UNCHANGEDSINCE" SP mod-sequence-valzer
                       ;; Only a single "UNCHANGEDSINCE" may be
                       ;; specified in a STORE operation

fetch-modifier      =/ chgsince-fetch-mod
                       ;; conforms to the generic "fetch-modifier"
                       ;; syntax defined in [IMAPABNF].

chgsince-fetch-mod  = "CHANGEDSINCE" SP mod-sequence-value
                       ;; CHANGEDSINCE FETCH modifier conforms to
                       ;; the fetch-modifier syntax

fetch-att           =/ fetch-mod-sequence
                       ;; modifies original IMAP4 fetch-att

fetch-mod-sequence  = "MODSEQ"

fetch-mod-resp      = "MODSEQ" SP "(" permsg-modsequence ")"

msg-att-dynamic     =/ fetch-mod-resp

search-key          =/ search-modsequence
                          ;; modifies original IMAP4 search-key
                          ;;
                          ;; This change applies to all commands
                          ;; referencing this non-terminal, in
                          ;; particular SEARCH.

search-modsequence  = "MODSEQ" [search-modseq-ext] SP
                      mod-sequence-valzer

search-modseq-ext   = SP entry-name SP entry-type-req

resp-text-code      =/ "HIGHESTMODSEQ" SP mod-sequence-value /
                       "NOMODSEQ" /
                       "MODIFIED" SP set

entry-name          = entry-flag-name

entry-flag-name     = DQUOTE "/flags/" attr-flag DQUOTE
                       ;; each system or user defined flag <flag>
                       ;; is mapped to "/flags/<flag>".
                       ;;
                       ;; <entry-flag-name> follows the escape rules
                       ;; used by "quoted" string as described in
                       ;; Section 4.3 of [IMAP4], e.g., for the flag
                       ;; \Seen the corresponding <entry-name> is
                       ;; "/flags/\\seen", and for the flag
                       ;; $MDNSent, the corresponding <entry-name>
                       ;; is "/flags/$mdnsent".

entry-type-resp     = "priv" / "shared"
                       ;; metadata item type

entry-type-req      = entry-type-resp / "all"
                       ;; perform SEARCH operation on private
                       ;; metadata item, shared metadata item or both

permsg-modsequence  = mod-sequence-value
                       ;; per message mod-sequence

mod-sequence-value  = 1*DIGIT
                       ;; Positive unsigned 64-bit integer
                       ;; (mod-sequence)
                       ;; (1 <= n < 18,446,744,073,709,551,615)

mod-sequence-valzer = "0" / mod-sequence-value

search-sort-mod-seq = "(" "MODSEQ" SP mod-sequence-value ")"

select-param        =/ condstore-param
                          ;; conforms to the generic "select-param"
                          ;; non-terminal syntax defined in [IMAPABNF].

condstore-param     = "CONDSTORE"

mailbox-data        =/ "SEARCH" [1*(SP nz-number) SP
                       search-sort-mod-seq]

attr-flag           = "\\Answered" / "\\Flagged" / "\\Deleted" /
                      "\\Seen" / "\\Draft" / attr-flag-keyword /
                      attr-flag-extension
                       ;; Does not include "\\Recent"

attr-flag-extension = "\\" atom
                       ;; Future expansion.  Client implementations
                       ;; MUST accept flag-extension flags.  Server
                       ;; implementations MUST NOT generate
                       ;; flag-extension flags except as defined by
                       ;; future standard or standards-track
                       ;; revisions of [IMAP4].

attr-flag-keyword   = atom
*)

module Condstore = struct
  type msg_att_extension +=
       MSG_ATT_MODSEQ of Uint64.t

  type resp_text_code_extension +=
       RESP_TEXT_CODE_HIGHESTMODSEQ of Uint64.t
     | RESP_TEXT_CODE_NOMODSEQ
     | RESP_TEXT_CODE_MODIFIED of ImapSet.t

  type status_info_extension +=
       STATUS_ATT_HIGHESTMODSEQ of Uint64.t

  type mailbox_data_extension +=
       MAILBOX_DATA_SEARCH of Uint32.t list * Uint64.t

  let fetch_att_modseq = FETCH_ATT_EXTENSION "MODSEQ"

  let condstore_printer : type a. a extension_kind -> a -> (Format.formatter -> unit) option =
    let open Format in
    function
      FETCH_DATA ->
        begin
          function
            MSG_ATT_MODSEQ n ->
              Some (fun ppf -> fprintf ppf "(mod-seq %s)" (Uint64.to_string n))
          | _ ->
              None
        end
    | RESP_TEXT_CODE ->
        begin
          function
            RESP_TEXT_CODE_HIGHESTMODSEQ n ->
              Some (fun ppf -> fprintf ppf "(highest-mod-seq %s)" (Uint64.to_string n))
          | RESP_TEXT_CODE_NOMODSEQ ->
              Some (fun ppf -> fprintf ppf "(no-mod-seq)")
          | RESP_TEXT_CODE_MODIFIED ns ->
              Some (fun ppf -> fprintf ppf "(modified@ ?)")
          | _ ->
              None
        end
    | MAILBOX_DATA ->
        begin
          function
            MAILBOX_DATA_SEARCH (ns, n) ->
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
            STATUS_ATT_HIGHESTMODSEQ n ->
              Some (fun ppf -> fprintf ppf "(highest-mod-seq %s)" (Uint64.to_string n))
          | _ ->
              None
        end
    | _ ->
        function _ -> None

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
      ret (RESP_TEXT_CODE_HIGHESTMODSEQ modseq)
    in
    let nomodseq = str "NOMODSEQ" >> ret RESP_TEXT_CODE_NOMODSEQ in
    let modified =
      str "MODIFIED" >> char ' ' >> sequence_set >>= fun set -> ret (RESP_TEXT_CODE_MODIFIED set)
    in
    match kind with
      RESP_TEXT_CODE ->
        altn [ highestmodseq; nomodseq; modified ]
    | FETCH_DATA ->
        str "MODSEQ" >> char ' ' >> char '(' >> permsg_modsequence >>= fun m -> char ')' >>
        ret (MSG_ATT_MODSEQ m)
    | STATUS_ATT ->
        str "HIGHESTMODSEQ" >> char ' ' >> mod_sequence_value >>= fun n ->
        ret (STATUS_ATT_HIGHESTMODSEQ n)
    | MAILBOX_DATA ->
        str "SEARCH" >> char ' ' >>
        opt (rep1 (char ' ' >> nz_number) >>= fun ns ->
             char ' ' >> search_sort_mod_seq >>= fun m -> ret (ns, m)) ([], Uint64.zero) >>=
        fun (ns, m) ->
        ret (MAILBOX_DATA_SEARCH (ns, m))
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
        | EXTENSION_DATA (MAILBOX_DATA, MAILBOX_DATA_SEARCH (res, m)) :: _ ->
            (res, m)
        | _ :: rest ->
            loop rest
      in
      loop s.rsp_info.rsp_extension_list
    in
    std_command sender >> gets cmd_handler

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
        | EXTENSION_DATA (RESP_TEXT_CODE, RESP_TEXT_CODE_HIGHESTMODSEQ m) :: _ ->
            m
        | EXTENSION_DATA (RESP_TEXT_CODE, RESP_TEXT_CODE_NOMODSEQ) :: _ ->
            Uint64.zero
        | _ :: rest ->
            loop rest
      in
      loop s.rsp_info.rsp_extension_list
    in
    modify (fun s -> s.sel_info <- fresh_selection_info ()) >>
    std_command cmd_sender >> gets cmd_handler

  let select_condstore mb =
    select_condstore_optional mb "SELECT" true

  let select mb =
    select_condstore_optional mb "SELECT" false >> ret ()

  let examine_condstore mb =
    select_condstore_optional mb "EXAMINE" true

  let examine mb =
    select_condstore_optional mb "EXAMINE" false >> ret ()

  let fetch set fetch_type =
    QResync.fetch_qresync_vanished set fetch_type Uint64.zero false >|= fst

  let uid_fetch set fetch_type =
    QResync.uid_fetch_qresync_vanished set fetch_type Uint64.zero false >|= fst

  let fetch_changedsince set modseq fetch_type =
    QResync.fetch_qresync_vanished set fetch_type modseq false >|= fst

  let uid_fetch_changedsince set modseq fetch_type =
    QResync.uid_fetch_qresync_vanished set fetch_type modseq false >|= fst

  let store_aux cmd set unchangedsince flags =
    let sender =
      let open ImapSend in
      let send_unchangedsince modseq =
        send "(UNCHANGEDSINCE " >> send (Uint64.to_string modseq) >> send ") "
      in
      send cmd >> char ' ' >> message_set set >> char ' ' >>
      opt send_unchangedsince unchangedsince >> store_att_flags flags
    in
    let handler s =
      let rec loop = function
          [] ->
            ImapSet.empty
        | EXTENSION_DATA (RESP_TEXT_CODE, RESP_TEXT_CODE_MODIFIED uids) :: _ ->
            uids
        | _ :: rest ->
            loop rest
    in
    loop s.rsp_info.rsp_extension_list
  in
  std_command sender >> gets handler

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

let starttls =
  std_command (ImapSend.raw "STARTTLS") >> modify (fun st -> st.cap_info <- [])

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
  std_command sender

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
       RESP_DATA_ID of (string * string option) list

  let id_printer : type a. a extension_kind -> a -> _ option =
    let open Format in
    function
      RESPONSE_DATA ->
        begin
          function
            RESP_DATA_ID params ->
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
        ret (RESP_DATA_ID params_list)
    | _ ->
        fail

  let id_handler s =
    let rec loop =
      function
        [] -> []
      | EXTENSION_DATA (RESPONSE_DATA, RESP_DATA_ID params) :: _ ->
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

(*
append-uid      = uniqueid

capability      =/ "UIDPLUS"

command-select  =/ uid-expunge

resp-code-apnd  = "APPENDUID" SP nz-number SP append-uid

resp-code-copy  = "COPYUID" SP nz-number SP uid-set SP uid-set

resp-text-code  =/ resp-code-apnd / resp-code-copy / "UIDNOTSTICKY"
                  ; incorporated before the expansion rule of
                  ;  atom [SP 1*<any TEXT-CHAR except "]">]
                  ; that appears in [IMAP]

uid-expunge     = "UID" SP "EXPUNGE" SP sequence-set

uid-set         = (uniqueid / uid-range) *("," uid-set)

uid-range       = (uniqueid ":" uniqueid)
                  ; two uniqueid values and all values
                  ; between these two regards of order.
                  ; Example: 2:4 and 4:2 are equivalent.

Servers that support [MULTIAPPEND] will have the following extension
to the above rules:

append-uid      =/ uid-set
                  ; only permitted if client uses [MULTIAPPEND]
                  ; to append multiple messages.
*)

module Uidplus = struct
  type resp_text_code_extension +=
       RESP_TEXT_CODE_APPENDUID of Uint32.t * ImapSet.t
     | RESP_TEXT_CODE_COPYUID of Uint32.t * ImapSet.t * ImapSet.t
     | RESP_TEXT_CODE_UIDNOTSTICKY

  let uidplus_printer : type a. a extension_kind -> a -> _ option =
    let open Format in
    function
      RESP_TEXT_CODE ->
        begin
          function
            RESP_TEXT_CODE_APPENDUID (uid1, uid2) ->
              Some (fun ppf -> fprintf ppf "@[<2>(uidplus-append %s ?)@]" (Uint32.to_string uid1)) (* FIXME *)
          | RESP_TEXT_CODE_COPYUID (uid, uidset1, uidset2) ->
              Some (fun ppf -> fprintf ppf "@[<2>(uidplus-copy %s ?)@]" (Uint32.to_string uid))
          | RESP_TEXT_CODE_UIDNOTSTICKY ->
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
      ret (RESP_TEXT_CODE_APPENDUID (uid, ImapSet.single uid2))
    in
    let resp_code_copy =
      str "COPYUID" >>
      char ' ' >>
      nz_number >>= fun uidvalidity ->
      char ' ' >>
      uid_set >>= fun src_uids ->
      char ' ' >>
      uid_set >>= fun dst_uids ->
      ret (RESP_TEXT_CODE_COPYUID (uidvalidity, src_uids, dst_uids))
    in
    let resp_code_uidnotsticky =
      str "UIDNOTSTICKY" >> ret RESP_TEXT_CODE_UIDNOTSTICKY
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
    std_command sender

  let extract_copy_uid s =
    let rec loop =
      function
        [] ->
          Uint32.zero, ImapSet.empty, ImapSet.empty
      | EXTENSION_DATA (RESP_TEXT_CODE, RESP_TEXT_CODE_COPYUID (uid, src, dst)) :: _ ->
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
      | EXTENSION_DATA (RESP_TEXT_CODE, RESP_TEXT_CODE_APPENDUID (uid, set)) :: _ ->
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

module XGmExt1 = struct
  type msg_att_extension +=
       MSG_ATT_XGMMSGID of Uint64.t
     | MSG_ATT_XGMTHRID of Uint64.t
     | MSG_ATT_XGMLABELS of string list

  let fetch_att_xgmmsgid = FETCH_ATT_EXTENSION "X-GM-MSGID"

  let fetch_att_xgmthrid = FETCH_ATT_EXTENSION "X-GM-THRID"

  let fetch_att_xgmlabels = FETCH_ATT_EXTENSION "X-GM-LABELS"

  let xgmext1_printer : type a. a extension_kind -> a -> _ option =
    let open Format in
    function
      FETCH_DATA ->
        begin
          function
            MSG_ATT_XGMMSGID id ->
              Some (fun ppf -> fprintf ppf "(x-gm-msgid %s)" (Uint64.to_string id))
          | MSG_ATT_XGMTHRID id ->
              Some (fun ppf -> fprintf ppf "(x-gm-thrid %s)" (Uint64.to_string id))
          | MSG_ATT_XGMLABELS labels ->
              let p ppf = List.iter (fun x -> fprintf ppf "@ %S" x) in
              Some (fun ppf -> fprintf ppf "@[<2>(x-gm-labels%a)@]" p labels)
          | _ ->
              None
        end
    | _ ->
        function _ -> None

  let xgmext1_parser : type a. a extension_kind -> a ImapParser.t = fun kind ->
    let open ImapParser in
    let msgid =
      str "X-GM-MSGID" >> char ' ' >> uint64 >>= fun n ->
      ret (MSG_ATT_XGMMSGID n)
    in
    let thrid =
      str "X-GM-THRID" >> char ' ' >> uint64 >>= fun n ->
      ret (MSG_ATT_XGMTHRID n)
    in
    let labels =
      str "X-GM-LABELS" >> char ' ' >>
      char '(' >> sep (char ' ') astring >>= fun labels -> char ')' >>
      ret (MSG_ATT_XGMLABELS labels)
    in
    match kind with
      FETCH_DATA ->
        altn [ msgid; thrid; labels ]
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
    std_command sender

  let store_xgmlabels =
    store_xgmlabels_aux "STORE"

  let uid_store_xgmlabels =
    store_xgmlabels_aux "UID STORE"

  let _ =
    ImapPrint.(register_printer {print = xgmext1_printer});
    ImapParser.(register_parser {parse = xgmext1_parser})
end

(*
atom = <atom>
   ; <atom> as defined in [RFC-2060]

Namespace = nil / "(" 1*( "(" string SP  (<"> QUOTED_CHAR <"> /
   nil) *(Namespace_Response_Extension) ")" ) ")"

Namespace_Command = "NAMESPACE"

Namespace_Response_Extension = SP string SP "(" string *(SP string)
   ")"

Namespace_Response = "*" SP "NAMESPACE" SP Namespace SP Namespace SP
   Namespace

   ; The first Namespace is the Personal Namespace(s)
   ; The second Namespace is the Other Users' Namespace(s)
   ; The third Namespace is the Shared Namespace(s)

   nil = <nil>
      ; <nil> as defined in [RFC-2060]

   QUOTED_CHAR = <QUOTED_CHAR>
      ; <QUOTED_CHAR> as defined in [RFC-2060]

   string = <string>
      ; <string> as defined in [RFC-2060]
      ; Note that  the namespace prefix is to a mailbox and following
      ; IMAP4 convention, any international string in the NAMESPACE
      ; response MUST be of modified UTF-7 format as described in
      ;  [RFC-2060].
*)

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
       RESP_DATA_NAMESPACE of namespace_data

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
        ret (RESP_DATA_NAMESPACE {ns_personal; ns_other; ns_shared})
    | _ ->
        fail
  let extract st =
    let rec loop = function
        [] ->
          fail ExtensionError
      | EXTENSION_DATA (RESPONSE_DATA, RESP_DATA_NAMESPACE ns) :: _ ->
          ret ns
      | _ :: rest ->
          loop rest
    in
    loop st.rsp_info.rsp_extension_list
  let namespace = std_command (send "NAMESPACE") >> get >>= extract
  let _ =
    ImapParser.(register_parser {parse})
end

(*
command-auth =/ compress

compress    = "COMPRESS" SP algorithm

capability  =/ "COMPRESS=" algorithm
              ;; multiple COMPRESS capabilities allowed

algorithm   = "DEFLATE"

resp-text-code =/ "COMPRESSIONACTIVE"
*)

module Compress = struct
  type resp_text_code_extension +=
       RESP_TEXT_CODE_COMPRESSIONACTIVE

  let parse : type a. a extension_kind -> a ImapParser.t = fun kind ->
    let open ImapParser in
    match kind with
      RESP_TEXT_CODE ->
        str "COMPRESSIONACTIVE" >> ret RESP_TEXT_CODE_COMPRESSIONACTIVE
    | _ ->
        fail

  let compress = std_command (send "COMPRESS DEFLATE")

  let _ =
    ImapParser.(register_parser {parse})
end

module Idle = struct
  let idle_done =
    ImapSend.(raw "DONE" >> crlf) >> flush >>
    liftP ImapParser.response >>= fun r ->
    modify (fun s -> response_store s r) >>
    handle_response r

  let idle_start =
    modify (fun st -> st.sel_info.sel_exists <- None; st.sel_info.sel_recent <- None) >>
    ImapSend.(send_tag >> char ' ' >> send "IDLE" >> crlf) >>
    flush >>
    liftP ImapParser.(rep_ response_data) >>
    liftP ImapParser.continue_req >>
    ret ()
end
