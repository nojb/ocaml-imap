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

let _ =
  Lwt_log.default := Lwt_log.channel ~template:"$(message)" ~close_mode:`Keep ~channel:Lwt_io.stderr ();
  Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Debug

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let _ =
  prerr_endline "Initialising SSL...";
  Ssl.init ()

type state =
  | DISCONNECTED
  | CONNECTED
  | LOGGEDIN
  | SELECTED

type session = {
  mutable imap_state : ImapTypes.state;
  mutable sock : Lwt_ssl.socket option;
  mutable state : state;
  mutable current_folder : string option;
  mutable uid_next : Uint32.t;
  mutable uid_validity : Uint32.t;
  mutable mod_sequence_value : Uint64.t;
  mutable folder_msg_count : int option;
  mutable first_unseen_uid : Uint32.t;
  mutable condstore_enabled : bool;
  username : string;
  password : string;
  host : string;
  port : int;
  mutable should_disconnect : bool;
  mut : Lwt_mutex.t;
  mutable num_ops : int
}

let use s f =
  lwt () = Lwt_mutex.lock s.mut in
  try_lwt
    s.num_ops <- s.num_ops + 1; f ()
  finally
    s.num_ops <- s.num_ops - 1;
    Lwt_mutex.unlock s.mut;
    Lwt.return ()

let operations_count s =
  s.num_ops

exception ErrorP of error

let _ =
  Printexc.register_printer
    (function
      | ErrorP err ->
          Some (ImapCore.string_of_error err)
      | _ ->
          None)

let fully_write sock buf pos len =
  lwt sock =
    match sock with
    | Some sock -> Lwt.return sock
    | None -> assert_lwt false
  in
  let rec loop pos len =
    if len <= 0 then
      Lwt.return ()
    else
      lwt n = Lwt_ssl.write sock buf pos len in
      loop (pos + n) (len - n)
  in
  loop pos len

let read sock buf pos len =
  lwt sock =
    match sock with
    | Some sock -> Lwt.return sock
    | None -> assert_lwt false
  in
  Lwt_ssl.read sock buf pos len

let run s c =
  let open ImapControl in
  let buf = Bytes.create 65536 in
  let rec loop in_buf = function
    | Ok (x, st) ->
        s.imap_state <- st;
        Lwt.return x
    | Fail (err, st) ->
        s.imap_state <- st;
        Lwt.fail (ErrorP err)
    | Flush (str, k) ->
        lwt () = Lwt_log.debug_f ">>>>\n%s>>>>\n" str in
        lwt () = fully_write s.sock str 0 (String.length str) in
        loop in_buf (k ())
    | Need k ->
        match_lwt read s.sock buf 0 (String.length buf) with
        | 0 ->
            loop in_buf (k End)
        | _ as n ->
            lwt () = Lwt_log.debug_f "<<<<\n%s<<<<\n" (String.sub buf 0 n) in
            Buffer.add_substring in_buf buf 0 n;
            loop in_buf (k More)
  in
  loop s.imap_state.in_buf (run c s.imap_state)
  
(* let ssl_context ssl_method ca_file = *)
(*   let version = *)
(*     match version with *)
(*       `TLSv1 -> Ssl.TLSv1 *)
(*     | `SSLv23 -> Ssl.SSLv23 *)
(*     | `SSLv3 -> Ssl.SSLv3 *)
(*   in *)
(*   let ctx = Ssl.create_context ssl_method Ssl.Client_context in *)
(*   match ca_file with *)
(*     None -> *)
(*       ctx *)
(*   | Some ca_file -> *)
(*       Ssl.load_verify_locations ctx ca_file ""; *)
(*       Ssl.set_verify ctx [Ssl.Verify_peer] None; *)
(*       ctx *)

let create_session ?(port=993) host username password =
  {imap_state = ImapCore.fresh_state;
   sock = None;
   state = DISCONNECTED;
   current_folder = None;
   uid_next = Uint32.zero;
   uid_validity = Uint32.zero;
   mod_sequence_value = Uint64.zero;
   folder_msg_count = None;
   first_unseen_uid = Uint32.zero;
   condstore_enabled = false;
   username;
   password;
   host;
   port;
   should_disconnect = false;
   mut = Lwt_mutex.create ();
   num_ops = 0}

type folder_flag =
    Marked
  | Unmarked
  | NoSelect
  | NoInferiors
  | Inbox
  | SentMail
  | Starred
  | AllMail
  | Trash
  | Drafts
  | Spam
  | Important
  | Archive

type message_flag =
    Seen
  | Answered
  | Flagged
  | Deleted
  | Draft
  | MDNSent
  | Forwarded
  | SubmitPending
  | Submitted

type messages_request_kind =
    Uid
  | Flags
  | Headers
  | Structure
  | InternalDate
  | FullHeaders
  | HeaderSubject
  | GmailLabels
  | GmailMessageID
  | GmailThreadID
  | ExtraHeaders of string list
  | Size

type fetch_request_type =
    UID
  | Sequence

type flags_request_kind =
    Add
  | Remove
  | Set

type workaround =
    Gmail
  | Yahoo
  | Exchange2003

type auth_capability =
    Anonymous
  | CRAMMD5
  | DigestMD5
  | External
  | GSSAPI
  | KerberosV4
  | Login
  | NTLM
  | OTP
  | Plain
  | SKey
  | SRP

type capability =
    ACL
  | Binary
  | Catenate
  | Children
  | CompressDeflate
  | Condstore
  | Enable
  | Idle
  | Id
  | LiteralPlus
  | MultiAppend
  | Namespace
  | QResync
  | Quote
  | Sort
  | StartTLS
  | ThreadORderedSubject
  | ThreadReferences
  | UIDPlus
  | Unselect
  | XList
  | Auth of auth_capability
  | XOAuth2
  | Gmail

type encoding =
    Bit7
  | Bit8
  | Binary
  | Base64
  | QuotedPrintable
  | Other
  | UUEncode

type error =
    Connection
  | TLSNotAvailable
  | Parse
  | Certificate
  | Authentication
  | GmailIMAPNotEnabled
  | GmailExceededBandwidthLimit
  | GmailTooManySimultaneousConnections
  | MobileMeMoved
  | YahooUnavailable
  | NonExistantFolder
  | Rename
  | Delete
  | Create
  | Subscribe
  | Append
  | Copy
  | Expunge
  | Fetch
  | Idle
  | Identity
  | Namespace
  | Store
  | Capability
  | StartTLSNotAvailable
  | SendMessageIllegalAttachment
  | StorageLimit
  | SendMessageNotAllowed
  | NeedsConnectToWebmail
  | SendMessage
  | AuthenticationRequired
  | FetchMessageList
  | DeleteMessage
  | InvalidAccount
  | File
  | Compression
  | NoSender
  | NoRecipient
  | Noop

type folder_status = {
  unseen_count : int;
  message_count : int;
  recent_count : int;
  uid_next : Uint32.t;
  uid_validity : Uint32.t;
  highest_mod_seq_value : Uint64.t
}

type address = {
  display_name : string;
  mailbox : string;
}

type header = {
  message_id : string;
  references : string list;
  in_reply_to : string list;
  sender : address;
  from : address;
  to_ : address list;
  cc : address list;
  bcc : address list;
  reply_to : address list;
  subject : string
}

type message = {
  uid : Uint32.t;
  size : int;
  mod_seq_value : Uint64.t;
  gmail_labels : string list;
  gmail_message_id : Uint64.t;
  gmail_thread_id : Uint64.t;
  flags : message_flag list;
  internal_date : float
}

exception Error of error

let connect ?(ssl_method = Ssl.TLSv1) s =
  lwt () = assert_lwt (s.state = DISCONNECTED) in
  try_lwt
    let he = Unix.gethostbyname s.host in
    let sa = Unix.ADDR_INET (he.Unix.h_addr_list.(0), s.port) in
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect fd sa in
    let context = Ssl.create_context ssl_method Ssl.Client_context in
    lwt sock = Lwt_ssl.ssl_connect fd context in
    s.sock <- Some sock;
    lwt _ = run s ImapCore.greeting in
    s.state <- CONNECTED;
    Lwt.return ()
  with
  | _ -> Lwt.fail (Error Connection)

let disconnect s =
  try_lwt
    match s.sock with
    | Some sock ->
        lwt () = Lwt_ssl.close sock in
        s.sock <- None;
        s.state <- DISCONNECTED;
        Lwt.return ()
    | None ->
        Lwt.return ()
  with
  | _ -> Lwt.return ()
      
let connect_if_needed s =
  lwt () =
    if s.should_disconnect then
      disconnect s
    else
      Lwt.return ()
  in
  match s.state with
    DISCONNECTED ->
      connect s
  | _ ->
      Lwt.return ()

let login s =
  lwt () =
    try_lwt
      run s (ImapCommands.login s.username s.password)
    with
      exn ->
        lwt () = Lwt_log.debug ~exn "login error" in
        match exn with
          ErrorP (ParseError _) -> Lwt.fail (Error Parse)
        | _ -> Lwt.fail (Error Authentication)
  in
  s.state <- LOGGEDIN;
  Lwt.return ()

let login_if_needed s =
  lwt () = connect_if_needed s in
  match s.state with
  | CONNECTED ->
      login s
  | _ ->
      Lwt.return ()

let get_mod_sequence_value state =
  let open ImapCondstore in
  let rec loop = function
    | [] -> Uint64.zero
    | CONDSTORE_RESP_TEXT_CODE (CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ n) :: _ -> n
    | CONDSTORE_RESP_TEXT_CODE CONDSTORE_RESPTEXTCODE_NOMODSEQ :: _ -> Uint64.zero
    | _ :: rest -> loop rest
  in
  loop state.rsp_info.rsp_extension_list

let select s folder =
  lwt () = assert_lwt (s.state = LOGGEDIN || s.state = SELECTED) in
  try_lwt
    lwt _ = run s (ImapCommands.select folder) in
    s.uid_next <- s.imap_state.sel_info.sel_uidnext;
    s.uid_validity <- s.imap_state.sel_info.sel_uidvalidity;
    s.state <- SELECTED;
    s.current_folder <- Some folder;
    s.folder_msg_count <- s.imap_state.sel_info.sel_exists;
    s.first_unseen_uid <- s.imap_state.sel_info.sel_first_unseen;
    s.mod_sequence_value <- get_mod_sequence_value s.imap_state;
    Lwt.return ()
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "select error" in
      match exn with
        ErrorP (ParseError _) ->
          Lwt.fail (Error Parse)
      | _ ->
          s.state <- LOGGEDIN;
          Lwt.fail (Error NonExistantFolder)

let select_if_needed s folder =
  lwt () = login_if_needed s in
  match s.state with
  | SELECTED ->
      lwt current_folder = (* FIXME *)
        match s.current_folder with
        | Some f -> Lwt.return f
        | None -> assert_lwt false
      in
      if String.lowercase current_folder <> String.lowercase folder then
        select s folder
      else
        Lwt.return ()
  | LOGGEDIN ->
      select s folder
  | _ ->
      Lwt.return () (* FIXME assert_lwt false *)

let folder_status s folder =
  lwt () = login_if_needed s in
  let status_att_list : status_att list =
    STATUS_ATT_UNSEEN :: STATUS_ATT_MESSAGES :: STATUS_ATT_RECENT ::
    STATUS_ATT_UIDNEXT :: STATUS_ATT_UIDVALIDITY ::
    (if s.condstore_enabled then STATUS_ATT_HIGHESTMODSEQ :: [] else [])
  in
  try_lwt
    lwt status = run s (ImapCommands.status folder status_att_list) in
    let fs = {unseen_count = 0;
              message_count = 0;
              recent_count = 0;
              uid_next = Uint32.zero;
              uid_validity = Uint32.zero;
              highest_mod_seq_value = Uint64.zero}
    in
    let rec loop fs = function
      | [] ->
          fs
      | STATUS_ATT_MESSAGES message_count :: rest ->
          loop {fs with message_count} rest
      | STATUS_ATT_RECENT recent_count :: rest ->
          loop {fs with recent_count} rest
      | STATUS_ATT_UIDNEXT uid_next :: rest ->
          loop {fs with uid_next} rest
      | STATUS_ATT_UIDVALIDITY uid_validity :: rest ->
          loop {fs with uid_validity} rest
      | STATUS_ATT_UNSEEN unseen_count :: rest ->
          loop {fs with unseen_count} rest
      | STATUS_ATT_HIGHESTMODSEQ highest_mod_seq_value :: rest ->
          loop {fs with highest_mod_seq_value} rest
      | _ :: rest ->
          loop fs rest
    in
    Lwt.return (loop fs status.st_info_list)
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "status error" in
      match exn with
        ErrorP (ParseError _) ->
          Lwt.fail (Error Parse)
      | _ ->
          Lwt.fail (Error NonExistantFolder)

let rename_folder s folder other_name =
  lwt () = select_if_needed s "INBOX" in
  try_lwt
    run s (ImapCommands.rename folder other_name)
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "rename error" in
      match exn with
        ErrorP (ParseError _) ->
          Lwt.fail (Error Parse)
      | _ ->
          Lwt.fail (Error Rename)

let delete_folder s ~folder =

  lwt () = select_if_needed s "INBOX" in

  try_lwt

    run s (ImapCommands.delete folder)

  with
    exn ->
      lwt () = Lwt_log.debug ~exn "delete error" in
      match exn with
        ErrorP (ParseError _) ->
          Lwt.fail (Error Parse)
      | _ ->
          Lwt.fail (Error Delete)

let create_folder s ~folder =

  lwt () = select_if_needed s "INBOX" in

  try_lwt

    run s (ImapCommands.create folder)

  with
    exn ->
      lwt () = Lwt_log.debug ~exn "create error" in
      match exn with
        ErrorP (ParseError _) ->
          Lwt.fail (Error Parse)
      | _ ->
          Lwt.fail (Error Create)

let flags_from_lep_att_dynamic att_list =
  let rec loop acc = function
      [] ->
        List.rev acc
    | FLAG_FETCH_OTHER flag :: rest ->
        begin
          match flag with
            FLAG_ANSWERED -> loop (Answered :: acc) rest
          | FLAG_FLAGGED  -> loop (Flagged :: acc) rest
          | FLAG_DELETED  -> loop (Deleted :: acc) rest
          | FLAG_SEEN     -> loop (Seen :: acc) rest
          | FLAG_DRAFT    -> loop (Draft :: acc) rest
          | FLAG_KEYWORD "$Forwarded"     -> loop (Forwarded :: acc) rest
          | FLAG_KEYWORD "$MDNSent"       -> loop (MDNSent :: acc) rest
          | FLAG_KEYWORD "$SubmitPending" -> loop (SubmitPending :: acc) rest
          | FLAG_KEYWORD "$Submitted"     -> loop (Submitted :: acc) rest
          | _ -> loop acc rest
        end
    | _ :: rest ->
        loop acc rest
  in
  loop [] att_list

(* let is_known_custom_flag = function *)
(*   | "$MDNSent" *)
(*   | "$Forwarded" *)
(*   | "$SubmitPending" *)
(*   | "$Submitted" -> true *)
(*   | _ -> false *)

(* let custom_flags_from_lep_att_dynamic att_list = *)
(*   let rec loop = function *)
(*     | [] -> [] *)
(*     | FLAG_FETCH_OTHER (FLAG_KEYWORD kw) :: rest when not (is_known_custom_flag kw) -> *)
(*         kw :: loop rest *)
(*     | _ :: rest -> *)
(*         loop rest *)
(*   in *)
(*   loop att_list *)

let header_from_imap env =
  assert false

let fetch_messages s ~folder ~request_kind ~fetch_by_uid ~imapset =
  
  lwt () = select_if_needed s folder in
  
  (* let needs_body = ref false in *)
  
  let fetch_atts, headers =
    let rec loop acc headers = function
        [] ->
          acc, headers
      | Uid :: rest ->
          loop (FETCH_ATT_UID :: acc) headers rest
      | Flags :: rest ->
          loop (FETCH_ATT_FLAGS :: acc) headers rest
      | GmailLabels :: rest ->
          loop (ImapXgmlabels.fetch_att_xgmlabels :: acc) headers rest
      | GmailThreadID :: rest ->
          failwith "fetch gmail thread id not implemented"
      | GmailMessageID :: rest ->
          loop (ImapXgmmsgid.fetch_att_xgmmsgid :: acc) headers rest
      | FullHeaders :: rest ->
          loop acc
            ("Date" :: "Subject" :: "From" :: "Sender" :: "Reply-To" ::
             "To" :: "Cc" :: "Message-ID" :: "References" :: "In-Reply-To" :: headers)
            rest
      | Headers :: rest ->
          loop (FETCH_ATT_ENVELOPE :: acc) ("References" :: headers) rest
      | HeaderSubject :: rest ->
          loop (FETCH_ATT_ENVELOPE :: acc) ("References" :: "Subject" :: headers) rest
      | Size :: rest ->
          loop (FETCH_ATT_RFC822_SIZE :: acc) headers rest
      | Structure :: rest ->
          loop (FETCH_ATT_BODYSTRUCTURE :: acc) headers rest
      | InternalDate :: rest ->
          loop (FETCH_ATT_INTERNALDATE :: acc) headers rest
      | ExtraHeaders extra :: rest ->
          loop acc (extra @ headers) rest
    in
    loop [] [] request_kind
  in
  
  let needs_headers = List.length headers > 0 in
  
  let fetch_atts =
    if needs_headers then
      FETCH_ATT_BODY_PEEK_SECTION
        (Some (SECTION_SPEC_SECTION_MSGTEXT (SECTION_MSGTEXT_HEADER_FIELDS headers)), None) :: fetch_atts
    else
      fetch_atts
  in

  let rec loop msg = function
      [] ->
        msg
    | MSG_ATT_ITEM_DYNAMIC flags :: rest ->
        let flags = flags_from_lep_att_dynamic flags in
        loop {msg with flags} rest
    | MSG_ATT_ITEM_STATIC (MSG_ATT_UID uid) :: rest ->
        loop {msg with uid} rest
    | MSG_ATT_ITEM_STATIC (MSG_ATT_RFC822_SIZE size) :: rest ->
        loop {msg with size} rest
    | MSG_ATT_ITEM_STATIC (MSG_ATT_INTERNALDATE dt) :: rest ->
        let tm = (* http://stackoverflow.com/a/12353013 *)
          {Unix.tm_year = dt.dt_year - 1900;
           tm_mon = dt.dt_month - 1;
           tm_mday = dt.dt_day;
           tm_hour = dt.dt_hour;
           tm_min = dt.dt_min;
           tm_sec = dt.dt_sec;
           tm_wday = 0;
           tm_yday = 0;
           tm_isdst = false}
        in
        let t = ImapUtils.timegm tm in
        let zone_hour = if dt.dt_zone >= 0 then dt.dt_zone / 100 else -(-dt.dt_zone / 100) in
        let zone_min = if dt.dt_zone >= 0 then dt.dt_zone mod 100 else -(-dt.dt_zone mod 100) in
        let internal_date = t -. float zone_hour *. 3600. -. float zone_min *. 60. in
        loop {msg with internal_date} rest
    (* | MSG_ATT_ITEM_STATIC (MSG_ATT_ENVELOPE env) :: rest -> *)
    (* loop {msg with header = header_from_imap env} rest *)
    | MSG_ATT_ITEM_EXTENSION (ImapCondstore.CONDSTORE_FETCH_DATA_MODSEQ mod_seq_value) :: rest ->
        loop {msg with mod_seq_value} rest
    | MSG_ATT_ITEM_EXTENSION (ImapXgmlabels.XGMLABELS_XGMLABELS gmail_labels) :: rest ->
        loop {msg with gmail_labels} rest
    | MSG_ATT_ITEM_EXTENSION (ImapXgmmsgid.XGMMSGID_MSGID gmail_message_id) :: rest ->
        loop {msg with gmail_message_id} rest
    | _ :: rest -> (* FIXME *)
        loop msg rest
  in
  
  let msg =
    loop {uid = Uint32.zero; size = 0; mod_seq_value = Uint64.zero;
          gmail_labels = []; gmail_message_id = Uint64.zero; gmail_thread_id = Uint64.zero;
          flags = []; internal_date = 0.0}
  in

  let fetch_type = FETCH_TYPE_FETCH_ATT_LIST fetch_atts in
  
  if fetch_by_uid then
    lwt result = run s (ImapCommands.uid_fetch imapset fetch_type) in
    Lwt.return (List.map (fun (atts, _) -> msg atts) result)
  else
    lwt result = run s (ImapCommands.fetch imapset fetch_type) in
    Lwt.return (List.map (fun (atts, _) -> msg atts) result)

let fetch_message_by_uid s folder uid =
  lwt () = select_if_needed s folder in
  try_lwt
    let fetch_type = FETCH_TYPE_FETCH_ATT (FETCH_ATT_BODY_PEEK_SECTION (None, None)) in
    match_lwt run s (ImapCommands.uid_fetch (ImapSet.single uid) fetch_type) with
    | (result, _) :: [] ->
        let rec loop = function
            [] ->
              assert false
          | MSG_ATT_ITEM_STATIC (MSG_ATT_BODY_SECTION {sec_body_part}) :: _ ->
              sec_body_part
          | _ :: rest ->
              loop rest
        in
        Lwt.return (loop result)
    | _ ->
        assert_lwt false
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "fetch error" in
      match exn with
        ErrorP (ParseError _) ->
          Lwt.fail (Error Parse)
      | _ ->
          Lwt.fail (Error Fetch)

let fetch_number_uid_mapping s ~folder ~from_uid ~to_uid =
  
  lwt () = select_if_needed s folder in

  let result = Hashtbl.create 0 in

  let imap_set = ImapSet.interval from_uid to_uid in

  let fetch_type = FETCH_TYPE_FETCH_ATT FETCH_ATT_UID in

  try_lwt
    
    lwt fetch_result = run s (ImapCommands.uid_fetch imap_set fetch_type) in

    let rec loop = function
        [] ->
          ()
      | (att_item, att_number) :: rest ->
          let rec loop1 = function
              [] ->
                loop rest
            | MSG_ATT_ITEM_STATIC (MSG_ATT_UID uid) :: _ ->
                if Uint32.compare uid from_uid >= 0 then
                  Hashtbl.add result att_number uid;
                loop rest
            | _ :: rest ->
                loop1 rest
          in
          loop1 att_item
    in
    loop fetch_result;

    Lwt.return result
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "fetch error" in
      match exn with
        ErrorP (ParseError _) ->
          Lwt.fail (Error Parse)
      (* | StreamError -> *)
      (* Lwt.fail (Error Connection) *)
      | _ ->
          Lwt.fail (Error Fetch)
  
let capability s =
  try_lwt
    lwt caps = run s ImapCommands.capability in
    let rec loop acc = function
        [] ->
          List.rev acc
      | CAPABILITY_NAME name :: rest ->
          begin match String.uppercase name with
              "STARTTLS"         -> loop (StartTLS :: acc) rest
            | "ID"               -> loop (Id :: acc) rest
            | "XLIST"            -> loop (XList :: acc) rest
            | "X-GM-EXT-1"       -> loop (Gmail :: acc) rest
            | "IDLE"             -> loop (Idle :: acc) rest
            | "CONDSTORE"        -> loop (Condstore :: acc) rest
            | "QRESYNC"          -> loop (QResync :: acc) rest
            | "XOAUTH2"          -> loop (XOAuth2 :: acc) rest
            | "COMPRESS=DEFLATE" -> loop (CompressDeflate :: acc) rest
            | "NAMESPACE"        -> loop (Namespace :: acc) rest
            | "CHILDREN"         -> loop (Children :: acc) rest
            | _                  -> loop acc rest
          end
      | CAPABILITY_AUTH_TYPE name :: rest ->
          begin match String.uppercase name with
            | "PLAIN" -> loop (Auth Plain :: acc) rest
            | "LOGIN" -> loop (Auth Login :: acc) rest
            | _ -> loop acc rest
          end
    in
    Lwt.return (loop [] caps)
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "capability error" in
      match exn with
        ErrorP (ParseError _) -> Lwt.fail (Error Parse)
      | _ -> Lwt.fail (Error Capability)

let enable_feature s feature =
  try_lwt
    lwt _ = run s (ImapEnable.enable [CAPABILITY_NAME feature]) in
    Lwt.return true
  with
  | _ -> Lwt.return false

(* let uid_next s = *)
(*   s.uid_next *)

(* let uid_validity s = *)
(*   s.uid_validity *)

(* let mod_sequence_value s = *)
(*   s.mod_sequence_value *)
