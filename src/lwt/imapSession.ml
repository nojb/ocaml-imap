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

type connected_info = {
  mutable imap_state : ImapTypes.state;
  sock : Lwt_ssl.socket;
  mutable condstore_enabled : bool;
  mut : Lwt_mutex.t;
  mutable num_ops : int
}

type selected_info = {
  current_folder : string;
  mutable uid_next : Uint32.t;
  mutable uid_validity : Uint32.t;
  mutable mod_sequence_value : Uint64.t;
  mutable folder_msg_count : int option;
  mutable first_unseen_uid : Uint32.t
}

type state =
    DISCONNECTED
  | CONNECTED of connected_info
  | LOGGEDIN of connected_info
  | SELECTED of connected_info * selected_info

type session = {
  mutable state : state;
  username : string;
  password : string;
  host : string;
  port : int
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

exception StreamError

let _ =
  Printexc.register_printer
    (function
      | ErrorP err ->
          Some (ImapCore.string_of_error err)
      | StreamError ->
          Some "input/output error"
      | _ ->
          None)

let fully_write sock buf pos len =
  let rec loop pos len =
    if len <= 0 then
      Lwt.return ()
    else
      lwt n = try_lwt Lwt_ssl.write sock buf pos len with _ -> raise_lwt StreamError in
      loop (pos + n) (len - n)
  in
  loop pos len

let read sock buf pos len =
  try_lwt Lwt_ssl.read sock buf pos len with _ -> raise_lwt StreamError

let run s c =
  let open ImapControl in
  let buf = Bytes.create 65536 in
  let rec loop in_buf = function
    | Ok (x, st) ->
        s.imap_state <- st;
        Lwt.return x
    | Fail (err, st) ->
        s.imap_state <- st;
        raise_lwt (ErrorP err)
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
  {state = DISCONNECTED;
   username;
   password;
   host;
   port}
   
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
  lwt () = assert_lwt (match s.state with DISCONNECTED -> true | _ -> false) in
  try_lwt
    let he = Unix.gethostbyname s.host in
    let sa = Unix.ADDR_INET (he.Unix.h_addr_list.(0), s.port) in
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect fd sa in
    let context = Ssl.create_context ssl_method Ssl.Client_context in
    lwt sock = Lwt_ssl.ssl_connect fd context in
    let ci =
      {imap_state = ImapCore.fresh_state;
       sock;
       condstore_enabled = false;
       mut = Lwt_mutex.create ();
       num_ops = 0}
    in
    lwt _ = run ci ImapCore.greeting in
    s.state <- CONNECTED ci;
    Lwt.return ci
  with
    _ -> raise_lwt (Error Connection)

let disconnect s =
  match s.state with
    CONNECTED ci
  | LOGGEDIN ci
  | SELECTED (ci, _) ->
      lwt () = try_lwt Lwt_ssl.close ci.sock with _ -> Lwt.return () in
      s.state <- DISCONNECTED;
      Lwt.return ()
  | DISCONNECTED ->
      Lwt.return ()
      
let connect_if_needed s =
  match s.state with
    DISCONNECTED ->
      connect s
  | CONNECTED ci
  | LOGGEDIN ci
  | SELECTED (ci, _) ->
      Lwt.return ci

let login s =
  match s.state with
    CONNECTED ci ->
      lwt () =
        try_lwt
          run ci (ImapCommands.login s.username s.password)
        with
          exn ->
            lwt () = Lwt_log.debug ~exn "login error" in
            match exn with
              StreamError -> lwt () = disconnect s in raise_lwt (Error Connection)
            | ErrorP (ParseError _) -> raise_lwt (Error Parse)
            | _ -> raise_lwt (Error Authentication)
      in
      s.state <- LOGGEDIN ci;
      Lwt.return ci
  | _ ->
      assert_lwt false

let login_if_needed s =
  lwt _ = connect_if_needed s in
  match s.state with
    CONNECTED _ ->
      login s
  | LOGGEDIN ci
  | SELECTED (ci, _) ->
      Lwt.return ci
  | DISCONNECTED ->
      assert_lwt false

let get_mod_sequence_value state =
  let open ImapCondstore in
  let rec loop = function
      [] -> Uint64.zero
    | CONDSTORE_RESP_TEXT_CODE (CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ n) :: _ -> n
    | CONDSTORE_RESP_TEXT_CODE CONDSTORE_RESPTEXTCODE_NOMODSEQ :: _ -> Uint64.zero
    | _ :: rest -> loop rest
  in
  loop state.rsp_info.rsp_extension_list

let select s folder =
  match s.state with
    LOGGEDIN ci
  | SELECTED (ci, _) ->
      lwt () =
        try_lwt
          run ci (ImapCommands.select folder)
        with
          exn ->
            lwt () = Lwt_log.debug ~exn "select error" in
            match exn with
              StreamError ->
                lwt () = disconnect s in
                raise_lwt (Error Connection)
            | ErrorP (ParseError _) ->
                raise_lwt (Error Parse)
            | _ ->
                s.state <- LOGGEDIN ci;
                raise_lwt (Error NonExistantFolder)
      in
      let st = ci.imap_state in
      let si =
        {current_folder = folder;
         uid_next = st.sel_info.sel_uidnext;
         uid_validity = st.sel_info.sel_uidvalidity;
         folder_msg_count = st.sel_info.sel_exists;
         first_unseen_uid = st.sel_info.sel_first_unseen;
         mod_sequence_value = get_mod_sequence_value st}
      in
      s.state <- SELECTED (ci, si);
      Lwt.return (ci, si)
  | _ ->
      assert_lwt false

let select_if_needed s folder =
  lwt _ = login_if_needed s in
  match s.state with
    SELECTED (ci, si)->
      if String.lowercase si.current_folder <> String.lowercase folder then
        select s folder
      else
        Lwt.return (ci, si)
  | LOGGEDIN _ ->
      select s folder
  | _ ->
      assert_lwt false

let folder_status s folder =
  let status_att_list : status_att list =
    STATUS_ATT_UNSEEN :: STATUS_ATT_MESSAGES :: STATUS_ATT_RECENT ::
    STATUS_ATT_UIDNEXT :: STATUS_ATT_UIDVALIDITY :: []
  in
  let rec loop fs = function
      [] ->
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
  lwt ci = login_if_needed s in
  let status_att_list : status_att list =
    if ci.condstore_enabled then STATUS_ATT_HIGHESTMODSEQ :: status_att_list else status_att_list
  in
  lwt status =
    try_lwt
      run ci (ImapCommands.status folder status_att_list)
    with
      exn ->
        lwt () = Lwt_log.debug ~exn "status error" in
        match exn with
          StreamError ->
            lwt () = disconnect s in
            raise_lwt (Error Connection)
        | ErrorP (ParseError _) ->
            raise_lwt (Error Parse)
        | _ ->
            raise_lwt (Error NonExistantFolder)
  in
  let fs =
    {unseen_count = 0;
     message_count = 0;
     recent_count = 0;
     uid_next = Uint32.zero;
     uid_validity = Uint32.zero;
     highest_mod_seq_value = Uint64.zero}
  in
  Lwt.return (loop fs status.st_info_list)
  
let rename_folder s folder other_name =
  lwt ci, _ = select_if_needed s "INBOX" in
  try_lwt
    run ci (ImapCommands.rename folder other_name)
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "rename error" in
      match exn with
        StreamError ->
          lwt () = disconnect s in
          raise_lwt (Error Connection)
      | ErrorP (ParseError _) ->
          raise_lwt (Error Parse)
      | _ ->
          raise_lwt (Error Rename)
  
let delete_folder s ~folder =
  lwt ci, _ = select_if_needed s "INBOX" in
  try_lwt
    run ci (ImapCommands.delete folder)
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "delete error" in
      match exn with
        StreamError ->
          lwt () = disconnect s in
          raise_lwt (Error Connection)
      | ErrorP (ParseError _) ->
          raise_lwt (Error Parse)
      | _ ->
          raise_lwt (Error Delete)
  
let create_folder s ~folder =
  lwt ci, _ = select_if_needed s "INBOX" in
  try_lwt
    run ci (ImapCommands.create folder)
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "create error" in
      match exn with
        StreamError ->
          lwt () = disconnect s in
          raise_lwt (Error Connection)
      | ErrorP (ParseError _) ->
          raise_lwt (Error Parse)
      | _ ->
          raise_lwt (Error Create)

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
  lwt ci, _ = select_if_needed s folder in
  if fetch_by_uid then
    lwt result = run ci (ImapCommands.uid_fetch imapset fetch_type) in
    Lwt.return (List.map (fun (atts, _) -> msg atts) result)
  else
    lwt result = run ci (ImapCommands.fetch imapset fetch_type) in
    Lwt.return (List.map (fun (atts, _) -> msg atts) result)
  
let fetch_message_by_uid s folder uid =
  let fetch_type = FETCH_TYPE_FETCH_ATT (FETCH_ATT_BODY_PEEK_SECTION (None, None)) in
  let extract_body = function
      (result, _) :: [] ->
        let rec loop = function
            [] ->
              assert false
          | MSG_ATT_ITEM_STATIC (MSG_ATT_BODY_SECTION {sec_body_part}) :: _ ->
              sec_body_part
          | _ :: rest ->
              loop rest
        in
        loop result
    | _ ->
        assert false
  in
  lwt ci, _ = select_if_needed s folder in
  try_lwt
    lwt result = run ci (ImapCommands.uid_fetch (ImapSet.single uid) fetch_type) in
    Lwt.return (extract_body result)
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "fetch error" in
      match exn with
        StreamError ->
          lwt () = disconnect s in
          raise_lwt (Error Connection)
      | ErrorP (ParseError _) ->
          raise_lwt (Error Parse)
      | _ ->
          raise_lwt (Error Fetch)
  
let fetch_number_uid_mapping s ~folder ~from_uid ~to_uid =
  let result = Hashtbl.create 0 in
  let imap_set = ImapSet.interval from_uid to_uid in
  let fetch_type = FETCH_TYPE_FETCH_ATT FETCH_ATT_UID in
  let rec extract_uid = function
      [] ->
        ()
    | (att_item, att_number) :: rest ->
        let rec loop = function
            [] ->
              extract_uid rest
          | MSG_ATT_ITEM_STATIC (MSG_ATT_UID uid) :: _ ->
              if Uint32.compare uid from_uid >= 0 then Hashtbl.add result att_number uid;
              extract_uid rest
          | _ :: rest ->
              loop rest
        in
        loop att_item
  in
  lwt ci, _ = select_if_needed s folder in
  lwt fetch_result =
    try_lwt
      run ci (ImapCommands.uid_fetch imap_set fetch_type)
    with
      exn ->
        lwt () = Lwt_log.debug ~exn "fetch error" in
        match exn with
          StreamError ->
            lwt () = disconnect s in
            raise_lwt (Error Connection)
        | ErrorP (ParseError _) ->
            raise_lwt (Error Parse)
        | _ ->
            raise_lwt (Error Fetch)
  in
  extract_uid fetch_result;
  Lwt.return result

let store_labels s ~folder ~uids ~kind ~labels =
  let fl_sign =
    match kind with
      Add -> STORE_ATT_FLAGS_ADD
    | Remove -> STORE_ATT_FLAGS_REMOVE
    | Set -> STORE_ATT_FLAGS_SET
  in
  lwt ci, _ = select_if_needed s folder in
  try_lwt
    run ci (ImapXgmlabels.uid_store_xgmlabels uids fl_sign true labels)
  with
    exn ->
      lwt () = Lwt_log.debug ~exn "store_labels error" in
      match exn with
        StreamError ->
          lwt () = disconnect s in raise_lwt (Error Connection)
      | ErrorP (ParseError _) ->
          raise_lwt (Error Parse)
      | _ ->
          raise_lwt (Error Store)

let add_labels s ~folder ~uids ~labels =
  store_labels s ~folder ~uids ~kind:Add ~labels

let remove_labels s ~folder ~uids ~labels =
  store_labels s ~folder ~uids ~kind:Remove ~labels

let set_labels s ~folder ~uids ~labels =
  store_labels s ~folder ~uids ~kind:Set ~labels

let capability s =
  lwt ci = connect_if_needed s in
  lwt caps =
    try_lwt
      run ci ImapCommands.capability
    with
      exn ->
        lwt () = Lwt_log.debug ~exn "capability error" in
        match exn with
          StreamError ->
            lwt () = disconnect s in
            raise_lwt (Error Connection)
        | ErrorP (ParseError _) ->
            raise_lwt (Error Parse)
        | _ ->
            raise_lwt (Error Capability)
  in
  let rec loop acc = function
      [] ->
        List.rev acc
    | CAPABILITY_NAME name :: rest ->
        begin
          match String.uppercase name with
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
        begin
          match String.uppercase name with
            "PLAIN" -> loop (Auth Plain :: acc) rest
          | "LOGIN" -> loop (Auth Login :: acc) rest
          | _ -> loop acc rest
        end
  in
  Lwt.return (loop [] caps)

let enable_feature s feature =
  try_lwt
    lwt _ = run s (ImapEnable.enable [CAPABILITY_NAME feature]) in
    Lwt.return true
  with
  | _ -> Lwt.return false

let uid_next s =
  match s.state with
    SELECTED (_, si) ->
      si.uid_next
  | _ ->
      assert false

let uid_validity s =
  match s.state with
    SELECTED (_, si) ->
      si.uid_validity
  | _ ->
      assert false

let mod_sequence_value s =
  match s.state with
    SELECTED (_, si) ->
      si.mod_sequence_value
  | _ ->
      assert false
