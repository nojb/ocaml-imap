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

module type IndexSet = sig
  type elt
  type t
  val empty : t
  val range : elt -> elt -> t
  val index : elt -> t
  val add_range : elt -> elt -> t -> t
  val add : elt -> t -> t
  val remove_range : elt -> elt -> t -> t
  val remove : elt -> t -> t
  val contains : elt -> t -> bool
  val to_string : t -> string
end

module IndexSet : sig
  include IndexSet with type elt = Uint32.t
  val to_imap_set : t -> ImapSet.t
  val of_imap_set : ImapSet.t -> t
end = struct
  type elt = Uint32.t
  type t = (elt * elt) list
  let cmp = Uint32.compare
  let succ = Uint32.add Uint32.one
  let pred n = Uint32.sub n Uint32.one
  let min l r = if cmp l r <= 0 then l else r
  let max l r = if cmp l r <= 0 then r else l
  let empty = []
  let range l r = if cmp l r <= 0 then (l, r) :: [] else (r, l) :: []
  let index n = range n n
  let add_range l r s =
    let l, r = if cmp l r <= 0 then l, r else r, l in
    let rec loop l r = function
        [] -> (l, r) :: []
      | (h, k) :: rest when cmp r (pred h) < 0 ->
          (l, r) :: (h, k) :: rest
      | (h, k) :: rest when cmp (succ k) l < 0 ->
          (h, k) :: loop l r rest
      | (h, k) :: rest (* when cmp (pred h) r <= 0 || cmp l (succ k) <= 0 *) ->
          loop (min l h) (max r k) rest
    in
    loop l r s
  let add n = add_range n n
  let remove_range l r s =
    let l, r = if cmp l r <= 0 then l, r else r, l in
    let rec loop l r = function
        [] -> []
      | (h, k) :: rest when cmp r h < 0 ->
          (h, k) :: rest
      | (h, k) :: rest when cmp k l < 0 ->
          (h, k) :: loop l r rest
      | (h, k) :: rest when cmp l h <= 0 && cmp r k < 0 ->
          (succ r, k) :: rest
      | (h, k) :: rest when cmp h l < 0 && cmp r k < 0 ->
          (h, pred l) :: (succ r, k) :: rest
      | (h, k) :: rest when cmp l h <= 0 && cmp k r <= 0 ->
          loop l r rest
      | (h, k) :: rest (* when cmp h l < 0 && cmp k r <= 0 *) ->
          (h, pred l) :: loop l r rest
    in
    loop l r s
  let remove n = remove_range n n
  let contains n s = List.exists (fun (l, r) -> cmp l n <= 0 && cmp n r <= 0) s
  let to_string s =
    String.concat "," (List.map (function
          (l, r) when Uint32.compare l r = 0 -> Uint32.to_string l
        | (l, r) -> Printf.sprintf "%s-%s" (Uint32.to_string l) (Uint32.to_string r)) s)
  let to_imap_set s =
    let f l = if cmp l Uint32.max_int = 0 then Uint32.zero else l in
    List.map (fun (l, r) -> (f l, f r)) s
  let of_imap_set s =
    let f l = if cmp l Uint32.zero = 0 then Uint32.max_int else l in
    List.fold_left (fun s (l, r) -> add_range (f l) (f r) s) empty s
end

module type Num = sig
  type t
  val of_int : int -> t
  val compare : t -> t -> int
  val zero : t
  val one : t
  val to_string : t -> string
  val of_string : string -> t
end

module Uid = Uint32
module UidSet = IndexSet

module Modseq = Uint64
module Gmsgid = Uint64
module Gthrid = Uint64

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

type search_key =
    All
  | From of string
  | To of string
  | Cc of string
  | Bcc of string
  | Recipient of string
  (** Recipient is the combination of To, Cc and Bcc *)
  | Subject of string
  | Content of string
  | Body of string
  | UIDs of ImapSet.t
  | Header of string * string
  | Read
  | Unread
  | Flagged
  | Unflagged
  | Answered
  | Unanswered
  | Draft
  | Undraft
  | Deleted
  | Spam
  | BeforeDate of float
  | OnDate of float
  | SinceDate of float
  | BeforeReceiveDate of float
  | OnReceiveDate of float
  | SinceReceiveDate of float
  | SizeLarger of int
  | SizeSmaller of int
  | GmailThreadID of Gthrid.t
  | GmailMessageID of Gmsgid.t
  | GmailRaw of string
  | Or of search_key * search_key
  | And of search_key * search_key
  | Not of search_key

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

module Identity = struct
  type t = (string * string option) list
  let create x = List.map (fun (k, v) -> (k, Some v)) x
  let get id key =
    match List.find (fun (k, _) -> String.lowercase k = String.lowercase key) id |> snd with
      None -> ""
    | Some s -> s
  let vendor id = get id "vendor"
  let version id = get id "version"
end 

type folder_status =
  { unseen_count : int;
    message_count : int;
    recent_count : int;
    uid_next : Uid.t;
    uid_validity : Uid.t;
    highest_mod_seq_value : Modseq.t }

type folder =
  { path : string;
    delimiter : char option;
    flags : folder_flag list }

type address =
  { display_name : string;
    mailbox : string }

type header =
  { message_id : string;
    references : string list;
    in_reply_to : string list;
    sender : address;
    from : address;
    to_ : address list;
    cc : address list;
    bcc : address list;
    reply_to : address list;
    subject : string }

type message =
  { uid : Uid.t;
    size : int;
    mod_seq_value : Modseq.t;
    gmail_labels : string list;
    gmail_message_id : Gmsgid.t;
    gmail_thread_id : Gthrid.t;
    flags : message_flag list;
    internal_date : float }

exception Error of error

type connected_info =
  { mutable imap_state : ImapTypes.state;
    sock : Lwt_ssl.socket;
    mutable condstore_enabled : bool;
    mutable compressor : (Cryptokit.transform * Cryptokit.transform) option;
    mutable capabilities : capability list }

type selected_info =
  { current_folder : string;
    uid_next : Uid.t;
    uid_validity : Uid.t;
    mod_sequence_value : Modseq.t;
    folder_msg_count : int option;
    first_unseen_uid : Uid.t }

type state =
    DISCONNECTED
  | CONNECTED of connected_info
  | LOGGEDIN of connected_info
  | SELECTED of connected_info * selected_info

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

let run c s =
  let open ImapControl in
  let buf = Bytes.create 65536 in
  let rec loop in_buf = function
      Ok (x, st) ->
        s.imap_state <- st;
        Lwt.return x
    | Fail (err, st) ->
        s.imap_state <- st;
        raise_lwt (ErrorP err)
    | Flush (str, k) ->
        lwt () = Lwt_log.debug_f ">>>>\n%s>>>>\n" str in
        lwt () =
          match s.compressor with
            None -> fully_write s.sock str 0 (String.length str)
          | Some (enc, _) ->
              enc # put_string str;
              enc # flush;
              let buf, pos, len = enc # get_substring in
              fully_write s.sock buf pos len
        in
        (* lwt () = fully_write s.sock str 0 (String.length str) in *)
        loop in_buf (k ())
    | Need k ->
        match_lwt read s.sock buf 0 (String.length buf) with
        | 0 ->
            loop in_buf (k End)
        | _ as n ->
            let str =
              match s.compressor with
                None -> String.sub buf 0 n
              | Some (_, dec) ->
                  dec # put_substring buf 0 n;
                  dec # flush;
                  dec # get_string
            in
            lwt () = Lwt_log.debug_f "<<<<\n%s<<<<\n" str in
            Buffer.add_substring in_buf str 0 (String.length str);
            loop in_buf (k More)
  in
  loop s.imap_state.in_buf (run c s.imap_state)

type connection =
  { mutable state : state;
    mutex : Lwt_mutex.t;
    mutable auto_disconnect : unit Lwt.t }

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

let disconnect c =
  match c.state with
    CONNECTED ci | LOGGEDIN ci | SELECTED (ci, _) ->
      lwt () = try_lwt Lwt_ssl.close ci.sock with _ -> Lwt.return_unit in
      c.state <- DISCONNECTED;
      Lwt.return_unit
  | DISCONNECTED ->
      Lwt.return_unit

let capabilities_of_imap_capabilities caps =
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
  loop [] caps

let cache_capabilities c =
  lwt ci =
    match c.state with
      CONNECTED ci | LOGGEDIN ci | SELECTED (ci, _) ->
        Lwt.return ci
    | DISCONNECTED ->
        assert_lwt false
  in                
  lwt caps =
    try_lwt
      if List.length ci.imap_state.cap_info > 0 then
        Lwt.return ci.imap_state.cap_info
      else
        run ImapCommands.capability ci
    with
      exn ->
        disconnect c >> Lwt_log.debug ~exn "capabilities failed" >> raise_lwt (Error Capability)
  in
  ci.capabilities <- capabilities_of_imap_capabilities caps;
  Lwt.return_unit

let connect ?(ssl_method = Ssl.TLSv1) ~port ~host c =
  lwt () = match c.state with
      DISCONNECTED ->
        Lwt.return_unit
    | _ ->
        assert_lwt false
  in
  try_lwt
    let he = Unix.gethostbyname host in
    let sa = Unix.ADDR_INET (he.Unix.h_addr_list.(0), port) in
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect fd sa in
    let context = Ssl.create_context ssl_method Ssl.Client_context in
    lwt sock = Lwt_ssl.ssl_connect fd context in
    let ci =
      { imap_state = ImapCore.fresh_state;
        sock;
        condstore_enabled = false;
        compressor = None;
        capabilities = [] }
    in
    lwt _ = run ImapCore.greeting ci in
    c.state <- CONNECTED ci;
    lwt () = cache_capabilities c in
    Lwt.return ci
  with
    _ -> raise_lwt (Error Connection)

let connect_if_needed ~port ~host c =
  match c.state with
    DISCONNECTED ->
      connect ~port ~host c
  | CONNECTED ci | LOGGEDIN ci | SELECTED (ci, _) ->
      Lwt.return ci

let enable_compression c =
  lwt ci = match c.state with
      CONNECTED ci | LOGGEDIN ci | SELECTED (ci, _) ->
        Lwt.return ci
    | DISCONNECTED ->
        assert_lwt false
  in
  lwt () = assert_lwt (ci.compressor = None) in
  lwt () =
    try_lwt
      run ImapCommands.Compress.compress ci
    with
      StreamError ->
        disconnect c >> raise_lwt (Error Connection)
    | ErrorP (ParseError _) ->
        raise_lwt (Error Parse)
    | _ ->
        raise_lwt (Error Compression)
  in
  ci.compressor <- Some (Cryptokit.Zlib.compress (), Cryptokit.Zlib.uncompress ());
  Lwt.return_unit

let enable_feature c feature =
  lwt ci =
    match c.state with
      CONNECTED ci | LOGGEDIN ci | SELECTED (ci, _) ->
        Lwt.return ci
    | DISCONNECTED ->
        assert_lwt false
  in
  try_lwt
    lwt _ = run (ImapCommands.Enable.enable [CAPABILITY_NAME feature]) ci in
    Lwt.return true
  with
    exn -> Lwt_log.debug_f ~exn "could not enable %S" feature >> Lwt.return false

let has_capability c cap =
  match c.state with
    CONNECTED ci | LOGGEDIN ci | SELECTED (ci, _) ->
      List.mem cap ci.capabilities
  | DISCONNECTED ->
      false

let enable_features s =
  lwt () =
    if has_capability s CompressDeflate then
      try_lwt
        enable_compression s
      with
        exn -> Lwt_log.debug ~exn "could not enable compression"
    else
      Lwt.return_unit
  in
  if has_capability s QResync then
    enable_feature s "QRESYNC" >> Lwt.return ()
  else if has_capability s Condstore then
    enable_feature s "CONDSTORE" >> Lwt.return ()
  else
    Lwt.return_unit

let contains str response =
  let rec index i =
    if i + String.length str > String.length response then
      false
    else
      let rec loop j =
        j >= String.length str || (str.[j] = response.[i + j] && loop (j+1))
      in
      loop 0
  in
  let rec loop i = index i || loop (i+1) in
  loop 0

let login ~username ~password c =
  lwt ci = match c.state with
      CONNECTED ci ->
        Lwt.return ci
    | LOGGEDIN _ | SELECTED _ | DISCONNECTED ->
        assert_lwt false
  in
  lwt () =
    try_lwt
      run (ImapCommands.login username password) ci
    with
      exn ->
        lwt () = Lwt_log.debug ~exn "login error" in
        match exn with
          StreamError ->
            disconnect c >> raise_lwt (Error Connection)
        | ErrorP (ParseError _) ->
            raise_lwt (Error Parse)
        | _ ->
            let contains str = contains str ci.imap_state.imap_response in
            if contains "not enabled for IMAP use" then
              raise_lwt (Error GmailIMAPNotEnabled)
            else if contains "bandwidth limits" then
              raise_lwt (Error GmailExceededBandwidthLimit)
            else if contains "Too many simultaneous connections" then
              raise_lwt (Error GmailTooManySimultaneousConnections)
            else if contains "Maximum number of connections" then
              raise_lwt (Error GmailTooManySimultaneousConnections)
            else if contains "http://me.com/move" then
              raise_lwt (Error MobileMeMoved)
            else if contains "OCF12" then
              raise_lwt (Error YahooUnavailable)
            else
              raise_lwt (Error Authentication)
  in
  c.state <- LOGGEDIN ci;
  lwt () = cache_capabilities c in
  lwt () = enable_features c in
  Lwt.return ci

let login_if_needed ~port ~host ~username ~password c =
  lwt _ = connect_if_needed ~port ~host c in
  match c.state with
    CONNECTED _ ->
      login ~username ~password c
  | LOGGEDIN ci | SELECTED (ci, _) ->
      Lwt.return ci
  | DISCONNECTED ->
      assert_lwt false

let get_mod_sequence_value state =
  let open ImapCommands.Condstore in
  let rec loop = function
      [] -> Modseq.zero
    | EXTENSION_DATA (RESP_TEXT_CODE, RESP_TEXT_CODE_HIGHESTMODSEQ n) :: _ -> n
    | EXTENSION_DATA (RESP_TEXT_CODE, RESP_TEXT_CODE_NOMODSEQ) :: _ -> Modseq.zero
    | _ :: rest -> loop rest
  in
  loop state.rsp_info.rsp_extension_list

let select c folder =
  lwt ci = match c.state with
      LOGGEDIN ci | SELECTED (ci, _) ->
        Lwt.return ci
    | DISCONNECTED | CONNECTED _ ->
        assert_lwt false
  in
  lwt () =
    try_lwt
      run (ImapCommands.select folder) ci
    with
      exn ->
        lwt () = Lwt_log.debug ~exn "select error" in
        match exn with
          StreamError ->
            disconnect c >> raise_lwt (Error Connection)
        | ErrorP (ParseError _) ->
            (* FIXME set state to LOGGEDIN ? *)
            raise_lwt (Error Parse)
        | _ ->
            c.state <- LOGGEDIN ci;
            raise_lwt (Error NonExistantFolder)
  in
  let st = ci.imap_state in
  let si =
    { current_folder = folder;
      uid_next = st.sel_info.sel_uidnext;
      uid_validity = st.sel_info.sel_uidvalidity;
      folder_msg_count = st.sel_info.sel_exists;
      first_unseen_uid = st.sel_info.sel_first_unseen;
      mod_sequence_value = get_mod_sequence_value st }
  in
  c.state <- SELECTED (ci, si);
  Lwt.return (ci, si)
  
let select_if_needed ~port ~host ~username ~password c folder =
  lwt _ = login_if_needed ~port ~host ~username ~password c in
  match c.state with
    SELECTED (ci, si)->
      if String.lowercase si.current_folder <> String.lowercase folder then
        select c folder
      else
        Lwt.return (ci, si)
  | LOGGEDIN _ ->
      select c folder
  | _ ->
      assert_lwt false

type session =
  { username : string;
    password : string;
    host : string;
    port : int;
    max_connections : int;
    mutable connections : connection list }

let create_session ?(max_connections = 2) ?(port = 993) ~host ~username ~password () =
  { username;
    password;
    host;
    port;
    max_connections;
    connections = [] }

let new_connection s =
  let r = { state = DISCONNECTED; mutex = Lwt_mutex.create (); auto_disconnect = Lwt.return_unit } in
  s.connections <- r :: s.connections;
  (* Lwt_log.ign_debug "new connection!"; *)
  r

let available_connection s =
  try
    List.find (fun c -> not (Lwt_mutex.is_locked c.mutex)) s.connections
  with
    Not_found ->
      if List.length s.connections < s.max_connections then
        new_connection s
      else
        List.nth s.connections (Random.int (List.length s.connections))

let with_loggedin s ?folder f ferr =
  let c =
    match folder with
      None ->
        available_connection s
    | Some folder ->
        try
          List.find begin fun c ->
            match c.state with
              SELECTED (_, si) ->
                if String.uppercase si.current_folder = String.uppercase folder then
                  begin
                    (* Lwt_log.ign_debug_f "choosing existing session for %s" si.current_folder; *)
                    true
                  end
                else
                  false
            | _ ->
                false
          end s.connections
        with
          Not_found -> available_connection s
  in
  try_lwt
    Lwt.cancel c.auto_disconnect;
    match folder with
      None ->
        Lwt_mutex.with_lock c.mutex
          (fun () -> login_if_needed s.port s.host s.username s.password c >>= f)
    | Some folder ->
        Lwt_mutex.with_lock c.mutex
          (fun () -> select_if_needed s.port s.host s.username s.password c folder >>= fun (ci, _) -> f ci)
  with
    StreamError ->
      disconnect c >> raise_lwt (Error Connection)
  | ErrorP (ParseError _) ->
      raise_lwt (Error Parse)
  | exn ->
      ferr exn
  finally
    if not (Lwt_mutex.is_locked c.mutex) then c.auto_disconnect <- Lwt_unix.sleep 60. >> disconnect c;
    Lwt.return_unit

let with_folder s folder f ferr =
  with_loggedin s ~folder f ferr

(* let with_loggedin s f ferr = *)
(*   let c = available_connection s in *)
(*   try_lwt *)
(*     if Lwt_mutex.is_empty c.mutex then Lwt.cancel c.auto_disconnect; *)
(*     Lwt_mutex.with_lock c.mutex *)
(*       (fun () -> login_if_needed s.port s.host s.username s.password c >>= f) *)
(*   with *)
(*     StreamError -> *)
(*       disconnect c >> raise_lwt (Error Connection) *)
(*   | ErrorP (ParseError _) -> *)
(*       raise_lwt (Error Parse) *)
(*   | exn -> *)
(*       ferr exn *)
(*   finally *)
(*     if Lwt_mutex.is_empty c.mutex then c.auto_disconnect <- Lwt_unix.sleep 60. >> disconnect c; *)
(*     Lwt.return_unit *)

let folder_status s ~folder =
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
  lwt status =
    with_loggedin s
      (fun ci ->
         let status_att_list : status_att list =
           if ci.condstore_enabled then STATUS_ATT_HIGHESTMODSEQ :: status_att_list else status_att_list
         in
         run (ImapCommands.status folder status_att_list) ci)
      (fun _ -> raise_lwt (Error NonExistantFolder))
  in
  let fs =
    { unseen_count = 0;
      message_count = 0;
      recent_count = 0;
      uid_next = Uid.zero;
      uid_validity = Uid.zero;
      highest_mod_seq_value = Modseq.zero }
  in
  Lwt.return (loop fs status.st_info_list)

let noop s =
  with_loggedin s (run ImapCommands.noop) (fun _ -> raise_lwt (Error Connection))

let mb_keyword_flag =
  [ "Inbox", Inbox;
    "AllMail", AllMail;
    "Sent", SentMail;
    "Spam", Spam;
    "Starred", Starred;
    "Trash", Trash;
    "Important", Important;
    "Drafts", Drafts;
    "Archive", Archive;
    "All", AllMail;
    "Junk", Spam;
    "Flagged", Starred ]

let imap_mailbox_flags_to_flags imap_flags =
  let flags =
    match imap_flags.mbf_sflag with
      None ->
        []
    | Some MBX_LIST_SFLAG_NOSELECT ->
        NoSelect :: []
    | Some MBX_LIST_SFLAG_MARKED ->
        Marked :: []
    | Some MBX_LIST_SFLAG_UNMARKED ->
        Unmarked :: []
  in
  let mb_keyword_flag = List.map (fun (k, v) -> (String.uppercase k, v)) mb_keyword_flag in
  List.fold_left (fun l imap_oflag ->
      match imap_oflag with
        MBX_LIST_OFLAG_NOINFERIORS ->
          NoInferiors :: l
      | MBX_LIST_OFLAG_EXT of_flag_ext ->
          let of_flag_ext = String.uppercase of_flag_ext in
          try List.assoc of_flag_ext mb_keyword_flag :: l with Not_found -> l)
    flags imap_flags.mbf_oflags

(* let fetch_delimiter_if_needed s = *)
(*   lwt ci = connect_if_needed s in *)
(*   match ci.delimiter with *)
(*     None -> *)
(*       lwt imap_folders = *)
(*         try_lwt *)
(*           run ci (ImapCommands.list "" "") *)
(*         with *)
(*           StreamError -> *)
(*             lwt () = disconnect s in *)
(*             raise_lwt (Error Connection) *)
(*         | _ -> *)
(*             None *)
(*   | Some c -> *)
(*       Lwt.return c *)
        

let fetch_all_folders s =
  let results mb_list =
    let flags = imap_mailbox_flags_to_flags mb_list.mb_flag in
    let path = if String.uppercase mb_list.mb_name = "INBOX" then "INBOX" else mb_list.mb_name in
    { path; delimiter = mb_list.mb_delimiter; flags }
  in
  lwt imap_folders =
        (* lwt delimiter = fetch_delimiter_if_needed ci in FIXME *)
    with_loggedin s
      (run (ImapCommands.list "" "*"))
      (fun _ -> raise_lwt (Error NonExistantFolder))
  in
  Lwt.return (List.map results imap_folders)

let rename_folder s ~folder ~new_name =
  with_folder s "INBOX"
    (run (ImapCommands.rename folder new_name))
    (fun _ -> raise_lwt (Error Rename))
  
let delete_folder s ~folder =
  with_folder s "INBOX"
    (run (ImapCommands.delete folder))
    (fun _ -> raise_lwt (Error Delete))
  
let create_folder s ~folder =
  with_folder s "INBOX"
    (run (ImapCommands.create folder))
    (fun _ -> raise_lwt (Error Create))

let copy_messages s ~folder ~uids ~dest =
  lwt uidvalidity, src_uid, dst_uid =
    with_folder s folder
      (run (ImapCommands.Uidplus.uidplus_uid_copy uids dest))
      (fun _ -> raise_lwt (Error Copy))
  in
  let h = Hashtbl.create 0 in
  ImapSet.iter2 (Hashtbl.add h) src_uid dst_uid;
  Lwt.return h

let expunge_folder s ~folder =
  with_folder s folder
    (run ImapCommands.expunge)
    (fun _ -> raise_lwt (Error Expunge))

let flags_from_lep_att_dynamic att_list =
  let rec loop (acc : message_flag list) = function
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
          loop (ImapCommands.Xgmlabels.fetch_att_xgmlabels :: acc) headers rest
      | GmailThreadID :: rest ->
          failwith "fetch gmail thread id not implemented"
      | GmailMessageID :: rest ->
          loop (ImapCommands.Xgmmsgid.fetch_att_xgmmsgid :: acc) headers rest
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
    | MSG_ATT_ITEM_EXTENSION (ImapCommands.Condstore.MSG_ATT_MODSEQ mod_seq_value) :: rest ->
        loop {msg with mod_seq_value} rest
    | MSG_ATT_ITEM_EXTENSION (ImapCommands.Xgmlabels.MSG_ATT_XGMLABELS gmail_labels) :: rest ->
        loop {msg with gmail_labels} rest
    | MSG_ATT_ITEM_EXTENSION (ImapCommands.Xgmmsgid.MSG_ATT_XGMMSGID gmail_message_id) :: rest ->
        loop {msg with gmail_message_id} rest
    | _ :: rest -> (* FIXME *)
        loop msg rest
  in
  let msg =
    loop {uid = Uid.zero; size = 0; mod_seq_value = Modseq.zero;
          gmail_labels = []; gmail_message_id = Gmsgid.zero; gmail_thread_id = Gthrid.zero;
          flags = []; internal_date = 0.0}
  in
  let fetch_type = FETCH_TYPE_FETCH_ATT_LIST fetch_atts in
  assert false
  (* lwt ci, _ = select_if_needed s folder in *)
  (* if fetch_by_uid then *)
  (*   lwt result = run ci (ImapCommands.uid_fetch imapset fetch_type) in *)
  (*   Lwt.return (List.map (fun (atts, _) -> msg atts) result) *)
  (* else *)
  (*   lwt result = run ci (ImapCommands.fetch imapset fetch_type) in *)
  (*   Lwt.return (List.map (fun (atts, _) -> msg atts) result) *)
  
let fetch_message_by_uid s ~folder ~uid =
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
  lwt result =
    with_folder s folder
      (run (ImapCommands.uid_fetch (ImapSet.single uid) fetch_type))
      (fun _ -> raise_lwt (Error Fetch))
  in
  Lwt.return (extract_body result)
  
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
              if Uid.compare uid from_uid >= 0 then Hashtbl.add result att_number uid;
              extract_uid rest
          | _ :: rest ->
              loop rest
        in
        loop att_item
  in
  lwt fetch_result =
    with_folder s folder
      (run (ImapCommands.uid_fetch imap_set fetch_type))
      (fun _ -> raise_lwt (Error Fetch))
  in
  extract_uid fetch_result;
  Lwt.return result

let imap_date_of_date t =
  let tm = Unix.localtime t in
  (tm.Unix.tm_mday, tm.Unix.tm_mon + 1, tm.Unix.tm_year + 1900)

let rec imap_search_key_from_search_key = function
    All                 -> SEARCH_KEY_ALL
  | From str            -> SEARCH_KEY_FROM str
  | To str              -> SEARCH_KEY_TO str
  | Cc str              -> SEARCH_KEY_CC str
  | Bcc str             -> SEARCH_KEY_BCC str
  | Recipient str       -> SEARCH_KEY_OR (SEARCH_KEY_TO str, SEARCH_KEY_OR (SEARCH_KEY_CC str, SEARCH_KEY_BCC str))
  | Subject str         -> SEARCH_KEY_SUBJECT str
  | Content str         -> SEARCH_KEY_TEXT str
  | Body str            -> SEARCH_KEY_BODY str
  | UIDs imapset        -> SEARCH_KEY_INSET imapset
  | Header (k, v)       -> SEARCH_KEY_HEADER (k, v)
  | Read                -> SEARCH_KEY_SEEN
  | Unread              -> SEARCH_KEY_UNSEEN
  | Flagged             -> SEARCH_KEY_FLAGGED
  | Unflagged           -> SEARCH_KEY_UNFLAGGED
  | Answered            -> SEARCH_KEY_ANSWERED
  | Unanswered          -> SEARCH_KEY_UNANSWERED
  | Draft               -> SEARCH_KEY_DRAFT
  | Undraft             -> SEARCH_KEY_UNDRAFT
  | Deleted             -> SEARCH_KEY_DELETED
  | Spam                -> SEARCH_KEY_KEYWORD "Junk"
  | BeforeDate t        -> SEARCH_KEY_SENTBEFORE (imap_date_of_date t)
  | OnDate t            -> SEARCH_KEY_SENTON (imap_date_of_date t)
  | SinceDate t         -> SEARCH_KEY_SENTSINCE (imap_date_of_date t)
  | BeforeReceiveDate t -> SEARCH_KEY_BEFORE (imap_date_of_date t)
  | OnReceiveDate t     -> SEARCH_KEY_ON (imap_date_of_date t)
  | SinceReceiveDate t  -> SEARCH_KEY_SINCE (imap_date_of_date t)
  | SizeLarger n        -> SEARCH_KEY_LARGER n
  | SizeSmaller n       -> SEARCH_KEY_SMALLER n
  | GmailThreadID id    -> SEARCH_KEY_XGMTHRID id
  | GmailMessageID id   -> SEARCH_KEY_XGMMSGID id
  | GmailRaw str        -> SEARCH_KEY_XGMRAW str
  | Or (k1, k2)         -> SEARCH_KEY_OR (imap_search_key_from_search_key k1, imap_search_key_from_search_key k2)
  | And (k1, k2)        -> SEARCH_KEY_AND (imap_search_key_from_search_key k1, imap_search_key_from_search_key k2)
  | Not k               -> SEARCH_KEY_NOT (imap_search_key_from_search_key k)

let search s ~folder ~key =
  (* let charset =  FIXME yahoo *)
  let key = imap_search_key_from_search_key key in
  lwt result_list =
    with_folder s folder
      (run (ImapCommands.uid_search key))
      (fun _ -> raise_lwt (Error Fetch))
  in
  let result = List.fold_left (fun s n -> UidSet.add n s) UidSet.empty result_list in
  Lwt.return result

let store_flags s ~folder ~uids ~kind ~flags ?(customflags = []) () =
  let imap_flag_of_flag = function
      Seen          -> FLAG_SEEN
    | Answered      -> FLAG_ANSWERED
    | Flagged       -> FLAG_FLAGGED
    | Deleted       -> FLAG_DELETED
    | Draft         -> FLAG_DRAFT
    | MDNSent       -> FLAG_KEYWORD "$MDNSent"
    | Forwarded     -> FLAG_KEYWORD "$Forwarded"
    | SubmitPending -> FLAG_KEYWORD "$SubmitPending"
    | Submitted     -> FLAG_KEYWORD "$Submitted"
  in
  let imap_flags = List.map (fun fl -> FLAG_KEYWORD fl) customflags in
  let imap_flags = List.fold_left (fun l fl -> imap_flag_of_flag fl :: l) imap_flags flags in
  let store_att_flags = { fl_sign = kind; fl_silent = true; fl_flag_list = imap_flags } in
  with_folder s folder
    (run (ImapCommands.uid_store uids store_att_flags))
    (fun _ -> raise_lwt (Error Store))

let add_flags s ~folder ~uids ~flags ?customflags () =
  let uids = UidSet.to_imap_set uids in
  store_flags s ~folder ~uids ~kind:STORE_ATT_FLAGS_ADD ~flags ?customflags ()
    
let remove_flags s ~folder ~uids ~flags ?customflags () =
  let uids = UidSet.to_imap_set uids in
  store_flags s ~folder ~uids ~kind:STORE_ATT_FLAGS_REMOVE ~flags ?customflags ()
    
let set_flags s ~folder ~uids ~flags ?customflags () =
  let uids = UidSet.to_imap_set uids in
  store_flags s ~folder ~uids ~kind:STORE_ATT_FLAGS_SET ~flags ?customflags ()

let store_labels s ~folder ~uids ~kind ~labels =
  with_folder s folder
    (run (ImapCommands.Xgmlabels.uid_store_xgmlabels uids kind true labels))
    (fun _ -> raise_lwt (Error Store))

let add_labels s ~folder ~uids ~labels =
  let uids = UidSet.to_imap_set uids in
  store_labels s ~folder ~uids ~kind:STORE_ATT_FLAGS_ADD ~labels

let remove_labels s ~folder ~uids ~labels =
  let uids = UidSet.to_imap_set uids in
  store_labels s ~folder ~uids ~kind:STORE_ATT_FLAGS_REMOVE ~labels

let set_labels s ~folder ~uids ~labels =
  let uids = UidSet.to_imap_set uids in
  store_labels s ~folder ~uids ~kind:STORE_ATT_FLAGS_SET ~labels

let capability s =
  assert false
  (* lwt ci = connect_if_needed s in *)
  (* lwt caps = *)
  (*   try_lwt *)
  (*     lwt caps = run ci ImapCommands.capability in *)
  (*     lwt () = cache_capabilities s in (\* FIXME refactor *\) *)
  (*     Lwt.return caps *)
  (*   with *)
  (*     exn -> *)
  (*       lwt () = Lwt_log.debug ~exn "capability error" in *)
  (*       match exn with *)
  (*         StreamError -> *)
  (*           disconnect s >> raise_lwt (Error Connection) *)
  (*       | ErrorP (ParseError _) -> *)
  (*           raise_lwt (Error Parse) *)
  (*       | _ -> *)
  (*           raise_lwt (Error Capability) *)
  (* in *)
  (* Lwt.return (capabilities_of_imap_capabilities caps) *)

let identity s client_id =
  (* lwt ci = connect_if_needed s in *)
  (* try_lwt *)
  (*   run ci (ImapCommands.Id.id client_id) *)
  (* with *)
  (*   exn -> *)
  (*     lwt () = Lwt_log.debug ~exn "identity error" in *)
  (*     match exn with *)
  (*       StreamError -> *)
  (*         disconnect s >> raise_lwt exn *)
  (*     | ErrorP (ParseError _) -> *)
  (*         raise_lwt (Error Parse) *)
  (*     | _ -> *)
  (*         raise_lwt (Error Identity) *)
  assert false

let uid_next s =
  match s.state with
    SELECTED (_, si) ->
      si.uid_next
  | _ ->
      invalid_arg "uid_next"

let uid_validity s =
  match s.state with
    SELECTED (_, si) ->
      si.uid_validity
  | _ ->
      invalid_arg "uid_validity"

let mod_sequence_value s =
  match s.state with
    SELECTED (_, si) ->
      si.mod_sequence_value
  | _ ->
      invalid_arg "mod_sequence_value"
