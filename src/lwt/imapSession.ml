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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let _ =
  prerr_endline "Initialising SSL...";
  Ssl.init ()

type state =
  | DISCONNECTED
  | LOGGEDIN
  | SELECTED

type session = {
  mutable imap_state : ImapTypes.state;
  sock : Lwt_ssl.socket;
  mutable state : state;
  mutable current_folder : string option;
  mutable uid_next : Uint32.t;
  mutable uid_validity : Uint32.t;
  mutable mod_sequence_value : Uint64.t;
  mutable folder_msg_count : int option;
  mutable first_unseen_uid : Uint32.t;
  username : string;
  password : string
}

exception ErrorP of error

let fully_write sock buf pos len =
  let rec loop pos len =
    if len <= 0 then
      Lwt.return ()
    else
      lwt n = Lwt_ssl.write sock buf pos len in
      loop (pos + n) (len - n)
  in
  loop pos len

let run s c =
  let open ImapControl in
  let buf = Bytes.create 65536 in
  let rec loop out_buf in_buf = function
    | Ok (x, st) ->
        Lwt.return (x, st)
    | Fail err ->
        Lwt.fail (ErrorP err)
    | Flush k ->
        let str = Buffer.contents out_buf in
        prerr_endline ">>>>";
        prerr_string str;
        prerr_endline ">>>>";
        Buffer.clear out_buf;
        lwt () = fully_write s.sock str 0 (String.length str) in
        loop out_buf in_buf (k ())
    | Need k ->
        match_lwt Lwt_ssl.read s.sock buf 0 (String.length buf) with
        | 0 ->
            loop out_buf in_buf (k End)
        | _ as n ->
            prerr_endline "<<<<";
            prerr_string (String.sub buf 0 n);
            prerr_endline "<<<<";
            Buffer.add_substring in_buf buf 0 n;
            loop out_buf in_buf (k More)
  in
  lwt x, st = loop s.imap_state.out_buf s.imap_state.in_buf (run c s.imap_state) in
  s.imap_state <- st;
  Lwt.return x

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

let fresh_session sock username password = {
  imap_state = ImapCore.fresh_state;
  state = DISCONNECTED;
  current_folder = None;
  uid_next = Uint32.zero;
  uid_validity = Uint32.zero;
  mod_sequence_value = Uint64.zero;
  folder_msg_count = None;
  first_unseen_uid = Uint32.zero;
  username;
  password;
  sock
}

let create_session ?(ssl_method = Ssl.TLSv1) ?(port=993) host username password =
  let he = Unix.gethostbyname host in
  let sockaddr = Unix.ADDR_INET (he.Unix.h_addr_list.(0), port) in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  lwt () = Lwt_unix.connect fd sockaddr in
  let context = Ssl.create_context ssl_method Ssl.Client_context in
  lwt sock = Lwt_ssl.ssl_connect fd context in
  Lwt.return (fresh_session sock username password)

type folder_flag =
  | Marked
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
  | Seen
  | Answered
  | Flagged
  | Deleted
  | Draft
  | MDNSent
  | Forwarded
  | SubmitPending
  | Submitted

type messages_request_kind =
  | Uid
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
  | UID
  | Sequence

type flags_request_kind =
  [ `Add
  | `Remove
  | `Set ]

type workaround =
  | Gmail
  | Yahoo
  | Exchange2003

type auth_capability =
  | Anonymous
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
  | ACL
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
  | Bit7
  | Bit8
  | Binary
  | Base64
  | QuotedPrintable
  | Other
  | UUEncode

type error =
  | Connection
  | TLSNotAvailable
  | Parse
  | Certificate
  | Authentication
  | GmailIMAPNotEnabled
  | GmailExceededBandwidthLimit
  | GmailTooManySimultaneousConnections
  | MobileMeMoved
  | YahooUnavailable
  | ErrorNonExistantFolder
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

(* let flag_from_lep = function *)
(*   | FLAG_ANSWERED -> Some Answered *)
(*   | FLAG_FLAGGED -> Some Flagged *)
(*   | FLAG_DELETED -> Some Deleted *)
(*   | FLAG_SEEN -> Some Seen *)
(*   | FLAG_DRAFT -> Some Draft *)
(*   | FLAG_KEYWORD "$Forwarded" -> Some Forwarded *)
(*   | FLAG_KEYWORD "$MDNSent" -> Some MDNSent *)
(*   | FLAG_KEYWORD "$SubmitPending" -> Some SubmitPending *)
(*   | FLAG_KEYWORD "$Submitted" -> Some Submitted *)
(*   | _ -> None *)

(* let flags_from_lep_att_dynamic att_list = *)
(*   let rec loop = function *)
(*     | [] -> [] *)
(*     | FLAG_FETCH_OTHER fl :: rest -> *)
(*         let fl = *)
(*           match flag_from_lep fl with *)
(*           | Some fl -> fl :: loop rest *)
(*           | None -> loop rest *)
(*         in *)
(*         fl *)
(*     | _ :: rest -> *)
(*         loop rest *)
(*   in *)
(*   loop att_list *)

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

(* let fetch_messages folder rk by_uid set modseq map startuid = *)
(*   let headers = ref [] in *)
(*   let fetch_atts = *)
(*     List.map (function *)
(*         | `Flags -> *)
(*             Some FETCH_ATT_FLAGS *)
(*         | `GmailLabels -> *)
(*             Some ImapXgmlabels.fetch_att_xgmlabels *)
(*         | `GmailThreadId -> *)
(*             failwith "fetch gmail thread id not implemented" *)
(*         | `GmailMessageId -> *)
(*             Some ImapXgmmsgid.fetch_att_xgmmsgid *)
(*         | `FullHeaders -> *)
(*             headers := *)
(*               "Date" :: "Subject" :: "From" :: "Sender" :: "Reply-To" :: *)
(*               "To" :: "Cc" :: "Message-ID" :: "References" :: "In-Reply-To" :: !headers; *)
(*             None *)
(*         | `Headers -> *)
(*             headers := "References" :: !headers; *)
(*             Some FETCH_ATT_ENVELOPE *)
(*         | `HeaderSubject -> *)
(*             headers := "References" :: "Subject" :: !headers; *)
(*             Some FETCH_ATT_ENVELOPE *)
(*         | `Size -> *)
(*             Some FETCH_ATT_RFC822_SIZE *)
(*         | `Structure -> *)
(*             needs_body := true; *)
(*             Some FETCH_ATT_RFC822_BODYSTRUCTURE *)
(*         | `InternalDate -> *)
(*             Some FETCH_ATT_INTERNALDATE *)
(*         | `ExtraHeaders extra -> *)
(*             headers := extra @ !headers) *)
(*       rk *)
(*   in *)
(*   let fetch_atts = *)
(*     if List.length !headers > 0 then *)
(*       FETCH_ATT_BODY_PEEK_SECTION *)
(*         (Some (SECTION_SPEC_SECTION_MSGTEXT (SECTION_MSGTEXT_HEADER_FIELDS !headers))) :: *)
(*       fetch_atts *)
(*     else *)
(*       fetch_atts *)
(*   in *)
(*   Commands. *)
(*   if by_uid then *)
(*     if  *)

(* open ImapControl *)

(* let lift m = lift (fun st -> st.imap_state) (fun st imap_state -> {st with imap_state}) m *)

exception Error of error

let connect s =
  try_lwt
    run s ImapCore.greeting
  with
  | _ -> Lwt.fail (Error Connection)

let login s =
  lwt () =
    try_lwt
      run s (ImapCommands.login s.username s.password)
    with
    | ErrorP ParseError -> Lwt.fail (Error Parse)
    | _ -> Lwt.fail (Error Authentication)
  in
  s.state <- LOGGEDIN;
  Lwt.return ()

(* let get_mod_sequence_value state = *)
(*   let open ImapCondstore in *)
(*   let rec loop = function *)
(*     | [] -> Uint64.zero *)
(*     | CONDSTORE_RESP_TEXT_CODE (CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ n) :: _ -> n *)
(*     | CONDSTORE_RESP_TEXT_CODE CONDSTORE_RESPTEXTCODE_NOMODSEQ :: _ -> Uint64.zero *)
(*     | _ :: rest -> loop rest *)
(*   in *)
(*   loop state.rsp_info.rsp_extension_list *)

(* let select folder : (unit, session, error) control = *)
(*   try_bind *)
(*     (lift (ImapCommands.select folder)) *)
(*     (fun _ -> *)
(*        modify (fun st -> {st with uid_next = st.imap_state.sel_info.sel_uidnext; *)
(*                                   uid_validity = st.imap_state.sel_info.sel_uidvalidity; *)
(*                                   state = SELECTED; *)
(*                                   current_folder = Some folder; *)
(*                                   folder_msg_count = st.imap_state.sel_info.sel_exists; *)
(*                                   first_unseen_uid = st.imap_state.sel_info.sel_first_unseen; *)
(*                                   mod_sequence_value = get_mod_sequence_value st.imap_state})) *)
(*     (function *)
(*       | ParseError -> fail `Parse *)
(*       | _ -> modify (fun st -> {st with state = LOGGEDIN})) *)

(* let enable_feature (feature : string) = *)
(*   try_bind *)
(*     (lift (ImapEnable.enable [CAPABILITY_NAME feature])) *)
(*     (fun _ -> ret true) *)
(*     (fun _ -> ret false) *)

(* let uid_next s = *)
(*   s.uid_next *)

(* let uid_validity s = *)
(*   s.uid_validity *)

(* let mod_sequence_value s = *)
(*   s.mod_sequence_value *)
