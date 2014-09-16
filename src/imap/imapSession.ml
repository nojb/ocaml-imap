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

type selected_info = {
  current_folder : string;
  uid_next : Uint32.t;
  uid_validity : Uint32.t;
  mod_sequence_value : Uint64.t;
  folder_msg_count : int option;
  first_unseen_uid : Uint32.t
}

type state =
  | DISCONNECTED
  | CONNECTED
  | LOGGEDIN
  | SELECTED of selected_info

type session = {
  imap_state : ImapTypes.state;
  state : state;
  condstore_enabled : bool;
  username : string;
  password : string
}

let fresh_session username password = {
  imap_state = ImapCore.fresh_state;
  state = DISCONNECTED;
  (* current_folder = None; *)
  (* uid_next = Uint32.zero; *)
  (* uid_validity = Uint32.zero; *)
  (* mod_sequence_value = Uint64.zero; *)
  (* folder_msg_count = None; *)
  (* first_unseen_uid = Uint32.zero; *)
  condstore_enabled = false;
  username;
  password
}

type folder_flag =
  [ `None
  | `Marked
  | `Unmarked
  | `NoSelect
  | `NoInferiors
  | `Inbox
  | `SentMail
  | `Starred
  | `AllMail
  | `Trash
  | `Drafts
  | `Spam
  | `Important
  | `Archive ]

type message_flag =
  [ `None
  | `Seen
  | `Answered
  | `Flagged
  | `Deleted
  | `Draft
  | `MDNSent
  | `Forwarded
  | `SubmitPending
  | `Submitted ]

type messages_request_kind =
  [ `Uid
  | `Flags
  | `Headers
  | `Structure
  | `InternalDate
  | `FullHeaders
  | `HeaderSubject
  | `GmailLabels
  | `GmailMessageID
  | `GmailThreadID
  | `ExtraHeaders of string list
  | `Size ]

type fetch_request_type =
  [ `UID
  | `Sequence ]

type flags_request_kind =
  [ `Add
  | `Remove
  | `Set ]

type workaround =
  [ `Gmail
  | `Yahoo
  | `Exchange2003 ]

type auth_capability =
  [ `Anonymous
  | `CRAMMD5
  | `DigestMD5
  | `External
  | `GSSAPI
  | `KerberosV4
  | `Login
  | `NTLM
  | `OTP
  | `Plain
  | `SKey
  | `SRP ]

type capability =
  [ `ACL
  | `Binary
  | `Catenate
  | `Children
  | `CompressDeflate
  | `Condstore
  | `Enable
  | `Idle
  | `Id
  | `LiteralPlus
  | `MultiAppend
  | `Namespace
  | `QResync
  | `Quote
  | `Sort
  | `StartTLS
  | `ThreadORderedSubject
  | `ThreadReferences
  | `UIDPlus
  | `Unselect
  | `XList
  | `Auth of auth_capability
  | `XOAuth2
  | `Gmail ]

type encoding =
  [ `Bit7
  | `Bit8
  | `Binary
  | `Base64
  | `QuotedPrintable
  | `Other
  | `UUEncode ]

type error =
  [ `None
  | `Connection
  | `TLSNotAvailable
  | `Parse
  | `Certificate
  | `Authentication
  | `GmailIMAPNotEnabled
  | `GmailExceededBandwidthLimit
  | `GmailTooManySimultaneousConnections
  | `MobileMeMoved
  | `YahooUnavailable
  | `NonExistantFolder
  | `Rename
  | `Delete
  | `Create
  | `Subscribe
  | `Append
  | `Copy
  | `Expunge
  | `Fetch
  | `Idle
  | `Identity
  | `Namespace
  | `Store
  | `Capability
  | `StartTLSNotAvailable
  | `SendMessageIllegalAttachment
  | `StorageLimit
  | `SendMessageNotAllowed
  | `NeedsConnectToWebmail
  | `SendMessage
  | `AuthenticationRequired
  | `FetchMessageList
  | `DeleteMessage
  | `InvalidAccount
  | `File
  | `Compression
  | `NoSender
  | `NoRecipient
  | `Noop
  | `State ]

type folder_status = {
  unseen_count : int;
  message_count : int;
  recent_count : int;
  uid_next : Uint32.t;
  uid_validity : Uint32.t;
  highest_mod_seq_value : Uint64.t
}

type 'a control = ('a, session, error) ImapControl.control

let flag_from_lep = function
  | FLAG_ANSWERED -> `Answered
  | FLAG_FLAGGED -> `Flagged
  | FLAG_DELETED -> `Deleted
  | FLAG_SEEN -> `Seen
  | FLAG_DRAFT -> `Draft
  | FLAG_KEYWORD "$Forwarded" -> `Forwarded
  | FLAG_KEYWORD "$MDNSent" -> `MDNSent
  | FLAG_KEYWORD "$SubmitPending" -> `SubmitPending
  | FLAG_KEYWORD "$Submitted" -> `Submitted
  | _ -> `None

let flags_from_lep_att_dynamic att_list =
  let rec loop = function
    | [] -> []
    | FLAG_FETCH_OTHER fl :: rest ->
        flag_from_lep fl :: loop rest
    | _ :: rest ->
        loop rest
  in
  loop att_list

let is_known_custom_flag = function
  | "$MDNSent"
  | "$Forwarded"
  | "$SubmitPending"
  | "$Submitted" -> true
  | _ -> false

let custom_flags_from_lep_att_dynamic att_list =
  let rec loop = function
    | [] -> []
    | FLAG_FETCH_OTHER (FLAG_KEYWORD kw) :: rest when not (is_known_custom_flag kw) ->
        kw :: loop rest
    | _ :: rest ->
        loop rest
  in
  loop att_list

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

open ImapControl

let lift m = lift (fun st -> st.imap_state) (fun st imap_state -> {st with imap_state}) m

let assert_state f =
  get >>= fun st -> if f st.state then ret () else fail `State

let connect =
  try_bind
    (lift ImapCore.greeting)
    (fun x -> modify (fun s -> {s with state = CONNECTED}) >> ret x)
    (fun _ -> fail `Connection)

let login =
  assert_state (function CONNECTED -> true | _ -> false) >>
  gets (fun s -> s.username, s.password) >>= fun (username, password) ->
  try_bind
    (lift (ImapCommands.login username password))
    (fun () ->
       modify (fun s -> {s with state = LOGGEDIN}))
    (function
      | ParseError -> fail `Parse
      | _ -> fail `Authentication)

let get_mod_sequence_value state =
  let open ImapCondstore in
  let rec loop = function
    | [] -> Uint64.zero
    | CONDSTORE_RESP_TEXT_CODE (CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ n) :: _ -> n
    | CONDSTORE_RESP_TEXT_CODE CONDSTORE_RESPTEXTCODE_NOMODSEQ :: _ -> Uint64.zero
    | _ :: rest -> loop rest
  in
  loop state.rsp_info.rsp_extension_list

let select folder : (unit, session, error) control =
  try_bind
    (lift (ImapCommands.select folder))
    (fun _ ->
       modify (fun st -> {st with state = SELECTED
                                      {uid_next = st.imap_state.sel_info.sel_uidnext;
                                       uid_validity = st.imap_state.sel_info.sel_uidvalidity;
                                       current_folder = folder;
                                       folder_msg_count = st.imap_state.sel_info.sel_exists;
                                       first_unseen_uid = st.imap_state.sel_info.sel_first_unseen;
                                       mod_sequence_value = get_mod_sequence_value st.imap_state}}))
    (function
      | ParseError -> fail `Parse
      | _ -> modify (fun st -> {st with state = LOGGEDIN}))

let folder_status folder =
  let att_list : status_att list =
    STATUS_ATT_UNSEEN :: STATUS_ATT_RECENT :: STATUS_ATT_UIDNEXT ::
    STATUS_ATT_UIDVALIDITY :: []
  in
  gets (fun st -> if st.condstore_enabled then (STATUS_ATT_HIGHESTMODSEQ : status_att) :: att_list else att_list)
  >>= fun att_list ->
  try_bind
    (lift (ImapCommands.status folder att_list))
    (fun status ->
       let rec loop fs = function
         | [] -> ret fs
         | STATUS_ATT_MESSAGES message_count :: rest ->
             loop {fs with message_count} rest
         | STATUS_ATT_UNSEEN unseen_count :: rest ->
             loop {fs with unseen_count} rest
         | STATUS_ATT_RECENT recent_count :: rest ->
             loop {fs with recent_count} rest
         | STATUS_ATT_UIDNEXT uid_next :: rest ->
             loop {fs with uid_next} rest
         | STATUS_ATT_UIDVALIDITY uid_validity :: rest ->
             loop {fs with uid_validity} rest
         | STATUS_ATT_HIGHESTMODSEQ highest_mod_seq_value :: rest ->
             loop {fs with highest_mod_seq_value} rest
         | _ :: rest ->
             loop fs rest
       in
       loop {message_count = 0; unseen_count = 0; recent_count = 0;
             uid_next = Uint32.zero; uid_validity = Uint32.zero;
             highest_mod_seq_value = Uint64.zero} status.st_info_list)
    (function
      | ParseError -> fail `Parse
      | _ -> fail `NonExistantFolder)

let enable_feature (feature : string) =
  try_bind
    (lift (ImapEnable.enable [CAPABILITY_NAME feature]))
    (fun _ -> ret true)
    (fun _ -> ret false)

let uid_next s =
  match s.state with
  | SELECTED sel ->
      sel.uid_next
  | _ ->
      invalid_arg "uid_next: bad state"

let uid_validity s =
  match s.state with
  | SELECTED sel ->
      sel.uid_validity
  | _ ->
      invalid_arg "uid_validity: bad state"

let mod_sequence_value s =
  match s.state with
  | SELECTED sel ->
      sel.mod_sequence_value
  | _ ->
      invalid_arg "mod_sequence_value: bad state"
