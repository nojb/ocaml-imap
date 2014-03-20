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

open Sexplib.Std
open Sexplib.Conv
open Imap_types
open Imap_response
open Imap_uint

let (>>=) = Lwt.(>>=)

let compare_ci s1 s2 =
  String.compare (String.uppercase s1) (String.uppercase s2)

module Identity = struct
  type t = {
    vendor : string;
    name : string;
    version : string;
    info : (string * string) list
  }
end

module Capability = struct
  type t =
    | ACL
    | Binary
    | Catenate
    | Children
    | Compress_deflate
    | Condstore
    | Enable
    | Idle
    | Id
    | Literal_plus
    | Multi_append
    | Namespace
    | Qresync
    | Quota
    | Sort
    | Start_tls
    | Thread_ordered_subject
    | Thread_references
    | Uid_plus
    | Unselect
    | Xlist
    | Auth_anonymous
    | Auth_cram_md5
    | Auth_digest_md5
    | Auth_external
    | Auth_gssapi
    | Auth_kerberos_v4
    | Auth_login
    | Auth_ntlm
    | Auth_otp
    | Auth_plain
    | Auth_skey
    | Auth_srp
    | Auth_xoauth2
    | Gmail
end

module Error = struct
  type t =
    | Connection
    | TLS_not_available
    | Parse
    | Certificate
    | Authentication
    | Gmail_IMAP_not_enabled
    | Gmail_exceeded_bandwidth_limit
    | Gmail_too_many_simulatenous_connections
    | MobileMe_moved
    | Yahoo_unavailable
    | Non_existent_folder
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
    | Start_tls_not_available
    | Send_message_illegal_attachment
    | Storage_limit
    | Send_message_not_allowed
    | Needs_connect_to_webmail
    | Send_message
    | Authentication_required
    | Fetch_message_list
    | Delete_message
    | Invalid_account
    | File
    | Compression
    | No_sender
    | No_recipient
    | Noop
end

exception Error of Error.t * string

module Part = struct
  type encoding =
    | Seven_bits
    | Eight_bits
    | Binary
    | Base64
    | Quoted_printable
    | Other
  
  type t = unit
end

module Message = struct
  type flag =
    | Seen
    | Answered
    | Flagged
    | Deleted
    | Draft
    | MDNSent
    | Forwarded
    | SubmitPending
    | Submitted

  type request =
    | Uid
    | Flags
    | Headers
    | Structure
    | Internal_date
    | Full_headers
    | Header_subject
    | Gmail_labels
    | Gmail_message_id
    | Gmail_thread_id
    | Extra_headers
    | Size

  type request_type =
    | By_uids
    | By_sequence

  type store_request_kind =
    | Add
    | Set
    | Remove
  
  type t = {
    uid : uint32;
    size : int;
    flags : flag list;
    modseq : uint64;
    gmail_labels : string list;
    gmail_message_id : uint64;
    gmail_thread_id : uint64
  }
end

module Folder = struct
  type status = {
    unseen_count : int;
    message_count : int;
    recent_count : int;
    uid_next : uint32;
    uid_validity : uint32;
    highestmodseq : uint64
  }

  type flag =
    | Marked
    | Unmarked
    | NoSelect
    | NoInferiors
    | Inbox
    | SentMail
    | Starred (* = Flagged *)
    | AllMail (* = All *)
    | Trash
    | Drafts
    | Spam (* = Junk *)
    | Important
    | Archive

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
      "Junk", Spam;
      "All", AllMail;
      "Flagged", Starred ]

  let flags_of_imap_flags mbf =
    let sflag =
      match mbf.mbf_sflag with
      | Some `Marked -> [Marked]
      | Some `Unmarked -> [Unmarked]
      | Some `Noselect -> [NoSelect]
      | None -> []
    in
    List.fold_right (fun oflag rest ->
        match oflag with
        | `Noinferiors -> NoInferiors :: rest
        | `Extension name ->
          if List.mem_assoc name mb_keyword_flag then
            let f = List.assoc name mb_keyword_flag in
            f :: rest
          else
            rest) mbf.mbf_oflags sflag

  type t = {
    path : string;
    delimiter : char;
    flags : flag list
  }
end

type connection_type =
  | Clear
  | Start_tls
  | Type_tls

type auth_type =
  | Sasl_none
  | Sasl_cram_md5
  | Sasl_plain
  | Sasl_gssapi
  | Sasl_digest_md5
  | Sasl_login
  | Xoauth2
  | Xoauth2_outlook

type selected_info = {
  folder : string;
  mutable uid_next : uint32;
  mutable uid_validity : uint32
} with sexp

type imap_state =
  | Disconnected
  | Non_authenticated
  | Authenticated
  | Selected of selected_info with sexp

type t = {
  imap : Imap.session sexp_opaque;
  mutable state : imap_state;
  mutable condstore_enabled : bool
} with sexp

let test_ssl_context =
  let _ = Ssl.init () in
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  Ssl.set_verify ctx [Ssl.Verify_peer] None;
  ctx

let create () =
  {state = Disconnected;
   imap = Imap.make "imap.gmail.com";
   condstore_enabled = true} (* XXX *)
   
let connect s =
  Lwt_log.info_f "connect %s\n" (Sexplib.Sexp.to_string_hum (sexp_of_t s)) >>= fun () ->
  lwt () = assert_lwt (s.state = Disconnected) in
  lwt () = match_lwt Imap.connect ~ssl_context:test_ssl_context s.imap with
    | `Needsauth ->
      s.state <- Non_authenticated;
      Lwt.return ()
    | `Preauth ->
      s.state <- Authenticated;
      Lwt.return ()
  in
  Lwt_log.info "connect ok"

let disconnect s =
  Imap.disconnect s.imap;
  s.state <- Disconnected

let connect_if_needed s =
  match s.state with
  | Disconnected ->
    connect s
  | _ ->
    Lwt.return ()

let login s =
  lwt () = Lwt_log.info "login" in
  lwt () = assert_lwt (s.state = Non_authenticated) in
  lwt () = Imap.login s.imap "imaplibtest@gmail.com" "hardpassword" in
  s.state <- Authenticated;
  Lwt.return ()

let login_if_needed s =
  lwt () = connect_if_needed s in
  match s.state with
  | Non_authenticated ->
    login s
  | _ ->
    Lwt.return ()

let folder_status s mb =
  lwt () = Lwt_log.info "status" in
  lwt () = assert_lwt (match s.state with Authenticated | Selected _ -> true | _ -> false) in
  let status_atts = [`UNSEEN; `MESSAGES; `RECENT; `UIDNEXT; `UIDVALIDITY] in
  let status_atts = if s.condstore_enabled then `HIGHESTMODSEQ :: status_atts else status_atts in
  try_lwt
    lwt resp = Imap.status s.imap mb status_atts in
    let unseen = ref 0 in
    let messages = ref 0 in
    let recent = ref 0 in
    let uid_next = ref Uint32.zero in
    let uid_validity = ref Uint32.zero in
    let highestmodseq = ref Uint64.zero in
    List.iter (function
        | `UNSEEN n -> unseen := n
        | `MESSAGES n -> messages := n
        | `RECENT n -> recent := n
        | `UIDNEXT n -> uid_next := n
        | `UIDVALIDITY n -> uid_validity := n
        | `HIGHESTMODSEQ n -> highestmodseq := n) resp.st_info_list;
    Lwt.return
      Folder.({unseen_count = !unseen; message_count = !messages;
               recent_count = !recent; uid_next = !uid_next;
               uid_validity = !uid_validity; highestmodseq = !highestmodseq})
  with
  | Imap.BAD
  | Imap.NO
  | Imap.BYE ->
    raise_lwt (Error (Error.Non_existent_folder, Imap.last_response s.imap))
  | Imap.Parse_error _ ->
    raise_lwt (Error (Error.Parse, ""))
  | exn ->
    (* s.should_disconnect <- true; *)
    raise_lwt (Error (Error.Connection, Printexc.to_string exn))

let fetch_all_folders s =
  lwt () = login_if_needed s in
  try_lwt
    lwt resp = Imap.list s.imap "" "*" in
    List.map (fun mb ->
        let flags = Folder.flags_of_imap_flags mb.mb_flag in
        let delim = mb.mb_delimiter in
        let name = if String.uppercase mb.mb_name = "INBOX" then "INBOX" else mb.mb_name in
        {Folder.path = name; Folder.delimiter = delim; Folder.flags = flags}) resp |>
    Lwt.return
  with
  | Imap.Io_error exn ->
    raise_lwt (Error (Error.Connection, Printexc.to_string exn))
  | Imap.Parse_error _ ->
    raise_lwt (Error (Error.Parse, ""))
  | Imap.BAD
  | Imap.NO
  | Imap.BYE ->
    raise_lwt (Error (Error.Non_existent_folder, Imap.last_response s.imap))

let select s mbox =
  lwt () = Lwt_log.info "select\n" in
  lwt () = match s.state with
    | Selected _
    | Authenticated -> Lwt.return ()
    | _ -> assert_lwt false
  in
  try_lwt
    lwt () = Imap.select s.imap mbox in
    let info = Imap.selection_info s.imap in
    s.state <- Selected
        {folder = mbox; uid_next = info.sel_uidnext;
         uid_validity = info.sel_uidvalidity};
    Lwt.return ()
  with
  | Imap.Io_error exn ->
    raise_lwt (Error (Error.Connection, Printexc.to_string exn))
  | Imap.Parse_error _ ->
    raise_lwt (Error (Error.Parse, ""))
  | Imap.BAD
  | Imap.NO ->
    s.state <- Authenticated;
    raise_lwt (Error (Error.Non_existent_folder, Imap.last_response s.imap))
  | Imap.BYE ->
    raise_lwt (Error (Error.Non_existent_folder, Imap.last_response s.imap))

let select_if_needed s mbox =
  lwt () = login_if_needed s in
  match s.state with
  | Selected info ->
    if compare_ci info.folder mbox <> 0 then
      select s mbox
    else
      Lwt.return ()
  | Authenticated ->
    select s mbox
  | _ ->
    assert_lwt false

let rename_folder s src dst =
  lwt () = select_if_needed s "INBOX" in
  try_lwt
    Imap.rename s.imap src dst
  with
  | Imap.Io_error exn ->
    raise_lwt (Error (Error.Connection, Printexc.to_string exn))
  | Imap.Parse_error _ ->
    raise_lwt (Error (Error.Parse, ""))
  | Imap.BAD
  | Imap.NO
  | Imap.BYE ->
    raise_lwt (Error (Error.Rename, Imap.last_response s.imap))

let delete_folder s src =
  lwt () = select_if_needed s "INBOX" in
  try_lwt
    Imap.delete s.imap src
  with
  | Imap.Io_error exn ->
    raise_lwt (Error (Error.Connection, Printexc.to_string exn))
  | Imap.Parse_error _ ->
    raise_lwt (Error (Error.Parse, ""))
  | Imap.BAD
  | Imap.NO
  | Imap.BYE ->
    raise_lwt (Error (Error.Delete, Imap.last_response s.imap))

let expunge s mbox =
  lwt () = select_if_needed s mbox in
  try_lwt
    Imap.expunge s.imap
  with
  | Imap.Io_error exn ->
    raise_lwt (Error (Error.Connection, Printexc.to_string exn))
  | Imap.Parse_error _ ->
    raise_lwt (Error (Error.Parse, ""))
  | Imap.BAD
  | Imap.NO
  | Imap.BYE ->
    raise_lwt (Error (Error.Expunge, Imap.last_response s.imap))
