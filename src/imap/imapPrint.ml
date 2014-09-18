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

open Format
open ImapTypes
open ImapTypesPrivate

let extension_print ppf e =
  let open ImapExtension in
  let rec loop =
    function
      [] ->
        fprintf ppf "(extension ..)"
    | p :: rest ->
        match p.ext_printer e with
          Some f -> f ppf
        | None -> loop rest
  in
  loop !extension_list

let capability_print ppf =
  function
    CAPABILITY_AUTH_TYPE t ->
      fprintf ppf "(auth-type %s)" t
  | CAPABILITY_NAME x ->
      fprintf ppf "(name %s)" x

let flag_print ppf =
  function
    FLAG_ANSWERED ->
      fprintf ppf "answered"
  | FLAG_FLAGGED ->
      fprintf ppf "flagged"
  | FLAG_DELETED ->
      fprintf ppf "deleted"
  | FLAG_SEEN ->
      fprintf ppf "seen"
  | FLAG_DRAFT ->
      fprintf ppf "draft"
  | FLAG_KEYWORD k ->
      fprintf ppf "(keyword %S)" k
  | FLAG_EXTENSION k ->
      fprintf ppf "(extension %S)" k

let flag_perm_print ppf =
  function
    FLAG_PERM_FLAG flag ->
      flag_print ppf flag
  | FLAG_PERM_ALL ->
      fprintf ppf "all"

let flag_fetch_print ppf =
  function
    FLAG_FETCH_RECENT ->
      fprintf ppf "recent"
  | FLAG_FETCH_OTHER f ->
      fprintf ppf "(other %a)" flag_print f
        
let resp_text_code_print ppf r =
  let p ppf =
    function
      RESP_TEXT_CODE_ALERT ->
        fprintf ppf "alert"
    | RESP_TEXT_CODE_BADCHARSET cs ->
        let p ppf = List.iter (fun x -> fprintf ppf "@ %S" x) in
        fprintf ppf "@[<2>(badcharset%a)@]" p cs
    | RESP_TEXT_CODE_CAPABILITY_DATA caps ->
        let p ppf = List.iter (fun x -> fprintf ppf "@ %a" capability_print x) in
        fprintf ppf "@[<2>(capability-data%a)@]" p caps
    | RESP_TEXT_CODE_PARSE ->
        fprintf ppf "parse"
    | RESP_TEXT_CODE_PERMANENTFLAGS flags ->
        let p ppf = List.iter (fun x -> fprintf ppf "@ %a" flag_perm_print x) in
        fprintf ppf "@[<2>(permanent-flags%a)@]" p flags
    | RESP_TEXT_CODE_READ_ONLY ->
        fprintf ppf "read-only"
    | RESP_TEXT_CODE_READ_WRITE ->
        fprintf ppf "read-write"
    | RESP_TEXT_CODE_TRYCREATE ->
        fprintf ppf "try-create"
    | RESP_TEXT_CODE_UIDNEXT uid ->
        fprintf ppf "(uid-next %s)" (Uint32.to_string uid)
    | RESP_TEXT_CODE_UIDVALIDITY uid ->
        fprintf ppf "(uid-validity %s)" (Uint32.to_string uid)
    | RESP_TEXT_CODE_UNSEEN n ->
        fprintf ppf "(unseen %s)" (Uint32.to_string n)
    (* | RESP_TEXT_CODE_UIDNOTSTICKY -> *)
        (* fprintf ppf "uid-not-sticky" *)
    (* | RESP_TEXT_CODE_COMPRESSIONACTIVE -> *)
        (* fprintf ppf "compression-active" *)
    | RESP_TEXT_CODE_EXTENSION e ->
        extension_print ppf e
    | RESP_TEXT_CODE_OTHER (k, None) ->
        fprintf ppf "(other %s)" (String.lowercase k)
    | RESP_TEXT_CODE_OTHER (k, Some v) ->
        fprintf ppf "(other %s %s)" (String.lowercase k) (String.lowercase v)
    | RESP_TEXT_CODE_NONE ->
        fprintf ppf "none"
  in
  fprintf ppf "@[<2>(resp-text-code@ %a)@]" p r

let resp_text_print ppf r =
  fprintf ppf "@[<2>(resp-text@ %a%t)@]" resp_text_code_print r.rsp_code
    (fun ppf -> if r.rsp_text <> "" then fprintf ppf "@ %S" r.rsp_text)

let resp_cond_state_print ppf r =
  let p ppf =
    function
      RESP_COND_STATE_OK ->
        fprintf ppf "ok"
    | RESP_COND_STATE_NO ->
        fprintf ppf "no"
    | RESP_COND_STATE_BAD ->
        fprintf ppf "bad"
  in
  fprintf ppf "@[<2>(resp-cond-state@ %a@ %a)@]" p r.rsp_type resp_text_print r.rsp_text

let resp_cond_auth_print ppf r =
  let p ppf =
    function
      RESP_COND_AUTH_OK ->
        fprintf ppf "ok"
    | RESP_COND_AUTH_PREAUTH ->
        fprintf ppf "preauth"
  in
  fprintf ppf "@[<2>(resp-cond-auth@ %a@ %a)@]" p r.rsp_type resp_text_print r.rsp_text

let resp_cond_bye_print ppf r =
  resp_text_print ppf r

let response_fatal_print ppf r =
  fprintf ppf "@[<2>(response-fatal@ %a)@]" resp_cond_bye_print r

let response_tagged_print ppf r =
  fprintf ppf "@[<2>(response-tagged@ (tag@ %s)@ %a)@]" r.rsp_tag resp_cond_state_print r.rsp_cond_state

let response_done_print ppf r =
  let p ppf =
    function
      RESP_DONE_TAGGED r ->
        response_tagged_print ppf r
    | RESP_DONE_FATAL r ->
        response_fatal_print ppf r
  in
  fprintf ppf "@[<2>(response-done@ %a)@]" p r

let mbx_list_sflag_print ppf flag =
  let p ppf =
    function
      MBX_LIST_SFLAG_NOSELECT ->
        fprintf ppf "noselect"
    | MBX_LIST_SFLAG_MARKED ->
        fprintf ppf "marked"
    | MBX_LIST_SFLAG_UNMARKED ->
        fprintf ppf "unmarked"
  in
  fprintf ppf "@[<2>(mbx-list-sflag@ %a)@]" p flag

let mbx_list_oflag_print ppf oflag =
  let p ppf =
    function
      MBX_LIST_OFLAG_NOINFERIORS ->
        fprintf ppf "noinferiors"
    | MBX_LIST_OFLAG_EXT s ->
        fprintf ppf "(ext %s)" s
  in
  fprintf ppf "@[<2>(mbx-list-oflag@ %a)@]" p oflag

let mbx_list_flags_print ppf flags =
  let sflag ppf =
    function
      None -> ()
    | Some sflag -> fprintf ppf "@ %a" mbx_list_sflag_print sflag
  in
  let p ppf = List.iter (fun x -> fprintf ppf "@ %a" mbx_list_oflag_print x) in
  fprintf ppf "@[<2>(mbx-list-flags%a%a)@]" sflag flags.mbf_sflag p flags.mbf_oflags 

let mailbox_list_print ppf r =
  fprintf ppf "@[<2>(mailbox-list%a@ (dir-separator %C)@ (mailbox %S))@]"
    mbx_list_flags_print r.mb_flag r.mb_delimiter r.mb_name

let status_info_print ppf r =
  let p ppf =
    function
      STATUS_ATT_MESSAGES n ->
        fprintf ppf "(messages %i)" n
    | STATUS_ATT_RECENT n ->
        fprintf ppf "(recent %i)" n
    | STATUS_ATT_UIDNEXT uid ->
        fprintf ppf "(uid-next %s)" (Uint32.to_string uid)
    | STATUS_ATT_UIDVALIDITY uid ->
        fprintf ppf "(uid-validity %s)" (Uint32.to_string uid)
    | STATUS_ATT_UNSEEN n ->
        fprintf ppf "(unseen %i)" n
    | STATUS_ATT_HIGHESTMODSEQ m ->
        fprintf ppf "(highest-mod-seq %s)" (Uint64.to_string m)
    | STATUS_ATT_EXTENSION e ->
        extension_print ppf e
  in
  fprintf ppf "@[<2>(status-info@ %a)@]" p r

let mailbox_data_status_print ppf r =
  let p ppf = List.iter (fun x -> fprintf ppf "@ %a" status_info_print x) in
  fprintf ppf "(mailbox-data-status@ %s%a)" r.st_mailbox p r.st_info_list

let mailbox_data_print ppf r =
  let p ppf =
    function
      MAILBOX_DATA_FLAGS flags ->
        let p ppf = List.iter (fun x -> fprintf ppf "@ %a" flag_print x) in
        fprintf ppf "@[<2>(flags%a)@]" p flags
    | MAILBOX_DATA_LIST list ->
        fprintf ppf "@[<2>(list@ %a)@]" mailbox_list_print list
    | MAILBOX_DATA_LSUB list ->
        fprintf ppf "@[<2>(lsub@ %a)@]" mailbox_list_print list
    | MAILBOX_DATA_SEARCH ns ->
        let p ppf = List.iter (fun n -> fprintf ppf "@ %s" (Uint32.to_string n)) in
        fprintf ppf "@[<2>(search%a)@]" p ns
    | MAILBOX_DATA_STATUS status ->
        fprintf ppf "@[<2>(status@ %a)@]" mailbox_data_status_print status
    | MAILBOX_DATA_EXISTS n ->
        fprintf ppf "(exists %i)" n
    | MAILBOX_DATA_RECENT n ->
        fprintf ppf "(recent %i)" n
    | MAILBOX_DATA_EXTENSION_DATA e ->
        extension_print ppf e
  in
  fprintf ppf "@[<2>(mailbox-data@ %a)@]" p r

let address_print ppf x =
  fprintf ppf "@[<2>(address@ (name %S)@ (addr %S)@ (mailbox %S)@ (host %S))@]"
    x.ad_personal_name x.ad_source_route x.ad_mailbox_name x.ad_host_name

let address_list_print ppf =
  function
    [] -> fprintf ppf "()"
  | x :: xs ->
      let p ppf = List.iter (fun x -> fprintf ppf "@ %a" address_print x) in
      fprintf ppf "@[<2>(%a%a)@]" address_print x p xs

let envelope_print ppf env =
  fprintf ppf "@[<2>(envelope@ (date %S)@ (subject %S)@ \
               (from %a)@ (sender %a)@ (reply-to %a)@ \
               (to %a)@ (cc %a)@ (bcc %a)@ (in-reply-to %S)@ \
               (message-id %S))@]"
    env.env_date
    env.env_subject
    address_list_print env.env_from
    address_list_print env.env_sender
    address_list_print env.env_reply_to
    address_list_print env.env_to
    address_list_print env.env_cc
    address_list_print env.env_bcc
    env.env_in_reply_to
    env.env_message_id

let date_time_print ppf dt =
  fprintf ppf "@[<2>(date-time %i/%i%i - %i:%i:%i %i)@]"
    dt.dt_day dt.dt_month dt.dt_year dt.dt_hour dt.dt_min dt.dt_month
    dt.dt_zone

let media_basic_print ppf med =
  let p ppf =
    function
      MEDIA_BASIC_APPLICATION ->
        fprintf ppf "application"
    | MEDIA_BASIC_AUDIO ->
        fprintf ppf "audio"
    | MEDIA_BASIC_IMAGE ->
        fprintf ppf "image"
    | MEDIA_BASIC_MESSAGE ->
        fprintf ppf "message"
    | MEDIA_BASIC_VIDEO ->
        fprintf ppf "video"
    | MEDIA_BASIC_OTHER s ->
        fprintf ppf "%s" s
  in
  fprintf ppf "@[<2>(media-basic@ %a@ %s)@]" p med.med_basic_type med.med_basic_subtype

let body_fields_print ppf fields =
  ()

let body_type_basic_print ppf b =
  fprintf ppf "@[<2>(body-type-basic@ %a@ %a)@]"
    media_basic_print b.bd_media_basic
    body_fields_print b.bd_fields

let body_type_msg_print ppf _ =
  ()

let body_type_text_print ppf _ =
  ()

let body_1part_print ppf b =
  let p ppf =
    function
      BODY_TYPE_1PART_BASIC b ->
        body_type_basic_print ppf b
    | BODY_TYPE_1PART_MSG b ->
        body_type_msg_print ppf b
    | BODY_TYPE_1PART_TEXT b ->
        body_type_text_print ppf b
  in
  fprintf ppf "@[<2>(body-type-1part@ %a)@]" p b.bd_data

let body_mpart_print ppf _ =
  ()

let body_print ppf b =
  let p ppf =
    function
      BODY_1PART p ->
        body_1part_print ppf p
    | BODY_MPART p ->
        body_mpart_print ppf p
  in
  fprintf ppf "@[<2>(body@ %a)@]" p b

let msg_att_item_static ppf =
  function
    MSG_ATT_ENVELOPE env -> (* FIXME *)
      envelope_print ppf env
  | MSG_ATT_INTERNALDATE dt ->
      fprintf ppf "@[<2>(internal-date@ %a)@]" date_time_print dt
  | MSG_ATT_RFC822 s ->
      fprintf ppf "@[<2>(rfc822@ %S)@]" s
  | MSG_ATT_RFC822_HEADER s ->
      fprintf ppf "@[<2>(rfc822-header@ %S)@]" s
  | MSG_ATT_RFC822_TEXT s ->
      fprintf ppf "@[<2>(rfc822-text@ %S)@]" s
  | MSG_ATT_RFC822_SIZE n ->
      fprintf ppf "@[<2>(rfc822-size %i)@]" n
  | MSG_ATT_BODY body ->
      fprintf ppf "@[<2>(body@ %a)@]" body_print body
  | MSG_ATT_BODYSTRUCTURE body ->
      fprintf ppf "@[<2>(bodystructure@ %a)@]" body_print body
  | MSG_ATT_BODY_SECTION _ ->
      fprintf ppf "@[<2>(body-section@ )@]"
  | MSG_ATT_UID n ->
      fprintf ppf "(uid %s)" (Uint32.to_string n)

let msg_att_item_dynamic ppf r =
  let p ppf = List.iter (fun x -> fprintf ppf "@ %a" flag_fetch_print x) in
  fprintf ppf "@[<2>(flags%a)@]" p r

let msg_att_item_print ppf r =
  let p ppf =
    function
      MSG_ATT_ITEM_DYNAMIC r ->
        fprintf ppf "@[<2>(mag-att-dynamic@ %a)@]" msg_att_item_dynamic r
    | MSG_ATT_ITEM_STATIC r ->
        fprintf ppf "@[<2>(msg-att-static@ %a)@]" msg_att_item_static r
    | MSG_ATT_ITEM_EXTENSION e ->
        fprintf ppf "@[<2>(msg-att-extension@ %a)@]" extension_print e
  in
  fprintf ppf "@[<2>(msg-att-item@ %a)@]" p r

let msg_att_print ppf (r, n) =
  let p ppf = List.iter (fun x -> fprintf ppf "@ %a" msg_att_item_print x) in
  fprintf ppf "@[<2>(msg-att%a@ %s)@]" p r (Uint32.to_string n)

let message_data_print ppf r =
  let p ppf =
    function
      MESSAGE_DATA_EXPUNGE n ->
        fprintf ppf "(expunge %s)" (Uint32.to_string n)
    | MESSAGE_DATA_FETCH att ->
        fprintf ppf "@[<2>(fetch@ %a)@]" msg_att_print att
  in
  fprintf ppf "@[<2>(message-data@ %a)@]" p r

let capability_data_print ppf r =
  let p ppf = List.iter (fun x -> fprintf ppf "@ %a" capability_print x) in
  fprintf ppf "@[<2>(capability-data%a)@]" p r

let response_data_print ppf r =
  let p ppf =
    function
      RESP_DATA_COND_STATE r ->
        resp_cond_state_print ppf r
    | RESP_DATA_COND_BYE r ->
        resp_cond_bye_print ppf r
    | RESP_DATA_MAILBOX_DATA r ->
        mailbox_data_print ppf r
    | RESP_DATA_MESSAGE_DATA r ->
        message_data_print ppf r
    | RESP_DATA_CAPABILITY_DATA r ->
        capability_data_print ppf r
    | RESP_DATA_EXTENSION_DATA e ->
        extension_print ppf e
  in
  fprintf ppf "@[<2>(response-data %a)@]" p r

let continue_req_print ppf r =
  let p ppf =
    function
      CONTINUE_REQ_TEXT r ->
        resp_text_print ppf r
    | CONTINUE_REQ_BASE64 r ->
        fprintf ppf "(base64 %s)" r
  in
  fprintf ppf "@[<2>(continue-req %a)@]" p r

let cont_req_or_resp_data_print ppf r =
  let p ppf =
    function
      RESP_CONT_REQ r ->
        continue_req_print ppf r
    | RESP_CONT_DATA r ->
        response_data_print ppf r
  in
  fprintf ppf "@[<2>(cont-req-or-resp-data@ %a)@]" p r

let response_print ppf r =
  let p ppf = List.iter (fun x -> fprintf ppf "@ %a" cont_req_or_resp_data_print x) in
  fprintf ppf "@[<2>(response%a@ %a)@]@."
    p r.rsp_cont_req_or_resp_data_list
    response_done_print r.rsp_resp_done

let greeting_print ppf g =
  let p ppf =
    function
      GREETING_RESP_COND_AUTH r ->
        resp_cond_auth_print ppf r
    | GREETING_RESP_COND_BYE r ->
        resp_cond_bye_print ppf r
  in
  fprintf ppf "@[<2>(greeting@ %a)@]@." p g
