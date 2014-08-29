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
open ImapTypes
open ImapUint

type capability_data =
  [ `CAPABILITY of capability list ] with sexp

type resp_text_code =
  [ `ALERT
  | `BADCHARSET of string list
  | capability_data
  | `PARSE
  | `PERMANENTFLAGS of flag_perm list
  | `READ_ONLY
  | `READ_WRITE
  | `TRYCREATE
  | `UIDNEXT of Uid.t
  | `UIDVALIDITY of Uid.t
  | `UNSEEN of Seq.t
  | `APPENDUID of Uid.t * Uid.t
  | `COPYUID of Uid.t * Uid_set.t * Uid_set.t
  | `UIDNOTSTICKY
  | `COMPRESSIONACTIVE
  | `HIGHESTMODSEQ of Modseq.t
  | `NOMODSEQ
  | `MODIFIED of Uint32_set.t
  | `OTHER of string * string
  | `NONE ] with sexp

type resp_text =
  resp_text_code * string with sexp

type resp_cond_auth =
  [ `OK of resp_text
  | `PREAUTH of resp_text ] with sexp

type resp_cond_bye =
  [ `BYE of resp_text ] with sexp

type response_fatal =
  resp_cond_bye with sexp

type resp_cond_state =
  [ `OK of resp_text
  | `NO of resp_text
  | `BAD of resp_text ] with sexp

type response_tagged =
  string * resp_cond_state with sexp

type response_done =
  [ `TAGGED of response_tagged
  | response_fatal ] with sexp

type message_data =
  [ `EXPUNGE of Seq.t
  | `FETCH of (Seq.t * msg_att list) ] with sexp

type mailbox_data =
  [ `FLAGS of flag list
  | `LIST of mailbox_list
  | `LSUB of mailbox_list
  | `SEARCH of Uint32.t list * Modseq.t
  | `STATUS of mailbox_data_status
  | `EXISTS of int
  | `RECENT of int ] with sexp

type id_response =
  [ `ID of (string * string) list ] with sexp

type namespace_response =
  [ `NAMESPACE of namespace list * namespace list * namespace list ] with sexp

type enable_response =
  [ `ENABLED of capability list ] with sexp

type response_data =
  [ resp_cond_state
  | resp_cond_bye
  | mailbox_data
  | message_data
  | capability_data
  | id_response
  | namespace_response
  | enable_response ] with sexp

type greeting =
  [ resp_cond_auth
  | resp_cond_bye ] with sexp

type continue_req =
  [ `CONT_REQ of [ `TEXT of resp_text | `BASE64 of string ] ] with sexp

type cont_req_or_resp_data_or_resp_done =
  [ continue_req
  | response_data
  | response_done ] with sexp

open ImapParser

(*
auth-type       = atom
                    ; Defined by [SASL]
*)
let auth_type =
  atom >|= fun a -> `AUTH_TYPE a

(*
capability      = ("AUTH=" auth-type) / atom
                    ; New capabilities MUST begin with "X" or be
                    ; registered with IANA as standard or
                    ; standards-track
*)
let capability =
  (string_ci "AUTH=" >> auth_type) <|>
  (atom >|= fun a -> `NAME a)

(*
capability-data = "CAPABILITY" *(SP capability) SP "IMAP4rev1"
                  *(SP capability)
                    ; Servers MUST implement the STARTTLS, AUTH=PLAIN,
                    ; and LOGINDISABLED capabilities
                    ; Servers which offer RFC 1730 compatibility MUST
                    ; list "IMAP4" as the first capability.
*)
let capability_data =
  string_ci "CAPABILITY" >> space >>
  separated_nonempty_list space capability >|= fun caps -> `CAPABILITY caps

(*
flag-extension  = "\\" atom
                    ; Future expansion.  Client implementations
                    ; MUST accept flag-extension flags.  Server
                    ; implementations MUST NOT generate
                    ; flag-extension flags except as defined by
                    ; future standard or standards-track
                    ; revisions of this specification.
*)
let flag_extension =
  bslash >> atom >|= fun s -> `Extension s

(*
flag-keyword    = atom
*)
let flag_keyword =
  atom >|= fun s -> `Keyword s

(*
flag            = "\Answered" / "\Flagged" / "\Deleted" /
                  "\Seen" / "\Draft" / flag-keyword / flag-extension
                    ; Does not include "\Recent"
*)
let flag =
  (bslash >> atom >|= fun s ->
   match String.capitalize s with
   | "Answered" -> `Answered
   | "Flagged" -> `Flagged
   | "Deleted" -> `Deleted
   | "Seen" -> `Seen
   | "Draft" -> `Draft
   | "Recent" -> `Recent
   | _ -> `Extension s) <|>
  flag_keyword

(*
flag-perm       = flag / "\*"
*)
let flag_perm =
  flag <|> (string "\\*" >| `All)

let flag_list =
  delimited lpar (separated_list space flag) rpar

(*
uniqueid        = nz-number
                    ; Strictly ascending
*)
let uniqueid =
  nz_number >|= Uid.of_uint32

(*
uid-range       = (uniqueid ":" uniqueid)
                  ; two uniqueid values and all values
                  ; between these two regards of order.
                  ; Example: 2:4 and 4:2 are equivalent.
*)
let uid_range =
  separated_pair uniqueid colon uniqueid

(*
uid-set         = (uniqueid / uid-range) *("," uid-set)
*)
let uid_set =
  separated_nonempty_list comma
    ((uniqueid >|= fun id -> Uid_set.single id) <|>
     (uid_range >|= Uid_set.interval)) >|=
  List.fold_left Uid_set.union Uid_set.empty

(*
seq-number      = nz-number / "*"
                    ; message sequence number (COPY, FETCH, STORE
                    ; commands) or unique identifier (UID COPY,
                    ; UID FETCH, UID STORE commands).
                    ; * represents the largest number in use.  In
                    ; the case of message sequence numbers, it is
                    ; the number of messages in a non-empty mailbox.
                    ; In the case of unique identifiers, it is the
                    ; unique identifier of the last message in the
                    ; mailbox or, if the mailbox is empty, the
                    ; mailbox's current UIDNEXT value.
                    ; The server should respond with a tagged BAD
                    ; response to a command that uses a message
                    ; sequence number greater than the number of
                    ; messages in the selected mailbox.  This
                    ; includes "*" if the selected mailbox is empty.
*)
let seq_number =
  nz_number <|> (star >| Uint32.zero)

(*
seq-range       = seq-number ":" seq-number
                    ; two seq-number values and all values between
                    ; these two regardless of order.
                    ; Example: 2:4 and 4:2 are equivalent and indicate
                    ; values 2, 3, and 4.
                    ; Example: a unique identifier sequence range of
                    ; 3291:* includes the UID of the last message in
                    ; the mailbox, even if that value is less than 3291.
*)
let seq_range =
  separated_pair seq_number colon seq_number >|= fun r ->
  Uint32_set.interval r

(*
sequence-set    = (seq-number / seq-range) *("," sequence-set)
                    ; set of seq-number values, regardless of order.
                    ; Servers MAY coalesce overlaps and/or execute the
                    ; sequence in any order.
                    ; Example: a message sequence number set of
                    ; 2,4:7,9,12:* for a mailbox with 15 messages is
                    ; equivalent to 2,4,5,6,7,9,12,13,14,15
                    ; Example: a message sequence number set of *:4,5:7
                    ; for a mailbox with 10 messages is equivalent to
                    ; 10,9,8,7,6,5,4,5,6,7 and MAY be reordered and
                    ; overlap coalesced to be 4,5,6,7,8,9,10.
*)
let sequence_set =
  separated_nonempty_list comma
    ((seq_number >|= fun x -> Uint32_set.single x) <|> seq_range) >|=
  List.fold_left Uint32_set.union Uint32_set.empty

(* [RFC 4551]
mod-sequence-value  = 1*DIGIT
                          ;; Positive unsigned 64-bit integer
                          ;; (mod-sequence)
                          ;; (1 <= n < 18,446,744,073,709,551,615)
*)
let mod_sequence_value =
  let number_re = Str.regexp "[0-9]*" in
  matches number_re >>= fun s -> try return (Modseq.of_string s) with _ -> fail

(*
resp-text-code  = "ALERT" /
                  "BADCHARSET" [SP "(" astring *(SP astring) ")" ] /
                  capability-data / "PARSE" /
                  "PERMANENTFLAGS" SP "("
                  [flag-perm *(SP flag-perm)] ")" /
                  "READ-ONLY" / "READ-WRITE" / "TRYCREATE" /
                  "UIDNEXT" SP nz-number / "UIDVALIDITY" SP nz-number /
                  "UNSEEN" SP nz-number /
                  atom [SP 1*<any TEXT-CHAR except "]">]

resp-code-apnd  = "APPENDUID" SP nz-number SP append-uid

resp-code-copy  = "COPYUID" SP nz-number SP uid-set SP uid-set

resp-text-code  =/ resp-code-apnd / resp-code-copy / "UIDNOTSTICKY"
                  ; incorporated before the expansion rule of
                  ;  atom [SP 1*<any TEXT-CHAR except "]">]
                  ; that appears in [IMAP]

resp-text-code   =/ "HIGHESTMODSEQ" SP mod-sequence-value /
                    "NOMODSEQ" /
                    "MODIFIED" SP set
*)
let resp_text_code : resp_text_code ImapParser.t =
  let other_re =
    (* same as [text_re] but also exclude ']' *)
    Str.regexp "[^]\r\n\x80-\xff]+"
  in
  let badcharset =
    loption (delimited (space >> lpar) (separated_nonempty_list space astring) rpar) >|= fun xs ->
    `BADCHARSET xs
  in
  let permanentflags =
    space >> delimited lpar (separated_list space flag_perm) rpar >|= fun flags ->
    `PERMANENTFLAGS flags
  in
  let uidnext = space >> nz_number >|= Uid.of_uint32 >|= fun n -> `UIDNEXT n in
  let uidvalidity = space >> nz_number >|= Uid.of_uint32 >|= fun n -> `UIDVALIDITY n in
  let unseen = space >> nz_number >|= Seq.of_uint32 >|= fun n -> `UNSEEN n in
  let appenduid =
    space >> nz_number >|= Uid.of_uint32 >>= fun uidvalidity ->
    nz_number >|= Uid.of_uint32 >|= fun uid -> `APPENDUID (uidvalidity, uid)
  in
  let copyuid =
    space >> nz_number >|= Uid.of_uint32 >>= fun uidvalidity ->
    uid_set >>= fun src_uids ->
    space >> uid_set >|= fun dst_uids ->
    `COPYUID (uidvalidity, src_uids, dst_uids)
  in
  let highestmodseq =
    space >> mod_sequence_value >|= fun modseq -> `HIGHESTMODSEQ modseq
  in
  let modified =
    space >> sequence_set >|= fun set -> `MODIFIED set
  in
  let other a = soption (space >> matches other_re) >|= fun s -> `OTHER (a, s) in
  choices [
    string_ci "ALERT" >| `ALERT;
    string_ci "BADCHARSET" >> badcharset;
    capability_data;
    string_ci "PARSE" >| `PARSE;
    string_ci "PERMANENTFLAGS" >> permanentflags;
    string_ci "READ-ONLY" >| `READ_ONLY;
    string_ci "READ-WRITE" >| `READ_WRITE;
    string_ci "TRYCREATE" >| `TRYCREATE;
    string_ci "UIDNEXT" >> uidnext;
    string_ci "UIDVALIDITY" >> uidvalidity;
    string_ci "UNSEEN" >> unseen;
    string_ci "APPENDUID" >> appenduid;
    string_ci "COPYUID" >> copyuid;
    string_ci "UIDNOTSTICKY" >| `UIDNOTSTICKY;
    string_ci "COMPRESSIONACTIVE" >| `COMPRESSIONACTIVE;
    string_ci "HIGHESTMODSEQ" >> highestmodseq;
    string_ci "NOMODSEQ" >| `NOMODSEQ;
    string_ci "MODIFIED" >> modified;
    atom >>= other
  ]

(*
resp-text       = ["[" resp-text-code "]" SP] text
*)
let resp_text : resp_text ImapParser.t =
  option (delimited lbra resp_text_code rbra) >>= function
  | None ->
    soption text >|= fun resp -> (`NONE, resp)
  | Some code ->
    (* we make the space optional if there is no resp_text_code - Gimap needs this. *)
    soption (space >> text) >|= fun resp -> (code, resp)

let base64 =
  let base64_re =
    Str.regexp
      "\\([a-zA-Z0-9+/][a-zA-Z0-9+/][a-zA-Z0-9+/][a-zA-Z0-9+/]\\)*\
       \\([a-zA-Z0-9+/][a-zA-Z0-9+/]==\\|[a-zA-Z0-9+/][a-zA-Z0-9+/][a-zA-Z0-9+/]=\\)?"
  in
  matches base64_re (* >|= Imap_utils.base64_decode *)

(*
resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
                    ; Status condition
*)
let resp_cond_state =
  let ok = space >> resp_text >|= fun rt -> `OK rt in
  let no = space >> resp_text >|= fun rt -> `NO rt in
  let bad = space >> resp_text >|= fun rt -> `BAD rt in
  choices [
    string_ci "OK" >> ok;
    string_ci "NO" >> no;
    string_ci "BAD" >> bad
  ]

(*
mbx-list-sflag  = "\Noselect" / "\Marked" / "\Unmarked"
                    ; Selectability flags; only one per LIST response
*)
let mbx_list_sflag =
  choices [
    string_ci "\\Noselect" >| `Noselect;
    string_ci "\\Marked" >| `Marked;
    string_ci "\\Unmarked" >| `Unmarked
  ]

(*
mbx-list-oflag  = "\Noinferiors" / flag-extension
                    ; Other flags; multiple possible per LIST response
*)
let mbx_list_oflag =
  (string_ci "\\Noinferiors" >| `Noinferiors) <|>
  flag_extension

let mbx_list_oflag_no_sflag =
  option mbx_list_sflag >>= function
  | None -> mbx_list_oflag
  | Some _ -> fail

(*
mbx-list-flags  = *(mbx-list-oflag SP) mbx-list-sflag
                  *(SP mbx-list-oflag) /
                  mbx-list-oflag *(SP mbx-list-oflag)
*)
let mbx_list_flags =
  (list (terminated mbx_list_oflag_no_sflag space) >>= fun oflags1 ->
   mbx_list_sflag >>= fun sflag -> list (space >> mbx_list_oflag) >|= fun oflags2 ->
  {mbf_sflag = Some sflag; mbf_oflags = oflags1 @ oflags2}) <|>
  (separated_nonempty_list space mbx_list_oflag >|= fun oflags ->
  {mbf_sflag = None; mbf_oflags = oflags})

let mailbox =
  let decode_mailbox_name s = try ImapUtils.decode_mutf7 s with _ -> s in
  astring >|= decode_mailbox_name

(*
mailbox-list    = "(" [mbx-list-flags] ")" SP
                   (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
*)
let mailbox_list =
  delimited lpar (option mbx_list_flags >|= function
    | Some flags -> flags
    | None -> {mbf_sflag = None; mbf_oflags = []}) rpar >>= fun flags ->
  space >> (quoted_char <|> (nil >| '\000')) >>= fun delim ->
  space >> mailbox >|= fun mb ->
  {mb_flag = flags; mb_delimiter = delim; mb_name = mb}

(*
status-att      = "MESSAGES" / "RECENT" / "UIDNEXT" / "UIDVALIDITY" /
                  "UNSEEN"

status-att          =/ "HIGHESTMODSEQ"
                          ;; extends non-terminal defined in RFC 3501.
*)
let status_att_number : status_info t =
  let messages = space >> number' >|= fun n -> `MESSAGES n in
  let recent = space >> number' >|= fun n -> `RECENT n in
  let uidnext = space >> nz_number >|= Uid.of_uint32 >|= fun n -> `UIDNEXT n in
  let uidvalidity = space >> nz_number >|= Uid.of_uint32 >|= fun n -> `UIDVALIDITY n in
  let unseen = space >> number' >|= fun n -> `UNSEEN n in
  let highestmodseq = space >> mod_sequence_value >|= fun n -> `HIGHESTMODSEQ n in
  choices [
    string_ci "MESSAGES" >> messages;
    string_ci "RECENT" >> recent;
    string_ci "UIDNEXT" >> uidnext;
    string_ci "UIDVALIDITY" >> uidvalidity;
    string_ci "UNSEEN" >> unseen;
    string_ci "HIGHESTMODSEQ" >> highestmodseq
  ]

(*
status-att-list =  status-att SP number *(SP status-att SP number)
*)
let status_att_list =
  separated_nonempty_list space status_att_number

let date_day_fixed =
  (space >> digit) <|> digits2

let date_month =
  string_of_length 3 >|= String.capitalize >>= function
  | "Jan" -> return 1
  | "Feb" -> return 2
  | "Mar" -> return 3
  | "Apr" -> return 4
  | "May" -> return 5
  | "Jun" -> return 6
  | "Jul" -> return 7
  | "Aug" -> return 8
  | "Sep" -> return 9
  | "Oct" -> return 10
  | "Nov" -> return 11
  | "Dec" -> return 12
  | _ -> fail

let time =
  digits2 >>= fun hours -> colon >>
  digits2 >>= fun minutes -> colon >>
  digits2 >|= fun seconds ->
  (hours, minutes, seconds)

let zone =
  (plus >> digits2 >>= fun hh -> digits2 >|= fun mm -> hh * 60 + mm) <|>
  (minus >> digits2 >>= fun hh -> digits2 >|= fun mm -> -hh * 60 - mm)

let date_time =
  delimited dquote
    (date_day_fixed >>= fun day ->
     dash >> date_month >>= fun month ->
     dash >> digits4 >>= fun year ->
     space >> time >>= fun (hour, min, sec) ->
     space >> zone >|= fun zone ->
     {dt_day = day; dt_month = month; dt_year = year;
      dt_hour = hour; dt_min = min; dt_sec = sec; dt_zone = zone})
    dquote
    
let header_fld_name =
  astring

let header_list =
  delimited lpar (separated_nonempty_list space header_fld_name) rpar

(*
section-msgtext = "HEADER" / "HEADER.FIELDS" [".NOT"] SP header-list /
                  "TEXT"
                    ; top-level or MESSAGE/RFC822 part
*)
let section_msgtext =
  let header_fields =
    boption (string_ci ".NOT") >>= fun has_not ->
    space >> header_list >|= fun hdrs ->
    if has_not then `HEADER_FIELDS_NOT hdrs else `HEADER_FIELDS hdrs
  in
  choices [
    string_ci "HEADER.FIELDS" >> header_fields;
    string_ci "HEADER" >| `HEADER;
    string_ci "TEXT" >| `TEXT
  ]

(*
section-part    = nz-number *("." nz-number)
                    ; body part nesting
*)
let section_part =
  separated_pair nz_number' dot (separated_list dot nz_number')

(*
section-text    = section-msgtext / "MIME"
                    ; text other than actual body part (headers, etc.)
*)
let section_text =
  (string_ci "MIME" >| `MIME) <|> section_msgtext

(*
section-spec    = section-msgtext / (section-part ["." section-text])
*)
let section_spec =
  section_msgtext <|>
  begin
    section_part >>= fun (n0, ns) -> option (dot >> section_text) >|= function
    | Some text ->
      `PART (n0, List.fold_right (fun p t -> `PART (p, t)) ns text)
    | None ->
      `PART (n0, List.fold_right (fun p t -> `PART (p, t)) ns `ALL)
  end

(*
section         = "[" [section-spec] "]"
*)
let section : [> section] ImapParser.t =
  delimited lbra section_spec rbra <|> return `ALL

let uint64 =
  let number_re = Str.regexp "[0-9]*" in
  matches number_re >>= fun s -> try return (Uint64.of_string s) with _ -> fail
 
(*
msg-att-static  = "ENVELOPE" SP envelope / "INTERNALDATE" SP date-time /
                  "RFC822" [".HEADER" / ".TEXT"] SP nstring /
                  "RFC822.SIZE" SP number /
                  "BODY" ["STRUCTURE"] SP body /
                  "BODY" section ["<" number ">"] SP nstring /
                  "UID" SP uniqueid
                    ; MUST NOT change for a message
*)
let msg_att_static : [> msg_att_static] ImapParser.t =
  let envelope = space >> ImapMime.envelope >|= fun e -> `ENVELOPE e in
  let internaldate = space >> date_time >|= fun dt -> `INTERNALDATE dt in
  let rfc822_header = space >> nstring' >|= fun s -> `RFC822_HEADER s in
  let rfc822_text = space >> nstring' >|= fun s -> `RFC822_TEXT s in
  let rfc822_size = space >> number' >|= fun n -> `RFC822_SIZE n in
  let rfc822 = space >> nstring' >|= fun s -> `RFC822 s in
  let bodystructure = space >> ImapMime.body >|= fun b -> `BODYSTRUCTURE b in
  let body =
    let body_ b =
      `BODY b
    in
    let section_ sec =
      begin option (delimited labra number' rabra) >|= function
        | Some n -> `PARTIAL (sec, n)
        | None -> (sec :> msg_att_section)
      end >>= fun sec ->
      space >> nstring' >|= fun s ->
      `BODYSECTION (sec, s)
    in
    choices [
      space >> ImapMime.body >|= body_;
      section >>= section_
    ]
  in
  let uid = space >> nz_number >|= Uid.of_uint32 >|= fun uid -> `UID uid in
  let x_gm_msgid = space >> uint64 >|= Gmsgid.of_uint64 >|= fun n -> `X_GM_MSGID n in
  let x_gm_thrid = space >> uint64 >|= Gthrid.of_uint64 >|= fun n -> `X_GM_THRID n in
  choices [
    string_ci "ENVELOPE" >> envelope;
    string_ci "INTERNALDATE" >> internaldate;
    string_ci "RFC822.HEADER" >> rfc822_header;
    string_ci "RFC822.TEXT" >> rfc822_text;
    string_ci "RFC822.SIZE" >> rfc822_size;
    string_ci "RFC822" >> rfc822;
    string_ci "BODYSTRUCTURE" >> bodystructure;
    string_ci "BODY" >> body;
    string_ci "UID" >> uid;
    string_ci "X-GM-MSGID" >> x_gm_msgid;
    string_ci "X-GM-THRID" >> x_gm_thrid
  ]

(*
permsg-modsequence  = mod-sequence-value
                          ;; per message mod-sequence
*)
let permsg_modsequence =
  mod_sequence_value

(*
msg-att-dynamic = "FLAGS" SP "(" [flag-fetch *(SP flag-fetch)] ")"
                    ; MAY change for a message

fetch-mod-resp      = "MODSEQ" SP "(" permsg-modsequence ")"

msg-att-dynamic     =/ fetch-mod-resp

*)
let msg_att_dynamic : [> msg_att_dynamic] ImapParser.t =
  let flags = space >> flag_list >|= fun flags -> `FLAGS flags in
  let modseq = space >> delimited lpar permsg_modsequence rpar >|= fun n -> `MODSEQ n in
  let x_gm_labels =
    space >> delimited lpar (separated_list space astring) rpar >|= fun labs ->
    `X_GM_LABELS (List.map ImapUtils.decode_mutf7 labs)
  in
  choices [
    string_ci "FLAGS" >> flags;
    string_ci "MODSEQ" >> modseq;
    string_ci "X-GM-LABELS" >> x_gm_labels
  ]

(*
msg-att         = "(" (msg-att-dynamic / msg-att-static)
                   *(SP (msg-att-dynamic / msg-att-static)) ")"
*)
let msg_att =
  delimited lpar
    (separated_nonempty_list space (msg_att_static <|> msg_att_dynamic))
    rpar

(*
search-sort-mod-seq = "(" "MODSEQ" SP mod-sequence-value ")"
*)
let search_sort_mod_seq =
  delimited lpar (string_ci "MODSEQ" >> space >> mod_sequence_value) rpar

(*
mailbox-data    =  "FLAGS" SP flag-list / "LIST" SP mailbox-list /
                   "LSUB" SP mailbox-list / "SEARCH" *(SP nz-number) /
                   "STATUS" SP mailbox SP "(" [status-att-list] ")" /
                   number SP "EXISTS" / number SP "RECENT"
   
mailbox-data        =/ "SEARCH" [1*(SP nz-number) SP
                          search-sort-mod-seq]
*)
let mailbox_data =
  let flags = space >> flag_list >|= fun flags -> `FLAGS flags in
  let list_ = space >> mailbox_list >|= fun mb -> `LIST mb in
  let lsub = space >> mailbox_list >|= fun mb -> `LSUB mb in
  let search =
    list (space >> nz_number) >>= function
    | [] ->
      return (`SEARCH ([], Modseq.zero))
    | ns ->
      option (space >> search_sort_mod_seq) >|= function
      | None -> `SEARCH (ns, Modseq.zero)
      | Some modseq -> `SEARCH (ns, modseq)
  in
  let status =
    space >> mailbox >>= fun mb ->
    space >> delimited lpar (loption status_att_list) rpar >|= fun att ->
    `STATUS {st_mailbox = mb; st_info_list = att}
  in
  let exists_or_recent n =
    space >> choices [
      string_ci "EXISTS" >| `EXISTS n;
      string_ci "RECENT" >| `RECENT n
    ]
  in
  choices [
    string_ci "FLAGS" >> flags;
    string_ci "LIST" >> list_;
    string_ci "LSUB" >> lsub;
    string_ci "SEARCH" >> search;
    string_ci "STATUS" >> status;
    number' >>= exists_or_recent
  ]

(*
message-data    = nz-number SP ("EXPUNGE" / ("FETCH" SP msg-att))
*)
let message_data =
  let expunge_or_fetch n =
    space >> choices [
      string_ci "EXPUNGE" >| `EXPUNGE n;
      string_ci "FETCH" >> space >> msg_att >|= fun att -> `FETCH (n, att)
    ]
  in
  nz_number >|= Seq.of_uint32 >>= expunge_or_fetch
 
(*
tag             = 1*<any ASTRING-CHAR except "+">
*)
let tag =
  let tag_re =
    (* same as [astring_re] except that we also exclude '+' *)
    Str.regexp "[^\x80-\xff(){ \x00-\x1f\x7f%*\\\"+]+"
  in
  matches tag_re

(*
response-tagged = tag SP resp-cond-state CRLF
*)
let response_tagged =
  tag >>= fun tag -> space >> resp_cond_state >>= fun resp -> crlf >| `TAGGED (tag, resp)

(*
resp-cond-auth  = ("OK" / "PREAUTH") SP resp-text
                    ; Authentication condition
*)
let resp_cond_auth =
  (string_ci "OK" >> space >> resp_text >|= fun rt -> `OK rt) <|>
  (string_ci "PREAUTH" >> space >> resp_text >|= fun rt -> `PREAUTH rt)

(*
resp-cond-bye   = "BYE" SP resp-text
*)
let resp_cond_bye =
  string_ci "BYE" >> space >> resp_text >|= fun rt -> `BYE rt

(*
greeting        = "*" SP (resp-cond-auth / resp-cond-bye) CRLF
*)
let greeting =
  delimited (star >> space) (choice resp_cond_auth resp_cond_bye) crlf

(*
continue-req    = "+" SP (resp-text / base64) CRLF
*)
let continue_req =
  plus >> option space >> (* we allow an optional space *)
  ((terminated base64 crlf >|= fun b64 -> `CONT_REQ (`BASE64 b64)) <|>
   (terminated resp_text crlf >|= fun rt -> `CONT_REQ (`TEXT rt)))

(*
id_params_list ::= "(" #(string SPACE nstring) ")" / nil
         ;; list of field value pairs
*)
let id_params_list =
  loption (delimited lpar
             (separated_list space
                (separated_pair imap_string space (noption imap_string))) rpar) >|=
  ImapUtils.option_map (function (k, Some v) -> Some (k, v) | (_, None) -> None)
  
(*
id_response ::= "ID" SPACE id_params_list
*)

let id_response =
  string_ci "ID" >> space >> id_params_list >|= fun params -> `ID params

(*
Namespace_Response_Extension = SP string SP "(" string *(SP string)
      ")"
*)
let namespace_response_extension =
  space >>
  separated_pair
    imap_string
    space
    (delimited lpar (separated_nonempty_list space imap_string) rpar)
  
(*
Namespace = nil / "(" 1*( "(" string SP  (<"> QUOTED_CHAR <"> /
      nil) *(Namespace_Response_Extension) ")" ) ")"
*)
let namespace =
  (nil >| []) <|>
  (delimited lpar
     (nonempty_list
        (delimited lpar
           (imap_string >|= ImapUtils.decode_mutf7 >>= fun prefix ->
            space >> (quoted_char <|> (nil >| '\000')) >>= fun delim ->
            list namespace_response_extension >|= fun exts ->
            {ns_prefix = prefix; ns_delimiter = delim; ns_extensions = exts})
           rpar))
     rpar)                            

(*
Namespace_Response = "*" SP "NAMESPACE" SP Namespace SP Namespace SP
      Namespace

      ; The first Namespace is the Personal Namespace(s)
      ; The second Namespace is the Other Users' Namespace(s)
      ; The third Namespace is the Shared Namespace(s)
*)
let namespace_response =
  string_ci "NAMESPACE" >>
  space >> namespace >>= fun personal ->
  space >> namespace >>= fun others ->
  space >> namespace >|= fun shared ->
  `NAMESPACE (personal, others, shared)

(*
enable-data   = "ENABLED" *(SP capability)
*)
let enable_data =
  string_ci "ENABLED" >>
  list (space >> capability) >|= fun caps ->
  `ENABLED caps

(*
response-data   = "*" SP (resp-cond-state / resp-cond-bye /
                  mailbox-data / message-data / capability-data) CRLF

response_data ::= "*" SPACE (resp_cond_state / resp_cond_bye /
         mailbox_data / message_data / capability_data / id_response)

response-data =/ "*" SP enable-data CRLF
*)
let response_data =
  delimited (star >> space)
    (choices [resp_cond_state;
              resp_cond_bye;
              mailbox_data;
              message_data;
              capability_data;
              id_response;
              namespace_response;
              enable_data])
    crlf

(*
response-fatal  = "*" SP resp-cond-bye CRLF
                    ; Server closes connection immediately
*)
let response_fatal =
  delimited (star >> space) resp_cond_bye crlf

(*
response-done   = response-tagged / response-fatal
*)
let response_done =
  response_tagged <|> response_fatal

let resp_data_or_resp_done =
  response_data <|> response_done

let cont_req_or_resp_data_or_resp_done =
  continue_req <|> response_data <|> response_done
