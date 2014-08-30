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
  atom >>= fun a -> return (`AUTH_TYPE a)

(*
capability      = ("AUTH=" auth-type) / atom
                    ; New capabilities MUST begin with "X" or be
                    ; registered with IANA as standard or
                    ; standards-track
*)
let capability =
  alt [(string_ci "AUTH=" >> auth_type); (atom >>= fun a -> return (`NAME a))]

(*
capability-data = "CAPABILITY" *(SP capability) SP "IMAP4rev1"
                  *(SP capability)
                    ; Servers MUST implement the STARTTLS, AUTH=PLAIN,
                    ; and LOGINDISABLED capabilities
                    ; Servers which offer RFC 1730 compatibility MUST
                    ; list "IMAP4" as the first capability.
*)
let capability_data =
  string_ci "CAPABILITY" >> char ' ' >>
  capability >>= fun x ->
  rep (char ' ' >> capability) >>= fun xs ->
  return (`CAPABILITY (x :: xs))

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
  char '\\' >> atom >>= fun s -> return (`Extension s)

(*
flag-keyword    = atom
*)
let flag_keyword =
  atom >>= fun s -> return (`Keyword s)

(*
flag            = "\Answered" / "\Flagged" / "\Deleted" /
                  "\Seen" / "\Draft" / flag-keyword / flag-extension
                    ; Does not include "\Recent"
*)
let flag =
  alt [
    begin
      char '\\' >> atom >>= fun s ->
      return
        (match String.capitalize s with
         | "Answered" -> `Answered
         | "Flagged" -> `Flagged
         | "Deleted" -> `Deleted
         | "Seen" -> `Seen
         | "Draft" -> `Draft
         | "Recent" -> `Recent
         | _ -> `Extension s)
    end;
    flag_keyword
  ]

(*
flag-perm       = flag / "\*"
*)
let flag_perm =
  alt [flag; (string "\\*" >> return `All)]

let flag_list =
  char '(' >>
  sep (char ' ') flag >>= fun xs ->
  char ')' >>
  return xs

(*
uniqueid        = nz-number
                    ; Strictly ascending
*)
let uniqueid =
  nz_number >>= fun x -> return (Uid.of_uint32 x)

(*
uid-range       = (uniqueid ":" uniqueid)
                  ; two uniqueid values and all values
                  ; between these two regards of order.
                  ; Example: 2:4 and 4:2 are equivalent.
*)
let uid_range =
  uniqueid >>= fun x ->
  char ':' >>
  uniqueid >>= fun y ->
  return (x, y)

(*
uid-set         = (uniqueid / uid-range) *("," uid-set)
*)
let uid_set =
  let elem =
    alt [(uniqueid >>= fun id -> return (Uid_set.single id));
         (uid_range >>= fun r -> return (Uid_set.interval r))]
  in
  elem >>= fun x ->
  rep (char ',' >> elem) >>= fun xs ->
  return (List.fold_left Uid_set.union x xs)

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
  alt [nz_number; (char '*' >> return Uint32.zero)]

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
  seq_number >>= fun x ->
  char ':' >>
  seq_number >>= fun y ->
  return (Uint32_set.interval (x, y))

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
  let elem = alt [(seq_number >>= fun x -> return (Uint32_set.single x)); seq_range] in
  elem >>= fun x ->
  rep (char ',' >> elem) >>= fun xs ->
  return (List.fold_left Uint32_set.union x xs)

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
let resp_text_code =
  let other_re =
    (* same as [text_re] but also exclude ']' *)
    Str.regexp "[^]\r\n\x80-\xff]+"
  in
  let badcharset =
    lopt
      begin
        char ' ' >> char '(' >> sep1 (char ' ') astring >>= fun xs -> char ')' >> return xs
      end >>= fun xs ->
    return (`BADCHARSET xs)
  in
  let permanentflags =
    char ' ' >> char '(' >>
    sep (char ' ') flag_perm >>= fun flags ->
    char ')' >>
    return (`PERMANENTFLAGS flags)
  in
  let uidnext = char ' ' >> nz_number >>= fun n -> return (`UIDNEXT (Uid.of_uint32 n)) in
  let uidvalidity = char ' ' >> nz_number >>= fun n -> return (`UIDVALIDITY (Uid.of_uint32 n)) in
  let unseen = char ' ' >> nz_number >>= fun n -> return (`UNSEEN (Seq.of_uint32 n)) in
  let appenduid =
    char ' ' >> nz_number >>= fun uidvalidity ->
    nz_number >>= fun uid -> return (`APPENDUID (Uid.of_uint32 uidvalidity, Uid.of_uint32 uid))
  in
  let copyuid =
    char ' ' >> nz_number >>= fun uidvalidity ->
    uid_set >>= fun src_uids ->
    char ' ' >> uid_set >>= fun dst_uids ->
    return (`COPYUID (Uid.of_uint32 uidvalidity, src_uids, dst_uids))
  in
  let highestmodseq =
    char ' ' >> mod_sequence_value >>= fun modseq -> return (`HIGHESTMODSEQ modseq)
  in
  let modified =
    char ' ' >> sequence_set >>= fun set -> return (`MODIFIED set)
  in
  let other a = sopt (char ' ' >> matches other_re) >>= fun s -> return (`OTHER (a, s)) in
  alt [
    string_ci "ALERT" >> return `ALERT;
    string_ci "BADCHARSET" >> badcharset;
    capability_data;
    string_ci "PARSE" >> return `PARSE;
    string_ci "PERMANENTFLAGS" >> permanentflags;
    string_ci "READ-ONLY" >> return `READ_ONLY;
    string_ci "READ-WRITE" >> return `READ_WRITE;
    string_ci "TRYCREATE" >> return `TRYCREATE;
    string_ci "UIDNEXT" >> uidnext;
    string_ci "UIDVALIDITY" >> uidvalidity;
    string_ci "UNSEEN" >> unseen;
    string_ci "APPENDUID" >> appenduid;
    string_ci "COPYUID" >> copyuid;
    string_ci "UIDNOTSTICKY" >> return `UIDNOTSTICKY;
    string_ci "COMPRESSIONACTIVE" >> return `COMPRESSIONACTIVE;
    string_ci "HIGHESTMODSEQ" >> highestmodseq;
    string_ci "NOMODSEQ" >> return `NOMODSEQ;
    string_ci "MODIFIED" >> modified;
    atom >>= other
  ]

(*
resp-text       = ["[" resp-text-code "]" SP] text
*)
let resp_text =
  opt (char '[' >> resp_text_code >>= fun rtc -> char ']' >> return rtc) >>= function
  | None ->
    sopt text >>= fun resp -> return (`NONE, resp)
  | Some code ->
    (* we make the space optional if there is no resp_text_code - Gimap needs this. *)
    sopt (char ' ' >> text) >>= fun resp -> return (code, resp)

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
  alt [
    begin string_ci "OK" >> char ' ' >> resp_text >>= fun rt -> return (`OK rt) end;
    begin string_ci "NO" >> char ' ' >> resp_text >>= fun rt -> return (`NO rt) end;
    begin string_ci "BAD" >> char ' ' >> resp_text >>= fun rt -> return (`BAD rt) end
  ]

(*
mbx-list-sflag  = "\Noselect" / "\Marked" / "\Unmarked"
                    ; Selectability flags; only one per LIST response
*)
let mbx_list_sflag =
  alt [
    string_ci "\\Noselect" >> return `Noselect;
    string_ci "\\Marked" >> return `Marked;
    string_ci "\\Unmarked" >> return `Unmarked
  ]

(*
mbx-list-oflag  = "\Noinferiors" / flag-extension
                    ; Other flags; multiple possible per LIST response
*)
let mbx_list_oflag =
  alt [(string_ci "\\Noinferiors" >> return `Noinferiors); flag_extension]

let mbx_list_oflag_no_sflag =
  opt mbx_list_sflag >>= function
  | None -> mbx_list_oflag
  | Some _ -> fail

(*
mbx-list-flags  = *(mbx-list-oflag SP) mbx-list-sflag
                  *(SP mbx-list-oflag) /
                  mbx-list-oflag *(SP mbx-list-oflag)
*)
let mbx_list_flags =
  alt [
    begin
      rep (mbx_list_oflag_no_sflag >>= fun oflags1 -> char ' ' >> return oflags1) >>= fun oflags1 ->
      mbx_list_sflag >>= fun sflag ->
      rep (char ' ' >> mbx_list_oflag) >>= fun oflags2 ->
      return {mbf_sflag = Some sflag; mbf_oflags = oflags1 @ oflags2}
    end;
    begin
      sep1 (char ' ') mbx_list_oflag >>= fun oflags ->
      return {mbf_sflag = None; mbf_oflags = oflags}
    end
  ]

let mailbox =
  let decode_mailbox_name s = try ImapUtils.decode_mutf7 s with _ -> s in
  astring >>= fun s -> return (decode_mailbox_name s)

(*
mailbox-list    = "(" [mbx-list-flags] ")" SP
                   (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
*)
let mailbox_list =
  char '(' >>
  opt mbx_list_flags >>= begin function
    | Some flags ->
        return flags
    | None ->
        return {mbf_sflag = None; mbf_oflags = []}
  end >>= fun mb_flag ->
  char ')' >> char ' ' >>
  alt [quoted_char; (nil >> return '\000')] >>= fun mb_delimiter ->
  char ' ' >> mailbox >>= fun mb_name ->
  return {mb_flag; mb_delimiter; mb_name}

(*
status-att      = "MESSAGES" / "RECENT" / "UIDNEXT" / "UIDVALIDITY" /
                  "UNSEEN"

status-att          =/ "HIGHESTMODSEQ"
                          ;; extends non-terminal defined in RFC 3501.
*)
let status_att_number =
  alt [
    begin
      string_ci "MESSAGES" >> char ' ' >> number' >>= fun n -> return (`MESSAGES n)
    end;
    begin
      string_ci "RECENT" >> char ' ' >> number' >>= fun n -> return (`RECENT n)
    end;
    begin
      string_ci "UIDNEXT" >> char ' ' >> nz_number >>= fun n -> return (`UIDNEXT (Uid.of_uint32 n))
    end;
    begin
      string_ci "UIDVALIDITY" >> char ' ' >> nz_number >>= fun n -> return (`UIDVALIDITY (Uid.of_uint32 n))
    end;
    begin
      string_ci "UNSEEN" >> char ' ' >> number' >>= fun n -> return (`UNSEEN n)
    end;
    begin
      string_ci "HIGHESTMODSEQ" >> char ' ' >> mod_sequence_value >>= fun n -> return (`HIGHESTMODSEQ n)
    end
  ]

(*
status-att-list =  status-att SP number *(SP status-att SP number)
*)
let status_att_list =
  sep1 (char ' ') status_att_number

let date_day_fixed =
  alt [(char ' ' >> digit); digits2]

let date_month =
  string_of_length 3 >>= fun s ->
  match String.capitalize s with
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
  digits2 >>= fun hours -> char ':' >>
  digits2 >>= fun minutes -> char ':' >>
  digits2 >>= fun seconds ->
  return (hours, minutes, seconds)

let zone =
  alt [(char '+' >> return 1); (char '-' >> return (-1))] >>= fun sign ->
  digits2 >>= fun hh ->
  digits2 >>= fun mm ->
  return (sign * (60 * hh + mm))

let date_time =
  char '\"' >>
  date_day_fixed >>= fun dt_day ->
  char '-' >> date_month >>= fun dt_month ->
  char '-' >> digits4 >>= fun dt_year ->
  char ' ' >> time >>= fun (dt_hour, dt_min, dt_sec) ->
  char ' ' >> zone >>= fun dt_zone ->
  char '\"' >>
  return {dt_day; dt_month; dt_year; dt_hour; dt_min; dt_sec; dt_zone}
    
let header_fld_name =
  astring

let header_list =
  char '(' >>
  sep1 (char ' ') header_fld_name >>= fun xs ->
  char ')' >>
  return xs

(*
section-msgtext = "HEADER" / "HEADER.FIELDS" [".NOT"] SP header-list /
                  "TEXT"
                    ; top-level or MESSAGE/RFC822 part
*)
let section_msgtext =
  let header_fields =
    opt (string_ci ".NOT") >>= fun has_not ->
    let has_not = match has_not with None -> false | Some _ -> true in
    char ' ' >> header_list >>= fun hdrs ->
    return (if has_not then `HEADER_FIELDS_NOT hdrs else `HEADER_FIELDS hdrs)
  in
  alt [
    (string_ci "HEADER.FIELDS" >> header_fields);
    (string_ci "HEADER" >> return `HEADER);
    (string_ci "TEXT" >> return `TEXT)
  ]

(*
section-part    = nz-number *("." nz-number)
                    ; body part nesting
*)
let section_part =
  nz_number' >>= fun n0 ->
  rep (char '.' >> nz_number') >>= fun ns ->
  return (n0, ns)

(*
section-text    = section-msgtext / "MIME"
                    ; text other than actual body part (headers, etc.)
*)
let section_text =
  alt [(string_ci "MIME" >> return `MIME); section_msgtext]

(*
section-spec    = section-msgtext / (section-part ["." section-text])
*)
let section_spec =
  alt [
    section_msgtext;
    begin
      section_part >>= fun (n0, ns) -> opt (char '.' >> section_text) >>= function
      | Some text ->
          return (`PART (n0, List.fold_right (fun p t -> `PART (p, t)) ns text))
      | None ->
          return (`PART (n0, List.fold_right (fun p t -> `PART (p, t)) ns `ALL))
    end
  ]

(*
section         = "[" [section-spec] "]"
*)
let section =
  alt [
    begin
      char '[' >>
      section_spec >>= fun x ->
      char ']' >>
      return x
    end;
    begin return `ALL end
  ]

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
let msg_att_static =
  (* let body = *)
  (*   let section_ (sec : ImapTypes.section_spec) = *)
  (*     opt (char '<' >> number' >>= fun x -> char '>' >> return x) >>= begin function *)
  (*       | Some n -> return (`PARTIAL (sec, n)) *)
  (*       | None -> return (sec :> msg_att_section) *)
  (*     end >>= fun sec -> *)
  (*     char ' ' >> nstring' >>= fun s -> *)
  (*     return (`BODYSECTION (sec, s)) *)
  (*   in *)
  (*   alt [ *)
  (*     (char ' ' >> ImapMime.body >>= fun b -> return (`BODY b)); *)
  (*     (section >>= section_) *)
  (*   ] *)
  (* in *)
  alt [
    begin
      string_ci "ENVELOPE" >> char ' ' >> ImapMime.envelope >>= fun e ->
      return (`ENVELOPE e)
    end;
    begin
      string_ci "INTERNALDATE" >> char ' ' >> date_time >>= fun dt ->
      return (`INTERNALDATE dt)
    end;
    begin
      string_ci "RFC822.HEADER" >> char ' ' >> nstring' >>= fun s ->
      return (`RFC822_HEADER s)
    end;
    begin
      string_ci "RFC822.TEXT" >> char ' ' >> nstring' >>= fun s ->
      return (`RFC822_TEXT s)
    end;
    begin
      string_ci "RFC822.SIZE" >> char ' ' >> number' >>= fun n ->
      return (`RFC822_SIZE n)
    end;
    begin
      string_ci "RFC822" >> char ' ' >> nstring' >>= fun s ->
      return (`RFC822 s)
    end;
    begin
      string_ci "BODYSTRUCTURE" >> char ' ' >> ImapMime.body >>= fun b ->
      return (`BODYSTRUCTURE b)
    end;
    (* string_ci "BODY" >> body; *)
    begin
      string_ci "UID" >> char ' ' >> nz_number >>= fun uid ->
      return (`UID (Uid.of_uint32 uid))
    end;
    begin
      string_ci "X-GM-MSGID" >> char ' ' >> uint64 >>= fun n ->
      return (`X_GM_MSGID (Gmsgid.of_uint64 n))
    end;
    begin
      string_ci "X-GM-THRID" >> char ' ' >> uint64 >>= fun n ->
      return (`X_GM_THRID (Gthrid.of_uint64 n))
    end
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
let msg_att_dynamic =
  let flags = char ' ' >> flag_list >>= fun flags -> return (`FLAGS flags) in
  let modseq = char ' ' >> char '(' >> permsg_modsequence >>= fun n -> char ')' >> return (`MODSEQ n) in
  let x_gm_labels =
    char ' ' >> char '(' >>
    sep (char ' ') astring >>= fun labs ->
    char ')' >>
    return (`X_GM_LABELS (List.map ImapUtils.decode_mutf7 labs))
  in
  alt [
    string_ci "FLAGS" >> flags;
    string_ci "MODSEQ" >> modseq;
    string_ci "X-GM-LABELS" >> x_gm_labels
  ]

(*
msg-att         = "(" (msg-att-dynamic / msg-att-static)
                   *(SP (msg-att-dynamic / msg-att-static)) ")"
*)
let msg_att =
  char '(' >>
  sep1 (char ' ') (alt [msg_att_static; msg_att_dynamic]) >>= fun xs ->
  char ')' >>
  return xs

(*
search-sort-mod-seq = "(" "MODSEQ" SP mod-sequence-value ")"
*)
let search_sort_mod_seq =
  char '(' >>
  string_ci "MODSEQ" >> char ' ' >> mod_sequence_value >>= fun x ->
  char ')' >>
  return x

(*
mailbox-data    =  "FLAGS" SP flag-list / "LIST" SP mailbox-list /
                   "LSUB" SP mailbox-list / "SEARCH" *(SP nz-number) /
                   "STATUS" SP mailbox SP "(" [status-att-list] ")" /
                   number SP "EXISTS" / number SP "RECENT"
   
mailbox-data        =/ "SEARCH" [1*(SP nz-number) SP
                          search-sort-mod-seq]
*)
let mailbox_data =
  let flags = char ' ' >> flag_list >>= fun flags -> return (`FLAGS flags) in
  let list_ = char ' ' >> mailbox_list >>= fun mb -> return (`LIST mb) in
  let lsub = char ' ' >> mailbox_list >>= fun mb -> return (`LSUB mb) in
  let search =
    rep (char ' ' >> nz_number) >>= function
    | [] ->
        return (`SEARCH ([], Modseq.zero))
    | ns ->
        opt (char ' ' >> search_sort_mod_seq) >>= fun modseq ->
        return (`SEARCH (ns, match modseq with None -> Modseq.zero | Some modseq -> modseq))
  in
  let status =
    char ' ' >> mailbox >>= fun mb ->
    char ' ' >> char '(' >> lopt status_att_list >>= fun att -> char ')' >>
    return (`STATUS {st_mailbox = mb; st_info_list = att})
  in
  let exists_or_recent n =
    char ' ' >> alt [
      (string_ci "EXISTS" >> return (`EXISTS n));
      (string_ci "RECENT" >> return (`RECENT n))
    ]
  in
  alt [
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
  nz_number >>= fun n ->
  char ' ' >>
  let n = Seq.of_uint32 n in
  alt [
    (string_ci "EXPUNGE" >> return (`EXPUNGE n));
    (string_ci "FETCH" >> char ' ' >> msg_att >>= fun att -> return (`FETCH (n, att)))
  ]
 
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
  tag >>= fun tag -> char ' ' >> resp_cond_state >>= fun resp ->
  string "\r\n" >>
  return (`TAGGED (tag, resp))

(*
resp-cond-auth  = ("OK" / "PREAUTH") SP resp-text
                    ; Authentication condition
*)
let resp_cond_auth =
  alt [
    begin
      string_ci "OK" >> char ' ' >> resp_text >>= fun rt ->
      return (`OK rt)
    end;
    begin
      string_ci "PREAUTH" >> char ' ' >> resp_text >>= fun rt ->
      return (`PREAUTH rt)
    end
  ]

(*
resp-cond-bye   = "BYE" SP resp-text
*)
let resp_cond_bye =
  string_ci "BYE" >> char ' ' >> resp_text >>= fun rt -> return (`BYE rt)

(*
greeting        = "*" SP (resp-cond-auth / resp-cond-bye) CRLF
*)
let greeting =
  char '*' >> char ' ' >> alt [resp_cond_auth; resp_cond_bye] >>= fun x ->
  string "\r\n" >>
  return x

(*
continue-req    = "+" SP (resp-text / base64) CRLF
*)
let continue_req =
  char '+' >> opt (char ' ') >> (* we allow an optional space *)
  alt [
    (base64 >>= fun b64 -> string "\r\n" >> return (`CONT_REQ (`BASE64 b64)));
    (resp_text >>= fun rt -> string "\r\n" >> return (`CONT_REQ (`TEXT rt)))
  ]

(*
id_params_list ::= "(" #(string SPACE nstring) ")" / nil
         ;; list of field value pairs
*)
let id_params_list =
  let param =
    imap_string >>= fun k ->
    char ' ' >>
    nstring >>= fun v ->
    return (k, v)
  in
  alt [
    begin
      char '(' >>
      sep1 (char ' ') param >>= fun xs ->
      char ')' >>
      return (ImapUtils.option_map (function (k, Some v) -> Some (k, v) | (_, None) -> None) xs)
    end;
    begin nil >> return [] end
  ]
  
(*
id_response ::= "ID" SPACE id_params_list
*)

let id_response =
  string_ci "ID" >> char ' ' >> id_params_list >>= fun params -> return (`ID params)

(*
Namespace_Response_Extension = SP string SP "(" string *(SP string) ")"
*)
let namespace_response_extension =
  char ' ' >>
  imap_string >>= fun n ->
  char ' ' >>
  char '(' >>
  sep1 (char ' ') imap_string >>= fun xs ->
  char ')' >>
  return (n, xs)
  
(*
Namespace = nil / "(" 1*( "(" string SP  (<"> QUOTED_CHAR <"> /
      nil) *(Namespace_Response_Extension) ")" ) ")"
*)
let namespace =
  alt [
    begin nil >> return [] end;
    begin
      char '(' >>
      rep1
        begin
          char '(' >> imap_string >>= fun ns_prefix ->
          char ' ' >>
          alt [quoted_char; nil >> return '\000'] >>= fun ns_delimiter ->
          rep namespace_response_extension >>= fun ns_extensions ->
          char ')' >>
          return {ns_prefix; ns_delimiter; ns_extensions}
        end >>= fun x ->
      char ')' >>
      return x
    end
  ]

(*
Namespace_Response = "*" SP "NAMESPACE" SP Namespace SP Namespace SP
      Namespace

      ; The first Namespace is the Personal Namespace(s)
      ; The second Namespace is the Other Users' Namespace(s)
      ; The third Namespace is the Shared Namespace(s)
*)
let namespace_response =
  string_ci "NAMESPACE" >>
  char ' ' >> namespace >>= fun personal ->
  char ' ' >> namespace >>= fun others ->
  char ' ' >> namespace >>= fun shared ->
  return (`NAMESPACE (personal, others, shared))

(*
enable-data   = "ENABLED" *(SP capability)
*)
let enable_data =
  string_ci "ENABLED" >>
  rep (char ' ' >> capability) >>= fun caps ->
  return (`ENABLED caps)

(*
response-data   = "*" SP (resp-cond-state / resp-cond-bye /
                  mailbox-data / message-data / capability-data) CRLF

response_data ::= "*" SPACE (resp_cond_state / resp_cond_bye /
         mailbox_data / message_data / capability_data / id_response)

response-data =/ "*" SP enable-data CRLF
*)
let response_data =
  char '*' >> char ' ' >>
  alt [
    resp_cond_state;
    resp_cond_bye;
    mailbox_data;
    message_data;
    capability_data;
    id_response;
    namespace_response;
    enable_data
  ] >>= fun x ->
  string "\r\n" >>
  return x

(*
response-fatal  = "*" SP resp-cond-bye CRLF
                    ; Server closes connection immediately
*)
let response_fatal =
  char '*' >> char ' ' >> resp_cond_bye >>= fun x -> string "\r\n" >> return x

(*
response-done   = response-tagged / response-fatal
*)
let response_done =
  alt [response_tagged; response_fatal]

let resp_data_or_resp_done =
  alt [response_data; response_done]

let cont_req_or_resp_data_or_resp_done =
  alt [continue_req; response_data; response_done]
