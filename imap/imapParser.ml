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

type 'a t =
    Buffer.t -> int -> 'a parse_result

let run p b i = p b i

let run_string p s =
  let b = Buffer.create 0 in
  Buffer.add_string b s;
  let rec loop = function
    | Ok (x, _) -> Some x
    | Fail _ -> None
    | Need k -> loop (k End)
  in
  loop (p b 0)

let bind p f b i =
  let rec loop =
    function
      Ok (x, i) -> f x b i
    | Fail _ as x -> x
    | Need k -> Need (fun inp -> loop (k inp))
  in
  loop (p b i)

let (>>=) = bind

let alt p q b i =
  let rec loop =
    function
      Ok _ as x -> x
    | Fail _ -> q b i
    | Need k -> Need (fun inp -> loop (k inp))
  in
  loop (p b i)

let ret x _ i =
  Ok (x, i)

let fail _ i =
  Fail i

let altn ps =
  List.fold_right alt ps fail

let (>>) p q =
  p >>= fun _ -> q

let app f p =
  p >>= fun x -> ret (f x)

let opt p x =
  alt p (ret x)

let some p =
  app (fun x -> Some x) p

let none p =
  app (fun _ -> None) p

let rec rep p =
  alt (rep1 p) (ret [])

and rep1 p =
  p >>= fun x -> rep p >>= fun xs -> ret (x :: xs)

let sep1 s p =
  p >>= fun x -> rep (s >> p) >>= fun xs -> ret (x :: xs)

let sep s p =
  alt (sep1 s p) (ret [])

let rec rep_ p =
  alt (rep1_ p) (ret ())

and rep1_ p =
  p >>= fun _ -> rep_ p

let take_from pos b i =
  Ok (Buffer.sub b pos (i - pos), i)

let rec char c b i =
  if i >= Buffer.length b then
    Need (function End -> Fail i | More -> char c b i)
  else if Buffer.nth b i = c then
    Ok (c, i + 1)
  else
    Fail i

let rec any_char b i =
  if i >= Buffer.length b then
    Need (function End -> Fail i | More -> any_char b i)
  else
    Ok (Buffer.nth b i, i + 1)

let satisfy f =
  any_char >>= fun c -> if f c then ret c else fail

let skip_while f =
  rep1_ (satisfy f)

let pos _ i =
  Ok (i, i)

let delay f x b i =
  f x b i

let skip n b i =
  let rec loop () =
    let left = Buffer.length b - i in
    if n <= left then
      Ok ((), i + n)
    else
      Need (function End -> Fail (i + left) | More -> loop ())
  in
  loop ()

let string_of_length n =
  pos >>= fun i -> skip n >> take_from i

let str u =
  string_of_length (String.length u) >>= fun s ->
  if String.uppercase s = String.uppercase u then ret s else fail

let rec accum f =
  pos >>= fun i -> skip_while f >> take_from i

let digit =
  satisfy (function '0' .. '9' -> true | _ -> false)

let nz_digit =
  satisfy (function '1' .. '9' -> true | _ -> false)

let nz_digits =
  pos >>= fun i -> nz_digit >> rep digit >> take_from i

let crlf =
  str "\r\n" >> ret ()

type extension_parser =
  { parse : 'a. 'a extension_kind -> 'a t }

let extension_list = ref []

let register_parser p = extension_list := p :: !extension_list

(*
number          = 1*DIGIT
                    ; Unsigned 32-bit integer
                    ; (0 <= n < 4,294,967,296)
*)
let number =
  app Uint32.of_string (accum (function '0' .. '9' -> true | _ -> false))

let rec eof b i =
  if i >= Buffer.length b then
    Need (function End -> Ok ((), i) | More -> eof b i)
  else
    Fail i

let number' =
  app int_of_string (accum (function '0' .. '9' -> true | _ -> false))

let escaped_char =
  any_char >>=
  function
    '\r' | '\n' | '\"' -> fail
  | '\\' -> satisfy (function '\\' | '\"' -> true | _ -> false)
  | '\x01' .. '\x7f' as c -> ret c
  | _ -> fail

let quoted_char =
  char '\"' >> escaped_char >>= fun c -> char '\"' >> ret c

let quoted =
  char '\"' >>= fun _ ->
  let b = Buffer.create 0 in
  rep_ (app (Buffer.add_char b) escaped_char) >>
  char '\"' >>= fun _ ->
  ret (Buffer.contents b)

(*
literal         = "{" number "}" CRLF *CHAR8
                    ; Number represents the number of CHAR8s
*)
let literal =
  char '{' >> number' >>= fun len -> char '}' >> str "\r\n" >>
  string_of_length len

(*
string          = quoted / literal
*)
let imap_string =
  alt quoted literal

(*
astring         = 1*ASTRING-CHAR / string
*)
let is_astring_char =
  function
    '\x80' .. '\xff' | '(' | ')'
  | '{' | ' ' | '\x00' .. '\x1f' | '\x7f'
  | '%' | '*' | '\\' | '\"' -> false
  | _ -> true

let astring =
  alt (accum is_astring_char) imap_string

(*
atom            = 1*ATOM-CHAR

ATOM-CHAR       = <any CHAR except atom-specials>

atom-specials   = "(" / ")" / "{" / SP / CTL / list-wildcards /
                  quoted-specials / resp-specials

quoted-specials = DQUOTE / "\\"

resp-specials   = "]"

list-wildcards  = "%" / "*"
*)
let is_atom_char = function
  | '\x80' .. '\xff'
  | '(' | ')' | '{' | ' ' | '\x00' .. '\x1f'
  | '\x7f' | '%' | '*' | '\\' | '\"' | ']' -> false
  | _ -> true

let atom =
  accum is_atom_char

(*
TEXT-CHAR       = <any CHAR except CR and LF>

text            = 1*TEXT-CHAR
*)
let is_text_char = function
  | '\r' | '\n' | '\x80' .. '\xff' -> false
  | _ -> true

let text =
  accum is_text_char

(*
nz-number       = digit-nz *DIGIT
                    ; Non-zero unsigned 32-bit integer
                    ; (0 < n < 4,294,967,296)
*)
let nz_number =
  app Uint32.of_string nz_digits

let nz_number' =
  app int_of_string nz_digits

let nil =
  str "nil" >> ret ()

let nstring =
  alt (some imap_string) (none nil)

let nstring' =
  app (function Some s -> s | None -> "") nstring

let digits1 =
  app (fun c -> Char.code c - Char.code '0') digit

let digits2 =
  pos >>= fun i -> digit >> digit >> app int_of_string (take_from i)

let digits4 =
  pos >>= fun i -> digit >> digit >> digit >> digit >> app int_of_string (take_from i)

let is_base64_char = function
  | 'a' .. 'z' | 'A' .. 'Z'
  | '0' .. '9' | '+' | '-' -> true
  | _ -> false

let base64_char =
  satisfy is_base64_char

let base64 =
  let base64_terminal =
    alt
      (base64_char >> base64_char >> base64_char >> str "=")
      (base64_char >> base64_char >> str "==")
  in
  pos >>= fun i ->
  rep_ (base64_char >> base64_char >> base64_char >> base64_char) >>
  base64_terminal >>  take_from i

let test p s =
  let b = Buffer.create 0 in
  let rec loop i = function
    | Ok (x, _) -> x
    | Fail _ -> failwith "parsing error"
    | Need k ->
        if i >= String.length s then
          loop i (k End)
        else
          begin
            Buffer.add_char b s.[i];
            loop (i+1) (k More)
          end
  in
  loop 0 (p b 0)

(** IMAP PARSER *)

let extension_parser : type a. a extension_kind -> a t = fun kind ->
  List.fold_right (fun p q -> alt (p.parse kind) q) !extension_list fail

let extension_parser : type a. a extension_kind -> a t = fun kind ->
  delay extension_parser kind

(*
auth-type       = atom
                    ; Defined by [SASL]
*)
let auth_type =
  atom >>= fun a -> ret (CAPABILITY_AUTH_TYPE a)

(*
capability      = ("AUTH=" auth-type) / atom
                    ; New capabilities MUST begin with "X" or be
                    ; registered with IANA as standard or
                    ; standards-track
*)
let capability =
  alt
    (str "AUTH=" >> auth_type)
    (atom >>= fun a -> ret (CAPABILITY_NAME a))

(*
capability-data = "CAPABILITY" *(SP capability) SP "IMAP4rev1"
                  *(SP capability)
                    ; Servers MUST implement the STARTTLS, AUTH=PLAIN,
                    ; and LOGINDISABLED capabilities
                    ; Servers which offer RFC 1730 compatibility MUST
                    ; list "IMAP4" as the first capability.
*)
let capability_data =
  str "CAPABILITY" >> char ' ' >> sep1 (char ' ') capability

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
  char '\\' >> atom

(*
flag-keyword    = atom
*)
let flag_keyword =
  atom >>= fun s -> ret (FLAG_KEYWORD s)

(*
flag            = "\Answered" / "\Flagged" / "\Deleted" /
                  "\Seen" / "\Draft" / flag-keyword / flag-extension
                    ; Does not include "\Recent"
*)
let flag =
  alt
    begin
      char '\\' >> atom >>= fun s ->
      ret
        (match String.capitalize s with
         | "Answered" -> FLAG_ANSWERED
         | "Flagged" -> FLAG_FLAGGED
         | "Deleted" -> FLAG_DELETED
         | "Seen" -> FLAG_SEEN
         | "Draft" -> FLAG_DRAFT
         | _ -> FLAG_EXTENSION s)
    end
    flag_keyword

let flag_fetch =
  alt (flag >>= fun flag -> ret (FLAG_FETCH_OTHER flag)) (str "\\Recent" >> ret FLAG_FETCH_RECENT)

(*
flag-perm       = flag / "\*"
*)
let flag_perm =
  alt (app (fun f -> FLAG_PERM_FLAG f) flag) (str "\\*" >> ret FLAG_PERM_ALL)

let flag_list =
  char '(' >> sep (char ' ') flag >>= fun xs -> char ')' >> ret xs

(*
uniqueid        = nz-number
                    ; Strictly ascending
*)
let uniqueid =
  nz_number

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
  alt nz_number (char '*' >> ret Uint32.zero)

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
  ret (ImapSet.interval x y)

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
  let elem = alt (seq_number >>= fun x -> ret (ImapSet.single x)) seq_range in
  elem >>= fun x ->
  rep (char ',' >> elem) >>= fun xs ->
  ret (List.fold_left ImapSet.union x xs)

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
*)
let is_text_other_char c =
  is_text_char c && (c <> ']')

let resp_text_code =
  let alert = str "ALERT" >> ret RESP_TEXT_CODE_ALERT in
  let badcharset =
    str "BADCHARSET" >>
    opt (char ' ' >> char '(' >> sep1 (char ' ') astring >>= fun xs -> char ')' >> ret xs) [] >>= fun xs ->
    ret (RESP_TEXT_CODE_BADCHARSET xs)
  in
  let capability = capability_data >>= fun c -> ret (RESP_TEXT_CODE_CAPABILITY_DATA c) in
  let parse = str "PARSE" >> ret RESP_TEXT_CODE_PARSE in
  let permanentflags =
    str "PERMANENTFLAGS" >> char ' ' >> char '(' >>
    sep (char ' ') flag_perm >>= fun flags ->
    char ')' >>
    ret (RESP_TEXT_CODE_PERMANENTFLAGS flags)
  in
  let read_only = str "READ-ONLY" >> ret RESP_TEXT_CODE_READ_ONLY in
  let read_write = str "READ-WRITE" >> ret RESP_TEXT_CODE_READ_WRITE in
  let trycreate = str "TRYCREATE" >> ret RESP_TEXT_CODE_TRYCREATE in
  let uidnext = str "UIDNEXT" >> char ' ' >> nz_number >>= fun n -> ret (RESP_TEXT_CODE_UIDNEXT n) in
  let uidvalidity = str "UIDVALIDITY" >> char ' ' >> nz_number >>= fun n -> ret (RESP_TEXT_CODE_UIDVALIDITY n) in
  let unseen = str "UNSEEN" >> char ' ' >> nz_number >>= fun n -> ret (RESP_TEXT_CODE_UNSEEN n) in
  let extension = extension_parser RESP_TEXT_CODE >>= fun e -> ret (RESP_TEXT_CODE_EXTENSION e) in
  let other =
    atom >>= fun a -> opt (char ' ' >> some (accum is_text_other_char)) None
    >>= fun s -> ret (RESP_TEXT_CODE_OTHER (a, s))
  in
  altn
    [alert; badcharset; capability; parse; permanentflags; read_only;
     read_write; trycreate; uidnext; uidvalidity; unseen; extension; other]
    (* (str "COMPRESSIONACTIVE" >> ret RESP_TEXT_CODE_COMPRESSIONACTIVE); *)

(*
resp-text       = ["[" resp-text-code "]" SP] text
*)
let resp_text =
  opt (char '[' >> resp_text_code >>= fun rsp_code -> char ']' >> ret (Some rsp_code)) None
  >>= function
  | None ->
      opt text "" >>= fun rsp_text -> ret {rsp_code = RESP_TEXT_CODE_NONE; rsp_text}
  | Some rsp_code ->
      (* we make the space optional if there is no resp_text_code - Gimap needs this. *)
      opt (char ' ' >> text) "" >>= fun rsp_text -> ret {rsp_code; rsp_text}

(*
resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
                    ; Status condition
*)
let resp_cond_state =
  altn [
    (str "OK" >> ret RESP_COND_STATE_OK);
    (str "NO" >> ret RESP_COND_STATE_NO);
    (str "BAD" >> ret RESP_COND_STATE_BAD)
  ]
  >>= fun rsp_type ->
  char ' ' >> resp_text >>= fun rsp_text ->
  ret {rsp_type; rsp_text}

(*
mbx-list-sflag  = "\Noselect" / "\Marked" / "\Unmarked"
                    ; Selectability flags; only one per LIST response
*)
let mbx_list_sflag =
  altn [
    str "\\Noselect" >> ret MBX_LIST_SFLAG_NOSELECT;
    str "\\Marked" >> ret MBX_LIST_SFLAG_MARKED;
    str "\\Unmarked" >> ret MBX_LIST_SFLAG_UNMARKED
  ]

(*
mbx-list-oflag  = "\Noinferiors" / flag-extension
                    ; Other flags; multiple possible per LIST response
*)
let mbx_list_oflag =
  alt
    (str "\\Noinferiors" >> ret MBX_LIST_OFLAG_NOINFERIORS)
    (flag_extension >>= fun s -> ret (MBX_LIST_OFLAG_EXT s))

let mbx_list_oflag_no_sflag =
  opt (mbx_list_sflag >> ret true) false >>= function
  | false -> mbx_list_oflag
  | true -> fail

(*
mbx-list-flags  = *(mbx-list-oflag SP) mbx-list-sflag
                  *(SP mbx-list-oflag) /
                  mbx-list-oflag *(SP mbx-list-oflag)
*)
let mbx_list_flags =
  alt
    begin
      rep (mbx_list_oflag_no_sflag >>= fun oflags1 -> char ' ' >> ret oflags1) >>= fun oflags1 ->
      mbx_list_sflag >>= fun sflag ->
      rep (char ' ' >> mbx_list_oflag) >>= fun oflags2 ->
      ret {mbf_sflag = Some sflag; mbf_oflags = oflags1 @ oflags2}
    end
    begin
      sep1 (char ' ') mbx_list_oflag >>= fun oflags ->
      ret {mbf_sflag = None; mbf_oflags = oflags}
    end

let mailbox =
  let decode_mailbox_name s = try ImapUtils.decode_mutf7 s with _ -> s in
  astring >>= fun s -> ret (decode_mailbox_name s)

(*
mailbox-list    = "(" [mbx-list-flags] ")" SP
                   (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
*)
let mailbox_list =
  char '(' >>
  opt mbx_list_flags {mbf_sflag = None; mbf_oflags = []} >>= fun mb_flag ->
  char ')' >> char ' ' >>
  alt (some quoted_char) (nil >> ret None) >>= fun mb_delimiter ->
  char ' ' >> mailbox >>= fun mb_name ->
  ret {mb_flag; mb_delimiter; mb_name}

(*
status-att      = "MESSAGES" / "RECENT" / "UIDNEXT" / "UIDVALIDITY" /
                  "UNSEEN"
*)
let status_att =
  let messages = str "MESSAGES" >> char ' ' >> number' >>= fun n -> ret (STATUS_ATT_MESSAGES n) in
  let recent = str "RECENT" >> char ' ' >> number' >>= fun n -> ret (STATUS_ATT_RECENT n) in
  let uidnext = str "UIDNEXT" >> char ' ' >> nz_number >>= fun n -> ret (STATUS_ATT_UIDNEXT n) in
  let uidvalidity = str "UIDVALIDITY" >> char ' ' >> nz_number >>= fun n -> ret (STATUS_ATT_UIDVALIDITY n) in
  let unseen = str "UNSEEN" >> char ' ' >> number' >>= fun n -> ret (STATUS_ATT_UNSEEN n) in
  let extension = extension_parser STATUS_ATT >>= fun e -> ret (STATUS_ATT_EXTENSION e) in
  altn [ messages; recent; uidnext; uidvalidity; unseen; extension ]

let address =
  char '(' >>
  nstring' >>= fun ad_personal_name -> char ' ' >>
  nstring' >>= fun ad_source_route -> char ' ' >>
  nstring' >>= fun ad_mailbox_name -> char ' ' >>
  nstring' >>= fun ad_host_name ->
  char ')' >>
  ret { ad_personal_name; ad_source_route; ad_mailbox_name; ad_host_name }

let address_list =
  alt
    (char '(' >> sep1 (char ' ') address >>= fun xs -> char ')' >> ret xs)
    (nil >> ret [])

let envelope =
  char '(' >>
  nstring' >>= fun env_date -> char ' ' >>
  nstring' >>= fun env_subject -> char ' ' >>
  address_list >>= fun env_from -> char ' ' >>
  address_list >>= fun env_sender -> char ' ' >>
  address_list >>= fun env_reply_to -> char ' ' >>
  address_list >>= fun env_to -> char ' ' >>
  address_list >>= fun env_cc -> char ' ' >>
  address_list >>= fun env_bcc -> char ' ' >>
  nstring' >>= fun env_in_reply_to -> char ' ' >>
  nstring' >>= fun env_message_id ->
  char ')' >>
  ret {
    env_date; env_subject; env_from; env_sender;
    env_reply_to; env_to; env_cc; env_bcc; env_in_reply_to;
    env_message_id
  }

let media_subtype =
  imap_string

(*
media-basic     = ((DQUOTE ("APPLICATION" / "AUDIO" / "IMAGE" /
                  "MESSAGE" / "VIDEO") DQUOTE) / string) SP
                  media-subtype
                    ; Defined in [MIME-IMT]
*)
let media_basic =
  let table = [
    "APPLICATION", MEDIA_BASIC_APPLICATION;
    "AUDIO", MEDIA_BASIC_AUDIO;
    "IMAGE", MEDIA_BASIC_IMAGE;
    "MESSAGE", MEDIA_BASIC_MESSAGE;
    "VIDEO", MEDIA_BASIC_VIDEO
  ]
  in
  let media_basic' =
    imap_string >>= fun s ->
    ret (try List.assoc (String.uppercase s) table with Not_found -> MEDIA_BASIC_OTHER s)
  in
  media_basic' >>= fun med_basic_type -> char ' ' >> media_subtype >>= fun med_basic_subtype ->
  ret {med_basic_type; med_basic_subtype}

(*
body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil
*)
let body_fld_param =
  let param = imap_string >>= fun k -> char ' ' >> imap_string >>= fun v -> ret (k, v) in
  alt
    (char '(' >> sep1 (char ' ') param >>= fun xs -> char ')' >> ret xs)
    (nil >> ret [])

(*
body-fld-enc    = (DQUOTE ("7BIT" / "8BIT" / "BINARY" / "BASE64"/
                  "QUOTED-PRINTABLE") DQUOTE) / string
*)
let body_fld_enc =
  let table = [
    "7BIT", BODY_FLD_ENC_7BIT;
    "8BIT", BODY_FLD_ENC_8BIT;
    "BINARY", BODY_FLD_ENC_BINARY;
    "BASE64", BODY_FLD_ENC_BASE64;
    "QUOTED-PRINTABLE", BODY_FLD_ENC_QUOTED_PRINTABLE
  ]
  in
  imap_string >>= fun s ->
  ret (try List.assoc (String.uppercase s) table with Not_found -> BODY_FLD_ENC_OTHER s)

(*
body-fld-id     = nstring
*)
let body_fld_id =
  nstring

(*
body-fld-desc   = nstring
*)
let body_fld_desc =
  nstring

(*
body-fld-octets = number
*)
let body_fld_octets =
  number'

(*
body-fields     = body-fld-param SP body-fld-id SP body-fld-desc SP
                  body-fld-enc SP body-fld-octets
*)
let body_fields =
  body_fld_param >>= fun bd_parameter ->
  char ' ' >> body_fld_id >>= fun bd_id ->
  char ' ' >> body_fld_desc >>= fun bd_description ->
  char ' ' >> body_fld_enc >>= fun bd_encoding ->
  char ' ' >> body_fld_octets >>= fun bd_size ->
  ret {bd_parameter; bd_id; bd_description; bd_encoding; bd_size}

(*
media-message   = DQUOTE "MESSAGE" DQUOTE SP DQUOTE "RFC822" DQUOTE
                    ; Defined in [MIME-IMT]
*)
let media_message =
  char '\"' >> str "MESSAGE" >> char '\"' >> char ' ' >>
  char '\"' >> str "RFC822" >> char '\"'

(*
media-text      = DQUOTE "TEXT" DQUOTE SP media-subtype
                    ; Defined in [MIME-IMT]
*)
let media_text =
  char '\"' >> str "TEXT" >> char '\"' >> char ' ' >> media_subtype

(*
body-fld-md5    = nstring
*)
let body_fld_md5 =
  nstring

(*
body-fld-dsp    = "(" string SP body-fld-param ")" / nil
*)
let body_fld_dsp =
  alt
    begin
      char '(' >>
      imap_string >>= fun dsp_type ->
      char ' ' >>
      body_fld_param >>= fun dsp_attributes ->
      char ')' >>
      ret (Some {dsp_type; dsp_attributes})
    end
    (nil >> ret None)

(*
body-fld-lang   = nstring / "(" string *(SP string) ")"
*)
let body_fld_lang =
  alt
    (char '(' >> sep1 (char ' ') imap_string >>= fun xs -> char ')' >> ret (BODY_FLD_LANG_LIST xs))
    (nstring >>= fun s -> ret (BODY_FLD_LANG_SINGLE s))

(*
body-extension  = nstring / number /
                   "(" body-extension *(SP body-extension) ")"
                    ; Future expansion.  Client implementations
                    ; MUST accept body-extension fields.  Server
                    ; implementations MUST NOT generate
                    ; body-extension fields except as defined by
                    ; future standard or standards-track
                    ; revisions of this specification.
*)
let rec body_extension () =
  altn [
    (char '(' >> sep1 (char ' ') (delay body_extension ()) >>= fun xs ->
     char ')' >> ret (BODY_EXTENSION_LIST xs));
    (number >>= fun n -> ret (BODY_EXTENSION_NUMBER n));
    (nstring >>= fun s -> ret (BODY_EXTENSION_NSTRING s))
  ]

let body_fld_loc =
  nstring

(*
body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang
                  [SP body-fld-loc *(SP body-extension)]]]
                    ; MUST NOT be returned on non-extensible
                    ; "BODY" fetch
*)
let body_ext_1part =
  body_fld_md5 >>= fun bd_md5 ->
  opt
    begin
      char ' ' >> body_fld_dsp >>= fun bd_disposition ->
      opt
        begin
          char ' ' >> some body_fld_lang >>= fun bd_language ->
          opt
            begin
              char ' ' >> body_fld_loc >>= fun bd_loc ->
              rep (char ' ' >> delay body_extension ()) >>= fun bd_extension_list ->
              ret {bd_md5; bd_disposition; bd_language; bd_loc; bd_extension_list}
            end
            {bd_md5; bd_disposition; bd_language; bd_loc = None; bd_extension_list = []}
        end
        {bd_md5; bd_disposition; bd_language = None; bd_loc = None; bd_extension_list = []}
    end
    {bd_md5 = None; bd_disposition = None; bd_language = None; bd_loc = None; bd_extension_list = []}

(*
body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang
                  [SP body-fld-loc *(SP body-extension)]]]
                    ; MUST NOT be returned on non-extensible
                    ; "BODY" fetch
*)
let body_ext_mpart =
  char ' ' >> body_fld_param >>= fun bd_parameter ->
  opt
    begin
      char ' ' >> body_fld_dsp >>= fun bd_disposition ->
      opt
        begin
          char ' ' >> some body_fld_lang >>= fun bd_language ->
          opt
            begin
              char ' ' >> body_fld_loc >>= fun bd_loc ->
              rep (char ' ' >> delay body_extension ()) >>= fun bd_extension_list ->
              ret {bd_parameter; bd_disposition; bd_language; bd_loc; bd_extension_list}
            end
            {bd_parameter; bd_disposition; bd_language; bd_loc = None; bd_extension_list = []}
        end
        {bd_parameter; bd_disposition; bd_language = None; bd_loc = None; bd_extension_list = []}
    end
    {bd_parameter; bd_disposition = None; bd_language = None; bd_loc = None; bd_extension_list = []}

(*
body-type-basic = media-basic SP body-fields
                    ; MESSAGE subtype MUST NOT be "RFC822"
*)
let rec body_type_basic () =
  media_basic >>= fun bd_media_basic ->
  char ' ' >> body_fields >>= fun bd_fields ->
  ret {bd_media_basic; bd_fields}

(*
body-type-msg   = media-message SP body-fields SP envelope
                  SP body SP body-fld-lines
*)
and body_type_msg () =
  media_message >> char ' ' >> body_fields >>= fun bd_fields ->
  char ' ' >> envelope >>= fun bd_envelope ->
  char ' ' >> delay body () >>= fun bd_body ->
  ret {bd_fields; bd_envelope; bd_body}

(*
body-type-text  = media-text SP body-fields SP body-fld-lines
*)
and body_type_text () =
  media_text >>= fun bd_media_text ->
  char ' ' >> body_fields >>= fun bd_fields ->
  char ' ' >> number' >>= fun bd_lines ->
  ret {bd_media_text; bd_fields; bd_lines}

(*
body-type-1part = (body-type-basic / body-type-msg / body-type-text)
                  [SP body-ext-1part]
*)
and body_type_1part () =
  altn [
    (delay body_type_msg () >>= fun b -> ret (BODY_TYPE_1PART_MSG b));
    (delay body_type_text () >>= fun b -> ret (BODY_TYPE_1PART_TEXT b));
    (delay body_type_basic () >>= fun b -> ret (BODY_TYPE_1PART_BASIC b))
  ] >>= fun bd_data ->
  opt
    (char ' ' >> body_ext_1part)
    {bd_md5 = None; bd_disposition = None; bd_language = None; bd_loc = None; bd_extension_list = []}
  >>= fun bd_ext_1part ->
  ret {bd_data; bd_ext_1part}

(*
body-type-mpart = 1*body SP media-subtype
                  [SP body-ext-mpart]
*)
and body_type_mpart () =
  rep1 (delay body ()) >>= fun bd_list ->
  char ' ' >> media_subtype >>= fun bd_media_subtype ->
  body_ext_mpart >>= fun bd_ext_mpart ->
  ret {bd_list; bd_media_subtype; bd_ext_mpart}

(*
body            = "(" (body-type-1part / body-type-mpart) ")"
*)
and body () =
  char '(' >>
  alt
    (delay body_type_1part () >>= fun b -> ret (BODY_1PART b))
    (delay body_type_mpart () >>= fun b -> ret (BODY_MPART b))
  >>= fun b ->
  char ')' >>
  ret b

(*
status-att-list =  status-att SP number *(SP status-att SP number)
*)
let date_day_fixed =
  alt (char ' ' >> digits1) digits2

let date_month =
  app String.capitalize (string_of_length 3) >>= function
  | "Jan" -> ret 1
  | "Feb" -> ret 2
  | "Mar" -> ret 3
  | "Apr" -> ret 4
  | "May" -> ret 5
  | "Jun" -> ret 6
  | "Jul" -> ret 7
  | "Aug" -> ret 8
  | "Sep" -> ret 9
  | "Oct" -> ret 10
  | "Nov" -> ret 11
  | "Dec" -> ret 12
  | _ -> fail

let time =
  digits2 >>= fun hours -> char ':' >>
  digits2 >>= fun minutes -> char ':' >>
  digits2 >>= fun seconds ->
  ret (hours, minutes, seconds)

let zone =
  alt (char '+' >> ret 1) (char '-' >> ret (-1)) >>= fun sign ->
  digits2 >>= fun hh ->
  digits2 >>= fun mm ->
  ret (sign * (60 * hh + mm))

let date_time =
  char '\"' >>
  date_day_fixed >>= fun dt_day ->
  char '-' >> date_month >>= fun dt_month ->
  char '-' >> digits4 >>= fun dt_year ->
  char ' ' >> time >>= fun (dt_hour, dt_min, dt_sec) ->
  char ' ' >> zone >>= fun dt_zone ->
  char '\"' >>
  ret {dt_day; dt_month; dt_year; dt_hour; dt_min; dt_sec; dt_zone}

let header_fld_name =
  astring

let header_list =
  char '(' >>
  sep1 (char ' ') header_fld_name >>= fun xs ->
  char ')' >>
  ret xs

(*
section-msgtext = "HEADER" / "HEADER.FIELDS" [".NOT"] SP header-list /
                  "TEXT"
                    ; top-level or MESSAGE/RFC822 part
*)
let section_msgtext =
  let header_fields =
    opt (str ".NOT" >> ret true) false >>= fun has_not ->
    char ' ' >> header_list >>= fun hs ->
    if has_not then
      ret (SECTION_MSGTEXT_HEADER_FIELDS_NOT hs)
    else
      ret (SECTION_MSGTEXT_HEADER_FIELDS hs)
  in
  altn [
    (str "HEADER.FIELDS" >> header_fields);
    (str "HEADER" >> ret SECTION_MSGTEXT_HEADER);
    (str "TEXT" >> ret SECTION_MSGTEXT_TEXT)
  ]

(*
section-part    = nz-number *("." nz-number)
                    ; body part nesting
*)
let section_part =
  sep1 (char '.') nz_number'

(*
section-text    = section-msgtext / "MIME"
                    ; text other than actual body part (headers, etc.)
*)
let section_text =
  alt
    (str "MIME" >> ret SECTION_TEXT_MIME)
    (section_msgtext >>= fun t -> ret (SECTION_TEXT_MSGTEXT t))

(*
section-spec    = section-msgtext / (section-part ["." section-text])
*)
let section_spec =
  alt
    (section_msgtext >>= fun s -> ret (SECTION_SPEC_SECTION_MSGTEXT s))
    begin
      section_part >>= fun p -> opt (char '.' >> some section_text) None >>= fun t ->
      ret (SECTION_SPEC_SECTION_PART (p, t))
    end

(*
section         = "[" [section-spec] "]"
*)
let section =
  char '[' >> opt (some section_spec) None >>= fun x -> char ']' >> ret x

let uint64 =
  accum (function '0' .. '9' -> true | _ -> false) >>= fun s ->
  try ret (Uint64.of_string s) with _ -> fail

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
  let body_ =
    let section_ sec_section =
      opt (char '<' >> some number' >>= fun x -> char '>' >> ret x) None >>= fun sec_origin_octet ->
      char ' ' >> nstring' >>= fun sec_body_part ->
      ret (MSG_ATT_BODY_SECTION {sec_section; sec_origin_octet; sec_body_part})
    in
    altn [
      (char ' ' >> delay body () >>= fun b -> ret (MSG_ATT_BODY b));
      (section >>= section_)
    ]
  in
  altn [
    (str "ENVELOPE" >> char ' ' >> envelope >>= fun e -> ret (MSG_ATT_ENVELOPE e));
    (str "INTERNALDATE" >> char ' ' >> date_time >>= fun dt -> ret (MSG_ATT_INTERNALDATE dt));
    (str "RFC822.HEADER" >> char ' ' >> nstring' >>= fun s -> ret (MSG_ATT_RFC822_HEADER s));
    (str "RFC822.TEXT" >> char ' ' >> nstring' >>= fun s -> ret (MSG_ATT_RFC822_TEXT s));
    (str "RFC822.SIZE" >> char ' ' >> number' >>= fun n -> ret (MSG_ATT_RFC822_SIZE n));
    (str "RFC822" >> char ' ' >> nstring' >>= fun s -> ret (MSG_ATT_RFC822 s));
    (str "BODYSTRUCTURE" >> char ' ' >> delay body () >>= fun b -> ret (MSG_ATT_BODYSTRUCTURE b));
    (str "BODY" >> body_);
    (str "UID" >> char ' ' >> nz_number >>= fun uid -> ret (MSG_ATT_UID uid))
  (*   (str "X-GM-THRID" >> char ' ' >> uint64 >>= fun n -> ret (MSG_ATT_X_GM_THRID (Gthrid.of_uint64 n))) *)
  ]

(*
msg-att-dynamic = "FLAGS" SP "(" [flag-fetch *(SP flag-fetch)] ")"
                    ; MAY change for a message
*)
let msg_att_dynamic =
  str "FLAGS" >> char ' ' >> char '(' >>
  sep (char ' ') flag_fetch >>= fun flags ->
  char ')' >>
  ret flags

(*
msg-att         = "(" (msg-att-dynamic / msg-att-static)
                   *(SP (msg-att-dynamic / msg-att-static)) ")"
*)
let msg_att =
  char '(' >>
  sep1 (char ' ')
    begin
      altn
        [ (msg_att_static >>= fun a -> ret (MSG_ATT_ITEM_STATIC a));
          (msg_att_dynamic >>= fun a -> ret (MSG_ATT_ITEM_DYNAMIC a));
          (extension_parser FETCH_DATA >>= fun e -> ret (MSG_ATT_ITEM_EXTENSION e)) ]
    end
  >>= fun xs ->
  char ')' >>
  ret xs

let status_info =
  alt status_att (extension_parser STATUS_ATT >>= fun e -> ret (STATUS_ATT_EXTENSION e))

let mailbox_data_flags =
  str "FLAGS" >> char ' ' >> flag_list >>= fun flags ->
  ret (MAILBOX_DATA_FLAGS flags)

let mailbox_data_list =
  str "LIST" >> char ' ' >> mailbox_list >>= fun mb ->
  ret (MAILBOX_DATA_LIST mb)

let mailbox_data_lsub =
  str "LSUB" >> char ' ' >> mailbox_list >>= fun mb ->
  ret (MAILBOX_DATA_LSUB mb)

let mailbox_data_search =
  str "SEARCH" >> rep (char ' ' >> nz_number) >>= fun ns ->
  ret (MAILBOX_DATA_SEARCH ns)

let mailbox_data_status =
  str "STATUS" >> char ' ' >> mailbox >>= fun st_mailbox ->
  char ' ' >> char '(' >> sep (char ' ') status_info >>= fun st_info_list ->
  char ')' >>
  ret (MAILBOX_DATA_STATUS {st_mailbox; st_info_list})

let mailbox_data_exists =
  number' >>= fun n -> char ' ' >> str "EXISTS" >>
  ret (MAILBOX_DATA_EXISTS n)

let mailbox_data_recent =
  number' >>= fun n -> char ' ' >> str "RECENT" >>
  ret (MAILBOX_DATA_RECENT n)

let mailbox_data_extension_data =
  extension_parser MAILBOX_DATA >>= fun e -> ret (MAILBOX_DATA_EXTENSION_DATA e)

(*
mailbox-data    =  "FLAGS" SP flag-list / "LIST" SP mailbox-list /
                   "LSUB" SP mailbox-list / "SEARCH" *(SP nz-number) /
                   "STATUS" SP mailbox SP "(" [status-att-list] ")" /
                   number SP "EXISTS" / number SP "RECENT"
*)
let mailbox_data =
  altn
    [ mailbox_data_flags; mailbox_data_list; mailbox_data_lsub; mailbox_data_search;
      mailbox_data_status; mailbox_data_exists; mailbox_data_recent; mailbox_data_extension_data ]

(*
message-data    = nz-number SP ("EXPUNGE" / ("FETCH" SP msg-att))
*)
let message_data =
  nz_number >>= fun n ->
  char ' ' >>
  alt
    (str "EXPUNGE" >> ret (MESSAGE_DATA_EXPUNGE n))
    (str "FETCH" >> char ' ' >> msg_att >>= fun att -> ret (MESSAGE_DATA_FETCH (att, n)))

(*
tag             = 1*<any ASTRING-CHAR except "+">
*)
let is_tag_char =
  function
    '\x80' .. '\xff' | '(' | ')'
  | '{' | ' ' | '\x00' .. '\x1f'
  | '\x7f' | '%' | '*' | '\\'
  | '\"' | '+' -> false
  | _ -> true

let tag =
  accum is_tag_char

(*
response-tagged = tag SP resp-cond-state CRLF
*)
let response_tagged =
  tag >>= fun rsp_tag -> char ' ' >> resp_cond_state >>= fun rsp_cond_state -> str "\r\n" >>
  ret {rsp_tag; rsp_cond_state}

(*
resp-cond-auth  = ("OK" / "PREAUTH") SP resp-text
                    ; Authentication condition
*)
let resp_cond_auth =
  alt (str "OK" >> ret RESP_COND_AUTH_OK) (str "PREAUTH" >> ret RESP_COND_AUTH_PREAUTH) >>=
  fun rsp_type ->
  char ' ' >> resp_text >>= fun rsp_text ->
  ret {rsp_type; rsp_text}

(*
resp-cond-bye   = "BYE" SP resp-text
*)
let resp_cond_bye =
  str "BYE" >> char ' ' >> resp_text

(*
greeting        = "*" SP (resp-cond-auth / resp-cond-bye) CRLF
*)
let greeting =
  char '*' >> char ' ' >>
  alt
    (resp_cond_auth >>= fun r -> ret (GREETING_RESP_COND_AUTH r))
    (resp_cond_bye >>= fun r -> ret (GREETING_RESP_COND_BYE r))
  >>= fun x ->
  str "\r\n" >>
  ret x

(*
continue-req    = "+" SP (resp-text / base64) CRLF
*)
let continue_req =
  char '+' >> opt (char ' ') '\000' >> (* we allow an optional space *)
  alt
    (base64 >>= fun b64 -> str "\r\n" >> ret (CONTINUE_REQ_BASE64 b64))
    (resp_text >>= fun rt -> str "\r\n" >> ret (CONTINUE_REQ_TEXT rt))

(*
response-data   = "*" SP (resp-cond-state / resp-cond-bye /
                  mailbox-data / message-data / capability-data) CRLF
*)
let response_data =
  char '*' >> char ' ' >>
  altn [
    (resp_cond_state >>= fun r -> ret (RESP_DATA_COND_STATE r));
    (resp_cond_bye >>= fun r -> ret (RESP_DATA_COND_BYE r));
    (mailbox_data >>= fun r -> ret (RESP_DATA_MAILBOX_DATA r));
    (message_data >>= fun r -> ret (RESP_DATA_MESSAGE_DATA r));
    (capability_data >>= fun r -> ret (RESP_DATA_CAPABILITY_DATA r));
    (extension_parser RESPONSE_DATA >>= fun e -> ret (RESP_DATA_EXTENSION_DATA e))
    (* namespace_response; *)
  ]
  >>= fun x ->
  str "\r\n" >>
  ret x

(*
response-fatal  = "*" SP resp-cond-bye CRLF
                    ; Server closes connection immediately
*)
let response_fatal =
  char '*' >> char ' ' >> resp_cond_bye >>= fun x -> str "\r\n" >> ret x

(*
response-done   = response-tagged / response-fatal
*)
let response_done =
  alt
    (response_tagged >>= fun r -> ret (RESP_DONE_TAGGED r))
    (response_fatal >>= fun r -> ret (RESP_DONE_FATAL r))

let cont_req_or_resp_data =
  alt
    (continue_req >>= fun r -> ret (RESP_CONT_REQ r))
    (response_data >>= fun r -> ret (RESP_CONT_DATA r))

let response =
  rep cont_req_or_resp_data >>= fun rsp_cont_req_or_resp_data_list ->
  response_done >>= fun rsp_resp_done ->
  ret {rsp_cont_req_or_resp_data_list; rsp_resp_done}

(* Parsing headers *)

type state =
  | START
  | CR
  | LF
  | WSP
  | OUT

let peek_char b i =
  let rec loop () =
    if i >= Buffer.length b then
      Need (function End -> Ok (None, i) | More -> loop ())
    else
      Ok (Some (Buffer.nth b i), i)
  in
  loop ()

let unstructured =
  let rec loop = function
    | OUT ->
        ret ()
    | START ->
        begin
          peek_char >>= function
          | None -> fail
          | Some '\r' -> any_char >> loop CR
          | Some '\n' -> any_char >> loop LF
          | Some _ -> any_char >> loop START
        end
    | CR ->
        begin
          peek_char >>= function
          | None -> fail
          | Some '\n' -> any_char >> loop LF
          | Some _ -> any_char >> loop START
        end
    | LF ->
        begin
          peek_char >>= function
          | None -> loop OUT
          | Some ' ' | Some '\t' -> any_char >> loop WSP
          | Some _ -> loop OUT
        end
    | WSP ->
        begin
          peek_char >>= function
          | None -> fail
          | Some '\r' -> any_char >> loop CR
          | Some '\n' -> any_char >> loop LF
          | Some _ -> any_char >> loop START
        end
  in
  rep_ (alt (char ' ') (char '\t')) >> pos >>= fun i ->
  loop START >> take_from i

let is_ftext = function
  | '\033' .. '\057'
  | '\059' .. '\126' -> true
  | _ -> false

let field_name =
  accum is_ftext

let imf_field =
  field_name >>= fun h ->
  char ':' >>
  unstructured >>= fun v ->
  ret (h, v)
