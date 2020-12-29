(* The MIT License (MIT)

   Copyright (c) 2015-2018 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

open Response

type buffer = { s : string; mutable p : int }

let some x = Some x

(* type 'a t = buffer -> ('a, string * int) result *)

let return x _ = Ok x

let ( *> ) p q buf = match p buf with Ok () -> q buf | Error _ as e -> e

let ( <* ) p q buf =
  match p buf with
  | Ok _ as o -> ( match q buf with Ok () -> o | Error _ as e -> e )
  | Error _ as e -> e

let ( <$> ) f p buf = match p buf with Ok x -> Ok (f x) | Error _ as e -> e

let ( >>= ) p f buf = match p buf with Ok x -> f x buf | Error _ as e -> e

let ( >|= ) p f buf = match p buf with Ok x -> Ok (f x) | Error _ as e -> e

let error buf = Error (buf.s, buf.p)

let is_eol buf = Ok (buf.p >= String.length buf.s)

let eol = is_eol >>= function true -> return () | false -> error

let curr buf =
  if buf.p >= String.length buf.s then Ok '\000' else Ok buf.s.[buf.p]

let next buf =
  assert (buf.p < String.length buf.s);
  buf.p <- buf.p + 1;
  Ok ()

let take n buf =
  if buf.p + n > String.length buf.s then (
    buf.p <- String.length buf.s;
    error buf )
  else
    let s = String.sub buf.s buf.p n in
    buf.p <- buf.p + n;
    Ok s

let char c = curr >>= fun c1 -> if c1 = c then next else error

let take_while1 f buf =
  let pos0 = buf.p in
  let pos = ref pos0 in
  while !pos < String.length buf.s && f buf.s.[!pos] do
    incr pos
  done;
  if pos0 = !pos then error buf
  else (
    buf.p <- !pos;
    Ok (String.sub buf.s pos0 (!pos - pos0)) )

(*
   CHAR           =  %x01-7F
                          ; any 7-bit US-ASCII character,
                            excluding NUL

   CTL            =  %x00-1F / %x7F
                          ; controls

   ATOM-CHAR       = <any CHAR except atom-specials>

   atom-specials   = "(" / ")" / "{" / SP / CTL / list-wildcards /
                     quoted-specials / resp-specials

   quoted-specials = DQUOTE / "\\"

   resp-specials   = "]"

   list-wildcards  = "%" / "*"

   atom            = 1*ATOM-CHAR
*)

let is_atom_char = function
  | '(' | ')' | '{' | ' '
  | '\x00' .. '\x1F'
  | '\x7F' | '%' | '*' | '"' | '\\' | ']' ->
      false
  | '\x01' .. '\x7F' -> true
  | _ -> false

let atom = take_while1 is_atom_char

(*
   quoted          = DQUOTE *QUOTED-CHAR DQUOTE

   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
                     '\\' quoted-specials
*)

let quoted_char =
  curr >>= function
  | '\\' -> (
      next *> curr >>= function
      | ('\\' | '"') as c -> next *> return c
      | _ -> error )
  | '"' -> error
  | '\x01' .. '\x7f' as c -> next *> return c
  | _ -> error

let quoted =
  let rec loop b =
    curr >>= function
    | '"' -> next *> return (Buffer.contents b)
    | _ ->
        quoted_char >>= fun c ->
        Buffer.add_char b c;
        loop b
  in
  char '"' >>= fun () -> loop (Buffer.create 17)

(*
   number          = 1*DIGIT
                       ; Unsigned 32-bit integer
                       ; (0 <= n < 4,294,967,296)

   nz-number       = digit-nz *DIGIT
                       ; Non-zero unsigned 32-bit integer
                       ; (0 < n < 4,294,967,296)

   uniqueid        = nz-number
                       ; Strictly ascending
*)

let is_digit = function '0' .. '9' -> true | _ -> false

let number =
  let f s = Scanf.sscanf s "%lu" (fun n -> n) in
  f <$> take_while1 is_digit

let nz_number = number

let uniqueid = number

(*
   literal         = "{" number "}" CRLF *CHAR8
                       ; Number represents the number of CHAR8s

   string          = quoted / literal
*)

let get_exactly n buf =
  if n + buf.p > String.length buf.s then error buf
  else
    let s = String.sub buf.s buf.p n in
    buf.p <- buf.p + n;
    Ok s

let literal =
  char '{' *> number >>= fun n ->
  char '}' *> eol *> get_exactly (Int32.to_int n)

let imap_string =
  curr >>= function '"' -> quoted | '{' -> literal | _ -> error

(*
   ASTRING-CHAR   = ATOM-CHAR / resp-specials

   astring         = 1*ASTRING-CHAR / string
*)

let is_astring_char c = is_atom_char c || c = ']'

let astring =
  curr >>= function
  | '"' | '{' -> imap_string
  | _ -> take_while1 is_astring_char

(*
   nil             = "NIL"

   nstring         = string / nil
*)

let nstring =
  curr >>= function
  | '"' | '{' -> imap_string
  | _ -> char 'N' *> char 'I' *> char 'L' *> return ""

(*
   TEXT-CHAR       = <any CHAR except CR and LF>

   text            = 1*TEXT-CHAR
*)

let is_text_char = function
  | '\r' | '\n' -> false
  | '\x01' .. '\x7F' -> true
  | _ -> false

let text =
  is_eol >>= function true -> return "" | false -> take_while1 is_text_char

let is_text_other_char = function ']' -> false | c -> is_text_char c

let text_1 =
  is_eol >>= function
  | true -> return ""
  | false -> take_while1 is_text_other_char

(*
   mbx-list-sflag  = "\Noselect" / "\Marked" / "\Unmarked"
                       ; Selectability flags; only one per LIST response

   mbx-list-oflag  = "\Noinferiors" / flag-extension
                       ; Other flags; multiple possible per LIST response

   mbx-list-flags  = *(mbx-list-oflag SP) mbx-list-sflag
                     *(SP mbx-list-oflag) /
                     mbx-list-oflag *(SP mbx-list-oflag)

   HasChildren = "\HasChildren"

   HasNoChildren = "\HasNoChildren"

   mbx-list-oflag =/  use-attr
                    ; Extends "mbx-list-oflag" from IMAP base [RFC3501]

   use-attr        =  "\All" / "\Archive" / "\Drafts" / "\Flagged" /
                      "\Junk" / "\Sent" / "\Trash" / use-attr-ext

   use-attr-ext    =  '\\' atom
                       ; Reserved for future extensions.  Clients
                       ; MUST ignore list attributes they do not understand
                       ; Server implementations MUST NOT generate
                       ; extension attributes except as defined by
                       ; future Standards-Track revisions of or
                       ; extensions to this specification.
*)

let mbx_flag =
  char '\\' *> atom >|= fun a ->
  match String.lowercase_ascii a with
  | "noselect" -> Noselect
  | "marked" -> Marked
  | "unmarked" -> Unmarked
  | "noinferiors" -> Noinferiors
  | "haschildren" -> HasChildren
  | "hasnochildren" -> HasNoChildren
  | "all" -> All
  | "archive" -> Archive
  | "drafts" -> Drafts
  | "flagged" -> Flagged
  | "junk" -> Junk
  | "sent" -> Sent
  | "trash" -> Trash
  | _ -> Extension a

let delim =
  curr >>= function
  | '"' -> some <$> (char '"' *> quoted_char <* char '"')
  | _ -> char 'N' *> char 'I' *> char 'L' *> return None

(*
   mailbox         = "INBOX" / astring
                       ; INBOX is case-insensitive.  All case variants of
                       ; INBOX (e.g., "iNbOx") MUST be interpreted as INBOX
                       ; not as an astring.  An astring which consists of
                       ; the case-insensitive sequence "I" "N" "B" "O" "X"
                       ; is considered to be INBOX and not an astring.
                       ;  Refer to section 5.1 for further
                       ; semantic details of mailbox names.
*)

let is_inbox s =
  String.length s = String.length "INBOX" && String.uppercase_ascii s = "INBOX"

let mailbox = astring >|= fun s -> if is_inbox s then "INBOX" else s

(*
   mailbox-list    = "(" [mbx-list-flags] ")" SP
                      (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
*)

let plist p =
  char '(' *> curr >>= function
  | ')' -> next *> return []
  | _ ->
      let rec loop acc =
        curr >>= function
        | ' ' -> next *> p >>= fun x -> loop (x :: acc)
        | ')' -> next *> return (List.rev acc)
        | _ -> error
      in
      p >>= fun x -> loop [ x ]

let mailbox_list =
  plist mbx_flag >>= fun flags ->
  char ' ' *> delim >>= fun delim ->
  char ' ' *> mailbox >>= fun mbox -> return (flags, delim, mbox)

(*
   auth-type       = atom
                       ; Defined by [SASL]

   capability      = ("AUTH=" auth-type) / atom
                       ; New capabilities MUST begin with "X" or be
                       ; registered with IANA as standard or
                       ; standards-track
*)

let capability = atom

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

   seq-range       = seq-number ":" seq-number
                       ; two seq-number values and all values between
                       ; these two regardless of order.
                       ; Example: 2:4 and 4:2 are equivalent and indicate
                       ; values 2, 3, and 4.
                       ; Example: a unique identifier sequence range of
                       ; 3291:* includes the UID of the last message in
                       ; the mailbox, even if that value is less than 3291.

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

   uid-set         = (uniqueid / uid-range) *("," uid-set)

   uid-range       = (uniqueid ":" uniqueid)
                     ; two uniqueid values and all values
                     ; between these two regards of order.
                     ; Example: 2:4 and 4:2 are equivalent.
*)

let mod_sequence_value =
  let f s = Scanf.sscanf s "%Lu" (fun n -> n) in
  f <$> take_while1 is_digit

let uid_range =
  uniqueid >>= fun n ->
  curr >>= function
  | ':' -> next *> uniqueid >|= fun m -> (n, m)
  | _ -> return (n, n)

let uid_set =
  let rec loop acc =
    curr >>= function
    | ',' -> next *> uid_range >>= fun r -> loop (r :: acc)
    | _ -> return (List.rev acc)
  in
  uid_range >>= fun r -> loop [ r ]

let sequence_set = uid_set

let set = sequence_set

(*
   flag-extension  = "\\" atom
                       ; Future expansion.  Client implementations
                       ; MUST accept flag-extension flags.  Server
                       ; implementations MUST NOT generate
                       ; flag-extension flags except as defined by
                       ; future standard or standards-track
                       ; revisions of this specification.

   flag-keyword    = atom

   flag            = "\Answered" / "\Flagged" / "\Deleted" /
                     "\Seen" / "\Draft" / flag-keyword / flag-extension
                       ; Does not include "\Recent"

   flag-perm       = flag / "\*"

   flag-fetch      = flag / "\Recent"
*)

let flag_gen recent any =
  curr >>= function
  | '\\' -> (
      next *> curr >>= function
      | '*' when any -> next *> return Any
      | _ -> (
          atom >|= fun a ->
          match String.lowercase_ascii a with
          | "recent" when recent -> Recent
          | "answered" -> Answered
          | "flagged" -> Flagged
          | "deleted" -> Deleted
          | "seen" -> Seen
          | "draft" -> Draft
          | _ -> Extension a ) )
  | _ -> atom >|= fun a -> Keyword a

let flag = flag_gen false false

let flag_fetch = flag_gen true false

let flag_perm = flag_gen false true

(*
   capability-data = "CAPABILITY" *(SP capability) SP "IMAP4rev1"
                     *(SP capability)
                       ; Servers MUST implement the STARTTLS, AUTH=PLAIN,
                       ; and LOGINDISABLED capabilities
                       ; Servers which offer RFC 1730 compatibility MUST
                       ; list "IMAP4" as the first capability.

   resp-text-code  = "ALERT" /
                     "BADCHARSET" [SP "(" astring *(SP astring) ")" ] /
                     capability-data / "PARSE" /
                     "PERMANENTFLAGS" SP "("
                     [flag-perm *(SP flag-perm)] ")" /
                     "READ-ONLY" / "READ-WRITE" / "TRYCREATE" /
                     "UIDNEXT" SP nz-number / "UIDVALIDITY" SP nz-number /
                     "UNSEEN" SP nz-number /
                     atom [SP 1*<any TEXT-CHAR except "]">]

   resp-text-code      =/ "HIGHESTMODSEQ" SP mod-sequence-value /
                          "NOMODSEQ" /
                          "MODIFIED" SP set

   resp-text-code      =/ "CLOSED"

   append-uid      = uniqueid

   resp-code-apnd  = "APPENDUID" SP nz-number SP append-uid

   resp-code-copy  = "COPYUID" SP nz-number SP uid-set SP uid-set

   resp-text-code  =/ resp-code-apnd / resp-code-copy / "UIDNOTSTICKY"
                     ; incorporated before the expansion rule of
                     ;  atom [SP 1*<any TEXT-CHAR except "]">]
                     ; that appears in [IMAP]

   resp-text-code =/ "COMPRESSIONACTIVE"


   resp-text-code =/  "USEATTR"
                    ; Extends "resp-text-code" from
                    ; IMAP [RFC3501]
*)

let append_uid = uniqueid

let slist p =
  let rec loop acc =
    curr >>= function
    | ' ' -> next *> p >>= fun x -> loop (x :: acc)
    | _ -> return (List.rev acc)
  in
  loop []

let resp_text_code =
  char '[' *> atom
  >>= (function
        | "ALERT" -> return ALERT
        | "BADCHARSET" -> (
            curr >>= function
            | ' ' -> next *> plist astring >|= fun l -> BADCHARSET l
            | _ -> return (BADCHARSET []) )
        | "CAPABILITY" -> slist atom >|= fun l -> (CAPABILITY l : code)
        | "PARSE" -> return PARSE
        | "PERMANENTFLAGS" ->
            char ' ' *> plist flag_perm >|= fun l -> PERMANENTFLAGS l
        | "READ-ONLY" -> return READ_ONLY
        | "READ-WRITE" -> return READ_WRITE
        | "TRYCREATE" -> return TRYCREATE
        | "UIDNEXT" -> char ' ' *> nz_number >|= fun n -> (UIDNEXT n : code)
        | "UIDVALIDITY" ->
            char ' ' *> nz_number >|= fun n -> (UIDVALIDITY n : code)
        | "UNSEEN" ->
            char ' ' *> nz_number >|= fun n -> (UNSEEN (Int32.to_int n) : code)
        | "CLOSED" -> return CLOSED
        | "HIGHESTMODSEQ" ->
            char ' ' *> mod_sequence_value >|= fun n -> (HIGHESTMODSEQ n : code)
        | "NOMODSEQ" -> return NOMODSEQ
        | "MODIFIED" -> char ' ' *> set >|= fun set -> MODIFIED set
        | "APPENDUID" ->
            char ' ' *> nz_number >>= fun n ->
            char ' ' *> append_uid >|= fun uid -> APPENDUID (n, uid)
        | "COPYUID" ->
            char ' ' *> nz_number >>= fun n ->
            char ' ' *> set >>= fun s1 ->
            char ' ' *> set >|= fun s2 -> COPYUID (n, s1, s2)
        | "UIDNOTSTICKY" -> return UIDNOTSTICKY
        | "COMPRESSIONACTIVE" -> return COMPRESSIONACTIVE
        | "USEATTR" -> return USEATTR
        | a -> (
            curr >>= function
            | ' ' -> text_1 >|= fun x -> OTHER (a, Some x)
            | _ -> return (OTHER (a, None)) ))
  <* char ']'

(*
   resp-text       = ["[" resp-text-code "]" SP] text
*)

let resp_text =
  (curr >>= function ' ' -> next | _ -> return ()) >>= fun () ->
  (curr >>= function '[' -> resp_text_code >|= Option.some | _ -> return None)
  >>= fun c ->
  (curr >>= function ' ' -> next | _ -> return ()) >>= fun () ->
  text >|= fun t -> (c, t)

let search_sort_mod_seq =
  char '(' *> atom
  >>= (function "MODSEQ" -> char ' ' *> mod_sequence_value | _ -> error)
  <* char ')'

(*
   address         = "(" addr-name SP addr-adl SP addr-mailbox SP
                     addr-host ")"

   addr-adl        = nstring
                       ; Holds route from [RFC-2822] route-addr if
                       ; non-NIL

   addr-host       = nstring
                       ; NIL indicates [RFC-2822] group syntax.
                       ; Otherwise, holds [RFC-2822] domain name

   addr-mailbox    = nstring
                       ; NIL indicates end of [RFC-2822] group; if
                       ; non-NIL and addr-host is NIL, holds
                       ; [RFC-2822] group name.
                       ; Otherwise, holds [RFC-2822] local-part
                       ; after removing [RFC-2822] quoting

   addr-name       = nstring
                       ; If non-NIL, holds phrase from [RFC-2822]
                       ; mailbox after removing [RFC-2822] quoting
*)

let address =
  char '(' *> nstring >>= fun ad_name ->
  char ' ' *> nstring >>= fun ad_adl ->
  char ' ' *> nstring >>= fun ad_mailbox ->
  char ' ' *> nstring >>= fun ad_host ->
  char ')' *> return { ad_name; ad_adl; ad_mailbox; ad_host }

(*
   envelope        = "(" env-date SP env-subject SP env-from SP
                     env-sender SP env-reply-to SP env-to SP env-cc SP
                     env-bcc SP env-in-reply-to SP env-message-id ")"

   env-bcc         = "(" 1*address ")" / nil

   env-cc          = "(" 1*address ")" / nil

   env-date        = nstring

   env-from        = "(" 1*address ")" / nil

   env-in-reply-to = nstring

   env-message-id  = nstring

   env-reply-to    = "(" 1*address ")" / nil

   env-sender      = "(" 1*address ")" / nil

   env-subject     = nstring

   env-to          = "(" 1*address ")" / nil
*)

let address_list =
  curr >>= function
  | '(' ->
      let rec loop acc =
        curr >>= function
        | ')' -> next *> return (List.rev acc)
        | _ -> address >>= fun ad -> loop (ad :: acc)
      in
      next *> loop []
  | _ -> char 'N' *> char 'I' *> char 'L' *> return []

let envelope =
  char '(' *> nstring >>= fun env_date ->
  char ' ' *> nstring >>= fun env_subject ->
  char ' ' *> address_list >>= fun env_from ->
  char ' ' *> address_list >>= fun env_sender ->
  char ' ' *> address_list >>= fun env_reply_to ->
  char ' ' *> address_list >>= fun env_to ->
  char ' ' *> address_list >>= fun env_cc ->
  char ' ' *> address_list >>= fun env_bcc ->
  char ' ' *> nstring >>= fun env_in_reply_to ->
  char ' ' *> nstring >>= fun env_message_id ->
  char ')'
  *> return
       {
         env_date;
         env_subject;
         env_from;
         env_sender;
         env_reply_to;
         env_to;
         env_cc;
         env_bcc;
         env_in_reply_to;
         env_message_id;
       }

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

let _body_extension = error

(*
   body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil

   body-fld-enc    = (DQUOTE ("7BIT" / "8BIT" / "BINARY" / "BASE64"/
                     "QUOTED-PRINTABLE") DQUOTE) / string

   body-fld-id     = nstring

   body-fld-desc   = nstring

   body-fld-octets = number

   body-fields     = body-fld-param SP body-fld-id SP body-fld-desc SP
                     body-fld-enc SP body-fld-octets
*)

let body_fld_param =
  curr >>= function
  | '(' ->
      plist
        ( imap_string >>= fun x ->
          char ' ' *> imap_string >|= fun y -> (x, y) )
  | _ -> char 'N' *> char 'I' *> char 'L' *> return []

let body_fld_octets = Int32.to_int <$> number

let body_fields =
  body_fld_param >>= fun fld_params ->
  char ' ' *> (some <$> nstring) >>= fun fld_id ->
  char ' ' *> (some <$> nstring) >>= fun fld_desc ->
  char ' ' *> imap_string >>= fun fld_enc ->
  char ' ' *> body_fld_octets >|= fun fld_octets ->
  { fld_params; fld_id; fld_desc; fld_enc; fld_octets }

(*
   body-fld-md5    = nstring

   body-fld-dsp    = "(" string SP body-fld-param ")" / nil

   body-fld-lang   = nstring / "(" string *(SP string) ")"

   body-fld-loc    = nstring

   body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang
                     [SP body-fld-loc *(SP body-extension)]]]
                       ; MUST NOT be returned on non-extensible
                       ; "BODY" fetch

   body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang
                     [SP body-fld-loc *(SP body-extension)]]]
                       ; MUST NOT be returned on non-extensible
                       ; "BODY" fetch
*)

let body_fld_md5 = nstring

let body_fld_dsp =
  curr >>= function
  | '(' ->
      next *> imap_string >>= fun s ->
      char ' ' *> body_fld_param >>= fun l -> char ')' *> return (Some (s, l))
  | _ -> char 'N' *> char 'I' *> char 'L' *> return None

let body_fld_lang =
  curr >>= function
  | '(' -> plist imap_string
  | _ -> ( nstring >|= function "" -> [] | s -> [ s ] )

let body_fld_loc = nstring

let body_ext_1part =
  body_fld_md5 >>= fun _md5 ->
  curr >>= function
  | ' ' -> (
      next *> body_fld_dsp >>= fun ext_dsp ->
      curr >>= function
      | ' ' -> (
          next *> body_fld_lang >>= fun ext_lang ->
          curr >>= function
          | ' ' ->
              next *> body_fld_loc >|= fun ext_loc ->
              { ext_dsp; ext_lang; ext_loc; ext_ext = [] }
          | _ -> return { ext_dsp; ext_lang; ext_loc = ""; ext_ext = [] } )
      | _ -> return { ext_dsp; ext_lang = []; ext_loc = ""; ext_ext = [] } )
  | _ -> return { ext_dsp = None; ext_lang = []; ext_loc = ""; ext_ext = [] }

let body_ext_mpart =
  body_fld_param >>= fun p ->
  (curr >>= function
   | ' ' -> (
       next *> body_fld_dsp >>= fun _ ->
       curr >>= function
       | ' ' -> (
           next *> body_fld_lang >>= fun _ ->
           curr >>= function
           | ' ' -> next *> body_fld_loc >|= ignore
           | _ -> return () )
       | _ -> return () )
   | _ -> return ())
  *> return p

(*
   body-fld-lines  = number

   media-subtype   = string
                       ; Defined in [MIME-IMT]

   media-basic     = ((DQUOTE ("APPLICATION" / "AUDIO" / "IMAGE" /
                     "MESSAGE" / "VIDEO") DQUOTE) / string) SP
                     media-subtype
                       ; Defined in [MIME-IMT]

   media-message   = DQUOTE "MESSAGE" DQUOTE SP DQUOTE "RFC822" DQUOTE
                       ; Defined in [MIME-IMT]

   media-text      = DQUOTE "TEXT" DQUOTE SP media-subtype
                       ; Defined in [MIME-IMT]

   body-type-basic = media-basic SP body-fields
                       ; MESSAGE subtype MUST NOT be "RFC822"

   body-type-msg   = media-message SP body-fields SP envelope
                     SP body SP body-fld-lines

   body-type-text  = media-text SP body-fields SP body-fld-lines

   body-type-1part = (body-type-basic / body-type-msg / body-type-text)
                     [SP body-ext-1part]

   body-type-mpart = 1*body SP media-subtype
                     [SP body-ext-mpart]

   body            = "(" (body-type-1part / body-type-mpart) ")"
*)

let fix f =
  let rec p buf = f p buf in
  f p

let body_fld_lines = Int32.to_int <$> number

let body_type_msg body =
  body_fields >>= fun fields ->
  char ' ' *> envelope >>= fun envelope ->
  char ' ' *> body >>= fun b ->
  char ' ' *> body_fld_lines >|= fun fld_lines ->
  Message (fields, envelope, b, fld_lines)

let body_type_text media_subtype =
  body_fields >>= fun fields ->
  char ' ' *> body_fld_lines >|= fun fld_lines ->
  Text (media_subtype, fields, fld_lines)

let body_type_basic media_type media_subtype =
  body_fields >|= fun fields -> Basic (media_type, media_subtype, fields)

let body_type_1part body =
  imap_string >>= fun media_type ->
  char ' ' *> imap_string >>= fun media_subtype ->
  ( match (media_type, media_subtype) with
  | "MESSAGE", "RFC822" -> char ' ' *> body_type_msg body
  | "TEXT", _ -> char ' ' *> body_type_text media_subtype
  | _ -> char ' ' *> body_type_basic media_type media_subtype )
  >>= fun body ->
  (curr >>= function
   | ' ' -> next *> body_ext_1part >>= fun _ -> return ()
   | _ -> return ())
  *> return body

let body_type_mpart body =
  let rec loop acc =
    curr >>= function
    | ' ' ->
        next *> imap_string >>= fun media_subtype ->
        (curr >>= function ' ' -> next *> body_ext_mpart | _ -> return [])
        >|= fun params -> Multipart (List.rev acc, media_subtype, params)
    | _ -> body >>= fun b -> loop (b :: acc)
  in
  loop []

let body body =
  char '(' *> curr
  >>= (function '(' -> body_type_mpart body | _ -> body_type_1part body)
  <* char ')'

let body = fix body

(*
   DIGIT           =  %x30-39
                          ; 0-9

   date-day-fixed  = (SP DIGIT) / 2DIGIT
                       ; Fixed-format version of date-day

   date-month      = "Jan" / "Feb" / "Mar" / "Apr" / "May" / "Jun" /
                     "Jul" / "Aug" / "Sep" / "Oct" / "Nov" / "Dec"

   time            = 2DIGIT ":" 2DIGIT ":" 2DIGIT
                       ; Hours minutes seconds

   zone            = ("+" / "-") 4DIGIT
                       ; Signed four-digit value of hhmm representing
                       ; hours and minutes east of Greenwich (that is,
                       ; the amount that the given time differs from
                       ; Universal Time).  Subtracting the timezone
                       ; from the given time will give the UT form.
                       ; The Universal Time zone is "+0000".

   date-year       = 4DIGIT

   date-time       = DQUOTE date-day-fixed "-" date-month "-" date-year
                     SP time SP zone DQUOTE
*)

(* DD-MMM-YYYY HH:MM:SS +ZZZZ *)
let date_time = char '"' *> take 26 <* char '"'

(*
   header-fld-name = astring

   header-list     = "(" header-fld-name *(SP header-fld-name) ")"

   section-msgtext = "HEADER" / "HEADER.FIELDS" [".NOT"] SP header-list /
                     "TEXT"
                       ; top-level or MESSAGE/RFC822 part

   section-part    = nz-number *("." nz-number)
                       ; body part nesting

   section-spec    = section-msgtext / (section-part ["." section-text])

   section-text    = section-msgtext / "MIME"
                       ; text other than actual body part (headers, etc.)

   section         = "[" [section-spec] "]"
*)

let _section = error

(* TODO *)

(*
   msg-att-static  = "ENVELOPE" SP envelope / "INTERNALDATE" SP date-time /
                     "RFC822" [".HEADER" / ".TEXT"] SP nstring /
                     "RFC822.SIZE" SP number /
                     "BODY" ["STRUCTURE"] SP body /
                     "BODY" section ["<" number ">"] SP nstring /
                     "UID" SP uniqueid
                       ; MUST NOT change for a message

   msg-att-dynamic = "FLAGS" SP "(" [flag-fetch *(SP flag-fetch)] ")"
                       ; MAY change for a message

   msg-att         = "(" (msg-att-dynamic / msg-att-static)
                      *(SP (msg-att-dynamic / msg-att-static)) ")"

   permsg-modsequence  = mod-sequence-value
                          ;; per message mod-sequence

   mod-sequence-value  = 1*DIGIT
                          ;; Positive unsigned 64-bit integer
                          ;; (mod-sequence)
                          ;; (1 <= n < 18,446,744,073,709,551,615)

   fetch-mod-resp      = "MODSEQ" SP "(" permsg-modsequence ")"

   msg-att-dynamic     =/ fetch-mod-resp

   msg-att-dynamic     =/ "X-GM-LABELS" SP "(" [astring 0*(SP astring)] ")" / nil
                          ; https://developers.google.com/gmail/imap_extensions

   msg-att-static      =/ "X-GM-MSGID" SP mod-sequence-value /
                          "X-GM-THRID" SP mod-sequecne-value
                          ; https://developers.google.com/gmail/imap_extensions
*)

let permsg_modsequence = mod_sequence_value

let msg_att =
  atom >>= function
  | "FLAGS" ->
      char ' ' *> plist flag_fetch >|= fun l -> (FLAGS l : message_attribute)
  | "MODSEQ" ->
      char ' ' *> char '(' *> permsg_modsequence >>= fun n ->
      char ')' *> return (MODSEQ n)
  | "X-GM-LABELS" -> (
      char ' ' *> curr >>= function
      | '(' -> plist astring >|= fun l -> X_GM_LABELS l
      | _ -> char 'N' *> char 'I' *> char 'L' *> return (X_GM_LABELS []) )
  | "ENVELOPE" -> char ' ' *> envelope >|= fun e -> ENVELOPE e
  | "INTERNALDATE" -> char ' ' *> date_time >|= fun s -> INTERNALDATE s
  | "RFC822.HEADER" -> char ' ' *> nstring >|= fun s -> RFC822_HEADER s
  | "RFC822.TEXT" -> char ' ' *> nstring >|= fun s -> RFC822_TEXT s
  | "RFC822.SIZE" ->
      char ' ' *> number >|= fun n -> RFC822_SIZE (Int32.to_int n)
  | "RFC822" -> char ' ' *> nstring >|= fun s -> RFC822 s
  | "BODYSTRUCTURE" -> char ' ' *> body >|= fun b -> BODYSTRUCTURE b
  (* | "BODY" ->
   *     let section =
   *       section >>= fun s -> sp *> nstring >>| fun x ->
   *       BODY_SECTION (s, x)
   *     in
   *     choice [sp *> body >>| (fun b -> BODY b); section] *)
  | "UID" -> char ' ' *> uniqueid >|= fun n -> UID n
  | "X-GM-MSGID" -> char ' ' *> mod_sequence_value >|= fun n -> X_GM_MSGID n
  | "X-GM-THRID" -> char ' ' *> mod_sequence_value >|= fun n -> X_GM_THRID n
  | _ -> error

(*
   status          = "STATUS" SP mailbox SP
                     "(" status-att *(SP status-att) ")"

   status-att      = "MESSAGES" / "RECENT" / "UIDNEXT" / "UIDVALIDITY" /
                     "UNSEEN"

   status-att-list =  status-att SP number *(SP status-att SP number)

   mod-sequence-valzer = "0" / mod-sequence-value

   status-att-val      =/ "HIGHESTMODSEQ" SP mod-sequence-valzer
                          ;; extends non-terminal defined in [IMAPABNF].
                          ;; Value 0 denotes that the mailbox doesn't
                          ;; support persistent mod-sequences
                          ;; as described in Section 3.1.2
*)

let mod_sequence_valzer =
  let f s = Scanf.sscanf s "%Lu" (fun n -> n) in
  f <$> take_while1 is_digit

let status_att =
  atom >>= function
  | "MESSAGES" -> char ' ' *> number >|= fun n -> MESSAGES (Int32.to_int n)
  | "RECENT" ->
      char ' ' *> number >|= fun n ->
      (RECENT (Int32.to_int n) : mailbox_attribute)
  | "UIDNEXT" -> char ' ' *> number >|= fun n -> UIDNEXT n
  | "UIDVALIDITY" -> char ' ' *> number >|= fun n -> UIDVALIDITY n
  | "UNSEEN" -> char ' ' *> number >|= fun n -> UNSEEN (Int32.to_int n)
  | "HIGHESTMODSEQ" ->
      char ' ' *> mod_sequence_valzer >|= fun n -> HIGHESTMODSEQ n
  | _ -> error

let known_ids = uid_set

(*
   resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
                       ; Status condition

   mailbox-data    =  "FLAGS" SP flag-list / "LIST" SP mailbox-list /
                      "LSUB" SP mailbox-list / "SEARCH" *(SP nz-number) /
                      "STATUS" SP mailbox SP "(" [status-att-list] ")" /
                      number SP "EXISTS" / number SP "RECENT"

   message-data    = nz-number SP ("EXPUNGE" / ("FETCH" SP msg-att))

   resp-cond-bye   = "BYE" SP resp-text

   response-data   = "*" SP (resp-cond-state / resp-cond-bye /
                     mailbox-data / message-data / capability-data) CRLF

   search-sort-mod-seq = "(" "MODSEQ" SP mod-sequence-value ")"

   mailbox-data        =/ "SEARCH" [1*(SP nz-number) SP
                          search-sort-mod-seq]

   known-uids          =  sequence-set
                          ;; sequence of UIDs, "*" is not allowed

   expunged-resp       =  "VANISHED" [SP "(EARLIER)"] SP known-uids

   message-data        =/ expunged-resp

   enable-data   = "ENABLED" *(SP capability)

   resp-cond-bye   = "BYE" SP resp-text

   resp-cond-auth  = ("OK" / "PREAUTH") SP resp-text
                       ; Authentication condition

   response-data =/ "*" SP enable-data CRLF
*)

let response_data =
  char '*' *> char ' ' *> curr >>= function
  | '0' .. '9' -> (
      number >>= fun n ->
      char ' ' *> atom >>= function
      | "EXISTS" -> return (EXISTS (Int32.to_int n))
      | "RECENT" -> return (RECENT (Int32.to_int n))
      | "EXPUNGE" -> return (EXPUNGE n)
      | "FETCH" -> char ' ' *> plist msg_att >|= fun x -> FETCH (n, x)
      | _ -> error )
  | _ -> (
      atom >>= function
      | "OK" ->
          resp_text >|= fun (code, message) -> (OK { code; message } : untagged)
      | "NO" ->
          resp_text >|= fun (code, message) -> (NO { code; message } : untagged)
      | "BAD" ->
          resp_text >|= fun (code, message) -> (BAD { code; message } : untagged)
      | "BYE" -> resp_text >|= fun (code, message) -> BYE { code; message }
      | "FLAGS" -> char ' ' *> plist flag >|= fun l -> FLAGS l
      | "LIST" -> char ' ' *> mailbox_list >|= fun (xs, c, m) -> LIST (xs, c, m)
      | "LSUB" -> char ' ' *> mailbox_list >|= fun (xs, c, m) -> LSUB (xs, c, m)
      | "SEARCH" ->
          let rec loop acc =
            curr >>= function
            | ' ' -> (
                next *> curr >>= function
                | '(' ->
                    search_sort_mod_seq >|= fun n ->
                    SEARCH (List.rev acc, Some n)
                | _ -> nz_number >>= fun n -> loop (n :: acc) )
            | _ -> return (SEARCH (List.rev acc, None))
          in
          loop []
      | "STATUS" ->
          char ' ' *> mailbox >>= fun mbox ->
          char ' ' *> plist status_att >|= fun l -> STATUS (mbox, l)
      | "CAPABILITY" -> slist capability >|= fun l -> CAPABILITY l
      | "ENABLED" -> slist capability >|= fun l -> ENABLED l
      | "PREAUTH" -> resp_text >|= fun (code, text) -> PREAUTH (code, text)
      | "VANISHED" -> (
          char ' ' *> curr >>= function
          | '(' ->
              (next *> atom >>= function "EARLIER" -> char ')' | _ -> error)
              >>= fun () ->
              char ' ' *> known_ids >|= fun ids -> VANISHED_EARLIER ids
          | _ -> known_ids >|= fun ids -> VANISHED ids )
      | _ -> error )

(*
   greeting        = "*" SP (resp-cond-auth / resp-cond-bye) CRLF

   continue-req    = "+" SP (resp-text / base64) CRLF

   tag             = 1*<any ASTRING-CHAR except "+">

   response-tagged = tag SP resp-cond-state CRLF

   response-fatal  = "*" SP resp-cond-bye CRLF
                       ; Server closes connection immediately

   response-done   = response-tagged / response-fatal
*)

let is_tag_char = function '+' -> false | c -> is_astring_char c

let tag = take_while1 is_tag_char

let resp_cond_state =
  atom >>= function
  | "OK" -> resp_text >|= fun (code, message) -> (OK, code, message)
  | "NO" -> resp_text >|= fun (code, message) -> (NO, code, message)
  | "BAD" -> resp_text >|= fun (code, message) -> (BAD, code, message)
  | _ -> error

let response =
  curr >>= function
  | '+' -> next *> resp_text >|= fun (_, x) -> Cont x
  | '*' -> response_data >|= fun x -> Untagged x
  | _ ->
      tag >>= fun tag ->
      char ' ' *> resp_cond_state >|= fun (status, code, message) ->
      Tagged { tag; status; code; message }

let response s p = response { s; p }
