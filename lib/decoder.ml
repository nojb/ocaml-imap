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
open Angstrom

let sp =
  char ' '

let pair sep p1 p2 =
  p1 >>= fun a1 -> sep *> p2 >>| fun a2 -> (a1, a2)

let triple sep p1 p2 p3 =
  p1 >>= fun a1 -> sep *> p2 >>= fun a2 -> sep *> p3 >>| fun a3 -> (a1, a2, a3)

let psep_by1 sep p =
  char '(' *> sep_by1 sep p <* char ')'

let psep_by sep p =
  char '(' *> sep_by sep p <* char ')'

let maybe p =
  option () (ignore <$> p)

let switch cases =
  choice (List.map (fun (s, p) -> string_ci s *> commit *> p) cases)

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
  | '\x00' .. '\x1F' | '\x7F'
  | '%' | '*' | '"' | '\\' | ']' -> false
  | '\x01' .. '\x7F' -> true
  | _ -> false

let atom =
  take_while1 is_atom_char

(*
   CR             =  %x0D
                                  ; carriage return

   LF             =  %x0A
                                  ; linefeed

   CRLF           =  CR LF
                          ; Internet standard newline
*)

let crlf =
  ignore <$> string "\r\n"

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

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let _is_nz_digit = function
  | '1'..'9' -> true
  | _ -> false

let number =
  (Int32.of_string <$> take_while1 is_digit) <?> "number"

let nz_number =
  (Int32.of_string <$> take_while1 is_digit) <?> "nz-number" (* FIXME != 0 *)

let uniqueid =
  (Int32.of_string <$> take_while1 is_digit) <?> "uniqueid"

(*
   quoted          = DQUOTE *QUOTED-CHAR DQUOTE

   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
                     '\\' quoted-specials
*)

let quoted_char =
  choice
    [
      satisfy (function '"' | '\\' -> false | '\x01'..'\x7f' -> true | _ -> false);
      char '\\' *> satisfy (function '"' | '\\' -> true | _ -> false);
    ] <?> "quoted-char"

let accumulate p =
  let rec loop cl =
    choice [p >>| (fun c -> `Add c); return `Stop] >>= function
    | `Add c ->
        loop (c :: cl)
    | `Stop ->
        let n = List.length cl in
        let b = Bytes.create n in
        List.iteri (fun i c -> Bytes.set b (n - 1 - i) c) cl;
        return (Bytes.unsafe_to_string b)
  in
  p >>= fun c -> loop [c]

let quoted =
  (char '"' *> accumulate quoted_char <* char '"') <?> "quoted"

(*
   literal         = "{" number "}" CRLF *CHAR8
                       ; Number represents the number of CHAR8s

   string          = quoted / literal
*)

let literal =
  (Int32.to_int <$> (char '{' *> number <* char '}' <* crlf) >>= take) <?> "literal"

let imap_string =
  choice [quoted; literal] <?> "string"

(*
   ASTRING-CHAR   = ATOM-CHAR / resp-specials

   astring         = 1*ASTRING-CHAR / string
*)

let is_astring_char c =
  is_atom_char c || c = ']'

let astring =
  choice [take_while1 is_astring_char; imap_string] <?> "astring"

(*
   TEXT-CHAR       = <any CHAR except CR and LF>

   text            = 1*TEXT-CHAR
*)

let is_text_char = function
  | '\r' | '\n' -> false
  | '\x01' .. '\x7F' -> true
  | _ -> false

let text =
  (* allow empty texts for greater tolerance *)
  choice [take_while1 is_text_char; return ""] <?> "text"

(*
   nil             = "NIL"

   nstring         = string / nil
*)

let nil =
  string_ci "NIL"

let some p =
  p >>| fun x -> Some x

let nstring =
  choice [nil *> return None; some imap_string]

let nstring' =
  nstring >>| function Some s -> s | None -> ""

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
*)

let flag_keyword =
  (atom >>| fun s -> Flag.Keyword s) <?> "flag-keyword"

let flag_extension =
  (char '\\' *> atom >>| fun s -> Flag.Extension s) <?> "flag-extension"

let flag =
  let open Flag in
  let cases =
    [
      "\\Answered", return Answered;
      "\\Flagged", return Flagged;
      "\\Deleted", return Deleted;
      "\\Seen", return Seen;
      "\\Draft", return Draft;
    ]
  in
  choice [switch cases; flag_keyword; flag_extension] <?> "flag"

(*
   flag-fetch      = flag / "\Recent"
*)

let flag_fetch =
  choice [switch ["\\Recent", return Flag.Recent]; flag] <?> "flag-fetch"

(*
   flag-perm       = flag / "\*"
*)

let flag_perm =
  choice [switch ["\\*", return Flag.Any]; flag] <?> "flag-perm"

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

let uid_range =
  (pair (char ':') uniqueid uniqueid) <?> "uid-range"

let uid_set =
  let uniqueid = uniqueid >>| fun n -> n, n in
  sep_by1 (char ',') (choice [uniqueid; uid_range]) <?> "uid-set"

(* We never parse '*' since it does not seem to show up in responses *)
let sequence_set =
  uid_set

let set =
  sequence_set

(*
   auth-type       = atom
                       ; Defined by [SASL]

   capability      = ("AUTH=" auth-type) / atom
                       ; New capabilities MUST begin with "X" or be
                       ; registered with IANA as standard or
                       ; standards-track

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

let is_text_other_char c =
  is_text_char c && (c <> ']')

let text_1 =
  choice [take_while1 is_text_other_char; return ""] (* We allow empty text_1 *)

let capability =
  let open Capability in
  let cases =
    [
      "COMPRESS=DEFLATE", return COMPRESS_DEFLATE;
      "CONDSTORE", return CONDSTORE;
      "ESEARCH", return ESEARCH;
      "ENABLE", return ENABLE;
      "IDLE", return IDLE;
      "LITERAL+", return LITERALPLUS;
      "LITERAL-", return LITERALMINUS;
      "UTF8=ACCEPT", return UTF8_ACCEPT;
      "UTF8=ONLY", return UTF8_ONLY;
      "NAMESPACE", return NAMESPACE;
      "ID", return ID;
      "QRESYNC", return QRESYNC;
      "UIDPLUS", return UIDPLUS;
      "UNSELECT", return UNSELECT;
      "XLIST", return XLIST;
      "AUTH=PLAIN", return AUTH_PLAIN;
      "AUTH=LOGIN", return AUTH_LOGIN;
      "XOAUTH2", return XOAUTH2;
      "X-GM-EXT-1", return X_GM_EXT_1;
    ]
  in
  choice [switch cases; atom >>| (fun s -> OTHER s)] <?> "capability"

let mod_sequence_value =
  (Int64.of_string <$> take_while1 is_digit) <?> "mod-sequence-value" (* FIXME non zero *)

let append_uid = uniqueid

let resp_text_code =
  let open Code in
  let cases =
    [
      "ALERT", return ALERT;
      "BADCHARSET", many (sp *> astring) >>| (fun l -> BADCHARSET l);
      "CAPABILITY", many (sp *> capability) >>| (fun l -> CAPABILITY l);
      "PARSE", return PARSE;
      "PERMANENTFLAGS", sp *> psep_by sp flag_perm >>| (fun l -> PERMANENTFLAGS l);
      "READ-ONLY", return READ_ONLY;
      "READ-WRITE", return READ_WRITE;
      "TRYCREATE", return TRYCREATE;
      "UIDNEXT", sp *> nz_number >>| (fun n -> UIDNEXT n);
      "UIDVALIDITY", sp *> nz_number >>| (fun n -> UIDVALIDITY n);
      "UNSEEN", sp *> nz_number >>| (fun n -> UNSEEN n);
      "CLOSED", return CLOSED;
      "HIGHESTMODSEQ", sp *> mod_sequence_value >>| (fun n -> HIGHESTMODSEQ n);
      "NOMODSEQ", return NOMODSEQ;
      "MODIFIED", sp *> set >>| (fun l -> MODIFIED l);
      "APPENDUID", sp *> pair sp nz_number append_uid >>| (fun (n, uid) -> APPENDUID (n, uid));
      "COPYUID", sp *> triple sp nz_number set set >>| (fun (n, s1, s2) -> COPYUID (n, s1, s2));
      "UIDNOTSTICKY", return UIDNOTSTICKY;
      "COMPRESSIONACTIVE", return COMPRESSIONACTIVE;
      "USEATTR", return USEATTR;
    ]
  in
  let other =
    atom >>= fun a -> option None (sp *> some text_1) >>| fun x ->
    OTHER (a, x)
  in
  choice [switch cases; other] <?> "resp-text-code"

(*
   resp-text       = ["[" resp-text-code "]" SP] text
*)

let resp_text =
  let resp_text_code = char '[' *> some resp_text_code <* char ']' <* maybe sp in
  pair (return ()) (option None resp_text_code) text <?> "resp-text"

(*
   resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
                       ; Status condition
*)

let resp_cond_state =
  let open Response.State in
  let cases =
    [
      "OK", sp *> resp_text >>| (fun (c, t) -> OK (c, t));
      "NO", sp *> resp_text >>| (fun (c, t) -> NO (c, t));
      "BAD", sp *> resp_text >>| (fun (c, t) -> BAD (c, t));
    ]
  in
  switch cases <?> "resp-cond-state"

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
  let open MailboxFlag in
  let cases =
    [
      "\\Noselect", return Noselect;
      "\\Marked", return Marked;
      "\\Unmarked", return Unmarked;
      "\\Noinferiors", return Noinferiors;
      "\\HasChildren", return HasChildren;
      "\\HasNoChildren", return HasNoChildren;
      "\\All", return All;
      "\\Archive", return Archive;
      "\\Drafts", return Drafts;
      "\\Flagged", return Flagged;
      "\\Junk", return Junk;
      "\\Sent", return Sent;
      "\\Trash", return Trash;
    ]
  in
  let extension = atom >>| fun s -> Extension s in
  choice [switch cases; extension] <?> "mbx-flag"

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

let mailbox =
  astring >>| fun s -> try Mutf7.decode s with _ -> s

let mailbox =
  mailbox <?> "mailbox"

(*
   mailbox-list    = "(" [mbx-list-flags] ")" SP
                      (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
*)

let delim =
  choice [char '"' *> some quoted_char <* char '"'; nil *> return None]

let mailbox_list =
  triple sp (psep_by sp mbx_flag) delim mailbox <?> "mailbox-list"

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
  (Int64.of_string <$> take_while1 is_digit) <?> "mod-sequence-valzer"

let status_att =
  let open Status.MailboxAttribute in
  let cases =
    [
      "MESSAGES", sp *> number >>| (fun n -> MESSAGES (Int32.to_int n));
      "RECENT", sp *> number >>| (fun n -> RECENT (Int32.to_int n));
      "UIDNEXT", sp *> number >>| (fun n -> UIDNEXT n);
      "UIDVALIDITY", sp *> number >>| (fun n -> UIDVALIDITY n);
      "UNSEEN", sp *> number >>| (fun n -> UNSEEN (Int32.to_int n));
      "HIGHESTMODSEQ", sp *> mod_sequence_valzer >>| (fun n -> HIGHESTMODSEQ n);
    ]
  in
  switch cases <?> "status-att"

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
  char '(' *>
  nstring' >>= fun ad_name ->
  sp *> nstring' >>= fun ad_adl ->
  sp *> nstring' >>= fun ad_mailbox ->
  sp *> nstring' >>= fun ad_host ->
  char ')' *>
  return {Envelope.Address.ad_name; ad_adl; ad_mailbox; ad_host}

let address =
  address <?> "address"

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

let envelope =
  let address_list =
    choice [char '(' *> commit *> many1 address <* char ')'; nil *> return []]
  in
  char '(' *>
  nstring' >>= fun env_date ->
  sp *> nstring' >>= fun env_subject ->
  sp *> address_list >>= fun env_from ->
  sp *> address_list >>= fun env_sender ->
  sp *> address_list >>= fun env_reply_to ->
  sp *> address_list >>= fun env_to ->
  sp *> address_list >>= fun env_cc ->
  sp *> address_list >>= fun env_bcc ->
  sp *> nstring' >>= fun env_in_reply_to ->
  sp *> nstring' >>= fun env_message_id ->
  char ')' *>
  return
    {
      Envelope.env_date;
      env_subject;
      env_from;
      env_sender;
      env_reply_to;
      env_to;
      env_cc;
      env_bcc;
      env_in_reply_to;
      env_message_id
    }

let envelope =
  envelope <?> "envelope"

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
  choice [psep_by1 sp (pair sp imap_string imap_string); nil *> return []] <?> "body-fld-param"

let body_fld_octets =
  number <?> "body-fld-octets"

let body_fields =
  let open MIME.Response.Fields in
  body_fld_param >>= fun fld_params ->
  sp *> nstring >>= fun fld_id ->
  sp *> nstring >>= fun fld_desc ->
  sp *> imap_string >>= fun fld_enc ->
  sp *> body_fld_octets >>| Int32.to_int >>| fun fld_octets ->
  {fld_params; fld_id; fld_desc; fld_enc; fld_octets}

let body_fields =
  body_fields <?> "body-fields"

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

let body_extension body_extension =
  let open MIME.Response.BodyExtension in
  let nstring = nstring' >>| fun s -> String s in
  let number = number >>| fun n -> Number n in
  let list = psep_by1 sp body_extension >>| fun l -> List l in
  choice [nstring; number; list]

let body_extension =
  fix body_extension <?> "body-extension"

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

let body_fld_dsp =
  let dsp = some (pair sp imap_string body_fld_param) in
  choice [char '(' *> dsp <* char ')'; nil *> return None] <?> "body-fld-dsp"

let body_fld_lang =
  let nstring = nstring' >>| fun s -> [s] in
  choice [nstring; psep_by1 sp imap_string] <?> "body-fld-lang"

let body_fld_loc =
  nstring <?> "body-fld-loc"

let body_ext_gen =
  let open MIME.Response.Extension in
  option None (sp *> some body_fld_dsp) >>= function
  | None ->
      return {ext_dsp = None; ext_lang = []; ext_loc = None; ext_ext = []}
  | Some ext_dsp ->
      option None (sp *> some body_fld_lang) >>= function
      | None ->
          return {ext_dsp; ext_lang = []; ext_loc = None; ext_ext = []}
      | Some ext_lang ->
          option None (sp *> some body_fld_loc) >>= function
          | None ->
              return {ext_dsp; ext_lang; ext_loc = None; ext_ext = []}
          | Some ext_loc ->
              many (sp *> body_extension) >>| fun ext_ext ->
              {ext_dsp; ext_lang; ext_loc; ext_ext}

let body_ext_1part =
  pair (return ()) nstring body_ext_gen <?> "body-ext-1part" (* FIXME *)

let body_ext_mpart =
  pair (return ()) body_fld_param body_ext_gen <?> "body-ext-mpart" (* FIXME *)

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

let body_fld_lines =
  (Int32.to_int <$> number) <?> "body-fld-lines"

let media_subtype =
  imap_string <?> "media-subtype"

let media_basic =
  pair sp imap_string media_subtype <?> "media-basic" (* FIXME *)

let body_type_mpart body = (* TODO Return the extension data *)
  let open MIME.Response in
  many1 body >>= fun bodies ->
  sp *> imap_string >>= fun media_subtype ->
  option None (sp *> some body_ext_mpart) >>| fun _ ->
  Multipart (bodies, media_subtype)

let body_type_mpart body =
  body_type_mpart body <?> "body-type-mpart"

let body_type_basic media_type media_subtype =
  let open MIME.Response in
  let aux =
    sp *> body_fields >>| fun body_fields ->
    Basic (media_type, media_subtype, body_fields)
  in
  aux <?> "body-type-basic"

let body_type_msg body =
  let open MIME.Response in
  let aux =
    sp *> body_fields >>= fun body_fields ->
    sp *> envelope >>= fun envelope ->
    sp *> body >>= fun body ->
    sp *> body_fld_lines >>| fun body_fld_lines ->
    Message (body_fields, envelope, body, body_fld_lines)
  in
  aux <?> "body-type-msg"

let body_type_text media_subtype =
  let open MIME.Response in
  let aux =
    sp *> body_fields >>= fun body_fields ->
    sp *> body_fld_lines >>| fun body_fld_lines ->
    Text (media_subtype, body_fields, body_fld_lines)
  in
  aux <?> "body-type-text"

let body_type_1part body = (* TODO Return the extension data *)
  let aux =
    media_basic >>= fun (media_type, media_subtype) ->
    let body =
      match media_type, media_subtype with
      | "MESSAGE", "RFC822" -> body_type_msg body
      | "TEXT", _ -> body_type_text media_subtype
      | _ -> body_type_basic media_type media_subtype
    in
    option None (sp *> some body_ext_1part) >>= fun _ -> body
  in
  aux <?> "body-type-1part"

let body =
  fix (fun body -> char '(' *> choice [body_type_1part body; body_type_mpart body] <* char ')') <?> "body"

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

let digit =
  satisfy is_digit >>| fun c -> Char.code c - Char.code '0'

let digits2 =
  digit >>= fun n -> digit >>| fun m -> n*10 + m

let digits4 =
  digit >>= fun n -> digit >>= fun m -> digit >>= fun p -> digit >>| fun q ->
  n*1000 + m*100 + p*10 + q

let date_day_fixed =
  choice [sp *> digit; digits2] <?> "date-day-fixed"

let date_month =
  let months =
    [
      "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
      "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec";
    ]
  in
  switch (List.mapi (fun i m -> m, return i) months) <?> "date-month"

let date_year =
  digits4 <?> "date-year"

let time =
  triple (char ':') digits2 digits2 digits2 <?> "time"

let zone =
  switch
    [
      "+", digits4;
      "-", digits4 >>| (fun n -> -n);
    ] <?> "zone"

let date_time =
  let open Fetch.Date in
  let open Fetch.Time in
  let aux =
    date_day_fixed >>= fun day ->
    char '-' *> date_month >>= fun month ->
    char '-' *> date_year >>= fun year ->
    char ' ' *> time >>= fun (hours, minutes, seconds) ->
    char ' ' *> zone >>| fun zone ->
    {day; month; year}, {hours; minutes; seconds; zone}
  in
  (char '"' *> aux <* char '"') <?> "date-time"

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

let header_fld_name =
  astring <?> "header-fld-name"

let header_list =
  psep_by1 sp header_fld_name <?> "header-list"

let section_msgtext =
  let open MIME.Section in
  let cases =
    [
      "HEADER.FIELDS.NOT", sp *> header_list >>| (fun l -> HEADER_FIELDS_NOT l);
      "HEADER.FIELDS", sp *> header_list >>| (fun l -> HEADER_FIELDS l);
      "HEADER", return HEADER;
      "TEXT", return TEXT;
    ]
  in
  switch cases <?> "section-msgtext"

let section_text =
  choice [section_msgtext; switch ["MIME", return MIME.Section.MIME]] <?> "section-text"

let section_part =
  (List.map Int32.to_int <$> sep_by (char '.') nz_number) <?> "section-part"

let section_spec =
  section_part >>= function
  | [] ->
      option None (some section_text) >>| fun sec -> [], sec
  | _ :: _ as nl ->
      option None (char '.' *> some section_text) >>| fun sec -> nl, sec

let section_spec =
  section_spec <?> "section-spec"

let section =
  (char '[' *> section_spec <* char ']') <?> "section"

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

let permsg_modsequence =
  mod_sequence_value <?> "permsg-modsequence"

let msg_att_dynamic =
  let open Fetch.MessageAttribute in
  let cases =
    [
      "FLAGS", sp *> psep_by sp flag_fetch >>| (fun l -> FLAGS l);
      "MODSEQ", sp *> char '(' *> permsg_modsequence <* char ')' >>| (fun n -> MODSEQ n);
      "X-GM-LABELS", sp *> choice [psep_by sp astring; nil *> return []] >>| (fun l -> X_GM_LABELS l);
    ]
  in
  switch cases <?> "msg-att-dynamic"

let msg_att_static =
  let open Fetch.MessageAttribute in
  let section =
    section >>= fun s -> sp *> nstring >>| fun x ->
    BODY_SECTION (s, x)
  in
  let cases =
    [
      "ENVELOPE", sp *> envelope >>| (fun e -> ENVELOPE e);
      "INTERNALDATE", sp *> date_time >>| (fun (d, t) -> INTERNALDATE (d, t));
      (* "RFC822.HEADER", sp *> nstring >>| (fun s -> RFC822_HEADER s); *)
      (* "RFC822.TEXT", sp *> nstring >>| (fun s -> RFC822_TEXT s); *)
      "RFC822.SIZE", sp *> number >>| (fun n -> RFC822_SIZE (Int32.to_int n));
      (* "RFC822", sp *> nstring >>| (fun s -> RFC822 s); *)
      "BODYSTRUCTURE", sp *> body >>| (fun b -> BODYSTRUCTURE b);
      "BODY", choice [sp *> body >>| (fun b -> BODY b); section];
      "UID", sp *> uniqueid >>| (fun n -> UID n);
      "X-GM-MSGID", sp *> mod_sequence_value >>| (fun n -> X_GM_MSGID n);
      "X-GM-THRID", sp *> mod_sequence_value >>| (fun n -> X_GM_THRID n);
    ]
  in
  switch cases <?> "msg-att-static"

let msg_att =
  psep_by1 sp (choice [msg_att_static; msg_att_dynamic]) <?> "msg-att"

(*
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

   response-data =/ "*" SP enable-data CRLF
*)

let search_sort_mod_seq =
  (char '(' *> switch ["MODSEQ", sp *> mod_sequence_value] <* char ')') <?> "search-sort-mod-seq"

let mailbox_data =
  let open Response.Untagged in
  let cases =
    [
      "FLAGS", sp *> psep_by sp flag >>| (fun l -> FLAGS l);
      "LIST", sp *> mailbox_list >>| (fun (xs, c, m) -> LIST (xs, c, m));
      "LSUB", sp *> mailbox_list >>| (fun (xs, c, m) -> LSUB (xs, c, m));
      "SEARCH", pair (return ()) (many (sp *> nz_number)) (option None (sp *> some search_sort_mod_seq)) >>| (fun (acc, n) -> SEARCH (acc, n));
      "STATUS", sp *> pair sp mailbox (psep_by sp status_att) >>| (fun (m, l) -> STATUS (m, l));
    ]
  in
  let otherwise =
    let cases n =
      [
        "EXISTS", return (EXISTS n);
        "RECENT", return (RECENT n);
      ]
    in
    number >>= fun n -> sp *> switch (cases (Int32.to_int n))
  in
  choice [switch cases; otherwise] <?> "mailbox-data"

let capability_data =
  let open Response.Untagged in
  let cases =
    [
      "CAPABILITY", many (sp *> capability) >>| (fun l -> CAPABILITY l);
    ]
  in
  switch cases <?> "capability-date"

let enable_data =
  let open Response.Untagged in
  let cases =
    [
      "ENABLED", many (sp *> capability) >>| (fun l -> ENABLED l);
    ]
  in
  switch cases <?> "enable-data"

let resp_cond_bye =
  let open Response.Untagged in
  let cases =
    [
      "BYE", sp *> resp_text >>| (fun (c, t) -> BYE (c, t));
    ]
  in
  switch cases <?> "resp-cond-bye"

let known_ids =
  uid_set

let expunged_resp =
  let open Response.Untagged in
  let cases =
    [
      "VANISHED (EARLIER)", sp *> known_ids >>| (fun l -> VANISHED_EARLIER l);
      "VANISHED", sp *> known_ids >>| (fun l -> VANISHED l);
    ]
  in
  switch cases <?> "expunged-resp"

let message_data =
  let open Response.Untagged in
  let cases n =
    [
      "EXPUNGE", return (EXPUNGE n);
      "FETCH", sp *> msg_att >>| (fun x -> FETCH (n, x));
    ]
  in
  (nz_number >>= fun n -> sp *> choice [switch (cases n); expunged_resp]) <?> "message-data"

let resp_cond_auth =
  let open Response.Untagged in
  let cases =
    [
      "PREAUTH", sp *> resp_text >>| (fun (c, t) -> PREAUTH (c, t));
    ]
  in
  switch cases <?> "resp-cond-auth"

let response_data =
  let open Response.Untagged in
  let resp_cond_state = resp_cond_state >>| fun st -> State st in
  let data =
    choice
      [
        resp_cond_state;
        resp_cond_bye;
        mailbox_data;
        message_data;
        capability_data;
        enable_data;
        resp_cond_auth;
      ]
  in
  (char '*' *> sp *> data <* crlf) <?> "response-data"

(*
   resp-cond-bye   = "BYE" SP resp-text

   resp-cond-auth  = ("OK" / "PREAUTH") SP resp-text
                       ; Authentication condition

   greeting        = "*" SP (resp-cond-auth / resp-cond-bye) CRLF

   continue-req    = "+" SP (resp-text / base64) CRLF

   tag             = 1*<any ASTRING-CHAR except "+">

   response-tagged = tag SP resp-cond-state CRLF

   response-fatal  = "*" SP resp-cond-bye CRLF
                       ; Server closes connection immediately

   response-done   = response-tagged / response-fatal
*)

let is_tag_char = function
  | '+' -> false
  | c -> is_astring_char c

let tag =
  take_while1 is_tag_char <?> "tag"

let response_tagged =
  let aux =
    pair sp tag resp_cond_state >>| fun (tag, state) -> Tagged (tag, state)
  in
  (aux <* crlf) <?> "response-tagged"

let continue_req =
  (* space is optional CHECKME ! base64 *)
  (char '+' *> maybe sp *> (resp_text >>| fun (_, x) -> Cont x) <* crlf) <?> "continue-req"

let response =
  let response_data = response_data >>| fun x -> Untagged x in
  choice [continue_req; response_data; response_tagged] <?> "response"
