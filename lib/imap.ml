(* The MIT License (MIT)

   Copyright (c) 2015-2017 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

module type NUMBER = sig
  type t
  val zero: t
  val of_int: int -> t
  val compare: t -> t -> int
end

module Uint32 = struct
  type t = int32 [@@deriving sexp]

  let zero = 0l

  let of_int n =
    Scanf.sscanf (Printf.sprintf "%u" n) "%lu" (fun n -> n)

  let msb n = Int32.(logand n (shift_left 1l 31)) <> 0l

  let compare n1 n2 =
    match msb n1, msb n2 with
    | true, true -> Int32.(compare (logand n1 0x7fffffffl) (logand n2 0x7fffffffl))
    | true, false -> 1
    | false, true -> -1
    | false, false -> Int32.compare n1 n2

  let succ n = Int32.succ n

  let (<) n m = compare n m < 0

  let (<=) n m = compare n m <= 0

  let min n m = if n <= m then n else m

  let max n m = if n <= m then m else n
end

module Modseq = struct
  type t = int64 [@@deriving sexp]

  let zero = 0L

  let of_int = Int64.of_int

  let compare = Int64.compare

  let of_string = Int64.of_string
end

module Uid = Uint32
module Seq = Uint32

module type NUMBER_SET = sig
  type elt
  type t [@@deriving sexp]

  val empty: t
  val singleton: elt -> t
  val union: t -> t -> t
  val add: elt -> t -> t
  val interval: elt -> elt -> t
  val of_list: elt list -> t
end

module Uint32Set = struct
  type t =
    (int32 * int32) list [@@deriving sexp] (* disjoint, sorted intervals *)

  let empty = []

  let singleton n = [(n, n)]

  let rec union s1 s2 =
    let open Int32 in
    let rec loop s1 s2 =
      match s1, s2 with
      | [], s2 -> s2
      | s1, [] -> s1
      | (a, b) :: x1, (c, d) :: x2 ->
          if succ b < c then (a, b) :: loop x1 s2
          else if succ d < a then (c, d) :: loop s1 x2
          else union [min a c, max b d] (union x1 x2)
    in
    loop s1 s2

  let add n s =
    union (singleton n) s

  let interval n m =
    if n <= m then [n, m] else [m, n]

  let of_list l =
    List.fold_left (fun s n -> add n s) empty l
end

module UidSet = Uint32Set
module SeqSet = Uint32Set

module Mutf7 = struct
  let recode ?nln ?encoding out_encoding src dst =
    let rec loop d e = match Uutf.decode d with
      | `Uchar _ as u -> ignore (Uutf.encode e u); loop d e
      | `End          -> ignore (Uutf.encode e `End)
      | `Malformed _  -> ignore (Uutf.encode e (`Uchar Uutf.u_rep)); loop d e
      | `Await        -> assert false
    in
    let d = Uutf.decoder ?nln ?encoding src in
    let e = Uutf.encoder out_encoding dst in
    loop d e

  let replace s ch1 ch2 =
    for i = 0 to Bytes.length s - 1 do
      if Bytes.get s i = ch1 then Bytes.set s i ch2
    done

  let encode s =
    let b = Buffer.create 0 in
    let rec a i =
      if i >= String.length s then
        ()
      else begin
        match s.[i] with
        | '&'                   -> Buffer.add_string b "&-"; a (i + 1)
        | '\x20' .. '\x7E' as c -> Buffer.add_char b c; a (i + 1)
        | _                     -> Buffer.add_char b '&'; u i
      end
    and u i =
      let upto j =
        let str = String.sub s i (j - i) and buf = Buffer.create 32 in
        recode ~encoding:`UTF_8 `UTF_16BE (`String str) (`Buffer buf);
        let str = B64.encode ~pad:false (Buffer.contents buf) in
        replace str '/' ',';
        Buffer.add_string b str; Buffer.add_char b '-'
      in
      let rec loop i =
        if i >= String.length s then
          upto i
        else begin
          match s.[i] with
          | '\x20' .. '\x7E' -> upto i; a i
          | _                -> loop (i + 1)
        end
      in
      loop i
    in
    a 0;
    Buffer.contents b

  let decode s =
    let b = Buffer.create 32 in
    let rec a i =
      if i >= String.length s then
        ()
      else begin
        match s.[i] with
        | '&' ->
            if i+1 < String.length s && s.[i] = '-' then (Buffer.add_char b '&'; a (i + 2)) else u (i + 1)
        | _ as c ->
            Buffer.add_char b c; a (i + 1)
      end
    and u i =
      let start = i in
      let rec loop i =
        if i >= String.length s then
          invalid_arg "unterminated base64 input"
        else begin
          match s.[i] with
          | '-' ->
              let str = String.sub s start (i - start) in
              replace str ',' '/';
              let str = B64.decode str in (* FIXME do we need to pad it with "===" ? *)
              recode ~encoding:`UTF_16BE `UTF_8 (`String str) (`Buffer b);
              a (i + 1)
          | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '+' | ',' ->
              loop (i+1)
          | _ ->
              invalid_arg "unexpected character"
        end
      in
      loop i
    in
    a 0;
    Buffer.contents b
end

type date =
  {
    day: int;
    month: int;
    year: int;
  } [@@deriving sexp]

type time =
  {
    hours: int;
    minutes: int;
    seconds: int;
    zone: int;
  } [@@deriving sexp]

type capability =
  | ACL
  | BINARY
  | CATENATE
  | CHILDREN
  | COMPRESS_DEFLATE
  | CONDSTORE
  | ENABLE
  | IDLE
  | ID
  | LITERALPLUS
  | LITERALMINUS
  | UTF8_ACCEPT
  | UTF8_ONLY
  | MULTIAPPEND
  | NAMESPACE
  | QRESYNC
  | QUOTE
  | SORT
  | STARTTLS
  | UIDPLUS
  | UNSELECT
  | XLIST
  | AUTH_ANONYMOUS
  | AUTH_LOGIN
  | AUTH_PLAIN
  | XOAUTH2
  | X_GM_EXT_1
  | OTHER of string [@@deriving sexp]

type address =
  {
    ad_name: string;
    ad_adl: string;
    ad_mailbox: string;
    ad_host: string;
  } [@@deriving sexp]

type envelope =
  {
    env_date: string;
    env_subject: string;
    env_from: address list;
    env_sender: address list;
    env_reply_to: address list;
    env_to: address list;
    env_cc: address list;
    env_bcc: address list;
    env_in_reply_to: string;
    env_message_id: string;
  } [@@deriving sexp]

module MIME = struct
  type fields =
    {
      fld_params : (string * string) list;
      fld_id : string option;
      fld_desc : string option;
      fld_enc : string;
      fld_octets : int;
    } [@@deriving sexp]

  type body_extension =
    | List of body_extension list
    | Number of int32
    | String of string [@@deriving sexp]

  type part_extension =
    {
      ext_dsp: (string * (string * string) list) option;
      ext_lang: string list;
      ext_loc: string option;
      ext_ext: body_extension list;
    } [@@deriving sexp]

  type mime =
    | Text of string * fields * int
    | Message of fields * envelope * mime * int
    | Basic of string * string * fields
    | Multipart of mime list * string [@@deriving sexp]
end

module Flag = struct
  type flag =
    | Answered
    | Flagged
    | Deleted
    | Seen
    | Draft
    | Keyword of string
    | Extension of string
    | Recent
    | Any [@@deriving sexp]
end

module MbxFlag = struct
  type mbx_flag =
    | Noselect
    | Marked
    | Unmarked
    | Noinferiors
    | HasChildren
    | HasNoChildren
    | All
    | Archive
    | Drafts
    | Flagged
    | Junk
    | Sent
    | Trash
    | Extension of string [@@deriving sexp]
end

module E = struct
  type rope =
    | Cat of rope * rope
    | Wait
    | Raw of string [@@deriving sexp]

  let rec is_empty = function
    | Cat (f, g) -> is_empty f && is_empty g
    | Wait -> false
    | Raw "" -> true
    | Raw _ -> false

  let empty = Raw ""

  let (++) f g =
    if is_empty f then g
    else if is_empty g then f
    else Cat (f, Cat (Raw " ", g))

  let literal s =
    Cat (Raw (Printf.sprintf "{%d}\r\n" (String.length s)), Cat (Wait, Raw s))

  let raw s =
    Raw s

  let str s =
    let literal_chars = function
      | '\x80' .. '\xFF' | '\r' | '\n' -> true
      | _ -> false
    in
    let quoted_chars = function
      | '(' | ')' | '{' | ' ' | '\x00' .. '\x1F' | '\x7F'
      | '%' | '*' | '\"' | '\\' -> true
      | _ -> false
    in
    let needs f s =
      let rec loop i = i < String.length s && (f s.[i] || loop (i+1)) in
      loop 0
    in
    if s = "" then
      raw "\"\""
    else if needs literal_chars s then
      literal s
    else if needs quoted_chars s then
      raw (Printf.sprintf "\"%s\"" s)
    else
      raw s

  let p f =
    Cat (Raw "(", Cat (f, Raw ")"))

  let mailbox s =
    str (Mutf7.encode s)

  let int n =
    raw (string_of_int n)

  let uint32 m =
    raw (Printf.sprintf "%lu" m)

  let uint64 m =
    raw (Printf.sprintf "%Lu" m)

  let label l =
    raw (Mutf7.encode l)

  let list ?(sep = ' ') f l =
    let rec loop = function
      | [] -> empty
      | [x] -> f x
      | x :: xs -> Cat (f x, Cat (Raw (String.make 1 sep), loop xs))
    in
    loop l

  let plist ?sep f l =
    Cat (Raw "(", Cat (list ?sep f l, Raw ")"))

  let date {day; month; year} =
    let months =
      [|
        "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
        "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec";
      |]
    in
    raw (Printf.sprintf "%d-%s-%4d" day months.(month) year)

  let eset s =
    let elt = function 0l -> "*" | n -> Printf.sprintf "%lu" n in
    let f = function
      | (lo, hi) when lo = hi -> raw (elt lo)
      | (lo, hi) -> raw (Printf.sprintf "%s:%s" (elt lo) (elt hi))
    in
    list ~sep:',' f s

    let string_of_capability = function
      | ACL -> "ACL"
      | BINARY -> "BINARY"
      | CATENATE -> "CATENATE"
      | CHILDREN -> "CHILDREN"
      | COMPRESS_DEFLATE -> "COMPRESS=DEFLATE"
      | CONDSTORE -> "CONDSTORE"
      | ENABLE -> "ENABLE"
      | IDLE -> "IDLE"
      | ID -> "ID"
      | LITERALPLUS -> "LITERAL+"
      | LITERALMINUS -> "LITERAL-"
      | UTF8_ACCEPT -> "UTF8=ACCEPT"
      | UTF8_ONLY -> "UTF8=ONLY"
      | MULTIAPPEND -> "MULTIAPPEND"
      | NAMESPACE -> "NAMESPACE"
      | QRESYNC -> "QRESYNC"
      | QUOTE -> "QUOTE"
      | SORT -> "SORT"
      | STARTTLS -> "STARTTLS"
      | UIDPLUS -> "UIDPLUS"
      | UNSELECT -> "UNSELECT"
      | XLIST -> "XLIST"
      | AUTH_ANONYMOUS -> "AUTH=ANONYMOUS"
      | AUTH_LOGIN -> "AUTH=LOGIN"
      | AUTH_PLAIN -> "AUTH=PLAIN"
      | XOAUTH2 -> "XOAUTH2"
      | X_GM_EXT_1 -> "X-GM-EXT-1"
      | OTHER s -> s

  let capability s =
    raw (string_of_capability s)

  let flag = function
    | Flag.Answered -> raw "\\Answered"
    | Flagged -> raw "\\Flagged"
    | Deleted -> raw "\\Deleted"
    | Seen -> raw "\\Seen"
    | Draft -> raw "\\Draft"
    | Keyword s -> raw s
    | Extension s -> raw ("\\" ^ s)
    | Recent -> raw "\\Recent"
    | Any -> raw "\\*"
end

module Code = struct
  type code =
    | ALERT
    | BADCHARSET of string list
    | CAPABILITY of capability list
    | PARSE
    | PERMANENTFLAGS of Flag.flag list
    | READ_ONLY
    | READ_WRITE
    | TRYCREATE
    | UIDNEXT of int32
    | UIDVALIDITY of int32
    | UNSEEN of int32
    | OTHER of string * string option
    | CLOSED
    | HIGHESTMODSEQ of int64
    | NOMODSEQ
    | MODIFIED of (int32 * int32) list
    | APPENDUID of int32 * int32
    | COPYUID of int32 * (int32 * int32) list * (int32 * int32) list
    | UIDNOTSTICKY
    | COMPRESSIONACTIVE
    | USEATTR [@@deriving sexp]
end

module Response = struct
  type state =
    | OK of Code.code option * string
    | NO of Code.code option * string
    | BAD of Code.code option * string [@@deriving sexp]

  type section =
    | HEADER
    | HEADER_FIELDS of string list
    | HEADER_FIELDS_NOT of string list
    | TEXT
    | MIME
    | Part of int * section
    | All
  [@@deriving sexp]

  type msg_att =
    | FLAGS of Flag.flag list
    | ENVELOPE of envelope
    | INTERNALDATE of date * time
    | RFC822 of string option
    | RFC822_HEADER of string option
    | RFC822_TEXT of string option
    | RFC822_SIZE of int
    | BODY of MIME.mime
    | BODYSTRUCTURE of MIME.mime
    | BODY_SECTION of section * int option * string option
    | UID of int32
    | MODSEQ of int64
    | X_GM_MSGID of int64
    | X_GM_THRID of int64
    | X_GM_LABELS of string list [@@deriving sexp]

  type mbx_att =
    | MESSAGES of int
    | RECENT of int
    | UIDNEXT of int32
    | UIDVALIDITY of int32
    | UNSEEN of int
    | HIGHESTMODSEQ of int64 [@@deriving sexp]

  type untagged =
    | State of state
    | BYE of Code.code option * string
    | PREAUTH of Code.code option * string
    | FLAGS of Flag.flag list
    | LIST of MbxFlag.mbx_flag list * char option * string
    | LSUB of MbxFlag.mbx_flag list * char option * string
    | SEARCH of int32 list * int64 option
    | STATUS of string * mbx_att list
    | EXISTS of int
    | RECENT of int
    | EXPUNGE of int32
    | FETCH of int32 * msg_att list
    | CAPABILITY of capability list
    | VANISHED of Uint32Set.t
    | VANISHED_EARLIER of Uint32Set.t
    | ENABLED of capability list [@@deriving sexp]

  type response =
    | Untagged of untagged
    | Cont of string
    | Tagged of string * state [@@deriving sexp]
end

module Fetch = struct
  type section = Response.section =
    | HEADER
    | HEADER_FIELDS of string list
    | HEADER_FIELDS_NOT of string list
    | TEXT
    | MIME
    | Part of int * section
    | All [@@deriving sexp]

  open E

  type t = rope

  let rec section = function
    | Response.HEADER -> raw "HEADER"
    | HEADER_FIELDS l -> raw "HEADER.FIELDS" ++ plist str l
    | HEADER_FIELDS_NOT l -> raw "HEADER.FIELDS.NOT" ++ plist str l
    | TEXT -> raw "TEXT"
    | MIME -> raw "MIME"
    | Part (n, s) -> Cat (int n, Cat (str ".", section s))
    | All -> empty

  let envelope = raw "ENVELOPE"
  let internaldate = raw "INTERNALDATE"
  let rfc822_header = raw "RFC822.HEADER"
  let rfc822_text = raw "RFC822.TEXT"
  let rfc822_size = raw "RFC822.SIZE"
  let rfc822 = raw "RFC822"
  let body = raw "BODY"
  let body_section ~peek s partial =
    let cmd = if peek then "BODY.PEEK" else "BODY" in
    let partial =
      match partial with
      | None -> empty
      | Some (n, l) -> raw (Printf.sprintf "<%d.%d>" n l)
    in
    Cat (raw cmd, Cat (raw "[", Cat (section s, Cat (raw "]", partial))))
  let bodystructure = raw "BODYSTRUCTURE"
  let uid = raw "UID"
  let flags = raw "FLAGS"

  let all = [flags; internaldate; rfc822_size; envelope]
  let fast = [flags; internaldate; rfc822_size]
  let full = [flags; internaldate; rfc822_size; envelope; body]

  let x_gm_msgid = raw "X-GM-MSGID"
  let x_gm_thrid = raw "X-GM-THRID"
  let x_gm_labels = raw "X-GM-LABELS"

  type response =
    {
      flags: Flag.flag list option;
      envelope: envelope option;
      internaldate: (date * time) option;
      rfc822: string option;
      rfc822_header: string option;
      rfc822_text: string option;
      rfc822_size: int option;
      body: MIME.mime option;
      bodystructure: MIME.mime option;
      body_section: (section * int option * string option) option;
      uid: Uid.t option;
      modseq: Modseq.t option;
      x_gm_msgid: Modseq.t option;
      x_gm_thrid: Modseq.t option;
      x_gm_labels: string list option;
    } [@@deriving sexp]

  let default =
    {
      flags = None;
      envelope = None;
      internaldate = None;
      rfc822 = None;
      rfc822_header = None;
      rfc822_text = None;
      rfc822_size = None;
      body = None;
      bodystructure = None;
      body_section = None;
      uid = None;
      modseq = None;
      x_gm_msgid = None;
      x_gm_thrid = None;
      x_gm_labels = None;
    }
end

module Status = struct
  type mbx_att_request =
    | MESSAGES
    | RECENT
    | UIDNEXT
    | UIDVALIDITY
    | UNSEEN
    | HIGHESTMODSEQ [@@deriving sexp]

  open E

  type t = rope

  let enc = function
    | (MESSAGES : mbx_att_request) -> raw "MESSAGES"
    | RECENT -> raw "RECENT"
    | UIDNEXT -> raw "UIDNEXT"
    | UIDVALIDITY -> raw "UIDVALIDITY"
    | UNSEEN -> raw "UNSEEN"
    | HIGHESTMODSEQ -> raw "HIGHESTMODSEQ"

  type response =
    {
      messages: int option;
      recent: int option;
      uidnext: Uid.t option;
      uidvalidity: Uid.t option;
      unseen: int option;
      highestmodseq: Modseq.t option;
    } [@@deriving sexp]

  let default =
    {
      messages = None;
      recent = None;
      uidnext = None;
      uidvalidity = None;
      unseen = None;
      highestmodseq = None;
    }
end

module Decoder = struct
  open Response
  open Angstrom

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
    char '\r' *> char '\n' *> return ()

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

  let is_nz_digit = function
    | '1'..'9' -> true
    | _ -> false

  let number =
    Int32.of_string <$> take_while1 is_digit

  let number =
    number <?> "number"

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
      ]

  let quoted_char =
    quoted_char <?> "quoted-char"

  let accumulate p =
    let b = Buffer.create 0 in
    let rec loop () =
      choice [p >>| (fun c -> `Add c); return `Stop] >>= function
      | `Add c ->
          Buffer.add_char b c;
          loop ()
      | `Stop ->
          return (Buffer.contents b)
    in
    (Buffer.add_char b <$> p) >>= loop

  let quoted =
    (char '"' *> accumulate quoted_char <* char '"') <?> "quoted"

(*
   literal         = "{" number "}" CRLF *CHAR8
                       ; Number represents the number of CHAR8s

   string          = quoted / literal
*)

  let literal =
    Int32.to_int <$> (char '{' *> number <* char '}' <* crlf) >>= take

  let literal =
    literal <?> "literal"

  let imap_string =
    choice [quoted; literal] <?> "string"

(*
   ASTRING-CHAR   = ATOM-CHAR / resp-specials

   astring         = 1*ASTRING-CHAR / string
*)

  let is_astring_char c =
    is_atom_char c || c = ']'

  let astring =
    choice [take_while1 is_astring_char; imap_string]

(*
   TEXT-CHAR       = <any CHAR except CR and LF>

   text            = 1*TEXT-CHAR
*)

  let is_text_char = function
    | '\r' | '\n' -> false
    | '\x01' .. '\x7F' -> true
    | _ -> false

  let text =
    choice [take_while1 is_text_char; return ""] (* allow empty texts for greater tolerance *)

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
    atom >>| fun s -> Flag.Keyword s

  let flag_keyword =
    flag_keyword <?> "flag-keyword"

  let flag_extension =
    char '\\' *> atom >>| fun s -> Flag.Extension s

  let flag_extension =
    flag_extension <?> "flag-extension"

  let switch cases =
    choice (List.map (fun (s, p) -> string_ci s *> commit *> p) cases)

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

  let sp = char ' '

  let pair sep p1 p2 =
    p1 >>= fun a1 -> sep *> p2 >>| fun a2 -> (a1, a2)

  let triple sep p1 p2 p3 =
    p1 >>= fun a1 -> sep *> p2 >>= fun a2 -> sep *> p3 >>| fun a3 -> (a1, a2, a3)

  let psep_by1 sep p =
    char '(' *> sep_by1 sep p <* char ')'

  let psep_by sep p =
    char '(' *> sep_by sep p <* char ')'

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
    let cases =
      [
        "COMPRESS=DEFLATE", return COMPRESS_DEFLATE;
        "CONDSTORE", return CONDSTORE;
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
    Modseq.of_string <$> take_while1 is_digit (* FIXME non zero *)

  let mod_sequence_value =
    mod_sequence_value <?> "mod-sequence-value"

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
    let resp_text_code = char '[' *> some resp_text_code <* char ']' <* char ' ' in
    pair (return ()) (option None resp_text_code) text <?> "resp-text"

(*
   resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
                       ; Status condition
*)

  let resp_cond_state =
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
    let open MbxFlag in
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
    Modseq.of_string <$> take_while1 is_digit

  let status_att =
    let cases =
      [
        "MESSAGES", sp *> number >>| (fun n -> (MESSAGES (Int32.to_int n) : mbx_att));
        "RECENT", sp *> number >>| (fun n -> (RECENT (Int32.to_int n) : mbx_att));
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
    return {ad_name; ad_adl; ad_mailbox; ad_host}

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
        env_date;
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
    body_fld_param >>= fun fld_params ->
    sp *> nstring >>= fun fld_id ->
    sp *> nstring >>= fun fld_desc ->
    sp *> imap_string >>= fun fld_enc ->
    sp *> body_fld_octets >>| Int32.to_int >>| fun fld_octets ->
    {MIME.fld_params; fld_id; fld_desc; fld_enc; fld_octets}

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
    let open MIME in
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
    option None (sp *> some body_fld_dsp) >>= function
    | None -> return {MIME.ext_dsp = None; ext_lang = []; ext_loc = None; ext_ext = []}
    | Some ext_dsp ->
        option None (sp *> some body_fld_lang) >>= function
        | None -> return {MIME.ext_dsp; ext_lang = []; ext_loc = None; ext_ext = []}
        | Some ext_lang ->
            option None (sp *> some body_fld_loc) >>= function
            | None -> return {MIME.ext_dsp; ext_lang; ext_loc = None; ext_ext = []}
            | Some ext_loc ->
                many (sp *> body_extension) >>| fun ext_ext ->
                {MIME.ext_dsp; ext_lang; ext_loc; ext_ext}

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

  let body_type_mpart body body_type_mpart = (* TODO Return the extension data *)
    many1 body >>= fun bodies ->
    sp *> imap_string >>= fun media_subtype ->
    option None (sp *> some body_ext_mpart) >>| fun _ ->
    MIME.Multipart (bodies, media_subtype)

  let body_type_mpart body =
    fix (body_type_mpart body) <?> "body-type-mpart"

  let body_type_basic media_type media_subtype =
    let aux =
      sp *> body_fields >>| fun body_fields ->
      MIME.Basic (media_type, media_subtype, body_fields)
    in
    aux <?> "body-type-basic"

  let body_type_msg body =
    let aux =
      sp *> body_fields >>= fun body_fields ->
      sp *> envelope >>= fun envelope ->
      sp *> body >>= fun body ->
      sp *> body_fld_lines >>| fun body_fld_lines ->
      MIME.Message (body_fields, envelope, body, body_fld_lines)
    in
    aux <?> "body-type-msg"

  let body_type_text media_subtype =
    let aux =
      sp *> body_fields >>= fun body_fields ->
      sp *> body_fld_lines >>| fun body_fld_lines ->
      MIME.Text (media_subtype, body_fields, body_fld_lines)
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
    triple (char ':') digits2 digits2 digits2

  let time =
    time <?> "time"

  let zone =
    switch
      [
        "+", digits4;
        "-", digits4 >>| (fun n -> -n);
      ]

  let zone =
    zone <?> "zone"

  let date_time =
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
    choice [section_msgtext; switch ["MIME", return MIME]] <?> "section-text"

  let section_part =
    sep_by1 (char '.') nz_number <?> "section-part"

  let section_spec =
    let aux =
      section_part >>= fun l ->
      choice [section_text; return All] >>| fun p ->
      List.fold_right (fun i x -> Part (Int32.to_int i, x)) l p
    in
    choice [section_msgtext; aux] <?> "section-spec"

  let section =
    (char '[' *> choice [section_spec; return All] <* char ']') <?> "section"

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
    let cases =
      [
        "FLAGS", sp *> psep_by sp flag_fetch >>| (fun l -> (FLAGS l : msg_att));
        "MODSEQ", sp *> permsg_modsequence >>| (fun n -> MODSEQ n);
        "X-GM-LABELS", sp *> choice [psep_by sp astring; nil *> return []] >>| (fun l -> X_GM_LABELS l);
      ]
    in
    switch cases <?> "msg-att-dynamic"

  let msg_att_static =
    let section =
      section >>= fun s ->
      option None (char '<' *> some (Int32.to_int <$> number) <* char '>') >>= fun r ->
      sp *> nstring >>| fun x ->
      BODY_SECTION (s, r, x)
    in
    let cases =
      [
        "ENVELOPE", sp *> envelope >>| (fun e -> ENVELOPE e);
        "INTERNALDATE", sp *> date_time >>| (fun (d, t) -> INTERNALDATE (d, t));
        "RFC822.HEADER", sp *> nstring >>| (fun s -> RFC822_HEADER s);
        "RFC822.TEXT", sp *> nstring >>| (fun s -> RFC822_TEXT s);
        "RFC822.SIZE", sp *> number >>| (fun n -> RFC822_SIZE (Int32.to_int n));
        "RFC822", sp *> nstring >>| (fun s -> RFC822 s);
        "BODYSTRUCTURE", sp *> body >>| (fun b -> BODYSTRUCTURE b);
        "BODY", choice [sp *> body >>| (fun b -> BODY b); section];
        "UID", sp *> uniqueid >>| (fun n -> UID n);
        "X-GM-MSGID", sp *> mod_sequence_value >>| (fun n -> X_GM_MSGID n);
        "X-GM-THRID", sp *> mod_sequence_value >>| (fun n -> X_GM_THRID n);
      ]
    in
    switch cases <?> "msg-att-static"

  let msg_att =
    psep_by1 sp (msg_att_static <|> msg_att_dynamic) <?> "msg-att"

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

  let flag_list =
    psep_by sp flag <?> "flag-list"

  let status_att_list =
    sep_by sp status_att <?> "status-att-list"

  let search_sort_mod_seq =
    (char '(' *> switch ["MODSEQ", sp *> mod_sequence_value] <* char ')') <?> "search-sort-mod-seq"

  let mailbox_data =
    let cases =
      [
        "FLAGS", sp *> flag_list >>| (fun l -> FLAGS l);
        "LIST", sp *> mailbox_list >>| (fun (xs, c, m) -> LIST (xs, c, m));
        "LSUB", sp *> mailbox_list >>| (fun (xs, c, m) -> LSUB (xs, c, m));
        "SEARCH", pair (return ()) (many (sp *> nz_number)) (option None (sp *> some search_sort_mod_seq)) >>| (fun (acc, n) -> SEARCH (acc, n));
        "STATUS", sp *> pair sp mailbox (char '(' *> status_att_list <* char ')') >>| (fun (m, l) ->
            STATUS (m, l));
      ]
    in
    let otherwise =
      number >>| Int32.to_int >>= fun n ->
      let cases =
        [
          "EXISTS", return (EXISTS n);
          "RECENT", return (RECENT n);
        ]
      in
      sp *> switch cases
    in
    choice [switch cases; otherwise] <?> "mailbox-data"

  let capability_data =
    let cases =
      [
        "CAPABILITY", many (sp *> capability) >>| (fun l -> CAPABILITY l);
      ]
    in
    switch cases <?> "capability-date"

  let enable_data =
    let cases =
      [
        "ENABLED", many (sp *> capability) >>| (fun l -> ENABLED l);
      ]
    in
    switch cases <?> "enable-data"

  let resp_cond_bye =
    let cases =
      [
        "BYE", sp *> resp_text >>| (fun (c, t) -> BYE (c, t));
      ]
    in
    switch cases <?> "resp-cond-bye"

  let known_ids =
    uid_set

  let expunged_resp =
    let cases =
      [
        "VANISHED (EARLIER)", sp *> known_ids >>| (fun l -> VANISHED_EARLIER l);
        "VANISHED", sp *> known_ids >>| (fun l -> VANISHED l);
      ]
    in
    switch cases <?> "expunged-resp"

  let message_data =
    let cases n =
      [
        "EXPUNGE", return (EXPUNGE n);
        "FETCH", sp *> msg_att >>| (fun x -> FETCH (n, x));
      ]
    in
    (nz_number >>= fun n -> sp *> choice [switch (cases n); expunged_resp]) <?> "message-data"

  let resp_cond_auth =
    let cases =
      [
        "PREAUTH", sp *> resp_text >>| (fun (c, t) -> PREAUTH (c, t));
      ]
    in
    switch cases <?> "resp-cond-auth"

  let response_data =
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
          resp_cond_auth; (* CHECK *)
        ]
    in
    char '*' *> sp *> data <* crlf

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

  let is_tag_char c =
    is_astring_char c && c != '+'

  let tag =
    take_while1 is_tag_char <?> "tag"

  let response_tagged =
    let aux =
      pair sp tag resp_cond_state >>| fun (tag, state) -> Tagged (tag, state)
    in
    (aux <* crlf) <?> "response-tagged"

  let continue_req =
    let resp_text = resp_text >>| fun (_, x) -> Cont x in
    (char '+' *> choice [sp *> resp_text; resp_text] <* crlf) <?> "continue-req"
  (* space is optional CHECKME ! base64 *)

  let response =
    let response_data = response_data >>| fun x -> Untagged x in
    choice [continue_req; response_data; response_tagged] <?> "response"

  let decode s =
    match parse_only response (`String s) with
    | Ok x -> x
    | Error s -> failwith s
end

module Search = struct
  open E

  type key = rope [@@deriving sexp]

  let all = raw "ALL"
  let seq s = eset s
  let answered = raw "ANSWERED"
  let bcc s = raw "BCC" ++ str s
  let before t = raw "BEFORE" ++ date t
  let body s = raw "BODY" ++ str s
  let cc s = raw "CC" ++ str s
  let deleted = raw "DELETED"
  let draft = raw "DRAFT"
  let flagged = raw "FLAGGED"
  let from s = raw "FROM" ++ str s
  let header s1 s2 = raw "HEADER" ++ str s1 ++ str s2
  let keyword s = raw "KEYWORD" ++ str s
  let larger n = raw "LARGER" ++ int n
  let new_ = raw "NEW"
  let not k = raw "NOT" ++ p k
  let old = raw "OLD"
  let on t = raw "ON" ++ date t
  let (||) k1 k2 = raw "OR" ++ p k1 ++ p k2
  let recent = raw "RECENT"
  let seen = raw "SEEN"
  let sent_before t = raw "SENTBEFORE" ++ date t
  let sent_on t = raw "SENTON" ++ date t
  let sent_since t = raw "SENTSINCE" ++ date t
  let since t = raw "SINCE" ++ date t
  let smaller n = raw "SMALLER" ++ int n
  let subject s = raw "SUBJECT" ++ str s
  let text s = raw "TEXT" ++ str s
  let to_ s = raw "TO" ++ str s
  let uid s = raw "UID" ++ eset s
  let unanswered = raw "UNANSWERED"
  let undeleted = raw "UNDELETED"
  let undraft = raw "UNDRAFT"
  let unflagged = raw "UNFLAGGED"
  let unkeyword s = raw "UNKEYWORD" ++ str s
  let unseen = raw "UNSEEN"
  let (&&) k1 k2 = p k1 ++ p k2
  let modseq n = raw "MODSEQ" ++ uint64 n
  let x_gm_raw s = raw "X-GM-RAW" ++ str s
  let x_gm_msgid n = raw "X-GM-MSGID" ++ uint64 n
  let x_gm_thrid n = raw "X-GM-THRID" ++ uint64 n
  let x_gm_labels l = raw "X-GM-LABELS" ++ list str l
end

module A = Angstrom.Buffered
module R = Response

type error =
  | Incorrect_tag of string * string
  | Decode_error of string * int
  | Unexpected_cont
  | Bad_greeting
  | Auth_error of string
  | No of string
  | Bad of string [@@deriving sexp]

open Lwt.Infix

type t =
  {
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;

    mutable tag: int;
    mutable unconsumed: A.unconsumed;

    mutable uidnext: Uid.t option;
    mutable messages: int option;
    mutable recent: int option;
    mutable unseen: int option;
    mutable uidvalidity: Uid.t option;

    mutable capabilities: capability list;
  }

let create_connection ic oc unconsumed =
  {
    ic;
    oc;
    tag = 0;
    unconsumed;
    uidnext = None;
    uidvalidity = None;
    recent = None;
    messages = None;
    unseen = None;
    capabilities = [];
  }

let tag {tag; _} =
  Printf.sprintf "%04d" tag

let uidnext {uidnext; _} =
  uidnext

let messages {messages; _} =
  messages

let recent {recent; _} =
  recent

let unseen {unseen; _} =
  unseen

let uidvalidity {uidvalidity; _} =
  uidvalidity

let parse {A.buffer; off; len} p =
  let input = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
  Bigarray.Array1.blit (Bigarray.Array1.sub buffer off len) input;
  let input = `Bigstring input in
  A.parse ~input p

let recv conn =
  let rec loop = function
    | A.Partial f ->
        Lwt_io.read ~count:128 conn.ic >>= fun s ->
        loop (f (`String s))
    | Done (unconsumed, r) ->
        conn.unconsumed <- unconsumed;
        Lwt.return r
    | Fail _ ->
        Lwt.fail (Failure "parse error")
  in
  loop (parse conn.unconsumed Decoder.response)

let rec send imap r process res =
  match r with
  | E.Cat (r1, r2) ->
      send imap r1 process res >>= fun res ->
      send imap r2 process res
  | Wait ->
      let rec loop res =
        recv imap >>= function
        | R.Cont _ ->
            Lwt.return res
        | R.Untagged u ->
            loop (process imap res u)
        | R.Tagged _ ->
            Lwt.fail (Failure "not expected")
      in
      Lwt_io.flush imap.oc >>= fun () -> loop res
  | Raw s ->
      Lwt_io.write imap.oc s >>= fun () ->
      Lwt.return res

let send imap r process res =
  send imap r process res >>= fun res ->
  Lwt_io.flush imap.oc >>= fun () ->
  Lwt.return res

let wrap_process f imap res u =
  begin match u with
  | R.State (OK (Some (UIDNEXT n), _)) ->
      imap.uidnext <- Some n
  | State (OK (Some (UIDVALIDITY n), _)) ->
      imap.uidvalidity <- Some n
  | State (OK (Some (CAPABILITY caps), _)) ->
      imap.capabilities <- caps
  | RECENT n ->
      imap.recent <- Some n
  | EXISTS n ->
      imap.messages <- Some n
  | STATUS (_, items) ->
      List.iter (function
          | R.MESSAGES n ->
              imap.messages <- Some n
          | RECENT n ->
              imap.recent <- Some n
          | UIDNEXT n ->
              imap.uidnext <- Some n
          | UIDVALIDITY n ->
              imap.uidvalidity <- Some n
          | UNSEEN n ->
              imap.unseen <- Some n
          | _ ->
              ()
        ) items
  | CAPABILITY caps ->
      imap.capabilities <- caps
  | _ ->
      ()
  end;
  f imap res u

let run imap format res process =
  let process = wrap_process process in
  let tag = tag imap in
  let r = E.(raw tag ++ format ++ raw "\r\n") in
  send imap r process res >>= fun res ->
  let rec loop res =
    recv imap >>= function
    | R.Cont _ ->
        Lwt.fail (Failure "unexpected")
    | Untagged u as r ->
        Printf.eprintf "%s\n%!" (Sexplib.Sexp.to_string_hum (R.sexp_of_response r));
        loop (process imap res u)
    | Tagged (t, _) as r ->
        Printf.eprintf "%s\n%!" (Sexplib.Sexp.to_string_hum (R.sexp_of_response r));
        imap.tag <- imap.tag + 1;
        Lwt.return res
  in
  loop res

(* let idle session = *)
(*   let tag = tag session in *)
(*   let format = E.(raw tag ++ raw "IDLE") in *)
(*   let state = Sending (format, WaitForResp (wait_for_idle tag)) in *)
(*   continue (session, state) *)

let login conn username password =
  let format = E.(str "LOGIN" ++ str username ++ str password) in
  let process conn _ _ = () in
  run conn format () process

let capability imap=
  let format = E.(str "CAPABILITY") in
  let process _ caps = function
    | R.CAPABILITY caps1 -> caps @ caps1
    | _ -> caps
  in
  run imap format [] process

let create imap m =
  let format = E.(str "CREATE" ++ mailbox m) in
  let process _ () _ = () in
  run imap format () process

let delete imap m =
  let format = E.(str "DELETE" ++ mailbox m) in
  let process _ () _ = () in
  run imap format () process

let rename imap m1 m2 =
  let format = E.(str "RENAME" ++ mailbox m1 ++ mailbox m2) in
  let process _ () _ = () in
  run imap format () process

let logout imap =
  let format = E.(str "LOGOUT") in
  let process _ () _ = () in
  run imap format () process

let noop imap =
  let format = E.(str "NOOP") in
  let process _ () _ = () in
  run imap format () process

let list imap ?(ref = "") s =
  let format = E.(str "LIST" ++ mailbox ref ++ str s) in
  let process _ res = function
    | R.LIST (flags, delim, mbox) -> res @ [flags, delim, mbox] (* CHECK *)
    | _ -> res
  in
  run imap format [] process

let status imap m att =
  let format = E.(str "STATUS" ++ mailbox m ++ p (list Status.enc att)) in
  let process _ res = function
    | R.STATUS (mbox, items) when m = mbox ->
        let aux resp = function
          | (MESSAGES n : R.mbx_att) -> {res with Status.messages = Some n}
          | RECENT n -> {res with recent = Some n}
          | UIDNEXT n -> {res with uidnext = Some n}
          | UIDVALIDITY n -> {res with uidvalidity = Some n}
          | UNSEEN n -> {res with unseen = Some n}
          | HIGHESTMODSEQ n -> {res with highestmodseq = Some n}
        in
        List.fold_left aux res items
    | _ ->
        res
  in
  run imap format Status.default process

let copy_gen imap cmd s m =
  let format = E.(raw cmd ++ eset s ++ mailbox m) in
  let process _ () _ = () in
  run imap format () process

let copy imap s m =
  copy_gen imap "COPY" s m

let uid_copy imap s m =
  copy_gen imap "UID COPY" s m

let check imap =
  let format = E.(str "CHECK") in
  let process _ () _ = () in
  run imap format () process

let close imap =
  let format = E.(str "CLOSE") in
  let process _ () _ = () in
  run imap format () process

let expunge imap =
  let format = E.(str "EXPUNGE") in
  let process _ info = function
    | R.EXPUNGE n -> n :: info
    | _ -> info
  in
  run imap format [] process

let search_gen imap cmd sk =
  let format = E.(raw cmd ++ sk) in
  let process _ (res, m) = function
    | R.SEARCH (ids, m1) -> ids @ res, m1
    | _ -> (res, m)
  in
  run imap format ([], None) process

let search imap =
  search_gen imap "SEARCH"

let uid_search imap =
  search_gen imap "UID SEARCH"

let select_gen imap cmd m =
  let format = E.(raw cmd ++ mailbox m) in
  let process _ () _ = () in
  run imap format () process

let condstore_select_gen imap cmd m =
  let format = E.(raw cmd ++ mailbox m ++ p (raw "CONDSTORE")) in
  let process _ m = function
    | R.State (OK (Some (Code.HIGHESTMODSEQ m), _)) -> m
    | _ -> m
  in
  run imap format 0L process

let select imap ?(read_only = false) mbox =
  select_gen imap (if read_only then "EXAMINE" else "SELECT") mbox

let condstore_select imap ?(read_only = false) mbox =
  condstore_select_gen imap (if read_only then "EXAMINE" else "SELECT") mbox

let append imap m ?(flags = []) data =
  let format = E.(raw "APPEND" ++ mailbox m ++ p (list flag flags) ++ literal data) in
  let process _ () _ = () in
  run imap format () process

let fetch_gen imap cmd ?changed_since set att =
  let open E in
  let att =
    match att with
    (* | `Fast -> raw "FAST" *)
    (* | `Full -> raw "FULL" *)
    (* | `All -> raw "ALL" *)
    | [x] -> x
    | xs -> p (list (fun x -> x) xs)
  in
  let changed_since =
    match changed_since with
    | None -> empty
    | Some m -> p (raw "CHANGEDSINCE" ++ uint64 m ++ raw "VANISHED")
  in
  let format = raw cmd ++ eset set ++ att ++ changed_since in
  let process _ res = function
    | R.FETCH (id, infos) ->
        let aux res = function
          | (Response.FLAGS l : R.msg_att) -> {res with Fetch.flags = Some l}
          | ENVELOPE e -> {res with envelope = Some e}
          | INTERNALDATE (d, t) -> {res with internaldate = Some (d, t)}
          | RFC822 (Some s) -> {res with rfc822 = Some s}
          | RFC822 None -> {res with rfc822 = Some ""}
          | RFC822_HEADER (Some s) -> {res with rfc822_header = Some s}
          | RFC822_HEADER None -> {res with rfc822_header = Some ""}
          | RFC822_TEXT (Some s) -> {res with rfc822_text = Some s}
          | RFC822_TEXT None -> {res with rfc822_text = Some ""}
          | RFC822_SIZE n -> {res with rfc822_size = Some n}
          | BODY x -> {res with body = Some x}
          | BODYSTRUCTURE x -> {res with bodystructure = Some x}
          | BODY_SECTION (sec, len, s) -> {res with body_section = Some (sec, len, s)}
          | UID n -> {res with uid = Some n}
          | MODSEQ n -> {res with modseq = Some n}
          | X_GM_MSGID n -> {res with x_gm_msgid = Some n}
          | X_GM_THRID n -> {res with x_gm_thrid = Some n}
          | X_GM_LABELS n -> {res with x_gm_labels = Some n}
        in
        List.fold_left aux res infos
    | _ ->
        res
  in
  run imap format Fetch.default process

let fetch imap ?changed_since set att =
  fetch_gen imap "FETCH" ?changed_since set att

let uid_fetch imap ?changed_since set att =
  fetch_gen imap "UID FETCH" ?changed_since set att

type store_mode =
  [`Add | `Remove | `Set]

type store_kind =
  [`Flags of Flag.flag list | `Labels of string list]

let store_gen imap cmd ?(silent = false) ?unchanged_since mode set att =
  let open E in
  let mode = match mode with `Add -> "+" | `Set -> "" | `Remove -> "-" in
  let silent = if silent then ".SILENT" else "" in
  let base =
    match att with
    | `Flags _ ->
        Printf.sprintf "%sFLAGS%s" mode silent
    | `Labels _ ->
        Printf.sprintf "%sX-GM-LABELS%s" mode silent
  in
  let att =
    match att with
    | `Flags flags -> list flag flags
    | `Labels labels -> list label labels
  in
  let unchanged_since =
    match unchanged_since with
    | None -> str ""
    | Some m -> p (raw "UNCHANGEDSINCE" ++ uint64 m)
  in
  let format = raw cmd ++ eset set ++ unchanged_since ++ raw base ++ p att in
  let process _ m _ = m in
  run imap format Fetch.default process

let store imap =
  store_gen imap "STORE"

let uid_store imap =
  store_gen imap "UID STORE"

let enable imap caps =
  let format = E.(str "ENABLE" ++ list capability caps) in
  let process _ caps = function
    | R.ENABLED caps1 -> caps1 @ caps
    | _ -> caps
  in
  run imap format [] process

let () =
  Ssl.init ()

let connect server username password mailbox =
  let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname server >>= fun he ->
  let addr = Lwt_unix.ADDR_INET (he.Unix.h_addr_list.(0), 993) in
  Lwt_unix.connect sock addr >>= fun () ->
  Lwt_ssl.ssl_connect sock ctx >>= fun sock ->
  let ic = Lwt_ssl.in_channel_of_descr sock in
  let oc = Lwt_ssl.out_channel_of_descr sock in
  let imap =
    create_connection ic oc {A.buffer = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0; off = 0; len = 0}
  in
  recv imap >>= function
  | R.Untagged _ as r ->
      Printf.eprintf "%s\n%!" (Sexplib.Sexp.to_string_hum (R.sexp_of_response r));
      login imap username password >>= fun () ->
      select imap mailbox >>= fun () ->
      Lwt.return imap
  | Tagged _ | Cont _ ->
      Lwt.fail (Failure "unexpected response")

let disconnect imap =
  logout imap >>= fun () ->
  Lwt.join [Lwt_io.close imap.ic; Lwt_io.close imap.oc]
