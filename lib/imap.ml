(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

let pp_qstr ppf s = Format.fprintf ppf "%S" s
let pp_char ppf c = Format.fprintf ppf "%C" c
let pp_opt f ppf = function
  | None   -> Format.fprintf ppf "none"
  | Some c -> Format.fprintf ppf "%a" f c
let pp_list ?(pp_sep = Format.pp_print_space) =
  Format.pp_print_list ~pp_sep

module type NUMBER = sig
  type t
  val zero: t
  val of_int: int -> t
  val compare: t -> t -> int
  val print: Format.formatter -> t -> unit
end

module Uint32 = struct
  type t = int32
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
  let print ppf n =
    Format.fprintf ppf "%lu" n
end

module Modseq = struct
  include Int64
  let print ppf n =
    Format.fprintf ppf "%Lu" n
end
module Uid = Uint32
module Seq = Uint32

module type NUMBER_SET = sig
  type elt
  type t
  val singleton: elt -> t
  val union: t -> t -> t
  val add: elt -> t -> t
  val interval: elt -> elt -> t
end

module Uint32Set = struct
  type t = (int32 * int32) list (* disjoint, sorted intervals *)
  let singleton n = [(n, n)]
  let union s1 s2 =
    assert false
  let add n s =
    assert false
  let interval n m =
    [(n, m)]
end

module UidSet = Uint32Set
module SeqSet = Uint32Set

module Mutf7 = struct

  (* Modified UTF-7 *)

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
  }

type time =
  {
    hours: int;
    minutes: int;
    seconds: int;
    zone: int;
  }

module Capability = struct
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
    | OTHER of string

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

  open Format

  let pp ppf c =
    pp_print_string ppf (String.lowercase_ascii (string_of_capability c))
end

module Envelope = struct
  type address =
    {
      ad_name: string;
      ad_adl: string;
      ad_mailbox: string;
      ad_host: string;
    }

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
    }

  open Format

  let pp_address ppf x =
    fprintf ppf "@[<hv 2>(address@ (name %S)@ (addr %S)@ (mailbox %S)@ (host %S))@]"
      x.ad_name x.ad_adl x.ad_mailbox x.ad_host

  let pp_address_list =
    pp_list pp_address

  let pp_envelope ppf env =
    fprintf ppf
      "@[<hv 2>(envelope@ (date %S)@ (subject %S)@ \
       (from %a)@ (sender %a)@ (reply-to %a)@ \
       (to %a)@ (cc %a)@ (bcc %a)@ (in-reply-to %S)@ \
       (message-id %S))@]"
      env.env_date
      env.env_subject
      pp_address_list env.env_from
      pp_address_list env.env_sender
      pp_address_list env.env_reply_to
      pp_address_list env.env_to
      pp_address_list env.env_cc
      pp_address_list env.env_bcc
      env.env_in_reply_to
      env.env_message_id
end

module MIME = struct
  type fields =
    {
      fld_params : (string * string) list;
      fld_id : string option;
      fld_desc : string option;
      fld_enc : string;
      fld_octets : int;
    }

  type body_extension =
    | List of body_extension list
    | Number of int32
    | String of string

  type part_extension =
    {
      ext_dsp: (string * (string * string) list) option;
      ext_lang: string list;
      ext_loc: string option;
      ext_ext: body_extension list;
    }

  type mime =
    | Text of string * fields * int
    | Message of fields * Envelope.envelope * mime * int
    | Basic of string * string * fields
    | Multipart of mime list * string

  open Format

  let pp_param ppf (k, v) =
    fprintf ppf "(%S@ %S)" k v

  let pp_fields ppf f =
    fprintf ppf "@[<hv 2>(fields@ @[<hv 2>(params@ %a)@]@ (id %a)@ (desc %a)@ (enc %S)@ (octets %d)@]"
      (pp_list pp_param) f.fld_params (pp_opt pp_print_string) f.fld_id
      (pp_opt pp_print_string) f.fld_desc f.fld_enc
      f.fld_octets

  let rec pp_mime ppf = function
    | Text (m, f, i) ->
        fprintf ppf "@[<2>(text@ %S@ %a@ %d)@]" m pp_fields f i
    | Message (f, e, b, i) ->
        fprintf ppf "@[<2>(message@ %a@ %a@ %a@ %d)@]"
          pp_fields f Envelope.pp_envelope e pp_mime b i
    | Basic (m, t, f) ->
        fprintf ppf "@[<2>(basic@ %S@ %S@ %a)@]" m t pp_fields f
    | Multipart (b, m) ->
        fprintf ppf "@[<2>(multipart@ %a@ %S)@]" (pp_list pp_mime) b m
end

module E = struct
  type rope =
    | Cat of rope * rope
    | Flush
    | Wait
    | Raw of string

  let rec is_empty = function
    | Cat (f, g) -> is_empty f && is_empty g
    | Flush -> false
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

  let capability s =
    raw (Capability.string_of_capability s)

  type result =
    | WaitForCont of string * rope
    | Done of string

  let rec out b k = function
    | Cat (f, g) ->
        out b (Cat (g, k)) f
    | Flush ->
        Done (Buffer.contents b)
    | Wait ->
        WaitForCont (Buffer.contents b, k)
    | Raw s ->
        Buffer.add_string b s;
        out b empty k

  let out k =
    out (Buffer.create 32) empty (Cat (k, Cat (Raw "\r\n", Flush)))
end

module Msg = struct
  type flag =
    [ `Answered
    | `Flagged
    | `Deleted
    | `Seen
    | `Draft
    | `Keyword of string
    | `Extension of string ]

  let flag = function
    | `Answered -> E.raw "\\Answered"
    | `Flagged -> E.raw "\\Flagged"
    | `Deleted -> E.raw "\\Deleted"
    | `Seen -> E.raw "\\Seen"
    | `Draft -> E.raw "\\Draft"
    | `Keyword s -> E.raw s
    | `Extension s -> E.raw ("\\" ^ s)

  type section =
    | HEADER
    | HEADER_FIELDS of string list
    | HEADER_FIELDS_NOT of string list
    | TEXT
    | MIME
    | Part of int * section
    | All

  type msg_att =
    | FLAGS of [ flag | `Recent ] list
    | ENVELOPE of Envelope.envelope
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
    | X_GM_LABELS of string list

  module FetchData = struct
    module A = struct
      type 'a attr =
        | FLAGS : [ flag | `Recent ] list attr
        | ENVELOPE : Envelope.envelope attr
        | INTERNALDATE : (date * time) attr
        | RFC822 : string attr
        | RFC822_HEADER : string attr
        | RFC822_TEXT : string attr
        | RFC822_SIZE : int attr
        | BODY : MIME.mime attr
        | BODYSTRUCTURE : MIME.mime attr
        | BODY_SECTION : section * (int * int) option -> string attr
        | UID : Uid.t attr
        | MODSEQ : Modseq.t attr
        | X_GM_MSGID : Modseq.t attr
        | X_GM_THRID : Modseq.t attr
        | X_GM_LABELS : string list attr
    end

    type 'a attr = 'a A.attr

    let flags = A.FLAGS
    let envelope = A.ENVELOPE
    let internal_date = A.INTERNALDATE
    let rfc822 = A.RFC822
    let rfc822_headers = A.RFC822_HEADER
    let rfc822_text = A.RFC822_TEXT
    let rfc822_size = A.RFC822_SIZE
    let body = A.BODY
    let body_structure = A.BODYSTRUCTURE
    let body_section ?range sec = A.BODY_SECTION (sec, range)
    let uid = A.UID
    let modseq = A.MODSEQ
    let gmail_msgid = A.X_GM_MSGID
    let gmail_thrid = A.X_GM_THRID
    let gmail_labels = A.X_GM_LABELS

    type t = Seq.t * msg_att list

    let seq (n, _) =
      n

    let attr (_, l) a =
      let rec loop: type a. a A.attr -> msg_att list -> a = fun a l ->
        match l with
        | att :: rest ->
            begin match a, att with
            | A.FLAGS, FLAGS flags -> flags
            | A.ENVELOPE, ENVELOPE envelope -> envelope
            | A.INTERNALDATE, INTERNALDATE (date, time) -> (date, time)
            | A.RFC822, RFC822 (Some s) -> s
            | A.RFC822, RFC822 None -> ""
            | A.RFC822_HEADER, RFC822_HEADER (Some s) -> s
            | A.RFC822_HEADER, RFC822_HEADER None -> ""
            | A.RFC822_TEXT, RFC822_TEXT (Some s) -> s
            | A.RFC822_TEXT, RFC822_TEXT None -> ""
            | A.RFC822_SIZE, RFC822_SIZE n -> n
            | A.BODY, BODY mime -> mime
            | A.BODYSTRUCTURE, BODYSTRUCTURE mime -> mime
            | A.BODY_SECTION (sec, Some (_, len)), BODY_SECTION (sec1, Some len1, s)
              when sec = sec1 && len = len1 -> begin match s with None -> "" | Some s -> s end
            | A.BODY_SECTION (sec, None), BODY_SECTION (sec1, None, s) ->
                begin match s with Some s -> s | None -> "" end
            | A.UID, UID uid -> uid
            | A.MODSEQ, MODSEQ modseq -> modseq
            | A.X_GM_MSGID, X_GM_MSGID msgid -> msgid
            | A.X_GM_THRID, X_GM_THRID thrid -> thrid
            | A.X_GM_LABELS, X_GM_LABELS labels -> labels
            | _ -> loop a rest
            end
        | [] ->
            raise Not_found
      in
      loop a l
  end

  module Request = struct
    open E

    type t = rope

    let rec section = function
      | HEADER -> raw "HEADER"
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

    let x_gm_labels = raw "X-GM-LABELS"
  end

  open Format

  let pp_set ppf s =
    let rg ppf (x, y) = fprintf ppf "%lu-%lu" x y in
    pp_list rg ppf s

  let pp_date_time ppf (d, t) =
    fprintf ppf "@[(date %02d %02d %04d)@ (time %02d %02d %02d %04d)@]"
      d.day d.month d.year t.hours t.minutes t.seconds t.zone

  let pp_flag ppf = function
    | `Answered    -> fprintf ppf "answered"
    | `Flagged     -> fprintf ppf "flagged"
    | `Deleted     -> fprintf ppf "deleted"
    | `Seen        -> fprintf ppf "seen"
    | `Draft       -> fprintf ppf "draft"
    | `Keyword k   -> fprintf ppf "(keyword %S)" k
    | `Extension k -> fprintf ppf "(extension %S)" k

  let pp_flag_perm ppf = function
    | `All       -> fprintf ppf "all"
    | #flag as f -> pp_flag ppf f

  let pp_flag_fetch ppf = function
    | `Recent    -> fprintf ppf "recent"
    | #flag as f -> pp_flag ppf f

  let rec pp_section ppf = function
    | HEADER -> fprintf ppf "header"
    | HEADER_FIELDS l ->
        fprintf ppf "@[<2>(header-fields %a)@]" (pp_list pp_qstr) l
    | HEADER_FIELDS_NOT l ->
        fprintf ppf "@[<2>(header-fields-not %a)@]" (pp_list pp_qstr) l
    | TEXT -> fprintf ppf "text"
    | MIME -> fprintf ppf "mime"
    | Part (n, s) -> fprintf ppf "@[<2>(part %d@ %a)@]" n pp_section s
    | All -> fprintf ppf "all"

  let pp ppf = function
    | FLAGS r ->
        fprintf ppf "@[<2>(flags %a)@]" (pp_list pp_flag_fetch) r
    | ENVELOPE e -> Envelope.pp_envelope ppf e
    | INTERNALDATE (d, t) ->
        fprintf ppf "@[<2>(internal-date@ %a)@]" pp_date_time (d, t)
    | RFC822 s -> fprintf ppf "(rfc822 %a)" (pp_opt pp_qstr) s
    | RFC822_HEADER s -> fprintf ppf "(rfc822-header %a)" (pp_opt pp_qstr) s
    | RFC822_TEXT s -> fprintf ppf "(rfc822-text %a)" (pp_opt pp_qstr) s
    | RFC822_SIZE n -> fprintf ppf "(rfc822-size %i)" n
    | BODY b -> fprintf ppf "@[<2>(body@ %a)@]" MIME.pp_mime b
    | BODYSTRUCTURE b -> fprintf ppf "@[<2>(bodystructure@ %a)@]" MIME.pp_mime b
    | BODY_SECTION (s, n, x) ->
        fprintf ppf "@[<2>(body-section@ %a@ %a@ %a)@]"
          pp_section s (pp_opt Format.pp_print_int) n (pp_opt pp_qstr) x
    | UID n -> fprintf ppf "(uid %lu)" n
    | MODSEQ m -> fprintf ppf "(modseq %Lu)" m
    | X_GM_MSGID m -> fprintf ppf "(gm-msgid %Lu)" m
    | X_GM_THRID m -> fprintf ppf "(gm-thrid %Lu)" m
    | X_GM_LABELS l ->
        fprintf ppf "@[<2>(gm-labels@ %a)@]" (pp_list pp_qstr) l
end

module Code = struct
  type code =
    | ALERT
    | BADCHARSET of string list
    | CAPABILITY of Capability.capability list
    | PARSE
    | PERMANENTFLAGS of [ Msg.flag | `All ] list
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
    | USEATTR

  open Format

  let pp ppf = function
    | ALERT ->
        fprintf ppf "alert"
    | BADCHARSET cs ->
        fprintf ppf "@[<2>(badcharset %a)@]" (pp_list pp_print_string) cs
    | CAPABILITY caps ->
        fprintf ppf "@[<2>(capability %a)@]" (pp_list Capability.pp) caps
    | PARSE ->
        fprintf ppf "parse"
    | PERMANENTFLAGS fl ->
        fprintf ppf "@[<2>(permanent-flags %a)@]" (pp_list Msg.pp_flag_perm) fl
    | READ_ONLY ->
        fprintf ppf "read-only"
    | READ_WRITE ->
        fprintf ppf "read-write"
    | TRYCREATE ->
        fprintf ppf "try-create"
    | UIDNEXT uid ->
        fprintf ppf "(uid-next %lu)" uid
    | UIDVALIDITY uid ->
        fprintf ppf "(uid-validity %lu)" uid
    | UNSEEN n ->
        fprintf ppf "(unseen %lu)" n
    | OTHER (k, v) ->
        fprintf ppf "(other@ %S@ %a)" k (pp_opt pp_qstr) v
    | CLOSED ->
        fprintf ppf "closed"
    | HIGHESTMODSEQ m ->
        fprintf ppf "(highest-modseq %Lu)" m
    | NOMODSEQ ->
        fprintf ppf "no-modseq"
    | MODIFIED s ->
        fprintf ppf "(modified@ %a)" Msg.pp_set s
    | APPENDUID (n, m) ->
        fprintf ppf "(append-uid %lu@ %lu)" n m
    | COPYUID (n, s1, s2) ->
        fprintf ppf "(copy-uid %lu@ %a@ %a)" n Msg.pp_set s1 Msg.pp_set s2
    | UIDNOTSTICKY ->
        fprintf ppf "uid-not-sticky"
    | COMPRESSIONACTIVE ->
        fprintf ppf "compression-active"
    | USEATTR ->
        fprintf ppf "use-attr"
end

module Mailbox = struct
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
    | Extension of string

  type mbx_att =
    | MESSAGES of int
    | RECENT of int
    | UIDNEXT of int32
    | UIDVALIDITY of int32
    | UNSEEN of int32
    | HIGHESTMODSEQ of int64

  module Request = struct
    open E

    type t = rope

    let messages = raw "MESSAGES"
    let recent = raw "RECENT"
    let uidnext = raw "UIDNEXT"
    let uidvalidity = raw "UIDVALIDITY"
    let unseen = raw "UNSEEN"
    let highestmodseq = raw "HIGHESTMODSEQ"
  end

  open Format

  let pp_mbx_flag ppf = function
    | Noselect -> fprintf ppf "noselect"
    | Marked -> fprintf ppf "marked"
    | Unmarked -> fprintf ppf "unmarked"
    | Noinferiors -> fprintf ppf "noinferiors"
    | HasChildren -> fprintf ppf "has-children"
    | HasNoChildren -> fprintf ppf "has-no-children"
    | All -> fprintf ppf "all"
    | Archive -> fprintf ppf "archive"
    | Drafts -> fprintf ppf "drafts"
    | Flagged -> fprintf ppf "flagged"
    | Junk -> fprintf ppf "junk"
    | Sent -> fprintf ppf "sent"
    | Trash -> fprintf ppf "trash"
    | Extension s -> fprintf ppf "(extension %s)" s

  let pp_mbx_status ppf = function
    | MESSAGES n -> fprintf ppf "(messages %i)" n
    | RECENT n -> fprintf ppf "(recent %i)" n
    | UIDNEXT uid -> fprintf ppf "(uid-next %lu)" uid
    | UIDVALIDITY uid -> fprintf ppf "(uid-validity %lu)" uid
    | UNSEEN n -> fprintf ppf "(unseen %lu)" n
    | HIGHESTMODSEQ m -> fprintf ppf "(highest-modseq %Lu)" m
end

(** {3 Mailbox status responses}

    Mailbox status items returned in the untagged [`Status]
    {{!untagged}response} to the {!status} command. *)

module StatusData = struct
  type _ attr =
    | MESSAGES : int attr
    | RECENT : int attr
    | UIDNEXT : int32 attr
    | UIDVALIDITY : int32 attr
    | UNSEEN : int32 attr
    | HIGHESTMODSEQ : int64 attr

  let messages = MESSAGES
  let recent = RECENT
  let uidnext = UIDNEXT
  let uidvalidity = UIDVALIDITY
  let unseen = UNSEEN
  let highestmodseq = HIGHESTMODSEQ

  type t =
    Mailbox.mbx_att list

  let attr t a =
    let rec loop: type a. Mailbox.mbx_att list -> a attr -> a = fun l a ->
      match l with
      | att :: l ->
          begin match a, att with
          | MESSAGES, Mailbox.MESSAGES n -> n
          | RECENT, Mailbox.RECENT n -> n
          | UIDNEXT, Mailbox.UIDNEXT uid -> uid
          | UIDVALIDITY, Mailbox.UIDVALIDITY uid -> uid
          | UNSEEN, Mailbox.UNSEEN n -> n
          | HIGHESTMODSEQ, Mailbox.HIGHESTMODSEQ modseq -> modseq
          | _ -> loop l a
          end
      | [] ->
          raise Not_found
    in
    loop t a
end

module Response = struct
  type state =
    | OK of Code.code option * string
    | NO of Code.code option * string
    | BAD of Code.code option * string

  type untagged =
    | State of state
    | BYE of Code.code option * string
    | PREAUTH of Code.code option * string
    | FLAGS of Msg.flag list
    | LIST of Mailbox.mbx_flag list * char option * string
    | LSUB of Mailbox.mbx_flag list * char option * string
    | SEARCH of int32 list * int64 option
    | STATUS of string * Mailbox.mbx_att list
    | EXISTS of int
    | RECENT of int
    | EXPUNGE of int32
    | FETCH of int32 * Msg.msg_att list
    | CAPABILITY of Capability.capability list
    | VANISHED of Uint32Set.t
    | VANISHED_EARLIER of Uint32Set.t
    | ENABLED of Capability.capability list

  type response =
    | Untagged of untagged
    | Cont of string
    | Tagged of string * state

  open Format

  let pp_state ppf = function
    | OK (c, t) ->
        fprintf ppf "@[<2>(ok@ %a@ %S)@]" (pp_opt Code.pp) c t
    | NO (c, t) ->
        fprintf ppf "@[<2>(no@ %a@ %S)@]" (pp_opt Code.pp) c t
    | BAD (c, t) ->
        fprintf ppf "@[<2>(bad@ %a@ %S)@]" (pp_opt Code.pp) c t

  let pp_untagged ppf = function
    | State s ->
        pp_state ppf s
    | BYE (c, t) ->
        fprintf ppf "@[<2>(bye@ %a@ %S)@]" (pp_opt Code.pp) c t
    | FLAGS flags ->
        fprintf ppf "@[<2>(flags@ %a)@]" (pp_list Msg.pp_flag) flags
    | LIST (f, s, m) ->
        fprintf ppf "@[<2>(list@ (flags@ %a)@ %a@ %S)@]" (pp_list Mailbox.pp_mbx_flag) f (pp_opt pp_char) s m
    | LSUB (f, s, m) ->
        fprintf ppf "@[<2>(lsub@ (flags@ %a)@ %a@ %S)@]" (pp_list Mailbox.pp_mbx_flag) f (pp_opt pp_char) s m
    | SEARCH (ns, m) ->
        fprintf ppf "@[<2>(search@ %a@ %a)@]"
          (pp_list (fun ppf -> fprintf ppf "%lu")) ns (pp_opt (fun ppf -> fprintf ppf "%Lu")) m
    | STATUS (m, s) ->
        fprintf ppf "@[<2>(status@ %S@ %a)@]" m (pp_list Mailbox.pp_mbx_status) s
    | EXISTS n ->
        fprintf ppf "(exists %i)" n
    | RECENT n ->
        fprintf ppf "(recent %i)" n
    | EXPUNGE n ->
        fprintf ppf "(expunge %lu)" n
    | FETCH (n, atts) ->
        fprintf ppf "@[<2>(fetch %lu@ %a)@]" n (pp_list Msg.pp) atts
    | CAPABILITY r ->
        fprintf ppf "@[<2>(capability %a)@]" (pp_list Capability.pp) r
    | PREAUTH (c, t) ->
        fprintf ppf "@[<2>(preauth@ %a@ %S)@]" (pp_opt Code.pp) c t
    | VANISHED s ->
        fprintf ppf "@[<2>(vanished@ %a)@]" Msg.pp_set s
    | VANISHED_EARLIER s ->
        fprintf ppf "@[<2>(vanished-earlier@ %a)@]" Msg.pp_set s
    | ENABLED s ->
        fprintf ppf "@[<2>(enabled@ %a)@]" (pp_list Capability.pp) s

  let pp_response ppf = function
    | Untagged u ->
        pp_untagged ppf u
    | Cont s ->
        fprintf ppf "@[<2>(cont@ %S)@]" s
    | Tagged (t, s) ->
        fprintf ppf "@[<2>(tagged@ %S@ %a)@]" t pp_state s
end

module Parser = struct
  exception Stop

  type state =
    {
      i: string;
      p: int ref;
      c: string list;
    }

  type 'a t = state -> 'a

  let char c d =
    if !(d.p) >= String.length d.i || d.i.[!(d.p)] != c then raise Stop;
    d.p := !(d.p) + 1;
    c

  let sp f d =
    ignore (char ' ' d);
    f d

  let while1 f d =
    let i0 = !(d.p) in
    let i = ref i0 in
    while !i < String.length d.i && f d.i.[!i] do
      incr i
    done;
    d.p := !i;
    if i0 = !i then raise Stop;
    String.sub d.i i0 (!i - i0)

  let capture p d =
    let i0 = !(d.p) in
    ignore (p d);
    String.sub d.i i0 (!(d.p) - i0)

  let const c _ =
    c

  let empty =
    const ()

  let protect p d =
    let p0 = !(d.p) in
    try p d with e -> d.p := p0; raise e

  let list ?(sep = ' ') p d =
    let rec loop acc =
      match protect (char sep) d with
      | _ ->
          loop (p d :: acc)
      | exception Stop ->
          List.rev acc
    in
    match protect p d with
    | x ->
        loop [x]
    | exception Stop ->
        []

  let list1 ?(sep = ' ') p d =
    let rec loop acc =
      match protect (char sep) d with
      | _ ->
          loop (p d :: acc)
      | exception Stop ->
          List.rev acc
    in
    loop [p d]

  let rep p d =
    let rec loop acc =
      match protect p d with
      | x ->
          loop (x :: acc)
      | exception Stop ->
          List.rev acc
    in
    match protect p d with
    | x ->
        loop [x]
    | exception Stop ->
        []

  let rep1 p d =
    let rec loop acc =
      match protect p d with
      | x ->
          loop (x :: acc)
      | exception Stop ->
          List.rev acc
    in
    loop [p d]

  let accumulate p d =
    let b = Buffer.create 0 in
    let rec loop () =
      match protect p d with
      | c ->
          Buffer.add_char b c;
          loop ()
      | exception Stop ->
          Buffer.contents b
    in
    loop ()

  let delimited l p r d =
    ignore (char l d);
    let x = p d in
    ignore (char r d);
    x

  let char_pred f d =
    if !(d.p) >= String.length d.i || not (f d.i.[!(d.p)]) then raise Stop;
    let c = d.i.[!(d.p)] in
    d.p := !(d.p) + 1;
    c

  let chars_pred f d =
    while !(d.p) < String.length d.i && f d.i.[!(d.p)] do
      incr d.p
    done

  let preceded p1 p2 d =
    ignore (p1 d);
    p2 d

  let terminated p1 p2 d =
    let x = p1 d in
    ignore (p2 d);
    x

  let option p d =
    match protect p d with
    | x -> Some x
    | exception Stop -> None

  let (|||) p1 p2 d =
    match protect p1 d with
    | x -> x
    | exception Stop -> p2 d

  let str_length n d =
    if !(d.p) + n > String.length d.i then raise Stop;
    let s = String.sub d.i !(d.p) n in
    d.p := !(d.p) + n;
    s

  let str_ci s d =
    let len = String.length s in
    if !(d.p) + len > String.length d.i then raise Stop;
    for i = 0 to len - 1 do
      if Char.lowercase_ascii s.[i] != Char.lowercase_ascii d.i.[!(d.p) + i] then raise Stop
    done;
    d.p := !(d.p) + len

  let switch cases otherwise d =
    let rec loop = function
      | (s, case) :: cases ->
          begin match protect (str_ci s) d with
          | () ->
              (* Printf.eprintf "--> [%S]\n%!" s; *)
              case d
          | exception Stop ->
              loop cases
          end
      | [] ->
          otherwise d
    in
    loop cases

  let pair ?sep p1 p2 d =
    let sep =
      match sep with
      | None -> (fun d -> ignore (char ' ' d))
      | Some sep -> (fun d -> ignore (sep d))
    in
    let x = p1 d in
    sep d;
    let y = p2 d in
    (x, y)

  let stop _ =
    raise Stop

  let plist ?sep p =
    delimited '(' (list ?sep p) ')'

  let plist1 ?sep p =
    delimited '(' (list1 ?sep p) ')'

  let push s p d =
    (* Printf.eprintf "+ %d %d [%s]\n%!" (List.length d.c) !(d.p) s; *)
    p {d with c = s :: d.c}
    (* | x -> *)
    (*     Printf.eprintf "- [%s]\n%!" s; *)
    (*     x *)
    (* | exception e -> *)
    (*     Printf.eprintf "- [%s] FAIL\n%!" s; *)
    (*     raise e *)
end

module Decoder = struct
  open Response
  open Parser

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
    while1 is_atom_char

(*
   CR             =  %x0D
                                  ; carriage return

   LF             =  %x0A
                                  ; linefeed

   CRLF           =  CR LF
                          ; Internet standard newline
*)

  let crlf d =
    ignore (char '\r' d);
    ignore (char '\n' d)

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

  let number d =
    Scanf.sscanf (while1 is_digit d) "%lu" (fun n -> n)

  let number =
    push "number" number

  let nz_number d =
    let s = capture (preceded (char_pred is_nz_digit) (chars_pred is_digit)) d in
    Scanf.sscanf s "%lu" (fun n -> n)

  let nz_number =
    push "nz-number" nz_number

  let uniqueid =
    nz_number

(*
   quoted          = DQUOTE *QUOTED-CHAR DQUOTE

   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
                     '\\' quoted-specials
*)

  let quoted_char =
    char_pred (function '"' | '\\' -> false | '\x01'..'\x7f' -> true | _ -> false) |||
    preceded (char '\\') (char_pred (function '"' | '\\' -> true | _ -> false))

  let quoted_char =
    push "quoted-char" quoted_char

  let rec quoted =
    push "quoted" (delimited '"' (accumulate quoted_char) '"')

(*
   literal         = "{" number "}" CRLF *CHAR8
                       ; Number represents the number of CHAR8s

   string          = quoted / literal
*)

  let literal d =
    let n = Int32.to_int (delimited '{' number '}' d) in
    ignore (crlf d);
    str_length n d

  let literal =
    push "literal" literal

  let imap_string =
    push "string" (quoted ||| literal)

(*
   ASTRING-CHAR   = ATOM-CHAR / resp-specials

   astring         = 1*ASTRING-CHAR / string
*)

  let is_astring_char c =
    is_atom_char c || c = ']'

  let astring =
    while1 is_astring_char ||| imap_string

(*
   TEXT-CHAR       = <any CHAR except CR and LF>

   text            = 1*TEXT-CHAR
*)

  let is_text_char = function
    | '\r' | '\n' -> false
    | '\x01' .. '\x7F' -> true
    | _ -> false

  let text =
    while1 is_text_char ||| const "" (* allow empty texts for greater tolerance *)

(*
   nil             = "NIL"

   nstring         = string / nil
*)

  let nil c =
    push "nil" (switch [ "NIL", const c ] stop)

  let nstring =
    let imap_string d = Some (imap_string d) in
    nil None ||| imap_string

  let nstring' d =
    match nstring d with
    | Some s -> s
    | None -> ""

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

  let flag_keyword d =
    `Keyword (atom d)

  let flag_keyword =
    push "flag-keyword" flag_keyword

  let flag_extension d =
    `Extension (preceded (char '\\') atom d)

  let flag_extension =
    push "flag-extension" flag_extension

  let flag =
    let cases =
      [
        "\\Answered", const `Answered;
        "\\Flagged", const `Flagged;
        "\\Deleted", const `Deleted;
        "\\Seen", const `Seen;
        "\\Draft", const `Draft;
      ]
    in
    push "flag" (switch cases (flag_keyword ||| flag_extension))

(*
   flag-fetch      = flag / "\Recent"
*)

  let flag_fetch =
    push "flag-fetch" (switch [ "\\Recent", const `Recent ] flag)

(*
   flag-perm       = flag / "\*"
*)

  let flag_perm =
    push "flag-perm" (switch [ "\\*", const `All ] flag)

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
    push "uid-range" (pair ~sep:(char ':') uniqueid uniqueid)

  let uid_set =
    let uniqueid d = let n = uniqueid d in n, n in
    push "uid-set" (list1 ~sep:',' (uniqueid ||| uid_range))

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
    while1 is_text_other_char ||| const "" (* We allow empty text_1 *)

  let capability =
    let open Capability in
    let cases =
      [
        "COMPRESS=DEFLATE", const COMPRESS_DEFLATE;
        "CONDSTORE", const CONDSTORE;
        "ENABLE", const ENABLE;
        "IDLE", const IDLE;
        "LITERAL+", const LITERALPLUS;
        "LITERAL-", const LITERALMINUS;
        "UTF8=ACCEPT", const UTF8_ACCEPT;
        "UTF8=ONLY", const UTF8_ONLY;
        "NAMESPACE", const NAMESPACE;
        "ID", const ID;
        "QRESYNC", const QRESYNC;
        "UIDPLUS", const UIDPLUS;
        "UNSELECT", const UNSELECT;
        "XLIST", const XLIST;
        "AUTH=PLAIN", const AUTH_PLAIN;
        "AUTH=LOGIN", const AUTH_LOGIN;
        "XOAUTH2", const XOAUTH2;
        "X-GM-EXT-1", const X_GM_EXT_1;
      ]
    in
    let otherwise d = OTHER (atom d) in
    push "capability" (switch cases otherwise)

  let mod_sequence_value d =
    Modseq.of_string (capture (preceded (char_pred is_nz_digit) (while1 is_digit)) d)

  let mod_sequence_value =
    push "mod-sequence-value" mod_sequence_value

  let append_uid = uniqueid

  let resp_text_code =
    let open Code in
    let cases =
      [
        "ALERT", const ALERT;
        "BADCHARSET", (fun d -> BADCHARSET (rep (sp astring) d));
        "CAPABILITY", (fun d -> CAPABILITY (sp (list1 capability) d));
        "PARSE", const PARSE;
        "PERMANENTFLAGS",
        (fun d -> PERMANENTFLAGS (sp (plist flag_perm) d));
        "READ-ONLY", const READ_ONLY;
        "READ-WRITE", const READ_WRITE;
        "TRYCREATE", const TRYCREATE;
        "UIDNEXT", (fun d -> UIDNEXT (sp nz_number d));
        "UIDVALIDITY", (fun d -> UIDVALIDITY (sp nz_number d));
        "UNSEEN", (fun d -> UNSEEN (sp nz_number d));
        "CLOSED", const CLOSED;
        "HIGHESTMODSEQ", (fun d -> HIGHESTMODSEQ (sp mod_sequence_value d));
        "NOMODSEQ", const NOMODSEQ;
        "MODIFIED", (fun d -> MODIFIED (sp set d));
        "APPENDUID",
        (fun d ->
           let n = sp nz_number d in
           let uid = append_uid d in
           APPENDUID (n, uid)
        );
        "COPYUID",
        (fun d ->
           let n = sp nz_number d in
           let s1 = sp set d in
           let s2 = sp set d in
           COPYUID (n, s1, s2)
        );
        "UIDNOTSTICKY", const UIDNOTSTICKY;
        "COMPRESSIONACTIVE", const COMPRESSIONACTIVE;
        "USEATTR", const USEATTR;
      ]
    in
    let other d =
      let a = atom d in
      let text d = Some (sp text_1 d) in
      OTHER (a, (text ||| const None) d)
    in
    push "resp-text-code" (switch cases other)

(*
   resp-text       = ["[" resp-text-code "]" SP] text
*)

  let resp_text =
    push "resp-text"
      (pair ~sep:empty (option (terminated (delimited '[' resp_text_code ']') (char ' '))) text)

(*
   resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
                       ; Status condition
*)

  let resp_cond_state =
    let cases =
      [
        "OK", (fun d -> let c, t = sp resp_text d in OK (c, t));
        "NO", (fun d -> let c, t = sp resp_text d in NO (c, t));
        "BAD", (fun d -> let c, t = sp resp_text d in BAD (c, t));
      ]
    in
    push "resp-cond-state" (switch cases stop)

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
    let open Mailbox in
    let cases =
      [
        "\\Noselect", const Noselect;
        "\\Marked", const Marked;
        "\\Unmarked", const Unmarked;
        "\\Noinferiors", const Noinferiors;
        "\\HasChildren", const HasChildren;
        "\\HasNoChildren", const HasNoChildren;
        "\\All", const All;
        "\\Archive", const Archive;
        "\\Drafts", const Drafts;
        "\\Flagged", const Flagged;
        "\\Junk", const Junk;
        "\\Sent", const Sent;
        "\\Trash", const Trash;
      ]
    in
    let extension d = Extension (atom d) in
    push "mbx-flag" (switch cases extension)

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

  let mailbox d =
    let s = astring d in
    try Mutf7.decode s with _ -> s

  let mailbox =
    push "mailbox" mailbox

(*
   mailbox-list    = "(" [mbx-list-flags] ")" SP
                      (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
*)

  let delim =
    let quoted_char d = Some (quoted_char d) in
    delimited '"' quoted_char '"' ||| nil None

  let mailbox_list d =
    let x = plist mbx_flag d in
    let y = sp delim d in
    let m = sp mailbox d in
    x, y, m

  let mailbox_list =
    push "mailbox-list" mailbox_list

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

  let mod_sequence_valzer d =
    Modseq.of_string (while1 is_digit d)

  let status_att =
    let open Mailbox in
    let cases =
      [
        "MESSAGES", (fun d -> MESSAGES (Int32.to_int (sp number d)));
        "RECENT", (fun d -> RECENT (Int32.to_int (sp number d)));
        "UIDNEXT", (fun d -> UIDNEXT (sp number d));
        "UIDVALIDITY", (fun d -> UIDVALIDITY (sp number d));
        "UNSEEN", (fun d -> UNSEEN (sp number d));
        "HIGHESTMODSEQ", (fun d -> HIGHESTMODSEQ (mod_sequence_valzer d));
      ]
    in
    push "status-att" (switch cases stop)

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

  let address d =
    ignore (char '(' d);
    let ad_name = nstring' d in
    let ad_adl = sp nstring' d in
    let ad_mailbox = sp nstring' d in
    let ad_host = sp nstring' d in
    ignore (char ')' d);
    {Envelope.ad_name; ad_adl; ad_mailbox; ad_host}

  let address =
    push "address" address

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

  let envelope d =
    let address_list = delimited '(' (rep1 address) ')' ||| nil [] in
    ignore (char '(' d);
    let env_date = nstring' d in
    let env_subject = sp nstring' d in
    let env_from = sp address_list d in
    let env_sender = sp address_list d in
    let env_reply_to = sp address_list d in
    let env_to = sp address_list d in
    let env_cc = sp address_list d in
    let env_bcc = sp address_list d in
    let env_in_reply_to = sp nstring' d in
    let env_message_id = sp nstring' d in
    ignore (char ')' d);
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
    push "envelope" envelope

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
    push "body-fld-param" (plist1 (pair imap_string imap_string) ||| nil [])

  let body_fld_octets =
    push "body-fld-octets" number

  let body_fields d =
    let fld_params = body_fld_param d in
    let fld_id = sp nstring d in
    let fld_desc = sp nstring d in
    let fld_enc = sp imap_string d in
    let fld_octets = Int32.to_int (sp body_fld_octets d) in
    {MIME.fld_params; fld_id; fld_desc; fld_enc; fld_octets}

  let body_fields =
    push "body-fields" body_fields

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

  let rec body_extension d =
    let open MIME in
    let nstring d = String (nstring' d) in
    let number d = Number (number d) in
    let list d = List (plist1 body_extension d) in
    push "body-extension" (nstring ||| number ||| list) d

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
    let dsp d = Some (pair imap_string body_fld_param d) in
    push "body-fld-dsp" (delimited '(' dsp ')' ||| nil None)

  let body_fld_lang =
    let nstring d = [nstring' d] in
    push "body-fld-lang" (nstring ||| plist1 imap_string)

  let body_fld_loc =
    push "body-fld-loc" nstring

  let body_ext_gen d =
    let ext_dsp = option (sp body_fld_dsp) d in
    let ext_lang =
      match ext_dsp with
      | None -> None
      | Some _ -> option (sp body_fld_lang) d
    in
    let ext_loc =
      match ext_lang with
      | None -> None
      | Some _ -> option (sp body_fld_loc) d
    in
    let ext_ext =
      match ext_loc with
      | None -> []
      | Some _ -> rep (sp body_extension) d
    in
    let ext_dsp = match ext_dsp with None -> None | Some x -> x in
    let ext_lang = match ext_lang with None -> [] | Some l -> l in
    let ext_loc = match ext_loc with None -> None | Some s -> s in
    {MIME.ext_dsp; ext_lang; ext_loc; ext_ext}

  let body_ext_1part =
    push "body-ext-1part" (pair ~sep:empty nstring body_ext_gen) (* FIXME *)

  let body_ext_mpart =
    push "body-ext-mpart" (pair ~sep:empty body_fld_param body_ext_gen) (* FIXME *)

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
    let number d = Int32.to_int (number d) in
    push "body-fld-lines" number

  let media_subtype =
    push "media-subtype" imap_string

  let media_basic =
    push "media-basic" (pair imap_string media_subtype) (* FIXME *)

  let rec body_type_mpart d = (* TODO Return the extension data *)
    let aux d =
      let bodies = rep1 body d in
      let media_subtype = sp imap_string d in
      ignore (option (sp body_ext_mpart) d);
      MIME.Multipart (bodies, media_subtype)
    in
    push "body-type-mpart" aux d

  and body_type_basic media_type media_subtype d =
    let aux d =
      let body_fields = sp body_fields d in
      MIME.Basic (media_type, media_subtype, body_fields)
    in
    push "body-type-basic" aux d

  and body_type_msg d =
    let aux d =
      let body_fields = sp body_fields d in
      let envelope = sp envelope d in
      let body = sp body d in
      let body_fld_lines = sp body_fld_lines d in
      MIME.Message (body_fields, envelope, body, body_fld_lines)
    in
    push "body-type-msg" aux d

  and body_type_text media_subtype d =
    let aux d =
      let body_fields = sp body_fields d in
      let body_fld_lines = sp body_fld_lines d in
      MIME.Text (media_subtype, body_fields, body_fld_lines)
    in
    push "body-type-text" aux d

  and body_type_1part d = (* TODO Return the extension data *)
    let aux d =
      let media_type, media_subtype = media_basic d in
      let body =
        match media_type, media_subtype with
        | "MESSAGE", "RFC822" -> body_type_msg d
        | "TEXT", _ -> body_type_text media_subtype d
        | _ -> body_type_basic media_type media_subtype d
      in
      ignore (option (sp body_ext_1part) d);
      body
    in
    push "body-type-1part" aux d

  and body d =
    push "body" (delimited '(' (body_type_1part ||| body_type_mpart) ')') d

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

  let digit d =
    Char.code (char_pred is_digit d) - Char.code '0'

  let digits n d =
    let rec loop acc i =
      if i >= n then
        acc
      else
        loop (10 * acc + digit d) (i+1)
    in
    loop 0 0

  let date_day_fixed =
    push "date-day-fixed" (preceded (char ' ') digit ||| digits 2)

  let date_month =
    let months =
      [
        "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
        "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec";
      ]
    in
    push "date-month" (switch (List.mapi (fun i m -> m, const i) months) stop)

  let date_year =
    push "date-year" (digits 4)

  let time d =
    let hours = digits 2 d in
    ignore (char ':' d);
    let minutes = digits 2 d in
    ignore (char ':' d);
    let seconds = digits 2 d in
    (hours, minutes, seconds)

  let time =
    push "time" time

  let zone d =
    match char_pred (function '-' | '+' -> true | _ -> false) d with
    | '+' ->
        digits 4 d
    | '-' ->
        - (digits 4 d)
    | _ ->
        assert false

  let zone =
    push "zone" zone

  let date_time =
    let aux d =
      let day = date_day_fixed d in
      ignore (char '-' d);
      let month = date_month d in
      ignore (char '-' d);
      let year = date_year d in
      ignore (char ' ' d);
      let hours, minutes, seconds = time d in
      ignore (char ' ' d);
      let zone = zone d in
      {day; month; year}, {hours; minutes; seconds; zone}
    in
    push "date-time" (delimited '"' aux '"')

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
    push "header-fld-name" astring

  let header_list =
    push "header-list" (plist1 header_fld_name)

  let section_msgtext =
    let open Msg in
    let cases =
      [
        "HEADER.FIELDS.NOT", (fun d -> HEADER_FIELDS_NOT (sp header_list d));
        "HEADER.FIELDS", (fun d -> HEADER_FIELDS (sp header_list d));
        "HEADER", const HEADER;
        "TEXT", const TEXT;
      ]
    in
    push "section-msgtext" (switch cases stop)

  let section_text =
    push "section-text" (section_msgtext ||| switch [ "MIME", const Msg.MIME ] stop)

  let section_part =
    push "section-part" (list1 ~sep:'.' nz_number)

  let rec section_spec =
    let open Msg in
    let aux d =
      let l = section_part d in
      let p = (section_text ||| const All) d in
      List.fold_right (fun i x -> Part (Int32.to_int i, x)) l p
    in
    push "section-spec" (section_msgtext ||| aux)

  let section =
    push "section" (delimited '[' (section_spec ||| const Msg.All) ']')

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
    push "permsg-modsequence" mod_sequence_value

  let msg_att_dynamic =
    let open Msg in
    let cases =
      [
        "FLAGS", (fun d -> FLAGS (sp (plist flag_fetch) d));
        "MODSEQ", (fun d -> MODSEQ (sp permsg_modsequence d));
        "X-GM-LABELS", (fun d -> X_GM_LABELS (sp (plist astring ||| nil []) d));
      ]
    in
    push "msg-att-dynamic" (switch cases stop)

  let msg_att_static =
    let open Msg in
    let cases =
      [
        "ENVELOPE", (fun d -> ENVELOPE (sp envelope d));
        "INTERNALDATE", (fun d -> let t1, t2 = sp date_time d in INTERNALDATE (t1, t2));
        "RFC822.HEADER", (fun d -> RFC822_HEADER (sp nstring d));
        "RFC822.TEXT", (fun d -> RFC822_TEXT (sp nstring d));
        "RFC822.SIZE", (fun d -> RFC822_SIZE (Int32.to_int (sp number d)));
        "RFC822", (fun d -> RFC822 (sp nstring d));
        "BODYSTRUCTURE", (fun d -> BODYSTRUCTURE (sp body d));
        "BODY",
        (fun d ->
           let body d = BODY (body d) in
           let section d =
             let s = section d in
             let r =
               let number d = Int32.to_int (number d) in
               option (delimited '<' number '>') d
             in
             let x = sp nstring d in
             BODY_SECTION (s, r, x)
           in
           (sp body ||| section) d
        );
        "UID", (fun d -> UID (sp uniqueid d));
        "X-GM-MSGID", (fun d -> X_GM_MSGID (sp mod_sequence_value d));
        "X-GM-THRID", (fun d -> X_GM_THRID (sp mod_sequence_value d));
      ]
    in
    push "msg-att-static" (switch cases stop)

  let msg_att =
    push "msg-att" (plist1 (msg_att_static ||| msg_att_dynamic))

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
    push "flag-list" (plist flag)

  let status_att_list =
    push "status-att-list" (list status_att)

  let search_sort_mod_seq =
    push "search-sort-mod-seq"
      (delimited '(' (switch [ "MODSEQ", sp mod_sequence_value ] stop) ')')

  let mailbox_data =
    let cases =
      [
        "FLAGS", (fun d -> FLAGS (sp flag_list d));
        "LIST", (fun d -> let xs, c, m = sp mailbox_list d in LIST (xs, c, m));
        "LSUB", (fun d -> let xs, c, m = sp mailbox_list d in LSUB (xs, c, m));
        "SEARCH",
        (fun d ->
           let acc, n =
             pair ~sep:empty (rep (sp nz_number)) (option (sp search_sort_mod_seq)) d
           in
           SEARCH (acc, n)
        );
        "STATUS",
        (fun d ->
           let m = sp mailbox d in
           STATUS (m, sp (delimited '(' status_att_list ')') d)
        )
      ]
    in
    let otherwise d =
      let n = number d in
      let cases =
        [
          "EXISTS", const (EXISTS (Int32.to_int n));
          "RECENT", const (RECENT (Int32.to_int n));
        ]
      in
      sp (switch cases stop) d
    in
    push "mailbox-data" (switch cases otherwise)

  let capability_data =
    let cases =
      [
        "CAPABILITY", (fun d -> CAPABILITY (sp (list1 capability) d));
      ]
    in
    push "capability-data" (switch cases stop)

  let enable_data =
    let cases =
      [
        "ENABLED", (fun d -> ENABLED (rep (sp capability) d));
      ]
    in
    push "enable-data" (switch cases stop)

  let resp_cond_bye =
    let cases =
      [
        "BYE", (fun d -> let c, t = sp resp_text d in BYE (c, t));
      ]
    in
    push "resp-cond-bye" (switch cases stop)

  let known_ids =
    uid_set

  let expunged_resp =
    let cases =
      [
        "VANISHED (EARLIER)", (fun d -> VANISHED_EARLIER (known_ids d));
        "VANISHED", (fun d -> VANISHED (known_ids d));
      ]
    in
    push "expunged-resp" (switch cases stop)

  let message_data d =
    let n = nz_number d in
    let cases =
      [
        "EXPUNGE", const (EXPUNGE n);
        "FETCH", (fun d -> FETCH (n, sp msg_att d));
      ]
    in
    push "message-data" (sp (switch cases expunged_resp)) d

  let resp_cond_auth =
    let cases =
      [
        "PREAUTH", (fun d -> let c, t = sp resp_text d in PREAUTH (c, t));
      ]
    in
    push "resp-cond-auth" (switch cases stop)

  let response_data =
    let resp_cond_state d = State (resp_cond_state d) in
    let data =
      resp_cond_state ||| resp_cond_bye ||| mailbox_data |||
      message_data ||| capability_data ||| enable_data |||
      resp_cond_auth (* CHECK *)
    in
    preceded (char '*') (terminated (sp data) crlf)

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
    push "tag" (while1 is_tag_char)

  let response_tagged =
    let aux d =
      let tag, state = pair tag resp_cond_state d in
      Tagged (tag, state)
    in
    push "response-tagged" (terminated aux crlf)

  let continue_req =
    let resp_text d = Cont (snd (resp_text d)) in
    push "continue-req" (terminated (preceded (char '+') (sp (resp_text ||| resp_text))) crlf)
  (* space is optional CHECKME ! base64 *)

  let response =
    let response_data d = Untagged (response_data d) in
    push "response" (continue_req ||| response_data ||| response_tagged)

  let decode i =
    let d = {i; p = ref 0; c = []} in
    response d
end

module Readline = struct
  type step =
    | Reading
    | LF
    | Lit of int

  type state =
    {
      src: string;
      off: int;
      len: int;
      step: step;
      eof: bool;
      buf: string;
    }

  type 'a res =
    | Refill of state
    | Next of state * 'a
    | Error

  let string_of_step = function
    | Reading -> "Reading"
    | LF -> "LF"
    | Lit n -> Printf.sprintf "Lit %d" n

  let print_state {src; off; len; step; eof; buf} =
    Printf.eprintf "src=%S off=%d len=%d step=%s eof=%B buf=%S\n%!" src off len (string_of_step step) eof buf

  let empty =
    {
      src = "";
      off = 0;
      len = 0;
      step = Reading;
      eof = false;
      buf = "";
    }

  let next state =
    if false then print_state state;
    let {src; off; len; step; eof; buf} = state in
    let rec go step i off len buf =
      if i >= off + len then
        let buf = buf ^ String.sub src off len in
        if eof then
          Error
        else
          let state = {state with step; buf; src = ""; off = 0; len = 0} in
          Refill state
      else begin
        match step, src.[i] with
        | Reading, '\r'
        | LF, '\r' ->
            go LF (i+1) off len buf
        | Lit n, _ ->
            let m = min n len in
            go (if m < n then Lit (n-m) else Reading) (i+m) off len buf
        | LF, '\n' ->
            let buf = buf ^ String.sub src off (i-off+1) in
            let off = i+1 and len = len-i+off-1 in
            let ai = String.length buf + i - off in
            let complete () =
              let state = {state with off; len; buf = ""; step = Reading} in
              Next (state, Decoder.decode buf)
            in
            let module S = struct type t = Start | Acc end in
            let open S in
            let rec scan step acc i =
              if i < 0 then
                complete ()
              else begin
                match step, buf.[i] with
                | Start, '}' ->
                    scan Acc false (i - 1)
                | Acc, '0' .. '9' ->
                    scan Acc true (i - 1)
                | Acc, '{' when acc ->
                    let n =
                      let acc = ref 0 in
                      for j = i+1 to ai-3 do
                        acc := 10 * !acc + (Char.code buf.[j] - Char.code '0')
                      done;
                      !acc
                    in
                    go (Lit n) (ai+1) off len buf
                | Start, _ | Acc, _ ->
                    complete ()
              end
            in
            scan Start false (ai-2)
        | Reading, _ | LF, _ ->
            go Reading (i+1) off len buf
      end
    in
    go step off off len buf

  let feed state src off len =
    if false then Printf.eprintf "feed: src=%S off=%d len=%d\n%!" src off len;
    let src = if state.src <> "" then state.src ^ src else src in
    let off = if state.src <> "" then state.off + off else off in
    let len = if state.src <> "" then state.len + len else len in
    {state with src; off; len; eof = len = 0}
end

(* Commands *)

module Search = struct
  open E

  type key = rope

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

module Auth = struct
  type step_fun =
    string -> [ `Ok of string * step_fun | `Error of string ]

  type authenticator =
    {
      name: string;
      step: step_fun;
    }

  let fail_fun _ =
    `Error "should have worked already!"

  let plain user pass =
    let step _ = `Ok (Printf.sprintf "\000%s\000%s" user pass, fail_fun) in
    {name = "PLAIN"; step}

  let xoauth2 user token =
    let s = Printf.sprintf "user=%s\001auth=Bearer %s\001\001" user token in
    let step _ = `Ok ("", fail_fun) in (* CHECK *)
    let step _ = `Ok (s, step) in
    {name = "XOAUTH2"; step}
end

(* Running commands *)

open Response

module Error = struct
  type error =
    | Incorrect_tag of string * string
    | Decode_error of string * int
    | Unexpected_cont
    | Bad_greeting
    | Auth_error of string

  open Format

  let pp ppf = function
    | Incorrect_tag (exp, tag) -> fprintf ppf "@[Incorrect@ tag@ %S,@ should@ be@ %S@]" tag exp
    | Decode_error (s, i) -> fprintf ppf "@[Decode@ error:@ %S %i@]" s i
    | Unexpected_cont -> fprintf ppf "@[Unexpected continuation request@]"
    | Bad_greeting -> fprintf ppf "@[Bad greeting@]"
    | Auth_error s -> fprintf ppf "@[Authentication error: %s@]" s
end

type 'a command =
  {
    format: E.rope;
    default: 'a;
    process: untagged -> 'a -> 'a;
  }

let login username password =
  let format = E.(str "LOGIN" ++ str username ++ str password) in
  let default = () in
  let process _ () = () in
  {format; default; process}

let capability =
  let format = E.(str "CAPABILITY") in
  let default = [] in
  let process u caps =
    match u with
    | CAPABILITY caps1 -> caps @ caps1
    | _ -> caps
  in
  {format; default; process}

let create m =
  let format = E.(str "CREATE" ++ mailbox m) in
  let default = () in
  let process _ () = () in
  {format; default; process}

let delete m =
  let format = E.(str "DELETE" ++ mailbox m) in
  let default = () in
  let process _ () = () in
  {format; default; process}

let rename m1 m2 =
  let format = E.(str "RENAME" ++ mailbox m1 ++ mailbox m2) in
  let default = () in
  let process _ () = () in
  {format; default; process}

let logout =
  let format = E.(str "LOGOUT") in
  let default = () in
  let process _ () = () in
  {format; default; process}

let noop =
  let format = E.(str "NOOP") in
  let default = () in
  let process _ () = () in
  {format; default; process}

let subscribe m =
  let format = E.(str "SUBSCRIBE" ++ mailbox m) in
  let default = () in
  let process _ () = () in
  {format; default; process}

let unsubscribe m =
  let format = E.(str "UNSUBSCRIBE" ++ mailbox m) in
  let default = () in
  let process _ () = () in
  {format; default; process}

let list ?(ref = "") s =
  let format = E.(str "LIST" ++ mailbox ref ++ str s) in
  let default = [] in
  let process u res =
    match u with
    | LIST (flags, delim, mbox) -> res @ [flags, delim, mbox] (* CHECK *)
    | _ -> res
  in
  {format; default; process}

let lsub ?(ref = "") s =
  let format = E.(str "LSUB" ++ mailbox ref ++ str s) in
  let default = [] in
  let process u res =
    match u with
    | LSUB (flags, delim, mbox) -> res @ [flags, delim, mbox]
    | _ -> res
  in
  {format; default; process}

let status m att =
  let format = E.(str "STATUS" ++ mailbox m ++ p (list (fun x -> x) att)) in
  let default = [] in
  let process u att =
    match u with
    | STATUS (mbox, items) when m = mbox ->
        items
    | _ ->
        att
  in
  {format; default; process}

let copy_gen cmd s m =
  let format = E.(cmd ++ eset s ++ mailbox m) in
  let default = () in
  let process _ () = () in
  {format; default; process}

let copy s m =
  copy_gen E.(raw "COPY") s m

let uid_copy s m =
  copy_gen E.(raw "UID" ++ raw "COPY") s m

let check =
  let format = E.(str "CHECK") in
  let default = () in
  let process _ () = () in
  {format; default; process}

let close =
  let format = E.(str "CLOSE") in
  let default = () in
  let process _ () = () in
  {format; default; process}

let expunge =
  let format = E.(str "EXPUNGE") in
  let default = [] in
  let process u info =
    match u with
    | EXPUNGE n -> n :: info
    | _ -> info
  in
  {format; default; process}

let search_gen cmd sk =
  let format = E.(cmd ++ sk) in
  let default = ([], None) in
  let process u (res, m) =
    match u with
    | SEARCH (ids, m1) -> ids @ res, m1
    | _ -> (res, m)
  in
  {format; default; process}

let search =
  search_gen E.(raw "SEARCH")

let uid_search =
  search_gen E.(raw "UID" ++ raw "SEARCH")

let select_gen cmd m =
  let open E in
  let format = cmd ++ mailbox m in
  let default = () in
  let process _ () = () in
  {format; default; process}

let condstore_select_gen cmd m =
  let open E in
  let format = cmd ++ mailbox m ++ p (raw "CONDSTORE") in
  let default = Int64.zero in
  let process u m =
    match u with
    | State (OK (Some (Code.HIGHESTMODSEQ m), _)) -> m
    | _ -> m
  in
  {format; default; process}

let select =
  select_gen E.(raw "SELECT")

let examine =
  select_gen E.(raw "EXAMINE")

let condstore_select =
  condstore_select_gen E.(raw "SELECT")

let condstore_examine =
  condstore_select_gen E.(raw "EXAMINE")

let append m ?(flags = []) data =
  let format = E.(raw "APPEND" ++ mailbox m ++ p (list Msg.flag flags) ++ literal data) in
  let default = () in
  let process _ () = () in
  {format; default; process}

let fetch_gen cmd ?changed ?(vanished = false) set att =
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
    match changed, vanished with
    | None, true -> invalid_arg "fetch_gen"
    | None, false -> empty
    | Some m, false -> p (raw "CHANGEDSINCE" ++ uint64 m)
    | Some m, true -> p (raw "CHANGEDSINCE" ++ uint64 m ++ raw "VANISHED")
  in
  let format = cmd ++ eset set ++ att ++ changed_since in
  let default = [] in
  let process u atts =
    match u with
    | FETCH (id, infos) ->
        (id, infos) :: atts
    | _ ->
        atts
  in
  {format; default; process}

let fetch ?changed ?vanished set att =
  fetch_gen E.(raw "FETCH") ?changed ?vanished set att

let uid_fetch ?changed ?vanished set att =
  fetch_gen E.(raw "UID" ++ raw "FETCH") ?changed ?vanished set att

let store_gen cmd ~silent ~unchanged mode set att =
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
    | `Flags flags -> list Msg.flag flags
    | `Labels labels -> list label labels
  in
  let unchanged_since =
    match unchanged with
    | None -> str ""
    | Some m -> p (raw "UNCHANGEDSINCE" ++ uint64 m)
  in
  let format = cmd ++ eset set ++ unchanged_since ++ raw base ++ p att in
  let default = [] in
  let process u m = m in
  {format; default; process}

let add_flags ?(silent = false) ?unchanged set flags =
  store_gen E.(raw "STORE") ~silent ~unchanged `Add set (`Flags flags)

let set_flags ?(silent = false) ?unchanged set flags =
  store_gen E.(raw "STORE") ~silent ~unchanged `Set set (`Flags flags)

let remove_flags ?(silent = false) ?unchanged set flags =
  store_gen E.(raw "STORE") ~silent ~unchanged `Remove set (`Flags flags)

let add_labels ?(silent = false) ?unchanged set labels =
  store_gen E.(raw "STORE") ~silent ~unchanged `Add set (`Labels labels)

let set_labels ?(silent = false) ?unchanged set labels =
  store_gen E.(raw "STORE") ~silent ~unchanged `Set set (`Labels labels)

let remove_labels ?(silent = false) ?unchanged set labels =
  store_gen E.(raw "STORE") ~silent ~unchanged `Remove set (`Labels labels)

let uid_add_flags ?(silent = false) ?unchanged set flags =
  store_gen E.(raw "UID" ++ raw "STORE") ~silent ~unchanged `Add set (`Flags flags)

let uid_set_flags ?(silent = false) ?unchanged set flags =
  store_gen E.(raw "UID" ++ raw "STORE") ~silent ~unchanged `Set set (`Flags flags)

let uid_remove_flags ?(silent = false) ?unchanged set flags =
  store_gen E.(raw "UID" ++ raw "STORE") ~silent ~unchanged `Remove set (`Flags flags)

let uid_add_labels ?(silent = false) ?unchanged set labels =
  store_gen E.(raw "UID" ++ raw "STORE") ~silent ~unchanged `Add set (`Labels labels)

let uid_set_labels ?(silent = false) ?unchanged set labels =
  store_gen E.(raw "UID" ++ raw "STORE") ~silent ~unchanged `Set set (`Labels labels)

let uid_remove_labels ?(silent = false) ?unchanged set labels =
  store_gen E.(raw "UID" ++ raw "STORE") ~silent ~unchanged `Remove set (`Labels labels)

let enable caps =
  let format = E.(str "ENABLE" ++ list capability caps) in
  let default = [] in
  let process u caps =
    match u with
    | ENABLED caps1 -> caps1 @ caps
    | _ -> caps
  in
  {format; default; process}

type session =
  {
    tag: int;
    buf: Readline.state;
  }

let initial_session =
  {
    tag = 1;
    buf = Readline.empty;
  }

type 'a state =
  | Sending : E.rope * 'a state -> 'a state
  | WaitForResp : (Response.response -> 'a transition) -> 'a state

and 'a transition =
  | Done of 'a
  | Next of 'a state

type 'a progress =
  session * 'a state

type 'a action =
  | Ok of 'a * session
  | No of string * session
  | Bad of string * session
  | Error of Error.error
  | Send of string * 'a progress
  | Refill of 'a progress

let tag session =
  Printf.sprintf "%04d" session.tag

let rec wait_for_auth tag step = function
  | Cont data ->
      begin match step (B64.decode data) with
      | `Ok (data, step) ->
          let data = B64.encode ~pad:true data in
          Next (Sending (E.raw data, WaitForResp (wait_for_auth tag step)))
      | `Error _ ->
          Next (Sending (E.raw "*", WaitForResp (wait_for_auth tag step)))
      end
  | Untagged _ ->
      Next (WaitForResp (wait_for_auth tag step))
  | Tagged (tag1, _) ->
      assert (tag = tag1);
      Done ()

let wait_for_greeting auth = function
  | Untagged (State (OK _)) ->
      Next (Sending (E.(raw "AUTH" ++ raw "AUTHENTICATE" ++ raw auth.Auth.name), WaitForResp (wait_for_auth "AUTH" auth.Auth.step)))
  | _ ->
      assert false

let wait_for_cont k = function
  | Cont _ ->
      Next k
  | _ ->
      assert false

let rec wait_for_resp tag init f = function
  | Cont _ ->
      assert false
  | Untagged r ->
      Next (WaitForResp (wait_for_resp tag (f r init) f))
  | Tagged (tag1, _) ->
      assert (tag = tag1);
      Done init

let initiate a =
  Refill (initial_session, WaitForResp (wait_for_greeting a))

let continue (session, state) =
  let rec loop buf = function
    | Done x ->
        Ok (x, {session with buf})
    | Next (Sending (format, k)) ->
        begin match E.out format with
        | E.Done s ->
            Send (s, ({session with buf}, k))
        | E.WaitForCont (s, format) ->
            Send (s, ({session with buf}, WaitForResp (wait_for_cont k)))
        end
    | Next (WaitForResp k as state) ->
        begin match Readline.next buf with
        | Readline.Next (buf, r) ->
            loop buf (k r)
        | Readline.Refill buf ->
            Refill ({session with buf}, state)
        | Readline.Error ->
            assert false
        end
  in
  loop session.buf (Next state)

let feed (session, state) s off len =
  {session with buf = Readline.feed session.buf s off len}, state

let run session cmd =
  let tag = tag session in
  let format = E.(raw tag ++ cmd.format) in
  let state = Sending (format, WaitForResp (wait_for_resp tag cmd.default cmd.process)) in
  continue (session, state)

let rec wait_for_idle tag = function
  | Cont _ ->
      Next (WaitForResp (wait_for_idle tag))
  | Untagged _ ->
      Next (Sending (E.(raw "DONE"), WaitForResp (wait_for_idle tag)))
  | Tagged (tag1, _) ->
      assert (tag = tag1);
      Done ()

let idle session =
  let tag = tag session in
  let format = E.(raw tag ++ raw "IDLE") in
  let state = Sending (format, WaitForResp (wait_for_idle tag)) in
  continue (session, state)
