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

let io_buffer_size = 65536                           (* IO_BUFFER_SIZE 4.0.0 *)

module Uint32 = struct
  include Uint32
  let printer ppf n = Format.pp_print_string ppf (to_string n)
end

module Uint64 = struct
  include Uint64
  let printer ppf n = Format.pp_print_string ppf (to_string n)
end

module Modseq = Uint64
module UID = Uint32
module Seq = Uint32

type _ uid_or_seq =
  | UID : UID.t uid_or_seq
  | Seq : Seq.t uid_or_seq

type _ condstore_flag =
  | Condstore : Uint64.t condstore_flag
  | No_condstore : unit condstore_flag

type uint32 = Uint32.t
type uint64 = Uint64.t

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

(* type month = *)
(*   [ `Jan | `Feb | `Mar | `Apr | `May | `Jun *)
(*   | `Jul | `Aug | `Sep | `Oct | `Nov | `Dec ] *)

type date = { day : int; month : int ; year : int }
type time = { hours : int; minutes : int; seconds : int; zone : int }

type flag =
  [ `Answered
  | `Flagged
  | `Deleted
  | `Seen
  | `Draft
  | `Keyword of string
  | `Extension of string ]

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

  let pp ppf = function
    | ACL ->
        fprintf ppf "acl"
    | BINARY ->
        fprintf ppf "binary"
    | CATENATE ->
        fprintf ppf "catenate"
    | CHILDREN ->
        fprintf ppf "children"
    | COMPRESS_DEFLATE ->
        fprintf ppf "compress-deflate"
    | CONDSTORE ->
        fprintf ppf "condstore"
    | ENABLE -> fprintf ppf "enable"
    | IDLE -> fprintf ppf "idle"
    | ID -> fprintf ppf "id"
    | LITERALPLUS ->
        fprintf ppf "literal+"
    | MULTIAPPEND ->
        fprintf ppf "multi-append"
    | NAMESPACE ->
        fprintf ppf "namespace"
    | QRESYNC ->
        fprintf ppf "qresync"
    | QUOTE ->
        fprintf ppf "quote"
    | SORT ->
        fprintf ppf "sort"
    | STARTTLS ->
        fprintf ppf "start-tls"
    | UIDPLUS ->
        fprintf ppf "uid-plus"
    | UNSELECT ->
        fprintf ppf "unselect"
    | XLIST ->
        fprintf ppf "xlist"
    | AUTH_ANONYMOUS ->
        fprintf ppf "auth-anonymous"
    | AUTH_LOGIN ->
        fprintf ppf "auth-login"
    | AUTH_PLAIN ->
        fprintf ppf "auth-plain"
    | XOAUTH2 ->
        fprintf ppf "xoauth2"
    | X_GM_EXT_1 ->
        fprintf ppf "gmail"
    | OTHER s ->
        fprintf ppf "(other %S)" s
end

module Response = struct
  type set =
    (uint32 * uint32) list

  type address =
    {
      ad_name : string;
      ad_adl : string;
      ad_mailbox : string;
      ad_host : string;
    }

  type envelope =
    {
      env_date : string;
      env_subject : string;
      env_from : address list;
      env_sender : address list;
      env_reply_to : address list;
      env_to : address list;
      env_cc : address list;
      env_bcc : address list;
      env_in_reply_to : string;
      env_message_id : string;
    }

  type fields =
    {
      fld_params : (string * string) list;
      fld_id : string option;
      fld_desc : string option;
      fld_enc : string;
      fld_octets : int;
    }

  type body_extension =
    [ `List of body_extension list
    | `Number of uint32
    | `String of string
    | `None ]

  type part_extension =
    {
      ext_dsp : (string * (string * string) list) option;
      ext_lang : string list;
      ext_loc : string option;
      ext_ext : body_extension list;
    }

  (* type mime_sext = *)
  (*   string option * part_extension *)

  (* type mime_mext = *)
  (*   (string * string) list * part_extension *)

  type mime =
    [ `Text of string * fields * int
    | `Message of fields * envelope * mime * int
    | `Basic of string * string * fields
    | `Multipart of mime list * string ]

  type section =
    [ `Header
    | `Header_fields of string list
    | `Header_fields_not of string list
    | `Text
    | `Mime
    | `Part of int * section
    | `All ]

  type fetch_response =
    | FLAGS of [ flag | `Recent ] list
    | ENVELOPE of envelope
    | INTERNALDATE of date * time
    | RFC822 of string option
    | RFC822_HEADER of string option
    | RFC822_TEXT of string option
    | RFC822_SIZE of int
    | BODY of mime
    | BODYSTRUCTURE of mime
    | BODY_SECTION of section * int option * string option
    | UID of uint32
    | MODSEQ of uint64
    | Gm_msgid of uint64
    | Gm_thrid of uint64
    | Gm_labels of string list

  module Code = struct
    type code =
      | ALERT
      | BADCHARSET of string list
      | CAPABILITY of Capability.capability list
      | PARSE
      | PERMANENTFLAGS of [ flag | `All ] list
      | READ_ONLY
      | READ_WRITE
      | TRYCREATE
      | UIDNEXT of uint32
      | UIDVALIDITY of uint32
      | UNSEEN of uint32
      | OTHER of string * string option
      | CLOSED
      | HIGHESTMODSEQ of uint64
      | NOMODSEQ
      | MODIFIED of (uint32 * uint32) list
      | APPENDUID of uint32 * uint32
      | COPYUID of uint32 * (uint32 * uint32) list * (uint32 * uint32) list
      | UIDNOTSTICKY
      | COMPRESSIONACTIVE
      | USEATTR

    open Format

    let pp ppf = function
      | ALERT ->
          fprintf ppf "alert"
      | BADCHARSET cs ->
          fprintf ppf "@[<2>(badcharset %a)@]"
            (pp_print_list ~pp_sep:pp_print_space pp_print_string) cs
      | CAPABILITY caps ->
          fprintf ppf "@[<2>(capability %a)@]"
            (pp_print_list ~pp_sep:pp_print_space Capability.pp) caps
      | PARSE ->
          fprintf ppf "parse"
      | PERMANENTFLAGS fl ->
          fprintf ppf "@[<2>(permanent-flags %a)@]"
            (pp_print_list ~pp_sep:pp_print_space pp_flag_perm) fl
      | READ_ONLY ->
          fprintf ppf "read-only"
      | READ_WRITE ->
          fprintf ppf "read-write"
      | TRYCREATE ->
          fprintf ppf "try-create"
      | UIDNEXT uid ->
          fprintf ppf "(uid-next %a)" Uint32.printer uid
      | UIDVALIDITY uid ->
          fprintf ppf "(uid-validity %a)" Uint32.printer uid
      | UNSEEN n ->
          fprintf ppf "(unseen %a)" Uint32.printer n
      | OTHER (k, v) ->
          fprintf ppf "(other@ %S@ %a)" k (pp_opt pp_qstr) v
      | CLOSED ->
          fprintf ppf "closed"
      | HIGHESTMODSEQ m ->
          fprintf ppf "(highest-modseq %a)" Uint64.printer m
      | NOMODSEQ ->
          fprintf ppf "no-modseq"
      | MODIFIED s ->
          fprintf ppf "(modified@ %a)" pp_set s
      | APPENDUID (n, m) ->
          fprintf ppf "(append-uid %a@ %a)" Uint32.printer n Uint32.printer m
      | COPYUID (n, s1, s2) ->
          fprintf ppf "(copy-uid %a@ %a@ %a)" Uint32.printer n pp_set s1 pp_set s2
      | UIDNOTSTICKY ->
          fprintf ppf "uid-not-sticky"
      | COMPRESSIONACTIVE ->
          fprintf ppf "compression-active"
      | USEATTR ->
          fprintf ppf "use-attr"
  end

  type mbx_flag =
    [ `Noselect
    | `Marked
    | `Unmarked
    | `Noinferiors
    | `HasChildren
    | `HasNoChildren
    | `All
    | `Archive
    | `Drafts
    | `Flagged
    | `Junk
    | `Sent
    | `Trash
    | `Extension of string ]

  type status_response =
    | MESSAGES of int
    | RECENT of int
    | UIDNEXT of uint32
    | UIDVALIDITY of uint32
    | UNSEEN of uint32
    | HIGHESTMODSEQ of uint64

  type state =
    | OK of Code.code option * string
    | NO of Code.code option * string
    | BAD of Code.code option * string

  type untagged =
    | State of state
    | Bye of Code.code * string
    | Preauth of Code.code * string
    | Flags of flag list
    | List of mbx_flag list * char option * string
    | Lsub of mbx_flag list * char option * string
    | Search of uint32 list * uint64 option
    | Status of string * status_response list
    | Exists of int
    | Recent of int
    | Expunge of uint32
    | Fetch of uint32 * fetch_response list
    | Capability of Capability.capability list
    | Vanished of set
    | Vanished_earlier of set
    | Enabled of Capability.capability list

  type response =
    | Untagged of untagged
    | Cont of string
    | Tagged of string * state

  let pp = Format.fprintf

  let pp_list f ppf = function
    | [] -> pp ppf "nil"
    | x :: xs ->
        pp ppf "@[%a" f x;
        List.iter (fun x -> pp ppf "@ %a" f x) xs;
        pp ppf "@]"

  let pp_string ppf s = pp ppf "%s" s
  let pp_qstr ppf s = pp ppf "%S" s
  let pp_char ppf c = pp ppf "%C" c

  let pp_flag ppf = function
    | `Answered    -> pp ppf "answered"
    | `Flagged     -> pp ppf "flagged"
    | `Deleted     -> pp ppf "deleted"
    | `Seen        -> pp ppf "seen"
    | `Draft       -> pp ppf "draft"
    | `Keyword k   -> pp ppf "(keyword %S)" k
    | `Extension k -> pp ppf "(extension %S)" k

  let pp_flag_perm ppf = function
    | `All       -> pp ppf "all"
    | #flag as f -> pp_flag ppf f

  let pp_flag_fetch ppf = function
    | `Recent    -> pp ppf "recent"
    | #flag as f -> pp_flag ppf f

  let pp_opt f ppf = function
    | None   -> pp ppf "nil"
    | Some c -> pp ppf "%a" f c

  let pp_address ppf x =
    pp ppf "@[<hv 2>(address@ (name %S)@ (addr %S)@ (mailbox %S)@ (host %S))@]"
      x.ad_name x.ad_adl x.ad_mailbox x.ad_host

  let pp_envelope ppf env =
    pp ppf "@[<hv 2>(envelope@ (date %S)@ (subject %S)@ \
            (from %a)@ (sender %a)@ (reply-to %a)@ \
            (to %a)@ (cc %a)@ (bcc %a)@ (in-reply-to %S)@ \
            (message-id %S))@]"
      env.env_date
      env.env_subject
      (pp_list pp_address) env.env_from
      (pp_list pp_address) env.env_sender
      (pp_list pp_address) env.env_reply_to
      (pp_list pp_address) env.env_to
      (pp_list pp_address) env.env_cc
      (pp_list pp_address) env.env_bcc
      env.env_in_reply_to
      env.env_message_id

  let pp_param ppf (k, v) =
    pp ppf "(%S@ %S)" k v

  let pp_fields ppf f =
    pp ppf "@[<hv 2>(fields@ @[<hv 2>(params@ %a)@]@ (id %a)@ (desc %a)@ (enc %S)@ (octets %d)@]"
      (pp_list pp_param) f.fld_params (pp_opt pp_string) f.fld_id (pp_opt pp_string) f.fld_desc f.fld_enc
      f.fld_octets

  let rec pp_mime ppf = function
    | `Text (m, f, i) ->
        pp ppf "@[<2>(text@ %S@ %a@ %d)@]" m pp_fields f i
    | `Message (f, e, b, i) ->
        pp ppf "@[<2>(message@ %a@ %a@ %a@ %d)@]" pp_fields f pp_envelope e pp_mime b i
    | `Basic (m, t, f) ->
        pp ppf "@[<2>(basic@ %S@ %S@ %a)@]" m t pp_fields f
    | `Multipart (b, m) ->
        pp ppf "@[<2>(multipart@ %a@ %S)@]" (pp_list pp_mime) b m

  let rec pp_section ppf : [< section] -> _ = function
    | `Header -> pp ppf "header"
    | `Header_fields l -> pp ppf "@[<2>(header-fields %a)@]" (pp_list pp_qstr) l
    | `Header_fields_not l -> pp ppf "@[<2>(header-fields-not %a)@]" (pp_list pp_qstr) l
    | `Text -> pp ppf "text"
    | `Mime -> pp ppf "mime"
    | `Part (n, s) -> pp ppf "@[<2>(part %d@ %a)@]" n pp_section s
    | `All -> pp ppf "all"

  let pp_date_time ppf (d, t) =
    pp ppf "@[(date %02d %02d %04d)@ (time %02d %02d %02d %04d)@]"
      d.day d.month d.year t.hours t.minutes t.seconds t.zone

  let pp_msg_att ppf = function
    | FLAGS r          -> pp ppf "@[<2>(flags %a)@]" (pp_list pp_flag_fetch) r
    | ENVELOPE e       -> pp_envelope ppf e
    | INTERNALDATE (d, t) -> pp ppf "@[<2>(internal-date@ %a)@]" pp_date_time (d, t)
    | RFC822 s         -> pp ppf "(rfc822 %a)" (pp_opt pp_qstr) s
    | RFC822_HEADER s  -> pp ppf "(rfc822-header %a)" (pp_opt pp_qstr) s
    | RFC822_TEXT s    -> pp ppf "(rfc822-text %a)" (pp_opt pp_qstr) s
    | RFC822_SIZE n    -> pp ppf "(rfc822-size %i)" n
    | BODY b           -> pp ppf "@[<2>(body@ %a)@]" pp_mime b
    | BODYSTRUCTURE b -> pp ppf "@[<2>(bodystructure@ %a)@]" pp_mime b
    | BODY_SECTION (s, n, x) -> pp ppf "@[<2>(body-section@ %a@ %a@ %a)@]"
                                   pp_section s (pp_opt Format.pp_print_int) n (pp_opt pp_qstr) x
    | UID n            -> pp ppf "(uid %a)" Uint32.printer n
    | MODSEQ m         -> pp ppf "(modseq %a)" Uint64.printer m
    | Gm_msgid m       -> pp ppf "(gm-msgid %a)" Uint64.printer m
    | Gm_thrid m       -> pp ppf "(gm-thrid %a)" Uint64.printer m
    | Gm_labels l      -> pp ppf "@[<2>(gm-labels@ %a)@]" (pp_list pp_qstr) l

  let pp_fetch_response = pp_msg_att

  let pp_set ppf s =
    let rg ppf (x, y) = pp ppf "%a-%a" Uint32.printer x Uint32.printer y in
    pp_list rg ppf s

  let pp_option f ppf = function
    | Some x -> f ppf x
    | None -> Format.pp_print_string ppf "none"

  let pp_state ppf = function
    | OK (c, t)  -> pp ppf "@[<2>(ok@ %a@ %S)@]" (pp_option Code.pp) c t
    | NO (c, t)  -> pp ppf "@[<2>(no@ %a@ %S)@]" (pp_option Code.pp) c t
    | BAD (c, t) -> pp ppf "@[<2>(bad@ %a@ %S)@]" (pp_option Code.pp) c t

  let pp_mbx_flag : _ -> [< mbx_flag] -> _ = fun ppf f ->
    match f with
    | `Noselect      -> pp ppf "noselect"
    | `Marked        -> pp ppf "marked"
    | `Unmarked      -> pp ppf "unmarked"
    | `Noinferiors   -> pp ppf "noinferiors"
    | `HasChildren   -> pp ppf "has-children"
    | `HasNoChildren -> pp ppf "has-no-children"
    | `All           -> pp ppf "all"
    | `Archive       -> pp ppf "archive"
    | `Drafts        -> pp ppf "drafts"
    | `Flagged       -> pp ppf "flagged"
    | `Junk          -> pp ppf "junk"
    | `Sent          -> pp ppf "sent"
    | `Trash         -> pp ppf "trash"
    | `Extension s   -> pp ppf "(extension %s)" s

  let pp_mbx_status ppf = function
    | MESSAGES n       -> pp ppf "(messages %i)" n
    | RECENT n         -> pp ppf "(recent %i)" n
    | UIDNEXT uid     -> pp ppf "(uid-next %a)" Uint32.printer uid
    | UIDVALIDITY uid -> pp ppf "(uid-validity %a)" Uint32.printer uid
    | UNSEEN n         -> pp ppf "(unseen %a)" Uint32.printer n
    | HIGHESTMODSEQ m -> pp ppf "(highest-modseq %a)" Uint64.printer m

  let pp_untagged ppf = function
    | State s            -> pp_state ppf s
    | Bye (c, t)         -> pp ppf "@[<2>(bye@ %a@ %S)@]" Code.pp c t
    | Flags flags        -> pp ppf "@[<2>(flags@ %a)@]" (pp_list pp_flag) flags
    | List (f, s, m)     -> pp ppf "@[<2>(list@ (flags@ %a)@ %a@ %S)@]" (pp_list pp_mbx_flag) f
                              (pp_opt pp_char) s m
    | Lsub (f, s, m)     -> pp ppf "@[<2>(lsub@ (flags@ %a)@ %a@ %S)@]" (pp_list pp_mbx_flag) f
                              (pp_opt pp_char) s m
    | Search (ns, m)     -> pp ppf "@[<2>(search@ %a@ %a)@]" (pp_list Uint32.printer) ns (pp_opt Uint64.printer) m
    | Status (m, s)      -> pp ppf "@[<2>(status@ %S@ %a)@]" m (pp_list pp_mbx_status) s
    | Exists n           -> pp ppf "(exists %i)" n
    | Recent n           -> pp ppf "(recent %i)" n
    | Expunge n          -> pp ppf "(expunge %a)" Uint32.printer n
    | Fetch (n, atts)    -> pp ppf "@[<2>(fetch %a@ %a)@]" Uint32.printer n (pp_list pp_msg_att) atts
    | Capability r       -> pp ppf "@[<2>(capability %a)@]" (pp_list Capability.pp) r
    | Preauth (c, t)     -> pp ppf "@[<2>(preauth@ %a@ %S)@]" Code.pp c t
    | Vanished s         -> pp ppf "@[<2>(vanished@ %a)@]" pp_set s
    | Vanished_earlier s -> pp ppf "@[<2>(vanished-earlier@ %a)@]" pp_set s
    | Enabled s          -> pp ppf "@[<2>(enabled@ %a)@]" (pp_list Capability.pp) s

  let pp_response ppf = function
    | Untagged u         -> pp_untagged ppf u
    | Cont s             -> pp ppf "@[<2>(cont@ %S)@]" s
    | Tagged (t, s)      -> pp ppf "@[<2>(tagged@ %S@ %a)@]" t pp_state s
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
              Next (state, buf)
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
    next {state with src; off; len; eof = len = 0}
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

  let rep f d =
    let i0 = !(d.p) in
    let i = ref i0 in
    while !i < String.length d.i && f d.i.[!i] do
      incr i
    done;
    d.p := !i;
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

  let list ?sep p d =
    let sep =
      match sep with
      | None -> (fun d -> ignore (char ' ' d))
      | Some sep -> (fun d -> ignore (sep d))
    in
    let rec loop acc =
      match protect sep d with
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

  let list1 ?sep p d =
    let sep =
      match sep with
      | None -> (fun d -> ignore (char ' ' d))
      | Some sep -> (fun d -> ignore (sep d))
    in
    let rec loop acc =
      match protect sep d with
      | _ ->
          loop (p d :: acc)
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
      if s.[i] != d.i.[!(d.p) + i] then raise Stop
    done;
    d.p := !(d.p) + len

  let switch cases otherwise d =
    let rec loop = function
      | (s, case) :: cases ->
          begin match protect (str_ci s) d with
          | () ->
              case d
          | exception Stop ->
              loop cases
          end
      | [] ->
          otherwise d
    in
    loop cases

  let pair ?sep p1 p2 d =
    let sep = match sep with None -> empty | Some sep -> (fun d -> ignore (sep d)) in
    let x = p1 d in
    ignore (sep d);
    let y = p2 d in
    (x, y)

  let stop _ = raise Stop

  let push s p d =
    p {d with c = s :: d.c}
end

module Decoder = struct
  open Response
  open Parser

  type error =
    [ `Expected_char of char
    | `Expected_string of string
    | `Unexpected_char of char
    | `Unexpected_string of string
    | `Illegal_char of char
    | `Unexpected_eoi ]

  let pp_error ppf (e, _, _) = match e with
    | `Expected_char c -> pp ppf "@[Expected@ character@ %C@]" c
    | `Expected_string s   -> pp ppf "@[Expected@ string@ %S@]" s
    | `Unexpected_char c   -> pp ppf "@[Unexpected@ character@ %C@]" c
    | `Unexpected_string s -> pp ppf "@[Unexpected@ string@ %S@]" s
    | `Illegal_char c      -> pp ppf "@[Illegal@ character@ %C@]" c
    | `Unexpected_eoi      -> pp ppf "@[Unexpected end of input@]"

  let pp_decode ppf = function
    | `Ok r -> pp ppf "@[<2>`Ok@ %a@]" pp_response r
    | `Read _ -> pp ppf "`Read"
    | `Error e -> pp ppf "@[`Error %a@]" pp_error e

  type result =
    [ `Ok of response
    | `Read of bytes * int * int * (int -> result)
    | `Error of error * string * int ]

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
    Uint32.of_string (while1 is_digit d)

  let nz_number d =
    Uint32.of_string (capture (preceded (char_pred is_nz_digit) (rep is_digit)) d)

  let uniqueid =
    nz_number

  (* let uint d = int_of_string (while1 is_digit d) *)
  (* let uint32 d = Uint32.of_string (while1 is_digit d) *)
  (* let uint64 d = Uint64.of_string (while1 is_digit d) *)

(*
   quoted          = DQUOTE *QUOTED-CHAR DQUOTE

   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
                     '\\' quoted-specials
*)

  let rec quoted =
    let rec aux =
      char_pred (function '\x01'..'\x7f' -> true | _ -> false) |||
      preceded (char '\\') (char_pred (function '"' | '\\' -> true | _ -> false))
   in
    delimited '"' (accumulate aux) '"'

(*
   literal         = "{" number "}" CRLF *CHAR8
                       ; Number represents the number of CHAR8s

   string          = quoted / literal
*)

  let literal d =
    let n = Uint32.to_int (delimited '{' number '}' d) in
    ignore (crlf d);
    str_length n d

  let imap_string =
    quoted ||| literal

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

  let nstring =
    switch [ "NIL", const None ] (fun d -> Some (imap_string d))

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

  let uid_set d =
    let uniqueid d = let n = uniqueid d in n, n in
    push "uid-set" (list1 ~sep:(char ',') (uniqueid ||| uid_range))

  (* We never parse '*' since it does not seem to show up in responses *)
  let sequence_set =
    uid_set

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
    let other d = `Other (atom d) in
    push "capability" (switch cases other)

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
        "BADCHARSET",
        (fun d ->
           BADCHARSET (switch [ " ", list1 astring ] (const []) d)
        );
        "CAPABILITY", (fun d -> CAPABILITY (sp (list1 capability) d));
        "PARSE", const PARSE;
        "PERMANENTFLAGS",
        (fun d -> PERMANENTFLAGS (sp (delimited '(' (list flag_perm) ')') d));
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
      (pair (option (terminated (delimited '[' resp_text_code ']') (char ' '))) text)

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
    let cases =
      [
        "Noselect", const `Noselect;
        "Marked", const `Marked;
        "Unmarked", const `Unmarked;
        "Noinferiors", const `Noinferiors;
        "HasChildren", const `HasChildren;
        "HasNoChildren", const `HasNoChildren;
        "All", const `All;
        "Archive", const `Archive;
        "Drafts", const `Drafts;
        "Flagged", const `Flagged;
        "Junk", const `Junk;
        "Sent", const `Sent;
        "Trash", const `Trash;
      ]
    in
    let extension d = `Extension (atom d) in
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
   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
                     '\\' quoted-specials

   mailbox-list    = "(" [mbx-list-flags] ")" SP
                      (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
*)

  let quoted_char d =
    let is_qchar = function
      | '\r' | '\n' | '\\' | '"' -> false
      | '\x01' .. '\x7F' -> true
      | _ -> false
    in
    match cur d with
    | '\\' ->
        junkc d;
        begin match cur d with
        | '\\' | '"' as c ->
            junkc d; c
        | c ->
            raise (Error (err_unexpected c d))
        end
    | c when is_qchar c ->
        junkc d;
        c
    | c ->
        raise (Error (err_unexpected c d))

  let delim =
    let quoted_char d = Some (quoted_char d) in
    delimited '"' quoted_char '"' ||| (switch [ "NIL" , const None ] stop)

  let mailbox_list d =
    let x = delimited '(' (list mbx_flag) ')' d in
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

   status-att-val      =/ "HIGHESTMODSEQ" SP mod-sequence-valzer
                          ;; extends non-terminal defined in [IMAPABNF].
                          ;; Value 0 denotes that the mailbox doesn't
                          ;; support persistent mod-sequences
                          ;; as described in Section 3.1.2
*)

  let mod_sequence_valzer d =
    Modseq.of_string (while1 is_digit d)

  let status_att =
    let cases =
      [
        "MESSAGES", (fun d -> MESSAGES (Uint32.to_int (sp number d)));
        "RECENT", (fun d -> RECENT (Uint32.to_int (sp number d)));
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
    {ad_name; ad_adl; ad_mailbox; ad_host}

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
    let address_list = delimited '(' (list ~sep:empty address) ')' in
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

  let p_fld_param d =
    let param d = let v = p_string d in p_sp d; let v' = p_string d in (v, v') in
    p_list param d

  let p_body_fields d =
    let params = p_fld_param d in
    p_sp d;
    let id = p_nstring d in
    p_sp d;
    let desc = p_nstring d in
    p_sp d;
    let enc = p_string d in
    p_sp d;
    let octets = p_uint d in
    {fld_params = params; fld_id = id; fld_desc = desc; fld_enc = enc; fld_octets = octets}

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
    if is_digit (cur d) then
      let n = p_uint32 d in
      `Number n
    else if cur d = '(' then
      `List (p_list1 p_body_ext d)
    else
      match p_nstring d with
      | Some x -> `String x
      | None -> `None

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

  let p_fld_dsp d =
    match cur d with
    | '(' ->
        junkc d;
        let s = p_string d in
        p_sp d;
        let p = p_fld_param d in
        p_ch ')' d;
        Some (s, p)
    | _ ->
        p_atomf "NIL" d;
        None

  let p_fld_lang d =
    match cur d with
    | '(' ->
        p_list1 p_string d
    | _ ->
        begin match p_nstring d with Some x -> [x] | None -> [] end

  let r_body_ext d =
    match cur d with
    | ' ' ->
        junkc d;
        let dsp = p_fld_dsp d in
        begin match cur d with
        | ' ' ->
            junkc d;
            let lang = p_fld_lang d in
            begin match cur d with
            | ' ' ->
                junkc d;
                let loc = p_nstring d in
                let ext = p_sep p_body_ext d in
                {ext_dsp = dsp; ext_lang = lang; ext_loc = loc; ext_ext = ext}
            | _ ->
                {ext_dsp = dsp; ext_lang = lang; ext_loc = None; ext_ext = []}
            end
        | _ ->
            {ext_dsp = dsp; ext_lang = []; ext_loc = None; ext_ext = []}
        end
    | _ ->
        {ext_dsp = None; ext_lang = []; ext_loc = None; ext_ext = []}

  let r_sbody_ext d =
    let md5 = p_nstring d in
    md5, r_body_ext d

  let r_mbody_ext d =
    let params = p_fld_param d in
    params, r_body_ext d

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

  let rec body_type_mpart d = (* TODO Return the extension data *)
    let rec loop acc =
      match cur d with
      | '(' ->
          let x = p_body d in
          loop (x :: acc)
      | ' ' ->
          junkc d;
          let m = p_string d in
          begin match cur d with
          | ' ' ->
              junkc d;
              let _ = r_mbody_ext d in
              `Multipart (List.rev acc, m)
          | _ ->
              `Multipart (List.rev acc, m)
          end
      | c ->
          failwith "unexpected" (* err_unexpected c d *)
    in
    loop []

  and body_type_1part d = (* TODO Return the extension data *)
    let ext x d =
      match peekc d with
      | Some ' ' ->
          junkc d;
          let _ = r_sbody_ext d in
          x
      | _ ->
          x
    in
    let m = p_string d in
    let t = p_string d in
    let f = p_body_fields d in
    match String.uppercase m, String.uppercase t with
    | "MESSAGE", "RFC822" ->
        p_sp d;
        let e = p_envelope d in
        let b = p_body d in
        let n = p_uint d in
        ext (`Message (f, e, b, n)) d
    | "TEXT", _ ->
        p_sp d;
        let n = p_uint d in
        ext (`Text (t, f, n)) d
    | _ ->
        ext (`Basic (m, t, f)) d

  and body =
    push "body" (delimited '(' (body_type_1part ||| body_type_mpart) ')')

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
      let time = time d in
      ignore (char ' ' d);
      let zone = zone d in
      (day, month, year, time, zone)
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

  let p_msgtext d =
    let a = p_atom d in
    match String.uppercase a with
    | "HEADER" -> `Header
    | "HEADER.FIELDS" ->
        p_sp d;
        `Header_fields (p_list1 p_astring d)
    | "HEADER.FIELDS.NOT" ->
        p_sp d;
        `Header_fields_not (p_list1 p_astring d)
    | "TEXT" ->
        `Text
    |  "MIME" ->
        `Mime
    | _ ->
        failwith "unexpected" (* err_unexpected_s a d *)

  let rec p_part d =
    if is_digit (cur d) then begin
      let n = p_uint d in
      p_ch '.' d;
      let s = p_part d in
      `Part (n, s)
    end else
      p_msgtext d

  let p_section d =
    p_ch '[' d;
    match cur d with
    | ']' ->
        junkc d;
        `All
    | _ ->
        let s = p_part d in
        p_ch ']' d;
        s

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
    let cases =
      [
        "FLAGS", (fun d -> FLAGS (sp (delimited '(' (list flag_fetch) ')') d));
        "MODSEQ", (fun d -> MODSEQ (sp permsg_modsequence d));
        "X-GM-LABELS",
        (fun d -> X_GM_LABELS (sp (delimited '(' (list astring) ')') d));
      ]
    in
    push "msg-att-dynamic" (switch cases stop)

  let msg_att_static =
    let cases =
      [
        "ENVELOPE", (fun d -> ENVELOPE (sp envelope d));
        "INTERNALDATE",
        (fun d -> let t1, t2 = sp date_time d in INTERNALDATE (t1, t2));
        "RFC822.HEADER", (fun d -> RFC822_HEADER (sp nstring d));
        "RFC822.TEXT", (fun d -> RFC822_TEXT (sp nstring d));
        "RFC822.SIZE", (fun d -> RFC822_SIZE (Uint32.to_int (sp number d)));
        "RFC822", (fun d -> RFC822 (sp nstring d));
        "BODYSTRUCTURE", (fun d -> BODYSTRUCTURE (sp body d));
        "BODY",
        begin match cur d with
        | ' ' ->
            junkc d;
            `Body (p_body d)
        | _ ->
            let s = p_section d in
            let orig =
              match cur d with
              | '<' ->
                  junkc d;
                  let n = p_uint d in
                  p_ch '>' d;
                  Some n
              | _ ->
                  None
            in
            p_sp d;
            let x = p_nstring d in
            `Body_section (s, orig, x)
        end;
     "UID", (fun d -> UID (sp uniqueid d));
     "X-GM-MSGID", (fun d -> X_GM_MSGID (sp mod_sequence_value d));
     "X-GM-THRID", (fun d -> X_GM_THRID (sp mod_sequence_value d));
      ]
    in
    push "msg-att-static" (switch cases stop)

  let msg_att =
    push "msg-att" (delimited '(' (list1 (msg_att_static ||| msg_att_dynamic)) ')')

(*
   mailbox-data    =  "FLAGS" SP flag-list / "LIST" SP mailbox-list /
                      "LSUB" SP mailbox-list / "SEARCH" *(SP nz-number) /
                      "STATUS" SP mailbox SP "(" [status-att-list] ")" /
                      number SP "EXISTS" / number SP "RECENT"

   message-data    = nz-number SP ("EXPUNGE" / ("FETCH" SP msg-att))

   resp-cond-bye   = "BYE" SP resp-text

   response-data   = "*" SP (resp-cond-state / resp-cond-bye /
                     mailbox-data / message-data / capability-data) CRLF

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
    push "flag-list" (delimited '(' (list flag) ')')

  let mailbox_data =
    let cases =
      [
        "FLAGS", FLAGS (sp flag_list d);
        "LIST" -> p_sp d; let xs, c, m = p_mailbox_list d in List (xs, c, m);
        "LSUB" -> p_sp d; let xs, c, m = p_mailbox_list d in Lsub (xs, c, m);
        "SEARCH" ->
        let rec nxt acc d =
          match peekc d with
          | Some ' ' ->
              junkc d;
              begin match peekc d with
              | Some '(' ->
                  junkc d;
                  p_atomf "MODSEQ" d;
                  p_sp d;
                  let n = p_uint64 d in
                  p_ch ')' d;
                  Search (List.rev acc, Some n)
              | _ ->
                  let n = p_uint32 d in
                  nxt (n :: acc) d
              end
          | _ ->
              Search (List.rev acc, None)
        in
        nxt [] d;
        "STATUS" ->
        p_sp d;
        let m = p_mailbox d in
        p_sp d;
        Status (m, p_list p_status_att d)
      ]
    in
    let otherwise d =
      | "EXISTS" -> Exists (Uint32.to_int n)
      | "RECENT" -> Recent (Uint32.to_int n)
    in
    push "mailbox-data" (switch cases otherwise)

  let capability_data =
    let cases =
      [
        "CAPABILITY", (fun d -> CAPABILITY (sp (list1 capability d)));
      ]
    in
    push "capability-data" (switch cases stop)

  let enable_data =
    let cases =
      [
        "ENABLED" -> let xs = p_sep p_cap d in Enabled xs;
      ]
    in
    push "enable-data" (switch cases stop)

  let resp_cond_state =
    let cases =
      [
        "OK" -> p_sp d; let c, t = p_resp_text d in State (Ok (c, t));
        "NO" -> p_sp d; let c, t = p_resp_text d in State (No (c, t));
        "BAD" -> p_sp d; let c, t = p_resp_text d in State (Bad (c, t));
      ]
    in
    push "resp-cond-state" (switch cases stop)

  let resp_cond_bye =
    let cases =
      [
        "BYE", (fun d -> let c, t = sp resp_text d in BYE (c, t));
      ]
    in
    push "resp-cond-bye" (switch cases stop)

  let message_data d =
    let n = p_uint32 d in (* FIXME uint vs uint32 *)
    p_sp d;
    let a = p_atom d in
    match String.uppercase a with
    | "EXPUNGE" -> Expunge n
    | "FETCH" -> p_sp d; Fetch (n, p_list1 p_msg_att d)
    | "VANISHED" ->
        p_sp d;
        begin match cur d with
        | '(' ->
            junkc d;
            p_atomf "EARLIER" d;
            p_ch ')' d;
            p_sp d;
            let s = p_set d in
            Vanished_earlier s
        | _ ->
            let s = p_set d in Vanished s
        end
    | _ -> failwith "unexpected" (* err_unexpected_s a d *)

  let response_data =
    resp_cond_state ||| resp_cond_bye ||| mailbox_data ||| message_data ||| capability_data ||| enable_data

    | "PREAUTH" -> p_sp d; let c, t = p_resp_text d in Preauth (c, t)
    | _ -> failwith "unexpected" (* err_unexpected_s a d *)

  let p_untagged d = (* '*' was eaten *)
    p_sp d;
    if is_digit (cur d) then
      p_response_data_n d
    else
      p_response_data_t d

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

  let p_tag d =
    p_while1 is_tag_char d

  let p_tagged d =
    let tag = p_tag d in
    p_sp d;
    let x = p_resp_cond_state d in
    tag, x

  let p_continue_req d = (* '+' was eaten *)
    match cur d with
    | ' ' ->
        junkc d;
        p_text d
    | _ ->
        p_text d

  let rec p_response d =
    let x =
      match cur d with
      | '+' ->
          junkc d;
          Cont (p_continue_req d)
      | '*' ->
          junkc d;
          Untagged (p_untagged d)
      | _ ->
          let c, x = p_tagged d in
          Tagged (c, x)
    in
    p_crlf d;
    x

  let decode i =
    p_response {i; i_pos = 0; i_max = String.length i}
end

(* Commands *)

type eset = (uint32 * uint32 option) list

type search_key =
  [ `Seq of eset
  | `All
  | `Answered
  | `Bcc of string
  | `Before of date
  | `Body of string
  | `Cc of string
  | `Deleted
  | `Draft
  | `Flagged
  | `From of string
  | `Header of string * string
  | `Keyword of string
  | `Larger of int
  | `New
  | `Not of search_key
  | `Old
  | `On of date
  | `Or of search_key * search_key
  | `Recent
  | `Seen
  | `Sent_before of date
  | `Sent_on of date
  | `Sent_since of date
  | `Since of date
  | `Smaller of int
  | `Subject of string
  | `Text of string
  | `To of string
  | `Uid of eset
  | `Unanswered
  | `Undeleted
  | `Undraft
  | `Unflagged
  | `Unkeyword of string
  | `Unseen
  | `And of search_key * search_key
  | `Modseq of uint64
  | `Gm_raw of string
  | `Gm_msgid of uint64
  | `Gm_thrid of uint64
  | `Gm_labels of string list ]

type fetch_query =
  [ `Envelope
  | `Internal_date
  | `Rfc822_header
  | `Rfc822_text
  | `Rfc822_size
  | `Rfc822
  | `Body
  | `Body_section of [ `Peek | `Look ] * Response.section * (int * int) option
  | `Body_structure
  | `Uid
  | `Flags ]

type status_query =
  [ `Messages
  | `Recent
  | `Uid_next
  | `Uid_validity
  | `Unseen
  | `Highest_modseq ]

(* Authenticator *)

type authenticator =
  { name : string;
    step : string -> [ `Ok of string | `Error of string ] }

let plain user pass =
  let step _ = `Ok (Printf.sprintf "\000%s\000%s" user pass) in
  { name = "PLAIN"; step }

let xoauth2 user token =
  let s = Printf.sprintf "user=%s\001auth=Bearer %s\001\001" user token in
  let stage = ref `Begin in
  let step _ = match !stage with
    | `Begin -> stage := `Error; `Ok s
    | `Error -> `Ok "" (* CHECK *)
  in
  { name = "XOAUTH2"; step }

(* type command_ordinary = *)
(*   [ `Login of string * string *)
(*   | `Capability *)
(*   | `Create of string *)
(*   | `Delete of string *)
(*   | `Rename of string * string *)
(*   | `Logout *)
(*   | `Noop *)
(*   | `Subscribe of string *)
(*   | `Unsubscribe of string *)
(*   | `List of string * string *)
(*   | `Lsub of string * string *)
(*   | `Status of string * status_query list *)
(*   | `Copy of [ `Uid | `Seq ] * eset * string *)
(*   | `Check *)
(*   | `Close *)
(*   | `Expunge *)
(*   | `Search of [ `Uid | `Seq ] * search_key *)
(*   | `Select of [ `Condstore | `Plain ] * string *)
(*   | `Examine of [ `Condstore | `Plain ] * string *)
(*   | `Append of string * flag list option * (date * time) option * string *)
(*   | `Fetch of [ `Uid | `Seq ] * eset * *)
(*               [ `All | `Fast | `Full | `List of fetch_query list ] * *)
(*               [ `Changed_since of uint64 | `Changed_since_vanished of uint64 | `All ] *)
(*   | `Store of [ `Uid | `Seq ] * eset * [ `Silent | `Loud ] * *)
(*               [ `Unchanged_since of uint64 | `All ] * *)
(*               [ `Add | `Set | `Remove ] * [ `Flags of flag list | `Labels of string list ] *)
(*   | `Enable of capability list ] *)

module E = struct

  (* (\* Encoder *\) *)

  (* type command_lexeme = *)
  (*   [ command_ordinary *)
  (*   | `Idle *)
  (*   | `Authenticate of string ] *)

  (* type encode = *)
  (*   [ `Cmd of string * command_lexeme | `Idle_done | `Auth_step of string | `Auth_error ] *)

  (* let pp_encode : _ -> [< encode] -> _ = fun ppf e -> match e with *)
  (*   | `Cmd _ -> pp ppf "`Cmd" (\* FIXME *\) *)
  (*   | `Idle_done -> pp ppf "`Idle_done" *)
  (*   | `Auth_step _ -> pp ppf "`Auth_step" *)
  (*   | `Auth_error -> pp ppf "`Auth_error" *)

  (* type encoder = *)
  (*   { *)
  (*     mutable o : string; *)
  (*     mutable o_pos : int; *)
  (*   } *)

  (* let flush k e = *)
  (*   if e.o_pos > 0 then *)
  (*     let rec k1 n = *)
  (*       if n < e.o_pos then *)
  (*         `Partial (e.o, n, e.o_pos - n, fun m -> k1 (n + m)) *)
  (*       else *)
  (*         (e.o_pos <- 0; k e) *)
  (*     in *)
  (*     k1 0 *)
  (*   else *)
  (*     k e *)

  (* let wait_for_cont k e = *)
  (*   `Wait_for_cont k *)

  (* let rec writes s k e = *)
  (*   let o_len = String.length e.o in *)
  (*   let rec loop j l e = *)
  (*     let rem = o_len - e.o_pos in *)
  (*     let len = if l > rem then rem else l in *)
  (*     String.unsafe_blit s j e.o e.o_pos len; *)
  (*     e.o_pos <- e.o_pos + len; *)
  (*     if len < l then flush (loop (j + len) (l - len)) e else k e *)
  (*   in *)
  (*   loop 0 (String.length s) e *)

  (* let w s k e = writes s k e *)

  (* let classify_string str = *)
  (*   let literal = function *)
  (*     | '\x80' .. '\xFF' | '\r' | '\n' -> true *)
  (*     | _ -> false *)
  (*   in *)
  (*   let quotes = function *)
  (*     | '(' | ')' | '{' | ' ' | '\x00' .. '\x1F' | '\x7F' *)
  (*     | '%' | '*' | '\"' | '\\' -> true *)
  (*     | _ -> false *)
  (*   in *)
  (*   let needs f s = *)
  (*     let rec loop i = *)
  (*       if i >= String.length s then false else *)
  (*       if f s.[i] then true else *)
  (*         loop (i+1) *)
  (*     in *)
  (*     loop 0 *)
  (*   in *)
  (*   if str = "" then `Quoted else *)
  (*   if needs literal str then `Literal else *)
  (*   if needs quotes str then `Quoted else *)
  (*     `Raw *)

  (* let w_literal x k e = *)
  (*   w (Printf.sprintf "{%d}\r\n" (String.length x)) (flush (wait_for_cont (w x k))) e *)

  (* let w_string x k e = match classify_string x with *)
  (*   | `Raw     -> w x k e *)
  (*   | `Quoted  -> w "\"" (w x (w "\"" k)) e *)
  (*   | `Literal -> w_literal x k e *)

  (* let w_sep ?(sep = ' ') f l k e = *)
  (*   let rec loop xs e = match xs with *)
  (*     | [] -> k e *)
  (*     | x :: [] -> f x k e *)
  (*     | x :: xs -> f x (w (String.make 1 sep) (loop xs)) e *)
  (*   in *)
  (*   loop l e *)

  (* let (&) x y k e = x (w " " (y k)) e *)
  (* let ($) x y k e = x (y k) e *)

  (* let w_crlf k e = *)
  (*   w "\r\n" k e *)

  (* let w_status_att = function *)
  (*   | `Messages -> w "MESSAGES" *)
  (*   | `Recent -> w "RECENT" *)
  (*   | `Uid_next -> w "UIDNEXT" *)
  (*   | `Uid_validity -> w "UIDVALIDITY" *)
  (*   | `Unseen -> w "UNSEEN" *)
  (*   | `Highest_modseq -> w "HIGHESTMODSEQ" *)

  (* let w_int n = w (string_of_int n) *)
  (* let w_uint32 m = w (Uint32.to_string m) *)
  (* let w_uint64 m = w (Uint64.to_string m) *)

  (* let w_label l = *)
  (*   w_string (Mutf7.encode l) *)

  (* let w_list f l k e = *)
  (*   let rec loop xs e = match xs with *)
  (*     | [] -> w ")" k e *)
  (*     | [x] -> f x (w ")" k) e *)
  (*     | x :: xs -> f x (w " " (loop xs)) e *)
  (*   in *)
  (*   w "(" (loop l) e *)

  (* let w_eset s k e = *)
  (*   let f = function *)
  (*     | (lo, Some hi) when lo = hi -> w_uint32 lo *)
  (*     | (lo, Some hi) -> w_uint32 lo $ w ":" $ w_uint32 hi *)
  (*     | (lo, None) -> w_uint32 lo $ w ":*" *)
  (*   in *)
  (*   w_sep ~sep:',' f s k e *)

  (* let w_mailbox s = *)
  (*   w_string (Mutf7.encode s) *)

  (* let months = *)
  (*   [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; *)
  (*      "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |] *)

  (* let w_date d = *)
  (*   w (Printf.sprintf "%d-%s-%4d" d.day months.(d.month) d.year) *)

  (* let w_p f x = w "(" $ f x $ w ")" *)

  (* let rec w_search_key : [< search_key] -> _ = function *)
  (*   | `Seq s -> w_eset s *)
  (*   | `All -> w "ALL" *)
  (*   | `Answered -> w "ANSWERED" *)
  (*   | `Bcc s -> w "BCC" & w_string s *)
  (*   | `Before d -> w "BEFORE" & w_date d *)
  (*   | `Body s -> w "BODY" & w_string s *)
  (*   | `Cc s -> w "CC" & w_string s *)
  (*   | `Deleted -> w "DELETED" *)
  (*   | `Draft -> w "DRAFT" *)
  (*   | `Flagged -> w "FLAGGED" *)
  (*   | `From s -> w "FROM" & w_string s *)
  (*   | `Header (s1, s2) -> w "HEADER" & w_string s1 & w_string s2 *)
  (*   | `Keyword s -> w "KEYWORD" & w_string s *)
  (*   | `Larger n -> w "LARGER" & w_int n *)
  (*   | `New -> w "NEW" *)
  (*   | `Old -> w "OLD" *)
  (*   | `On d -> w "ON" & w_date d *)
  (*   | `Recent -> w "RECENT" *)
  (*   | `Seen -> w "SEEN" *)
  (*   | `Sent_before d -> w "SENTBEFORE" & w_date d *)
  (*   | `Sent_on d -> w "SENT_ON" & w_date d *)
  (*   | `Sent_since d -> w "SENTSINCE" & w_date d *)
  (*   | `Since d -> w "SINCE" & w_date d *)
  (*   | `Smaller n -> w "SMALLER" & w_int n *)
  (*   | `Subject s -> w "SUBJECT" & w_string s *)
  (*   | `Text s -> w "TEXT" & w_string s *)
  (*   | `To s -> w "TO" & w_string s *)
  (*   | `Uid s -> w "UID" & w_eset s *)
  (*   | `Unanswered -> w "UNANSWERED" *)
  (*   | `Undeleted -> w "UNDELETED" *)
  (*   | `Undraft -> w "UNDRAFT" *)
  (*   | `Unflagged -> w "UNFLAGGED" *)
  (*   | `Unkeyword s -> w "UNKEYWORD" & w_string s *)
  (*   | `Unseen -> w "UNSEEN" *)
  (*   | `Not sk -> w "NOT" & w_p w_search_key sk *)
  (*   | `Or (sk1, sk2) -> w "OR" & w_p w_search_key sk1 & w_p w_search_key sk2 *)
  (*   | `And (sk1, sk2) -> w_p w_search_key sk1 & w_p w_search_key sk2 *)
  (*   | `Modseq m -> w "MODSEQ" & w_uint64 m *)
  (*   | `Gm_raw s -> w "X-GM-RAW" & w_string s *)
  (*   | `Gm_msgid m -> w "X-GM-MSGID" & w_uint64 m *)
  (*   | `Gm_thrid m -> w "X-GM-THRID" & w_uint64 m *)
  (*   | `Gm_labels l -> w "X-GM-LABELS" & w_list w_string l *)

  (* let rec w_section : [< section] -> _ = function *)
  (*   | `Header -> w "HEADER" *)
  (*   | `Header_fields l -> w "HEADER.FIELDS" & w_list w l *)
  (*   | `Header_fields_not l -> w "HEADER.FIELDS.NOT" & w_list w l *)
  (*   | `Text -> w "TEXT" *)
  (*   | `Mime -> w "MIME" *)
  (*   | `Part (n, s) -> w_int n $ w "." $ w_section s *)
  (*   | `All -> w "" *)

  (* let w_fetch_att : [< fetch_query] -> _ = function *)
  (*   | `Envelope -> w "ENVELOPE" *)
  (*   | `Internal_date -> w "INTERNALDATE" *)
  (*   | `Rfc822_header -> w "RFC822.HEADER" *)
  (*   | `Rfc822_text -> w "RFC822.TEXT" *)
  (*   | `Rfc822_size -> w "RFC822.SIZE" *)
  (*   | `Rfc822 -> w "RFC822" *)
  (*   | `Body -> w "BODY" *)
  (*   | `Body_section (peek, s, partial) -> *)
  (*       let cmd = match peek with `Peek -> "BODY.PEEK" | `Look -> "BODY" in *)
  (*       let partial = match partial with *)
  (*         | None -> w "" *)
  (*         | Some (n, l) -> w "<" $ w_int n $ w "." $ w_int l $ w ">" *)
  (*       in *)
  (*       w cmd $ w "[" $ w_section s $ w "]" $ partial *)
  (*   | `Body_structure -> w "BODYSTRUCTURE" *)
  (*   | `Uid -> w "UID" *)
  (*   | `Flags -> w "FLAGS" *)

  (* let w_flag = function *)
  (*   | `Answered -> w "\\Answered" *)
  (*   | `Flagged -> w "\\Flagged" *)
  (*   | `Deleted -> w "\\Deleted" *)
  (*   | `Seen -> w "\\Seen" *)
  (*   | `Draft -> w "\\Draft" *)
  (*   | `Keyword s -> w s *)
  (*   | `Extension s -> w ("\\" ^ s) *)

  (* let w_tagged : [< command_lexeme] -> _ = function *)
  (*   | `Capability -> w "CAPABILITY" *)
  (*   | `Login (u, p) -> w "LOGIN" & w_string u & w_string p *)
  (*   | `Logout -> w "LOGOUT" *)
  (*   | `Noop -> w "NOOP" *)
  (*   | `Subscribe m -> w "SUBSCRIBE" & w_mailbox m *)
  (*   | `Unsubscribe m -> w "UNSUBSCRIBE" & w_mailbox m *)
  (*   | `List (m, s) -> w "LIST" & w_mailbox m & w_string s *)
  (*   | `Lsub (m, s) -> w "LSUB" & w_mailbox m & w_string s *)
  (*   | `Select (`Plain, m) -> w "SELECT" & w_mailbox m *)
  (*   | `Select (`Condstore, m) -> w "SELECT" & w_mailbox m & w "(CONDSTORE)" *)
  (*   | `Examine (`Plain, m) -> w "EXAMINE" & w_mailbox m *)
  (*   | `Examine (`Condstore, m) -> w "EXAMINE" & w_mailbox m & w "(CONDSTORE)" *)
  (*   | `Append (m, flags, internaldate, data) -> *)
  (*       let flags = match flags with None -> w "" | Some f -> w " " $ w_list w_flag f in *)
  (*       (\* TODO internaldate *\) *)
  (*       w "APPEND" & w_mailbox m $ flags & w_literal data *)
  (*   | `Create m -> w "CREATE" & w_mailbox m *)
  (*   | `Delete m -> w "DELETE" & w_mailbox m *)
  (*   | `Rename (m1, m2) -> w "RENAME" & w_mailbox m1 & w_mailbox m2 *)
  (*   | `Status (m, att) -> w "STATUS" & w_mailbox m & w_list w_status_att att *)
  (*   | `Close -> w "CLOSE" *)
  (*   | `Check -> w "CHECK" *)
  (*   | `Expunge -> w "EXPUNGE" *)
  (*   | `Fetch (uid, set, att, changed_since) -> *)
  (*       let changed_since = match changed_since with *)
  (*         | `All -> w "" *)
  (*         | `Changed_since m -> *)
  (*             w " (" $ w "CHANGEDSINCE" & w_uint64 m $ w ")" *)
  (*         | `Changed_since_vanished m -> *)
  (*             w " (" $ w "CHANGEDSINCE" & w_uint64 m & w "VANISHED" $ w ")" *)
  (*       in *)
  (*       let cmd = match uid with `Seq -> "FETCH" | `Uid -> "UID FETCH" in *)
  (*       let att = match att with *)
  (*         | `Fast -> w "FAST" *)
  (*         | `Full -> w "FULL" *)
  (*         | `All -> w "ALL" *)
  (*         | `List [x] -> w_fetch_att x *)
  (*         | `List l -> w_list w_fetch_att l *)
  (*       in *)
  (*       w cmd & w_eset set & att $ changed_since *)
  (*   | `Store (uid, set, silent, unchanged_since, mode, att) -> *)
  (*       let mode = match mode with `Add -> "+" | `Set -> "" | `Remove -> "-" in *)
  (*       let silent = match silent with `Silent -> ".SILENT" | `Loud -> "" in *)
  (*       let base = match att with *)
  (*         | `Flags _ -> mode ^ "FLAGS" ^ silent *)
  (*         | `Labels _ -> mode ^ "X-GM-LABELS" ^ silent *)
  (*       in *)
  (*       let att = match att with *)
  (*         | `Flags flags -> w_list w_flag flags *)
  (*         | `Labels labels -> w_list w_label labels *)
  (*       in *)
  (*       let unchanged_since = match unchanged_since with *)
  (*         | `All -> w "" *)
  (*         | `Unchanged_since m -> w " (" $ w "UNCHANGEDSINCE" & w_uint64 m $ w ")" *)
  (*       in *)
  (*       let cmd = match uid with `Seq -> "STORE" | `Uid -> "UID STORE" in *)
  (*       w cmd & w_eset set $ unchanged_since & w base & att *)
  (*   | `Copy (uid, set, m) -> *)
  (*       let cmd = match uid with `Seq -> "COPY" | `Uid -> "UID COPY" in *)
  (*       w cmd & w_eset set & w_mailbox m *)
  (*   | `Search (uid, sk) -> *)
  (*       let cmd = match uid with `Seq -> "SEARCH" | `Uid -> "UID SEARCH" in *)
  (*       w cmd & w_search_key sk *)
  (*   | `Enable c -> *)
  (*       w "ENABLE" & w_sep (fun x -> w (string_of_capability x)) c *)
  (*   | `Idle -> w "IDLE" *)
  (*   | `Authenticate name -> *)
  (*       w "AUTHENTICATE" & w name *)

  (* let ret _ = `Ok *)

  (* let rec encode e = function *)
  (*   | `Cmd (tag, x) -> w tag (w " " (w_tagged x (w_crlf (flush ret)))) e *)
  (*   | `Idle_done -> w "DONE" (w_crlf (flush ret)) e *)
  (*   | `Auth_step data -> w data (w_crlf (flush ret)) e *)
  (*   | `Auth_error -> w "*" (w_crlf (flush ret)) e *)

  (* let encoder () = *)
  (*   { *)
  (*     o = Bytes.create 4096; *)
  (*     o_pos = 0; *)
  (*   } *)

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
    raw (Uint32.to_string m)

  let uint64 m =
    raw (Uint64.to_string m)

  let label l =
    raw (Mutf7.encode l)

  let list ?(sep = ' ') f l =
    let rec loop = function
      | [] -> empty
      | [x] -> f x
      | x :: xs -> Cat (f x, Cat (Raw (String.make 1 sep), loop xs))
    in
    loop l

  let eset s =
    let f = function
      | (lo, Some hi) when lo = hi -> uint32 lo
      | (lo, Some hi) -> str (Printf.sprintf "%s:%s" (Uint32.to_string lo) (Uint32.to_string hi))
      | (lo, None) -> str (Printf.sprintf "%s:*" (Uint32.to_string lo))
    in
    list ~sep:',' f s

  let status_att = function
    | `Messages -> raw "MESSAGES"
    | `Recent -> raw "RECENT"
    | `Uid_next -> raw "UIDNEXT"
    | `Uid_validity -> raw "UIDVALIDITY"
    | `Unseen -> raw "UNSEEN"
    | `Highest_modseq -> raw "HIGHESTMODSEQ"

  let search_key _ =
    assert false

  let flag = function
    | `Answered -> raw "\\Answered"
    | `Flagged -> raw "\\Flagged"
    | `Deleted -> raw "\\Deleted"
    | `Seen -> raw "\\Seen"
    | `Draft -> raw "\\Draft"
    | `Keyword s -> raw s
    | `Extension s -> raw ("\\" ^ s)

  let fetch_att = function
    | ENVELOPE -> raw "ENVELOPE"
    | INTERNALDATE -> raw "INTERNALDATE"
    | RFC822_HEADER -> raw "RFC822.HEADER"
    | RFC822_TEXT -> raw "RFC822.TEXT"
    | RFC822_SIZE -> raw "RFC822.SIZE"
    | RFC822 -> raw "RFC822"
    | BODY -> raw "BODY"
    | Body_section (peek, s, partial) ->
        assert false
        (* let cmd = match peek with `Peek -> str "BODY.PEEK" | `Look -> str "BODY" in *)
        (* let partial = *)
        (*   match partial with *)
        (*   | None -> w "" *)
        (*   | Some (n, l) -> w "<" $ w_int n $ w "." $ w_int l $ w ">" *)
        (* in *)
        (* cmd $ w "[" $ w_section s $ w "]" $ partial *)
    | BODYSTRUCTURE -> raw "BODYSTRUCTURE"
    | UID -> raw "UID"
    | FLAGS -> raw "FLAGS"

  let capability _ =
    assert false

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
    let k = Cat (k, Cat (Raw "\r\n", Flush)) in
    out (Buffer.create 32) empty k
end

(* Running commands *)

open Response

type error =
  [ `Incorrect_tag of string * string
  | `Decode_error of Decoder.error * string * int
  | `Unexpected_cont
  | `Bad_greeting
  | `Auth_error of string
  | `Bad of code * string
  | `No of code * string ]

let pp_error ppf : error -> _ = function
  | `Incorrect_tag (exp, tag) -> pp ppf "@[Incorrect@ tag@ %S,@ should@ be@ %S@]" tag exp
  | `Decode_error e -> pp ppf "@[Decode@ error:@ %a@]" Decoder.pp_error e
  | `Unexpected_cont -> pp ppf "@[Unexpected continuation request@]"
  | `Bad_greeting -> pp ppf "@[Bad greeting@]"
  | `Auth_error s -> pp ppf "@[Authentication error: %s@]" s
  | `Bad (c, t) -> pp ppf "@[BAD:@ %a@ %S@]" pp_code c t
  | `No (c, t) -> pp ppf "@[NO:@ %a@ %S@]" pp_code c t

type state =
  {
    tag: int;
  }

let client =
  {
    tag = 1;
  }

type 'a command =
  {
    format: E.rope;
    default: 'a;
    process: untagged -> 'a -> 'a;
  }

type 'a progress_state =
  | Sending of 'a command
  | WaitingForCont of Readline.state * E.rope
  | Receiving of Readline.state * 'a * (untagged -> 'a -> 'a)

type 'a progress =
  {
    state: state;
    progress_state: 'a progress_state;
  }

type 'a result =
  | Ok of 'a
  | Error of error
  | Send of string * 'a progress
  | Refill of 'a progress

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
    | Capability caps1 -> caps @ caps1
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
    | List (flags, delim, mbox) -> res @ [flags, delim, mbox] (* CHECK *)
    | _ -> res
  in
  {format; default; process}

let lsub ?(ref = "") s =
  let format = E.(str "LSUB" ++ mailbox ref ++ str s) in
  let default = [] in
  let process u res =
    match u with
    | Lsub (flags, delim, mbox) -> res @ [flags, delim, mbox]
    | _ -> res
  in
  {format; default; process}

let status m att =
  let format = E.(str "STATUS" ++ mailbox m ++ p (list status_att att)) in
  let default = [] in
  let process u items =
    match u with
    | Status (mbox, items1) -> items @ [mbox, items1]
    | _ -> items
  in
  {format; default; process}

let copy ?(uid = true) s m =
  let open E in
  let cmd = str "COPY" in
  let cmd = if uid then str "UID" ++ cmd else cmd in
  let format = cmd ++ eset s ++ mailbox m in
  let default = () in
  let process _ () = () in
  {format; default; process}

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
    | Expunge n -> n :: info
    | _ -> info
  in
  {format; default; process}

let search ?(uid = true) sk =
  let open E in
  let cmd = str "SEARCH" in
  let cmd = if uid then str "UID" ++ cmd else cmd in
  cmd ++ search_key sk

let select_gen: type a. _ -> a condstore_flag -> _ -> a command = fun cmd condstore_flag m ->
  let open E in
  let condstore = match condstore_flag with Condstore -> p (str "CONDSTORE") | No_condstore -> str "" in
  let format = cmd ++ mailbox m ++ condstore in
  let default : a =
    match condstore_flag with
    | Condstore -> Modseq.zero
    | No_condstore -> ()
  in
  let process: untagged -> a -> a = fun u m ->
    match condstore_flag, u with
    | Condstore, State (Ok (`Highest_modseq m, _)) -> m
    | _ -> m
  in
  {format; default; process}

let select =
  select_gen E.(str "SELECT")

let examine =
  select_gen E.(str "EXAMINE")

let append m ?(flags = []) data =
  let format = E.(str "APPEND" ++ mailbox m ++ p (list flag flags) ++ literal data) in
  let default = () in
  let process _ () = () in
  {format; default; process}

let fetch_gen: type a. a uid_or_seq -> changed:_ -> vanished:_ -> _ -> _ -> (a * _) list command = fun uid ~changed ~vanished set att ->
  let open E in
  let cmd = str "FETCH" in
  let cmd = match uid with UID -> str "UID" ++ cmd | Seq -> cmd in
  let att =
    match att with
    | `Fast -> str "FAST"
    | `Full -> str "FULL"
    | `All -> str "ALL"
    | `List [x] -> fetch_att x
    | `List xs -> p (list fetch_att xs)
  in
  let changed_since =
    match changed, vanished with
    | None, true -> invalid_arg "fetch_gen"
    | None, false -> str ""
    | Some m, false -> p (str "CHANGEDSINCE" ++ uint64 m)
    | Some m, true -> p (str "CHANGEDSINCE" ++ uint64 m ++ str "VANISHED")
  in
  let format = cmd ++ eset set ++ att ++ changed_since in
  let default = [] in
  let process: untagged -> (a * 'b) list -> (a * 'b) list = fun u info ->
    match uid, u with
    | UID, Fetch (id, infos) -> (id, infos) :: info
    | Seq, Fetch (id, infos) -> (id, infos) :: info
    | _ -> info
  in
  {format; default; process}

let fetch uid ?changed ?(vanished = false) set att =
  fetch_gen uid ~changed ~vanished set (`List att)

let fetch_fast uid ?changed ?(vanished = false) set =
  fetch_gen uid ~changed ~vanished set `Fast

let fetch_full uid ?changed ?(vanished = false) set =
  fetch_gen uid ~changed ~vanished set `Full

let fetch_all uid ?changed ?(vanished = false) set =
  fetch_gen uid ~changed ~vanished set `All

let store_gen: type a. a uid_or_seq -> silent:_ -> unchanged:_ -> _ -> _ -> _ -> (a * _) list command = fun uid ~silent ~unchanged mode set att ->
  let open E in
  let cmd = str "STORE" in
  let cmd = match uid with UID -> str "UID" ++ cmd | Seq -> cmd in
  let mode = match mode with `Add -> "+" | `Set -> "" | `Remove -> "-" in
  let silent = if silent then ".SILENT" else "" in
  let base =
    match att with
    | `Flags _ -> str (Printf.sprintf "%sFLAGS%s" mode silent)
    | `Labels _ -> str (Printf.sprintf "%sX-GM-LABELS%s" mode silent)
  in
  let att =
    match att with
    | `Flags flags -> list flag flags
    | `Labels labels -> list label labels
  in
  let unchanged_since =
    match unchanged with
    | None -> str ""
    | Some m -> p (str "UNCHANGEDSINCE" ++ uint64 m)
  in
  let format = cmd ++ eset set ++ unchanged_since ++ base ++ p att in
  let default = [] in
  let process u m = m in
  {format; default; process}

let store_add_flags uid ?(silent = false) ?unchanged set flags =
  store_gen uid ~silent ~unchanged `Add set (`Flags flags)

let store_set_flags uid ?(silent = false) ?unchanged set flags =
  store_gen uid ~silent ~unchanged `Set set (`Flags flags)

let store_remove_flags uid ?(silent = false) ?unchanged set flags =
  store_gen uid ~silent ~unchanged `Remove set (`Flags flags)

let store_add_labels uid ?(silent = false) ?unchanged set labels =
  store_gen uid ~silent ~unchanged `Add set (`Labels labels)

let store_set_labels uid ?(silent = false) ?unchanged set labels =
  store_gen uid ~silent ~unchanged `Set set (`Labels labels)

let store_remove_labels uid ?(silent = false) ?unchanged set labels =
  store_gen uid ~silent ~unchanged `Remove set (`Labels labels)

let enable caps =
  let format = E.(str "ENABLE" ++ list capability caps) in
  let default = [] in
  let process u caps =
    match u with
    | Enabled caps1 -> caps1 @ caps
    | _ -> caps
  in
  {format; default; process}

let authenticate {name; _} =
  let format = E.(str "AUTHENTICATE" ++ raw name) in
  let default = () in
  let process _ () = () in
  {format; default; process}

let idle () =
  assert false
  (* let stop = ref false in *)
  (* let stop_l = Lazy.from_fun (fun () -> stop := true) in *)
  (* `Idle stop, (fun () -> Lazy.force stop_l) *)

(* let rec cont_req k r c = *)
(*   match r with *)
(*   | Cont _ -> k c *)
(*   | _ -> decode (cont_req k) c *)

(* and encode x k c = *)
(*   let rec loop = function *)
(*     | `Partial (s, i, l, k) -> *)
(*         `Write (s, i, l, (fun n -> loop (k n))) *)
(*     | `Wait_for_cont k -> *)
(*         decode (cont_req (fun c -> loop (k c.e))) c *)
(*     | `Ok -> *)
(*         k c *)
(*   in *)
(*   loop (E.encode c.e x) *)

(* and decode k c = *)
(*   let rec loop = function *)
(*     | `Ok x -> *)
(*         k x c *)
(*     | `Read (s, i, l, k) -> *)
(*         `Read (s, i, l, fun n -> loop (k n)) *)
(*     | `Error e -> *)
(*         `Error (`Decode_error e) (\* FIXME resume on the next line of input *\) *)
(*   in *)
(*   loop (D.decode c.d) *)

(* let rec h_tagged tag r c = *)
(*   let cur = string_of_int c.tag in *)
(*   c.tag <- c.tag + 1; *)
(*   if tag <> cur then *)
(*     `Error (`Incorrect_tag (cur, tag)) *)
(*   else (\* FIXME alert the user and continue ? *\) *)
(*     match r with *)
(*     | Ok (code, s) -> `Ok (code, s) *)
(*     | Bad (code, s) -> `Error (`Bad (code, s)) *)
(*     | No (code, s) -> `Error (`No (code, s)) *)

(* and h_response r c = *)
(*   match r with *)
(*   | Untagged r -> *)
(*       `Untagged (r, fun () -> decode h_response c) *)
(*   | Cont _ -> *)
(*       `Error `Unexpected_cont (\* FIXME ignore and continue ? *\) *)
(*   | Tagged (g, r) -> *)
(*       h_tagged g r c *)

(* and h_idle_response r c = *)
(*   match r with *)
(*   | Untagged r -> *)
(*       `Untagged (r, fun () -> decode h_idle_response c) *)
(*   | Cont _ when not c.idling -> *)
(*       c.idling <- true; decode h_idle_response c *)
(*   | Cont _ -> *)
(*       `Error `Unexpected_cont *)
(*   | Tagged (g, r) -> *)
(*       c.idling <- false; *)
(*       h_tagged g r c *)

(* and h_authenticate auth r c = *)
(*   match r with *)
(*   | Untagged r -> *)
(*       `Untagged (r, fun () -> decode (h_authenticate auth) c) *)
(*   | Tagged (g, r) -> *)
(*       h_tagged g r c *)
(*   | Cont data -> *)
(*       begin match auth.step (B64.decode data) with *)
(*       | `Ok data -> *)
(*           let data = B64.encode ~pad:true data in *)
(*           encode (`Auth_step data) (decode (h_authenticate auth)) c *)
(*       | `Error s -> *)
(*           encode `Auth_error (fun _ -> `Error (`Auth_error s)) c (\* (await (decode (h_authenticate auth)))) c *\) *)
(*       end *)

(* let h_greetings r c = *)
(*   match r with *)
(*   | Untagged (State (Ok (code, s))) -> `Ok (code, s) *)
(*   | _ -> `Error `Bad_greeting (\* FIXME continue until [`Ok] ? *\) *)

let continue progress =
  let {state; progress_state} = progress in
  match progress_state with
  | Sending {format; default; process} ->
      begin match E.out format with
      | E.Done s ->
          let progress_state = Receiving (Readline.empty, default, process) in
          Send (s, {state; progress_state})
      | E.WaitForCont (s, format) ->
          Send (s, {state; progress_state = WaitingForCont (Readline.empty, format)})
      end
  | WaitingForCont _
  | Receiving _ ->
      Refill progress

let feed progress s off len =
  let {state; progress_state} = progress in
  match progress_state with
  | WaitingForCont (buf, _) ->
      assert false
  | Receiving (buf, acc, process) ->
      let rec loop acc = function
        | Readline.Refill buf ->
            Refill {state; progress_state = Receiving (buf, acc, process)}
        | Readline.Next (buf, x) ->
            begin match Decoder.decode x with
            | Untagged u ->
                loop (process u acc) (Readline.next buf)
            | _ ->
                assert false
            end
        | Readline.Error ->
            Error `Bad_greeting (* FIXME *)
      in
      loop acc (Readline.feed buf s off len)
  | Sending _ ->
      invalid_arg "feed"

let run state cmd =
  {
    state;
    progress_state = Sending cmd;
  }
  (* match cmd with *)
  (* | `Authenticate a -> *)
  (*     encode (`Cmd (string_of_int c.tag, `Authenticate a.name)) *)
  (*       (decode (h_authenticate a)) c *)
  (* | `Idle stop -> *)
  (*     c.idle_stop <- stop; *)
  (*     encode (`Cmd (string_of_int c.tag, `Idle)) *)
  (*       (decode h_idle_response) c *)
  (* | #command_ordinary as cmd -> *)
  (*     encode (`Cmd (string_of_int c.tag, cmd)) *)
  (*       (decode h_response) c *)

(* let connection () = *)
(*   let c = *)
(*     { *)
(*       e = E.encoder (); *)
(*       d = D.decoder (); *)
(*       idling = false; *)
(*       idle_stop = ref false; *)
(*       tag = 0; *)
(*     } *)
(*   in *)
(*   c, decode h_greetings c *)
