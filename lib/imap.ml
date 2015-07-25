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

type uint32 = Uint32.t
type uint64 = Uint64.t

type set = (uint32 * uint32) list

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
      if i >= String.length s then () else
      match s.[i] with
      | '&'                   -> Buffer.add_string b "&-"; a (i + 1)
      | '\x20' .. '\x7E' as c -> Buffer.add_char b c; a (i + 1)
      | _                     -> Buffer.add_char b '&'; u i
    and u i =
      let upto j =
        let str = String.sub s i (j - i) and buf = Buffer.create 32 in
        recode ~encoding:`UTF_8 `UTF_16BE (`String str) (`Buffer buf);
        let str = B64.encode ~pad:false (Buffer.contents buf) in
        replace str '/' ',';
        Buffer.add_string b str; Buffer.add_char b '-'
      in
      let rec loop i =
        if i >= String.length s then upto i else
        match s.[i] with
        | '\x20' .. '\x7E' -> upto i; a i
        | _                -> loop (i + 1)
      in
      loop i
    in
    a 0;
    Buffer.contents b

  let decode s =
    let b = Buffer.create 32 in
    let rec a i =
      if i >= String.length s then () else
      match s.[i] with
      | '&' ->
          if i+1 < String.length s && s.[i] = '-' then (Buffer.add_char b '&'; a (i + 2)) else u (i + 1)
      | _ as c ->
          Buffer.add_char b c; a (i + 1)
    and u i =
      let start = i in
      let rec loop i =
        if i >= String.length s then invalid_arg "unterminated base64 input" else
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

type address =
  { ad_name : string;
    ad_adl : string;
    ad_mailbox : string;
    ad_host : string }

type envelope =
  { env_date : string;
    env_subject : string;
    env_from : address list;
    env_sender : address list;
    env_reply_to : address list;
    env_to : address list;
    env_cc : address list;
    env_bcc : address list;
    env_in_reply_to : string;
    env_message_id : string }

type capability =
  [ `Acl
  | `Binary
  | `Catenate
  | `Children
  | `Compress_deflate
  | `Condstore
  | `Enable
  | `Idle
  | `Id
  | `Literal_plus
  | `Multi_append
  | `Namespace
  | `Qresync
  | `Quote
  | `Sort
  | `Start_tls
  | `Uid_plus
  | `Unselect
  | `Xlist
  | `Auth of [ `Anonymous
             | `Login
             | `Plain ]
  | `Xoauth2
  | `Gmail
  | `Other of string ]

let string_of_capability = function
  | `Acl -> "ACL"
  | `Binary -> "BINARY"
  | `Catenate -> "CATENATE"
  | `Children -> "CHILDREN"
  | `Compress_deflate -> "COMPRESS=DEFLATE"
  | `Condstore -> "CONDSTORE"
  | `Enable -> "ENABLE"
  | `Idle -> "IDLE"
  | `Id -> "ID"
  | `Literal_plus -> "LITERAL+"
  | `Multi_append -> "MULTIAPPEND"
  | `Namespace -> "NAMESPACE"
  | `Qresync -> "QRESYNC"
  | `Quote -> "QUOTE"
  | `Sort -> "SORT"
  | `Start_tls -> "STARTTLS"
  | `Uid_plus -> "UIDPLUS"
  | `Unselect -> "UNSELECT"
  | `Xlist -> "XLIST"
  | `Auth `Anonymous -> "AUTH=ANONYMOUS"
  | `Auth `Login -> "AUTH=LOGIN"
  | `Auth `Plain -> "AUTH=PLAIN"
  | `Xoauth2 -> "XOAUTH2"
  | `Gmail -> "X-GM-EXT-1"
  | `Other s -> s

type fields =
  { fld_params : (string * string) list;
    fld_id : string option;
    fld_desc : string option;
    fld_enc : string;
    fld_octets : int }

type body_extension =
  [ `List of body_extension list
  | `Number of uint32
  | `String of string
  | `None ]

type part_extension =
  { ext_dsp : (string * (string * string) list) option;
    ext_lang : string list;
    ext_loc : string option;
    ext_ext : body_extension list }

(* type mime_sext = *)
(*   string option * part_extension *)

(* type mime_mext = *)
(*   (string * string) list * part_extension *)

type mime =
  [ `Text of string * fields * int
  | `Message of fields * envelope * mime * int
  | `Basic of string * string * fields
  | `Multipart of mime list * string ]

type flag =
  [ `Answered
  | `Flagged
  | `Deleted
  | `Seen
  | `Draft
  | `Keyword of string
  | `Extension of string ]

type section =
  [ `Header
  | `Header_fields of string list
  | `Header_fields_not of string list
  | `Text
  | `Mime
  | `Part of int * section
  | `All ]

type fetch_response =
  [ `Flags of [ flag | `Recent ] list
  | `Envelope of envelope
  | `Internal_date of date * time
  | `Rfc822 of string option
  | `Rfc822_header of string option
  | `Rfc822_text of string option
  | `Rfc822_size of int
  | `Body of mime
  | `Body_structure of mime
  | `Body_section of section * int option * string option
  | `Uid of uint32
  | `Modseq of uint64
  | `Gm_msgid of uint64
  | `Gm_thrid of uint64
  | `Gm_labels of string list ]

type code =
  [ `Alert
  | `Bad_charset of string list
  | `Capability of capability list
  | `Parse
  | `Permanent_flags of [ flag | `All ] list
  | `Read_only
  | `Read_write
  | `Try_create
  | `Uid_next of uint32
  | `Uid_validity of uint32
  | `Unseen of uint32
  | `Other of string * string option
  | `Closed
  | `Highest_modseq of uint64
  | `No_modseq
  | `Modified of (uint32 * uint32) list
  | `Append_uid of uint32 * uint32
  | `Copy_uid of uint32 * (uint32 * uint32) list * (uint32 * uint32) list
  | `Uid_not_sticky
  | `Compression_active
  | `Use_attr
  | `None ]

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
  [ `Messages of int
  | `Recent of int
  | `Uid_next of uint32
  | `Uid_validity of uint32
  | `Unseen of uint32
  | `Highest_modseq of uint64 ]

type state =
  [ `Ok of code * string | `No of code * string | `Bad of code * string ]

type untagged =
  [ state
  | `Bye of code * string
  | `Preauth of code * string
  | `Flags of flag list
  | `List of mbx_flag list * char option * string
  | `Lsub of mbx_flag list * char option * string
  | `Search of uint32 list * uint64 option
  | `Status of string * status_response list
  | `Exists of int
  | `Recent of int
  | `Expunge of uint32
  | `Fetch of uint32 * fetch_response list
  | `Capability of capability list
  | `Vanished of set
  | `Vanished_earlier of set
  | `Enabled of capability list ]

type response =
  [ untagged
  | `Cont of string
  | `Tagged of string * state ]

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

let pp_msg_att : _ -> [< fetch_response] -> _ = fun ppf att ->
  match att with
  | `Flags r          -> pp ppf "@[<2>(flags %a)@]" (pp_list pp_flag_fetch) r
  | `Envelope e       -> pp_envelope ppf e
  | `Internal_date (d, t) -> pp ppf "@[<2>(internal-date@ %a)@]" pp_date_time (d, t)
  | `Rfc822 s         -> pp ppf "(rfc822 %a)" (pp_opt pp_qstr) s
  | `Rfc822_header s  -> pp ppf "(rfc822-header %a)" (pp_opt pp_qstr) s
  | `Rfc822_text s    -> pp ppf "(rfc822-text %a)" (pp_opt pp_qstr) s
  | `Rfc822_size n    -> pp ppf "(rfc822-size %i)" n
  | `Body b           -> pp ppf "@[<2>(body@ %a)@]" pp_mime b
  | `Body_structure b -> pp ppf "@[<2>(bodystructure@ %a)@]" pp_mime b
  | `Body_section (s, n, x) -> pp ppf "@[<2>(body-section@ %a@ %a@ %a)@]"
                                 pp_section s (pp_opt Format.pp_print_int) n (pp_opt pp_qstr) x
  | `Uid n            -> pp ppf "(uid %a)" Uint32.printer n
  | `Modseq m         -> pp ppf "(modseq %a)" Uint64.printer m
  | `Gm_msgid m       -> pp ppf "(gm-msgid %a)" Uint64.printer m
  | `Gm_thrid m       -> pp ppf "(gm-thrid %a)" Uint64.printer m
  | `Gm_labels l      -> pp ppf "@[<2>(gm-labels@ %a)@]" (pp_list pp_qstr) l

let pp_fetch_response = pp_msg_att

let pp_set ppf s =
  let rg ppf (x, y) = pp ppf "%a-%a" Uint32.printer x Uint32.printer y in
  pp_list rg ppf s

let pp_cap ppf = function
  | `Acl              -> pp ppf "acl"
  | `Binary           -> pp ppf "binary"
  | `Catenate         -> pp ppf "catenate"
  | `Children         -> pp ppf "children"
  | `Compress_deflate -> pp ppf "compress-deflate"
  | `Condstore        -> pp ppf "condstore"
  | `Enable           -> pp ppf "enable"
  | `Idle             -> pp ppf "idle"
  | `Id               -> pp ppf "id"
  | `Literal_plus     -> pp ppf "literal+"
  | `Multi_append     -> pp ppf "multi-append"
  | `Namespace        -> pp ppf "namespace"
  | `Qresync          -> pp ppf "qresync"
  | `Quote            -> pp ppf "quote"
  | `Sort             -> pp ppf "sort"
  | `Start_tls        -> pp ppf "start-tls"
  | `Uid_plus         -> pp ppf "uid-plus"
  | `Unselect         -> pp ppf "unselect"
  | `Xlist            -> pp ppf "xlist"
  | `Auth `Anonymous  -> pp ppf "auth-anonymous"
  | `Auth `Login      -> pp ppf "auth-login"
  | `Auth `Plain      -> pp ppf "auth-plain"
  | `Xoauth2          -> pp ppf "xoauth2"
  | `Gmail            -> pp ppf "gmail"
  | `Other s          -> pp ppf "(other %S)" s

let pp_capability = pp_cap

let pp_code : _ -> [< code] -> _ = fun ppf c ->
  match c with
  | `Alert                -> pp ppf "alert"
  | `Bad_charset cs       -> pp ppf "@[<2>(badcharset %a)@]" (pp_list pp_string) cs
  | `Capability caps      -> pp ppf "@[<2>(capability %a)@]" (pp_list pp_cap) caps
  | `Parse                -> pp ppf "parse"
  | `Permanent_flags fl   -> pp ppf "@[<2>(permanent-flags %a)@]" (pp_list pp_flag_perm) fl
  | `Read_only            -> pp ppf "read-only"
  | `Read_write           -> pp ppf "read-write"
  | `Try_create           -> pp ppf "try-create"
  | `Uid_next uid         -> pp ppf "(uid-next %a)" Uint32.printer uid
  | `Uid_validity uid     -> pp ppf "(uid-validity %a)" Uint32.printer uid
  | `Unseen n             -> pp ppf "(unseen %a)" Uint32.printer n
  | `Other (k, v)         -> pp ppf "(other@ %S@ %a)" k (pp_opt pp_qstr) v
  | `Closed               -> pp ppf "closed"
  | `Highest_modseq m     -> pp ppf "(highest-modseq %a)" Uint64.printer m
  | `No_modseq            -> pp ppf "no-modseq"
  | `Modified s           -> pp ppf "(modified@ %a)" pp_set s
  | `Append_uid (n, m)    -> pp ppf "(append-uid %a@ %a)" Uint32.printer n Uint32.printer m
  | `Copy_uid (n, s1, s2) -> pp ppf "(copy-uid %a@ %a@ %a)" Uint32.printer n pp_set s1 pp_set s2
  | `Uid_not_sticky       -> pp ppf "uid-not-sticky"
  | `Compression_active   -> pp ppf "compression-active"
  | `Use_attr             -> pp ppf "use-attr"
  | `None                 -> pp ppf "none"

let pp_state ppf = function
  | `Ok (c, t)  -> pp ppf "@[<2>(ok@ %a@ %S)@]" pp_code c t
  | `No (c, t)  -> pp ppf "@[<2>(no@ %a@ %S)@]" pp_code c t
  | `Bad (c, t) -> pp ppf "@[<2>(bad@ %a@ %S)@]" pp_code c t

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

let pp_mbx_status : _ -> [< status_response] -> _ = fun ppf s ->
  match s with
  | `Messages n       -> pp ppf "(messages %i)" n
  | `Recent n         -> pp ppf "(recent %i)" n
  | `Uid_next uid     -> pp ppf "(uid-next %a)" Uint32.printer uid
  | `Uid_validity uid -> pp ppf "(uid-validity %a)" Uint32.printer uid
  | `Unseen n         -> pp ppf "(unseen %a)" Uint32.printer n
  | `Highest_modseq m -> pp ppf "(highest-modseq %a)" Uint64.printer m

let pp_response : _ -> [< response] -> _ = fun ppf r ->
  match r with
  | #state as s         -> pp_state ppf s
  | `Bye (c, t)         -> pp ppf "@[<2>(bye@ %a@ %S)@]" pp_code c t
  | `Flags flags        -> pp ppf "@[<2>(flags@ %a)@]" (pp_list pp_flag) flags
  | `List (f, s, m)     -> pp ppf "@[<2>(list@ (flags@ %a)@ %a@ %S)@]" (pp_list pp_mbx_flag) f
                             (pp_opt pp_char) s m
  | `Lsub (f, s, m)     -> pp ppf "@[<2>(lsub@ (flags@ %a)@ %a@ %S)@]" (pp_list pp_mbx_flag) f
                             (pp_opt pp_char) s m
  | `Search (ns, m)     -> pp ppf "@[<2>(search@ %a@ %a)@]" (pp_list Uint32.printer) ns (pp_opt Uint64.printer) m
  | `Status (m, s)      -> pp ppf "@[<2>(status@ %S@ %a)@]" m (pp_list pp_mbx_status) s
  | `Exists n           -> pp ppf "(exists %i)" n
  | `Recent n           -> pp ppf "(recent %i)" n
  | `Expunge n          -> pp ppf "(expunge %a)" Uint32.printer n
  | `Fetch (n, atts)    -> pp ppf "@[<2>(fetch %a@ %a)@]" Uint32.printer n (pp_list pp_msg_att) atts
  | `Capability r       -> pp ppf "@[<2>(capability %a)@]" (pp_list pp_cap) r
  | `Preauth (c, t)     -> pp ppf "@[<2>(preauth@ %a@ %S)@]" pp_code c t
  | `Cont s             -> pp ppf "@[<2>(cont@ %S)@]" s
  | `Tagged (t, s)      -> pp ppf "@[<2>(tagged@ %S@ %a)@]" t pp_state s
  | `Vanished s         -> pp ppf "@[<2>(vanished@ %a)@]" pp_set s
  | `Vanished_earlier s -> pp ppf "@[<2>(vanished-earlier@ %a)@]" pp_set s
  | `Enabled s          -> pp ppf "@[<2>(enabled@ %a)@]" (pp_list pp_cap) s

module D = struct

  (* Decoder *)

  type src = [ `String of string | `Channel of in_channel | `Manual ]

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
    | `Await -> pp ppf "`Await"
    | `Error e -> pp ppf "@[`Error %a@]" pp_error e

  type decoder =
    { src : src;
      mutable i : string;
      mutable i_pos : int;
      mutable i_max : int;
      buf : Buffer.t;
      mutable k : decoder -> [ `Ok of response | `Await | `Error of error * string * int ] }

  external ($) : ('a -> 'b) -> 'a -> 'b = "%apply"

  let (--) a b =
    if String.length a <> String.length b then false else
    let rec loop i =
      if i >= String.length a then true else
      if Char.uppercase a.[i] = Char.uppercase b.[i] then loop (i + 1) else
      false
    in
    loop 0

  let err e d = `Error (e, d.i, d.i_pos)
  let err_expected c d = err (`Expected_char c) d
  let err_expected_s s d = err (`Expected_string s) d
  let err_unexpected_s a d = err (`Unexpected_string a) d
  let err_unexpected c d = err (`Unexpected_char c) d
  let err_quoted c d = err (`Illegal_char c) d
  let err_unexpected_eoi d = err `Unexpected_eoi d

  let ret x _ = `Ok x
  let cur d = d.i.[d.i_pos]
  let badd d = Buffer.add_char d.buf $ cur d
  let buf d = let s = Buffer.contents d.buf in (Buffer.clear d.buf; s)
  let decode d = d.k d
  let rec eoi d = d.k <- err_unexpected_eoi

  let src d s j l =                                     (* set [d.i] with [s]. *)
    if j < 0 || l < 0 || j + l > String.length s then invalid_arg "bounds" else
    if l = 0 then eoi d else
    (d.i <- s; d.i_pos <- j; d.i_max <- j + l)

  let refill k d = match d.src with
    | `Manual     -> d.k <- k; `Await
    | `String _   -> eoi d; k d
    | `Channel ic ->
        let rc = input ic d.i 0 (String.length d.i) in
        (src d d.i 0 rc; k d)

  let readc k d =
    if d.i_pos + 1 < d.i_max then (d.i_pos <- d.i_pos + 1; k d) else (* CHECK + 1 *)
    refill k d

  let p_ch c k d =
    if cur d = c then readc k d else err_expected c d

  let p_sp k d = p_ch ' ' k d

  let p_while1 f k d = (* TODO do not use d.buf if not necessary *)
    if f $ cur d then
      let rec nxt d = if f $ cur d then (badd d; readc nxt d) else k $ buf d $ d in
      (badd d; readc nxt d)
    else
    err_unexpected $ cur d $ d

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

  let p_atom k d =
    p_while1 is_atom_char k d

  let p_atomf s k d =
    p_atom (fun a d -> if a -- s then k d else err_expected_s s d) d

  let p_t3 p1 p2 p3 k d =
    p1 (fun x -> p_sp $ p2 (fun y -> p_sp $ p3 (k x y))) d

  let p_t2 p1 p2 k d =
    p1 (fun x -> p_sp $ p2 (k x)) d

  let p_sep p k d =
    let rec loop acc d =
      if cur d = ' ' then readc $ p (fun x -> loop (x :: acc)) $ d else
      k $ List.rev acc $ d
    in
    loop [] d

  let p_list1 p k d =
    let rec loop acc d =
      if cur d = ' ' then readc $ p (fun x -> loop (x :: acc)) $ d else
      if cur d = ')' then readc $ k (List.rev acc) $ d else
      err_unexpected $ cur d $ d
    in
    p_ch '(' $ p (fun x -> loop [x]) $ d

  let p_list p k d =
    let rec loop acc d =
      if cur d = ' ' then readc $ p (fun x -> loop (x :: acc)) $ d else
      if cur d = ')' then readc $ k (List.rev acc) $ d else
      err_unexpected $ cur d $ d
    in
    if is_atom_char $ cur d then p_atomf "NIL" $ k [] $ d else
    p_ch '(' (fun d -> if cur d = ')' then readc $ k [] $ d else p (fun x -> loop [x]) d) d

  let p_list_nosp p k d =
    let rec loop acc d =
      if cur d = ')' then readc $ k (List.rev acc) $ d else
      p (fun x -> loop (x :: acc)) $ d
    in
    if is_atom_char $ cur d then p_atomf "NIL" $ k [] $ d else
    p_ch '(' (fun d -> if cur d = ')' then readc $ k [] $ d else p (fun x -> loop [x]) d) d

(*
   CR             =  %x0D
                                  ; carriage return

   LF             =  %x0A
                                  ; linefeed

   CRLF           =  CR LF
                          ; Internet standard newline
*)

  let p_crlf k d = (* does not advance the i_pos pointer beyond the '\n' character because we might be at eoi *)
    p_ch '\r' (fun d -> if cur d = '\n' then k d else err_expected '\n' d) d

(*
   number          = 1*DIGIT
                       ; Unsigned 32-bit integer
                       ; (0 <= n < 4,294,967,296)

   nz-number       = digit-nz *DIGIT
                       ; Non-zero unsigned 32-bit integer
                       ; (0 < n < 4,294,967,296)
*)

  let is_digit = function '0' .. '9' -> true | _ -> false
  let p_uint k d = p_while1 is_digit (fun s -> k $ int_of_string s) d
  let p_uint32 k d = p_while1 is_digit (fun s -> k $ Uint32.of_string s) d
  let p_uint64 k d = p_while1 is_digit (fun s -> k $ Uint64.of_string s) d

(*
   quoted          = DQUOTE *QUOTED-CHAR DQUOTE

   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
                     '\\' quoted-specials
*)

  let rec p_quoted k d = (* "" was eaten *)
    match cur d with
    | '\r' | '\n' -> err_quoted $ cur d $ d
    | '"' -> readc $ k (buf d) $ d
    | '\\' ->
        let nxt d =
          if cur d = '"' || cur d = '\\' then (badd d; readc (p_quoted k) d) else
          err_unexpected $ cur d $ d
        in
        readc nxt d
    | '\x01' .. '\x7F' -> badd d; readc (p_quoted k) d
    | _ -> err_unexpected $ cur d $ d

(*
   literal         = "{" number "}" CRLF *CHAR8
                       ; Number represents the number of CHAR8s

   string          = quoted / literal
*)

  let p_literal n k d = (* reads n bytes *)
    if d.i_max - d.i_pos > n then begin
      let s = String.sub d.i d.i_pos n in
      d.i_pos <- d.i_pos + n;
      k s d
    end else
    let rec loop rem d =
      let have = d.i_max - d.i_pos in
      if have > rem then begin
        Buffer.add_substring d.buf d.i d.i_pos rem;
        d.i_pos <- d.i_pos + rem;
        k $ buf d $ d
      end else begin
        Buffer.add_substring d.buf d.i d.i_pos have;
        d.i_pos <- d.i_max;
        readc $ loop (rem - have) $ d
      end
    in
    loop n d

  let p_string k d =
    if cur d = '"' then readc $ p_quoted k $ d else
    if cur d = '{' then readc $ p_uint (fun n -> p_ch '}' $ p_crlf (readc (p_literal n k))) $ d else
    err_unexpected $ cur d $ d

(*
   ASTRING-CHAR   = ATOM-CHAR / resp-specials

   astring         = 1*ASTRING-CHAR / string
*)

  let is_astring_char c =
    is_atom_char c || c = ']'

  let p_astring k d =
    if is_astring_char $ cur d then p_while1 is_astring_char k d else
    p_string k d

(*
   TEXT-CHAR       = <any CHAR except CR and LF>

   text            = 1*TEXT-CHAR
*)

  let is_text_char = function
    | '\r' | '\n' -> false
    | '\x01' .. '\x7F' -> true
    | _ -> false

  let p_text k d =
    if is_text_char $ cur d then p_while1 is_text_char k d else
    k "" d (* allow empty texts for greater tolerance *)

(*
   nil             = "NIL"

   nstring         = string / nil
*)

  let p_nstring k d =
    if is_atom_char $ cur d then p_atomf "NIL" $ k None $ d else
    p_string (fun s -> k $ Some s) d

  let p_nstring' k d =
    p_nstring (function Some s -> k s | None -> k "") d

(*
   DIGIT          =  %x30-39
                          ; 0-9
*)

  let p_digit k d =
    if is_digit $ cur d then k $ (Char.code $ cur d) - 0x30 $ d else
    err_unexpected $ cur d $ d

  let p_digit2 k d =
    p_digit (fun n -> p_digit (fun m -> k $ 10 * n + m)) d

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

  let p_flag k d =
    if cur d = '\\' then
      readc $ p_atom begin fun a d ->
        if a -- "Answered" then k `Answered d else
        if a -- "Flagged" then k `Flagged d else
        if a -- "Deleted" then k `Deleted d else
        if a -- "Seen" then k `Seen d else
        if a -- "Draft" then k `Draft d else
        k $ `Extension a $ d
      end $ d
    else
    p_atom $ (fun a d -> k $ `Keyword a $ d) $ d

(*
   flag-fetch      = flag / "\Recent"
*)

  let p_flag_fetch k d =
    if cur d = '\\' then
      readc $ p_atom begin fun a d ->
        if a -- "Answered" then k `Answered d else
        if a -- "Flagged" then k `Flagged d else
        if a -- "Deleted" then k `Deleted d else
        if a -- "Seen" then k `Seen d else
        if a -- "Draft" then k `Draft d else
        if a -- "Recent" then k `Recent d else
        k $ `Extension a $ d
      end $ d
    else
    p_atom $ (fun a d -> k $ `Keyword a $ d) $ d

(*
   flag-perm       = flag / "\*"
*)

  let p_flag_perm k d =
    if cur d = '\\' then
      readc begin fun d ->
        if cur d = '*' then readc $ k `All $ d else
        p_atom begin fun a d ->
          if a -- "Answered" then k `Answered d else
          if a -- "Flagged" then k `Flagged d else
          if a -- "Deleted" then k `Deleted d else
          if a -- "Seen" then k `Seen d else
          if a -- "Draft" then k `Draft d else
          k $ `Extension a $ d
        end d
      end d
    else
    p_atom $ (fun a d -> k $ `Keyword a $ d) $ d

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

  (* We never parse '*' since it does not seem to show up in responses *)
  let p_set k d =
    let rg k d =
      let nxt n d = if cur d = ':' then readc $ p_uint32 (fun m -> k (n, m)) $ d else k $ (n, n) $ d in
      (* if cur d = '*' then err_illegal_range d else *)
      p_uint32 nxt d
    in
    let rec loop acc d =
      if cur d = ',' then readc $ rg (fun r -> loop (r :: acc)) $ d else
      k (List.rev acc) d
    in
    rg (fun r -> loop [r]) d


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

  let p_text_1 k d =
    if is_text_other_char $ cur d then p_while1 is_text_other_char k d else
    k "" d (* We allow empty text_1 *)

  let p_cap k d =
    p_atom begin fun a d ->
      if a -- "COMPRESS=DEFLATE" then k `Compress_deflate d else
      if a -- "CONDSTORE" then k `Condstore d else
      if a -- "ENABLE" then k `Enable d else
      if a -- "IDLE" then k `Idle d else
      if a -- "LITERAL+" then k `Literal_plus d else
      if a -- "NAMESPACE" then k `Namespace d else
      if a -- "ID" then k `Id d else
      if a -- "QRESYNC" then k `Qresync d else
      if a -- "UIDPLUS" then k `Uid_plus d else
      if a -- "UNSELECT" then k `Unselect d else
      if a -- "XLIST" then k `Xlist d else
      if a -- "AUTH=PLAIN" then k $ `Auth `Plain $ d else
      if a -- "AUTH=LOGIN" then k $ `Auth `Login $ d else
      if a -- "XOAUTH2" then k `Xoauth2 d else
      if a -- "X-GM-EXT-1" then k `Gmail d else
      k $ `Other a $ d
    end d

  let p_code k d = (* '[' was eaten *)
    p_atom begin fun a d ->
      if a -- "ALERT" then k `Alert d else
      if a -- "BADCHARSET" then
        if cur d = ' ' then readc $ p_list1 p_astring (fun xs -> k $ `Bad_charset xs) $ d else
        k $ `Bad_charset [] $ d else
      if a -- "CAPABILITY" then p_sep p_cap (fun xs -> k $ `Capability xs) d else
      if a -- "PARSE" then k `Parse d else
      if a -- "PERMANENTFLAGS" then p_sp $ p_list p_flag_perm (fun xs -> k $ `Permanent_flags xs) $ d else
      if a -- "READ-ONLY" then k `Read_only d else
      if a -- "READ-WRITE" then k `Read_write d else
      if a -- "TRYCREATE" then k `Try_create d else
      if a -- "UIDNEXT" then p_sp $ p_uint32 (fun n -> k $ `Uid_next n) $ d else
      if a -- "UIDVALIDITY" then p_sp $ p_uint32 (fun n -> k $ `Uid_validity n) $ d else
      if a -- "UNSEEN" then p_sp $ p_uint32 (fun n -> k $ `Unseen n) $ d else
      if a -- "CLOSED" then k `Closed d else
      if a -- "HIGHESTMODSEQ" then p_sp $ p_uint64 (fun n -> k $ `Highest_modseq n) $ d else
      if a -- "NOMODSEQ" then k `No_modseq d else
      if a -- "MODIFIED" then p_sp $ p_set (fun s -> k $ `Modified s) $ d else
      if a -- "APPENDUID" then p_sp $ p_t2 p_uint32 p_uint32 (fun n m -> k $ `Append_uid (n, m)) $ d else
      if a -- "COPYUID" then p_sp $ p_t3 p_uint32 p_set p_set (fun n s1 s2 -> k $ `Copy_uid (n, s1, s2)) $ d else
      if a -- "UIDNOTSTICKY" then k `Uid_not_sticky d else
      if a -- "COMPRESSIONACTIVE" then k `Compression_active d else
      if a -- "USEATTR" then k `Use_attr d else
      if cur d = ' ' then readc $ p_text_1 (fun x -> k $ `Other (a, Some x)) $ d else
      k $ `Other (a, None) $ d
    end d

(*
   resp-text       = ["[" resp-text-code "]" SP] text
*)

  let p_resp_text k d =
    if cur d = '[' then
      let nxt x d = p_ch ']' (fun d -> if cur d = ' ' then readc $ p_text (k x) $ d else k x "" d) d in
      readc $ p_code nxt $ d
    else
    p_text $ k `None $ d

(*
   resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
                       ; Status condition
*)

  let p_resp_cond_state k d =
    p_atom begin fun a d ->
      if a -- "OK" then p_sp $ p_resp_text (fun c t -> k $ `Ok (c, t)) $ d else
      if a -- "NO" then p_sp $ p_resp_text (fun c t -> k $ `No (c, t)) $ d else
      if a -- "BAD" then p_sp $ p_resp_text (fun c t -> k $ `Bad (c, t)) $ d else
      err_unexpected_s a d
    end d

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

  let p_mbx_flag k d =
    p_ch '\\' $ p_atom begin fun a d ->
      if a -- "Noselect" then k `Noselect d else
      if a -- "Marked" then k `Marked d else
      if a -- "Unmarked" then k `Unmarked d else
      if a -- "Noinferiors" then k `Noinferiors d else
      if a -- "HasChildren" then k `HasChildren d else
      if a -- "HasNoChildren" then k `HasNoChildren d else
      if a -- "All" then k `All d else
      if a -- "Archive" then k `Archive d else
      if a -- "Drafts" then k `Drafts d else
      if a -- "Flagged" then k `Flagged d else
      if a -- "Junk" then k `Junk d else
      if a -- "Sent" then k `Sent d else
      if a -- "Trash" then k `Trash d else
      k $ `Extension a $ d
    end $ d

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

  let p_mailbox k d =
    let decode_mailbox_name s = try Mutf7.decode s with _ -> s in (* FIXME handle error *)
    p_astring (fun s -> k $ decode_mailbox_name s) d

(*
   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
                     '\\' quoted-specials

   mailbox-list    = "(" [mbx-list-flags] ")" SP
                      (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
*)

  let p_quoted_char k d =
    let is_qchar = function '\r' | '\n' | '\\' | '"' -> false | '\x01' .. '\x7F' -> true | _ -> false in
    if cur d = '\\' then
      let nxt d =
        if cur d = '\\' || cur d = '"' then let c = cur d in readc $ k c $ d else
        err_unexpected $ cur d $ d
      in
      readc nxt d
    else
    if is_qchar $ cur d then let c = cur d in readc $ k c $ d else
    err_unexpected $ cur d $ d

  let p_delim k d =
    if cur d = '"' then readc $ p_quoted_char (fun c -> p_ch '"' $ k (Some c)) $ d else
    p_atomf "NIL" $ k None $ d

  let p_mailbox_list k d =
    p_t3 $ p_list p_mbx_flag $ p_delim $ p_mailbox $ k $ d

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

  let p_status_att k d =
    p_atom begin fun a d ->
      if a -- "MESSAGES" then p_sp $ p_uint (fun n -> k $ `Messages n) $ d else
      if a -- "RECENT" then p_sp $ p_uint (fun n -> k $ `Recent n) $ d else
      if a -- "UIDNEXT" then p_sp $ p_uint32 (fun n -> k $ `Uid_next n) $ d else
      if a -- "UIDVALIDITY" then p_sp $ p_uint32 (fun n -> k $ `Uid_validity n) $ d else
      if a -- "UNSEEN" then p_sp $ p_uint32 (fun n -> k $ `Unseen n) $ d else
      if a -- "HIGHESTMODSEQ" then p_sp $ p_uint64 (fun n -> k $ `Highest_modseq n) $ d else
      err_unexpected_s a d
    end d

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

  let p_address k d =
    p_ch '(' begin
      p_nstring' @@ fun ad_name ->
      p_sp $ p_nstring' @@ fun ad_adl ->
      p_sp $ p_nstring' @@ fun ad_mailbox ->
      p_sp $ p_nstring' @@ fun ad_host ->
      p_ch ')' $ k {ad_name; ad_adl; ad_mailbox; ad_host}
    end d

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

  let p_envelope k d =
    p_ch '(' begin
             p_nstring'       @@ fun env_date ->
      p_sp $ p_nstring'       @@ fun env_subject ->
      p_sp $ p_list_nosp p_address @@ fun env_from ->
      p_sp $ p_list_nosp p_address @@ fun env_sender ->
      p_sp $ p_list_nosp p_address @@ fun env_reply_to ->
      p_sp $ p_list_nosp p_address @@ fun env_to ->
      p_sp $ p_list_nosp p_address @@ fun env_cc ->
      p_sp $ p_list_nosp p_address @@ fun env_bcc ->
      p_sp $ p_nstring'       @@ fun env_in_reply_to ->
      p_sp $ p_nstring'       @@ fun env_message_id ->
      p_ch ')' $
      k {env_date; env_subject; env_from; env_sender;
         env_reply_to; env_to; env_cc; env_bcc; env_in_reply_to;
         env_message_id}
    end d

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

  let p_fld_param k d =
    let param k d = p_string (fun v -> p_sp $ p_string (fun v' -> k (v, v'))) d in
    p_list param k d

  let p_body_fields k d =
    p_fld_param begin fun params ->
      p_sp $ p_nstring @@ fun id ->
      p_sp $ p_nstring @@ fun desc ->
      p_sp $ p_string  @@ fun enc ->
      p_sp $ p_uint    @@ fun octets ->
      k {fld_params = params; fld_id = id; fld_desc = desc; fld_enc = enc; fld_octets = octets}
    end d

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

  let rec p_body_ext k d =
    if is_digit $ cur d then p_uint32 (fun n -> k $ `Number n) d else
    if cur d = '(' then p_list1 p_body_ext (fun xs -> k $ `List xs) d else
    p_nstring (function Some x -> k $ `String x | None -> k $ `None) d

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

  let p_fld_dsp k d =
    if cur d = '(' then
      readc $ p_string (fun s -> p_sp $ p_fld_param (fun p -> p_ch ')' $ k (Some (s, p)))) $ d
    else
    p_atomf "NIL" (k None) d

  let p_fld_lang k d =
    if cur d = '(' then p_list1 p_string k d else
    p_nstring (function Some x -> k [x] | None -> k []) d

  let r_body_ext k d =
    if cur d = ' ' then readc $ p_fld_dsp (fun dsp d ->
        if cur d = ' ' then readc $ p_fld_lang (fun lang d ->
            if cur d = ' ' then readc $ p_nstring (fun loc ->
                p_sep p_body_ext (fun ext ->
                    k {ext_dsp = dsp; ext_lang = lang; ext_loc = loc; ext_ext = ext})) $ d else
            k {ext_dsp = dsp; ext_lang = lang; ext_loc = None; ext_ext = []} d) $ d else
        k {ext_dsp = dsp; ext_lang = []; ext_loc = None; ext_ext = []} d) $ d else
    k {ext_dsp = None; ext_lang = []; ext_loc = None; ext_ext = []} d

  let r_sbody_ext k d =
    p_nstring (fun md5 -> r_body_ext (k md5)) d

  let r_mbody_ext k d =
    p_fld_param (fun params -> r_body_ext (k params)) d

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

  let rec p_mbody k d = (* TODO Return the extension data *)
    let rec loop acc d =
      if cur d = '(' then p_body (fun x -> loop (x :: acc)) d else
      if cur d = ' ' then
        readc $ p_string (fun m d ->
            if cur d = ' ' then readc $ r_mbody_ext (fun _ _ -> k $ `Multipart (List.rev acc, m)) $ d else
            k $ `Multipart (List.rev acc, m) $ d) $ d else
      err_unexpected $ cur d $ d
    in
    loop [] d

  and p_sbody k d = (* TODO Return the extension data *)
    let ext k d = if cur d = ' ' then readc $ r_sbody_ext (fun _ _ -> k) $ d else k d in
    let msg f k d = p_t3 p_envelope p_body p_uint (fun e b n -> ext $ k (`Message (f, e, b, n))) d in
    let nxt m t f d =
      if m -- "MESSAGE" && t -- "RFC822" then p_sp $ msg f k $ d else
      if m -- "TEXT" then p_sp $ p_uint (fun n -> ext $ k (`Text (t, f, n))) $ d else
      ext $ k (`Basic (m, t, f)) $ d
    in
    p_t3 p_string p_string p_body_fields nxt d

  and p_body k d =
    let nxt k d = if cur d = '(' then p_mbody k d else p_sbody k d in
    p_ch '(' $ nxt (fun x d -> p_ch ')' $ k x $ d) $ d

(*
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

  let p_date_time k d =
    let p s d =
      try
        Scanf.sscanf s "%2d-%3s-%4d %2d:%2d:%2d %5d" begin fun day m year hr mn sc z ->
          let month =
            if m -- "Jan" then 0 else
            if m -- "Feb" then 1 else
            if m -- "Mar" then 2 else
            if m -- "Apr" then 3 else
            if m -- "May" then 4 else
            if m -- "Jun" then 5 else
            if m -- "Jul" then 6 else
            if m -- "Aug" then 7 else
            if m -- "Sep" then 8 else
            if m -- "Oct" then 9 else
            if m -- "Nov" then 10 else
            if m -- "Dec" then 11 else
            assert false
          in
          p_ch '"' $ k { day; month; year } { hours = hr; minutes = mn; seconds = sc; zone = z } $ d
        end
      with _ ->
        err_unexpected_s s d
    in
    p_ch '"' $ p_literal 26 p $ d

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

  let p_msgtext k d =
    p_atom begin fun a d ->
      if a -- "HEADER" then k `Header d else
      if a -- "HEADER.FIELDS" then p_sp $ p_list1 p_astring (fun xs -> k $ `Header_fields xs) $ d else
      if a -- "HEADER.FIELDS.NOT" then p_sp $ p_list1 p_astring (fun xs -> k $ `Header_fields_not xs) $ d else
      if a -- "TEXT" then k `Text d else
      if a -- "MIME" then k `Mime d else
      err_unexpected_s a d
    end d

  let rec p_part k d =
    if is_digit $ cur d then p_uint (fun n -> p_ch '.' $ p_part (fun s -> k $ `Part (n, s))) d else
    p_msgtext k d

  let p_section k d =
    let nxt d =
      if cur d = ']' then readc $ k `All $ d else
      p_part (fun s -> p_ch ']' $ k s) d
    in
    p_ch '[' nxt d

(*
   uniqueid        = nz-number
                       ; Strictly ascending

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

  let p_msg_att k d =
    p_while1 (fun ch -> is_atom_char ch && ch <> '[') begin fun a d ->
      if a -- "FLAGS" then p_sp $ p_list p_flag_fetch (fun xs -> k $ `Flags xs) $ d else
      if a -- "ENVELOPE" then p_sp $ p_envelope (fun x -> k $ `Envelope x) $ d else
      if a -- "INTERNALDATE" then p_sp $ p_date_time (fun d t -> k $ `Internal_date (d, t)) $ d else
      if a -- "RFC822.HEADER" then p_sp $ p_nstring (fun x -> k $ `Rfc822_header x) $ d else
      if a -- "RFC822.TEXT" then p_sp $ p_nstring (fun x -> k $ `Rfc822_text x) $ d else
      if a -- "RFC822.SIZE" then p_sp $ p_uint (fun x -> k $ `Rfc822_size x) $ d else
      if a -- "RFC822" then p_sp $ p_nstring (fun x -> k $ `Rfc822 x) $ d else
      if a -- "BODYSTRUCTURE" then p_sp $ p_body (fun x -> k $ `Body_structure x) $ d else
      if a -- "BODY" then
        let orig k d =
          if cur d = '<' then readc $ p_uint (fun n -> p_ch '>' $ k (Some n)) $ d else
          k None d
        in
        if cur d = ' ' then readc $ p_body (fun x -> k $ `Body x) $ d else
        p_section (fun s -> orig (fun n -> p_sp $ p_nstring (fun x -> k $ `Body_section (s, n, x)))) d
      else
      if a -- "UID" then p_sp $ p_uint32 (fun x -> k $ `Uid x) $ d else
      if a -- "MODSEQ" then p_sp $ p_ch '(' (p_uint64 (fun n -> p_ch ')' $ k (`Modseq n))) $ d else
      if a -- "X-GM-MSGID" then p_sp $ p_uint64 (fun n -> k $ `Gm_msgid n) $ d else
      if a -- "X-GM-THRID" then p_sp $ p_uint64 (fun n -> k $ `Gm_thrid n) $ d else
      if a -- "X-GM-LABELS" then p_sp $ p_list p_astring (fun xs -> k $ `Gm_labels xs) $ d else
      err_unexpected_s a d
    end d

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

  let p_response_data_t k d =
    p_atom begin fun a d ->
      if a -- "OK" then p_sp $ p_resp_text (fun c t -> k $ `Ok (c, t)) $ d else
      if a -- "NO" then p_sp $ p_resp_text (fun c t -> k $ `No (c, t)) $ d else
      if a -- "BAD" then p_sp $ p_resp_text (fun c t -> k $ `Bad (c, t)) $ d else
      if a -- "BYE" then p_sp $ p_resp_text (fun c t -> k $ `Bye (c, t)) $ d else
      if a -- "FLAGS" then p_sp $ p_list p_flag (fun xs -> k $ `Flags xs) $ d else
      if a -- "LIST" then p_sp $ p_mailbox_list (fun xs c m -> k $ `List (xs, c, m)) $ d else
      if a -- "LSUB" then p_sp $ p_mailbox_list (fun xs c m -> k $ `Lsub (xs, c, m)) $ d else
      if a -- "SEARCH" then
        let rec nxt acc d =
          let nxt d =
            if cur d = '(' then
              readc $ p_atomf "MODSEQ"
                (p_sp (p_uint64 (fun n -> p_ch ')' $ k (`Search (List.rev acc, Some n))))) $ d
            else
            p_uint32 (fun n -> nxt (n :: acc)) d
          in
          if cur d = ' ' then readc nxt d else
          k $ `Search (List.rev acc, None) $ d
        in
        nxt [] d else
      if a -- "STATUS" then
        let nxt m d = p_sp $ p_list p_status_att (fun a -> k $ `Status (m, a)) $ d in
        p_sp $ p_mailbox nxt $ d else
      if a -- "CAPABILITY" then p_sep p_cap (fun xs -> k $ `Capability xs) d else
      if a -- "PREAUTH" then p_sp $ p_resp_text (fun c t -> k $ `Preauth (c, t)) $ d else
      if a -- "VANISHED" then
        let nxt d =
          if cur d = '(' then
            readc $ p_atomf "EARLIER" (p_ch ')' $ p_sp (p_set (fun s -> k $ `Vanished_earlier s))) $ d
          else
          p_set (fun s -> k $ `Vanished s) d
        in
        p_sp nxt d
      else
      if a -- "ENABLED" then p_sep p_cap (fun xs -> k $ `Enabled xs) d else
      err_unexpected_s a d
    end d

  let p_response_data_n k d =
    let nxt n a d =
      if a -- "EXPUNGE" then k $ `Expunge n $ d else
      if a -- "FETCH" then p_sp $ p_list1 p_msg_att (fun a -> k $ `Fetch (n, a)) $ d else
      if a -- "EXISTS" then k $ `Exists (Uint32.to_int n) $ d else
      if a -- "RECENT" then k $ `Recent (Uint32.to_int n) $ d else
      err_unexpected_s a d
    in
    p_t2 p_uint32 p_atom nxt d (* FIXME uint vs uint32 *)

  let p_untagged k d = (* '*' was eaten *)
    let nxt d = if is_digit $ cur d then p_response_data_n k d else p_response_data_t k d in
    p_sp nxt d

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

  let p_tag k d =
    p_while1 is_tag_char k d

  let p_tagged k d =
    p_t2 p_tag p_resp_cond_state k d

  let p_continue_req k d = (* '+' was eaten *)
    if cur d = ' ' then readc $ p_text k $ d else
    p_text k d

  let rec p_response k d =
    let k x d = p_crlf $ k x $ d in
    if cur d = '+' then readc $ p_continue_req (fun s -> k $ `Cont s) $ d else
    if cur d = '*' then readc $ p_untagged k $ d else
    p_tagged (fun t s -> k $ `Tagged (t, s)) d

  let rec p_response_ d =
    let ret x d = d.k <- p_response_; ret x d in
    readc (p_response ret) d

  let decoder src =
    let i, i_pos, i_max = match src with
      | `String s -> s, 0, String.length s
      | `Manual -> "", 0, 0
      | `Channel _ -> Bytes.create io_buffer_size, 0, 0
    in
    { src = (src :> src); i; i_pos; i_max;
      buf = Buffer.create 4096; k = p_response_ }

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
  | `Body_section of [ `Peek | `Look ] * section * (int * int) option
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

type command_ordinary =
  [ `Login of string * string
  | `Capability
  | `Create of string
  | `Delete of string
  | `Rename of string * string
  | `Logout
  | `Noop
  | `Subscribe of string
  | `Unsubscribe of string
  | `List of string * string
  | `Lsub of string * string
  | `Status of string * status_query list
  | `Copy of [ `Uid | `Seq ] * eset * string
  | `Check
  | `Close
  | `Expunge
  | `Search of [ `Uid | `Seq ] * search_key
  | `Select of [ `Condstore | `Plain ] * string
  | `Examine of [ `Condstore | `Plain ] * string
  | `Append of string * flag list option * (date * time) option * string
  | `Fetch of [ `Uid | `Seq ] * eset *
              [ `All | `Fast | `Full | `List of fetch_query list ] *
              [ `Changed_since of uint64 | `Changed_since_vanished of uint64 | `All ]
  | `Store of [ `Uid | `Seq ] * eset * [ `Silent | `Loud ] *
              [ `Unchanged_since of uint64 | `All ] *
              [ `Add | `Set | `Remove ] * [ `Flags of flag list | `Labels of string list ]
  | `Enable of capability list ]

type command =
  [ command_ordinary

  | `Idle of bool ref

  | `Authenticate of authenticator ]

let login u p = `Login (u, p)
let capability = `Capability
let create m = `Create m
let delete m = `Delete m
let rename m1 m2 = `Rename (m1, m2)
let logout = `Logout
let noop = `Noop
let subscribe m = `Subscribe m
let unsubscribe m = `Unsubscribe m
let list ?(ref = "") s = `List (ref, s)
let lsub ?(ref = "") s = `Lsub (ref, s)
let status m att = `Status (m, att)
let copy ?(uid = true) s m = let uid = if uid then `Uid else `Seq in `Copy (uid, s, m)
let check = `Check
let close = `Close
let expunge = `Expunge
let search ?(uid = true) sk = let uid = if uid then `Uid else `Seq in `Search (uid, sk)
let select ?(condstore = false) m =
  let condstore = if condstore then `Condstore else `Plain in `Select (condstore, m)
let examine ?(condstore = false) m =
  let condstore = if condstore then `Condstore else `Plain in `Examine (condstore, m)

let append m ?flags data =
  `Append (m, flags, None, data)

let fetch_aux ~uid ~changed ~vanished set att =
  let uid = if uid then `Uid else `Seq in
  let changed = match changed, vanished with
    | None, true -> invalid_arg "VANISHED requires CHANGEDSINCE"
    | None, false -> `All
    | Some m, true -> `Changed_since_vanished m
    | Some m, false -> `Changed_since m
  in
  `Fetch (uid, set, att, changed)
let fetch ?(uid = true) ?changed ?(vanished = false) set att =
  fetch_aux ~uid ~changed ~vanished set (`List att)
let fetch_fast ?(uid = true) ?changed ?(vanished = false) set =
  fetch_aux ~uid ~changed ~vanished set `Fast
let fetch_full ?(uid = true) ?changed ?(vanished = false) set =
  fetch_aux ~uid ~changed ~vanished set `Full
let fetch_all ?(uid = true) ?changed ?(vanished = false) set =
  fetch_aux ~uid ~changed ~vanished set `All

let store_aux ~uid ~silent ~unchanged mode set att =
  let uid = if uid then `Uid else `Seq in
  let unchanged = match unchanged with None -> `All | Some m -> `Unchanged_since m in
  let silent = if silent then `Silent else `Loud in
  `Store (uid, set, silent, unchanged, mode, att)
let store_add_flags ?(uid = true) ?(silent = false) ?unchanged set flags =
  store_aux ~uid ~silent ~unchanged `Add set (`Flags flags)
let store_set_flags ?(uid = true) ?(silent = false) ?unchanged set flags =
  store_aux ~uid ~silent ~unchanged `Set set (`Flags flags)
let store_remove_flags ?(uid = true) ?(silent = false) ?unchanged set flags =
  store_aux ~uid ~silent ~unchanged `Remove set (`Flags flags)
let store_add_labels ?(uid = true) ?(silent = false) ?unchanged set labels =
  store_aux ~uid ~silent ~unchanged `Add set (`Labels labels)
let store_set_labels ?(uid = true) ?(silent = false) ?unchanged set labels =
  store_aux ~uid ~silent ~unchanged `Set set (`Labels labels)
let store_remove_labels ?(uid = true) ?(silent = false) ?unchanged set labels =
  store_aux ~uid ~silent ~unchanged `Remove set (`Labels labels)

let enable caps = `Enable caps

let authenticate a = `Authenticate a

let idle () = let stop = ref false in `Idle stop, lazy (stop := true)

module E = struct

  (* Encoder *)

  type command_lexeme =
    [ command_ordinary
    | `Idle
    | `Authenticate of string ]

  type dst =
    [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

  type encode =
    [ `Cmd of string * command_lexeme | `Idle_done | `Auth_step of string | `Auth_error | `Await ]

  let pp_encode : _ -> [< encode] -> _ = fun ppf e -> match e with
    | `Cmd _ -> pp ppf "`Cmd" (* FIXME *)
    | `Await -> pp ppf "`Await"
    | `Idle_done -> pp ppf "`Idle_done"
    | `Auth_step _ -> pp ppf "`Auth_step"
    | `Auth_error -> pp ppf "`Auth_error"

  type encoder =
    { dst : dst;
      mutable o : string;
      mutable o_pos : int;
      mutable o_max : int;
      mutable k : encoder -> encode -> [ `Ok | `Partial | `Wait_for_cont ] }

  let dst_rem e = e.o_max - e.o_pos

  let dst e s j l =
    if j < 0 || l < 0 || j + l > String.length s then invalid_arg "bounds";
    e.o <- s; e.o_pos <- j; e.o_max <- j + l

  let partial k e = function
    | `Await -> k e
    | `Cmd _ | `Idle_done | `Auth_step _ | `Auth_error ->
        invalid_arg "cannot encode now, use `Await first"

  let flush k e = match e.dst with
    | `Manual -> if e.o_pos > 0 || e.o_max = 0 then (e.k <- partial k; `Partial) else k e
    | `Buffer b -> Buffer.add_substring b e.o 0 e.o_pos; e.o_pos <- 0; k e
    | `Channel oc -> output oc e.o 0 e.o_pos; e.o_pos <- 0; k e

  let wait_for_cont k e =
    (e.k <- partial k; `Wait_for_cont)

  let rec writes s k e =
    let rec loop j l e =
      let rem = dst_rem e in
      let len = if l > rem then rem else l in
      String.unsafe_blit s j e.o e.o_pos len;
      e.o_pos <- e.o_pos + len;
      if len < l then flush (loop (j + len) (l - len)) e else k e
    in
    loop 0 (String.length s) e

  let w s k e = writes s k e

  let classify_string str =
    let literal = function
      | '\x80' .. '\xFF' | '\r' | '\n' -> true
      | _ -> false
    in
    let quotes = function
      | '(' | ')' | '{' | ' ' | '\x00' .. '\x1F' | '\x7F'
      | '%' | '*' | '\"' | '\\' -> true
      | _ -> false
    in
    let needs f s =
      let rec loop i =
        if i >= String.length s then false else
        if f s.[i] then true else
        loop (i+1)
      in
      loop 0
    in
    if str = "" then `Quoted else
    if needs literal str then `Literal else
    if needs quotes str then `Quoted else
    `Raw

  let w_literal x k e =
    w (Printf.sprintf "{%d}\r\n" (String.length x)) (flush (wait_for_cont (w x k))) e

  let w_string x k e = match classify_string x with
    | `Raw     -> w x k e
    | `Quoted  -> w "\"" (w x (w "\"" k)) e
    | `Literal -> w_literal x k e

  let w_sep ?(sep = ' ') f l k e =
    let rec loop xs e = match xs with
      | [] -> k e
      | x :: [] -> f x k e
      | x :: xs -> f x (w (String.make 1 sep) (loop xs)) e
    in
    loop l e

  let (&) x y k e = x (w " " (y k)) e
  let ($) x y k e = x (y k) e

  let w_crlf k e =
    w "\r\n" k e

  let w_status_att = function
    | `Messages -> w "MESSAGES"
    | `Recent -> w "RECENT"
    | `Uid_next -> w "UIDNEXT"
    | `Uid_validity -> w "UIDVALIDITY"
    | `Unseen -> w "UNSEEN"
    | `Highest_modseq -> w "HIGHESTMODSEQ"

  let w_int n = w (string_of_int n)
  let w_uint32 m = w (Uint32.to_string m)
  let w_uint64 m = w (Uint64.to_string m)

  let w_label l =
    w_string (Mutf7.encode l)

  let w_list f l k e =
    let rec loop xs e = match xs with
      | [] -> w ")" k e
      | [x] -> f x (w ")" k) e
      | x :: xs -> f x (w " " (loop xs)) e
    in
    w "(" (loop l) e

  let w_eset s k e =
    let f = function
      | (lo, Some hi) when lo = hi -> w_uint32 lo
      | (lo, Some hi) -> w_uint32 lo $ w ":" $ w_uint32 hi
      | (lo, None) -> w_uint32 lo $ w ":*"
    in
    w_sep ~sep:',' f s k e

  let w_mailbox s =
    w_string (Mutf7.encode s)

  let months =
    [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
       "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

  let w_date d =
    w (Printf.sprintf "%d-%s-%4d" d.day months.(d.month) d.year)

  let w_p f x = w "(" $ f x $ w ")"

  let rec w_search_key : [< search_key] -> _ = function
    | `Seq s -> w_eset s
    | `All -> w "ALL"
    | `Answered -> w "ANSWERED"
    | `Bcc s -> w "BCC" & w_string s
    | `Before d -> w "BEFORE" & w_date d
    | `Body s -> w "BODY" & w_string s
    | `Cc s -> w "CC" & w_string s
    | `Deleted -> w "DELETED"
    | `Draft -> w "DRAFT"
    | `Flagged -> w "FLAGGED"
    | `From s -> w "FROM" & w_string s
    | `Header (s1, s2) -> w "HEADER" & w_string s1 & w_string s2
    | `Keyword s -> w "KEYWORD" & w_string s
    | `Larger n -> w "LARGER" & w_int n
    | `New -> w "NEW"
    | `Old -> w "OLD"
    | `On d -> w "ON" & w_date d
    | `Recent -> w "RECENT"
    | `Seen -> w "SEEN"
    | `Sent_before d -> w "SENTBEFORE" & w_date d
    | `Sent_on d -> w "SENT_ON" & w_date d
    | `Sent_since d -> w "SENTSINCE" & w_date d
    | `Since d -> w "SINCE" & w_date d
    | `Smaller n -> w "SMALLER" & w_int n
    | `Subject s -> w "SUBJECT" & w_string s
    | `Text s -> w "TEXT" & w_string s
    | `To s -> w "TO" & w_string s
    | `Uid s -> w "UID" & w_eset s
    | `Unanswered -> w "UNANSWERED"
    | `Undeleted -> w "UNDELETED"
    | `Undraft -> w "UNDRAFT"
    | `Unflagged -> w "UNFLAGGED"
    | `Unkeyword s -> w "UNKEYWORD" & w_string s
    | `Unseen -> w "UNSEEN"
    | `Not sk -> w "NOT" & w_p w_search_key sk
    | `Or (sk1, sk2) -> w "OR" & w_p w_search_key sk1 & w_p w_search_key sk2
    | `And (sk1, sk2) -> w_p w_search_key sk1 & w_p w_search_key sk2
    | `Modseq m -> w "MODSEQ" & w_uint64 m
    | `Gm_raw s -> w "X-GM-RAW" & w_string s
    | `Gm_msgid m -> w "X-GM-MSGID" & w_uint64 m
    | `Gm_thrid m -> w "X-GM-THRID" & w_uint64 m
    | `Gm_labels l -> w "X-GM-LABELS" & w_list w_string l

  let rec w_section : [< section] -> _ = function
    | `Header -> w "HEADER"
    | `Header_fields l -> w "HEADER.FIELDS" & w_list w l
    | `Header_fields_not l -> w "HEADER.FIELDS.NOT" & w_list w l
    | `Text -> w "TEXT"
    | `Mime -> w "MIME"
    | `Part (n, s) -> w_int n $ w "." $ w_section s
    | `All -> w ""

  let w_fetch_att : [< fetch_query] -> _ = function
    | `Envelope -> w "ENVELOPE"
    | `Internal_date -> w "INTERNALDATE"
    | `Rfc822_header -> w "RFC822.HEADER"
    | `Rfc822_text -> w "RFC822.TEXT"
    | `Rfc822_size -> w "RFC822.SIZE"
    | `Rfc822 -> w "RFC822"
    | `Body -> w "BODY"
    | `Body_section (peek, s, partial) ->
        let cmd = match peek with `Peek -> "BODY.PEEK" | `Look -> "BODY" in
        let partial = match partial with
          | None -> w ""
          | Some (n, l) -> w "<" $ w_int n $ w "." $ w_int l $ w ">"
        in
        w cmd $ w "[" $ w_section s $ w "]" $ partial
    | `Body_structure -> w "BODYSTRUCTURE"
    | `Uid -> w "UID"
    | `Flags -> w "FLAGS"

  let w_flag = function
    | `Answered -> w "\\Answered"
    | `Flagged -> w "\\Flagged"
    | `Deleted -> w "\\Deleted"
    | `Seen -> w "\\Seen"
    | `Draft -> w "\\Draft"
    | `Keyword s -> w s
    | `Extension s -> w ("\\" ^ s)

  let w_tagged : [< command_lexeme] -> _ = function
    | `Capability -> w "CAPABILITY"
    | `Login (u, p) -> w "LOGIN" & w_string u & w_string p
    | `Logout -> w "LOGOUT"
    | `Noop -> w "NOOP"
    | `Subscribe m -> w "SUBSCRIBE" & w_mailbox m
    | `Unsubscribe m -> w "UNSUBSCRIBE" & w_mailbox m
    | `List (m, s) -> w "LIST" & w_mailbox m & w_string s
    | `Lsub (m, s) -> w "LSUB" & w_mailbox m & w_string s
    | `Select (`Plain, m) -> w "SELECT" & w_mailbox m
    | `Select (`Condstore, m) -> w "SELECT" & w_mailbox m & w "(CONDSTORE)"
    | `Examine (`Plain, m) -> w "EXAMINE" & w_mailbox m
    | `Examine (`Condstore, m) -> w "EXAMINE" & w_mailbox m & w "(CONDSTORE)"
    | `Append (m, flags, internaldate, data) ->
        let flags = match flags with None -> w "" | Some f -> w " " $ w_list w_flag f in
        (* TODO internaldate *)
        w "APPEND" & w_mailbox m $ flags & w_literal data
    | `Create m -> w "CREATE" & w_mailbox m
    | `Delete m -> w "DELETE" & w_mailbox m
    | `Rename (m1, m2) -> w "RENAME" & w_mailbox m1 & w_mailbox m2
    | `Status (m, att) -> w "STATUS" & w_mailbox m & w_list w_status_att att
    | `Close -> w "CLOSE"
    | `Check -> w "CHECK"
    | `Expunge -> w "EXPUNGE"
    | `Fetch (uid, set, att, changed_since) ->
        let changed_since = match changed_since with
          | `All -> w ""
          | `Changed_since m ->
              w " (" $ w "CHANGEDSINCE" & w_uint64 m $ w ")"
          | `Changed_since_vanished m ->
              w " (" $ w "CHANGEDSINCE" & w_uint64 m & w "VANISHED" $ w ")"
        in
        let cmd = match uid with `Seq -> "FETCH" | `Uid -> "UID FETCH" in
        let att = match att with
          | `Fast -> w "FAST"
          | `Full -> w "FULL"
          | `All -> w "ALL"
          | `List [x] -> w_fetch_att x
          | `List l -> w_list w_fetch_att l
        in
        w cmd & w_eset set & att $ changed_since
    | `Store (uid, set, silent, unchanged_since, mode, att) ->
        let mode = match mode with `Add -> "+" | `Set -> "" | `Remove -> "-" in
        let silent = match silent with `Silent -> ".SILENT" | `Loud -> "" in
        let base = match att with
          | `Flags _ -> mode ^ "FLAGS" ^ silent
          | `Labels _ -> mode ^ "X-GM-LABELS" ^ silent
        in
        let att = match att with
          | `Flags flags -> w_list w_flag flags
          | `Labels labels -> w_list w_label labels
        in
        let unchanged_since = match unchanged_since with
          | `All -> w ""
          | `Unchanged_since m -> w " (" $ w "UNCHANGEDSINCE" & w_uint64 m $ w ")"
        in
        let cmd = match uid with `Seq -> "STORE" | `Uid -> "UID STORE" in
        w cmd & w_eset set $ unchanged_since & w base & att
    | `Copy (uid, set, m) ->
        let cmd = match uid with `Seq -> "COPY" | `Uid -> "UID COPY" in
        w cmd & w_eset set & w_mailbox m
    | `Search (uid, sk) ->
        let cmd = match uid with `Seq -> "SEARCH" | `Uid -> "UID SEARCH" in
        w cmd & w_search_key sk
    | `Enable c ->
        w "ENABLE" & w_sep (fun x -> w (string_of_capability x)) c
    | `Idle -> w "IDLE"
    | `Authenticate name ->
        w "AUTHENTICATE" & w name

  let rec encode_ e = function
    | `Await -> `Ok
    | `Cmd (tag, x) -> w tag (w " " (w_tagged x (w_crlf (flush encode_loop)))) e
    | `Idle_done -> w "DONE" (w_crlf (flush encode_loop)) e
    | `Auth_step data -> w data (w_crlf (flush encode_loop)) e
    | `Auth_error -> w "*" (w_crlf (flush encode_loop)) e

  and encode_loop e = e.k <- encode_; `Ok

  let encoder dst =
    let o, o_pos, o_max = match dst with
      | `Manual -> "", 0, 0
      | `Buffer _
      | `Channel _ -> Bytes.create io_buffer_size, 0, io_buffer_size
    in
    { dst = (dst :> dst); o; o_pos; o_max; k = encode_ }

  let encode e v = e.k e v
end

(* Running commands *)

type error =
  [ `Incorrect_tag of string * string
  | `Decode_error of D.error * string * int
  | `Unexpected_cont
  | `Bad_greeting
  | `Auth_error of string
  | `Bad of code * string
  | `No of code * string ]

let pp_error ppf : error -> _ = function
  | `Incorrect_tag (exp, tag) -> pp ppf "@[Incorrect@ tag@ %S,@ should@ be@ %S@]" tag exp
  | `Decode_error e -> pp ppf "@[Decode@ error:@ %a@]" D.pp_error e
  | `Unexpected_cont -> pp ppf "@[Unexpected continuation request@]"
  | `Bad_greeting -> pp ppf "@[Bad greeting@]"
  | `Auth_error s -> pp ppf "@[Authentication error: %s@]" s
  | `Bad (c, t) -> pp ppf "@[BAD:@ %a@ %S@]" pp_code c t
  | `No (c, t) -> pp ppf "@[NO:@ %a@ %S@]" pp_code c t

type result =
  [ `Untagged of untagged
  | `Ok of code * string
  | `Error of error
  | `Await_src
  | `Await_dst ]

type connection =
  { e : E.encoder;
    d : D.decoder;
    mutable idle_stop : bool ref; (* whether the user has signaled to send DONE *)
    mutable idling : bool; (* whether we are idling *)
    mutable tag : int;
    mutable k : connection -> [ `Cmd of command | `Await ] -> result }

let src c = D.src c.d
let dst c = E.dst c.e
let dst_rem c = E.dst_rem c.e

let ret v k c = c.k <- k; v
let err e k c = ret (`Error e) k c
let run c = c.k c
let fatal _ = invalid_arg "connection should not be used"

let rec await k c = function
  | `Await when c.idling && !(c.idle_stop) ->
      c.idle_stop := false;
      encode `Idle_done k c
  | `Await -> k c
  | `Cmd _ -> invalid_arg "can't send command now, try `Await"

(* let rec decode_to_cont k c = (\* FIXME signal error ? *\) *)
  (* decode false (function `Cont _ -> k | _ -> decode_to_cont k) c *)

and cont_req k r c = match r with
  | `Cont _ -> k c
  | _ -> decode (cont_req k) c

and encode x k c = match E.encode c.e x with
  | `Partial ->
      ret `Await_dst (await (encode `Await k)) c
  | `Wait_for_cont ->
      decode (cont_req (encode `Await k)) c
      (* decode_to_cont (encode `Await k) c *)
  | `Ok -> k c

and decode (k : _ -> connection -> _) c = match D.decode c.d with
  | `Ok x -> k x c
  | `Await -> ret `Await_src (await (decode k)) c
  | `Error e -> err (`Decode_error e) fatal c (* FIXME resume on the next line of input *)

let rec h_tagged tag r c =
  let cur = string_of_int c.tag in
  c.tag <- c.tag + 1;
  if tag <> cur then err (`Incorrect_tag (cur, tag)) fatal c else (* FIXME alert the user and continue ? *)
  match r with
  | `Ok _ as ok -> ret ok h_response_loop c
  | (`Bad _ | `No _) as e -> err e h_response_loop c

and h_response r c = match r with
  | #untagged as r -> ret (`Untagged r) (await (decode h_response)) c
  | `Cont _ -> err `Unexpected_cont fatal c (* FIXME ignore and continue ? *)
  | `Tagged (g, r) -> h_tagged g r c

and h_idle_response r c = match r with
  | #untagged as r -> ret (`Untagged r) (await (decode h_idle_response)) c
  | `Cont _ when not c.idling -> c.idling <- true; decode h_idle_response c
  | `Cont _ -> err `Unexpected_cont fatal c
  | `Tagged (g, r) -> c.idling <- false; h_tagged g r c

and h_authenticate auth r c = match r with
  | #untagged as r -> ret (`Untagged r) (await (decode (h_authenticate auth))) c
  | `Tagged (g, r) -> h_tagged g r c
  | `Cont data ->
      begin match auth.step (B64.decode data) with
      | `Ok data ->
          let data = B64.encode ~pad:true data in
          encode (`Auth_step data) (decode (h_authenticate auth)) c
      | `Error s ->
          encode `Auth_error (err (`Auth_error s) (await (decode (h_authenticate auth)))) c
      end

and h_response_loop c = function
  | `Cmd (`Authenticate a) ->
      encode (`Cmd (string_of_int c.tag, `Authenticate a.name))
        (decode (h_authenticate a)) c
  | `Cmd (`Idle stop) ->
      c.idle_stop <- stop;
      encode (`Cmd (string_of_int c.tag, `Idle)) (decode h_idle_response) c
  | `Cmd (#command_ordinary as cmd) ->
      encode (`Cmd (string_of_int c.tag, cmd)) (decode h_response) c
  | `Await -> `Ok (`None, "")

let h_greetings r c = match r with
  | `Ok _ as ok -> ret ok h_response_loop c
  | _ -> err `Bad_greeting fatal c (* FIXME continue until [`Ok] ? *)

let connection () =
  { e = E.encoder `Manual; d = D.decoder `Manual; idling = false;
    idle_stop = ref false; tag = 0; k = await (decode h_greetings) }
