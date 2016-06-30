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

open Lwt.Infix

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

  exception Error of [ `Error of error * string * int ]

  (* let err e d = `Error (e, d.i, d.i_pos) *)
  (* let err_expected c d = err (`Expected_char c) d *)
  (* let err_expected_s s d = err (`Expected_string s) d *)
  (* let err_unexpected_s a d = err (`Unexpected_string a) d *)
  (* let err_unexpected c d = err (`Unexpected_char c) d *)
  (* let err_quoted c d = err (`Illegal_char c) d *)
  (* let err_unexpected_eoi d = err `Unexpected_eoi d *)

  module Sub = Astring.String.Sub

  let re = Str.regexp "{\\([0-9]+\\)}$"

  let readline ic =
    let buf = Buffer.create 50 in
    let buf2 = Bytes.create 101 in
    let rec loop () =
      Lwt_io.read_line ic >>= fun line ->
      Buffer.add_string buf line;
      Buffer.add_string buf "\r\n";
      if Str.string_match re line 0 then
        let len = int_of_string (Str.matched_group 1 line) in
        let rec aux rem =
          if rem <= 0 then
            loop ()
          else begin
            Lwt_io.read_into ic buf2 0 rem >>= fun n ->
            Buffer.add_subbytes buf buf2 0 n;
            aux (rem - n)
          end
        in
        aux len
      else
        Lwt.return (Buffer.contents buf)
    in
    loop ()

  let ch c s =
    match Sub.head s with
    | Some c' when c = c' ->
        Sub.tail s
    | _ ->
        failwith "ch"

  let sp s = ch ' ' s

  let while1 f s =
    let l, r = Sub.span ~min:1 ~sat:f s in
    if Sub.is_empty l then
      failwith "while1"
    else
      Sub.to_string l, r
        (* raise (Error (err_unexpected (cur d) d)) *)

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
    | '(' | ')' | '{' | ' ' | '\x00' .. '\x1F' | '\x7F' | '%' | '*' | '"' | '\\' | ']' -> false
    | '\x01' .. '\x7F' -> true
    | _ -> false

  let atom s =
    let a, s = Sub.span ~min:1 ~sat:is_atom_char s in
    if Sub.is_empty a then failwith "atom";
    Sub.to_string a, s

  let atomf str s =
    let a, s = atom s in
    if String.uppercase_ascii a <> String.uppercase_ascii str then
      failwith "atomf" (* raise (Error (err_expected_s s d)) *)
    else
      s

  let triple f1 f2 f3 s =
    let x1, s = f1 s in
    let x2, s = f2 (sp s) in
    let x3, s = f3 (sp s) in
    (x1, x2, x3), s

  let sep f s =
    let rec loop acc s =
      match Sub.head s with
      | Some ' ' ->
          let x, s = f (Sub.tail s) in
          loop (x :: acc) s
      | _ ->
          List.rev acc, s
    in
    loop [] s

  let list1 f s =
    let rec loop acc s =
      match Sub.head s with
      | Some ' ' ->
          let x, s = f (Sub.tail s) in
          loop (x :: acc) s
      | Some ')' ->
          List.rev acc, Sub.tail s
      | _ ->
          failwith "list1" (* err_unexpected (cur d) d *)
    in
    let x, s = f (ch '(' s) in
    loop [x] s

  let list_gen delim f s =
    let rec loop acc s =
      match Sub.head s with
      | Some ')' ->
          List.rev acc, Sub.tail s
      | Some c ->
          let s =
            match delim with
            | None -> s
            | Some c' when c = c' -> Sub.tail s
            | Some c -> failwith "list_gen" (* raise (Error (err_unexpected c d)) *)
          in
          let x, s = f s in
          loop (x :: acc) s
      | None ->
          failwith "eof"
    in
    match Sub.head s with
    | Some c when is_atom_char c ->
        let s = atomf "NIL" s in
        [], s
    | _ ->
        begin match Sub.head (ch '(' s) with
        | Some ')' ->
            [], Sub.tail s
        | _ ->
            let x, s = f s in
            loop [x] s
        end

  let list f s =
    list_gen (Some ' ') f s

(*
   CR             =  %x0D
                                  ; carriage return

   LF             =  %x0A
                                  ; linefeed

   CRLF           =  CR LF
                          ; Internet standard newline
*)

  let crlf s =
    if Sub.is_prefix (Sub.v "\r\n") s then
      Sub.tail (Sub.tail s)
    else
      failwith "crlf"

(*
   number          = 1*DIGIT
                       ; Unsigned 32-bit integer
                       ; (0 <= n < 4,294,967,296)

   nz-number       = digit-nz *DIGIT
                       ; Non-zero unsigned 32-bit integer
                       ; (0 < n < 4,294,967,296)
*)

  let is_digit = function '0' .. '9' -> true | _ -> false
  let uint s = let n, s = while1 is_digit s in int_of_string n, s
  let uint32 s = let n, s = while1 is_digit s in Uint32.of_string n, s
  let uint64 s = let n, s = while1 is_digit s in Uint64.of_string n, s

(*
   quoted          = DQUOTE *QUOTED-CHAR DQUOTE

   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
                     '\\' quoted-specials
*)

  let quoted s = (* "" was eaten *)
    let buf = Buffer.create 0 in
    let rec loop s =
      match Sub.head s with
      | Some ('\r' | '\n' as c) ->
          failwith "quoted" (* raise (Error (err_quoted c d)) *)
      | Some '"' ->
          Buffer.contents buf, Sub.tail s
      | Some '\\' ->
          begin match Sub.head (Sub.tail s) with
          | Some ('"' | '\\' as c) ->
              Buffer.add_char buf c;
              loop (Sub.tail s)
          | None ->
              failwith "Eof"
          | Some _ ->
              failwith "quoted"
              (* raise (Error (err_unexpected c d)) *)
          end
      | Some ('\x01' .. '\x7F' as c) ->
          Buffer.add_char buf c;
          loop (Sub.tail s)
      | Some c ->
          failwith "quoted"
          (* raise (Error (err_unexpected c d)) *)
      | None ->
          failwith "eof"
    in
    loop s

(*
   literal         = "{" number "}" CRLF *CHAR8
                       ; Number represents the number of CHAR8s

   string          = quoted / literal
*)

  let string s =
    match Sub.head s with
    | Some '"' ->
        quoted (Sub.tail s)
    | Some '{' ->
        let n, s = uint (Sub.tail s) in
        let s = crlf (ch '}' s) in
        let l, r = Sub.span ~min:n ~max:n s in
        Sub.to_string l, r
    | _ ->
        failwith "string"
        (* err_unexpected c d *)

(*
   ASTRING-CHAR   = ATOM-CHAR / resp-specials

   astring         = 1*ASTRING-CHAR / string
*)

  let is_astring_char c =
    is_atom_char c || c = ']'

  let astring s =
    match Sub.head s with
    | Some c when is_astring_char c ->
        let l, r = Sub.span ~min:1 ~sat:is_astring_char s in
        Sub.to_string l, r
    | _ ->
        string s

(*
   TEXT-CHAR       = <any CHAR except CR and LF>

   text            = 1*TEXT-CHAR
*)

  let is_text_char = function
    | '\r' | '\n' -> false
    | '\x01' .. '\x7F' -> true
    | _ -> false

  let text s =
    match Sub.head s with
    | Some c when is_text_char c ->
        while1 is_text_char s
    | _ ->
        "", s (* allow empty texts for greater tolerance *)

(*
   nil             = "NIL"

   nstring         = string / nil
*)

  let nstring s =
    match Sub.head s with
    | Some c when is_atom_char c ->
        let s = atomf "NIL" s in
        None, s
    | _ ->
        let str, s = string s in
        Some str, s

  let nstring' s =
    let x, s = nstring s in
    match x with
    | Some str -> str, s
    | None -> "", s

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

  let flag s =
    match Sub.head s with
    | Some '\\' ->
        let a, s = atom (Sub.tail s) in
        let x =
          match String.capitalize_ascii a with
          | "Answered" -> `Answered
          | "Flagged" -> `Flagged
          | "Deleted" -> `Deleted
          | "Seen" -> `Seen
          | "Draft" -> `Draft
          | _ -> `Extension a
        in
        x, s
    | _ ->
        let a, s = atom s in
        `Keyword a, s

(*
   flag-fetch      = flag / "\Recent"
*)

  let flag_fetch s =
    let fl =
      match Sub.head s with
      | Some '\\' ->
          let a, s = atom (Sub.tail s) in
          begin match String.capitalize_ascii a with
          | "Answered" -> `Answered
          | "Flagged" -> `Flagged
          | "Deleted" -> `Deleted
          | "Seen" -> `Seen
          | "Draft" -> `Draft
          | "Recent" -> `Recent
          | _ -> `Extension a
          end
      | _ ->
          `Keyword (atom s)
    in
    fl, s

(*
   flag-perm       = flag / "\*"
*)

  let flag_perm s =
    match Sub.head s with
    | Some '\\' ->
        begin match Sub.head (Sub.tail s) with
        | Some '*' ->
            let s = Sub.tail s in
            `All, s
        | _ ->
            let a, s = atom s in
            let res =
              match String.capitalize_ascii a with
              | "Answered" -> `Answered
              | "Flagged" -> `Flagged
              | "Deleted" -> `Deleted
              | "Seen" -> `Seen
              | "Draft" -> `Draft
              | _ -> `Extension a
            in
            res, s
        end
    | _ ->
        let a, s = atom s in
        `Keyword a, s

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
  let set s =
    let rg s =
      (* if cur d = '*' then err_illegal_range d else *)
      let n, s = uint32 s in
      match Sub.head s with
      | Some ':' ->
          let m, s = uint32 (Sub.tail s) in
          (n, m), s
      | _ ->
          (n, n), s
    in
    let rec loop acc s =
      match Sub.head s with
      | Some ',' ->
          let x, s = rg (Sub.tail s) in
          loop (x :: acc) s
      | _ ->
          List.rev acc, s
    in
    let x, s = rg s in
    loop [x] s


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

  let text_1 s =
    match Sub.head s with
    | Some c when is_text_other_char c ->
        let l, r = Sub.span ~min:1 ~sat:is_text_other_char s in
        Sub.to_string l, r
    | _ ->
        "", s (* We allow empty text_1 *)

  let capability s =
    let a, s = atom s in
    let cap =
      match String.uppercase_ascii a with
      | "COMPRESS=DEFLATE" -> `Compress_deflate
      | "CONDSTORE" -> `Condstore
      | "ENABLE" -> `Enable
      | "IDLE" -> `Idle
      | "LITERAL+" -> `Literal_plus
      | "NAMESPACE" -> `Namespace
      | "ID" -> `Id
      | "QRESYNC" -> `Qresync
      | "UIDPLUS" -> `Uid_plus
      | "UNSELECT" -> `Unselect
      | "XLIST" -> `Xlist
      | "AUTH=PLAIN" -> `Auth `Plain
      | "AUTH=LOGIN" -> `Auth `Login
      | "XOAUTH2" -> `Xoauth2
      | "X-GM-EXT-1" -> `Gmail
      | _ -> `Other a
    in
    cap, s

  let code s = (* '[' was eaten *)
    let a, s = atom s in
    match String.uppercase_ascii a with
    | "ALERT" ->
        `Alert, s
    | "BADCHARSET" ->
        begin match Sub.head s with
        | Some ' ' ->
            let xs, s = list1 astring (Sub.tail s) in
            `Bad_charset xs, s
        | _ ->
            `Bad_charset [], s
        end
    | "CAPABILITY" ->
        let xs = sep capability s in
        `Capability xs, s
    | "PARSE" ->
        `Parse, s
    | "PERMANENTFLAGS" ->
        let xs, s = list flag_perm (sp s) in
        `Permanent_flags xs, s
    | "READ-ONLY" ->
        `Read_only, s
    | "READ-WRITE" ->
        `Read_write, s
    | "TRYCREATE" ->
        `Try_create, s
    | "UIDNEXT" ->
        let n = uint32 (sp s) in
        `Uid_next n, s
    | "UIDVALIDITY" ->
        let n, s = uint32 (sp s) in
        `Uid_validity n, s
    | "UNSEEN" ->
        let n, s = uint32 (sp s) in
        `Unseen n, s
    | "CLOSED" ->
        `Closed, s
    | "HIGHESTMODSEQ" ->
        let n, s = uint64 (sp s) in
        `Highest_modseq n, s
    | "NOMODSEQ" ->
        `No_modseq, s
    | "MODIFIED" ->
        let set, s = set (sp s) in
        `Modified set, s
    | "APPENDUID" ->
        let n, s = uint32 (sp s) in
        let m, s = uint32 (sp s) in
        `Append_uid (n, m), s
    | "COPYUID" ->
        let n, s = uint32 (sp s) in
        let set1, s = set s in
        let set2, s = set s in
        `Copy_uid (n, set1, set2), s
    | "UIDNOTSTICKY" ->
        `Uid_not_sticky, s
    | "COMPRESSIONACTIVE" ->
        `Compression_active, s
    | "USEATTR" ->
        `Use_attr, s
    | _ ->
        begin match Sub.head s with
        | Some ' ' ->
            let x, s = text_1 (Sub.tail s) in
            `Other (a, Some x), s
        | _ ->
            `Other (a, None), s
        end

(*
   resp-text       = ["[" resp-text-code "]" SP] text
*)

  let resp_text s =
    match Sub.head s with
    | Some '[' ->
        let x, s = code (Sub.tail s) in
        begin match Sub.head (ch ']' s) with
        | Some ' ' ->
            let txt, s = text (Sub.tail s) in
            (x, txt), s
        | _ ->
            (x, ""), s
        end
    | _ ->
        let txt, s = text s in
        (`None, txt), s

(*
   resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
                       ; Status condition
*)

  let resp_cond_state s =
    let a, s = atom s in
    match String.uppercase_ascii a with
    | "OK" ->
        let (c, t), s = resp_text (sp s) in
        `Ok (c, t), s
    | "NO" ->
        let (c, t), s = resp_text (sp s) in
        `No (c, t), s
    | "BAD" ->
        let (c, t), s = resp_text (sp s) in
        `Bad (c, t), s
    | _ ->
        failwith "resp_cond" (* err_unexpected_s a d *)

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

  let mbx_flag s =
    let a, s = atom (ch '\\' s) in
    let flag =
      match String.capitalize_ascii a with
      | "Noselect" -> `Noselect
      | "Marked" -> `Marked
      | "Unmarked" -> `Unmarked
      | "Noinferiors" -> `Noinferiors
      | "HasChildren" -> `HasChildren
      | "HasNoChildren" -> `HasNoChildren
      | "All" -> `All
      | "Archive" -> `Archive
      | "Drafts" -> `Drafts
      | "Flagged" -> `Flagged
      | "Junk" -> `Junk
      | "Sent" -> `Sent
      | "Trash" -> `Trash
      | _ -> `Extension a
    in
    flag, s

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

  let mailbox s =
    let decode_mailbox_name s = try Mutf7.decode s with _ -> s in (* FIXME handle error *)
    let mbox, s = astring s in
    decode_mailbox_name mbox, s

(*
   QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
                     '\\' quoted-specials

   mailbox-list    = "(" [mbx-list-flags] ")" SP
                      (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
*)

  let quoted_char s =
    let is_qchar = function
      | '\r' | '\n' | '\\' | '"' -> false
      | '\x01' .. '\x7F' -> true
      | _ -> false
    in
    match Sub.head s with
    | Some '\\' ->
        begin match Sub.head (Sub.tail s) with
        | Some ('\\' | '"' as c) ->
            c, Sub.tail s
        | Some c ->
            failwith "quoted_char" (* raise (Error (err_unexpected c d)) *)
        | None ->
            failwith "eof"
        end
    | Some c when is_qchar c ->
        c, Sub.tail s
    | Some c ->
        (* raise (Error (err_unexpected c d)) *)
        failwith "quoted char"
    | None ->
        failwith "eof"

  let delim s =
    match Sub.head s with
    | Some '"' ->
        let c, s = quoted_char (Sub.tail s) in
        Some c, ch '"' s
    | _ ->
        let s = atomf "NIL" s in
        None, s

  let mailbox_list s =
    let x, s = list mbx_flag s in
    let y, s = delim (sp s) in
    let z, s = mailbox (sp s) in
    (x, y, z), s

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

  let status_att s =
    let a, s = atom s in
    match String.uppercase_ascii a with
    | "MESSAGES" ->
        let n, s = uint (sp s) in
        `Messages n, s
    | "RECENT" ->
        let n, s = uint (sp s) in
        `Recent n, s
    | "UIDNEXT" ->
        let n, s = uint32 (sp s) in
        `Uid_next n, s
    | "UIDVALIDITY" ->
        let n, s = uint32 (sp s) in
        `Uid_validity n, s
    | "UNSEEN" ->
        let n, s = uint32 (sp s) in
        `Unseen n, s
    | "HIGHESTMODSEQ" ->
        let n, s = uint64 (sp s) in
        `Highest_modseq n, s
    | _ ->
        failwith "status_att" (* raise (Error (err_unexpected_s a d)) *)

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

  let address s =
    let ad_name, s = nstring' (ch '(' s) in
    let ad_adl, s = nstring' (sp s) in
    let ad_mailbox, s = nstring' (sp s) in
    let ad_host, s = nstring' (sp s) in
    let s = ch ')' s in
    {ad_name; ad_adl; ad_mailbox; ad_host}, s

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

  let envelope s =
    let address_list = list_gen None address in
    let env_date, s = nstring' (ch '(' s) in
    let env_subject, s = nstring' (sp s) in
    let env_from, s = address_list (sp s) in
    let env_sender, s = address_list (sp s) in
    let env_reply_to, s = address_list (sp s) in
    let env_to, s = address_list (sp s) in
    let env_cc, s = address_list (sp s) in
    let env_bcc, s = address_list (sp s) in
    let env_in_reply_to, s = nstring' (sp s) in
    let env_message_id, s = nstring' (sp s) in
    let s = ch ')' s in
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
    }, s

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

  let fld_param s =
    let param s =
      let v1, s = string s in
      let v2, s = string (sp s) in
      (v1, v2), s
    in
    list param s

  let body_fields s =
    let fld_params, s = fld_param s in
    let fld_id, s = nstring (sp s) in
    let fld_desc, s = nstring (sp s) in
    let fld_enc, s = string (sp s) in
    let fld_octets, s = uint (sp s) in
    {
      fld_params;
      fld_id;
      fld_desc;
      fld_enc;
      fld_octets;
    }, s

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

  let rec body_ext s =
    match Sub.head s with
    | Some c when is_digit c ->
        let n, s = uint32 s in
        `Number n, s
    | Some '(' ->
        let xs, s = list1 body_ext s in
        `List xs, s
    | _ ->
        let str, s = nstring s in
        let res = match str with
          | Some x -> `String x
          | None -> `None
        in
        res, s

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

  let fld_dsp s =
    match Sub.head s with
    | Some '(' ->
        let str, s = string (sp s) in
        let p, s = fld_param (sp s) in
        let s = ch ')' s in
        Some (str, p), s
    | _ ->
        let s = atomf "NIL" s in
        None, s

  let fld_lang s =
    match Sub.head s with
    | Some '(' ->
        list1 string s
    | _ ->
        let str, s = nstring s in
        let res = match str with Some x -> [x] | None -> [] in
        res, s

  let body_ext s =
    match Sub.head s with
    | Some ' ' ->
        let dsp, s = fld_dsp (Sub.tail s) in
        begin match Sub.head s with
        | Some ' ' ->
            let lang, s = fld_lang (Sub.tail s) in
            begin match Sub.head s with
            | Some ' ' ->
                let loc, s = nstring (Sub.tail s) in
                let ext, s = sep body_ext s in
                {ext_dsp = dsp; ext_lang = lang; ext_loc = loc; ext_ext = ext}, s
            | _ ->
                {ext_dsp = dsp; ext_lang = lang; ext_loc = None; ext_ext = []}, s
            end
        | _ ->
            {ext_dsp = dsp; ext_lang = []; ext_loc = None; ext_ext = []}, s
        end
    | _ ->
        {ext_dsp = None; ext_lang = []; ext_loc = None; ext_ext = []}, s

  let sbody_ext s =
    let md5, s = nstring s in
    let x, s = body_ext s in
    (md5, x), s

  let mbody_ext s =
    let params, s = fld_param s in
    let x = body_ext s in
    (params, x), s

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

  let rec mbody s = (* TODO Return the extension data *)
    let rec loop acc d =
      match Sub.head s with
      | Some '(' ->
          let x, s = body s in
          loop (x :: acc) s
      | Some ' ' ->
          let m, s = string (Sub.tail s) in
          begin match Sub.head s with
          | Some ' ' ->
              let _, s = mbody_ext (Sub.tail s) in
              `Multipart (List.rev acc, m), s
          | _ ->
              `Multipart (List.rev acc, m), s
          end
      | _ ->
          failwith "mbody" (* err_unexpected c d *)
    in
    loop [] s

  and sbody s = (* TODO Return the extension data *)
    let (m, t, f), s = triple string string body_fields s in
    let ext s =
      match Sub.head s with
      | Some ' ' ->
          let _, s = sbody_ext (Sub.tail s) in
          s
      | _ ->
          s
    in
    match String.uppercase_ascii m, String.uppercase_ascii t with
    | "MESSAGE", "RFC822" ->
        let (e, b, n), s = triple envelope body uint (sp s) in
        let s = ext s in
        `Message (f, e, b, n), s
    | "TEXT", _ ->
        let n, s = uint (sp s) in
        let s = ext s in
        `Text (t, f, n), s
    | _ ->
        let s = ext s in
        `Basic (m, t, f), s

  and body s =
    let res, s =
      match Sub.head (ch '(' s) with
      | Some '(' ->
          mbody s
      | _ ->
          sbody s
    in
    let s = ch ')' s in
    res, s

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

  let scanf s fmt f =
    let r = ref s in
    let ic =
      Scanf.Scanning.from_function (fun () ->
          match Sub.head !r with
          | Some c -> c
          | None -> raise End_of_file
        )
    in
    match Scanf.bscanf ic fmt f with
    | res ->
        res, !r
    | exception _ ->
        failwith "scanf" (* raise (Error (err_unexpected_s s d)) *)

  let date_time s =
    scanf s "\"%2d-%3s-%4d %2d:%2d:%2d %5d\"" begin fun day m year hr mn sc z ->
      let month =
        match String.capitalize_ascii m with
        | "Jan" -> 0
        | "Feb" -> 1
        | "Mar" -> 2
        | "Apr" -> 3
        | "May" -> 4
        | "Jun" -> 5
        | "Jul" -> 6
        | "Aug" -> 7
        | "Sep" -> 8
        | "Oct" -> 9
        | "Nov" -> 10
        | "Dec" -> 11
        | _ -> assert false
      in
      {day; month; year}, {hours = hr; minutes = mn; seconds = sc; zone = z}
    end

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

  let msgtext s =
    let a, s = atom s in
    match String.uppercase_ascii a with
    | "HEADER" -> `Header, s
    | "HEADER.FIELDS" ->
        let xs, s = list1 astring (sp s) in
        `Header_fields xs, s
    | "HEADER.FIELDS.NOT" ->
        let xs, s = list1 astring (sp s) in
        `Header_fields_not xs, s
    | "TEXT" ->
        `Text, s
    |  "MIME" ->
        `Mime, s
    | _ ->
        failwith "msgtext"
        (* err_unexpected_s a d *)

  let rec part s =
    match Sub.head s with
    | Some c when is_digit c ->
        let n, s = uint s in
        let p, s = part (ch '.' s) in
        `Part (n, p), s
    | _ ->
        msgtext s

  let section s =
    let s = ch '[' s in
    match Sub.head s with
    | Some ']' ->
        let s = Sub.tail s in
        `All, s
    | _ ->
        let p, s = part s in
        let s = ch ']' s in
        p, s

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

  let msg_att s =
    let a, s = while1 (fun ch -> is_atom_char ch && ch <> '[') s in
    match String.uppercase_ascii a with
    | "FLAGS" ->
        let xs, s = list flag_fetch (sp s) in
        `Flags xs, s
    | "ENVELOPE" ->
        let x, s = envelope (sp s) in
        `Envelope x, s
    | "INTERNALDATE" ->
        let (t1, t2), s = date_time (sp s) in `Internal_date (t1, t2), s
    | "RFC822.HEADER" ->
        let x, s = nstring (sp s) in `Rfc822_header x, s
    | "RFC822.TEXT" ->
        let x, s = nstring (sp s) in
        `Rfc822_text x, s
    | "RFC822.SIZE" ->
        let x, s = uint (sp s) in
        `Rfc822_size x, s
    | "RFC822" ->
        let x, s = nstring (sp s) in
        `Rfc822 x, s
    | "BODYSTRUCTURE" ->
        let x, s = body (sp s) in
        `Body_structure x, s
    | "BODY" ->
        begin match Sub.head s with
        | Some ' ' ->
            let x, s = body (Sub.tail s) in
            `Body x, s
        | _ ->
            let sec, s = section s in
            let orig, s =
              match Sub.head s with
              | Some '<' ->
                  let n, s = uint (Sub.tail s) in
                  let s = ch '>' s in
                  Some n, s
              | _ ->
                  None, s
            in
            let x, s = nstring (sp s) in
            `Body_section (sec, orig, x), s
        end
    | "UID" ->
        let x = uint32 (sp s) in
        `Uid x, s
    | "MODSEQ" ->
        let n, s = uint64 (ch '(' (sp s)) in
        let s = ch ')' s in
        `Modseq n, s
    | "X-GM-MSGID" ->
        let n, s = uint64 (sp s) in
        `Gm_msgid n, s
    | "X-GM-THRID" ->
        let n, s = uint64 (sp s) in
        `Gm_thrid n, s
    | "X-GM-LABELS" ->
        let xs, s = list astring (sp s) in
        `Gm_labels xs, s
    | _ ->
        failwith "msg_att" (* err_unexpected_s a d *)

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

  let response_data_t s  =
    let a, s = atom s in
    match String.uppercase_ascii a with
    | "OK" -> let (c, t), s = resp_text (sp s) in `Ok (c, t), s
    | "NO" -> let (c, t), s = resp_text (sp s) in `No (c, t), s
    | "BAD" -> let (c, t), s = resp_text (sp s) in `Bad (c, t), s
    | "BYE" -> let (c, t), s = resp_text (sp s) in `Bye (c, t), s
    | "FLAGS" -> let xs, s = list flag (sp s) in `Flags xs, s
    | "LIST" -> let (xs, c, m), s = mailbox_list (sp s) in `List (xs, c, m), s
    | "LSUB" -> let (xs, c, m), s = mailbox_list (sp s) in `Lsub (xs, c, m), s
    | "SEARCH" ->
        let rec loop acc s =
          match Sub.head s with
          | Some ' ' ->
              begin match Sub.head (Sub.tail s) with
              | Some '(' ->
                  let s = atomf "MODSEQ" (Sub.tail s) in
                  let n, s = uint64 (sp s) in
                  let s = ch ')' s in
                  `Search (List.rev acc, Some n), s
              | _ ->
                  let n, s = uint32 s in
                  loop (n :: acc) s
              end
          | _ ->
              `Search (List.rev acc, None), s
        in
        loop [] s
    | "STATUS" ->
        let m, s = mailbox (sp s) in
        let a, s = list status_att (sp s) in
        `Status (m, a), s
    | "CAPABILITY" -> let xs, s = sep capability s in `Capability xs, s
    | "PREAUTH" -> let (c, t), s = resp_text (sp s) in `Preauth (c, t), s
    | "VANISHED" ->
        begin match Sub.head (sp s) with
        | Some '(' ->
            let s = atomf "EARLIER" (Sub.tail s) in
            let set, s = set (sp (ch ')' s)) in
            `Vanished_earlier set, s
        | _ ->
            let set, s = set s in
            `Vanished set, s
        end
    | "ENABLED" ->
        let xs, s = sep capability s in `Enabled xs, s
    | _ ->
        failwith "response_data_t"
  (* err_unexpected_s a d *)

  let response_data_n s =
    let n, s = uint32 s in (* FIXME uint vs uint32 *)
    let a, s = atom (sp s) in
    match String.uppercase_ascii a with
    | "EXPUNGE" -> `Expunge n, s
    | "FETCH" -> let a, s = list1 msg_att (sp s) in `Fetch (n, a), s
    | "EXISTS" -> `Exists (Uint32.to_int n), s
    | "RECENT" -> `Recent (Uint32.to_int n), s
    | _ ->
        failwith "response_data_n" (* err_unexpected_s a d *)

  let untagged s = (* '*' was eaten *)
    match Sub.head (sp s) with
    | Some c when is_digit c ->
        response_data_n s
    | _ ->
        response_data_t s

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

  let tag s =
    while1 is_tag_char s

  let tagged s =
    let tag, s = tag s in
    let x = resp_cond_state (sp s) in
    (tag, x), s

  let continue_req s = (* '+' was eaten *)
    match Sub.head s with
    | Some ' ' ->
        text (Sub.tail s)
    | _ ->
        text s

  let response s =
    let res =
      match Sub.head s with
      | Some '+' ->
          let x, s = continue_req (Sub.tail s) in
          `Cont x, s
      | Some '*' ->
          untagged (Sub.tail s)
      | _ ->
          let (t, str), s = tagged s in
          `Tagged (t, str), s
    in
    let s = crlf s in
    assert (Sub.is_empty s);
    res

  let readresp ic =
    readline ic >|= fun line ->
    Lwt.return (response (Sub.v line))
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

let idle () =
  let stop = ref false in
  let stop_l = Lazy.from_fun (fun () -> stop := true) in
  `Idle stop, (fun () -> Lazy.force stop_l)

module E = struct

  (* Encoder *)

  type command_lexeme =
    [ command_ordinary
    | `Idle
    | `Authenticate of string ]

  type encode =
    [ `Cmd of string * command_lexeme | `Idle_done | `Auth_step of string | `Auth_error ]

  let pp_encode : _ -> [< encode] -> _ = fun ppf e -> match e with
    | `Cmd _ -> pp ppf "`Cmd" (* FIXME *)
    | `Idle_done -> pp ppf "`Idle_done"
    | `Auth_step _ -> pp ppf "`Auth_step"
    | `Auth_error -> pp ppf "`Auth_error"

  type encoder =
    {
      mutable o : string;
      mutable o_pos : int;
    }

  let flush k e =
    if e.o_pos > 0 then
      let rec k1 n =
        if n < e.o_pos then
          `Partial (e.o, n, e.o_pos - n, fun m -> k1 (n + m))
        else
          (e.o_pos <- 0; k e)
      in
      k1 0
    else
      k e

  let w s (_, oc) = Lwt_io.write s oc

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

  let literal x ic oc =
    Printf.ksprintf (Lwt_io.write oc) "{%d}\r\n" (String.length x) >>= fun () ->
    Lwt_io.flush oc >>= fun () ->
    let rec loop = function
      | `Untagged x as r -> `Untagged (x, D.readresp ic >>= loop)
      | `Cont -> Lwt_io.write oc x
    in
    readresp >>= loop

  let w_string x ic oc =
    match classify_string x with
    | `Raw     -> Lwt_io.write oc x
    | `Quoted  -> Lwt_io.fprint oc "\"%s\"" x
    | `Literalal -> literal x ic oc

  let sep ?(sep = ' ') f l ic oc =
    let rec loop = function
      | [] -> Lwt.return_unit
      | x :: [] -> f x ic oc
      | x :: xs ->
          f x >>= fun () -> Lwt_io.write_char oc sep >>= fun () -> loop xs
    in
    loop l

  let (&) x y k e = x (w " " (y k)) e
  let ($) x y k e = x (y k) e

  let crlf ic oc =
    Lwt_io.write oc "\r\n"

  let status_att = function
    | `Messages -> w "MESSAGES"
    | `Recent -> w "RECENT"
    | `Uid_next -> w "UIDNEXT"
    | `Uid_validity -> w "UIDVALIDITY"
    | `Unseen -> w "UNSEEN"
    | `Highest_modseq -> w "HIGHESTMODSEQ"

  let int n = w (string_of_int n)
  let uint32 m = w (Uint32.to_string m)
  let uint64 m = w (Uint64.to_string m)

  let label l =
    w_string (Mutf7.encode l)

  let list f l ic oc =
    let rec loop xs e = match xs with
      | [] -> w ")" ic oc
      | [x] ->
          f x ic oc >>= fun () ->
          Lwt_io.write_char oc ')'
      | x :: xs ->
          f x ic oc >>= fun () ->
          Lwt_io.write_char oc ' ' >>= fun () ->
          loop xs
    in
    Lwt_io.write_char oc '(' >>= fun () -> loop l

  let w_eset s k e =
    let f = function
      | (lo, Some hi) when lo = hi -> w_uint32 lo
      | (lo, Some hi) -> w_uint32 lo $ w ":" $ w_uint32 hi
      | (lo, None) -> w_uint32 lo $ w ":*"
    in
    w_sep ~sep:',' f s k e

  let mailbox s =
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

  let fetch_att : [< fetch_query] -> _ = function
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

  let flag = function
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

  let ret _ = `Ok

  let rec encode e = function
    | `Cmd (tag, x) -> w tag (w " " (w_tagged x (w_crlf (flush ret)))) e
    | `Idle_done -> w "DONE" (w_crlf (flush ret)) e
    | `Auth_step data -> w data (w_crlf (flush ret)) e
    | `Auth_error -> w "*" (w_crlf (flush ret)) e

  let encoder () =
    {
      o = Bytes.create 4096;
      o_pos = 0;
    }
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
  [ `Untagged of untagged * (unit -> result)
  | `Ok of code * string
  | `Error of error
  | `Read of string * int * int * (int -> result)
  | `Write of string * int * int * (int -> result) ]

type connection =
  {
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
    mutex: Lwt_mutex.t;
    mutable idle_stop : bool ref; (* whether the user has signaled to send DONE *)
    mutable idling : bool; (* whether we are idling *)
    mutable tag : int
  }

(* and decode (k : _ -> connection -> _) c = *)
(*   let rec loop = function *)
(*     | `Ok x -> *)
(*         k x c *)
(*     | `Read (s, i, l, k) -> *)
(*         `Read (s, i, l, fun n -> loop (k n)) *)
(*     | `Error e -> *)
(*         `Error (`Decode_error e) (\* FIXME resume on the next line of input *\) *)
(*   in *)
(*   loop (D.decode c.d) *)

let rec h_tagged tag r c =
  let cur = string_of_int c.tag in
  c.tag <- c.tag + 1;
  if tag <> cur then
    `Error (`Incorrect_tag (cur, tag))
  else (* FIXME alert the user and continue ? *)
    match r with
    | `Ok _ as ok -> ok
    | (`Bad _ | `No _) as e -> `Error e

and h_response r c =
  match r with
  | #untagged as r ->
      `Untagged (r, fun () -> decode h_response c)
  | `Cont _ ->
      `Error `Unexpected_cont (* FIXME ignore and continue ? *)
  | `Tagged (g, r) ->
      h_tagged g r c

and h_idle_response r c =
  match r with
  | #untagged as r ->
      `Untagged (r, fun () -> decode h_idle_response c)
  | `Cont _ when not c.idling ->
      c.idling <- true; decode h_idle_response c
  | `Cont _ ->
      `Error `Unexpected_cont
  | `Tagged (g, r) ->
      c.idling <- false;
      h_tagged g r c

and h_authenticate auth r c =
  match r with
  | #untagged as r ->
      `Untagged (r, fun () -> decode (h_authenticate auth) c)
  | `Tagged (g, r) ->
      h_tagged g r c
  | `Cont data ->
      begin match auth.step (B64.decode data) with
      | `Ok data ->
          let data = B64.encode ~pad:true data in
          encode (`Auth_step data) (decode (h_authenticate auth)) c
      | `Error s ->
          encode `Auth_error (fun _ -> `Error (`Auth_error s)) c (* (await (decode (h_authenticate auth)))) c *)
      end

let h_greetings r c =
  match r with
  | `Ok _ as ok -> ok
  | _ -> `Error `Bad_greeting (* FIXME continue until [`Ok] ? *)

let run c cmd =
  match cmd with
  | `Authenticate a ->
      encode (`Cmd (string_of_int c.tag, `Authenticate a.name))
        (decode (h_authenticate a)) c
  | `Idle stop ->
      c.idle_stop <- stop;
      encode (`Cmd (string_of_int c.tag, `Idle))
        (decode h_idle_response) c
  | #command_ordinary as cmd ->
      encode (`Cmd (string_of_int c.tag, cmd))
        (decode h_response) c

let connect ?(port = 993) host =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect sock (Unix.ADDR_INET (addr, port)) >>= fun () ->
  let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  Lwt_ssl.ssl_connect sock ctx >>= fun ssl_sock ->
  let ic = Lwt_ssl.in_channel_of_descr ssl_sock in
  let oc = Lwt_ssl.out_channel_of_descr ssl_sock in
  let c =
    {
      ic;
      oc;
      sock = ssl_sock;
      mutex = Lwt_mutex.create ();
      idling = false;
      idle_stop = ref false;
      tag = 0;
    }
  in
  decode h_greetings c >>= fun resp ->
  Lwt.return (c, resp)
