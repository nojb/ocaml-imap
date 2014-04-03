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
open Sexplib.Conv
open Imap_uint

type address = {
  addr_name : string;
  addr_adl : string;
  addr_mailbox : string;
  addr_host : string
} with sexp

type envelope = {
  env_date : string;
  env_subject : string;
  env_from : address list;
  env_sender : address list;
  env_reply_to : address list;
  env_to : address list;
  env_cc : address list;
  env_bcc : address list;
  env_in_reply_to : string;
  env_message_id : string
} with sexp

type media_basic =
  [ `APPLICATION
  | `AUDIO
  | `IMAGE
  | `MESSAGE
  | `VIDEO
  | `OTHER of string ] with sexp

type encoding =
  [ `BIT7
  | `BIT8
  | `BINARY
  | `BASE64
  | `QUOTED_PRINTABLE
  | `OTHER of string ] with sexp

type extension =
  | List of extension list
  | Number of Uint32.t
  | String of string option with sexp

type exts = {
  ext_md5 : Digest.t option sexp_opaque;
  ext_dsp : (string * (string * string) list) option;
  ext_lang : string list;
  ext_exts : extension list
} with sexp

type mexts = {
  mext_param : (string * string) list;
  mext_dsp : (string * (string * string) list) option;
  mext_lang : string list;
  mext_exts : extension list
} with sexp

type basic = {
  basic_type : media_basic;
  basic_subtype : string;
} with sexp

type text = {
  text_subtype : string;
  text_lines : int
} with sexp

type message = {
  message_envelope : envelope;
  message_body : body;
  message_lines : int
}

and 'a single_part = {
  bd_param : (string * string) list;
  bd_id : string option;
  bd_desc : string option;
  bd_enc : encoding;
  bd_octets : int;
  bd_other : 'a;
  bd_ext : exts
}

and multi_part = {
  bd_subtype : string;
  bd_parts : body list;
  bd_mexts : mexts
}

and body =
  | Basic of basic single_part
  | Text of text single_part
  | Message of message single_part
  | Multi_part of multi_part with sexp

open Imap_parser

let address =
  delimited lpar
    (nstring' >>= fun addr_name -> space >>
     nstring' >>= fun addr_adl -> space >>
     nstring' >>= fun addr_mailbox -> space >>
     nstring' >|= fun addr_host ->
     { addr_name; addr_adl; addr_mailbox; addr_host })
    rpar

let address_list =
  delimited lpar (separated_nonempty_list space address) rpar <|> (nil >| [])

let envelope =
  delimited lpar
    (nstring' >>= fun env_date -> space >>
     nstring' >>= fun env_subject -> space >>
     address_list >>= fun env_from -> space >>
     address_list >>= fun env_sender -> space >>
     address_list >>= fun env_reply_to -> space >>
     address_list >>= fun env_to -> space >>
     address_list >>= fun env_cc -> space >>
     address_list >>= fun env_bcc -> space >>
     nstring' >>= fun env_in_reply_to -> space >>
     nstring' >|= fun env_message_id ->
     { env_date; env_subject; env_from; env_sender;
       env_reply_to; env_to; env_cc; env_bcc; env_in_reply_to;
       env_message_id })
    rpar

let media_subtype =
  imap_string
    
(*
media-basic     = ((DQUOTE ("APPLICATION" / "AUDIO" / "IMAGE" /
                  "MESSAGE" / "VIDEO") DQUOTE) / string) SP
                  media-subtype
                    ; Defined in [MIME-IMT]
*)
let media_basic : (media_basic * string) t =
  let table = [
    "APPLICATION", `APPLICATION;
    "AUDIO", `AUDIO;
    "IMAGE", `IMAGE;
    "MESSAGE", `MESSAGE;
    "VIDEO", `VIDEO
  ]
  in
  let media_basic' =
    imap_string >|= fun s ->
    try List.assoc (String.uppercase s) table with Not_found -> `OTHER s
  in
  separated_pair media_basic' space media_subtype

(*
body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil
*)
let body_fld_param : (string * string) list t =
  (delimited lpar
     (separated_nonempty_list space (separated_pair imap_string space imap_string)) rpar) <|>
  (nil >| [])

(*
body-fld-enc    = (DQUOTE ("7BIT" / "8BIT" / "BINARY" / "BASE64"/
                  "QUOTED-PRINTABLE") DQUOTE) / string
*)
let body_fld_enc : encoding Imap_parser.t =
  let table = [
    "7BIT", `BIT7;
    "8BIT", `BIT8;
    "BINARY", `BINARY;
    "BASE64", `BASE64;
    "QUOTED-PRINTABLE", `QUOTED_PRINTABLE
  ]
  in
  imap_string >|= fun s ->
  try List.assoc (String.uppercase s) table with Not_found -> `OTHER s

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
  body_fld_param >>= fun param ->
  space >> body_fld_id >>= fun id ->
  space >> body_fld_desc >>= fun desc ->
  space >> body_fld_enc >>= fun enc ->
  space >> body_fld_octets >|= fun octets ->
  (param, id, desc, enc, octets)

(*
media-message   = DQUOTE "MESSAGE" DQUOTE SP DQUOTE "RFC822" DQUOTE
                    ; Defined in [MIME-IMT]
*)
let media_message =
  delimited dquote (string_ci "MESSAGE") dquote >> space >>
  delimited dquote (string_ci "RFC822") dquote

(*
media-text      = DQUOTE "TEXT" DQUOTE SP media-subtype
                    ; Defined in [MIME-IMT]
*)
let media_text =
  delimited dquote (string_ci "TEXT") dquote >> space >> media_subtype

(*
body-fld-md5    = nstring
*)
let body_fld_md5 =
  (imap_string >|= fun s -> Some s) <|> (nil >| None)

(*
body-fld-dsp    = "(" string SP body-fld-param ")" / nil
*)
let body_fld_dsp =
  (delimited lpar (separated_pair imap_string space body_fld_param) rpar >|= fun dsp -> Some dsp) <|>
  (nil >| None)

(*
body-fld-lang   = nstring / "(" string *(SP string) ")"
*)
let body_fld_lang =
  (delimited lpar (separated_nonempty_list space imap_string) rpar) <|>
  (nstring >|= function None -> [] | Some s -> [s])

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
  (delimited lpar (separated_nonempty_list space (fix body_extension)) rpar >|= fun l -> List l) <|>
  (number >|= fun n -> Number n) <|>
  (nstring >|= fun s -> String s)

(*
body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang
                  [SP body-fld-loc *(SP body-extension)]]]
                    ; MUST NOT be returned on non-extensible
                    ; "BODY" fetch
*)
let body_ext_1part' =
  option (space >> body_fld_md5) >>= begin function
    | Some md5 ->
      option (space >> body_fld_dsp) >>= begin function
        | Some dsp ->
          option (space >> body_fld_lang) >>= begin function
            | Some lang ->
              list (space >> fix body_extension) >>= fun ext ->
              return (md5, dsp, lang, ext)
            | None ->
              return (md5, dsp, [], [])
          end
        | None ->
          return (md5, None, [], [])
      end
    | None ->
      return (None, None, [], [])
  end

let body_ext_1part =
  body_ext_1part' >|= fun (md5, dsp, lang, ext) ->
  { ext_md5 = md5; ext_dsp = dsp; ext_lang = lang; ext_exts = ext }

(*
body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang
                  [SP body-fld-loc *(SP body-extension)]]]
                    ; MUST NOT be returned on non-extensible
                    ; "BODY" fetch
*)
let body_ext_mpart' =
  option (space >> body_fld_param) >>= begin function
    | Some param ->
      option (space >> body_fld_dsp) >>= begin function
        | Some dsp ->
          option (space >> body_fld_lang) >>= begin function
            | Some lang ->
              list (space >> fix body_extension) >>= fun ext ->
              return (param, dsp, lang, ext)
            | None ->
              return (param, dsp, [], [])
          end
        | None ->
          return (param, None, [], [])
      end
    | None ->
      return ([], None, [], [])
  end

let body_ext_mpart =
  body_ext_mpart' >|= fun (param, dsp, lang, ext) ->
  { mext_param = param; mext_dsp = dsp;
    mext_lang = lang; mext_exts = ext }

(*
body-type-basic = media-basic SP body-fields
                    ; MESSAGE subtype MUST NOT be "RFC822"
*)
let rec body_type_basic () =
  media_basic >>= fun (basic_type, basic_subtype) ->
  space >> body_fields >>= fun (bd_param, bd_id, bd_desc, bd_enc, bd_octets) ->
  body_ext_1part >|= fun bd_ext ->
  Basic
    { bd_param; bd_id; bd_desc; bd_enc; bd_octets;
      bd_other = { basic_type; basic_subtype }; bd_ext }

(*
body-type-msg   = media-message SP body-fields SP envelope
                  SP body SP body-fld-lines
*)
and body_type_msg () =
  media_message >> space >> body_fields
  >>= fun (bd_param, bd_id, bd_desc, bd_enc, bd_octets) ->
  space >> envelope >>= fun message_envelope ->
  space >> (body ()) >>= fun message_body ->
  number' >>= fun message_lines ->
  body_ext_1part >|= fun bd_ext ->
  Message
    { bd_param; bd_id; bd_desc; bd_enc; bd_octets;
      bd_other = { message_envelope; message_body; message_lines };
      bd_ext }

(*
body-type-text  = media-text SP body-fields SP body-fld-lines
*)
and body_type_text () =
  media_text >>= fun text_subtype ->
  space >> body_fields >>= fun (bd_param, bd_id, bd_desc, bd_enc, bd_octets) ->
  space >> number' >>= fun text_lines ->
  body_ext_1part >|= fun bd_ext ->
  Text
    { bd_param; bd_id; bd_desc; bd_enc; bd_octets;
      bd_other = { text_subtype; text_lines }; bd_ext }

(*
body-type-1part = (body-type-basic / body-type-msg / body-type-text)
                  [SP body-ext-1part]
*)
and body_type_1part () =
  fix body_type_msg <|> fix body_type_text <|> fix body_type_basic

(*
body-type-mpart = 1*body SP media-subtype
                  [SP body-ext-mpart]
*)
and body_type_mpart () =
  nonempty_list (fix body) >>= fun bs ->
  space >> media_subtype >>= fun media_subtype ->
  body_ext_mpart >|= fun ext ->
  Multi_part
    { bd_subtype = media_subtype; bd_parts = bs; bd_mexts = ext }

(*
body            = "(" (body-type-1part / body-type-mpart) ")"
*)
and body () =
  delimited lpar (fix body_type_1part <|> fix body_type_mpart) rpar

let body = fix body
