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
open ImapUint
  
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

open ImapParser

let address =
  char '(' >>
  nstring' >>= fun addr_name -> char ' ' >>
  nstring' >>= fun addr_adl -> char ' ' >>
  nstring' >>= fun addr_mailbox -> char ' ' >>
  nstring' >>= fun addr_host ->
  char ')' >>
  return { addr_name; addr_adl; addr_mailbox; addr_host }

let address_list =
  alt [
    begin
      char '(' >>
      sep1 (char ' ') address >>= fun xs ->
      char ')' >>
      return xs
    end;
    begin
      string_ci "NIL" >> return []
    end
  ]

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
  return {
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
let media_basic : (media_basic * string, _) t =
  let table = [
    "APPLICATION", `APPLICATION;
    "AUDIO", `AUDIO;
    "IMAGE", `IMAGE;
    "MESSAGE", `MESSAGE;
    "VIDEO", `VIDEO
  ]
  in
  let media_basic' =
    imap_string >>= fun s ->
    return (try List.assoc (String.uppercase s) table with Not_found -> `OTHER s)
  in
  media_basic' >>= fun mb -> char ' ' >> media_subtype >>= fun mst -> return (mb, mst)

(*
body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil
*)
let body_fld_param : ((string * string) list, _) t =
  let param = imap_string >>= fun k -> char ' ' >> imap_string >>= fun v -> return (k, v) in
  alt [
    begin
      char '(' >>
      sep1 (char ' ') param >>= fun xs ->
      char ')' >>
      return xs
    end;
    begin
      string_ci "nil" >> return []
    end
  ]

(*
body-fld-enc    = (DQUOTE ("7BIT" / "8BIT" / "BINARY" / "BASE64"/
                  "QUOTED-PRINTABLE") DQUOTE) / string
*)
let body_fld_enc : (encoding, _) ImapParser.t =
  let table = [
    "7BIT", `BIT7;
    "8BIT", `BIT8;
    "BINARY", `BINARY;
    "BASE64", `BASE64;
    "QUOTED-PRINTABLE", `QUOTED_PRINTABLE
  ]
  in
  imap_string >>= fun s ->
  return (try List.assoc (String.uppercase s) table with Not_found -> `OTHER s)

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
  char ' ' >> body_fld_id >>= fun id ->
  char ' ' >> body_fld_desc >>= fun desc ->
  char ' ' >> body_fld_enc >>= fun enc ->
  char ' ' >> body_fld_octets >>= fun octets ->
  return (param, id, desc, enc, octets)

(*
media-message   = DQUOTE "MESSAGE" DQUOTE SP DQUOTE "RFC822" DQUOTE
                    ; Defined in [MIME-IMT]
*)
let media_message =
  char '\"' >> string_ci "MESSAGE" >> char '\"' >> char ' ' >>
  char '\"' >> string_ci "RFC822" >> char '\"'

(*
media-text      = DQUOTE "TEXT" DQUOTE SP media-subtype
                    ; Defined in [MIME-IMT]
*)
let media_text =
  char '\"' >> string_ci "TEXT" >> char '\"' >> char ' ' >> media_subtype

(*
body-fld-md5    = nstring
*)
let body_fld_md5 =
  nstring

(*
body-fld-dsp    = "(" string SP body-fld-param ")" / nil
*)
let body_fld_dsp =
  alt [
    begin
      char '(' >>
      imap_string >>= fun k ->
      char ' ' >>
      body_fld_param >>= fun p ->
      char ')' >>
      return (Some (k, p))
    end;
    begin
      string_ci "nil" >> return None
    end
  ]

(*
body-fld-lang   = nstring / "(" string *(SP string) ")"
*)
let body_fld_lang =
  alt [
    begin
      char '(' >>
      sep1 (char ' ') imap_string >>= fun xs ->
      char ')' >>
      return xs
    end;
    begin
      nstring >>= function None -> return [] | Some s -> return [s]
    end
  ]

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
  alt [
    begin
      char '(' >>
      sep1 (char ' ') (fix body_extension) >>= fun xs ->
      char ')' >>
      return (List xs)
    end;
    begin
      number >>= fun n -> return (Number n)
    end;
    begin
      nstring >>= fun s -> return (String s)
    end
  ]

(*
body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang
                  [SP body-fld-loc *(SP body-extension)]]]
                    ; MUST NOT be returned on non-extensible
                    ; "BODY" fetch
*)
let body_ext_1part' =
  opt (char ' ' >> body_fld_md5) >>= begin function
    | Some md5 ->
      opt (char ' ' >> body_fld_dsp) >>= begin function
        | Some dsp ->
          opt (char ' ' >> body_fld_lang) >>= begin function
            | Some lang ->
              rep (char ' ' >> fix body_extension) >>= fun ext ->
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
  body_ext_1part' >>= fun (ext_md5, ext_dsp, ext_lang, ext_exts) ->
  return { ext_md5; ext_dsp; ext_lang; ext_exts }

(*
body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang
                  [SP body-fld-loc *(SP body-extension)]]]
                    ; MUST NOT be returned on non-extensible
                    ; "BODY" fetch
*)
let body_ext_mpart' =
  opt (char ' ' >> body_fld_param) >>= begin function
    | Some param ->
      opt (char ' ' >> body_fld_dsp) >>= begin function
        | Some dsp ->
          opt (char ' ' >> body_fld_lang) >>= begin function
            | Some lang ->
              rep (char ' ' >> fix body_extension) >>= fun ext ->
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
  body_ext_mpart' >>= fun (mext_param, mext_dsp, mext_lang, mext_exts) ->
  return { mext_param; mext_dsp; mext_lang; mext_exts }

(*
body-type-basic = media-basic SP body-fields
                    ; MESSAGE subtype MUST NOT be "RFC822"
*)
let rec body_type_basic () =
  media_basic >>= fun (basic_type, basic_subtype) ->
  char ' ' >> body_fields >>= fun (bd_param, bd_id, bd_desc, bd_enc, bd_octets) ->
  body_ext_1part >>= fun bd_ext ->
  return (Basic {bd_param; bd_id; bd_desc; bd_enc; bd_octets;
                 bd_other = { basic_type; basic_subtype }; bd_ext})

(*
body-type-msg   = media-message SP body-fields SP envelope
                  SP body SP body-fld-lines
*)
and body_type_msg () =
  media_message >> char ' ' >> body_fields >>= fun (bd_param, bd_id, bd_desc, bd_enc, bd_octets) ->
  char ' ' >> envelope >>= fun message_envelope ->
  char ' ' >> body () >>= fun message_body ->
  number' >>= fun message_lines ->
  body_ext_1part >>= fun bd_ext ->
  return (Message
            { bd_param; bd_id; bd_desc; bd_enc; bd_octets;
              bd_other = { message_envelope; message_body; message_lines };
              bd_ext })

(*
body-type-text  = media-text SP body-fields SP body-fld-lines
*)
and body_type_text () =
  media_text >>= fun text_subtype ->
  char ' ' >> body_fields >>= fun (bd_param, bd_id, bd_desc, bd_enc, bd_octets) ->
  char ' ' >> number' >>= fun text_lines ->
  body_ext_1part >>= fun bd_ext ->
  return (Text
            { bd_param; bd_id; bd_desc; bd_enc; bd_octets;
              bd_other = { text_subtype; text_lines }; bd_ext })

(*
body-type-1part = (body-type-basic / body-type-msg / body-type-text)
                  [SP body-ext-1part]
*)
and body_type_1part () =
  alt [fix body_type_msg; fix body_type_text; fix body_type_basic]

(*
body-type-mpart = 1*body SP media-subtype
                  [SP body-ext-mpart]
*)
and body_type_mpart () =
  rep1 (fix body) >>= fun bs ->
  char ' ' >> media_subtype >>= fun media_subtype ->
  body_ext_mpart >>= fun ext ->
  return (Multi_part { bd_subtype = media_subtype; bd_parts = bs; bd_mexts = ext })

(*
body            = "(" (body-type-1part / body-type-mpart) ")"
*)
and body () =
  char '(' >>
  alt [fix body_type_1part; fix body_type_mpart] >>= fun b ->
  char ')' >>
  return b

let body = fix body
