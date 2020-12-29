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

type date = { day : int; month : int; year : int }

module Uint32 : sig
  type t = int32

  module Set : sig
    type elt

    type t

    val empty : t

    val singleton : elt -> t

    val union : t -> t -> t

    val add : elt -> t -> t

    val add_interval : elt -> elt -> t -> t

    val of_list : elt list -> t

    val elements : t -> (elt * elt) list
  end
  with type elt := t
end = struct
  type t = int32

  let msb n = Int32.(logand n (shift_left 1l 31)) <> 0l

  let compare n1 n2 =
    match (msb n1, msb n2) with
    | true, true ->
        Int32.(compare (logand n1 0x7fffffffl) (logand n2 0x7fffffffl))
    | true, false -> 1
    | false, true -> -1
    | false, false -> Int32.compare n1 n2

  let succ n = Int32.succ n

  let ( < ) n m = compare n m < 0

  let ( <= ) n m = compare n m <= 0

  let min n m = if n <= m then n else m

  let max n m = if n <= m then m else n

  module Set = struct
    type t = (int32 * int32) list

    (* disjoint, sorted intervals *)

    let empty = []

    let singleton n = [ (n, n) ]

    let elements x = x

    let rec union s1 s2 =
      let rec loop s1 s2 =
        match (s1, s2) with
        | [], s2 -> s2
        | s1, [] -> s1
        | (a, b) :: x1, (c, d) :: x2 ->
            if succ b < c then (a, b) :: loop x1 s2
            else if succ d < a then (c, d) :: loop s1 x2
            else union [ (min a c, max b d) ] (union x1 x2)
      in
      loop s1 s2

    let add n s = union (singleton n) s

    let add_interval n m s = if n <= m then union [ (n, m) ] s else s

    let of_list l = List.fold_left (fun s n -> add n s) empty l
  end
end

type flag =
  | Answered
  | Flagged
  | Deleted
  | Seen
  | Draft
  | Keyword of string
  | Extension of string
  | Recent
  | Any

type code =
  | ALERT
  | BADCHARSET of string list
  | CAPABILITY of string list
  | PARSE
  | PERMANENTFLAGS of flag list
  | READ_ONLY
  | READ_WRITE
  | TRYCREATE
  | UIDNEXT of int32
  | UIDVALIDITY of int32
  | UNSEEN of int
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

type mime_msgtext =
  | HEADER
  | HEADER_FIELDS of string list
  | HEADER_FIELDS_NOT of string list
  | TEXT
  | MIME

type address = {
  ad_name : string;
  ad_adl : string;
  ad_mailbox : string;
  ad_host : string;
}

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
  env_message_id : string;
}

type mime_section = int list * mime_msgtext option

type mime_fields = {
  fld_params : (string * string) list;
  fld_id : string option;
  fld_desc : string option;
  fld_enc : string;
  fld_octets : int;
}

type sexp = List of sexp list | Number of int32 | String of string

type mime_extension = {
  ext_dsp : (string * (string * string) list) option;
  ext_lang : string list;
  ext_loc : string;
  ext_ext : sexp list;
}

type mime =
  | Text of string * mime_fields * int
  | Message of mime_fields * envelope * mime * int
  | Basic of string * string * mime_fields
  | Multipart of mime list * string * (string * string) list

type message_attribute =
  | FLAGS of flag list
  | ENVELOPE of envelope
  | INTERNALDATE of string (* Date.t * Time.t *)
  | RFC822 of string
  | RFC822_HEADER of string
  | RFC822_TEXT of string
  | RFC822_SIZE of int
  | BODY of mime
  | BODYSTRUCTURE of mime
  | BODY_SECTION of mime_section * string option
  | UID of int32
  | MODSEQ of int64
  | X_GM_MSGID of int64
  | X_GM_THRID of int64
  | X_GM_LABELS of string list

type mailbox_flag =
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

type mailbox_attribute =
  | MESSAGES of int
  | RECENT of int
  | UIDNEXT of int32
  | UIDVALIDITY of int32
  | UNSEEN of int
  | HIGHESTMODSEQ of int64

type untagged =
  | OK of { code : code option; message : string }
  | NO of { code : code option; message : string }
  | BAD of { code : code option; message : string }
  | BYE of { code : code option; message : string }
  | PREAUTH of code option * string
  | FLAGS of flag list
  | LIST of mailbox_flag list * char option * string
  | LSUB of mailbox_flag list * char option * string
  | SEARCH of int32 list * int64 option
  | STATUS of string * mailbox_attribute list
  | EXISTS of int
  | RECENT of int
  | EXPUNGE of int32
  | FETCH of int32 * message_attribute list
  | CAPABILITY of string list
  | VANISHED of Uint32.Set.t
  | VANISHED_EARLIER of Uint32.Set.t
  | ENABLED of string list

type status = OK | NO | BAD

type response =
  | Untagged of untagged
  | Cont of string
  | Tagged of {
      tag : string;
      status : status;
      code : code option;
      message : string;
    }
