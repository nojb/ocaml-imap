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

open ImapTypes

type atom =
  [ `Raw of string
  | `Cont_req ]

module Append = struct
  type 'a t =
    | Atom of 'a
    | Empty
    | Append of 'a t * 'a t

  let (++) x y = Append (x, y)
      
  let rec fold f init = function
    | Atom a -> f init a
    | Empty -> init
    | Append (x, y) -> fold f (fold f init x) y
end

type t =
  atom Append.t

let (++) =
  Append.(++)

let fold =
  Append.fold

let raw s =
  Append.Atom (`Raw s)

let continuation_req =
  Append.Atom `Cont_req

let char ch =
  raw (String.make 1 ch)

let space =
  char ' '

let int n =
  raw (string_of_int n)

let uint32 n =
  raw (Uint32.to_string n)

let uint64 n out =
  raw (Uint64.to_string n)

let crlf =
  raw "\r\n"

let null =
  Append.Empty

let nil =
  raw "NIL"

let rec separated sep f = function
  | [] -> null
  | x :: [] -> f x
  | x :: xs -> f x ++ sep ++ separated sep f xs

let list f = function
  | [] -> nil
  | _ as xs ->
    char '(' ++ separated (char ' ') f xs ++ char ')'

let separated_pair l sep r (x, y) =
  l x ++ sep ++ r y

let literal str =
  char '{' ++ int (String.length str) ++ char '}' ++ crlf ++ continuation_req ++ raw str

let quoted_string str =
  let escaped =
    Str.global_substitute (Str.regexp "[\"\\]")
      (fun s -> let s = Str.matched_string s in "\\" ^ s) str
  in
  char '\"' ++ raw escaped ++ char '\"'
  
let needs_literal = function
  | '\x80' .. '\xff' | '\r' | '\n' -> true
  | _ -> false

let needs_quotes = function
  | '(' | ')' | '{' | ' ' | '\x00' .. '\x1f' | '\x7f'
  | '%' | '*' | '\"' | '\\' -> true
  | _ -> false

let string_exists f s =
  let rec loop i =
    if i >= String.length s then false
    else if f s.[i] then true else loop (i+1)
  in
  loop 0

let string str =
  if str = "" then
    raw "\"\""
  else if string_exists needs_literal str then
    literal str
  else if string_exists needs_quotes str then
    quoted_string str
  else
    raw str

let nstring = function
  | Some str -> string str
  | None -> nil

let mailbox box =
  string (ImapUtils.encode_mutf7 box)

let months =
  [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep";
    "Oct"; "Nov"; "Dec"|]

let date_time x =
  let tm = Unix.gmtime x in
  raw (Printf.sprintf "\"%02d-%s-%04d %02d:%02d:%02d +0000\""
         tm.Unix.tm_mday months.(tm.Unix.tm_mon) (tm.Unix.tm_year + 1900)
         tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec)      

let message_interval (n, m) =
  if Uint32.compare n m = 0 then uint32 n
  else if Uint32.compare n Uint32.zero = 0 then
    uint32 m ++ raw ":*"
  else if Uint32.compare m Uint32.zero = 0 then
    uint32 n ++ raw ":*"
  else
    uint32 n ++ raw ":" ++ uint32 m
  (* | ImapSet.Top, ImapSet.Top -> raw "*" *)
  (* | ImapSet.Num n, ImapSet.Top -> uint32 n @> raw ":*" *)
  (* | ImapSet.Top, ImapSet.Num n -> uint32 n @> raw ":*" *)
  (* | ImapSet.Num n, ImapSet.Num m -> *)
  (*   if Uint32.compare n m = 0 then uint32 n *)
  (*   else uint32 n @> raw ":" @> uint32 m *)

let message_set set =
  separated (raw ",") message_interval set

let section_msgtext =
  function
    SECTION_MSGTEXT_HEADER  -> raw "HEADER"
  | SECTION_MSGTEXT_HEADER_FIELDS xs -> raw "HEADER.FIELDS" ++ space ++ list string xs
  | SECTION_MSGTEXT_HEADER_FIELDS_NOT xs -> raw "HEADER.FIELDS.NOT" ++ space ++ list string xs
  | SECTION_MSGTEXT_TEXT -> raw "TEXT"
(* | `ALL -> null *)

let section_part l =
  separated (char '.') int l

let section_text =
  function
    SECTION_TEXT_MSGTEXT t ->
      section_msgtext t
  | SECTION_TEXT_MIME ->
      raw "MIME"

let rec section_spec =
  function
    SECTION_SPEC_SECTION_MSGTEXT sec ->
      section_msgtext sec
  | SECTION_SPEC_SECTION_PART (part, None) ->
      section_part part
  | SECTION_SPEC_SECTION_PART (part, Some t) ->
      section_part part ++ char '.' ++ section_text t

and section =
  function
    None ->
      raw "[]"
  | Some spec ->
      char '[' ++ section_spec spec ++ char ']'

let fetch_att =
  function
    FETCH_ATT_ENVELOPE -> raw "ENVELOPE"
  | FETCH_ATT_INTERNALDATE -> raw "INTERNALDATE"
  | FETCH_ATT_RFC822 -> raw "RFC822"
  | FETCH_ATT_RFC822_HEADER -> raw "RFC822.HEADER"
  | FETCH_ATT_RFC822_TEXT -> raw "RFC822.TEXT"
  | FETCH_ATT_RFC822_SIZE -> raw "RFC822.SIZE"
  | FETCH_ATT_BODY -> raw "BODY"
  | FETCH_ATT_BODY_SECTION (sec, None) -> raw "BODY" ++ space ++ section sec
  | FETCH_ATT_BODY_SECTION (sec, Some (ofs, len)) ->
      raw "BODY" ++ space ++ section sec ++ char '<' ++ int ofs ++ char '.' ++ int len ++ char '>'
  | FETCH_ATT_BODY_PEEK_SECTION (sec, None) -> raw "BODY.PEEK" ++ space ++ section sec
  | FETCH_ATT_BODY_PEEK_SECTION (sec, Some (ofs, len)) ->
      raw "BODY.PEEK" ++ space ++ section sec ++ char '<' ++ int ofs ++ char '.' ++ int len ++ char '>'
  | FETCH_ATT_BODYSTRUCTURE -> raw "BODYSTRUCTURE"
  | FETCH_ATT_UID -> raw "UID"
  | FETCH_ATT_FLAGS -> raw "FLAGS"
  (* | `MODSEQ -> raw "MODSEQ" *)
  (* | `X_GM_MSGID -> raw "X-GM-MSGID" *)
  (* | `X_GM_THRID -> raw "X-GM-THRID" *)
(* | `X_GM_LABELS -> raw "X-GM-LABELS" *)

let day_month_year (dd, mmm, yyyy) =
  let month_names =
    [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
  in
  raw (Printf.sprintf "%d-%3s-%4d" dd month_names.(mmm) yyyy)

let flag =
  function
    FLAG_ANSWERED -> raw "\\Answered"
  | FLAG_FLAGGED -> raw "\\Flagged"
  | FLAG_DELETED -> raw "\\Deleted"
  | FLAG_SEEN -> raw "\\Seen"
  | FLAG_DRAFT -> raw "\\Draft"
  | FLAG_KEYWORD s -> raw s
  | FLAG_EXTENSION s -> char '\\' ++ raw s (* FIXME: encode in MUTF7 ? *)

let gm_label s =
  raw (ImapUtils.encode_mutf7 s)

let entry_type_req =
  function
    `All -> "all"
  | `Shared -> "shared"
  | `Priv -> "priv"

let search_modseq_ext =
  function
    None -> null
  | Some (flg, req) ->
    space ++ raw "\"/flags/" ++ flag flg ++ raw "\" " ++ raw (entry_type_req req)

external seq_set_to_uint32_set : Seq_set.t -> Uint32_set.t = "%identity"
external uid_set_to_uint32_set : Uid_set.t -> Uint32_set.t = "%identity"

let rec search_key =
  function
    SEARCH_KEY_ALL -> raw "ALL"
  | SEARCH_KEY_ANSWERED -> raw "ANSWERED"
  | SEARCH_KEY_BCC s -> raw "BCC " ++ string s
  | SEARCH_KEY_BEFORE d -> raw "BEFORE " ++ day_month_year d
  | SEARCH_KEY_BODY s -> raw "BODY " ++ string s
  | SEARCH_KEY_CC s -> raw "CC " ++ string s
  | SEARCH_KEY_DELETED -> raw "DELETED"
  | SEARCH_KEY_FLAGGED -> raw "FLAGGED"
  | SEARCH_KEY_FROM s -> raw "FROM " ++ string s
  | SEARCH_KEY_KEYWORD s -> raw "KEYWORD " ++ string s
  | SEARCH_KEY_NEW -> raw "NEW"
  | SEARCH_KEY_OLD -> raw "OLD"
  | SEARCH_KEY_ON d -> raw "ON " ++ day_month_year d
  | SEARCH_KEY_RECENT -> raw "RECENT"
  | SEARCH_KEY_SEEN -> raw "SEEN"
  | SEARCH_KEY_SINCE d -> raw "SINCE " ++ day_month_year d
  | SEARCH_KEY_SUBJECT s -> raw "SUBJECT " ++ string s
  | SEARCH_KEY_TEXT s -> raw "TEXT " ++ string s
  | SEARCH_KEY_TO s -> raw "TO " ++ string s
  | SEARCH_KEY_UNANSWERED -> raw "UNANSWERED"
  | SEARCH_KEY_UNDELETED -> raw "UNDELETED"
  | SEARCH_KEY_UNFLAGGED -> raw "UNDELETED"
  | SEARCH_KEY_UNKEYWORD s -> raw "UNKEYWORD " ++ string s
  | SEARCH_KEY_UNSEEN -> raw "UNSEEN"
  | SEARCH_KEY_DRAFT -> raw "DRAFT"
  | SEARCH_KEY_HEADER (s1, s2) -> raw "HEADER " ++ string s1 ++ raw " " ++ string s2
  | SEARCH_KEY_LARGER n -> raw "LARGER " ++ int n
  | SEARCH_KEY_NOT q -> raw "NOT " ++ search_key q
  | SEARCH_KEY_OR (q1, q2) -> raw "OR " ++ search_key q1 ++ raw " " ++ search_key q2
  | SEARCH_KEY_SENTBEFORE d -> raw "SENTBEFORE " ++ day_month_year d
  | SEARCH_KEY_SENTON d -> raw "SENTON " ++ day_month_year d
  | SEARCH_KEY_SENTSINCE d -> raw "SENTSINCE " ++ day_month_year d
  | SEARCH_KEY_SMALLER n -> raw "SMALLER " ++ int n
  | SEARCH_KEY_UID set -> message_set (uid_set_to_uint32_set set)
  | SEARCH_KEY_UNDRAFT -> raw "UNDRAFT"
  | SEARCH_KEY_INSET set -> message_set (seq_set_to_uint32_set set)
  | SEARCH_KEY_AND (q1, q2) ->
    raw "(" ++ search_key q1 ++ raw " " ++ search_key q2 ++ raw ")"
  | SEARCH_KEY_MODSEQ (ext, modseq) ->
    raw "MODSEQ" ++ search_modseq_ext ext ++ space ++ raw (Modseq.to_string modseq)
  | SEARCH_KEY_XGMRAW str ->
    raw "X-GM-RAW " ++ quoted_string str
  | SEARCH_KEY_XGMMSGID msgid ->
    raw "X-GM-MSGID " ++ raw (Gmsgid.to_string msgid)
  | SEARCH_KEY_XGMTHRID thrid ->
    raw "X-GM-THRID " ++ raw (Gthrid.to_string thrid)
  (* | SEARCH_KEY_XGMLABELS lab -> *)
  (*   raw "X-GM-LABEL " ++ gm_label lab *)

let status_att (att : status_att) =
  let to_string : status_att -> string =
    function
      STATUS_ATT_MESSAGES -> "MESSAGES"
    | STATUS_ATT_RECENT -> "RECENT"
    | STATUS_ATT_UIDNEXT -> "UIDNEXT"
    | STATUS_ATT_UIDVALIDITY -> "UIDVALIDITY"
    | STATUS_ATT_UNSEEN -> "UNSEEN"
    | STATUS_ATT_HIGHESTMODSEQ -> "HIGHESTMODSEQ"
  in
  raw (to_string att)

(* let store_att = function *)
(*   | `FLAGS flags -> raw "FLAGS" ++ space ++ list flag flags *)
(*   | `FLAGS_SILENT flags -> raw "FLAGS.SILENT" ++ space ++ list flag flags *)
(*   | `X_GM_LABELS labels -> raw "X-GM-LABELS" ++ space ++ list gm_label labels *)
(*   | `X_GM_LABELS_SILENT labels -> raw "X-GM-LABELS.SILENT" ++ space ++ list gm_label labels *)
