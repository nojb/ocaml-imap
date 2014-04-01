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

open Imap_uint

module Make (IO : IO.S) = struct
  let (>>=) = IO.bind
  let (>|=) t f = IO.bind t (fun x -> IO.return (f x))

  type t =
      IO.oc * (unit -> unit IO.t) -> unit IO.t

  let rec (@>) (f : t) (g : t) : t =
    fun io -> f io >>= fun () -> g io

  let continuation_req (_, get_cont_request) =
    get_cont_request ()

  let raw s (io, _) =
    IO.write io s

  let char ch out =
    raw (String.make 1 ch) out

  let space out =
    char ' ' out

  let int n out =
    raw (string_of_int n) out

  let uint32 n out =
    raw (Uint32.to_string n) out

  let uint64 n out =
    raw (Uint64.to_string n) out

  let crlf out =
    raw "\r\n" out

  let null _ =
    IO.return ()
  (* Lwt.return `Ok *)

  let rec separated sep f = function
    | [] -> null
    | x :: [] -> f x
    | x :: xs -> f x @> sep @> separated sep f xs

  let list f xs =
    char '(' @> separated (char ' ') f xs @> char ')'

  let separated_pair l sep r (x, y) =
    l x @> sep @> r y

  let literal str =
    char '{' @> int (String.length str) @> char '}' @> crlf @> continuation_req @> raw str

  let quoted_string str =
    let escaped =
      Str.global_substitute (Str.regexp "[\"\\]") (fun s ->
          let s = Str.matched_string s in
          "\\" ^ s) str
    in
    char '"' @> raw escaped @> char '"'

  let needs_literal = function
    | '\x80' .. '\xff' | '\r' | '\n' -> true
    | _ -> false

  let needs_quotes = function
    | '(' | ')' | '{' | ' ' | '\x00' .. '\x1f' | '\x7f'
    | '%' | '*' | '"' | '\\' -> true
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

  let nil =
    raw "NIL"

  let nstring = function
    | Some str -> string str
    | None -> nil

  let mailbox box =
    string (Imap_utils.encode_mutf7 box)

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
      uint32 m @> raw ":*"
    else if Uint32.compare m Uint32.zero = 0 then
      uint32 n @> raw ":*"
    else
      uint32 n @> raw ":" @> uint32 m
  (* | ImapSet.Top, ImapSet.Top -> raw "*" *)
  (* | ImapSet.Num n, ImapSet.Top -> uint32 n @> raw ":*" *)
  (* | ImapSet.Top, ImapSet.Num n -> uint32 n @> raw ":*" *)
  (* | ImapSet.Num n, ImapSet.Num m -> *)
  (*   if Uint32.compare n m = 0 then uint32 n *)
  (*   else uint32 n @> raw ":" @> uint32 m *)

  let message_set set =
    separated (raw ",") message_interval set

  let section_msgtext = function
    | `HEADER  -> raw "HEADER"
    | `HEADER_FIELDS xs -> raw "HEADER.FIELDS" @> space @> list string xs
    | `HEADER_FIELDS_NOT xs -> raw "HEADER.FIELDS.NOT" @> space @> list string xs
    | `TEXT -> raw "TEXT"
    | `ALL -> null

  let rec section_spec : Imap_types.section_spec -> t = function
    | #Imap_types.section_msgtext as sec -> section_msgtext sec
    | `MIME -> raw "MIME"
    | `PART (n, `ALL) -> int n
    | `PART (n, sec) -> int n @> char '.' @> section_spec sec

  and section : Imap_types.section -> t = function
    | #Imap_types.section_msgtext as sec -> section_msgtext sec
    | `PART (n, `ALL) -> int n
    | `PART (n, sec) -> int n @> char '.' @> section_spec sec

  let fetch_att_section kind = function
    | #Imap_types.section as sec ->
      raw kind @> raw "[" @> section sec @> raw "]"
    | `PARTIAL (sec, n, m) ->
      raw kind @> raw "[" @> section sec @> raw "]<" @> int n @> char '.' @> int m @> char '>'

  let fetch_att = function
    | `ENVELOPE -> raw "ENVELOPE"
    | `INTERNALDATE -> raw "INTERNALDATE"
    | `RFC822 -> raw "RFC822"
    | `RFC822_HEADER -> raw "RFC822.HEADER"
    | `RFC822_TEXT -> raw "RFC822.TEXT"
    | `RFC822_SIZE -> raw "RFC822.SIZE"
    | `BODY -> raw "BODY"
    | `BODYSECTION sec -> fetch_att_section "BODY" sec
    | `BODYPEEK sec -> fetch_att_section "BODY.PEEK" sec
    | `BODYSTRUCTURE -> raw "BODYSTRUCTURE"
    | `UID -> raw "UID"
    | `FLAGS -> raw "FLAGS"
    | `MODSEQ -> raw "MODSEQ"
    | `X_GM_MSGID -> raw "X-GM-MSGID"
    | `X_GM_THRID -> raw "X-GM-THRID"
    | `X_GM_LABELS -> raw "X-GM-LABELS"

  let day_month_year (dd, mmm, yyyy) =
    let month_names =
      [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
    in
    raw (Printf.sprintf "%d-%3s-%4d" dd month_names.(mmm) yyyy)

  let flag = function
    | `Answered -> raw "\\Answered"
    | `Flagged -> raw "\\Flagged"
    | `Deleted -> raw "\\Deleted"
    | `Seen -> raw "\\Seen"
    | `Draft -> raw "\\Draft"
    | `Recent -> raw "\\Recent"
    | `Keyword s -> raw s
    | `Extension s -> char '\\' @> raw s (* FIXME: encode in MUTF7 ? *)

  let gm_label s =
    raw (Imap_utils.encode_mutf7 s)

  let entry_type_req = function
    | `All -> "all"
    | `Shared -> "shared"
    | `Priv -> "priv"

  let search_modseq_ext = function
    | None -> null
    | Some (flg, req) ->
      space @> raw "\"/flags/" @> flag flg @> raw "\" " @> raw (entry_type_req req)

  let rec search_key = function
    | `ALL -> raw "ALL"
    | `ANSWERED -> raw "ANSWERED"
    | `BCC s -> raw "BCC " @> string s
    | `BEFORE d -> raw "BEFORE " @> day_month_year d
    | `BODY s -> raw "BODY " @> string s
    | `CC s -> raw "CC " @> string s
    | `DELETED -> raw "DELETED"
    | `FLAGGED -> raw "FLAGGED"
    | `FROM s -> raw "FROM " @> string s
    | `KEYWORD s -> raw "KEYWORD " @> string s
    | `NEW -> raw "NEW"
    | `OLD -> raw "OLD"
    | `ON d -> raw "ON " @> day_month_year d
    | `RECENT -> raw "RECENT"
    | `SEEN -> raw "SEEN"
    | `SINCE d -> raw "SINCE " @> day_month_year d
    | `SUBJECT s -> raw "SUBJECT " @> string s
    | `TEXT s -> raw "TEXT " @> string s
    | `TO s -> raw "TO " @> string s
    | `UNANSWERED -> raw "UNANSWERED"
    | `UNDELETED -> raw "UNDELETED"
    | `UNFLAGGED -> raw "UNDELETED"
    | `UNKEYWORD s -> raw "UNKEYWORD " @> string s
    | `UNSEEN -> raw "UNSEEN"
    | `DRAFT -> raw "DRAFT"
    | `HEADER (s1, s2) -> raw "HEADER " @> string s1 @> raw " " @> string s2
    | `LARGER n -> raw "LARGER " @> int n
    | `NOT q -> raw "NOT " @> search_key q
    | `OR (q1, q2) -> raw "OR " @> search_key q1 @> raw " " @> search_key q2
    | `SENTBEFORE d -> raw "SENTBEFORE " @> day_month_year d
    | `SENTON d -> raw "SENTON " @> day_month_year d
    | `SENTSINCE d -> raw "SENTSINCE " @> day_month_year d
    | `SMALLER n -> raw "SMALLER " @> int n
    | `UID set -> message_set set
    | `UNDRAFT -> raw "UNDRAFT"
    | `INSET set -> message_set set
    | `AND (q1, q2) ->
      raw "(" @> search_key q1 @> raw " " @> search_key q2 @> raw ")"
    | `MODSEQ (ext, modseq) ->
      raw "MODSEQ" @> search_modseq_ext ext @> space @> uint64 modseq
    | `X_GM_RAW str ->
      raw "X-GM-RAW " @> quoted_string str
    | `X_GM_MSGID msgid ->
      raw "X-GM-MSGID " @> raw (Uint64.to_string msgid)
    | `X_GM_THRID thrid ->
      raw "X-GM-THRID " @> raw (Uint64.to_string thrid)
    | `X_GM_LABELS lab ->
      raw "X-GM-LABEL " @> gm_label lab

  let status_att att =
    let to_string = function
      | `MESSAGES -> "MESSAGES"
      | `RECENT -> "RECENT"
      | `UIDNEXT -> "UIDNEXT"
      | `UIDVALIDITY -> "UIDVALIDITY"
      | `UNSEEN -> "UNSEEN"
      | `HIGHESTMODSEQ -> "HIGHESTMODSEQ"
    in
    raw (to_string att)

  let store_att = function
    | `FLAGS flags -> raw "FLAGS" @> space @> list flag flags
    | `FLAGS_SILENT flags -> raw "FLAGS.SILENT" @> space @> list flag flags
    | `X_GM_LABELS labels -> raw "X-GM-LABELS" @> space @> list gm_label labels
    | `X_GM_LABELS_SILENT labels -> raw "X-GM-LABELS.SILENT" @> space @> list gm_label labels
end
