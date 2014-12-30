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

(* Characters and their classes. *)

let ux_eoi = max_int                 (* End of input, outside unicode range. *)
let ux_soi = max_int - 1           (* Start of input, outside unicode range. *)
let u_lpar = 0x28 (* ( *)
let u_rpar = 0x29 (* ) *)
let u_sp = 0x20 (*   *)
let u_cr = 0x0D (* \r *)
let u_lf = 0x0A (* \n *)
let u_lbrace = 0x7B (* { *)
let u_rbrace = 0x7D (* } *)
let u_lbrack = 0x5B (* [ *)
let u_rbrack = 0x5D (* ] *)
let u_star = 0x2A (* * *)
let u_plus = 0x2B (* + *)
let u_bslash = 0x5C (* \ *)
let u_dquote = 0x22 (* "" *)
let u_lang = 0x3C (* < *)
let is_white = function 0x20 | 0x09 | 0x0D | 0x0A -> true | _ -> false
let is_digit = fun c -> 0x30 <= c && c <= 0x39

(* type flag = *)
(*   [ `Answered *)
(*   | `Flagged *)
(*   | `Deleted *)
(*   | `Seen *)
(*   | `Draft *)
(*   | `Extension of string *)
(*   | `Keyword of string ] *)

(* type month = *)
(*   [ `Jan | `Feb | `Mar | `Apr | `May | `Jun *)
(*   | `Jul | `Aug | `Sep | `Oct | `Nov | `Dec ] *)

type lexeme =
  [ `String of string
  | `Number of string
  | `Int of int
  | `Nz of int
  | `Int32 of int32
  | `Char of char
  | `Open
  | `Close
  | `Alert
  | `Bad_charset
  | `Capability
  | `Parse
  | `Read_only
  | `Read_write
  | `Try_create
  | `Uid_next
  | `Uid_validity
  | `Unseen
  | `Answered
  | `Flagged
  | `Deleted
  | `Seen
  | `Draft
  | `Extension of string
  | `Keyword of string
  | `All
  | `Recent
  (* | `Flag of flag *)
  (* | `Flag_perm of [ flag | `All ] *)
  (* | `Flag_fetch of [ flag | `Recent ] *)
  | `Permanent_flags
  | `Ok
  | `No
  | `Bad
  | `Bye
  | `Flags
  | `Expunge
  | `Date_time of string
  | `Internal_date
  | `Nil
  | `Rfc822_header
  | `Rfc822_text
  | `Rfc822_size
  | `Rfc822
  | `Uid
  | `Fetch
  | `Noselect
  | `Marked
  | `Unmarked
  | `Noinferiors
  | `List
  | `Lsub
  | `No_tag
  | `Tag of string
  | `Messages
  | `Status
  | `Plus
  | `Search
  | `Exists
  | `Header
  | `Header_fields
  | `Header_fields_not
  | `Text
  | `Mime
  | `Body
  | `Body_structure ]

type error =
  [ `Illegal_quoted of int
  | `Illegal_escape of int
  | `Illegal_number of string
  | `Illegal_nz_number of string
  | `Expected of [ `Char of char | `String of string ]
  | `Unexpected of [ `Char of int | `String of string ] ]

type decode = [ `Lexeme of lexeme | `Error of error | `End | `Await ]

type src = [ `Manual | `String of string | `Channel of in_channel ]

type decoder =
  { src : src;
    mutable i : string;
    mutable i_pos : int;
    mutable i_max : int;
    mutable c : int;
    mutable l : string;
    buf : Buffer.t;
    mutable k : decoder -> decode }

let id x = x
let eoi d = d.i <- ""; d.i_pos <- max_int; d.i_max <- 0; d.c <- ux_eoi
let decode_src d s j l =
  if (j < 0 || l < 0 || j + l > String.length s) then invalid_arg "bounds";
  if (l = 0) then eoi d else
  (d.i <- s; d.i_pos <- j; d.i_max <- j + l - 1;)
let badd d = Buffer.add_char d.buf (Char.chr d.c)
let baddu d = Buffer.add_char d.buf (Char.uppercase (Char.chr d.c))
let buf d = let s = Buffer.contents d.buf in (Buffer.clear d.buf; s)
let lit d = let s = d.l in (d.l <- ""; s)
let ret v k d = d.k <- k; v
let rec eof d = ret `End eof d
let err_quoted c = `Error (`Illegal_quoted c)
let err_escape c = `Error (`Illegal_escape c)
let err_number s = `Error (`Illegal_number s)
let err_nz_number s = `Error (`Illegal_nz_number s)
let err_accept c = `Error (`Expected (`Char c))
let err_unexpected c = `Error (`Unexpected (`Char c))
let err_unexpected_s s = `Error (`Unexpected (`String s))
let err_expecteds s = `Error (`Expected (`String s))
let refill k d = match d.src with
  | `Manual -> d.k <- k; `Await
  | `String _ -> eoi d; k d
  | `Channel ic ->
      let rc = input ic d.i 0 (String.length d.i) in
      decode_src d d.i 0 rc;
      k d

let rec readc k d =
  if d.i_pos > d.i_max then
    (if d.c = ux_eoi then k d else refill (readc k) d)
  else begin
    d.c <- Char.code (String.get d.i d.i_pos);
    d.i_pos <- d.i_pos + 1;
    k d
  end

let accept_c c k d =
  if d.c = Char.code c then readc k d else
  ret (err_accept c) k d

let r_sp k d =
  accept_c ' ' k d

let r_crlf k d =
  accept_c '\r' (accept_c '\n' k) d

let accept_s s k d =
  let rec loop i d =
    if i >= String.length s then k d else
    if Char.uppercase (Char.chr d.c) = Char.uppercase s.[i] then readc (loop (i + 1)) d else
    ret (err_expecteds s) k d
  in
  loop 0 d

let r_list1 r k d =
  let rec nxt d =
    if d.c = u_sp then readc (r nxt) d else
    accept_c ')' (ret (`Lexeme `Close) k) d
  in
  accept_c '(' (ret (`Lexeme `Open) (r nxt)) d

let r_list r k d =
  let rec nxt d =
    if d.c = u_sp then readc (r nxt) d else
    if d.c = u_rpar then ret (`Lexeme `Close) (readc k) d else
    ret (err_unexpected d.c) k d
  in
  accept_c '(' (ret (`Lexeme `Open)
                  (fun d -> if d.c = 0x29 (* ')' *) then ret (`Lexeme `Close) (readc k) d else r nxt d)) d

let is_alpha_char c =
  (Char.code 'a' <= c && c <= Char.code 'z') || (Char.code 'A' <= c && c <= Char.code 'Z')

(****)

let rec readn k d =
  if is_digit d.c then (badd d; readc (readn k) d) else
  k d

let r_number k d =
  (* FIXME signedness *)
  readn (fun d -> ret (`Lexeme (`Number (buf d))) k d) d

let r_nz_number k d =
  r_number k d
  (* FIXME signedness & non-zeroness *)
  (* readn (fun d -> ret (`Lexeme (`Number (buf d))) k d) d *)

(* let rec r_number k d = *)
(*   if is_digit d.c then (badd d; readc (r_number k) d) else *)
(*   let s = buf d in *)
(*   try ret (`Lexeme (`Int (int_of_string s))) k d with *)
(*   | Failure _ -> ret (err_number s) k d *)

(* let rec r_nz_number k d = *)
(*   if is_digit d.c then (badd d; readc (r_nz_number k) d) else *)
(*   let s = buf d in *)
(*   try *)
(*     let n = int_of_string s in *)
(*     if n = 0 then ret (err_nz_number s) k d else *)
(*     ret (`Lexeme (`Nz n)) k d *)
(*   with *)
(*   | Failure _ -> ret (err_nz_number s) k d *)

(*
CHAR           =  %x01-7F
                               ; any 7-bit US-ASCII character,
                                  excluding NUL

TEXT-CHAR       = <any CHAR except CR and LF>
*)

(* let is_text_char c = *)
  (* 0x01 <= c && c <= 0x7F && (c <> 0x0D) && (c <> 0x0A) *)

(*
QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
                  "\\" quoted-specials

quoted-specials = DQUOTE / "\\"
*)
let r_quoted_char k d =
  if d.c = u_cr || d.c = u_lf then ret (err_unexpected d.c) k d else
  if d.c = u_bslash then
    let nxt d =
      if d.c = u_bslash || d.c = u_dquote then ret (`Lexeme (`Char (Char.chr d.c))) (readc k) d else
      ret (err_unexpected d.c) k d
    in
    readc nxt d
  else
  if 0x01 <= d.c && d.c <= 0x7F then ret (`Lexeme (`Char (Char.chr d.c))) (readc k) d else
  ret (err_unexpected d.c) k d

(*
quoted          = DQUOTE *QUOTED-CHAR DQUOTE
*)
let rec r_quoted ?(pp = id) k d = (* "" was eaten *)
  if d.c = u_cr || d.c = u_lf then ret (err_quoted d.c) k d else
  if d.c = u_dquote then ret (`Lexeme (`String (pp (buf d)))) (readc k) d else
  if d.c = u_bslash then
    let nxt d =
      if d.c = u_dquote || d.c = u_bslash then (badd d; readc (r_quoted k) d) else
      ret (err_escape d.c) k d
    in
    readc nxt d
  else
  if 0x01 <= d.c && d.c <= 0x7F then (badd d; readc (r_quoted k) d) else
  ret (err_quoted d.c) k d

  (* match d.c with *)
  (* | 0x0D (\* \r *\) | 0x0A (\* \n *\) -> ret (err_quoted d.c) k d *)
  (* | 0x22 (\* "" *\) -> ret (`String (buf d)) (readc k) d *)
  (* | 0x5C (\* \\ *\) -> *)
  (* | 0x01 .. 0x7F -> (badd d; readc (r_quoted k) d) *)
  (* | _ -> ret (err_quoted d.c) k d *)

(*
literal         = "{" number "}" CRLF *CHAR8
                    ; Number represents the number of CHAR8s
*)

let readl n k d =
  (* Printf.printf "readl i_max=%d i_pos=%d n=%d\n%!" d.i_max d.i_pos n; *)
  if d.i_max - d.i_pos + 2 >= n then begin
    let l = String.sub d.i (d.i_pos - 1) n in
    d.i_pos <- d.i_pos + n;
    d.l <- l;
    k d
  end else begin
    d.l <- String.create n;
    let rec loop off rem d =
      let have = d.i_max - d.i_pos + 2 in
      if have >= n then begin
        String.blit d.i (d.i_pos - 1) d.l off n;
        d.i_pos <- d.i_pos + n;
        k d
      end else begin
        String.blit d.i (d.i_pos - 1) d.l off have;
        d.i_pos <- d.i_max + 1;
        readc (loop (off + have) (n - have)) d
      end
    in
    loop 0 n d
  end

let rec r_literal ?(pp = id) k d = (* { was eaten *)
  if is_digit d.c then (badd d; readc (r_literal k) d) else
  if d.c = u_rbrace then
    let s = buf d in
    let n = int_of_string s in
    readc (r_crlf (readl n (fun d -> ret (`Lexeme (`String (pp (lit d)))) k d))) d
  else
  ret (err_unexpected d.c) k d

(*
string          = quoted / literal
*)
let r_string ?(pp = id) k d =
  if d.c = u_dquote then readc (r_quoted ~pp k) d else
  if d.c = u_lbrace then readc (r_literal ~pp k) d else
  ret (err_unexpected d.c) k d

(*
astring         = 1*ASTRING-CHAR / string
*)
let is_astring_char c =
  if 0 <= c && c <= 255 then
    match Char.chr c with
    | '\x80' .. '\xff' | '(' | ')'
    | '{' | ' ' | '\x00' .. '\x1f' | '\x7f'
    | '%' | '*' | '\\' | '\"' -> false
    | _ -> true
  else
  false

let r_astring ?(pp = id) k d =
  if is_astring_char d.c then
    let rec nxt d =
      (* Printf.printf "nxt: i_pos=%d i_max=%d c=%i\n%!" d.i_pos d.i_max d.c; *)
      if is_astring_char d.c then (badd d; readc nxt d) else
      ret (`Lexeme (`String (pp (buf d)))) k d in
    nxt d
  else
  r_string ~pp k d
(* alt (accum is_astring_char) imap_string *)

(*
nil             = "NIL"
*)
(* let nil = *)
(*   str "nil" *)

(*
nstring         = string / nil
*)
let r_nstring k d =
  if d.c = 0x4E (* N *) || d.c = 0x6E (* n *) then accept_s "NIL" (ret (`Lexeme `Nil) k) d else
  r_string k d

(*
atom            = 1*ATOM-CHAR

ATOM-CHAR       = <any CHAR except atom-specials>

atom-specials   = "(" / ")" / "{" / SP / CTL / list-wildcards /
                  quoted-specials / resp-specials

quoted-specials = DQUOTE / "\\"

resp-specials   = "]"

list-wildcards  = "%" / "*"
*)
let is_atom_char c =
  match c with
  | 0x28 (* ( *)
  | 0x29 (* ) *)
  | 0x7B (* { *)
  | 0x20 (*   *)
  | 0x7F
  | 0x25 (* % *)
  | 0x2A (* * *)
  | 0x5C (* \ *)
  | 0x22 (* "" *)
  | 0x5D (* ] *) -> false
  | _ -> 0x20 <= c && c <= 0x7F
  (* if 0 <= c && c <= 255 then *)
  (*   match Char.chr c with *)
  (*   | '\x80' .. '\xff' *)
  (*   | '(' | ')' | '{' | ' ' | '\x00' .. '\x1f' *)
  (*   | '\x7f' | '%' | '*' | '\\' | '\"' | ']' -> false *)
  (*   | _ -> true *)
  (* else *)
  (* false *)

let rec r_atom k d =
  if is_atom_char d.c then (badd d; readc (r_atom k) d) else
  ret (`Lexeme (`String (buf d))) k d

(****)

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
let r_flag k d =
  if d.c = u_bslash then
    let rec nxt d =
      if is_atom_char d.c then (baddu d; readc nxt d) else
      match buf d with
      | "ANSWERED" -> ret (`Lexeme `Answered) k d
      | "FLAGGED" -> ret (`Lexeme `Flagged) k d
      | "DELETED" -> ret (`Lexeme `Deleted) k d
      | "SEEN" -> ret (`Lexeme `Seen) k d
      | "DRAFT" -> ret (`Lexeme `Draft) k d
      | s -> ret (`Lexeme (`Extension s)) k d
    in
    readc nxt d
  else
  let rec nxt d =
    if is_atom_char d.c then (badd d; readc nxt d) else
    ret (`Lexeme (`Keyword (buf d))) k d
  in
  nxt d

(*
flag-fetch      = flag / "\Recent"
*)
let r_flag_fetch k d =
  if d.c = u_bslash then
    let rec nxt d =
      if is_atom_char d.c then (baddu d; readc nxt d) else
      match buf d with
      | "ANSWERED" -> ret (`Lexeme `Answered) k d
      | "FLAGGED" -> ret (`Lexeme `Flagged) k d
      | "DELETED" -> ret (`Lexeme `Deleted) k d
      | "SEEN" -> ret (`Lexeme `Seen) k d
      | "DRAFT" -> ret (`Lexeme `Draft) k d
      | "RECENT" -> ret (`Lexeme `Recent) k d
      | s -> ret (`Lexeme (`Extension s)) k d
    in
    readc nxt d
  else
  let rec nxt d =
    if is_atom_char d.c then (badd d; readc nxt d) else
    ret (`Lexeme (`Keyword (buf d))) k d
  in
  nxt d

(*
flag-perm       = flag / "\*"
*)
let r_flag_perm k d =
  if d.c = u_bslash then
    let rec nxt d =
      if is_atom_char d.c then (baddu d; readc nxt d) else
      match buf d with
      | "ANSWERED" -> ret (`Lexeme `Answered) k d
      | "FLAGGED" -> ret (`Lexeme `Flagged) k d
      | "DELETED" -> ret (`Lexeme `Deleted) k d
      | "SEEN" -> ret (`Lexeme `Seen) k d
      | "DRAFT" -> ret (`Lexeme `Draft) k d
      | s -> ret (`Lexeme (`Extension s)) k d
    in
    readc (fun d -> if d.c = u_star then ret (`Lexeme `All) (readc k) d else nxt d) d
  else
  let rec nxt d =
    if is_atom_char d.c then (badd d; readc nxt d) else
    ret (`Lexeme (`Keyword (buf d))) k d
  in
  nxt d

(****)

let rec r_while f k d =
  if f d.c then (badd d; readc (r_while f k)) d else
  k (buf d) d

(*
CHAR           =  %x01-7F
                       ; any 7-bit US-ASCII character,
                         excluding NUL

text            = 1*TEXT-CHAR

TEXT-CHAR       = <any CHAR except CR and LF>
*)
let is_text_char c =
  match c with
  | 0x0D (* '\r' *)
  | 0x0A (* '\n' *) -> false
  | _ -> 0x01 <= c && c <= 0x7F

let rec r_text k d =
  if is_text_char d.c then (badd d; readc (r_text k) d) else
  ret (`Lexeme (`String (buf d))) k d

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
*)
let is_text_1_char c =
  match c with
  | 0x0D (* '\r' *)
  | 0x0A (* '\n' *)
  | 0x5D (* ']' *) -> false
  | _ -> 0x01 <= c && c <= 0x7F
(* let is_text_other_char c = *)
(*   is_text_char c && (c <> ']') *)

let rec r_text_1 k d =
  if is_text_1_char d.c then (badd d; readc (r_text_1 k) d) else
  ret (`Lexeme (`String (buf d))) k d

let rec r_resp_text_code k d =
  (* let extension = extension_parser RESP_TEXT_CODE >>= fun e -> ret (RESP_TEXT_CODE_EXTENSION e) in *)
  if is_atom_char d.c then (baddu d; readc (r_resp_text_code k) d) else
  match buf d with
  | "ALERT" -> ret (`Lexeme `Alert) k d
  | "BADCHARSET" ->
      let nxt d = if d.c = u_sp then readc (r_list1 r_astring k) d else k d in
      ret (`Lexeme `Bad_charset) nxt d
  | "CAPABILITY" ->
      let rec nxt d = if d.c = u_sp then readc (r_atom nxt) d else k d in
      ret (`Lexeme `Capability) nxt d
  | "PARSE" -> ret (`Lexeme `Parse) k d
  | "PERMANENTFLAGS" -> ret (`Lexeme `Permanent_flags) (r_sp (r_list r_flag_perm k)) d
  | "READ-ONLY" -> ret (`Lexeme `Read_only) k d
  | "READ-WRITE" -> ret (`Lexeme `Read_write) k d
  | "TRYCREATE" -> ret (`Lexeme `Try_create) k d
  | "UIDNEXT" -> ret (`Lexeme `Uid_next) (r_sp (r_nz_number k)) d
  | "UIDVALIDITY" -> ret (`Lexeme `Uid_validity) (r_sp (r_nz_number k)) d
  | "UNSEEN" -> ret (`Lexeme `Unseen) (r_sp (r_nz_number k)) d
  | _ as a ->
      let nxt d = if d.c = u_sp then readc (r_text_1 k) d else k d in
      ret (`Lexeme (`String a)) nxt d

(*
resp-text       = ["[" resp-text-code "]" SP] text
*)
let r_resp_text k d =
  if d.c = u_lbrack then
    let nxt d = if d.c = u_sp then readc (r_text k) d else k d in
    readc (r_resp_text_code (accept_c ']' nxt)) d
  else
  r_text k d

(*
resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text
                    ; Status condition
*)
let rec r_resp_cond_state k d =
  if is_atom_char d.c then (baddu d; readc (r_resp_cond_state k) d) else
  match buf d with
  | "OK" -> ret (`Lexeme `Ok) (r_sp (r_resp_text k)) d
  | "NO" -> ret (`Lexeme `No) (r_sp (r_resp_text k)) d
  | "BAD" -> ret (`Lexeme `Bad) (r_sp (r_resp_text k)) d
  | s -> ret (err_unexpected_s s) k d

(****)

(*
date-day-fixed  = (SP DIGIT) / 2DIGIT
                    ; Fixed-format version of date-day
*)
(* let r_date_day_fixed k d = *)
(*   if d.c = 0x20 (\* ' ' *\) then *)
(*     readc *)
(*       (fun d -> if is_digit d.c then ret (`Lexeme (`Day (d.c - Char.code '0'))) k d else *)
(*         ret (err_unexpected d.c) k d) d *)
(*   else *)
(*   if is_digit d.c then *)
(*     let n = (d.c - Char.code '0') * 10 in *)
(*     readc (fun d -> if is_digit d.c then ret (`Lexeme (`Day (n + (d.c - Char.code '0')))) k d else *)
(*             ret (err_unexpected d.c) k d) d *)
(*   else *)
(*   ret (err_unexpected d.c) k d *)
  (* alt (char ' ' >> digits1) digits2 *)

(*
date-month      = "Jan" / "Feb" / "Mar" / "Apr" / "May" / "Jun" /
                  "Jul" / "Aug" / "Sep" / "Oct" / "Nov" / "Dec"
*)
(* let r_date_month k d = *)
(*   let m = String.create 3 in *)
(*   let i = ref 0 in *)
(*   let r k d = *)
(*     if is_alpha_char d.c then begin *)
(*       m.[i] <- Char.uppercase (Char.chr d.c); *)
(*       incr i; *)
(*       k d *)
(*     end else *)
(*     ret (err_unexpected d.c) k d *)
(*   in *)
(*   let nxt d = *)
(*     match m with *)
(*     | "JAN" -> ret (`Lexeme (`Month `Jan)) k d *)
(*     | "FEB" -> ret (`Lexeme (`Month `Feb)) k d *)
(*     | "MAR" -> ret (`Lexeme (`Month `Mar)) k d *)
(*     | "APR" -> ret (`Lexeme (`Month `Apr)) k d *)
(*     | "MAY" -> ret (`Lexeme (`Month `May)) k d *)
(*     | "JUN" -> ret (`Lexeme (`Month `Jun)) k d *)
(*     | "JUL" -> ret (`Lexeme (`Month `Jul)) k d *)
(*     | "AUG" -> ret (`Lexeme (`Month `Aug)) k d *)
(*     | "SEP" -> ret (`Lexeme (`Month `Sep)) k d *)
(*     | "OCT" -> ret (`Lexeme (`Month `Oct)) k d *)
(*     | "NOV" -> ret (`Lexeme (`Month `Nov)) k d *)
(*     | "DEC" -> ret (`Lexeme (`Month `Dec)) k d *)
(*     | s -> ret (err_unexpected_s s) k d *)
(*   in *)
(*   r (r (r nxt)) d *)
  (* app String.capitalize (string_of_length 3) >>= function *)
  (* | "Jan" -> ret 1 *)
  (* | "Feb" -> ret 2 *)
  (* | "Mar" -> ret 3 *)
  (* | "Apr" -> ret 4 *)
  (* | "May" -> ret 5 *)
  (* | "Jun" -> ret 6 *)
  (* | "Jul" -> ret 7 *)
  (* | "Aug" -> ret 8 *)
  (* | "Sep" -> ret 9 *)
  (* | "Oct" -> ret 10 *)
  (* | "Nov" -> ret 11 *)
  (* | "Dec" -> ret 12 *)
  (* | _ -> fail *)

(* let time = *)
(*   digits2 >>= fun hours -> char ':' >> *)
(*   digits2 >>= fun minutes -> char ':' >> *)
(*   digits2 >>= fun seconds -> *)
(*   ret (hours, minutes, seconds) *)

(* let zone = *)
(*   alt (char '+' >> ret 1) (char '-' >> ret (-1)) >>= fun sign -> *)
(*   digits2 >>= fun hh -> *)
(*   digits2 >>= fun mm -> *)
(*   ret (sign * (60 * hh + mm)) *)

(*
date-year       = 4DIGIT
*)
(* let r_date_year k d = *)
(*   let y = ref 0 in *)
(*   let nxt k d = *)
(*     if is_digit d.c then (y := !y * 10 + (d.c - Char.code '0'); k d) else *)
(*     ret (err_unexpected d.c) k d *)
(*   in *)
(*   nxt (nxt (nxt (nxt (fun d -> ret (`Lexeme (`Year !y)) k d)))) d *)

(*
time            = 2DIGIT ":" 2DIGIT ":" 2DIGIT
                    ; Hours minutes seconds
*)
(* let r_time k d = *)
(*   let t = ref 0 in *)
(*   let nxt k d = *)
(*     if is_digit d.c then (t := !t * 10 + (d.c - Char.code '0'); k d) else *)
(*     ret (err_unexpected d.c) k d *)
(*   in *)
(*   nxt (nxt (fun d -> *)

(*
date-time       = DQUOTE date-day-fixed "-" date-month "-" date-year
                  SP time SP zone DQUOTE
*)
let r_date_time k d =
  readl 28 (fun d -> ret (`Lexeme (`Date_time (lit d))) k d) d
  (* accept_c '\"' ( *)
  (* r_date_day_fixed (fun y -> *)
  (* accept_c '-' ( *)
  (* r_date_month ( *)
  (* accept_c '-' ( *)
  (*   r_date_year ( *)
  (*     ret *)
  (* accept_c ' ' ( *)
  (* r_time ( *)
  (* accept ' ' ( *)
  (* r_zone ( *)
  (* accept '\"' k)))))))))) d *)

  (* char '\"' >> *)
  (* date_day_fixed >>= fun dt_day -> *)
  (* char '-' >> date_month >>= fun dt_month -> *)
  (* char '-' >> digits4 >>= fun dt_year -> *)
  (* char ' ' >> time >>= fun (dt_hour, dt_min, dt_sec) -> *)
  (* char ' ' >> zone >>= fun dt_zone -> *)
  (* char '\"' >> *)
  (* ret {dt_day; dt_month; dt_year; dt_hour; dt_min; dt_sec; dt_zone} *)

(****)

(*
header-fld-name = astring

header-list     = "(" header-fld-name *(SP header-fld-name) ")"

section-text    = section-msgtext / "MIME"
                    ; text other than actual body part (headers, etc.)

section-part    = nz-number *("." nz-number)
                    ; body part nesting

section-msgtext = "HEADER" / "HEADER.FIELDS" [".NOT"] SP header-list /
                  "TEXT"
                    ; top-level or MESSAGE/RFC822 part

section-spec    = section-msgtext / (section-part ["." section-text])

section         = "[" [section-spec] "]"
*)

let r_section_part k d =
  let rec nxt d = if d.c = 0x2E (* '.' *) then readc nxt d else k d in
  r_nz_number nxt d

let rec r_section_text k d =
  if is_atom_char d.c then (baddu d; readc (r_section_text k) d) else
  match buf d with
  | "HEADER" -> ret (`Lexeme `Header) k d
  | "HEADER.FIELDS" -> ret (`Lexeme `Header_fields) (r_list1 r_astring k) d
  | "HEADER.FIELDS.NOT" -> ret (`Lexeme `Header_fields_not) (r_list1 r_astring k) d
  | "TEXT" -> ret (`Lexeme `Text) k d
  | "MIME" -> ret (`Lexeme `Mime) k d
  | s -> ret (err_unexpected_s s) k d

let rec r_section_msgtext k d =
  if is_atom_char d.c then (baddu d; readc (r_section_msgtext k) d) else
  match buf d with
  | "HEADER" -> ret (`Lexeme `Header) k d
  | "HEADER.FIELDS" -> ret (`Lexeme `Header_fields) (r_sp (r_list1 r_astring k)) d
  | "HEADER.FIELDS.NOT" -> ret (`Lexeme `Header_fields_not) (r_sp (r_list1 r_astring k)) d
  | "TEXT" -> ret (`Lexeme `Text) k d
  | s -> ret (err_unexpected_s s) k d

let r_section k d =
  let nxt d =
    if d.c = u_rbrack then readc k d else
    if is_digit d.c then r_section_part (accept_c '.' (r_section_text k)) d else
    r_section_msgtext k d
  in
  accept_c '[' nxt d

(****)


(* let media_subtype = *)
(*   imap_string *)

(*
*)
(* let media_basic = *)
(*   let table = [ *)
(*     "APPLICATION", MEDIA_BASIC_APPLICATION; *)
(*     "AUDIO", MEDIA_BASIC_AUDIO; *)
(*     "IMAGE", MEDIA_BASIC_IMAGE; *)
(*     "MESSAGE", MEDIA_BASIC_MESSAGE; *)
(*     "VIDEO", MEDIA_BASIC_VIDEO *)
(*   ] *)
(*   in *)
(*   let media_basic' = *)
(*     imap_string >>= fun s -> *)
(*     ret (try List.assoc (String.uppercase s) table with Not_found -> MEDIA_BASIC_OTHER s) *)
(*   in *)
(*   media_basic' >>= fun med_basic_type -> char ' ' >> media_subtype >>= fun med_basic_subtype -> *)
(*   ret {med_basic_type; med_basic_subtype} *)

(*
body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil
*)
(* let body_fld_param = *)
(*   let param = imap_string >>= fun k -> char ' ' >> imap_string >>= fun v -> ret (k, v) in *)
(*   alt *)
(*     (char '(' >> sep1 (char ' ') param >>= fun xs -> char ')' >> ret xs) *)
(*     (nil >> ret []) *)

(*
body-fld-enc    = (DQUOTE ("7BIT" / "8BIT" / "BINARY" / "BASE64"/
                  "QUOTED-PRINTABLE") DQUOTE) / string
*)
(* let body_fld_enc = *)
(*   let table = [ *)
(*     "7BIT", BODY_FLD_ENC_7BIT; *)
(*     "8BIT", BODY_FLD_ENC_8BIT; *)
(*     "BINARY", BODY_FLD_ENC_BINARY; *)
(*     "BASE64", BODY_FLD_ENC_BASE64; *)
(*     "QUOTED-PRINTABLE", BODY_FLD_ENC_QUOTED_PRINTABLE *)
(*   ] *)
(*   in *)
(*   imap_string >>= fun s -> *)
(*   ret (try List.assoc (String.uppercase s) table with Not_found -> BODY_FLD_ENC_OTHER s) *)

(*
body-fld-id     = nstring
*)
(* let body_fld_id = *)
(*   nstring *)

(*
body-fld-desc   = nstring
*)
(* let body_fld_desc = *)
(*   nstring *)

(*
body-fld-octets = number
*)
(* let body_fld_octets = *)
(*   number' *)

(*
body-fields     = body-fld-param SP body-fld-id SP body-fld-desc SP
                  body-fld-enc SP body-fld-octets
*)
(* let body_fields = *)
(*   body_fld_param >>= fun bd_parameter -> *)
(*   char ' ' >> body_fld_id >>= fun bd_id -> *)
(*   char ' ' >> body_fld_desc >>= fun bd_description -> *)
(*   char ' ' >> body_fld_enc >>= fun bd_encoding -> *)
(*   char ' ' >> body_fld_octets >>= fun bd_size -> *)
(*   ret {bd_parameter; bd_id; bd_description; bd_encoding; bd_size} *)

(*
*)
(* let media_message = *)
(*   char '\"' >> str "MESSAGE" >> char '\"' >> char ' ' >> *)
(*   char '\"' >> str "RFC822" >> char '\"' *)

(*
*)
(* let media_text = *)
(*   char '\"' >> str "TEXT" >> char '\"' >> char ' ' >> media_subtype *)

(*
body-fld-md5    = nstring
*)
(* let body_fld_md5 = *)
(*   nstring *)

(*
body-fld-dsp    = "(" string SP body-fld-param ")" / nil
*)
(* let body_fld_dsp = *)
(*   alt *)
(*     begin *)
(*       char '(' >> *)
(*       imap_string >>= fun dsp_type -> *)
(*       char ' ' >> *)
(*       body_fld_param >>= fun dsp_attributes -> *)
(*       char ')' >> *)
(*       ret (Some {dsp_type; dsp_attributes}) *)
(*     end *)
(*     (nil >> ret None) *)

(*
body-fld-lang   = nstring / "(" string *(SP string) ")"
*)
(* let body_fld_lang = *)
(*   alt *)
(*     (char '(' >> sep1 (char ' ') imap_string >>= fun xs -> char ')' >> ret (BODY_FLD_LANG_LIST xs)) *)
(*     (nstring >>= fun s -> ret (BODY_FLD_LANG_SINGLE s)) *)

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
(* let rec body_extension () = *)
(*   altn [ *)
(*     (char '(' >> sep1 (char ' ') (delay body_extension ()) >>= fun xs -> *)
(*      char ')' >> ret (BODY_EXTENSION_LIST xs)); *)
(*     (number >>= fun n -> ret (BODY_EXTENSION_NUMBER n)); *)
(*     (nstring >>= fun s -> ret (BODY_EXTENSION_NSTRING s)) *)
(*   ] *)

(* let body_fld_loc = *)
(*   nstring *)

(*
body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang
                  [SP body-fld-loc *(SP body-extension)]]]
                    ; MUST NOT be returned on non-extensible
                    ; "BODY" fetch
*)
(* let body_ext_1part = *)
(*   body_fld_md5 >>= fun bd_md5 -> *)
(*   opt *)
(*     begin *)
(*       char ' ' >> body_fld_dsp >>= fun bd_disposition -> *)
(*       opt *)
(*         begin *)
(*           char ' ' >> some body_fld_lang >>= fun bd_language -> *)
(*           opt *)
(*             begin *)
(*               char ' ' >> body_fld_loc >>= fun bd_loc -> *)
(*               rep (char ' ' >> delay body_extension ()) >>= fun bd_extension_list -> *)
(*               ret {bd_md5; bd_disposition; bd_language; bd_loc; bd_extension_list} *)
(*             end *)
(*             {bd_md5; bd_disposition; bd_language; bd_loc = None; bd_extension_list = []} *)
(*         end *)
(*         {bd_md5; bd_disposition; bd_language = None; bd_loc = None; bd_extension_list = []} *)
(*     end *)
(*     {bd_md5 = None; bd_disposition = None; bd_language = None; bd_loc = None; bd_extension_list = []} *)

(*
body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang
                  [SP body-fld-loc *(SP body-extension)]]]
                    ; MUST NOT be returned on non-extensible
                    ; "BODY" fetch
*)
(* let body_ext_mpart = *)
(*   char ' ' >> body_fld_param >>= fun bd_parameter -> *)
(*   opt *)
(*     begin *)
(*       char ' ' >> body_fld_dsp >>= fun bd_disposition -> *)
(*       opt *)
(*         begin *)
(*           char ' ' >> some body_fld_lang >>= fun bd_language -> *)
(*           opt *)
(*             begin *)
(*               char ' ' >> body_fld_loc >>= fun bd_loc -> *)
(*               rep (char ' ' >> delay body_extension ()) >>= fun bd_extension_list -> *)
(*               ret {bd_parameter; bd_disposition; bd_language; bd_loc; bd_extension_list} *)
(*             end *)
(*             {bd_parameter; bd_disposition; bd_language; bd_loc = None; bd_extension_list = []} *)
(*         end *)
(*         {bd_parameter; bd_disposition; bd_language = None; bd_loc = None; bd_extension_list = []} *)
(*     end *)
(*     {bd_parameter; bd_disposition = None; bd_language = None; bd_loc = None; bd_extension_list = []} *)

(*
media-subtype   = string
                    ; Defined in [MIME-IMT]

media-basic     = ((DQUOTE ("APPLICATION" / "AUDIO" / "IMAGE" /
                  "MESSAGE" / "VIDEO") DQUOTE) / string) SP
                  media-subtype
                    ; Defined in [MIME-IMT]

body-type-basic = media-basic SP body-fields
                    ; MESSAGE subtype MUST NOT be "RFC822"

media-message   = DQUOTE "MESSAGE" DQUOTE SP DQUOTE "RFC822" DQUOTE
                    ; Defined in [MIME-IMT]

body-type-msg   = media-message SP body-fields SP envelope
                  SP body SP body-fld-lines

media-text      = DQUOTE "TEXT" DQUOTE SP media-subtype
                    ; Defined in [MIME-IMT]

body-type-text  = media-text SP body-fields SP body-fld-lines
*)
(* let rec body_type_basic () = *)
(*   media_basic >>= fun bd_media_basic -> *)
(*   char ' ' >> body_fields >>= fun bd_fields -> *)
(*   ret {bd_media_basic; bd_fields} *)

(* and body_type_msg () = *)
(*   media_message >> char ' ' >> body_fields >>= fun bd_fields -> *)
(*   char ' ' >> envelope >>= fun bd_envelope -> *)
(*   char ' ' >> delay body () >>= fun bd_body -> *)
(*   ret {bd_fields; bd_envelope; bd_body} *)

(* and body_type_text () = *)
(*   media_text >>= fun bd_media_text -> *)
(*   char ' ' >> body_fields >>= fun bd_fields -> *)
(*   char ' ' >> number' >>= fun bd_lines -> *)
(*   ret {bd_media_text; bd_fields; bd_lines} *)

(* and body_type_1part () = *)
(*   altn [ *)
(*     (delay body_type_msg () >>= fun b -> ret (BODY_TYPE_1PART_MSG b)); *)
(*     (delay body_type_text () >>= fun b -> ret (BODY_TYPE_1PART_TEXT b)); *)
(*     (delay body_type_basic () >>= fun b -> ret (BODY_TYPE_1PART_BASIC b)) *)
(*   ] >>= fun bd_data -> *)
(*   opt *)
(*     (char ' ' >> body_ext_1part) *)
(*     {bd_md5 = None; bd_disposition = None; bd_language = None; bd_loc = None; bd_extension_list = []} *)
(*   >>= fun bd_ext_1part -> *)
(*   ret {bd_data; bd_ext_1part} *)

(* and body_type_mpart () = *)
(*   rep1 (delay body ()) >>= fun bd_list -> *)
(*   char ' ' >> media_subtype >>= fun bd_media_subtype -> *)
(*   body_ext_mpart >>= fun bd_ext_mpart -> *)
(*   ret {bd_list; bd_media_subtype; bd_ext_mpart} *)

(*
body-type-1part = (body-type-basic / body-type-msg / body-type-text)
                  [SP body-ext-1part]

body-type-mpart = 1*body SP media-subtype
                  [SP body-ext-mpart]

body            = "(" (body-type-1part / body-type-mpart) ")"
*)
(* and body k d = *)
(*   char '(' >> *)
(*   alt *)
(*     (delay body_type_1part () >>= fun b -> ret (BODY_1PART b)) *)
(*     (delay body_type_mpart () >>= fun b -> ret (BODY_MPART b)) *)
(*   >>= fun b -> *)
(*   char ')' >> *)
(*   ret b *)

let rec r_body k d =
  ret (err_expecteds "crash") k d
  (* let nxt d = *)
  (*   if d.c = u_lpar then r_body_mpart (accept_c ')' k) d else *)
  (*   reads (fun d -> *)
  (*       match buf d with *)
  (*       | " *)
  (* in *)
  (* accept_c '(' nxt d *)

(****)

(* let msg_att_static = *)
(*   let body_ = *)
(*     let section_ sec_section = *)
(*       opt (char '<' >> some number' >>= fun x -> char '>' >> ret x) None >>= fun sec_origin_octet -> *)
(*       char ' ' >> nstring' >>= fun sec_body_part -> *)
(*       ret (MSG_ATT_BODY_SECTION {sec_section; sec_origin_octet; sec_body_part}) *)
(*     in *)
(*     altn [ *)
(*       (char ' ' >> delay body () >>= fun b -> ret (MSG_ATT_BODY b)); *)
(*       (section >>= section_) *)
(*     ] *)
(*   in *)
(*   altn [ *)
(*     (str "ENVELOPE" >> char ' ' >> envelope >>= fun e -> ret (MSG_ATT_ENVELOPE e)); *)
(*     (str "INTERNALDATE" >> char ' ' >> date_time >>= fun dt -> ret (MSG_ATT_INTERNALDATE dt)); *)
(*     (str "RFC822.HEADER" >> char ' ' >> nstring' >>= fun s -> ret (MSG_ATT_RFC822_HEADER s)); *)
(*     (str "RFC822.TEXT" >> char ' ' >> nstring' >>= fun s -> ret (MSG_ATT_RFC822_TEXT s)); *)
(*     (str "RFC822.SIZE" >> char ' ' >> number' >>= fun n -> ret (MSG_ATT_RFC822_SIZE n)); *)
(*     (str "RFC822" >> char ' ' >> nstring' >>= fun s -> ret (MSG_ATT_RFC822 s)); *)
(*     (str "BODYSTRUCTURE" >> char ' ' >> delay body () >>= fun b -> ret (MSG_ATT_BODYSTRUCTURE b)); *)
(*     (str "BODY" >> body_); *)
(*     (str "UID" >> char ' ' >> nz_number >>= fun uid -> ret (MSG_ATT_UID uid)) *)
(*   (\*   (str "X-GM-THRID" >> char ' ' >> uint64 >>= fun n -> ret (MSG_ATT_X_GM_THRID (Gthrid.of_uint64 n))) *\) *)
(*   ] *)

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
*)
let rec r_msg_att k d =
  if is_atom_char d.c then (badd d; readc (r_msg_att k) d) else
  match buf d with
  | "FLAGS" -> ret (`Lexeme `Flags) (r_sp (r_list r_flag_fetch k)) d
  (* | "ENVELOPE" -> ret (`Lexeme `Envelope) (accept_c ' ' (r_envelope k)) d *)
  | "INTERNALDATE" -> ret (`Lexeme `Internal_date) (r_sp (r_date_time k)) d
  | "RFC822.HEADER" -> ret (`Lexeme `Rfc822_header) (r_sp (r_nstring k)) d
  | "RFC822.TEXT" -> ret (`Lexeme `Rfc822_text) (r_sp (r_nstring k)) d
  | "RFC822.SIZE" -> ret (`Lexeme `Rfc822_size) (r_sp (r_number k)) d
  | "RFC822" -> ret (`Lexeme `Rfc822) (r_sp (r_nstring k)) d
  | "BODYSTRUCTURE" -> ret (`Lexeme `Body_structure) (r_sp (r_body k)) d
  | "BODY" ->
      let nxt d =
        if d.c = u_sp then readc (r_body k) d else
        if d.c = u_lang then readc (r_number (accept_c '>' (r_sp (r_nstring k)))) d else
        r_sp (r_nstring k) d
      in
      ret (`Lexeme `Body) nxt d
  | "UID" -> ret (`Lexeme `Uid) (r_sp (r_nz_number k)) d
  | s -> ret (err_unexpected_s s) k d

(****)

(*
mbx-list-sflag  = "\Noselect" / "\Marked" / "\Unmarked"
                    ; Selectability flags; only one per LIST response

mbx-list-oflag  = "\Noinferiors" / flag-extension
                    ; Other flags; multiple possible per LIST response

mbx-list-flags  = *(mbx-list-oflag SP) mbx-list-sflag
                  *(SP mbx-list-oflag) /
                  mbx-list-oflag *(SP mbx-list-oflag)
*)
let r_mbx_list_flag k d =
  let rec nxt d =
    if is_atom_char d.c then (baddu d; readc nxt d) else
    match buf d with
    | "NOSELECT" -> ret (`Lexeme `Noselect) k d
    | "MARKED" -> ret (`Lexeme `Marked) k d
    | "UNMARKED" -> ret (`Lexeme `Unmarked) k d
    | "NOINFERIORS" -> ret (`Lexeme `Noinferiors) k d
    | s -> ret (`Lexeme (`Extension s)) k d
  in
  accept_c '\\' nxt d

(*
mailbox-list    = "(" [mbx-list-flags] ")" SP
                   (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox
*)
let r_mailbox k d =
  let pp s = try (* decode_mutf7 *) s with _ -> s in
  r_astring ~pp k d
  (* let decode_mailbox_name s = try ImapUtils.decode_mutf7 s with _ -> s in *)
  (* astring >>= fun s -> ret (decode_mailbox_name s) *)
let r_delim k d =
  match d.c with
  | 0x4E (* N *) | 0x6E (* n *) -> accept_s "NIL" (ret (`Lexeme `Nil) k) d
  | _ -> accept_c '\"' (r_quoted_char (accept_c '\"' k)) d
let r_mailbox_list k d =
  r_list r_mbx_list_flag (r_sp (r_delim (r_sp (r_mailbox k)))) d

(* let mailbox_data_extension_data = *)
(*   app (fun e -> MAILBOX_DATA_EXTENSION_DATA e) (extension_parser MAILBOX_DATA) *)

(*
status          = "STATUS" SP mailbox SP
                  "(" status-att *(SP status-att) ")"

status-att      = "MESSAGES" / "RECENT" / "UIDNEXT" / "UIDVALIDITY" /
                  "UNSEEN"

status-att-list =  status-att SP number *(SP status-att SP number)
*)
let rec r_status_att k d =
  if is_alpha_char d.c then (baddu d; readc (r_status_att k) d) else
  match buf d with
  | "MESSAGES" -> ret (`Lexeme `Messages) k d
  | "RECENT" -> ret (`Lexeme `Recent) k d
  | "UIDNEXT" -> ret (`Lexeme `Uid_next) k d
  | "UIDVALIDITY" -> ret (`Lexeme `Uid_validity) k d
  | "UNSEEN" -> ret (`Lexeme `Unseen) k d
  | s -> ret (err_unexpected_s s) k d

(*
message-data    = nz-number SP ("EXPUNGE" / ("FETCH" SP msg-att))

mailbox-data    =  "FLAGS" SP flag-list / "LIST" SP mailbox-list /
                   "LSUB" SP mailbox-list / "SEARCH" *(SP nz-number) /
                   "STATUS" SP mailbox SP "(" [status-att-list] ")" /
                   number SP "EXISTS" / number SP "RECENT"

response-data   = "*" SP (resp-cond-state / resp-cond-bye /
                  mailbox-data / message-data / capability-data) CRLF
*)
let rec r_response_data k d =
  if is_atom_char d.c then (baddu d; readc (r_response_data k) d) else
  match buf d with
  | "OK" -> ret (`Lexeme `Ok) (r_sp (r_resp_text (r_crlf k))) d
  | "NO" -> ret (`Lexeme `No) (r_sp (r_resp_text (r_crlf k))) d
  | "BAD" -> ret (`Lexeme `Bad) (r_sp (r_resp_text (r_crlf k))) d
  | "BYE" -> ret (`Lexeme `Bye) (r_sp (r_resp_text (r_crlf eof))) d
  | "FLAGS" -> ret (`Lexeme `Flags) (r_sp (r_list r_flag (r_crlf k))) d
  | "LIST" -> ret (`Lexeme `List) (r_sp (r_mailbox_list (r_crlf k))) d
  | "LSUB" -> ret (`Lexeme `Lsub) (r_sp (r_mailbox_list (r_crlf k))) d
  | "SEARCH" ->
      let rec nxt d = if d.c = 0x20 then readc (r_nz_number nxt) d else r_crlf k d in
      ret (`Lexeme `Search) nxt d
  | "STATUS" ->
      let r_status_att_item k d = r_status_att (r_sp (r_number k)) d in
      let r_status k d = r_mailbox (r_sp (r_list r_status_att_item k)) d in
      ret (`Lexeme `Status) (r_sp (r_status (r_crlf k))) d
  | "CAPABILITY" ->
      let rec nxt d = if d.c = u_sp then readc (r_atom nxt) d else r_crlf k d in
      ret (`Lexeme `Capability) nxt d
  | s -> ret (err_unexpected_s s) eof d (* FIXME *)

let r_response_data_n k d =
  let rec nxt d =
    if is_atom_char d.c then (baddu d; readc nxt d) else
    match buf d with
    | "EXPUNGE" -> ret (`Lexeme `Expunge) (r_crlf k) d
    | "FETCH" -> ret (`Lexeme `Fetch) (r_sp (r_list1 r_msg_att (r_crlf k))) d
    | "EXISTS" -> ret (`Lexeme `Exists) (r_crlf k) d
    | "RECENT" -> ret (`Lexeme `Recent) (r_crlf k) d
    | s -> ret (err_unexpected_s s) eof d (* FIXME *)
  in
  (* fixme EXPUNGE FETCH use nz-number *)
  r_number (r_sp nxt) d

let r_untagged k d =
  let nxt d = if is_digit d.c then r_response_data_n k d else r_response_data k d in
  accept_c '*' (ret (`Lexeme `No_tag) (r_sp nxt)) d

(****)

(*
continue-req    = "+" SP (resp-text / base64) CRLF
*)
let r_continue_req k d =
  let rec nxt d =
    match d.c with
    | 0x0D (* '\r' *) ->
        ret (`Lexeme (`String (buf d))) (readc (accept_c '\n' k)) d
    | _ -> (* FIXME check for TEXT-CHAR *)
        (badd d; readc nxt d)
  in
  accept_c '+' (ret (`Lexeme `Plus) (fun d -> if d.c = u_sp then readc nxt d else nxt d)) d
  (* char '+' >> opt (char ' ') '\000' >> (\* we allow an optional space *\) *)
  (* alt *)
  (*   (base64 >>= fun b64 -> crlf >> ret (CONTINUE_REQ_BASE64 b64)) *)
  (*   (resp_text >>= fun rt -> crlf >> ret (CONTINUE_REQ_TEXT rt)) *)

(*
tag             = 1*<any ASTRING-CHAR except "+">
*)
let is_tag_char c =
  is_astring_char c && c <> u_plus
let rec r_tag k d =
  if is_tag_char d.c then (badd d; readc (r_tag k) d) else
  ret (`Lexeme (`Tag (buf d))) k d

(*
resp-cond-bye   = "BYE" SP resp-text

response-fatal  = "*" SP resp-cond-bye CRLF
                    ; Server closes connection immediately

response-tagged = tag SP resp-cond-state CRLF

response-done   = response-tagged / response-fatal

response        = *(continue-req / response-data) response-done
*)
let r_tagged k d =
  r_tag (r_sp (r_resp_cond_state (r_crlf k))) d
let rec r_response d =
  if d.c = u_plus then r_continue_req r_response d else
  if d.c = u_star then r_untagged r_response d else
  r_tagged r_response d

(****)

let rec r_lexeme d =
  if d.c = ux_soi then readc r_lexeme d else
  r_response d

let io_buffer_size = 4096

let decode d = d.k d

let decoder src =
  let i, i_pos, i_max = match src with
    | `Manual -> "", max_int, 0
    | `String s -> s, 0, String.length s - 1
    | `Channel _ -> String.create io_buffer_size, max_int, 0
  in
  { src; i; i_pos; i_max; c = ux_soi; l = "";
    buf = Buffer.create 0; k = r_lexeme }

(* let run p b i = p b i *)

(* let run_string p s = *)
(*   let b = Buffer.create 0 in *)
(*   Buffer.add_string b s; *)
(*   let rec loop = function *)
(*     | Ok (x, _) -> Some x *)
(*     | Fail _ -> None *)
(*     | Need k -> loop (k End) *)
(*   in *)
(*   loop (p b 0) *)

(* let bind p f b i = *)
(*   let rec loop = *)
(*     function *)
(*       Ok (x, i) -> f x b i *)
(*     | Fail _ as x -> x *)
(*     | Need k -> Need (fun inp -> loop (k inp)) *)
(*   in *)
(*   loop (p b i) *)

(* let rec rep p = *)
(*   alt (rep1 p) (ret []) *)

(* and rep1 p = *)
(*   p >>= fun x -> rep p >>= fun xs -> ret (x :: xs) *)

(* let sep1 s p = *)
(*   p >>= fun x -> rep (s >> p) >>= fun xs -> ret (x :: xs) *)

(* let sep s p = *)
(*   alt (sep1 s p) (ret []) *)

(* let rec rep_ p = *)
(*   alt (rep1_ p) (ret ()) *)

(* and rep1_ p = *)
(*   p >>= fun _ -> rep_ p *)

(* let take p b i = *)
(*   let i0 = i in *)
(*   let rec loop = function *)
(*     | Ok (_, i) -> Ok (Buffer.sub b i0 (i - i0), i) *)
(*     | Fail _ as fail -> fail *)
(*     | Need k -> Need (fun inp -> loop (k inp)) *)
(*   in *)
(*   loop (p b i) *)

(* let rec char c b i = *)
(*   if i >= Buffer.length b then *)
(*     Need (function End -> Fail i | More -> char c b i) *)
(*   else if Buffer.nth b i = c then *)
(*     Ok (c, i + 1) *)
(*   else *)
(*     Fail i *)

(* let rec any_char b i = *)
(*   if i >= Buffer.length b then *)
(*     Need (function End -> Fail i | More -> any_char b i) *)
(*   else *)
(*     Ok (Buffer.nth b i, i + 1) *)

(* let satisfy f = *)
(*   any_char >>= fun c -> if f c then ret c else fail *)

(* let skip_while f = *)
(*   rep1_ (satisfy f) *)

(* let delay f x b i = *)
(*   f x b i *)

(* let skip n b i = *)
(*   let rec loop () = *)
(*     let left = Buffer.length b - i in *)
(*     if n <= left then *)
(*       Ok ((), i + n) *)
(*     else *)
(*       Need (function End -> Fail (i + left) | More -> loop ()) *)
(*   in *)
(*   loop () *)

(* let string_of_length n = *)
(*   take (skip n) *)

(* let str s k d = *)
(*   let rec k' i d = *)
(*     if i_pos + String.length s > i_max then *)
(*       readc k' i d *)
(*     else if (\* check *\) then *)
(*       k d *)
(*     else *)
(*       error *)
(*   in *)
(*   k' d *)

  (* if i_pos + String.length  *)
  (* let rec loop j = *)
  (*   if j >= String.length u then *)
  (*     Ok ((), i + String.length u) *)
  (*   else if i + j >= Buffer.length b then *)
  (*     Need (function End -> Fail (i + j) | More -> loop j) *)
  (*   else if Char.uppercase u.[j] = Char.uppercase (Buffer.nth b (i + j)) then *)
  (*     loop (j + 1) *)
  (*   else *)
  (*     Fail (i + j) *)
  (* in *)
  (* loop 0 *)

(* let rec accum f = *)
(*   take (skip_while f) *)

(* let r_char c k d = *)
(*   if d.c = c then k d else ret (err_char c d) k d *)

(* let r_digit k d = *)
(*   match d.c with *)
(*   | '0' .. '9' -> readc (k d) *)
(*   | *)
(*   satisfy (function '0' .. '9' -> true | _ -> false) *)

(* let nz_digit = *)
(*   satisfy (function '1' .. '9' -> true | _ -> false) *)

(* let nz_digits = *)
(*   take (nz_digit >> rep digit) *)

(* let crlf k d = *)
(*   str "\r\n" k d *)

(* type extension_parser = *)
(*   { parse : 'a. 'a extension_kind -> 'a t } *)

(* let extension_list = ref [] *)

(* let register_parser p = extension_list := p :: !extension_list *)

(* let is_digit = function '0' .. '9' -> true | _ -> false *)

(*
number          = 1*DIGIT
                    ; Unsigned 32-bit integer
                    ; (0 <= n < 4,294,967,296)
*)
(* let number k d = *)
(*   if is_digit d.c then (badd d; readc (number k) d) else *)
(*   let s = buf d in *)
(*   try ret (`Lexeme (`Uint32 (Uint32.of_string s))) k d with *)
(*   | Failure _ -> ret (err_number s) k d *)

(* let rec eof b i = *)
(*   if i >= Buffer.length b then *)
(*     Need (function End -> Ok ((), i) | More -> eof b i) *)
(*   else *)
(*     Fail i *)

(* let number' = *)
(*   app int_of_string (accum (function '0' .. '9' -> true | _ -> false)) *)

(*
quoted          = DQUOTE *QUOTED-CHAR DQUOTE
*)
(* let r_quoted k d = (\* DQUOTE was eaten *\) *)
(*   match d.c with *)
(*   | '\r' | '\n' -> ret (err_escape d.c) k d *)
(*   | '\"' -> readc k d *)
(*   | '\\' -> *)
(*       let esc d = match d.c with *)
(*         | '\\' | '"' -> (badd d; readc k d) *)
(*         | _ -> ret (err_escape d.c) k d *)
(*       in *)
(*       readc esc d *)
(*   | '\x01' .. '\x7f' -> (badd d; readc k d) *)
(*   | _ -> ret (err_escape d.c) k d *)

(*
literal         = "{" number "}" CRLF *CHAR8
                    ; Number represents the number of CHAR8s
*)
(* let literal = (\* { was eaten *\) *)
(*   (\* char '{' >> *\) number' >>= fun len -> char '}' >> crlf >> string_of_length len *)

(*
string          = quoted / literal
*)
(* let r_string k d = *)
(*   match d.c with *)
(*   | '\"' -> readc (r_quoted k) d *)
(*   | '{' -> readc (r_literal k) d *)
(*   | _ -> ret (err_string d) k d *)
(*   (\* alt quoted literal *\) *)

(*
astring         = 1*ASTRING-CHAR / string
*)
(* let is_astring_char = *)
(*   function *)
(*     '\x80' .. '\xff' | '(' | ')' *)
(*   | '{' | ' ' | '\x00' .. '\x1f' | '\x7f' *)
(*   | '%' | '*' | '\\' | '\"' -> false *)
(*   | _ -> true *)

(* let r_astring k d = *)
(*   if is_astring_char d.c then begin *)
(*     let rec eat d = *)
(*       if is_astring_char d.c then (badd d; readc eat d) else *)
(*       k d *)
(*     in *)
(*     badd d; readc eat d *)
(*   end else *)
(*   r_string k d *)
(*   (\* alt (accum is_astring_char) imap_string *\) *)

(*
atom            = 1*ATOM-CHAR

ATOM-CHAR       = <any CHAR except atom-specials>

atom-specials   = "(" / ")" / "{" / SP / CTL / list-wildcards /
                  quoted-specials / resp-specials

quoted-specials = DQUOTE / "\\"

resp-specials   = "]"

list-wildcards  = "%" / "*"
*)
(* let is_atom_char = function *)
(*   | '\x80' .. '\xff' *)
(*   | '(' | ')' | '{' | ' ' | '\x00' .. '\x1f' *)
(*   | '\x7f' | '%' | '*' | '\\' | '\"' | ']' -> false *)
(*   | _ -> true *)

(* let atom = *)
(*   accum is_atom_char *)

(*
TEXT-CHAR       = <any CHAR except CR and LF>

text            = 1*TEXT-CHAR
*)
(* let is_text_char = function *)
(*   | '\r' | '\n' | '\x80' .. '\xff' -> false *)
(*   | _ -> true *)

(* let text = *)
(*   accum is_text_char *)

(* (\* *)
(* nz-number       = digit-nz *DIGIT *)
(*                     ; Non-zero unsigned 32-bit integer *)
(*                     ; (0 < n < 4,294,967,296) *)
(* *\) *)
(* let r_nz_number k d = *)
(*   match d.c with *)
(*   | '1' .. '9' -> *)
(*       badd d; *)
(*       let rec loop d = *)
(*         match d.c with *)
(*         | '0' .. '9' -> *)
(*             badd d; readc loop d *)
(*         | _ -> *)
(*             k d *)
(*       in *)
(*       readc loop d *)
(*   | _ -> *)
(*       ret (err_nz_number d) k d *)
(*   (\* app Uint32.of_string nz_digits *\) *)

(* let nz_number' = *)
(*   app int_of_string nz_digits *)

(* let nil = *)
(*   str "nil" *)

(* let nstring = *)
(*   alt (some imap_string) (none nil) *)

(* let nstring' = *)
(*   app (function Some s -> s | None -> "") nstring *)

(* let digits1 = *)
(*   app (fun c -> Char.code c - Char.code '0') digit *)

(* let digits2 = *)
(*   app int_of_string (take (digit >> digit)) *)

(* let digits4 = *)
(*   app int_of_string (take (digit >> digit >> digit >> digit)) *)

(* let is_base64_char = function *)
(*   | 'a' .. 'z' | 'A' .. 'Z' *)
(*   | '0' .. '9' | '+' | '-' -> true *)
(*   | _ -> false *)

(* let base64_char = *)
(*   satisfy is_base64_char *)

(* let base64 = *)
(*   let base64_terminal = *)
(*     alt *)
(*       (base64_char >> base64_char >> base64_char >> str "=") *)
(*       (base64_char >> base64_char >> str "==") *)
(*   in *)
(*   take *)
(*     (rep_ (base64_char >> base64_char >> base64_char >> base64_char) >> base64_terminal) *)

(* let test p s = *)
(*   let b = Buffer.create 0 in *)
(*   let rec loop i = function *)
(*     | Ok (x, _) -> x *)
(*     | Fail _ -> failwith "parsing error" *)
(*     | Need k -> *)
(*         if i >= String.length s then *)
(*           loop i (k End) *)
(*         else *)
(*           begin *)
(*             Buffer.add_char b s.[i]; *)
(*             loop (i+1) (k More) *)
(*           end *)
(*   in *)
(*   loop 0 (p b 0) *)

(* (\** IMAP PARSER *\) *)

(* let extension_parser : type a. a extension_kind -> a t = fun kind -> *)
(*   List.fold_right (fun p q -> alt (p.parse kind) q) !extension_list fail *)

(* let extension_parser : type a. a extension_kind -> a t = fun kind -> *)
(*   delay extension_parser kind *)

(* (\* *)
(* auth-type       = atom *)
(*                     ; Defined by [SASL] *)
(* *\) *)
(* let r_auth_type k d = *)
(*   atom k d *)
(*   (\* atom >>= fun a -> ret (CAPABILITY_AUTH_TYPE a) *\) *)

(* (\* *)
(* capability      = ("AUTH=" auth-type) / atom *)
(*                     ; New capabilities MUST begin with "X" or be *)
(*                     ; registered with IANA as standard or *)
(*                     ; standards-track *)
(* *\) *)
(* let capability = *)
(*   alt *)
(*     (str "AUTH=" >> auth_type) *)
(*     (atom >>= fun a -> ret (CAPABILITY_NAME a)) *)

(* (\* *)
(* capability-data = "CAPABILITY" *(SP capability) SP "IMAP4rev1" *)
(*                   *(SP capability) *)
(*                     ; Servers MUST implement the STARTTLS, AUTH=PLAIN, *)
(*                     ; and LOGINDISABLED capabilities *)
(*                     ; Servers which offer RFC 1730 compatibility MUST *)
(*                     ; list "IMAP4" as the first capability. *)
(* *\) *)
(* let capability_data k d = *)
(*   let rec loop d = *)
(*     if d.c = ' ' then readc (capability (loop k)) d else *)
(*     k d *)
(*   in *)
(*   r_name "CAPABILITY" `Capability (r_space (loop k)) d *)
(*   (\* str "CAPABILITY" >> char ' ' >> sep1 (char ' ') capability *\) *)

(* (\* *)
(* flag-extension  = "\\" atom *)
(*                     ; Future expansion.  Client implementations *)
(*                     ; MUST accept flag-extension flags.  Server *)
(*                     ; implementations MUST NOT generate *)
(*                     ; flag-extension flags except as defined by *)
(*                     ; future standard or standards-track *)
(*                     ; revisions of this specification. *)
(* *\) *)
(* let flag_extension = *)
(*   char '\\' >> atom *)

(* (\* *)
(* flag-keyword    = atom *)
(* *\) *)
(* let flag_keyword = *)
(*   atom >>= fun s -> ret (FLAG_KEYWORD s) *)

(* (\* *)
(* flag            = "\Answered" / "\Flagged" / "\Deleted" / *)
(*                   "\Seen" / "\Draft" / flag-keyword / flag-extension *)
(*                     ; Does not include "\Recent" *)
(* *\) *)
(* let flag = *)
(*   alt *)
(*     begin *)
(*       char '\\' >> atom >>= fun s -> *)
(*       ret *)
(*         (match String.capitalize s with *)
(*          | "Answered" -> FLAG_ANSWERED *)
(*          | "Flagged" -> FLAG_FLAGGED *)
(*          | "Deleted" -> FLAG_DELETED *)
(*          | "Seen" -> FLAG_SEEN *)
(*          | "Draft" -> FLAG_DRAFT *)
(*          | _ -> FLAG_EXTENSION s) *)
(*     end *)
(*     flag_keyword *)

(* let flag_fetch = *)
(*   alt (flag >>= fun flag -> ret (FLAG_FETCH_OTHER flag)) (str "\\Recent" >> ret FLAG_FETCH_RECENT) *)

(* (\* *)
(* flag-perm       = flag / "\*" *)
(* *\) *)
(* let flag_perm = *)
(*   alt (app (fun f -> FLAG_PERM_FLAG f) flag) (str "\\*" >> ret FLAG_PERM_ALL) *)

(* let flag_list = *)
(*   char '(' >> sep (char ' ') flag >>= fun xs -> char ')' >> ret xs *)

(* (\* *)
(* uniqueid        = nz-number *)
(*                     ; Strictly ascending *)
(* *\) *)
(* let uniqueid = *)
(*   nz_number *)

(* (\* *)
(* seq-number      = nz-number / "*" *)
(*                     ; message sequence number (COPY, FETCH, STORE *)
(*                     ; commands) or unique identifier (UID COPY, *)
(*                     ; UID FETCH, UID STORE commands). *)
(*                     ; * represents the largest number in use.  In *)
(*                     ; the case of message sequence numbers, it is *)
(*                     ; the number of messages in a non-empty mailbox. *)
(*                     ; In the case of unique identifiers, it is the *)
(*                     ; unique identifier of the last message in the *)
(*                     ; mailbox or, if the mailbox is empty, the *)
(*                     ; mailbox's current UIDNEXT value. *)
(*                     ; The server should respond with a tagged BAD *)
(*                     ; response to a command that uses a message *)
(*                     ; sequence number greater than the number of *)
(*                     ; messages in the selected mailbox.  This *)
(*                     ; includes "*" if the selected mailbox is empty. *)
(* *\) *)
(* let seq_number = *)
(*   alt nz_number (char '*' >> ret Uint32.zero) *)

(* (\* *)
(* seq-range       = seq-number ":" seq-number *)
(*                     ; two seq-number values and all values between *)
(*                     ; these two regardless of order. *)
(*                     ; Example: 2:4 and 4:2 are equivalent and indicate *)
(*                     ; values 2, 3, and 4. *)
(*                     ; Example: a unique identifier sequence range of *)
(*                     ; 3291:* includes the UID of the last message in *)
(*                     ; the mailbox, even if that value is less than 3291. *)
(* *\) *)
(* let seq_range = *)
(*   seq_number >>= fun x -> *)
(*   char ':' >> *)
(*   seq_number >>= fun y -> *)
(*   ret (ImapSet.interval x y) *)

(* (\* *)
(* sequence-set    = (seq-number / seq-range) *("," sequence-set) *)
(*                     ; set of seq-number values, regardless of order. *)
(*                     ; Servers MAY coalesce overlaps and/or execute the *)
(*                     ; sequence in any order. *)
(*                     ; Example: a message sequence number set of *)
(*                     ; 2,4:7,9,12:* for a mailbox with 15 messages is *)
(*                     ; equivalent to 2,4,5,6,7,9,12,13,14,15 *)
(*                     ; Example: a message sequence number set of *:4,5:7 *)
(*                     ; for a mailbox with 10 messages is equivalent to *)
(*                     ; 10,9,8,7,6,5,4,5,6,7 and MAY be reordered and *)
(*                     ; overlap coalesced to be 4,5,6,7,8,9,10. *)
(* *\) *)
(* let sequence_set = *)
(*   let elem = alt seq_range (seq_number >>= fun x -> ret (ImapSet.single x)) in *)
(*   elem >>= fun x -> *)
(*   rep (char ',' >> elem) >>= fun xs -> *)
(*   ret (List.fold_left ImapSet.union x xs) *)

(* (\* *)
(* resp-text-code  = "ALERT" / *)
(*                   "BADCHARSET" [SP "(" astring *(SP astring) ")" ] / *)
(*                   capability-data / "PARSE" / *)
(*                   "PERMANENTFLAGS" SP "(" *)
(*                   [flag-perm *(SP flag-perm)] ")" / *)
(*                   "READ-ONLY" / "READ-WRITE" / "TRYCREATE" / *)
(*                   "UIDNEXT" SP nz-number / "UIDVALIDITY" SP nz-number / *)
(*                   "UNSEEN" SP nz-number / *)
(*                   atom [SP 1*<any TEXT-CHAR except "]">] *)
(* *\) *)
(* let is_text_other_char c = *)
(*   is_text_char c && (c <> ']') *)

(* let resp_text_code k d = *)
(*   let alert = str "ALERT" >> ret RESP_TEXT_CODE_ALERT in *)
(*   let badcharset = *)
(*     str "BADCHARSET" >> *)
(*     opt (char ' ' >> char '(' >> sep1 (char ' ') astring >>= fun xs -> char ')' >> ret xs) [] >>= fun xs -> *)
(*     ret (RESP_TEXT_CODE_BADCHARSET xs) *)
(*   in *)
(*   let capability = capability_data >>= fun c -> ret (RESP_TEXT_CODE_CAPABILITY_DATA c) in *)
(*   let parse = str "PARSE" >> ret RESP_TEXT_CODE_PARSE in *)
(*   let permanentflags = *)
(*     str "PERMANENTFLAGS" >> char ' ' >> char '(' >> *)
(*     sep (char ' ') flag_perm >>= fun flags -> *)
(*     char ')' >> *)
(*     ret (RESP_TEXT_CODE_PERMANENTFLAGS flags) *)
(*   in *)
(*   let read_only = str "READ-ONLY" >> ret RESP_TEXT_CODE_READ_ONLY in *)
(*   let read_write = str "READ-WRITE" >> ret RESP_TEXT_CODE_READ_WRITE in *)
(*   let trycreate = str "TRYCREATE" >> ret RESP_TEXT_CODE_TRYCREATE in *)
(*   let uidnext = str "UIDNEXT" >> char ' ' >> nz_number >>= fun n -> ret (RESP_TEXT_CODE_UIDNEXT n) in *)
(*   let uidvalidity = str "UIDVALIDITY" >> char ' ' >> nz_number >>= fun n -> ret (RESP_TEXT_CODE_UIDVALIDITY n) in *)
(*   let unseen = str "UNSEEN" >> char ' ' >> nz_number >>= fun n -> ret (RESP_TEXT_CODE_UNSEEN n) in *)
(*   let extension = extension_parser RESP_TEXT_CODE >>= fun e -> ret (RESP_TEXT_CODE_EXTENSION e) in *)
(*   let other = *)
(*     atom >>= fun a -> opt (char ' ' >> some (accum is_text_other_char)) None *)
(*     >>= fun s -> ret (RESP_TEXT_CODE_OTHER (a, s)) *)
(*   in *)

(*   if is_atom_char d.c then (baddu d; readc (resp_text_code k) d) else *)
(*   match buf d with *)
(*   | "ALERT" -> ret `Alert k d *)
(*   | "BADCHARSET" -> *)
(*       let k d = *)
(*         if d.c = ' ' then *)
(*           let rec loop k = *)
(*             if d.c = ' ' then readc (r_astring (loop k)) d else *)
(*             if d.c = ')' then readc k d *)
(*           in *)
(*           readc (r_char '(' (r_astring (loop k))) d *)
(*         else *)
(*         k d *)
(*       in *)
(*       ret `Bad_charset k d *)
(*   | "CAPABILITY" -> *)
(*       let rec loop k = *)
(*         if d.c = ' ' then readc (r_capability (loop k)) d else *)
(*         k d *)
(*       in *)
(*       r_space (loop k) d *)
(*   | "PARSE" -> ret `Parse k d *)
(*   | "PERMANENTFLAGS" -> *)
(*       let rec loop k = *)
(*         match d.c with *)
(*         | ')' -> ret `Pl (readc k) d *)
(*         | ' ' -> readc (r_flag_perm (loop k)) d *)
(*         | _ -> ret (err_flag_perm d) k d *)
(*       in *)
(*       r_space (r_lp (fun d -> match d.c with ')' -> ret `Pl (readc k) | _ -> r_flag_perm (loop k))) d *)
(*   | "READ-ONLY" -> ret `Read_only k d *)
(*   | "READ-WRITE" -> ret `Read_write k d *)
(*   | "TRYCREATE" -> ret `Try_create k d *)
(*   | "UIDNEXT" -> ret `Uid_next (r_space (r_nz_number k)) d *)
(*   | "UIDVALIDITY" -> ret `Uid_validity (r_space (r_nz_number k)) d *)
(*   | "UNSEEN" -> ret `Unseen (r_space (r_nz_number k)) d *)
(*   | _ as a -> *)
(*       let k d = *)
(*         if d.c = ' ' then readc (r_text_other k) d else *)
(*         k d *)
(*       in *)
(*       ret (`Lexeme (`String a)) k d *)

(*   (\* altn *\) *)
(*   (\*   [alert; badcharset; capability; parse; permanentflags; read_only; *\) *)
(*   (\*    read_write; trycreate; uidnext; uidvalidity; unseen; extension; other] *\) *)
(* (\* (str "COMPRESSIONACTIVE" >> ret RESP_TEXT_CODE_COMPRESSIONACTIVE); *\) *)

(*         [ match% *)

(* let r_lb k d = *)
(*   match d.c with *)
(*   | ']' -> ret `Br (readc k) d *)
(*   | _ -> ret (err_br d) k d *)

(* (\* *)
(* resp-text       = ["[" resp-text-code "]" SP] text *)
(* *\) *)
(* let r_resp_text k d = *)
(*   if d.c = '[' then *)
(*     ret `Bl *)
(*       (readc (r_resp_text_code (r_lb (fun d -> match d.c with ']' -> readc -> match d.c with ' ' -> readc (r_text k) d | _ -> ())))) d else *)
(*   r_text k d *)
(*   (\* opt (char '[' >> resp_text_code >>= fun rsp_code -> char ']' >> ret (Some rsp_code)) None *\) *)
(*   (\* >>= function *\) *)
(*   (\* | None -> *\) *)
(*   (\*     opt text "" >>= fun rsp_text -> ret {rsp_code = RESP_TEXT_CODE_NONE; rsp_text} *\) *)
(*   (\* | Some rsp_code -> *\) *)
(*   (\*     (\\* we make the space optional if there is no resp_text_code - Gimap needs this. *\\) *\) *)
(*   (\*     opt (char ' ' >> text) "" >>= fun rsp_text -> ret {rsp_code; rsp_text} *\) *)

(* (\* *)
(* resp-cond-state = ("OK" / "NO" / "BAD") SP resp-text *)
(*                     ; Status condition *)
(* *\) *)
(* let resp_cond_state = *)
(*   altn [ *)
(*     (str "OK" >> ret RESP_COND_STATE_OK); *)
(*     (str "NO" >> ret RESP_COND_STATE_NO); *)
(*     (str "BAD" >> ret RESP_COND_STATE_BAD) *)
(*   ] *)
(*   >>= fun rsp_type -> *)
(*   char ' ' >> resp_text >>= fun rsp_text -> *)
(*   ret {rsp_type; rsp_text} *)

(* (\* *)
(* mbx-list-sflag  = "\Noselect" / "\Marked" / "\Unmarked" *)
(*                     ; Selectability flags; only one per LIST response *)
(* *\) *)
(* let mbx_list_sflag = *)
(*   altn [ *)
(*     str "\\Noselect" >> ret MBX_LIST_SFLAG_NOSELECT; *)
(*     str "\\Marked" >> ret MBX_LIST_SFLAG_MARKED; *)
(*     str "\\Unmarked" >> ret MBX_LIST_SFLAG_UNMARKED *)
(*   ] *)

(* (\* *)
(* mbx-list-oflag  = "\Noinferiors" / flag-extension *)
(*                     ; Other flags; multiple possible per LIST response *)
(* *\) *)
(* let mbx_list_oflag = *)
(*   alt *)
(*     (str "\\Noinferiors" >> ret MBX_LIST_OFLAG_NOINFERIORS) *)
(*     (flag_extension >>= fun s -> ret (MBX_LIST_OFLAG_EXT s)) *)

(* let mbx_list_oflag_no_sflag = *)
(*   opt (mbx_list_sflag >> ret true) false >>= function *)
(*   | false -> mbx_list_oflag *)
(*   | true -> fail *)

(* (\* *)
(* mbx-list-flags  = *(mbx-list-oflag SP) mbx-list-sflag *)
(*                   *(SP mbx-list-oflag) / *)
(*                   mbx-list-oflag *(SP mbx-list-oflag) *)
(* *\) *)
(* let mbx_list_flags = *)
(*   alt *)
(*     begin *)
(*       rep (mbx_list_oflag_no_sflag >>= fun oflags1 -> char ' ' >> ret oflags1) >>= fun oflags1 -> *)
(*       mbx_list_sflag >>= fun sflag -> *)
(*       rep (char ' ' >> mbx_list_oflag) >>= fun oflags2 -> *)
(*       ret {mbf_sflag = Some sflag; mbf_oflags = oflags1 @ oflags2} *)
(*     end *)
(*     begin *)
(*       sep1 (char ' ') mbx_list_oflag >>= fun oflags -> *)
(*       ret {mbf_sflag = None; mbf_oflags = oflags} *)
(*     end *)

(* let mailbox = *)
(*   let decode_mailbox_name s = try ImapUtils.decode_mutf7 s with _ -> s in *)
(*   astring >>= fun s -> ret (decode_mailbox_name s) *)

(* (\* *)
(* mailbox-list    = "(" [mbx-list-flags] ")" SP *)
(*                    (DQUOTE QUOTED-CHAR DQUOTE / nil) SP mailbox *)
(* *\) *)
(* let mailbox_list = *)
(*   char '(' >> *)
(*   opt mbx_list_flags {mbf_sflag = None; mbf_oflags = []} >>= fun mb_flag -> *)
(*   char ')' >> char ' ' >> *)
(*   alt (some quoted_char) (nil >> ret None) >>= fun mb_delimiter -> *)
(*   char ' ' >> mailbox >>= fun mb_name -> *)
(*   ret {mb_flag; mb_delimiter; mb_name} *)

(*
status-att      = "MESSAGES" / "RECENT" / "UIDNEXT" / "UIDVALIDITY" /
                  "UNSEEN"
*)
(* let status_att = *)
(*   let messages = str "MESSAGES" >> char ' ' >> number' >>= fun n -> ret (STATUS_ATT_MESSAGES n) in *)
(*   let recent = str "RECENT" >> char ' ' >> number' >>= fun n -> ret (STATUS_ATT_RECENT n) in *)
(*   let uidnext = str "UIDNEXT" >> char ' ' >> nz_number >>= fun n -> ret (STATUS_ATT_UIDNEXT n) in *)
(*   let uidvalidity = str "UIDVALIDITY" >> char ' ' >> nz_number >>= fun n -> ret (STATUS_ATT_UIDVALIDITY n) in *)
(*   let unseen = str "UNSEEN" >> char ' ' >> number' >>= fun n -> ret (STATUS_ATT_UNSEEN n) in *)
(*   let extension = extension_parser STATUS_ATT >>= fun e -> ret (STATUS_ATT_EXTENSION e) in *)
(*   altn [ messages; recent; uidnext; uidvalidity; unseen; extension ] *)

(* let address = *)
(*   char '(' >> *)
(*   nstring' >>= fun ad_personal_name -> char ' ' >> *)
(*   nstring' >>= fun ad_source_route -> char ' ' >> *)
(*   nstring' >>= fun ad_mailbox_name -> char ' ' >> *)
(*   nstring' >>= fun ad_host_name -> *)
(*   char ')' >> *)
(*   ret { ad_personal_name; ad_source_route; ad_mailbox_name; ad_host_name } *)

(* let address_list = *)
(*   alt *)
(*     (char '(' >> sep1 (char ' ') address >>= fun xs -> char ')' >> ret xs) *)
(*     (nil >> ret []) *)

(* let envelope = *)
(*   char '(' >> *)
(*   nstring' >>= fun env_date -> char ' ' >> *)
(*   nstring' >>= fun env_subject -> char ' ' >> *)
(*   address_list >>= fun env_from -> char ' ' >> *)
(*   address_list >>= fun env_sender -> char ' ' >> *)
(*   address_list >>= fun env_reply_to -> char ' ' >> *)
(*   address_list >>= fun env_to -> char ' ' >> *)
(*   address_list >>= fun env_cc -> char ' ' >> *)
(*   address_list >>= fun env_bcc -> char ' ' >> *)
(*   nstring' >>= fun env_in_reply_to -> char ' ' >> *)
(*   nstring' >>= fun env_message_id -> *)
(*   char ')' >> *)
(*   ret { *)
(*     env_date; env_subject; env_from; env_sender; *)
(*     env_reply_to; env_to; env_cc; env_bcc; env_in_reply_to; *)
(*     env_message_id *)
(*   } *)

(* let media_subtype = *)
(*   imap_string *)

(* (\* *)
(* media-basic     = ((DQUOTE ("APPLICATION" / "AUDIO" / "IMAGE" / *)
(*                   "MESSAGE" / "VIDEO") DQUOTE) / string) SP *)
(*                   media-subtype *)
(*                     ; Defined in [MIME-IMT] *)
(* *\) *)
(* let media_basic = *)
(*   let table = [ *)
(*     "APPLICATION", MEDIA_BASIC_APPLICATION; *)
(*     "AUDIO", MEDIA_BASIC_AUDIO; *)
(*     "IMAGE", MEDIA_BASIC_IMAGE; *)
(*     "MESSAGE", MEDIA_BASIC_MESSAGE; *)
(*     "VIDEO", MEDIA_BASIC_VIDEO *)
(*   ] *)
(*   in *)
(*   let media_basic' = *)
(*     imap_string >>= fun s -> *)
(*     ret (try List.assoc (String.uppercase s) table with Not_found -> MEDIA_BASIC_OTHER s) *)
(*   in *)
(*   media_basic' >>= fun med_basic_type -> char ' ' >> media_subtype >>= fun med_basic_subtype -> *)
(*   ret {med_basic_type; med_basic_subtype} *)

(* (\* *)
(* body-fld-param  = "(" string SP string *(SP string SP string) ")" / nil *)
(* *\) *)
(* let body_fld_param = *)
(*   let param = imap_string >>= fun k -> char ' ' >> imap_string >>= fun v -> ret (k, v) in *)
(*   alt *)
(*     (char '(' >> sep1 (char ' ') param >>= fun xs -> char ')' >> ret xs) *)
(*     (nil >> ret []) *)

(* (\* *)
(* body-fld-enc    = (DQUOTE ("7BIT" / "8BIT" / "BINARY" / "BASE64"/ *)
(*                   "QUOTED-PRINTABLE") DQUOTE) / string *)
(* *\) *)
(* let body_fld_enc = *)
(*   let table = [ *)
(*     "7BIT", BODY_FLD_ENC_7BIT; *)
(*     "8BIT", BODY_FLD_ENC_8BIT; *)
(*     "BINARY", BODY_FLD_ENC_BINARY; *)
(*     "BASE64", BODY_FLD_ENC_BASE64; *)
(*     "QUOTED-PRINTABLE", BODY_FLD_ENC_QUOTED_PRINTABLE *)
(*   ] *)
(*   in *)
(*   imap_string >>= fun s -> *)
(*   ret (try List.assoc (String.uppercase s) table with Not_found -> BODY_FLD_ENC_OTHER s) *)

(* (\* *)
(* body-fld-id     = nstring *)
(* *\) *)
(* let body_fld_id = *)
(*   nstring *)

(* (\* *)
(* body-fld-desc   = nstring *)
(* *\) *)
(* let body_fld_desc = *)
(*   nstring *)

(* (\* *)
(* body-fld-octets = number *)
(* *\) *)
(* let body_fld_octets = *)
(*   number' *)

(* (\* *)
(* body-fields     = body-fld-param SP body-fld-id SP body-fld-desc SP *)
(*                   body-fld-enc SP body-fld-octets *)
(* *\) *)
(* let body_fields = *)
(*   body_fld_param >>= fun bd_parameter -> *)
(*   char ' ' >> body_fld_id >>= fun bd_id -> *)
(*   char ' ' >> body_fld_desc >>= fun bd_description -> *)
(*   char ' ' >> body_fld_enc >>= fun bd_encoding -> *)
(*   char ' ' >> body_fld_octets >>= fun bd_size -> *)
(*   ret {bd_parameter; bd_id; bd_description; bd_encoding; bd_size} *)

(* (\* *)
(* media-message   = DQUOTE "MESSAGE" DQUOTE SP DQUOTE "RFC822" DQUOTE *)
(*                     ; Defined in [MIME-IMT] *)
(* *\) *)
(* let media_message = *)
(*   char '\"' >> str "MESSAGE" >> char '\"' >> char ' ' >> *)
(*   char '\"' >> str "RFC822" >> char '\"' *)

(* (\* *)
(* media-text      = DQUOTE "TEXT" DQUOTE SP media-subtype *)
(*                     ; Defined in [MIME-IMT] *)
(* *\) *)
(* let media_text = *)
(*   char '\"' >> str "TEXT" >> char '\"' >> char ' ' >> media_subtype *)

(* (\* *)
(* body-fld-md5    = nstring *)
(* *\) *)
(* let body_fld_md5 = *)
(*   nstring *)

(* (\* *)
(* body-fld-dsp    = "(" string SP body-fld-param ")" / nil *)
(* *\) *)
(* let body_fld_dsp = *)
(*   alt *)
(*     begin *)
(*       char '(' >> *)
(*       imap_string >>= fun dsp_type -> *)
(*       char ' ' >> *)
(*       body_fld_param >>= fun dsp_attributes -> *)
(*       char ')' >> *)
(*       ret (Some {dsp_type; dsp_attributes}) *)
(*     end *)
(*     (nil >> ret None) *)

(* (\* *)
(* body-fld-lang   = nstring / "(" string *(SP string) ")" *)
(* *\) *)
(* let body_fld_lang = *)
(*   alt *)
(*     (char '(' >> sep1 (char ' ') imap_string >>= fun xs -> char ')' >> ret (BODY_FLD_LANG_LIST xs)) *)
(*     (nstring >>= fun s -> ret (BODY_FLD_LANG_SINGLE s)) *)

(* (\* *)
(* body-extension  = nstring / number / *)
(*                    "(" body-extension *(SP body-extension) ")" *)
(*                     ; Future expansion.  Client implementations *)
(*                     ; MUST accept body-extension fields.  Server *)
(*                     ; implementations MUST NOT generate *)
(*                     ; body-extension fields except as defined by *)
(*                     ; future standard or standards-track *)
(*                     ; revisions of this specification. *)
(* *\) *)
(* let rec body_extension () = *)
(*   altn [ *)
(*     (char '(' >> sep1 (char ' ') (delay body_extension ()) >>= fun xs -> *)
(*      char ')' >> ret (BODY_EXTENSION_LIST xs)); *)
(*     (number >>= fun n -> ret (BODY_EXTENSION_NUMBER n)); *)
(*     (nstring >>= fun s -> ret (BODY_EXTENSION_NSTRING s)) *)
(*   ] *)

(* let body_fld_loc = *)
(*   nstring *)

(* (\* *)
(* body-ext-1part  = body-fld-md5 [SP body-fld-dsp [SP body-fld-lang *)
(*                   [SP body-fld-loc *(SP body-extension)]]] *)
(*                     ; MUST NOT be returned on non-extensible *)
(*                     ; "BODY" fetch *)
(* *\) *)
(* let body_ext_1part = *)
(*   body_fld_md5 >>= fun bd_md5 -> *)
(*   opt *)
(*     begin *)
(*       char ' ' >> body_fld_dsp >>= fun bd_disposition -> *)
(*       opt *)
(*         begin *)
(*           char ' ' >> some body_fld_lang >>= fun bd_language -> *)
(*           opt *)
(*             begin *)
(*               char ' ' >> body_fld_loc >>= fun bd_loc -> *)
(*               rep (char ' ' >> delay body_extension ()) >>= fun bd_extension_list -> *)
(*               ret {bd_md5; bd_disposition; bd_language; bd_loc; bd_extension_list} *)
(*             end *)
(*             {bd_md5; bd_disposition; bd_language; bd_loc = None; bd_extension_list = []} *)
(*         end *)
(*         {bd_md5; bd_disposition; bd_language = None; bd_loc = None; bd_extension_list = []} *)
(*     end *)
(*     {bd_md5 = None; bd_disposition = None; bd_language = None; bd_loc = None; bd_extension_list = []} *)

(* (\* *)
(* body-ext-mpart  = body-fld-param [SP body-fld-dsp [SP body-fld-lang *)
(*                   [SP body-fld-loc *(SP body-extension)]]] *)
(*                     ; MUST NOT be returned on non-extensible *)
(*                     ; "BODY" fetch *)
(* *\) *)
(* let body_ext_mpart = *)
(*   char ' ' >> body_fld_param >>= fun bd_parameter -> *)
(*   opt *)
(*     begin *)
(*       char ' ' >> body_fld_dsp >>= fun bd_disposition -> *)
(*       opt *)
(*         begin *)
(*           char ' ' >> some body_fld_lang >>= fun bd_language -> *)
(*           opt *)
(*             begin *)
(*               char ' ' >> body_fld_loc >>= fun bd_loc -> *)
(*               rep (char ' ' >> delay body_extension ()) >>= fun bd_extension_list -> *)
(*               ret {bd_parameter; bd_disposition; bd_language; bd_loc; bd_extension_list} *)
(*             end *)
(*             {bd_parameter; bd_disposition; bd_language; bd_loc = None; bd_extension_list = []} *)
(*         end *)
(*         {bd_parameter; bd_disposition; bd_language = None; bd_loc = None; bd_extension_list = []} *)
(*     end *)
(*     {bd_parameter; bd_disposition = None; bd_language = None; bd_loc = None; bd_extension_list = []} *)

(* (\* *)
(* body-type-basic = media-basic SP body-fields *)
(*                     ; MESSAGE subtype MUST NOT be "RFC822" *)
(* *\) *)
(* let rec body_type_basic () = *)
(*   media_basic >>= fun bd_media_basic -> *)
(*   char ' ' >> body_fields >>= fun bd_fields -> *)
(*   ret {bd_media_basic; bd_fields} *)

(* (\* *)
(* body-type-msg   = media-message SP body-fields SP envelope *)
(*                   SP body SP body-fld-lines *)
(* *\) *)
(* and body_type_msg () = *)
(*   media_message >> char ' ' >> body_fields >>= fun bd_fields -> *)
(*   char ' ' >> envelope >>= fun bd_envelope -> *)
(*   char ' ' >> delay body () >>= fun bd_body -> *)
(*   ret {bd_fields; bd_envelope; bd_body} *)

(* (\* *)
(* body-type-text  = media-text SP body-fields SP body-fld-lines *)
(* *\) *)
(* and body_type_text () = *)
(*   media_text >>= fun bd_media_text -> *)
(*   char ' ' >> body_fields >>= fun bd_fields -> *)
(*   char ' ' >> number' >>= fun bd_lines -> *)
(*   ret {bd_media_text; bd_fields; bd_lines} *)

(* (\* *)
(* body-type-1part = (body-type-basic / body-type-msg / body-type-text) *)
(*                   [SP body-ext-1part] *)
(* *\) *)
(* and body_type_1part () = *)
(*   altn [ *)
(*     (delay body_type_msg () >>= fun b -> ret (BODY_TYPE_1PART_MSG b)); *)
(*     (delay body_type_text () >>= fun b -> ret (BODY_TYPE_1PART_TEXT b)); *)
(*     (delay body_type_basic () >>= fun b -> ret (BODY_TYPE_1PART_BASIC b)) *)
(*   ] >>= fun bd_data -> *)
(*   opt *)
(*     (char ' ' >> body_ext_1part) *)
(*     {bd_md5 = None; bd_disposition = None; bd_language = None; bd_loc = None; bd_extension_list = []} *)
(*   >>= fun bd_ext_1part -> *)
(*   ret {bd_data; bd_ext_1part} *)

(* (\* *)
(* body-type-mpart = 1*body SP media-subtype *)
(*                   [SP body-ext-mpart] *)
(* *\) *)
(* and body_type_mpart () = *)
(*   rep1 (delay body ()) >>= fun bd_list -> *)
(*   char ' ' >> media_subtype >>= fun bd_media_subtype -> *)
(*   body_ext_mpart >>= fun bd_ext_mpart -> *)
(*   ret {bd_list; bd_media_subtype; bd_ext_mpart} *)

(* (\* *)
(* body            = "(" (body-type-1part / body-type-mpart) ")" *)
(* *\) *)
(* and body () = *)
(*   char '(' >> *)
(*   alt *)
(*     (delay body_type_1part () >>= fun b -> ret (BODY_1PART b)) *)
(*     (delay body_type_mpart () >>= fun b -> ret (BODY_MPART b)) *)
(*   >>= fun b -> *)
(*   char ')' >> *)
(*   ret b *)

(* (\* *)
(* status-att-list =  status-att SP number *(SP status-att SP number) *)
(* *\) *)
(* let date_day_fixed = *)
(*   alt (char ' ' >> digits1) digits2 *)

(* let date_month = *)
(*   app String.capitalize (string_of_length 3) >>= function *)
(*   | "Jan" -> ret 1 *)
(*   | "Feb" -> ret 2 *)
(*   | "Mar" -> ret 3 *)
(*   | "Apr" -> ret 4 *)
(*   | "May" -> ret 5 *)
(*   | "Jun" -> ret 6 *)
(*   | "Jul" -> ret 7 *)
(*   | "Aug" -> ret 8 *)
(*   | "Sep" -> ret 9 *)
(*   | "Oct" -> ret 10 *)
(*   | "Nov" -> ret 11 *)
(*   | "Dec" -> ret 12 *)
(*   | _ -> fail *)

(* let time = *)
(*   digits2 >>= fun hours -> char ':' >> *)
(*   digits2 >>= fun minutes -> char ':' >> *)
(*   digits2 >>= fun seconds -> *)
(*   ret (hours, minutes, seconds) *)

(* let zone = *)
(*   alt (char '+' >> ret 1) (char '-' >> ret (-1)) >>= fun sign -> *)
(*   digits2 >>= fun hh -> *)
(*   digits2 >>= fun mm -> *)
(*   ret (sign * (60 * hh + mm)) *)

(* let date_time = *)
(*   char '\"' >> *)
(*   date_day_fixed >>= fun dt_day -> *)
(*   char '-' >> date_month >>= fun dt_month -> *)
(*   char '-' >> digits4 >>= fun dt_year -> *)
(*   char ' ' >> time >>= fun (dt_hour, dt_min, dt_sec) -> *)
(*   char ' ' >> zone >>= fun dt_zone -> *)
(*   char '\"' >> *)
(*   ret {dt_day; dt_month; dt_year; dt_hour; dt_min; dt_sec; dt_zone} *)

(* let header_fld_name = *)
(*   astring *)

(* let header_list = *)
(*   char '(' >> *)
(*   sep1 (char ' ') header_fld_name >>= fun xs -> *)
(*   char ')' >> *)
(*   ret xs *)

(* (\* *)
(* section-msgtext = "HEADER" / "HEADER.FIELDS" [".NOT"] SP header-list / *)
(*                   "TEXT" *)
(*                     ; top-level or MESSAGE/RFC822 part *)
(* *\) *)
(* let section_msgtext = *)
(*   let header_fields = *)
(*     opt (str ".NOT" >> ret true) false >>= fun has_not -> *)
(*     char ' ' >> header_list >>= fun hs -> *)
(*     if has_not then *)
(*       ret (SECTION_MSGTEXT_HEADER_FIELDS_NOT hs) *)
(*     else *)
(*       ret (SECTION_MSGTEXT_HEADER_FIELDS hs) *)
(*   in *)
(*   altn [ *)
(*     (str "HEADER.FIELDS" >> header_fields); *)
(*     (str "HEADER" >> ret SECTION_MSGTEXT_HEADER); *)
(*     (str "TEXT" >> ret SECTION_MSGTEXT_TEXT) *)
(*   ] *)

(* (\* *)
(* section-part    = nz-number *("." nz-number) *)
(*                     ; body part nesting *)
(* *\) *)
(* let section_part = *)
(*   sep1 (char '.') nz_number' *)

(* (\* *)
(* section-text    = section-msgtext / "MIME" *)
(*                     ; text other than actual body part (headers, etc.) *)
(* *\) *)
(* let section_text = *)
(*   alt *)
(*     (str "MIME" >> ret SECTION_TEXT_MIME) *)
(*     (section_msgtext >>= fun t -> ret (SECTION_TEXT_MSGTEXT t)) *)

(* (\* *)
(* section-spec    = section-msgtext / (section-part ["." section-text]) *)
(* *\) *)
(* let section_spec = *)
(*   alt *)
(*     (section_msgtext >>= fun s -> ret (SECTION_SPEC_SECTION_MSGTEXT s)) *)
(*     begin *)
(*       section_part >>= fun p -> opt (char '.' >> some section_text) None >>= fun t -> *)
(*       ret (SECTION_SPEC_SECTION_PART (p, t)) *)
(*     end *)

(* (\* *)
(* section         = "[" [section-spec] "]" *)
(* *\) *)
(* let section = *)
(*   char '[' >> opt (some section_spec) None >>= fun x -> char ']' >> ret x *)

(* let uint64 = *)
(*   app Uint64.of_string (accum (function '0' .. '9' -> true | _ -> false)) *)

(* (\* *)
(* msg-att-static  = "ENVELOPE" SP envelope / "INTERNALDATE" SP date-time / *)
(*                   "RFC822" [".HEADER" / ".TEXT"] SP nstring / *)
(*                   "RFC822.SIZE" SP number / *)
(*                   "BODY" ["STRUCTURE"] SP body / *)
(*                   "BODY" section ["<" number ">"] SP nstring / *)
(*                   "UID" SP uniqueid *)
(*                     ; MUST NOT change for a message *)
(* *\) *)
(* let msg_att_static = *)
(*   let body_ = *)
(*     let section_ sec_section = *)
(*       opt (char '<' >> some number' >>= fun x -> char '>' >> ret x) None >>= fun sec_origin_octet -> *)
(*       char ' ' >> nstring' >>= fun sec_body_part -> *)
(*       ret (MSG_ATT_BODY_SECTION {sec_section; sec_origin_octet; sec_body_part}) *)
(*     in *)
(*     altn [ *)
(*       (char ' ' >> delay body () >>= fun b -> ret (MSG_ATT_BODY b)); *)
(*       (section >>= section_) *)
(*     ] *)
(*   in *)
(*   altn [ *)
(*     (str "ENVELOPE" >> char ' ' >> envelope >>= fun e -> ret (MSG_ATT_ENVELOPE e)); *)
(*     (str "INTERNALDATE" >> char ' ' >> date_time >>= fun dt -> ret (MSG_ATT_INTERNALDATE dt)); *)
(*     (str "RFC822.HEADER" >> char ' ' >> nstring' >>= fun s -> ret (MSG_ATT_RFC822_HEADER s)); *)
(*     (str "RFC822.TEXT" >> char ' ' >> nstring' >>= fun s -> ret (MSG_ATT_RFC822_TEXT s)); *)
(*     (str "RFC822.SIZE" >> char ' ' >> number' >>= fun n -> ret (MSG_ATT_RFC822_SIZE n)); *)
(*     (str "RFC822" >> char ' ' >> nstring' >>= fun s -> ret (MSG_ATT_RFC822 s)); *)
(*     (str "BODYSTRUCTURE" >> char ' ' >> delay body () >>= fun b -> ret (MSG_ATT_BODYSTRUCTURE b)); *)
(*     (str "BODY" >> body_); *)
(*     (str "UID" >> char ' ' >> nz_number >>= fun uid -> ret (MSG_ATT_UID uid)) *)
(*   (\*   (str "X-GM-THRID" >> char ' ' >> uint64 >>= fun n -> ret (MSG_ATT_X_GM_THRID (Gthrid.of_uint64 n))) *\) *)
(*   ] *)

(* (\* *)
(* msg-att-dynamic = "FLAGS" SP "(" [flag-fetch *(SP flag-fetch)] ")" *)
(*                     ; MAY change for a message *)
(* *\) *)
(* let msg_att_dynamic = *)
(*   str "FLAGS" >> char ' ' >> char '(' >> *)
(*   sep (char ' ') flag_fetch >>= fun flags -> *)
(*   char ')' >> *)
(*   ret flags *)

(* (\* *)
(* msg-att         = "(" (msg-att-dynamic / msg-att-static) *)
(*                    *(SP (msg-att-dynamic / msg-att-static)) ")" *)
(* *\) *)
(* let msg_att = *)
(*   char '(' >> *)
(*   sep1 (char ' ') *)
(*     begin *)
(*       altn *)
(*         [ (app (fun a -> MSG_ATT_ITEM_STATIC a) msg_att_static); *)
(*           (app (fun a -> MSG_ATT_ITEM_DYNAMIC a) msg_att_dynamic); *)
(*           (app (fun e -> MSG_ATT_ITEM_EXTENSION e) (extension_parser FETCH_DATA)) ] *)
(*     end *)
(*   >>= fun xs -> *)
(*   char ')' >> *)
(*   ret xs *)

(* let status_info = *)
(*   alt status_att (app (fun e -> STATUS_ATT_EXTENSION e) (extension_parser STATUS_ATT)) *)

(* let mailbox_data_flags = *)
(*   app (fun flags -> MAILBOX_DATA_FLAGS flags) (str "FLAGS" >> char ' ' >> flag_list) *)

(* let mailbox_data_list = *)
(*   app (fun mb -> MAILBOX_DATA_LIST mb) (str "LIST" >> char ' ' >> mailbox_list) *)

(* let mailbox_data_lsub = *)
(*   app (fun mb -> MAILBOX_DATA_LSUB mb) (str "LSUB" >> char ' ' >> mailbox_list) *)

(* let mailbox_data_search = *)
(*   app (fun ns -> MAILBOX_DATA_SEARCH ns) (str "SEARCH" >> rep (char ' ' >> nz_number)) *)

(* let mailbox_data_status = *)
(*   str "STATUS" >> char ' ' >> mailbox >>= fun st_mailbox -> *)
(*   char ' ' >> char '(' >> sep (char ' ') status_info >>= fun st_info_list -> *)
(*   char ')' >> *)
(*   ret (MAILBOX_DATA_STATUS {st_mailbox; st_info_list}) *)

(* let mailbox_data_exists = *)
(*   number' >>= fun n -> char ' ' >> str "EXISTS" >> *)
(*   ret (MAILBOX_DATA_EXISTS n) *)

(* let mailbox_data_recent = *)
(*   number' >>= fun n -> char ' ' >> str "RECENT" >> *)
(*   ret (MAILBOX_DATA_RECENT n) *)

(* let mailbox_data_extension_data = *)
(*   app (fun e -> MAILBOX_DATA_EXTENSION_DATA e) (extension_parser MAILBOX_DATA) *)

(*
mailbox-data    =  "FLAGS" SP flag-list / "LIST" SP mailbox-list /
                   "LSUB" SP mailbox-list / "SEARCH" *(SP nz-number) /
                   "STATUS" SP mailbox SP "(" [status-att-list] ")" /
                   number SP "EXISTS" / number SP "RECENT"
*)
(* let mailbox_data = *)
(*   altn *)
(*     [ mailbox_data_flags; mailbox_data_list; mailbox_data_lsub; mailbox_data_search; *)
(*       mailbox_data_status; mailbox_data_exists; mailbox_data_recent; mailbox_data_extension_data ] *)

(*
message-data    = nz-number SP ("EXPUNGE" / ("FETCH" SP msg-att))
*)
(* let message_data = *)
(*   nz_number >>= fun n -> *)
(*   char ' ' >> *)
(*   alt *)
(*     (str "EXPUNGE" >> ret (MESSAGE_DATA_EXPUNGE n)) *)
(*     (str "FETCH" >> char ' ' >> msg_att >>= fun att -> ret (MESSAGE_DATA_FETCH (att, n))) *)

(* (\* *)
(* tag             = 1*<any ASTRING-CHAR except "+"> *)
(* *\) *)
(* let is_tag_char = *)
(*   function *)
(*     '\x80' .. '\xff' | '(' | ')' *)
(*   | '{' | ' ' | '\x00' .. '\x1f' *)
(*   | '\x7f' | '%' | '*' | '\\' *)
(*   | '\"' | '+' -> false *)
(*   | _ -> true *)

(* let tag = *)
(*   accum is_tag_char *)

(* (\* *)
(* response-tagged = tag SP resp-cond-state CRLF *)
(* *\) *)
(* let response_tagged = *)
(*   tag >>= fun rsp_tag -> char ' ' >> resp_cond_state >>= fun rsp_cond_state -> crlf >> *)
(*   ret {rsp_tag; rsp_cond_state} *)

(* (\* *)
(* resp-cond-auth  = ("OK" / "PREAUTH") SP resp-text *)
(*                     ; Authentication condition *)
(* *\) *)
(* let resp_cond_auth = *)
(*   alt *)
(*     (app (fun _ -> RESP_COND_AUTH_OK) (str "OK")) *)
(*     (app (fun _ -> RESP_COND_AUTH_PREAUTH) (str "PREAUTH")) >>= *)
(*   fun rsp_type -> *)
(*   char ' ' >> resp_text >>= fun rsp_text -> *)
(*   ret {rsp_type; rsp_text} *)

(*
resp-cond-bye   = "BYE" SP resp-text
*)
(* let resp_cond_bye = *)
(*   str "BYE" >> char ' ' >> resp_text *)

(* (\* *)
(* greeting        = "*" SP (resp-cond-auth / resp-cond-bye) CRLF *)
(* *\) *)
(* let greeting = *)
(*   char '*' >> char ' ' >> *)
(*   alt *)
(*     (app (fun r -> GREETING_RESP_COND_AUTH r) resp_cond_auth) *)
(*     (app (fun r -> GREETING_RESP_COND_BYE r) resp_cond_bye) *)
(*   >>= fun x -> *)
(*   crlf >> *)
(*   ret x *)

(* (\* *)
(* continue-req    = "+" SP (resp-text / base64) CRLF *)
(* *\) *)
(* let continue_req = *)
(*   char '+' >> opt (char ' ') '\000' >> (\* we allow an optional space *\) *)
(*   alt *)
(*     (base64 >>= fun b64 -> crlf >> ret (CONTINUE_REQ_BASE64 b64)) *)
(*     (resp_text >>= fun rt -> crlf >> ret (CONTINUE_REQ_TEXT rt)) *)

(* let r_nz_number k d = *)
(*   match d.c with *)
(*   | '0' .. '9' -> badd d; readc (r_nz_number k) d *)
(*   | _ -> *)
(*       let s = buf d in *)
(*       let n = int_of_string s in *)
(*       ret (`Int n) k d *)

(* let rec r_search k d = *)
(*   if d.c = ' ' then readc (r_nz_number (r_search k)) d else *)
(*   k d *)

(* let r_status_att k d = *)
(*   if is_alpha d.c then (baddu d; readc (r_status_att k) d) else *)
(*   match buf d with *)
(*   | "MESSAGES" -> ret `Messages (r_space (r_number k)) d *)
(*   | "RECENT" -> ret `Recent (r_space (r_number k)) d *)
(*   | "UIDNEXT" -> ret `Uid_next (r_space (r_nz_number k)) d *)
(*   | "UIDVALIDITY" -> ret `Uid_validity (r_space (r_nz_number k)) d *)
(*   | "UNSEEN" -> ret `Unseen (r_space (r_number k)) d *)
(*   | s -> ret (err_unexpecteds s) k d *)

(* let r_status_att_list k d = *)
(*   match d.c with *)
(*   | ')' -> *)
(*       ret `Close (readc k) d *)
(*   | _ -> *)
(*       r_status_att begin fun d -> *)
(*         match d.c with *)
(*         | ' ' -> readc (r_status_att_list k) d *)
(*         | ')' -> ret `Close (readc k) d *)
(*         | _ -> ret (err_noclose) k d *)
(*       end d *)

(* let r_status k d = *)
(*   r_mailbox (r_space (r_openp (r_status_att_list k))) d *)

(* "* " *)
(* let accepts s k d = *)
(*   let rec loop i = *)
(*     if i >= String.length s then k d else *)
(*     if i >= d.i_max then await else *)
(*     if String.uppercase s.[i] = String.uppercase d.i.[i] then loop (i + 1) else *)
(*     ret (err_expecteds s) k d *)
(*   in *)
(*   loop 0 *)

(*
response-data   = "*" SP (resp-cond-state / resp-cond-bye /
                  mailbox-data / message-data / capability-data) CRLF
*)
(* let r_esponse_data_t k d = *)
(*   if is_alpha d.c then (baddu d; readc (r_response_data_txt k) d) else *)
(*   let k d = r_crlf k d in *)
(*   match buf d with *)
(*   | "OK" -> ret `Ok (readc (r_resp_text k) d *)
(*   | "NO" -> ret `No (readc (r_resp_text k)) d *)
(*   | "BAD" -> ret `Bad (readc (r_resp_text k)) d *)
(*   | "BYE" -> ret `Bye (readc (r_resp_text k)) d *)
(*   | "FLAGS" -> ret `Flags (readc (r_flag_list k)) d *)
(*   | "LIST" -> ret `List (readc (r_mailbox_list k)) d *)
(*   | "LSUB" -> ret `Lsub (readc (r_mailbox_list k)) d *)
(*   | "SEARCH" -> ret `Search (readc (r_search k)) d *)
(*   | "STATUS" -> ret `Status (readc (r_status k)) d *)
(*   | "CAPABILITY" -> ret `Capability (readc (r_capability k)) d *)
(*   | s -> ret (err_resp_data s) k d *)

(* let r_response_data_n k d = *)
(*   match d.c with *)
(*   | '0' .. '9' -> badd d; readc (r_response_data_n k) d *)
(*   | ' ' -> *)
(*       begin *)
(*         let s = buf d in *)
(*         try *)
(*           let n = int_of_string s in *)
(*           ret (`Int n) (readc k) d *)
(*         with *)
(*         | Failure -> ret (err_num s) k d *)
(*       end *)
(*   | _ -> *)
(*       ret (err_unexpectedc d.c) k d *)

(* let r_response_data k d = *)
(*   if is_digit d.c then r_response_data_n  else *)
(*   r_response_data_text k d *)
(*   accepts "* " (fun d -> strs [] *)
(*   altn [ *)
(*     (app (fun r -> RESP_DATA_COND_STATE r) resp_cond_state); *)
(*     (app (fun r -> RESP_DATA_COND_BYE r) resp_cond_bye); *)
(*     (app (fun r -> RESP_DATA_MAILBOX_DATA r) mailbox_data); *)
(*     (app (fun r -> RESP_DATA_MESSAGE_DATA r) message_data); *)
(*     (app (fun r -> RESP_DATA_CAPABILITY_DATA r) capability_data); *)
(*     (app (fun e -> RESP_DATA_EXTENSION_DATA e) (extension_parser RESPONSE_DATA)) *)
(*     (\* namespace_response; *\) *)
(*   ] *)
(*   >>= fun x -> *)
(*   str "\r\n" >> *)
(*   ret x *)

(* (\* *)
(* response-fatal  = "*" SP resp-cond-bye CRLF *)
(*                     ; Server closes connection immediately *)
(* *\) *)
(* let response_fatal = *)
(*   char '*' >> char ' ' >> resp_cond_bye >>= fun x -> str "\r\n" >> ret x *)

(* (\* *)
(* response-done   = response-tagged / response-fatal *)
(* *\) *)
(* let response_done = *)
(*   alt *)
(*     (app (fun r -> RESP_DONE_TAGGED r) response_tagged) *)
(*     (app (fun r -> RESP_DONE_FATAL r) response_fatal) *)

(* let cont_req_or_resp_data = *)
(*   alt *)
(*     (app (fun r -> RESP_CONT_REQ r) continue_req) *)
(*     (app (fun r -> RESP_CONT_DATA r) response_data) *)

(* let response = *)
(*   rep cont_req_or_resp_data >>= fun rsp_cont_req_or_resp_data_list -> *)
(*   response_done >>= fun rsp_resp_done -> *)
(*   ret {rsp_cont_req_or_resp_data_list; rsp_resp_done} *)
