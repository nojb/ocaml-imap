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

open Sexplib.Std
open Response

exception Error of string * int

type buffer =
  {
    read_line: (string -> unit) -> unit;
    mutable line: string;
    mutable pos: int;
  }

let is_eol buf =
  buf.pos >= String.length buf.line

let curr buf =
  if buf.pos >= String.length buf.line then
    '\000'
  else
    buf.line.[buf.pos]

let next buf =
  buf.pos <- buf.pos + 1

let error buf =
  raise (Error (buf.line, buf.pos))

let char c buf =
  if curr buf = c then next buf else error buf

let take_while1 f buf =
  let pos0 = buf.pos in
  let pos = ref pos0 in
  while !pos < String.length buf.line && f buf.line.[!pos] do
    incr pos
  done;
  if pos0 = !pos then error buf;
  buf.pos <- !pos;
  String.sub buf.line pos0 (!pos - pos0)

let is_atom_char = function
  | '(' | ')' | '{' | ' '
  | '\x00' .. '\x1F' | '\x7F'
  | '%' | '*' | '"' | '\\' | ']' -> false
  | '\x01' .. '\x7F' -> true
  | _ -> false

let atom =
  take_while1 is_atom_char

let quoted_char r buf =
  match curr buf with
  | '\\' ->
      next buf;
      begin match curr buf with
      | '\\' | '"' as c ->
          next buf;
          r := c;
          true
      | _ ->
          error buf
      end
  | '"' ->
      false
  | '\x01'..'\x7f' as c ->
      next buf;
      r := c;
      true
  | _ ->
      error buf

let quoted buf =
  char '"' buf;
  let b = Buffer.create 17 in
  let r = ref '\000' in
  while quoted_char r buf do Buffer.add_char b !r done;
  char '"' buf;
  Buffer.contents b

let literal buf _k =
  error buf

let imap_string buf k =
  match curr buf with
  | '"' ->
      k (quoted buf)
  | '{' ->
      literal buf k
  | _ ->
      assert false

let is_astring_char c =
  is_atom_char c || c = ']'

let astring buf k =
  match curr buf with
  | '"' | '{' ->
      imap_string buf k
  | _ ->
      k (take_while1 is_astring_char buf)

let is_text_char = function
  | '\r' | '\n' -> false
  | '\x01' .. '\x7F' -> true
  | _ -> false

let text buf =
  if is_eol buf then "" else take_while1 is_text_char buf

let is_text_other_char c =
  is_text_char c && (c <> ']')

let text_1 buf =
  if is_eol buf then "" else take_while1 is_text_other_char buf

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let number buf =
  Scanf.sscanf (take_while1 is_digit buf) "%lu" (fun n -> n)

let nz_number =
  number

let uniqueid =
  number

let mbx_flag buf =
  let open MailboxFlag in
  char '\\' buf;
  let a = atom buf in
  match String.lowercase_ascii a with
  | "noselect" -> Noselect
  | "marked" -> Marked
  | "unmarked" -> Unmarked
  | "noinferiors" -> Noinferiors
  | "haschildren" -> HasChildren
  | "hasnochildren" -> HasNoChildren
  | "all" -> All
  | "archive" -> Archive
  | "drafts" -> Drafts
  | "flagged" -> Flagged
  | "junk" -> Junk
  | "sent" -> Sent
  | "trash" -> Trash
  | _ -> Extension a

let delim buf =
  match curr buf with
  | '"' ->
      next buf;
      let r = ref '\000' in
      if quoted_char r buf then
        (char '"' buf; Some !r)
      else
        error buf
  | _ ->
      char 'N' buf;
      char 'I' buf;
      char 'L' buf;
      None

let is_inbox s =
  String.length s = String.length "INBOX" &&
  String.uppercase_ascii s = "INBOX"

let mailbox buf k =
  astring buf (fun s -> if is_inbox s then k "INBOX" else k s)

let mailbox_list buf k =
  char '(' buf;
  let flags =
    if curr buf = ')' then []
    else
      let rec loop acc buf =
        if curr buf = ' ' then
          (next buf; loop (mbx_flag buf :: acc) buf)
        else
          List.rev acc
      in
      loop [mbx_flag buf] buf
  in
  char ')' buf;
  char ' ' buf;
  let delim = delim buf in
  char ' ' buf;
  mailbox buf (k flags delim)

let capability buf =
  let open Capability in
  match atom buf with
  | "COMPRESS=DEFLATE" -> COMPRESS_DEFLATE
  | "CONDSTORE" -> CONDSTORE
  | "ESEARCH" -> ESEARCH
  | "ENABLE" -> ENABLE
  | "IDLE" -> IDLE
  | "LITERAL+" -> LITERALPLUS
  | "LITERAL-" -> LITERALMINUS
  | "UTF8=ACCEPT" -> UTF8_ACCEPT
  | "UTF8=ONLY" -> UTF8_ONLY
  | "NAMESPACE" -> NAMESPACE
  | "ID" -> ID
  | "QRESYNC" -> QRESYNC
  | "UIDPLUS" -> UIDPLUS
  | "UNSELECT" -> UNSELECT
  | "XLIST" -> XLIST
  | "AUTH=PLAIN" -> AUTH_PLAIN
  | "AUTH=LOGIN" -> AUTH_LOGIN
  | "XOAUTH2" -> XOAUTH2
  | "X-GM-EXT-1"  -> X_GM_EXT_1
  | a -> OTHER a

let mod_sequence_value buf =
  Scanf.sscanf (take_while1 is_digit buf) "%Lu" (fun n -> n)

let uid_range buf =
  let n = uniqueid buf in
  if curr buf = ':' then
    (next buf; (n, uniqueid buf))
  else
    (n, n)

let uid_set buf =
  let rec loop acc buf =
    match curr buf with
    | ',' ->
        next buf;
        loop (uid_range buf :: acc) buf
    | _ ->
        List.rev acc
  in
  loop [uid_range buf] buf

let sequence_set =
  uid_set

let set =
  sequence_set

let resp_text_code buf k =
  let open Code in
  let k code = char ']' buf; k code in
  char '[' buf;
  match atom buf with
  | "ALERT" ->
      k ALERT
  (* | "BADCHARSET" ->
   *     many (sp *> astring) >>| (fun l -> BADCHARSET l) *)
  | "CAPABILITY" ->
      let rec loop acc buf =
        if curr buf = ' ' then
          (next buf; loop (capability buf :: acc) buf)
        else
          k (CAPABILITY (List.rev acc))
      in
      loop [] buf
  | "PARSE" ->
      k PARSE
  (* | "PERMANENTFLAGS" ->
   *     sp *> psep_by sp flag_perm >>| (fun l -> PERMANENTFLAGS l) *)
  | "READ-ONLY" ->
      k READ_ONLY
  | "READ-WRITE" ->
      k READ_WRITE
  | "TRYCREATE" ->
      k TRYCREATE
  | "UIDNEXT" ->
      char ' ' buf;
      k (UIDNEXT (nz_number buf))
  | "UIDVALIDITY" ->
      char ' ' buf;
      k (UIDVALIDITY (nz_number buf))
  | "UNSEEN" ->
      char ' ' buf;
      k (UNSEEN (nz_number buf))
  | "CLOSED" ->
      k CLOSED
  | "HIGHESTMODSEQ" ->
      char ' ' buf;
      k (HIGHESTMODSEQ (mod_sequence_value buf))
  | "NOMODSEQ" ->
      k NOMODSEQ
  | "MODIFIED" ->
      char ' ' buf;
      k (MODIFIED (set buf))
  (* | "APPENDUID" ->
   *     sp *> pair sp nz_number append_uid >>| (fun (n, uid) -> APPENDUID (n, uid)) *)
  (* | "COPYUID" ->
   *     sp *> triple sp nz_number set set >>| (fun (n, s1, s2) -> COPYUID (n, s1, s2)) *)
  | "UIDNOTSTICKY" ->
      k UIDNOTSTICKY
  | "COMPRESSIONACTIVE" ->
      k COMPRESSIONACTIVE
  | "USEATTR" ->
      k USEATTR
  | a ->
      let x = if curr buf = ' ' then Some (text_1 buf) else None in
      k (OTHER (a, x))

let resp_text buf k =
  let code buf k =
    match curr buf with
    | '[' -> resp_text_code buf (fun code -> k (Some code))
    | _ -> k None
  in
  code buf (fun code -> if curr buf = ' ' then next buf; k code (text buf))

let search_sort_mod_seq buf =
  char '(' buf;
  match atom buf with
  | "MODSEQ" ->
      char ' ' buf;
      let n = mod_sequence_value buf in
      char ')' buf;
      n
  | _ ->
      error buf

let permsg_modsequence =
  mod_sequence_value

let flag_with_recent with_recent buf =
  let open Flag in
  match curr buf with
  | '\\' ->
      next buf;
      let a = atom buf in
      begin match String.lowercase_ascii a with
      | "recent" when with_recent -> Recent
      | "answered" -> Answered
      | "flagged" -> Flagged
      | "deleted" -> Deleted
      | "seen" -> Seen
      | "draft" -> Draft
      | _ -> Extension a
      end
  | _ ->
      Keyword (atom buf)

let flag =
  flag_with_recent false

let flag_fetch =
  flag_with_recent true

let nstring buf k =
  match curr buf with
  | '"' | '{' ->
      imap_string buf k
  | _ ->
      char 'N' buf;
      char 'I' buf;
      char 'L' buf;
      k ""

let msg_att buf k =
  let open Fetch.MessageAttribute in
  match atom buf with
  | "FLAGS" ->
      char ' ' buf;
      char '(' buf;
      let flags =
        if curr buf = ')' then
          (next buf; [])
        else
          let rec loop acc buf =
            if curr buf = ' ' then
              (next buf; loop (flag_fetch buf :: acc) buf)
            else
              (char ')' buf; List.rev acc)
          in
          loop [flag_fetch buf] buf
      in
      k (FLAGS flags)
  | "MODSEQ" ->
      char ' ' buf;
      char '(' buf;
      let n = permsg_modsequence buf in
      char ')' buf;
      k (MODSEQ n)
  (* | "X-GM-LABELS" ->
   *     sp *> choice [psep_by sp astring; nil *> return []] >>| (fun l -> X_GM_LABELS l) *)
  (* | "ENVELOPE" ->
   *     sp *> envelope >>| (fun e -> ENVELOPE e) *)
  (* | "INTERNALDATE" ->
   *     sp *> date_time >>| (fun (d, t) -> INTERNALDATE (d, t)) *)
  | "RFC822.HEADER" ->
      char ' ' buf;
      nstring buf (fun s -> k (RFC822_HEADER s));
  | "RFC822.TEXT" ->
      char ' ' buf;
      nstring buf (fun s -> k (RFC822_TEXT s));
  | "RFC822.SIZE" ->
      char ' ' buf;
      k (RFC822_SIZE (Int32.to_int (number buf)))
  (* | "RFC822" ->
   *     sp *> nstring' >>| (fun s -> RFC822 s) *)
  (* | "BODYSTRUCTURE" ->
   *     sp *> body >>| (fun b -> BODYSTRUCTURE b) *)
  (* | "BODY" ->
   *     let section =
   *       section >>= fun s -> sp *> nstring >>| fun x ->
   *       BODY_SECTION (s, x)
   *     in
   *     choice [sp *> body >>| (fun b -> BODY b); section] *)
  | "UID" ->
      char ' ' buf;
      k (UID (uniqueid buf))
  | "X-GM-MSGID" ->
      char ' ' buf;
      k (X_GM_MSGID (mod_sequence_value buf))
  | "X-GM-THRID" ->
      char ' ' buf;
      k (X_GM_THRID (mod_sequence_value buf))
  | _ ->
      error buf

let msg_att buf k =
  char '(' buf;
  let rec loop acc buf k =
    if curr buf = ' ' then
      (next buf; msg_att buf (fun att -> loop (att :: acc) buf k))
    else
      (char ')' buf; k (List.rev acc))
  in
  msg_att buf (fun att -> loop [att] buf k)

let mod_sequence_valzer buf =
  Scanf.sscanf (take_while1 is_digit buf) "%Lu" (fun n -> n)

let status_att buf =
  let open Status.MailboxAttribute in
  match atom buf with
  | "MESSAGES" ->
      char ' ' buf;
      MESSAGES (Int32.to_int (number buf))
  | "RECENT" ->
      char ' ' buf;
      RECENT (Int32.to_int (number buf))
  | "UIDNEXT" ->
      char ' ' buf;
      UIDNEXT (number buf)
  | "UIDVALIDITY" ->
      char ' ' buf;
      UIDVALIDITY (number buf)
  | "UNSEEN" ->
      char ' ' buf;
      UNSEEN (Int32.to_int (number buf))
  | "HIGHESTMODSEQ" ->
      char ' ' buf;
      HIGHESTMODSEQ (mod_sequence_valzer buf)
  | _ ->
      error buf

let known_ids =
  uid_set

let response_data buf k =
  char '*' buf;
  char ' ' buf;
  let open Response.Untagged in
  match curr buf with
  | '0'..'9' ->
      let n = number buf in
      char ' ' buf;
      begin match atom buf with
      | "EXISTS" ->
          k (EXISTS (Int32.to_int n))
      | "RECENT" ->
          k (RECENT (Int32.to_int n))
      | "EXPUNGE" ->
          k (EXPUNGE n)
      | "FETCH" ->
          char ' ' buf;
          msg_att buf (fun x -> k (FETCH (n, x)))
      | _ ->
          error buf
      end
  | _ ->
      begin match atom buf with
      | "OK" ->
          if curr buf = ' ' then next buf;
          resp_text buf (fun code text -> k (State (OK (code, text))))
      | "NO" ->
          if curr buf = ' ' then next buf;
          resp_text buf (fun code text -> k (State (NO (code, text))))
      | "BAD" ->
          if curr buf = ' ' then next buf;
          resp_text buf (fun code text -> k (State (BAD (code, text))))
      | "BYE" ->
          if curr buf = ' ' then next buf;
          resp_text buf (fun code text -> k (BYE (code, text)))
      | "FLAGS" ->
          char ' ' buf;
          char '(' buf;
          let l =
            if curr buf = ')' then
              (next buf; [])
            else
              let rec loop acc buf =
                if curr buf = ' ' then
                  (next buf; loop (flag buf :: acc) buf)
                else
                  (char ')' buf; List.rev acc)
              in
              loop [flag buf] buf
          in
          k (FLAGS l)
      | "LIST" ->
          char ' ' buf;
          mailbox_list buf (fun xs c m -> k (LIST (xs, c, m)))
      | "LSUB" ->
          char ' ' buf;
          mailbox_list buf (fun xs c m -> k (LSUB (xs, c, m)))
      | "SEARCH" ->
          let rec loop acc buf =
            if curr buf = ' ' then begin
              next buf;
              if curr buf = '(' then
                List.rev acc, Some (search_sort_mod_seq buf)
              else
                loop (nz_number buf :: acc) buf
            end else
              List.rev acc, None
          in
          let nums, modseq = loop [] buf in
          k (SEARCH (nums, modseq))
      | "STATUS" ->
          char ' ' buf;
          mailbox buf (fun mbox ->
              char ' ' buf;
              char '(' buf;
              let l =
                if curr buf = ')' then
                  (next buf; [])
                else
                  let rec loop acc buf =
                    if curr buf = ' ' then
                      (next buf; loop (status_att buf :: acc) buf)
                    else
                      (char ')' buf; List.rev acc)
                  in
                  loop [status_att buf] buf
              in
              k (STATUS (mbox, l))
            )
      | "CAPABILITY" ->
          let rec loop acc buf =
            if curr buf = ' ' then
              (next buf; loop (capability buf :: acc) buf)
            else
              k (CAPABILITY (List.rev acc))
          in
          loop [] buf
      | "ENABLED" ->
          let rec loop acc buf =
            if curr buf = ' ' then
              (next buf; loop (capability buf :: acc) buf)
            else
              k (ENABLED (List.rev acc))
          in
          loop [] buf
      | "PREAUTH" ->
          if curr buf = ' ' then next buf;
          resp_text buf (fun code text -> k (PREAUTH (code, text)))
      | "VANISHED" ->
          char ' ' buf;
          if curr buf = '(' then begin
            next buf;
            match atom buf with
            | "EARLIER" ->
                char ')' buf;
                char ' ' buf;
                k (VANISHED_EARLIER (known_ids buf))
            | _ ->
                error buf
          end else
            k (VANISHED (known_ids buf))
      | _ ->
          error buf
      end

let is_tag_char = function
  | '+' -> false
  | c -> is_astring_char c

let tag =
  take_while1 is_tag_char

let resp_cond_state buf k =
  let open Response.State in
  match atom buf with
  | "OK" ->
      if curr buf = ' ' then next buf;
      resp_text buf (fun code text -> k (OK (code, text)))
  | "NO" ->
      if curr buf = ' ' then next buf;
      resp_text buf (fun code text -> k (NO (code, text)))
  | "BAD" ->
      if curr buf = ' ' then next buf;
      resp_text buf (fun code text -> k (BAD (code, text)))
  | _ ->
      error buf

let response buf k =
  match curr buf with
  | '+' ->
      next buf;
      if curr buf = ' ' then next buf;
      resp_text buf (fun _ x -> k (Cont x))
  | '*' ->
      response_data buf (fun u -> k (Untagged u))
  | _ ->
      let tag = tag buf in
      char ' ' buf;
      resp_cond_state buf (fun state -> k (Tagged (tag, state)))

let parse s =
  let buf =
    { read_line = (fun _k -> assert false);
      line = s;
      pos = 0 }
  in
  let result = ref (Cont "") in
  match response buf (fun u -> result := u) with
  | () ->
      !result |> Response.sexp_of_t |> Sexplib.Sexp.to_string_hum |> print_endline
  | exception Error (line, pos) ->
      Printf.eprintf "Parsing error:\n%s\n%s^\n" line (String.make pos ' ')

let%expect_test _ =
  parse {|+ YGgGCSqGSIb3EgECAgIAb1kwV6ADAgEFoQMCAQ+iSzBJoAMC|};
  [%expect {| (Cont YGgGCSqGSIb3EgECAgIAb1kwV6ADAgEFoQMCAQ+iSzBJoAMC) |}]

let%expect_test _ =
  parse {|+ YDMGCSqGSIb3EgECAgIBAAD/////6jcyG4GE3KkTzBeBiVHe|};
  [%expect {| (Cont YDMGCSqGSIb3EgECAgIBAAD/////6jcyG4GE3KkTzBeBiVHe) |}]

let%expect_test _ =
  parse {|+|};
  [%expect {| (Cont "") |}]

let%expect_test _ =
  parse {|+ Ready for literal data|};
  [%expect {| (Cont "Ready for literal data") |}]

let%expect_test _ =
  parse {|+ Ready for additional command text|};
  [%expect {| (Cont "Ready for additional command text") |}]

let%expect_test _ =
  parse {|abcd OK CAPABILITY completed|};
  [%expect {| (Tagged abcd (OK () "CAPABILITY completed")) |}]

let%expect_test _ =
  parse {|efgh OK STARTLS completed|};
  [%expect {| (Tagged efgh (OK () "STARTLS completed")) |}]

let%expect_test _ =
  parse {|ijkl OK CAPABILITY completed|};
  [%expect {| (Tagged ijkl (OK () "CAPABILITY completed")) |}]

let%expect_test _ =
  parse {|a002 OK NOOP completed|};
  [%expect {| (Tagged a002 (OK () "NOOP completed")) |}]

let%expect_test _ =
  parse {|a047 OK NOOP completed|};
  [%expect {| (Tagged a047 (OK () "NOOP completed")) |}]

let%expect_test _ =
  parse {|A023 OK LOGOUT completed|};
  [%expect {| (Tagged A023 (OK () "LOGOUT completed")) |}]

let%expect_test _ =
  parse {|a001 OK CAPABILITY completed|};
  [%expect {| (Tagged a001 (OK () "CAPABILITY completed")) |}]

let%expect_test _ =
  parse {|a002 OK Begin TLS negotiation now|};
  [%expect {| (Tagged a002 (OK () "Begin TLS negotiation now")) |}]

let%expect_test _ =
  parse {|a003 OK CAPABILITY completed|};
  [%expect {| (Tagged a003 (OK () "CAPABILITY completed")) |}]

let%expect_test _ =
  parse {|a004 OK LOGIN completed|};
  [%expect {| (Tagged a004 (OK () "LOGIN completed")) |}]

let%expect_test _ =
  parse {|A001 OK GSSAPI authentication successful|};
  [%expect {| (Tagged A001 (OK () "GSSAPI authentication successful")) |}]

let%expect_test _ =
  parse {|a001 OK LOGIN completed|};
  [%expect {| (Tagged a001 (OK () "LOGIN completed")) |}]

let%expect_test _ =
  parse {|A142 OK [READ-WRITE] SELECT completed|};
  [%expect {| (Tagged A142 (OK (READ_WRITE) "SELECT completed")) |}]

let%expect_test _ =
  parse {|A932 OK [READ-ONLY] EXAMINE completed|};
  [%expect {| (Tagged A932 (OK (READ_ONLY) "EXAMINE completed")) |}]

let%expect_test _ =
  parse {|A003 OK CREATE completed|};
  [%expect {| (Tagged A003 (OK () "CREATE completed")) |}]

let%expect_test _ =
  parse {|A004 OK CREATE completed|};
  [%expect {| (Tagged A004 (OK () "CREATE completed")) |}]

let%expect_test _ =
  parse {|A682 OK LIST completed|};
  [%expect {| (Tagged A682 (OK () "LIST completed")) |}]

let%expect_test _ =
  parse {|A683 OK DELETE completed|};
  [%expect {| (Tagged A683 (OK () "DELETE completed")) |}]

let%expect_test _ =
  parse {|A684 NO Name "foo" has inferior hierarchical names|};
  [%expect {| (Tagged A684 (NO () "Name \"foo\" has inferior hierarchical names")) |}]

let%expect_test _ =
  parse {|A685 OK DELETE Completed|};
  [%expect {| (Tagged A685 (OK () "DELETE Completed")) |}]

let%expect_test _ =
  parse {|A686 OK LIST completed|};
  [%expect {| (Tagged A686 (OK () "LIST completed")) |}]

let%expect_test _ =
  parse {|A687 OK DELETE Completed|};
  [%expect {| (Tagged A687 (OK () "DELETE Completed")) |}]

let%expect_test _ =
  parse {|A82 OK LIST completed|};
  [%expect {| (Tagged A82 (OK () "LIST completed")) |}]

let%expect_test _ =
  parse {|A83 OK DELETE completed|};
  [%expect {| (Tagged A83 (OK () "DELETE completed")) |}]

let%expect_test _ =
  parse {|A84 OK DELETE Completed|};
  [%expect {| (Tagged A84 (OK () "DELETE Completed")) |}]

let%expect_test _ =
  parse {|* CAPABILITY IMAP4rev1 STARTTLS AUTH=GSSAPI|};
  [%expect {|
    (Untagged
     (CAPABILITY ((OTHER IMAP4rev1) (OTHER STARTTLS) (OTHER AUTH=GSSAPI)))) |}]

let%expect_test _ =
  parse {|* CAPABILITY IMAP4rev1 AUTH=GSSAPI AUTH=PLAIN|};
  [%expect {| (Untagged (CAPABILITY ((OTHER IMAP4rev1) (OTHER AUTH=GSSAPI) AUTH_PLAIN))) |}]

let%expect_test _ =
  parse {|* 22 EXPUNGE|};
  [%expect {| (Untagged (EXPUNGE 22)) |}]

let%expect_test _ =
  parse {|* 23 EXISTS|};
  [%expect {| (Untagged (EXISTS 23)) |}]

let%expect_test _ =
  parse {|* 3 RECENT|};
  [%expect {| (Untagged (RECENT 3)) |}]

let%expect_test _ =
  parse {|* 14 FETCH (FLAGS (\Seen \Deleted))|};
  [%expect {| (Untagged (FETCH 14 ((FLAGS (Seen Deleted))))) |}]

let%expect_test _ =
  parse {|* BYE IMAP4rev1 Server logging out|};
  [%expect {| (Untagged (BYE () "IMAP4rev1 Server logging out")) |}]

let%expect_test _ =
  parse {|* CAPABILITY IMAP4rev1 STARTTLS LOGINDISABLED|};
  [%expect {|
    (Untagged
     (CAPABILITY ((OTHER IMAP4rev1) (OTHER STARTTLS) (OTHER LOGINDISABLED)))) |}]

let%expect_test _ =
  parse {|* CAPABILITY IMAP4rev1 AUTH=PLAIN|};
  [%expect {| (Untagged (CAPABILITY ((OTHER IMAP4rev1) AUTH_PLAIN))) |}]

let%expect_test _ =
  parse {|* OK IMAP4rev1 Server|};
  [%expect {| (Untagged (State (OK () "IMAP4rev1 Server"))) |}]

let%expect_test _ =
  parse {|* 172 EXISTS|};
  [%expect {| (Untagged (EXISTS 172)) |}]

let%expect_test _ =
  parse {|* 1 RECENT|};
  [%expect {| (Untagged (RECENT 1)) |}]

let%expect_test _ =
  parse {|* OK [UNSEEN 12] Message 12 is first unseen|};
  [%expect {| (Untagged (State (OK ((UNSEEN 12)) "Message 12 is first unseen"))) |}]

let%expect_test _ =
  parse {|* OK [UIDVALIDITY 3857529045] UIDs valid|};
  [%expect {| (Untagged (State (OK ((UIDVALIDITY -437438251)) "UIDs valid"))) |}]

let%expect_test _ =
  parse {|* OK [UIDNEXT 4392] Predicted next UID|};
  [%expect {| (Untagged (State (OK ((UIDNEXT 4392)) "Predicted next UID"))) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Deleted Seen Draft))) |}]

let%expect_test _ =
  parse {|* OK [PERMANENTFLAGS (\Deleted \Seen \*)] Limited|};
  [%expect {|
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Deleted \\Seen \\*)"))) Limited))) |}]

let%expect_test _ =
  parse {|* 17 EXISTS|};
  [%expect {| (Untagged (EXISTS 17)) |}]

let%expect_test _ =
  parse {|* 2 RECENT|};
  [%expect {| (Untagged (RECENT 2)) |}]

let%expect_test _ =
  parse {|* OK [UNSEEN 8] Message 8 is first unseen|};
  [%expect {| (Untagged (State (OK ((UNSEEN 8)) "Message 8 is first unseen"))) |}]

let%expect_test _ =
  parse {|* OK [UIDNEXT 4392] Predicted next UID|};
  [%expect {| (Untagged (State (OK ((UIDNEXT 4392)) "Predicted next UID"))) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Deleted Seen Draft))) |}]

let%expect_test _ =
  parse {|* OK [PERMANENTFLAGS ()] No permanent flags permitted|};
  [%expect {|
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" ()"))) "No permanent flags permitted"))) |}]

let%expect_test _ =
  parse {|* LIST () "/" blurdybloop|};
  [%expect {| (Untagged (LIST () (/) blurdybloop)) |}]

let%expect_test _ =
  parse {|* LIST (\Noselect) "/" foo|};
  [%expect {| (Untagged (LIST (Noselect) (/) foo)) |}]

let%expect_test _ =
  parse {|* LIST () "/" foo/bar|};
  [%expect {| (Untagged (LIST () (/) foo/bar)) |}]

let%expect_test _ =
  parse {|* LIST (\Noselect) "/" foo|};
  [%expect {| (Untagged (LIST (Noselect) (/) foo)) |}]

let%expect_test _ =
  parse {|* LIST () "." blurdybloop|};
  [%expect {| (Untagged (LIST () (.) blurdybloop)) |}]

let%expect_test _ =
  parse {|* LIST () "." foo|};
  [%expect {| (Untagged (LIST () (.) foo)) |}]

let%expect_test _ =
  parse {|* LIST () "." foo.bar|};
  [%expect {| (Untagged (LIST () (.) foo.bar)) |}]

let%expect_test _ =
  parse {|* LIST () "." foo.bar|};
  [%expect {| (Untagged (LIST () (.) foo.bar)) |}]

let%expect_test _ =
  parse {|A85 OK LIST completed|};
  [%expect {| (Tagged A85 (OK () "LIST completed")) |}]

let%expect_test _ =
  parse {|* LIST (\Noselect) "." foo|};
  [%expect {| (Untagged (LIST (Noselect) (.) foo)) |}]

let%expect_test _ =
  parse {|A86 OK LIST completed|};
  [%expect {| (Tagged A86 (OK () "LIST completed")) |}]

let%expect_test _ =
  parse {|* LIST () "/" blurdybloop|};
  [%expect {| (Untagged (LIST () (/) blurdybloop)) |}]

let%expect_test _ =
  parse {|* LIST (\Noselect) "/" foo|};
  [%expect {| (Untagged (LIST (Noselect) (/) foo)) |}]

let%expect_test _ =
  parse {|* LIST () "/" foo/bar|};
  [%expect {| (Untagged (LIST () (/) foo/bar)) |}]

let%expect_test _ =
  parse {|A682 OK LIST completed|};
  [%expect {| (Tagged A682 (OK () "LIST completed")) |}]

let%expect_test _ =
  parse {|A683 OK RENAME completed|};
  [%expect {| (Tagged A683 (OK () "RENAME completed")) |}]

let%expect_test _ =
  parse {|A684 OK RENAME Completed|};
  [%expect {| (Tagged A684 (OK () "RENAME Completed")) |}]

let%expect_test _ =
  parse {|* LIST () "/" sarasoop|};
  [%expect {| (Untagged (LIST () (/) sarasoop)) |}]

let%expect_test _ =
  parse {|* LIST (\Noselect) "/" zowie|};
  [%expect {| (Untagged (LIST (Noselect) (/) zowie)) |}]

let%expect_test _ =
  parse {|* LIST () "/" zowie/bar|};
  [%expect {| (Untagged (LIST () (/) zowie/bar)) |}]

let%expect_test _ =
  parse {|A685 OK LIST completed|};
  [%expect {| (Tagged A685 (OK () "LIST completed")) |}]

let%expect_test _ =
  parse {|* LIST () "." INBOX|};
  [%expect {| (Untagged (LIST () (.) INBOX)) |}]

let%expect_test _ =
  parse {|* LIST () "." INBOX.bar|};
  [%expect {| (Untagged (LIST () (.) INBOX.bar)) |}]

let%expect_test _ =
  parse {|Z432 OK LIST completed|};
  [%expect {| (Tagged Z432 (OK () "LIST completed")) |}]

let%expect_test _ =
  parse {|Z433 OK RENAME completed|};
  [%expect {| (Tagged Z433 (OK () "RENAME completed")) |}]

let%expect_test _ =
  parse {|* LIST () "." INBOX|};
  [%expect {| (Untagged (LIST () (.) INBOX)) |}]

let%expect_test _ =
  parse {|* LIST () "." INBOX.bar|};
  [%expect {| (Untagged (LIST () (.) INBOX.bar)) |}]

let%expect_test _ =
  parse {|* LIST () "." old-mail|};
  [%expect {| (Untagged (LIST () (.) old-mail)) |}]

let%expect_test _ =
  parse {|Z434 OK LIST completed|};
  [%expect {| (Tagged Z434 (OK () "LIST completed")) |}]

let%expect_test _ =
  parse {|A002 OK SUBSCRIBE completed|};
  [%expect {| (Tagged A002 (OK () "SUBSCRIBE completed")) |}]

let%expect_test _ =
  parse {|A002 OK UNSUBSCRIBE completed|};
  [%expect {| (Tagged A002 (OK () "UNSUBSCRIBE completed")) |}]

let%expect_test _ =
  parse {|* LIST (\Noselect) "/" ""|};
  [%expect {| (Untagged (LIST (Noselect) (/) "")) |}]

let%expect_test _ =
  parse {|A101 OK LIST Completed|};
  [%expect {| (Tagged A101 (OK () "LIST Completed")) |}]

let%expect_test _ =
  parse {|* LIST (\Noselect) "." #news.|};
  [%expect {| (Untagged (LIST (Noselect) (.) #news.)) |}]

let%expect_test _ =
  parse {|A102 OK LIST Completed|};
  [%expect {| (Tagged A102 (OK () "LIST Completed")) |}]

let%expect_test _ =
  parse {|* LIST (\Noselect) "/" /|};
  [%expect {| (Untagged (LIST (Noselect) (/) /)) |}]

let%expect_test _ =
  parse {|A103 OK LIST Completed|};
  [%expect {| (Tagged A103 (OK () "LIST Completed")) |}]

let%expect_test _ =
  parse {|* LIST (\Noselect) "/" ~/Mail/foo|};
  [%expect {| (Untagged (LIST (Noselect) (/) ~/Mail/foo)) |}]

let%expect_test _ =
  parse {|* LIST () "/" ~/Mail/meetings|};
  [%expect {| (Untagged (LIST () (/) ~/Mail/meetings)) |}]

let%expect_test _ =
  parse {|A202 OK LIST completed|};
  [%expect {| (Tagged A202 (OK () "LIST completed")) |}]

let%expect_test _ =
  parse {|* LSUB () "." #news.comp.mail.mime|};
  [%expect {| (Untagged (LSUB () (.) #news.comp.mail.mime)) |}]

let%expect_test _ =
  parse {|* LSUB () "." #news.comp.mail.misc|};
  [%expect {| (Untagged (LSUB () (.) #news.comp.mail.misc)) |}]

let%expect_test _ =
  parse {|A002 OK LSUB completed|};
  [%expect {| (Tagged A002 (OK () "LSUB completed")) |}]

let%expect_test _ =
  parse {|* LSUB (\NoSelect) "." #news.comp.mail|};
  [%expect {| (Untagged (LSUB (Noselect) (.) #news.comp.mail)) |}]

let%expect_test _ =
  parse {|A003 OK LSUB completed|};
  [%expect {| (Tagged A003 (OK () "LSUB completed")) |}]

let%expect_test _ =
  parse {|* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)|};
  [%expect {| (Untagged (STATUS blurdybloop ((MESSAGES 231) (UIDNEXT 44292)))) |}]

let%expect_test _ =
  parse {|A042 OK STATUS completed|};
  [%expect {| (Tagged A042 (OK () "STATUS completed")) |}]

let%expect_test _ =
  parse {|A003 OK APPEND completed|};
  [%expect {| (Tagged A003 (OK () "APPEND completed")) |}]

let%expect_test _ =
  parse {|FXXZ OK CHECK Completed|};
  [%expect {| (Tagged FXXZ (OK () "CHECK Completed")) |}]

let%expect_test _ =
  parse {|A341 OK CLOSE completed|};
  [%expect {| (Tagged A341 (OK () "CLOSE completed")) |}]

let%expect_test _ =
  parse {|* 3 EXPUNGE|};
  [%expect {| (Untagged (EXPUNGE 3)) |}]

let%expect_test _ =
  parse {|* 3 EXPUNGE|};
  [%expect {| (Untagged (EXPUNGE 3)) |}]

let%expect_test _ =
  parse {|* 5 EXPUNGE|};
  [%expect {| (Untagged (EXPUNGE 5)) |}]

let%expect_test _ =
  parse {|* 8 EXPUNGE|};
  [%expect {| (Untagged (EXPUNGE 8)) |}]

let%expect_test _ =
  parse {|A202 OK EXPUNGE completed|};
  [%expect {| (Tagged A202 (OK () "EXPUNGE completed")) |}]

let%expect_test _ =
  parse {|* SEARCH 2 84 882|};
  [%expect {| (Untagged (SEARCH (2 84 882) ())) |}]

let%expect_test _ =
  parse {|A282 OK SEARCH completed|};
  [%expect {| (Tagged A282 (OK () "SEARCH completed")) |}]

let%expect_test _ =
  parse {|* SEARCH|};
  [%expect {| (Untagged (SEARCH () ())) |}]

let%expect_test _ =
  parse {|A283 OK SEARCH completed|};
  [%expect {| (Tagged A283 (OK () "SEARCH completed")) |}]

let%expect_test _ =
  parse {|* SEARCH 43|};
  [%expect {| (Untagged (SEARCH (43) ())) |}]

let%expect_test _ =
  parse {|A284 OK SEARCH completed|};
  [%expect {| (Tagged A284 (OK () "SEARCH completed")) |}]

let%expect_test _ =
  parse {|A654 OK FETCH completed|};
  [%expect {| (Tagged A654 (OK () "FETCH completed")) |}]

let%expect_test _ =
  parse {|* 2 FETCH (FLAGS (\Deleted \Seen))|};
  [%expect {| (Untagged (FETCH 2 ((FLAGS (Deleted Seen))))) |}]

let%expect_test _ =
  parse {|* 3 FETCH (FLAGS (\Deleted))|};
  [%expect {| (Untagged (FETCH 3 ((FLAGS (Deleted))))) |}]

let%expect_test _ =
  parse {|* 4 FETCH (FLAGS (\Deleted \Flagged \Seen))|};
  [%expect {| (Untagged (FETCH 4 ((FLAGS (Deleted Flagged Seen))))) |}]

let%expect_test _ =
  parse {|A003 OK STORE completed|};
  [%expect {| (Tagged A003 (OK () "STORE completed")) |}]

let%expect_test _ =
  parse {|A003 OK COPY completed|};
  [%expect {| (Tagged A003 (OK () "COPY completed")) |}]

let%expect_test _ =
  parse {|* 23 FETCH (FLAGS (\Seen) UID 4827313)|};
  [%expect {| (Untagged (FETCH 23 ((FLAGS (Seen)) (UID 4827313)))) |}]

let%expect_test _ =
  parse {|* 24 FETCH (FLAGS (\Seen) UID 4827943)|};
  [%expect {| (Untagged (FETCH 24 ((FLAGS (Seen)) (UID 4827943)))) |}]

let%expect_test _ =
  parse {|* 25 FETCH (FLAGS (\Seen) UID 4828442)|};
  [%expect {| (Untagged (FETCH 25 ((FLAGS (Seen)) (UID 4828442)))) |}]

let%expect_test _ =
  parse {|A999 OK UID FETCH completed|};
  [%expect {| (Tagged A999 (OK () "UID FETCH completed")) |}]

let%expect_test _ =
  parse {|* CAPABILITY IMAP4rev1 XPIG-LATIN|};
  [%expect {| (Untagged (CAPABILITY ((OTHER IMAP4rev1) (OTHER XPIG-LATIN)))) |}]

let%expect_test _ =
  parse {|a441 OK CAPABILITY completed|};
  [%expect {| (Tagged a441 (OK () "CAPABILITY completed")) |}]

let%expect_test _ =
  parse {|A442 OK XPIG-LATIN ompleted-cay|};
  [%expect {| (Tagged A442 (OK () "XPIG-LATIN ompleted-cay")) |}]

let%expect_test _ =
  parse {|* OK IMAP4rev1 server ready|};
  [%expect {| (Untagged (State (OK () "IMAP4rev1 server ready"))) |}]

let%expect_test _ =
  parse {|* OK [ALERT] System shutdown in 10 minutes|};
  [%expect {| (Untagged (State (OK (ALERT) "System shutdown in 10 minutes"))) |}]

let%expect_test _ =
  parse {|A001 OK LOGIN Completed|};
  [%expect {| (Tagged A001 (OK () "LOGIN Completed")) |}]

let%expect_test _ =
  parse {|* NO Disk is 98% full, please delete unnecessary data|};
  [%expect {| (Untagged (State (NO () "Disk is 98% full, please delete unnecessary data"))) |}]

let%expect_test _ =
  parse {|A222 OK COPY completed|};
  [%expect {| (Tagged A222 (OK () "COPY completed")) |}]

let%expect_test _ =
  parse {|* NO Disk is 98% full, please delete unnecessary data|};
  [%expect {| (Untagged (State (NO () "Disk is 98% full, please delete unnecessary data"))) |}]

let%expect_test _ =
  parse {|* NO Disk is 99% full, please delete unnecessary data|};
  [%expect {| (Untagged (State (NO () "Disk is 99% full, please delete unnecessary data"))) |}]

let%expect_test _ =
  parse {|A223 NO COPY failed: disk is full|};
  [%expect {| (Tagged A223 (NO () "COPY failed: disk is full")) |}]

let%expect_test _ =
  parse {|* BAD Command line too long|};
  [%expect {| (Untagged (State (BAD () "Command line too long"))) |}]

let%expect_test _ =
  parse {|* BAD Empty command line|};
  [%expect {| (Untagged (State (BAD () "Empty command line"))) |}]

let%expect_test _ =
  parse {|* BAD Disk crash, attempting salvage to a new disk!|};
  [%expect {| (Untagged (State (BAD () "Disk crash, attempting salvage to a new disk!"))) |}]

let%expect_test _ =
  parse {|* OK Salvage successful, no data lost|};
  [%expect {| (Untagged (State (OK () "Salvage successful, no data lost"))) |}]

let%expect_test _ =
  parse {|A443 OK Expunge completed|};
  [%expect {| (Tagged A443 (OK () "Expunge completed")) |}]

let%expect_test _ =
  parse {|* PREAUTH IMAP4rev1 server logged in as Smith|};
  [%expect {| (Untagged (PREAUTH () "IMAP4rev1 server logged in as Smith")) |}]

let%expect_test _ =
  parse {|* BYE Autologout; idle for too long|};
  [%expect {| (Untagged (BYE () "Autologout; idle for too long")) |}]

let%expect_test _ =
  parse {|* CAPABILITY IMAP4rev1 STARTTLS AUTH=GSSAPI XPIG-LATIN|};
  [%expect {|
    (Untagged
     (CAPABILITY
      ((OTHER IMAP4rev1) (OTHER STARTTLS) (OTHER AUTH=GSSAPI) (OTHER XPIG-LATIN)))) |}]

let%expect_test _ =
  parse {|* LIST (\Noselect) "/" ~/Mail/foo|};
  [%expect {| (Untagged (LIST (Noselect) (/) ~/Mail/foo)) |}]

let%expect_test _ =
  parse {|* LSUB () "." #news.comp.mail.misc|};
  [%expect {| (Untagged (LSUB () (.) #news.comp.mail.misc)) |}]

let%expect_test _ =
  parse {|* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)|};
  [%expect {| (Untagged (STATUS blurdybloop ((MESSAGES 231) (UIDNEXT 44292)))) |}]

let%expect_test _ =
  parse {|* SEARCH 2 3 6|};
  [%expect {| (Untagged (SEARCH (2 3 6) ())) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Deleted Seen Draft))) |}]

let%expect_test _ =
  parse {|* 23 EXISTS|};
  [%expect {| (Untagged (EXISTS 23)) |}]

let%expect_test _ =
  parse {|* 5 RECENT|};
  [%expect {| (Untagged (RECENT 5)) |}]

let%expect_test _ =
  parse {|* 44 EXPUNGE|};
  [%expect {| (Untagged (EXPUNGE 44)) |}]

let%expect_test _ =
  parse {|* 23 FETCH (FLAGS (\Seen) RFC822.SIZE 44827)|};
  [%expect {| (Untagged (FETCH 23 ((FLAGS (Seen)) (RFC822_SIZE 44827)))) |}]

let%expect_test _ =
  parse {|A001 OK LOGIN completed|};
  [%expect {| (Tagged A001 (OK () "LOGIN completed")) |}]

let%expect_test _ =
  parse {|A044 BAD No such command as "BLURDYBLOOP"|};
  [%expect {| (Tagged A044 (BAD () "No such command as \"BLURDYBLOOP\"")) |}]

let%expect_test _ =
  parse {|* OK IMAP4rev1 Service Ready|};
  [%expect {| (Untagged (State (OK () "IMAP4rev1 Service Ready"))) |}]

let%expect_test _ =
  parse {|a001 OK LOGIN completed|};
  [%expect {| (Tagged a001 (OK () "LOGIN completed")) |}]

let%expect_test _ =
  parse {|* 18 EXISTS|};
  [%expect {| (Untagged (EXISTS 18)) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Deleted Seen Draft))) |}]

let%expect_test _ =
  parse {|* 2 RECENT|};
  [%expect {| (Untagged (RECENT 2)) |}]

let%expect_test _ =
  parse {|* OK [UNSEEN 17] Message 17 is the first unseen message|};
  [%expect {|
    (Untagged
     (State (OK ((UNSEEN 17)) "Message 17 is the first unseen message"))) |}]

let%expect_test _ =
  parse {|* OK [UIDVALIDITY 3857529045] UIDs valid|};
  [%expect {| (Untagged (State (OK ((UIDVALIDITY -437438251)) "UIDs valid"))) |}]

let%expect_test _ =
  parse {|a002 OK [READ-WRITE] SELECT completed|};
  [%expect {| (Tagged a002 (OK (READ_WRITE) "SELECT completed")) |}]

let%expect_test _ =
  parse {|* 12 FETCH (FLAGS (\Seen) INTERNALDATE "17-Jul-1996 02:44:25 -0700"|};
  [%expect {|
    Parsing error:
    * 12 FETCH (FLAGS (\Seen) INTERNALDATE "17-Jul-1996 02:44:25 -0700"
                                          ^ |}]

let%expect_test _ =
  parse {|a003 OK FETCH completed|};
  [%expect {| (Tagged a003 (OK () "FETCH completed")) |}]

let%expect_test _ =
  parse {|* 172 EXISTS|};
  [%expect {| (Untagged (EXISTS 172)) |}]

let%expect_test _ =
  parse {|* 1 RECENT|};
  [%expect {| (Untagged (RECENT 1)) |}]

let%expect_test _ =
  parse {|* OK [UNSEEN 12] Message 12 is first unseen|};
  [%expect {| (Untagged (State (OK ((UNSEEN 12)) "Message 12 is first unseen"))) |}]

let%expect_test _ =
  parse {|* OK [UIDVALIDITY 3857529045] UIDs valid|};
  [%expect {| (Untagged (State (OK ((UIDVALIDITY -437438251)) "UIDs valid"))) |}]

let%expect_test _ =
  parse {|* OK [UIDNEXT 4392] Predicted next UID|};
  [%expect {| (Untagged (State (OK ((UIDNEXT 4392)) "Predicted next UID"))) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Deleted Seen Draft))) |}]

let%expect_test _ =
  parse {|* OK [PERMANENTFLAGS (\Deleted \Seen \*)] Limited|};
  [%expect {|
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Deleted \\Seen \\*)"))) Limited))) |}]

let%expect_test _ =
  parse {|* OK [HIGHESTMODSEQ 715194045007]|};
  [%expect {| (Untagged (State (OK ((HIGHESTMODSEQ 715194045007)) ""))) |}]

let%expect_test _ =
  parse {|A142 OK [READ-WRITE] SELECT completed|};
  [%expect {| (Tagged A142 (OK (READ_WRITE) "SELECT completed")) |}]

let%expect_test _ =
  parse {|* 172 EXISTS|};
  [%expect {| (Untagged (EXISTS 172)) |}]

let%expect_test _ =
  parse {|* 1 RECENT|};
  [%expect {| (Untagged (RECENT 1)) |}]

let%expect_test _ =
  parse {|* OK [UNSEEN 12] Message 12 is first unseen|};
  [%expect {| (Untagged (State (OK ((UNSEEN 12)) "Message 12 is first unseen"))) |}]

let%expect_test _ =
  parse {|* OK [UIDVALIDITY 3857529045] UIDs valid|};
  [%expect {| (Untagged (State (OK ((UIDVALIDITY -437438251)) "UIDs valid"))) |}]

let%expect_test _ =
  parse {|* OK [UIDNEXT 4392] Predicted next UID|};
  [%expect {| (Untagged (State (OK ((UIDNEXT 4392)) "Predicted next UID"))) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Deleted Seen Draft))) |}]

let%expect_test _ =
  parse {|* OK [PERMANENTFLAGS (\Deleted \Seen \*)] Limited|};
  [%expect {|
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Deleted \\Seen \\*)"))) Limited))) |}]

let%expect_test _ =
  parse {|* OK [NOMODSEQ] Sorry, this mailbox format doesn't support|};
  [%expect {|
    (Untagged
     (State (OK (NOMODSEQ) "Sorry, this mailbox format doesn't support"))) |}]

let%expect_test _ =
  parse {|A142 OK [READ-WRITE] SELECT completed|};
  [%expect {| (Tagged A142 (OK (READ_WRITE) "SELECT completed")) |}]

let%expect_test _ =
  parse {|* 1 FETCH (UID 4 MODSEQ (12121231000))|};
  [%expect {| (Untagged (FETCH 1 ((UID 4) (MODSEQ 12121231000)))) |}]

let%expect_test _ =
  parse {|* 2 FETCH (UID 6 MODSEQ (12121230852))|};
  [%expect {| (Untagged (FETCH 2 ((UID 6) (MODSEQ 12121230852)))) |}]

let%expect_test _ =
  parse {|* 4 FETCH (UID 8 MODSEQ (12121230956))|};
  [%expect {| (Untagged (FETCH 4 ((UID 8) (MODSEQ 12121230956)))) |}]

let%expect_test _ =
  parse {|a103 OK Conditional Store completed|};
  [%expect {| (Tagged a103 (OK () "Conditional Store completed")) |}]

let%expect_test _ =
  parse {|* 50 FETCH (MODSEQ (12111230047))|};
  [%expect {| (Untagged (FETCH 50 ((MODSEQ 12111230047)))) |}]

let%expect_test _ =
  parse {|a104 OK Store (conditional) completed|};
  [%expect {| (Tagged a104 (OK () "Store (conditional) completed")) |}]

let%expect_test _ =
  parse {|* OK [HIGHESTMODSEQ 12111230047]|};
  [%expect {| (Untagged (State (OK ((HIGHESTMODSEQ 12111230047)) ""))) |}]

let%expect_test _ =
  parse {|* 50 FETCH (MODSEQ (12111230048))|};
  [%expect {| (Untagged (FETCH 50 ((MODSEQ 12111230048)))) |}]

let%expect_test _ =
  parse {|c101 OK Store (conditional) completed|};
  [%expect {| (Tagged c101 (OK () "Store (conditional) completed")) |}]

let%expect_test _ =
  parse {|* 5 FETCH (MODSEQ (320162350))|};
  [%expect {| (Untagged (FETCH 5 ((MODSEQ 320162350)))) |}]

let%expect_test _ =
  parse {|d105 OK [MODIFIED 7,9] Conditional STORE failed|};
  [%expect {| (Tagged d105 (OK ((MODIFIED ((7 7) (9 9)))) "Conditional STORE failed")) |}]

let%expect_test _ =
  parse {|* 7 FETCH (MODSEQ (320162342) FLAGS (\Seen \Deleted))|};
  [%expect {| (Untagged (FETCH 7 ((MODSEQ 320162342) (FLAGS (Seen Deleted))))) |}]

let%expect_test _ =
  parse {|* 5 FETCH (MODSEQ (320162350))|};
  [%expect {| (Untagged (FETCH 5 ((MODSEQ 320162350)))) |}]

let%expect_test _ =
  parse {|* 9 FETCH (MODSEQ (320162349) FLAGS (\Answered))|};
  [%expect {| (Untagged (FETCH 9 ((MODSEQ 320162349) (FLAGS (Answered))))) |}]

let%expect_test _ =
  parse {|d105 OK [MODIFIED 7,9] Conditional STORE failed|};
  [%expect {| (Tagged d105 (OK ((MODIFIED ((7 7) (9 9)))) "Conditional STORE failed")) |}]

let%expect_test _ =
  parse {|a102 OK [MODIFIED 12] Conditional STORE failed|};
  [%expect {| (Tagged a102 (OK ((MODIFIED ((12 12)))) "Conditional STORE failed")) |}]

let%expect_test _ =
  parse {|* 100 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 100 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|* 102 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 102 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|* 150 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 150 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|a106 OK [MODIFIED 101] Conditional STORE failed|};
  [%expect {| (Tagged a106 (OK ((MODIFIED ((101 101)))) "Conditional STORE failed")) |}]

let%expect_test _ =
  parse {|* 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed))|};
  [%expect {| (Untagged (FETCH 101 ((MODSEQ 303011130956) (FLAGS ((Keyword $Processed)))))) |}]

let%expect_test _ =
  parse {|a107 OK|};
  [%expect {| (Tagged a107 (OK () "")) |}]

let%expect_test _ =
  parse {|* 101 FETCH (MODSEQ (303011130956) FLAGS (\Deleted \Answered))|};
  [%expect {| (Untagged (FETCH 101 ((MODSEQ 303011130956) (FLAGS (Deleted Answered))))) |}]

let%expect_test _ =
  parse {|b107 OK|};
  [%expect {| (Tagged b107 (OK () "")) |}]

let%expect_test _ =
  parse {|* 101 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 101 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|b108 OK Conditional Store completed|};
  [%expect {| (Tagged b108 (OK () "Conditional Store completed")) |}]

let%expect_test _ =
  parse {|* 100 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 100 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|* 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed))|};
  [%expect {| (Untagged (FETCH 101 ((MODSEQ 303011130956) (FLAGS ((Keyword $Processed)))))) |}]

let%expect_test _ =
  parse {|* 102 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 102 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|* 150 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 150 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|a106 OK [MODIFIED 101] Conditional STORE failed|};
  [%expect {| (Tagged a106 (OK ((MODIFIED ((101 101)))) "Conditional STORE failed")) |}]

let%expect_test _ =
  parse {|* 100 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 100 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|* 101 FETCH (MODSEQ (303011130956) FLAGS (\Deleted \Answered))|};
  [%expect {| (Untagged (FETCH 101 ((MODSEQ 303011130956) (FLAGS (Deleted Answered))))) |}]

let%expect_test _ =
  parse {|* 102 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 102 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|* 150 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 150 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|a106 OK [MODIFIED 101] Conditional STORE failed|};
  [%expect {| (Tagged a106 (OK ((MODIFIED ((101 101)))) "Conditional STORE failed")) |}]

let%expect_test _ =
  parse {|* 101 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 101 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|b108 OK Conditional Store completed|};
  [%expect {| (Tagged b108 (OK () "Conditional Store completed")) |}]

let%expect_test _ =
  parse {|* 100 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 100 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|* 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed \Deleted))|};
  [%expect {|
    (Untagged
     (FETCH 101 ((MODSEQ 303011130956) (FLAGS ((Keyword $Processed) Deleted))))) |}]

let%expect_test _ =
  parse {|* 102 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 102 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|* 150 FETCH (MODSEQ (303181230852))|};
  [%expect {| (Untagged (FETCH 150 ((MODSEQ 303181230852)))) |}]

let%expect_test _ =
  parse {|a106 OK Conditional STORE completed|};
  [%expect {| (Tagged a106 (OK () "Conditional STORE completed")) |}]

let%expect_test _ =
  parse {|* 1 FETCH (MODSEQ (320172342) FLAGS (\SEEN))|};
  [%expect {| (Untagged (FETCH 1 ((MODSEQ 320172342) (FLAGS (Seen))))) |}]

let%expect_test _ =
  parse {|* 3 FETCH (MODSEQ (320172342) FLAGS (\SEEN))|};
  [%expect {| (Untagged (FETCH 3 ((MODSEQ 320172342) (FLAGS (Seen))))) |}]

let%expect_test _ =
  parse {|B001 NO [MODIFIED 2] Some of the messages no longer exist.|};
  [%expect {|
    (Tagged B001
     (NO ((MODIFIED ((2 2)))) "Some of the messages no longer exist.")) |}]

let%expect_test _ =
  parse {|* 4 EXPUNGE|};
  [%expect {| (Untagged (EXPUNGE 4)) |}]

let%expect_test _ =
  parse {|* 4 EXPUNGE|};
  [%expect {| (Untagged (EXPUNGE 4)) |}]

let%expect_test _ =
  parse {|* 4 EXPUNGE|};
  [%expect {| (Untagged (EXPUNGE 4)) |}]

let%expect_test _ =
  parse {|* 4 EXPUNGE|};
  [%expect {| (Untagged (EXPUNGE 4)) |}]

let%expect_test _ =
  parse {|* 2 FETCH (MODSEQ (320172340) FLAGS (\Deleted \Answered))|};
  [%expect {| (Untagged (FETCH 2 ((MODSEQ 320172340) (FLAGS (Deleted Answered))))) |}]

let%expect_test _ =
  parse {|B002 OK NOOP Completed.|};
  [%expect {| (Tagged B002 (OK () "NOOP Completed.")) |}]

let%expect_test _ =
  parse {|* 2 FETCH (MODSEQ (320180050) FLAGS (\SEEN \Flagged))|};
  [%expect {| (Untagged (FETCH 2 ((MODSEQ 320180050) (FLAGS (Seen Flagged))))) |}]

let%expect_test _ =
  parse {|b003 OK Conditional Store completed|};
  [%expect {| (Tagged b003 (OK () "Conditional Store completed")) |}]

let%expect_test _ =
  parse {|* 1 FETCH (UID 4 MODSEQ (65402) FLAGS (\Seen))|};
  [%expect {| (Untagged (FETCH 1 ((UID 4) (MODSEQ 65402) (FLAGS (Seen))))) |}]

let%expect_test _ =
  parse {|* 2 FETCH (UID 6 MODSEQ (75403) FLAGS (\Deleted))|};
  [%expect {| (Untagged (FETCH 2 ((UID 6) (MODSEQ 75403) (FLAGS (Deleted))))) |}]

let%expect_test _ =
  parse {|* 4 FETCH (UID 8 MODSEQ (29738) FLAGS ($NoJunk $AutoJunk))|};
  [%expect {|
    (Untagged
     (FETCH 4
      ((UID 8) (MODSEQ 29738) (FLAGS ((Keyword $NoJunk) (Keyword $AutoJunk)))))) |}]

let%expect_test _ =
  parse {|s100 OK FETCH completed|};
  [%expect {| (Tagged s100 (OK () "FETCH completed")) |}]

let%expect_test _ =
  parse {|* 1 FETCH (MODSEQ (624140003))|};
  [%expect {| (Untagged (FETCH 1 ((MODSEQ 624140003)))) |}]

let%expect_test _ =
  parse {|* 2 FETCH (MODSEQ (624140007))|};
  [%expect {| (Untagged (FETCH 2 ((MODSEQ 624140007)))) |}]

let%expect_test _ =
  parse {|* 3 FETCH (MODSEQ (624140005))|};
  [%expect {| (Untagged (FETCH 3 ((MODSEQ 624140005)))) |}]

let%expect_test _ =
  parse {|a OK Fetch complete|};
  [%expect {| (Tagged a (OK () "Fetch complete")) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Deleted Seen Draft))) |}]

let%expect_test _ =
  parse {|* OK [PERMANENTFLAGS (\Answered \Deleted \Seen \*)] Limited|};
  [%expect {|
    (Untagged
     (State
      (OK ((OTHER PERMANENTFLAGS (" (\\Answered \\Deleted \\Seen \\*)")))
       Limited))) |}]

let%expect_test _ =
  parse {|* 7 FETCH (MODSEQ (2121231000))|};
  [%expect {| (Untagged (FETCH 7 ((MODSEQ 2121231000)))) |}]

let%expect_test _ =
  parse {|A160 OK Store completed|};
  [%expect {| (Tagged A160 (OK () "Store completed")) |}]

let%expect_test _ =
  parse {|* 7 FETCH (FLAGS (\Deleted \Answered) MODSEQ (12121231000))|};
  [%expect {| (Untagged (FETCH 7 ((FLAGS (Deleted Answered)) (MODSEQ 12121231000)))) |}]

let%expect_test _ =
  parse {|C180 OK Noop completed|};
  [%expect {| (Tagged C180 (OK () "Noop completed")) |}]

let%expect_test _ =
  parse {|* 7 FETCH (FLAGS (\Deleted \Answered) MODSEQ (12121231000))|};
  [%expect {| (Untagged (FETCH 7 ((FLAGS (Deleted Answered)) (MODSEQ 12121231000)))) |}]

let%expect_test _ =
  parse {|D210 OK Noop completed|};
  [%expect {| (Tagged D210 (OK () "Noop completed")) |}]

let%expect_test _ =
  parse {|* 7 FETCH (MODSEQ (12121231777))|};
  [%expect {| (Untagged (FETCH 7 ((MODSEQ 12121231777)))) |}]

let%expect_test _ =
  parse {|A240 OK Store completed|};
  [%expect {| (Tagged A240 (OK () "Store completed")) |}]

let%expect_test _ =
  parse {|* 7 FETCH (FLAGS (\Deleted \Answered \Seen) MODSEQ (12))|};
  [%expect {| (Untagged (FETCH 7 ((FLAGS (Deleted Answered Seen)) (MODSEQ 12)))) |}]

let%expect_test _ =
  parse {|C270 OK Noop completed|};
  [%expect {| (Tagged C270 (OK () "Noop completed")) |}]

let%expect_test _ =
  parse {|D300 OK Noop completed|};
  [%expect {| (Tagged D300 (OK () "Noop completed")) |}]

let%expect_test _ =
  parse {|* 7 FETCH (MODSEQ (12121245160))|};
  [%expect {| (Untagged (FETCH 7 ((MODSEQ 12121245160)))) |}]

let%expect_test _ =
  parse {|A330 OK Store completed|};
  [%expect {| (Tagged A330 (OK () "Store completed")) |}]

let%expect_test _ =
  parse {|* 7 FETCH (FLAGS (\Deleted) MODSEQ (12121245160))|};
  [%expect {| (Untagged (FETCH 7 ((FLAGS (Deleted)) (MODSEQ 12121245160)))) |}]

let%expect_test _ =
  parse {|C360 OK Noop completed|};
  [%expect {| (Tagged C360 (OK () "Noop completed")) |}]

let%expect_test _ =
  parse {|* 7 FETCH (FLAGS (\Deleted) MODSEQ (12121245160))|};
  [%expect {| (Untagged (FETCH 7 ((FLAGS (Deleted)) (MODSEQ 12121245160)))) |}]

let%expect_test _ =
  parse {|D390 OK Noop completed|};
  [%expect {| (Tagged D390 (OK () "Noop completed")) |}]

let%expect_test _ =
  parse {|* SEARCH 2 5 6 7 11 12 18 19 20 23 (MODSEQ 917162500)|};
  [%expect {| (Untagged (SEARCH (2 5 6 7 11 12 18 19 20 23) (917162500))) |}]

let%expect_test _ =
  parse {|a OK Search complete|};
  [%expect {| (Tagged a (OK () "Search complete")) |}]

let%expect_test _ =
  parse {|* SEARCH|};
  [%expect {| (Untagged (SEARCH () ())) |}]

let%expect_test _ =
  parse {|t OK Search complete, nothing found|};
  [%expect {| (Tagged t (OK () "Search complete, nothing found")) |}]

let%expect_test _ =
  parse {|* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)|};
  [%expect {| (Untagged (STATUS blurdybloop ((MESSAGES 231) (UIDNEXT 44292)))) |}]

let%expect_test _ =
  parse {|A042 OK STATUS completed|};
  [%expect {| (Tagged A042 (OK () "STATUS completed")) |}]

let%expect_test _ =
  parse {|* 172 EXISTS|};
  [%expect {| (Untagged (EXISTS 172)) |}]

let%expect_test _ =
  parse {|* 1 RECENT|};
  [%expect {| (Untagged (RECENT 1)) |}]

let%expect_test _ =
  parse {|* OK [UNSEEN 12] Message 12 is first unseen|};
  [%expect {| (Untagged (State (OK ((UNSEEN 12)) "Message 12 is first unseen"))) |}]

let%expect_test _ =
  parse {|* OK [UIDVALIDITY 3857529045] UIDs valid|};
  [%expect {| (Untagged (State (OK ((UIDVALIDITY -437438251)) "UIDs valid"))) |}]

let%expect_test _ =
  parse {|* OK [UIDNEXT 4392] Predicted next UID|};
  [%expect {| (Untagged (State (OK ((UIDNEXT 4392)) "Predicted next UID"))) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Deleted Seen Draft))) |}]

let%expect_test _ =
  parse {|* OK [PERMANENTFLAGS (\Deleted \Seen \*)] Limited|};
  [%expect {|
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Deleted \\Seen \\*)"))) Limited))) |}]

let%expect_test _ =
  parse {|* OK [HIGHESTMODSEQ 715194045007]|};
  [%expect {| (Untagged (State (OK ((HIGHESTMODSEQ 715194045007)) ""))) |}]

let%expect_test _ =
  parse {|A142 OK [READ-WRITE] SELECT completed, CONDSTORE is now enabled|};
  [%expect {| (Tagged A142 (OK (READ_WRITE) "SELECT completed, CONDSTORE is now enabled")) |}]

let%expect_test _ =
  parse {|* ESEARCH (TAG "a") ALL 1:3,5 MODSEQ 1236|};
  [%expect {|
    Parsing error:
    * ESEARCH (TAG "a") ALL 1:3,5 MODSEQ 1236
             ^ |}]

let%expect_test _ =
  parse {|a OK Extended SEARCH completed|};
  [%expect {| (Tagged a (OK () "Extended SEARCH completed")) |}]

let%expect_test _ =
  parse {|* ESEARCH (TAG "a") ALL 5,3,2,1 MODSEQ 1236|};
  [%expect {|
    Parsing error:
    * ESEARCH (TAG "a") ALL 5,3,2,1 MODSEQ 1236
             ^ |}]

let%expect_test _ =
  parse {|a OK Extended SORT completed|};
  [%expect {| (Tagged a (OK () "Extended SORT completed")) |}]

let%expect_test _ =
  parse {|* 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed \Deleted))|};
  [%expect {|
    (Untagged
     (FETCH 101 ((MODSEQ 303011130956) (FLAGS ((Keyword $Processed) Deleted))))) |}]

let%expect_test _ =
  parse {|* 464 EXISTS|};
  [%expect {| (Untagged (EXISTS 464)) |}]

let%expect_test _ =
  parse {|* 3 RECENT|};
  [%expect {| (Untagged (RECENT 3)) |}]

let%expect_test _ =
  parse {|* OK [UIDVALIDITY 3857529045] UIDVALIDITY|};
  [%expect {| (Untagged (State (OK ((UIDVALIDITY -437438251)) UIDVALIDITY))) |}]

let%expect_test _ =
  parse {|* OK [UIDNEXT 550] Predicted next UID|};
  [%expect {| (Untagged (State (OK ((UIDNEXT 550)) "Predicted next UID"))) |}]

let%expect_test _ =
  parse {|* OK [HIGHESTMODSEQ 90060128194045007] Highest mailbox|};
  [%expect {| (Untagged (State (OK ((HIGHESTMODSEQ 90060128194045007)) "Highest mailbox"))) |}]

let%expect_test _ =
  parse {|* OK [UNSEEN 12] Message 12 is first unseen|};
  [%expect {| (Untagged (State (OK ((UNSEEN 12)) "Message 12 is first unseen"))) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Draft \Deleted \Seen)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Draft Deleted Seen))) |}]

let%expect_test _ =
  parse {|* OK [PERMANENTFLAGS (\Answered \Flagged \Draft)]|};
  [%expect {|
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Answered \\Flagged \\Draft)"))) ""))) |}]

let%expect_test _ =
  parse {|A02 OK [READ-WRITE] Sorry, UIDVALIDITY mismatch|};
  [%expect {| (Tagged A02 (OK (READ_WRITE) "Sorry, UIDVALIDITY mismatch")) |}]

let%expect_test _ =
  parse {|* OK [CLOSED]|};
  [%expect {| (Untagged (State (OK (CLOSED) ""))) |}]

let%expect_test _ =
  parse {|* 100 EXISTS|};
  [%expect {| (Untagged (EXISTS 100)) |}]

let%expect_test _ =
  parse {|* 11 RECENT|};
  [%expect {| (Untagged (RECENT 11)) |}]

let%expect_test _ =
  parse {|* OK [UIDVALIDITY 67890007] UIDVALIDITY|};
  [%expect {| (Untagged (State (OK ((UIDVALIDITY 67890007)) UIDVALIDITY))) |}]

let%expect_test _ =
  parse {|* OK [UIDNEXT 600] Predicted next UID|};
  [%expect {| (Untagged (State (OK ((UIDNEXT 600)) "Predicted next UID"))) |}]

let%expect_test _ =
  parse {|* OK [HIGHESTMODSEQ 90060115205545359] Highest|};
  [%expect {| (Untagged (State (OK ((HIGHESTMODSEQ 90060115205545359)) Highest))) |}]

let%expect_test _ =
  parse {|* OK [UNSEEN 7] There are some unseen|};
  [%expect {| (Untagged (State (OK ((UNSEEN 7)) "There are some unseen"))) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Draft \Deleted \Seen)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Draft Deleted Seen))) |}]

let%expect_test _ =
  parse {|* OK [PERMANENTFLAGS (\Answered \Flagged \Draft)]|};
  [%expect {|
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Answered \\Flagged \\Draft)"))) ""))) |}]

let%expect_test _ =
  parse {|* VANISHED (EARLIER) 41,43:116,118,120:211,214:540|};
  [%expect {|
    (Untagged
     (VANISHED_EARLIER ((41 41) (43 116) (118 118) (120 211) (214 540)))) |}]

let%expect_test _ =
  parse {|* 49 FETCH (UID 117 FLAGS (\Seen \Answered) MODSEQ (12111230047))|};
  [%expect {|
    (Untagged
     (FETCH 49 ((UID 117) (FLAGS (Seen Answered)) (MODSEQ 12111230047)))) |}]

let%expect_test _ =
  parse {|* 50 FETCH (UID 119 FLAGS (\Draft $MDNSent) MODSEQ (12111230047))|};
  [%expect {|
    (Untagged
     (FETCH 50
      ((UID 119) (FLAGS (Draft (Keyword $MDNSent))) (MODSEQ 12111230047)))) |}]

let%expect_test _ =
  parse {|* 51 FETCH (UID 541 FLAGS (\Seen $Forwarded) MODSEQ (12111230047))|};
  [%expect {|
    (Untagged
     (FETCH 51
      ((UID 541) (FLAGS (Seen (Keyword $Forwarded))) (MODSEQ 12111230047)))) |}]

let%expect_test _ =
  parse {|A03 OK [READ-WRITE] mailbox selected|};
  [%expect {| (Tagged A03 (OK (READ_WRITE) "mailbox selected")) |}]

let%expect_test _ =
  parse {|* 10003 EXISTS|};
  [%expect {| (Untagged (EXISTS 10003)) |}]

let%expect_test _ =
  parse {|* 4 RECENT|};
  [%expect {| (Untagged (RECENT 4)) |}]

let%expect_test _ =
  parse {|* OK [UIDVALIDITY 67890007] UIDVALIDITY|};
  [%expect {| (Untagged (State (OK ((UIDVALIDITY 67890007)) UIDVALIDITY))) |}]

let%expect_test _ =
  parse {|* OK [UIDNEXT 30013] Predicted next UID|};
  [%expect {| (Untagged (State (OK ((UIDNEXT 30013)) "Predicted next UID"))) |}]

let%expect_test _ =
  parse {|* OK [HIGHESTMODSEQ 90060115205545359] Highest mailbox|};
  [%expect {| (Untagged (State (OK ((HIGHESTMODSEQ 90060115205545359)) "Highest mailbox"))) |}]

let%expect_test _ =
  parse {|* OK [UNSEEN 7] There are some unseen messages in the mailbox|};
  [%expect {|
    (Untagged
     (State (OK ((UNSEEN 7)) "There are some unseen messages in the mailbox"))) |}]

let%expect_test _ =
  parse {|* FLAGS (\Answered \Flagged \Draft \Deleted \Seen)|};
  [%expect {| (Untagged (FLAGS (Answered Flagged Draft Deleted Seen))) |}]

let%expect_test _ =
  parse {|* OK [PERMANENTFLAGS (\Answered \Flagged \Draft)]|};
  [%expect {|
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Answered \\Flagged \\Draft)"))) ""))) |}]

let%expect_test _ =
  parse {|* VANISHED (EARLIER) 1:2,4:5,7:8,10:11,13:14,89|};
  [%expect {| (Untagged (VANISHED_EARLIER ((1 2) (4 5) (7 8) (10 11) (13 14) (89 89)))) |}]

let%expect_test _ =
  parse {|* 1 FETCH (UID 3 FLAGS (\Seen \Answered $Important) MODSEQ (90060115194045027))|};
  [%expect {|
    (Untagged
     (FETCH 1
      ((UID 3) (FLAGS (Seen Answered (Keyword $Important)))
       (MODSEQ 90060115194045027)))) |}]
