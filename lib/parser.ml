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
  (* | "HIGHESTMODSEQ" ->
   *     sp *> mod_sequence_value >>| (fun n -> HIGHESTMODSEQ n) *)
  | "NOMODSEQ" ->
      k NOMODSEQ
  (* | "MODIFIED" ->
   *     sp *> set >>| (fun l -> MODIFIED l)
   * | "APPENDUID" ->
   *     sp *> pair sp nz_number append_uid >>| (fun (n, uid) -> APPENDUID (n, uid))
   * | "COPYUID" ->
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

let mod_sequence_value buf =
  Scanf.sscanf (take_while1 is_digit buf) "%Lu" (fun n -> n)

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

let msg_att buf k =
  let open Fetch.MessageAttribute in
  match atom buf with
  (* | "FLAGS" ->
   *     sp *> psep_by sp flag_fetch >>| (fun l -> FLAGS l) *)
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
  (* "RFC822.HEADER", sp *> nstring >>| (fun s -> RFC822_HEADER s); *)
    (* "RFC822.TEXT", sp *> nstring >>| (fun s -> RFC822_TEXT s); *)
  (* | "RFC822.SIZE" ->
   *     sp *> number >>| (fun n -> RFC822_SIZE (Int32.to_int n)) *)
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
      (* | "VANISHED (EARLIER)" ->
       *     sp *> known_ids >>| (fun l -> VANISHED_EARLIER l)
       * | "VANISHED" ->
       *     sp *> known_ids >>| (fun l -> VANISHED l) *)
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
      (* | "FLAGS" ->
       *     sp *> psep_by sp flag >>| (fun l -> FLAGS l) *)
      | "LIST" ->
          char ' ' buf;
          mailbox_list buf (fun xs c m -> k (LIST (xs, c, m)))
      | "LSUB" ->
          char ' ' buf;
          mailbox_list buf (fun xs c m -> k (LSUB (xs, c, m)))
      | "SEARCH" ->
          let rec loop acc buf =
            if curr buf = ' ' then
              (next buf; loop (nz_number buf :: acc) buf)
            else
              List.rev acc
          in
          let nums = loop [] buf in
          let modseq =
            if curr buf = ' ' then
              (next buf; Some (search_sort_mod_seq buf))
            else
              None
          in
          k (SEARCH (nums, modseq))
      (* | "STATUS" ->
       *     sp *> pair sp mailbox (psep_by sp status_att) >>| (fun (m, l) -> STATUS (m, l)) *)
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
  let tests =
    [
      {|+ YGgGCSqGSIb3EgECAgIAb1kwV6ADAgEFoQMCAQ+iSzBJoAMC|};
      {|+ YDMGCSqGSIb3EgECAgIBAAD/////6jcyG4GE3KkTzBeBiVHe|};
      {|+|};
      {|+ Ready for literal data|};
      {|+ Ready for additional command text|};
      {|abcd OK CAPABILITY completed|};
      {|efgh OK STARTLS completed|};
      {|ijkl OK CAPABILITY completed|};
      {|a002 OK NOOP completed|};
      {|a047 OK NOOP completed|};
      {|A023 OK LOGOUT completed|};
      {|a001 OK CAPABILITY completed|};
      {|a002 OK Begin TLS negotiation now|};
      {|a003 OK CAPABILITY completed|};
      {|a004 OK LOGIN completed|};
      {|A001 OK GSSAPI authentication successful|};
      {|a001 OK LOGIN completed|};
      {|A142 OK [READ-WRITE] SELECT completed|};
      {|A932 OK [READ-ONLY] EXAMINE completed|};
      {|A003 OK CREATE completed|};
      {|A004 OK CREATE completed|};
      {|A682 OK LIST completed|};
      {|A683 OK DELETE completed|};
      {|A684 NO Name "foo" has inferior hierarchical names|};
      {|A685 OK DELETE Completed|};
      {|A686 OK LIST completed|};
      {|A687 OK DELETE Completed|};
      {|A82 OK LIST completed|};
      {|A83 OK DELETE completed|};
      {|A84 OK DELETE Completed|};
      {|* CAPABILITY IMAP4rev1 STARTTLS AUTH=GSSAPI|};
      {|* CAPABILITY IMAP4rev1 AUTH=GSSAPI AUTH=PLAIN|};
      {|* 22 EXPUNGE|};
      {|* 23 EXISTS|};
      {|* 3 RECENT|};
      {|* 14 FETCH (FLAGS (\Seen \Deleted))|};
      {|* BYE IMAP4rev1 Server logging out|};
      {|* CAPABILITY IMAP4rev1 STARTTLS LOGINDISABLED|};
      {|* CAPABILITY IMAP4rev1 AUTH=PLAIN|};
      {|* OK IMAP4rev1 Server|};
      {|* 172 EXISTS|};
      {|* 1 RECENT|};
      {|* OK [UNSEEN 12] Message 12 is first unseen|};
      {|* OK [UIDVALIDITY 3857529045] UIDs valid|};
      {|* OK [UIDNEXT 4392] Predicted next UID|};
      {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
      {|* OK [PERMANENTFLAGS (\Deleted \Seen \*)] Limited|};
      {|* 17 EXISTS|};
      {|* 2 RECENT|};
      {|* OK [UNSEEN 8] Message 8 is first unseen|};
      {|* OK [UIDNEXT 4392] Predicted next UID|};
      {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
      {|* OK [PERMANENTFLAGS ()] No permanent flags permitted|};
      {|* LIST () "/" blurdybloop|};
      {|* LIST (\Noselect) "/" foo|};
      {|* LIST () "/" foo/bar|};
      {|* LIST (\Noselect) "/" foo|};
      {|* LIST () "." blurdybloop|};
      {|* LIST () "." foo|};
      {|* LIST () "." foo.bar|};
      {|* LIST () "." foo.bar|};
      {|A85 OK LIST completed|};
      {|* LIST (\Noselect) "." foo|};
      {|A86 OK LIST completed|};
      {|* LIST () "/" blurdybloop|};
      {|* LIST (\Noselect) "/" foo|};
      {|* LIST () "/" foo/bar|};
      {|A682 OK LIST completed|};
      {|A683 OK RENAME completed|};
      {|A684 OK RENAME Completed|};
      {|* LIST () "/" sarasoop|};
      {|* LIST (\Noselect) "/" zowie|};
      {|* LIST () "/" zowie/bar|};
      {|A685 OK LIST completed|};
      {|* LIST () "." INBOX|};
      {|* LIST () "." INBOX.bar|};
      {|Z432 OK LIST completed|};
      {|Z433 OK RENAME completed|};
      {|* LIST () "." INBOX|};
      {|* LIST () "." INBOX.bar|};
      {|* LIST () "." old-mail|};
      {|Z434 OK LIST completed|};
      {|A002 OK SUBSCRIBE completed|};
      {|A002 OK UNSUBSCRIBE completed|};
      {|* LIST (\Noselect) "/" ""|};
      {|A101 OK LIST Completed|};
      {|* LIST (\Noselect) "." #news.|};
      {|A102 OK LIST Completed|};
      {|* LIST (\Noselect) "/" /|};
      {|A103 OK LIST Completed|};
      {|* LIST (\Noselect) "/" ~/Mail/foo|};
      {|* LIST () "/" ~/Mail/meetings|};
      {|A202 OK LIST completed|};
      {|* LSUB () "." #news.comp.mail.mime|};
      {|* LSUB () "." #news.comp.mail.misc|};
      {|A002 OK LSUB completed|};
      {|* LSUB (\NoSelect) "." #news.comp.mail|};
      {|A003 OK LSUB completed|};
      {|* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)|};
      {|A042 OK STATUS completed|};
      {|A003 OK APPEND completed|};
      {|FXXZ OK CHECK Completed|};
      {|A341 OK CLOSE completed|};
      {|* 3 EXPUNGE|};
      {|* 3 EXPUNGE|};
      {|* 5 EXPUNGE|};
      {|* 8 EXPUNGE|};
      {|A202 OK EXPUNGE completed|};
      {|* SEARCH 2 84 882|};
      {|A282 OK SEARCH completed|};
      {|* SEARCH|};
      {|A283 OK SEARCH completed|};
      {|* SEARCH 43|};
      {|A284 OK SEARCH completed|};
      {|A654 OK FETCH completed|};
      {|* 2 FETCH (FLAGS (\Deleted \Seen))|};
      {|* 3 FETCH (FLAGS (\Deleted))|};
      {|* 4 FETCH (FLAGS (\Deleted \Flagged \Seen))|};
      {|A003 OK STORE completed|};
      {|A003 OK COPY completed|};
      {|* 23 FETCH (FLAGS (\Seen) UID 4827313)|};
      {|* 24 FETCH (FLAGS (\Seen) UID 4827943)|};
      {|* 25 FETCH (FLAGS (\Seen) UID 4828442)|};
      {|A999 OK UID FETCH completed|};
      {|* CAPABILITY IMAP4rev1 XPIG-LATIN|};
      {|a441 OK CAPABILITY completed|};
      {|A442 OK XPIG-LATIN ompleted-cay|};
      {|* OK IMAP4rev1 server ready|};
      {|* OK [ALERT] System shutdown in 10 minutes|};
      {|A001 OK LOGIN Completed|};
      {|* NO Disk is 98% full, please delete unnecessary data|};
      {|A222 OK COPY completed|};
      {|* NO Disk is 98% full, please delete unnecessary data|};
      {|* NO Disk is 99% full, please delete unnecessary data|};
      {|A223 NO COPY failed: disk is full|};
      {|* BAD Command line too long|};
      {|* BAD Empty command line|};
      {|* BAD Disk crash, attempting salvage to a new disk!|};
      {|* OK Salvage successful, no data lost|};
      {|A443 OK Expunge completed|};
      {|* PREAUTH IMAP4rev1 server logged in as Smith|};
      {|* BYE Autologout; idle for too long|};
      {|* CAPABILITY IMAP4rev1 STARTTLS AUTH=GSSAPI XPIG-LATIN|};
      {|* LIST (\Noselect) "/" ~/Mail/foo|};
      {|* LSUB () "." #news.comp.mail.misc|};
      {|* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)|};
      {|* SEARCH 2 3 6|};
      {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
      {|* 23 EXISTS|};
      {|* 5 RECENT|};
      {|* 44 EXPUNGE|};
      {|* 23 FETCH (FLAGS (\Seen) RFC822.SIZE 44827)|};
      {|A001 OK LOGIN completed|};
      {|A044 BAD No such command as "BLURDYBLOOP"|};
      {|* OK IMAP4rev1 Service Ready|};
      {|a001 OK LOGIN completed|};
      {|* 18 EXISTS|};
      {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
      {|* 2 RECENT|};
      {|* OK [UNSEEN 17] Message 17 is the first unseen message|};
      {|* OK [UIDVALIDITY 3857529045] UIDs valid|};
      {|a002 OK [READ-WRITE] SELECT completed|};
      {|* 12 FETCH (FLAGS (\Seen) INTERNALDATE "17-Jul-1996 02:44:25 -0700"|};
      {|a003 OK FETCH completed|};
      {|* 172 EXISTS|};
      {|* 1 RECENT|};
      {|* OK [UNSEEN 12] Message 12 is first unseen|};
      {|* OK [UIDVALIDITY 3857529045] UIDs valid|};
      {|* OK [UIDNEXT 4392] Predicted next UID|};
      {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
      {|* OK [PERMANENTFLAGS (\Deleted \Seen \*)] Limited|};
      {|* OK [HIGHESTMODSEQ 715194045007]|};
      {|A142 OK [READ-WRITE] SELECT completed|};
      {|* 172 EXISTS|};
      {|* 1 RECENT|};
      {|* OK [UNSEEN 12] Message 12 is first unseen|};
      {|* OK [UIDVALIDITY 3857529045] UIDs valid|};
      {|* OK [UIDNEXT 4392] Predicted next UID|};
      {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
      {|* OK [PERMANENTFLAGS (\Deleted \Seen \*)] Limited|};
      {|* OK [NOMODSEQ] Sorry, this mailbox format doesn't support|};
      {|A142 OK [READ-WRITE] SELECT completed|};
      {|* 1 FETCH (UID 4 MODSEQ (12121231000))|};
      {|* 2 FETCH (UID 6 MODSEQ (12121230852))|};
      {|* 4 FETCH (UID 8 MODSEQ (12121230956))|};
      {|a103 OK Conditional Store completed|};
      {|* 50 FETCH (MODSEQ (12111230047))|};
      {|a104 OK Store (conditional) completed|};
      {|* OK [HIGHESTMODSEQ 12111230047]|};
      {|* 50 FETCH (MODSEQ (12111230048))|};
      {|c101 OK Store (conditional) completed|};
      {|* 5 FETCH (MODSEQ (320162350))|};
      {|d105 OK [MODIFIED 7,9] Conditional STORE failed|};
      {|* 7 FETCH (MODSEQ (320162342) FLAGS (\Seen \Deleted))|};
      {|* 5 FETCH (MODSEQ (320162350))|};
      {|* 9 FETCH (MODSEQ (320162349) FLAGS (\Answered))|};
      {|d105 OK [MODIFIED 7,9] Conditional STORE failed|};
      {|a102 OK [MODIFIED 12] Conditional STORE failed|};
      {|* 100 FETCH (MODSEQ (303181230852))|};
      {|* 102 FETCH (MODSEQ (303181230852))|};
      {|* 150 FETCH (MODSEQ (303181230852))|};
      {|a106 OK [MODIFIED 101] Conditional STORE failed|};
      {|* 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed))|};
      {|a107 OK|};
      {|* 101 FETCH (MODSEQ (303011130956) FLAGS (\Deleted \Answered))|};
      {|b107 OK|};
      {|* 101 FETCH (MODSEQ (303181230852))|};
      {|b108 OK Conditional Store completed|};
      {|* 100 FETCH (MODSEQ (303181230852))|};
      {|* 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed))|};
      {|* 102 FETCH (MODSEQ (303181230852))|};
      {|* 150 FETCH (MODSEQ (303181230852))|};
      {|a106 OK [MODIFIED 101] Conditional STORE failed|};
      {|* 100 FETCH (MODSEQ (303181230852))|};
      {|* 101 FETCH (MODSEQ (303011130956) FLAGS (\Deleted \Answered))|};
      {|* 102 FETCH (MODSEQ (303181230852))|};
      {|* 150 FETCH (MODSEQ (303181230852))|};
      {|a106 OK [MODIFIED 101] Conditional STORE failed|};
      {|* 101 FETCH (MODSEQ (303181230852))|};
      {|b108 OK Conditional Store completed|};
      {|* 100 FETCH (MODSEQ (303181230852))|};
      {|* 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed \Deleted|};
      {|* 102 FETCH (MODSEQ (303181230852))|};
      {|* 150 FETCH (MODSEQ (303181230852))|};
      {|a106 OK Conditional STORE completed|};
      {|* 1 FETCH (MODSEQ (320172342) FLAGS (\SEEN))|};
      {|* 3 FETCH (MODSEQ (320172342) FLAGS (\SEEN))|};
      {|B001 NO [MODIFIED 2] Some of the messages no longer exist.|};
      {|* 4 EXPUNGE|};
      {|* 4 EXPUNGE|};
      {|* 4 EXPUNGE|};
      {|* 4 EXPUNGE|};
      {|* 2 FETCH (MODSEQ (320172340) FLAGS (\Deleted \Answered))|};
      {|B002 OK NOOP Completed.|};
      {|* 2 FETCH (MODSEQ (320180050) FLAGS (\SEEN \Flagged))|};
      {|b003 OK Conditional Store completed|};
      {|* 1 FETCH (UID 4 MODSEQ (65402) FLAGS (\Seen))|};
      {|* 2 FETCH (UID 6 MODSEQ (75403) FLAGS (\Deleted))|};
      {|* 4 FETCH (UID 8 MODSEQ (29738) FLAGS ($NoJunk $AutoJunk|};
      {|s100 OK FETCH completed|};
      {|* 1 FETCH (MODSEQ (624140003))|};
      {|* 2 FETCH (MODSEQ (624140007))|};
      {|* 3 FETCH (MODSEQ (624140005))|};
      {|a OK Fetch complete|};
      {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
      {|* OK [PERMANENTFLAGS (\Answered \Deleted \Seen \*)] Limited|};
      {|* 7 FETCH (MODSEQ (2121231000))|};
      {|A160 OK Store completed|};
      {|* 7 FETCH (FLAGS (\Deleted \Answered) MODSEQ (12121231000))|};
      {|C180 OK Noop completed|};
      {|* 7 FETCH (FLAGS (\Deleted \Answered) MODSEQ (12121231000))|};
      {|D210 OK Noop completed|};
      {|* 7 FETCH (MODSEQ (12121231777))|};
      {|A240 OK Store completed|};
      {|* 7 FETCH (FLAGS (\Deleted \Answered \Seen) MODSEQ|};
      {|C270 OK Noop completed|};
      {|D300 OK Noop completed|};
      {|* 7 FETCH (MODSEQ (12121245160))|};
      {|A330 OK Store completed|};
      {|* 7 FETCH (FLAGS (\Deleted) MODSEQ (12121245160))|};
      {|C360 OK Noop completed|};
      {|* 7 FETCH (FLAGS (\Deleted) MODSEQ (12121245160))|};
      {|D390 OK Noop completed|};
      {|* SEARCH 2 5 6 7 11 12 18 19 20 23 (MODSEQ 917162500)|};
      {|a OK Search complete|};
      {|* SEARCH|};
      {|t OK Search complete, nothing found|};
      {|* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292|};
      {|A042 OK STATUS completed|};
      {|* 172 EXISTS|};
      {|* 1 RECENT|};
      {|* OK [UNSEEN 12] Message 12 is first unseen|};
      {|* OK [UIDVALIDITY 3857529045] UIDs valid|};
      {|* OK [UIDNEXT 4392] Predicted next UID|};
      {|* FLAGS (\Answered \Flagged \Deleted \Seen \Draft)|};
      {|* OK [PERMANENTFLAGS (\Deleted \Seen \*)] Limited|};
      {|* OK [HIGHESTMODSEQ 715194045007]|};
      {|A142 OK [READ-WRITE] SELECT completed, CONDSTORE is now enabled|};
      {|* ESEARCH (TAG "a") ALL 1:3,5 MODSEQ 1236|};
      {|a OK Extended SEARCH completed|};
      {|* ESEARCH (TAG "a") ALL 5,3,2,1 MODSEQ 1236|};
      {|a OK Extended SORT completed|};
      {|* 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed \Deleted|};
      {|* 464 EXISTS|};
      {|* 3 RECENT|};
      {|* OK [UIDVALIDITY 3857529045] UIDVALIDITY|};
      {|* OK [UIDNEXT 550] Predicted next UID|};
      {|* OK [HIGHESTMODSEQ 90060128194045007] Highest mailbox|};
      {|* OK [UNSEEN 12] Message 12 is first unseen|};
      {|* FLAGS (\Answered \Flagged \Draft \Deleted \Seen)|};
      {|* OK [PERMANENTFLAGS (\Answered \Flagged \Draft)]|};
      {|A02 OK [READ-WRITE] Sorry, UIDVALIDITY mismatch|};
      {|* OK [CLOSED]|};
      {|* 100 EXISTS|};
      {|* 11 RECENT|};
      {|* OK [UIDVALIDITY 67890007] UIDVALIDITY|};
      {|* OK [UIDNEXT 600] Predicted next UID|};
      {|* OK [HIGHESTMODSEQ 90060115205545359] Highest|};
      {|* OK [UNSEEN 7] There are some unseen|};
      {|* FLAGS (\Answered \Flagged \Draft \Deleted \Seen)|};
      {|* OK [PERMANENTFLAGS (\Answered \Flagged \Draft)]|};
      {|* VANISHED (EARLIER) 41,43:116,118,120:211,214:540|};
      {|* 49 FETCH (UID 117 FLAGS (\Seen \Answered) MODSEQ|};
      {|* 50 FETCH (UID 119 FLAGS (\Draft $MDNSent) MODSEQ|};
      {|* 51 FETCH (UID 541 FLAGS (\Seen $Forwarded) MODSEQ|};
      {|A03 OK [READ-WRITE] mailbox selected|};
      {|* 10003 EXISTS|};
      {|* 4 RECENT|};
      {|* OK [UIDVALIDITY 67890007] UIDVALIDITY|};
      {|* OK [UIDNEXT 30013] Predicted next UID|};
      {|* OK [HIGHESTMODSEQ 90060115205545359] Highest mailbox|};
      {|* OK [UNSEEN 7] There are some unseen messages in the mailbox|};
      {|* FLAGS (\Answered \Flagged \Draft \Deleted \Seen)|};
      {|* OK [PERMANENTFLAGS (\Answered \Flagged \Draft)]|};
      {|* VANISHED (EARLIER) 1:2,4:5,7:8,10:11,13:14,[...],|};
      {|* 1 FETCH (UID 3 FLAGS (\Seen \Answered $Important) MODSEQ|};
    ]
  in
  List.iter parse tests;
  [%expect {|
    (Cont YGgGCSqGSIb3EgECAgIAb1kwV6ADAgEFoQMCAQ+iSzBJoAMC)
    (Cont YDMGCSqGSIb3EgECAgIBAAD/////6jcyG4GE3KkTzBeBiVHe)
    (Cont "")
    (Cont "Ready for literal data")
    (Cont "Ready for additional command text")
    (Tagged abcd (OK () "CAPABILITY completed"))
    (Tagged efgh (OK () "STARTLS completed"))
    (Tagged ijkl (OK () "CAPABILITY completed"))
    (Tagged a002 (OK () "NOOP completed"))
    (Tagged a047 (OK () "NOOP completed"))
    (Tagged A023 (OK () "LOGOUT completed"))
    (Tagged a001 (OK () "CAPABILITY completed"))
    (Tagged a002 (OK () "Begin TLS negotiation now"))
    (Tagged a003 (OK () "CAPABILITY completed"))
    (Tagged a004 (OK () "LOGIN completed"))
    (Tagged A001 (OK () "GSSAPI authentication successful"))
    (Tagged a001 (OK () "LOGIN completed"))
    (Tagged A142 (OK (READ_WRITE) "SELECT completed"))
    (Tagged A932 (OK (READ_ONLY) "EXAMINE completed"))
    (Tagged A003 (OK () "CREATE completed"))
    (Tagged A004 (OK () "CREATE completed"))
    (Tagged A682 (OK () "LIST completed"))
    (Tagged A683 (OK () "DELETE completed"))
    (Tagged A684 (NO () "Name \"foo\" has inferior hierarchical names"))
    (Tagged A685 (OK () "DELETE Completed"))
    (Tagged A686 (OK () "LIST completed"))
    (Tagged A687 (OK () "DELETE Completed"))
    (Tagged A82 (OK () "LIST completed"))
    (Tagged A83 (OK () "DELETE completed"))
    (Tagged A84 (OK () "DELETE Completed"))
    (Untagged
     (CAPABILITY ((OTHER IMAP4rev1) (OTHER STARTTLS) (OTHER AUTH=GSSAPI))))
    (Untagged (CAPABILITY ((OTHER IMAP4rev1) (OTHER AUTH=GSSAPI) AUTH_PLAIN)))
    (Untagged (EXPUNGE 22))
    (Untagged (EXISTS 23))
    (Untagged (RECENT 3))
    (Untagged (BYE () "IMAP4rev1 Server logging out"))
    (Untagged
     (CAPABILITY ((OTHER IMAP4rev1) (OTHER STARTTLS) (OTHER LOGINDISABLED))))
    (Untagged (CAPABILITY ((OTHER IMAP4rev1) AUTH_PLAIN)))
    (Untagged (State (OK () "IMAP4rev1 Server")))
    (Untagged (EXISTS 172))
    (Untagged (RECENT 1))
    (Untagged (State (OK ((UNSEEN 12)) "Message 12 is first unseen")))
    (Untagged (State (OK ((UIDVALIDITY -437438251)) "UIDs valid")))
    (Untagged (State (OK ((UIDNEXT 4392)) "Predicted next UID")))
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Deleted \\Seen \\*)"))) Limited)))
    (Untagged (EXISTS 17))
    (Untagged (RECENT 2))
    (Untagged (State (OK ((UNSEEN 8)) "Message 8 is first unseen")))
    (Untagged (State (OK ((UIDNEXT 4392)) "Predicted next UID")))
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" ()"))) "No permanent flags permitted")))
    (Untagged (LIST () (/) blurdybloop))
    (Untagged (LIST (Noselect) (/) foo))
    (Untagged (LIST () (/) foo/bar))
    (Untagged (LIST (Noselect) (/) foo))
    (Untagged (LIST () (.) blurdybloop))
    (Untagged (LIST () (.) foo))
    (Untagged (LIST () (.) foo.bar))
    (Untagged (LIST () (.) foo.bar))
    (Tagged A85 (OK () "LIST completed"))
    (Untagged (LIST (Noselect) (.) foo))
    (Tagged A86 (OK () "LIST completed"))
    (Untagged (LIST () (/) blurdybloop))
    (Untagged (LIST (Noselect) (/) foo))
    (Untagged (LIST () (/) foo/bar))
    (Tagged A682 (OK () "LIST completed"))
    (Tagged A683 (OK () "RENAME completed"))
    (Tagged A684 (OK () "RENAME Completed"))
    (Untagged (LIST () (/) sarasoop))
    (Untagged (LIST (Noselect) (/) zowie))
    (Untagged (LIST () (/) zowie/bar))
    (Tagged A685 (OK () "LIST completed"))
    (Untagged (LIST () (.) INBOX))
    (Untagged (LIST () (.) INBOX.bar))
    (Tagged Z432 (OK () "LIST completed"))
    (Tagged Z433 (OK () "RENAME completed"))
    (Untagged (LIST () (.) INBOX))
    (Untagged (LIST () (.) INBOX.bar))
    (Untagged (LIST () (.) old-mail))
    (Tagged Z434 (OK () "LIST completed"))
    (Tagged A002 (OK () "SUBSCRIBE completed"))
    (Tagged A002 (OK () "UNSUBSCRIBE completed"))
    (Untagged (LIST (Noselect) (/) ""))
    (Tagged A101 (OK () "LIST Completed"))
    (Untagged (LIST (Noselect) (.) #news.))
    (Tagged A102 (OK () "LIST Completed"))
    (Untagged (LIST (Noselect) (/) /))
    (Tagged A103 (OK () "LIST Completed"))
    (Untagged (LIST (Noselect) (/) ~/Mail/foo))
    (Untagged (LIST () (/) ~/Mail/meetings))
    (Tagged A202 (OK () "LIST completed"))
    (Untagged (LSUB () (.) #news.comp.mail.mime))
    (Untagged (LSUB () (.) #news.comp.mail.misc))
    (Tagged A002 (OK () "LSUB completed"))
    (Untagged (LSUB (Noselect) (.) #news.comp.mail))
    (Tagged A003 (OK () "LSUB completed"))
    (Tagged A042 (OK () "STATUS completed"))
    (Tagged A003 (OK () "APPEND completed"))
    (Tagged FXXZ (OK () "CHECK Completed"))
    (Tagged A341 (OK () "CLOSE completed"))
    (Untagged (EXPUNGE 3))
    (Untagged (EXPUNGE 3))
    (Untagged (EXPUNGE 5))
    (Untagged (EXPUNGE 8))
    (Tagged A202 (OK () "EXPUNGE completed"))
    (Untagged (SEARCH (2 84 882) ()))
    (Tagged A282 (OK () "SEARCH completed"))
    (Untagged (SEARCH () ()))
    (Tagged A283 (OK () "SEARCH completed"))
    (Untagged (SEARCH (43) ()))
    (Tagged A284 (OK () "SEARCH completed"))
    (Tagged A654 (OK () "FETCH completed"))
    (Tagged A003 (OK () "STORE completed"))
    (Tagged A003 (OK () "COPY completed"))
    (Tagged A999 (OK () "UID FETCH completed"))
    (Untagged (CAPABILITY ((OTHER IMAP4rev1) (OTHER XPIG-LATIN))))
    (Tagged a441 (OK () "CAPABILITY completed"))
    (Tagged A442 (OK () "XPIG-LATIN ompleted-cay"))
    (Untagged (State (OK () "IMAP4rev1 server ready")))
    (Untagged (State (OK (ALERT) "System shutdown in 10 minutes")))
    (Tagged A001 (OK () "LOGIN Completed"))
    (Untagged (State (NO () "Disk is 98% full, please delete unnecessary data")))
    (Tagged A222 (OK () "COPY completed"))
    (Untagged (State (NO () "Disk is 98% full, please delete unnecessary data")))
    (Untagged (State (NO () "Disk is 99% full, please delete unnecessary data")))
    (Tagged A223 (NO () "COPY failed: disk is full"))
    (Untagged (State (BAD () "Command line too long")))
    (Untagged (State (BAD () "Empty command line")))
    (Untagged (State (BAD () "Disk crash, attempting salvage to a new disk!")))
    (Untagged (State (OK () "Salvage successful, no data lost")))
    (Tagged A443 (OK () "Expunge completed"))
    (Untagged (PREAUTH () "IMAP4rev1 server logged in as Smith"))
    (Untagged (BYE () "Autologout; idle for too long"))
    (Untagged
     (CAPABILITY
      ((OTHER IMAP4rev1) (OTHER STARTTLS) (OTHER AUTH=GSSAPI) (OTHER XPIG-LATIN))))
    (Untagged (LIST (Noselect) (/) ~/Mail/foo))
    (Untagged (LSUB () (.) #news.comp.mail.misc))
    (Untagged (SEARCH (2 3 6) ()))
    (Untagged (EXISTS 23))
    (Untagged (RECENT 5))
    (Untagged (EXPUNGE 44))
    (Tagged A001 (OK () "LOGIN completed"))
    (Tagged A044 (BAD () "No such command as \"BLURDYBLOOP\""))
    (Untagged (State (OK () "IMAP4rev1 Service Ready")))
    (Tagged a001 (OK () "LOGIN completed"))
    (Untagged (EXISTS 18))
    (Untagged (RECENT 2))
    (Untagged
     (State (OK ((UNSEEN 17)) "Message 17 is the first unseen message")))
    (Untagged (State (OK ((UIDVALIDITY -437438251)) "UIDs valid")))
    (Tagged a002 (OK (READ_WRITE) "SELECT completed"))
    (Tagged a003 (OK () "FETCH completed"))
    (Untagged (EXISTS 172))
    (Untagged (RECENT 1))
    (Untagged (State (OK ((UNSEEN 12)) "Message 12 is first unseen")))
    (Untagged (State (OK ((UIDVALIDITY -437438251)) "UIDs valid")))
    (Untagged (State (OK ((UIDNEXT 4392)) "Predicted next UID")))
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Deleted \\Seen \\*)"))) Limited)))
    (Untagged (State (OK ((OTHER HIGHESTMODSEQ (" 715194045007"))) "")))
    (Tagged A142 (OK (READ_WRITE) "SELECT completed"))
    (Untagged (EXISTS 172))
    (Untagged (RECENT 1))
    (Untagged (State (OK ((UNSEEN 12)) "Message 12 is first unseen")))
    (Untagged (State (OK ((UIDVALIDITY -437438251)) "UIDs valid")))
    (Untagged (State (OK ((UIDNEXT 4392)) "Predicted next UID")))
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Deleted \\Seen \\*)"))) Limited)))
    (Untagged
     (State (OK (NOMODSEQ) "Sorry, this mailbox format doesn't support")))
    (Tagged A142 (OK (READ_WRITE) "SELECT completed"))
    (Untagged (FETCH 1 ((UID 4) (MODSEQ 12121231000))))
    (Untagged (FETCH 2 ((UID 6) (MODSEQ 12121230852))))
    (Untagged (FETCH 4 ((UID 8) (MODSEQ 12121230956))))
    (Tagged a103 (OK () "Conditional Store completed"))
    (Untagged (FETCH 50 ((MODSEQ 12111230047))))
    (Tagged a104 (OK () "Store (conditional) completed"))
    (Untagged (State (OK ((OTHER HIGHESTMODSEQ (" 12111230047"))) "")))
    (Untagged (FETCH 50 ((MODSEQ 12111230048))))
    (Tagged c101 (OK () "Store (conditional) completed"))
    (Untagged (FETCH 5 ((MODSEQ 320162350))))
    (Tagged d105 (OK ((OTHER MODIFIED (" 7,9"))) "Conditional STORE failed"))
    (Untagged (FETCH 5 ((MODSEQ 320162350))))
    (Tagged d105 (OK ((OTHER MODIFIED (" 7,9"))) "Conditional STORE failed"))
    (Tagged a102 (OK ((OTHER MODIFIED (" 12"))) "Conditional STORE failed"))
    (Untagged (FETCH 100 ((MODSEQ 303181230852))))
    (Untagged (FETCH 102 ((MODSEQ 303181230852))))
    (Untagged (FETCH 150 ((MODSEQ 303181230852))))
    (Tagged a106 (OK ((OTHER MODIFIED (" 101"))) "Conditional STORE failed"))
    (Tagged a107 (OK () ""))
    (Tagged b107 (OK () ""))
    (Untagged (FETCH 101 ((MODSEQ 303181230852))))
    (Tagged b108 (OK () "Conditional Store completed"))
    (Untagged (FETCH 100 ((MODSEQ 303181230852))))
    (Untagged (FETCH 102 ((MODSEQ 303181230852))))
    (Untagged (FETCH 150 ((MODSEQ 303181230852))))
    (Tagged a106 (OK ((OTHER MODIFIED (" 101"))) "Conditional STORE failed"))
    (Untagged (FETCH 100 ((MODSEQ 303181230852))))
    (Untagged (FETCH 102 ((MODSEQ 303181230852))))
    (Untagged (FETCH 150 ((MODSEQ 303181230852))))
    (Tagged a106 (OK ((OTHER MODIFIED (" 101"))) "Conditional STORE failed"))
    (Untagged (FETCH 101 ((MODSEQ 303181230852))))
    (Tagged b108 (OK () "Conditional Store completed"))
    (Untagged (FETCH 100 ((MODSEQ 303181230852))))
    (Untagged (FETCH 102 ((MODSEQ 303181230852))))
    (Untagged (FETCH 150 ((MODSEQ 303181230852))))
    (Tagged a106 (OK () "Conditional STORE completed"))
    (Tagged B001
     (NO ((OTHER MODIFIED (" 2"))) "Some of the messages no longer exist."))
    (Untagged (EXPUNGE 4))
    (Untagged (EXPUNGE 4))
    (Untagged (EXPUNGE 4))
    (Untagged (EXPUNGE 4))
    (Tagged B002 (OK () "NOOP Completed."))
    (Tagged b003 (OK () "Conditional Store completed"))
    (Tagged s100 (OK () "FETCH completed"))
    (Untagged (FETCH 1 ((MODSEQ 624140003))))
    (Untagged (FETCH 2 ((MODSEQ 624140007))))
    (Untagged (FETCH 3 ((MODSEQ 624140005))))
    (Tagged a (OK () "Fetch complete"))
    (Untagged
     (State
      (OK ((OTHER PERMANENTFLAGS (" (\\Answered \\Deleted \\Seen \\*)")))
       Limited)))
    (Untagged (FETCH 7 ((MODSEQ 2121231000))))
    (Tagged A160 (OK () "Store completed"))
    (Tagged C180 (OK () "Noop completed"))
    (Tagged D210 (OK () "Noop completed"))
    (Untagged (FETCH 7 ((MODSEQ 12121231777))))
    (Tagged A240 (OK () "Store completed"))
    (Tagged C270 (OK () "Noop completed"))
    (Tagged D300 (OK () "Noop completed"))
    (Untagged (FETCH 7 ((MODSEQ 12121245160))))
    (Tagged A330 (OK () "Store completed"))
    (Tagged C360 (OK () "Noop completed"))
    (Tagged D390 (OK () "Noop completed"))
    (Tagged a (OK () "Search complete"))
    (Untagged (SEARCH () ()))
    (Tagged t (OK () "Search complete, nothing found"))
    (Tagged A042 (OK () "STATUS completed"))
    (Untagged (EXISTS 172))
    (Untagged (RECENT 1))
    (Untagged (State (OK ((UNSEEN 12)) "Message 12 is first unseen")))
    (Untagged (State (OK ((UIDVALIDITY -437438251)) "UIDs valid")))
    (Untagged (State (OK ((UIDNEXT 4392)) "Predicted next UID")))
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Deleted \\Seen \\*)"))) Limited)))
    (Untagged (State (OK ((OTHER HIGHESTMODSEQ (" 715194045007"))) "")))
    (Tagged A142 (OK (READ_WRITE) "SELECT completed, CONDSTORE is now enabled"))
    (Tagged a (OK () "Extended SEARCH completed"))
    (Tagged a (OK () "Extended SORT completed"))
    (Untagged (EXISTS 464))
    (Untagged (RECENT 3))
    (Untagged (State (OK ((UIDVALIDITY -437438251)) UIDVALIDITY)))
    (Untagged (State (OK ((UIDNEXT 550)) "Predicted next UID")))
    (Untagged
     (State
      (OK ((OTHER HIGHESTMODSEQ (" 90060128194045007"))) "Highest mailbox")))
    (Untagged (State (OK ((UNSEEN 12)) "Message 12 is first unseen")))
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Answered \\Flagged \\Draft)"))) "")))
    (Tagged A02 (OK (READ_WRITE) "Sorry, UIDVALIDITY mismatch"))
    (Untagged (State (OK (CLOSED) "")))
    (Untagged (EXISTS 100))
    (Untagged (RECENT 11))
    (Untagged (State (OK ((UIDVALIDITY 67890007)) UIDVALIDITY)))
    (Untagged (State (OK ((UIDNEXT 600)) "Predicted next UID")))
    (Untagged
     (State (OK ((OTHER HIGHESTMODSEQ (" 90060115205545359"))) Highest)))
    (Untagged (State (OK ((UNSEEN 7)) "There are some unseen")))
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Answered \\Flagged \\Draft)"))) "")))
    (Tagged A03 (OK (READ_WRITE) "mailbox selected"))
    (Untagged (EXISTS 10003))
    (Untagged (RECENT 4))
    (Untagged (State (OK ((UIDVALIDITY 67890007)) UIDVALIDITY)))
    (Untagged (State (OK ((UIDNEXT 30013)) "Predicted next UID")))
    (Untagged
     (State
      (OK ((OTHER HIGHESTMODSEQ (" 90060115205545359"))) "Highest mailbox")))
    (Untagged
     (State (OK ((UNSEEN 7)) "There are some unseen messages in the mailbox")))
    (Untagged
     (State (OK ((OTHER PERMANENTFLAGS (" (\\Answered \\Flagged \\Draft)"))) "")))
    Parsing error:
    * 14 FETCH (FLAGS (\Seen \Deleted))
                     ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
           ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
           ^
    Parsing error:
    * STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)
            ^
    Parsing error:
    * 2 FETCH (FLAGS (\Deleted \Seen))
                    ^
    Parsing error:
    * 3 FETCH (FLAGS (\Deleted))
                    ^
    Parsing error:
    * 4 FETCH (FLAGS (\Deleted \Flagged \Seen))
                    ^
    Parsing error:
    * 23 FETCH (FLAGS (\Seen) UID 4827313)
                     ^
    Parsing error:
    * 24 FETCH (FLAGS (\Seen) UID 4827943)
                     ^
    Parsing error:
    * 25 FETCH (FLAGS (\Seen) UID 4828442)
                     ^
    Parsing error:
    * STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)
            ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
           ^
    Parsing error:
    * 23 FETCH (FLAGS (\Seen) RFC822.SIZE 44827)
                     ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
           ^
    Parsing error:
    * 12 FETCH (FLAGS (\Seen) INTERNALDATE "17-Jul-1996 02:44:25 -0700"
                     ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
           ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
           ^
    Parsing error:
    * 7 FETCH (MODSEQ (320162342) FLAGS (\Seen \Deleted))
                                       ^
    Parsing error:
    * 9 FETCH (MODSEQ (320162349) FLAGS (\Answered))
                                       ^
    Parsing error:
    * 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed))
                                            ^
    Parsing error:
    * 101 FETCH (MODSEQ (303011130956) FLAGS (\Deleted \Answered))
                                            ^
    Parsing error:
    * 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed))
                                            ^
    Parsing error:
    * 101 FETCH (MODSEQ (303011130956) FLAGS (\Deleted \Answered))
                                            ^
    Parsing error:
    * 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed \Deleted
                                            ^
    Parsing error:
    * 1 FETCH (MODSEQ (320172342) FLAGS (\SEEN))
                                       ^
    Parsing error:
    * 3 FETCH (MODSEQ (320172342) FLAGS (\SEEN))
                                       ^
    Parsing error:
    * 2 FETCH (MODSEQ (320172340) FLAGS (\Deleted \Answered))
                                       ^
    Parsing error:
    * 2 FETCH (MODSEQ (320180050) FLAGS (\SEEN \Flagged))
                                       ^
    Parsing error:
    * 1 FETCH (UID 4 MODSEQ (65402) FLAGS (\Seen))
                                         ^
    Parsing error:
    * 2 FETCH (UID 6 MODSEQ (75403) FLAGS (\Deleted))
                                         ^
    Parsing error:
    * 4 FETCH (UID 8 MODSEQ (29738) FLAGS ($NoJunk $AutoJunk
                                         ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
           ^
    Parsing error:
    * 7 FETCH (FLAGS (\Deleted \Answered) MODSEQ (12121231000))
                    ^
    Parsing error:
    * 7 FETCH (FLAGS (\Deleted \Answered) MODSEQ (12121231000))
                    ^
    Parsing error:
    * 7 FETCH (FLAGS (\Deleted \Answered \Seen) MODSEQ
                    ^
    Parsing error:
    * 7 FETCH (FLAGS (\Deleted) MODSEQ (12121245160))
                    ^
    Parsing error:
    * 7 FETCH (FLAGS (\Deleted) MODSEQ (12121245160))
                    ^
    Parsing error:
    * SEARCH 2 5 6 7 11 12 18 19 20 23 (MODSEQ 917162500)
                                       ^
    Parsing error:
    * STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292
            ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
           ^
    Parsing error:
    * ESEARCH (TAG "a") ALL 1:3,5 MODSEQ 1236
             ^
    Parsing error:
    * ESEARCH (TAG "a") ALL 5,3,2,1 MODSEQ 1236
             ^
    Parsing error:
    * 101 FETCH (MODSEQ (303011130956) FLAGS ($Processed \Deleted
                                            ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Draft \Deleted \Seen)
           ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Draft \Deleted \Seen)
           ^
    Parsing error:
    * VANISHED (EARLIER) 41,43:116,118,120:211,214:540
              ^
    Parsing error:
    * 49 FETCH (UID 117 FLAGS (\Seen \Answered) MODSEQ
                             ^
    Parsing error:
    * 50 FETCH (UID 119 FLAGS (\Draft $MDNSent) MODSEQ
                             ^
    Parsing error:
    * 51 FETCH (UID 541 FLAGS (\Seen $Forwarded) MODSEQ
                             ^
    Parsing error:
    * FLAGS (\Answered \Flagged \Draft \Deleted \Seen)
           ^
    Parsing error:
    * VANISHED (EARLIER) 1:2,4:5,7:8,10:11,13:14,[...],
              ^
    Parsing error:
    * 1 FETCH (UID 3 FLAGS (\Seen \Answered $Important) MODSEQ
                          ^ |}]
