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

let quoted buf =
  error buf

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
  take_while1 is_text_char buf

let is_text_other_char c =
  is_text_char c && (c <> ']')

let text_1 =
  take_while1 is_text_other_char

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
  match atom buf with
  | "Noselect" -> Noselect
  | "Marked" -> Marked
  | "Unmarked" -> Unmarked
  | "Noinferiors" -> Noinferiors
  | "HasChildren" -> HasChildren
  | "HasNoChildren" -> HasNoChildren
  | "All" -> All
  | "Archive" -> Archive
  | "Drafts" -> Drafts
  | "Flagged" -> Flagged
  | "Junk" -> Junk
  | "Sent" -> Sent
  | "Trash" -> Trash
  | a -> Extension a

let delim buf =
  match curr buf with
  | '"' ->
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
  let k code = char ']' buf; char ' ' buf; k code in
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
  code buf (fun code -> k code (text buf))

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
      (* | "FETCH" ->
       *     sp *> msg_att >>| (fun x -> FETCH (n, x))
       * | "VANISHED (EARLIER)" ->
       *     sp *> known_ids >>| (fun l -> VANISHED_EARLIER l)
       * | "VANISHED" ->
       *     sp *> known_ids >>| (fun l -> VANISHED l) *)
      | _ ->
          error buf
      end
  | _ ->
      begin match atom buf with
      | "OK" ->
          char ' ' buf;
          resp_text buf (fun code text -> k (State (OK (code, text))))
      | "NO" ->
          char ' ' buf;
          resp_text buf (fun code text -> k (State (NO (code, text))))
      | "BAD" ->
          char ' ' buf;
          resp_text buf (fun code text -> k (State (BAD (code, text))))
      | "BYE" ->
          char ' ' buf;
          resp_text buf (fun code text -> k (BYE (code, text)))
      (* | "FLAGS" ->
       *     sp *> psep_by sp flag >>| (fun l -> FLAGS l) *)
      | "LIST" ->
          char ' ' buf;
          mailbox_list buf (fun xs c m -> k (LIST (xs, c, m)))
      | "LSUB" ->
          char ' ' buf;
          mailbox_list buf (fun xs c m -> k (LSUB (xs, c, m)))
      (* | "SEARCH" ->
       *     pair (return ()) (many (sp *> nz_number)) (option None (sp *> some search_sort_mod_seq)) >>| (fun (acc, n) -> SEARCH (acc, n))
       * | "STATUS" ->
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
          char ' ' buf;
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
      char ' ' buf;
      resp_text buf (fun code text -> k (OK (code, text)))
  | "NO" ->
      char ' ' buf;
      resp_text buf (fun code text -> k (NO (code, text)))
  | "BAD" ->
      char ' ' buf;
      resp_text buf (fun code text -> k (BAD (code, text)))
  | _ ->
      error buf

let response buf k =
  match curr buf with
  | '+' ->
      next buf;
      if curr buf = ' ' then next buf;
      if not (is_eol buf) then resp_text buf (fun _ x -> k (Cont x))
      else k (Cont "")
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
    ]
  in
  List.iter parse tests;
  [%expect {| |}]
