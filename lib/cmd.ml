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

(* type error = *)
(*   | Incorrect_tag of string * string *)
(*   | Decode_error of string * int *)
(*   | Unexpected_cont *)
(*   | Bad_greeting *)
(*   | Auth_error of string *)
(*   | Server_error of string *)

(* exception Error of error *)

(* let () = *)
(*   Printexc.register_printer (function *)
(*       | Error (Decode_error (s, pos)) -> *)
(*           Some (Printf.sprintf "Parsing error:\n%s\n%s^\n" s (String.make pos ' ')) *)
(*       | _ -> *)
(*           None *)
(*     ) *)
open Common
open Response

let capability_to_string = function
  | IMAP4rev1 -> "IMAP4rev1"
  | ACL -> "ACL"
  | BINARY -> "BINARY"
  | CATENATE -> "CATENATE"
  | CHILDREN -> "CHILDREN"
  | COMPRESS_DEFLATE -> "COMPRESS=DEFLATE"
  | CONDSTORE -> "CONDSTORE"
  | ESEARCH -> "ESEARCH"
  | ENABLE -> "ENABLE"
  | IDLE -> "IDLE"
  | ID -> "ID"
  | LITERALPLUS -> "LITERAL+"
  | LITERALMINUS -> "LITERAL-"
  | UTF8_ACCEPT -> "UTF8=ACCEPT"
  | UTF8_ONLY -> "UTF8=ONLY"
  | MULTIAPPEND -> "MULTIAPPEND"
  | NAMESPACE -> "NAMESPACE"
  | QRESYNC -> "QRESYNC"
  | QUOTE -> "QUOTE"
  | SORT -> "SORT"
  | STARTTLS -> "STARTTLS"
  | UIDPLUS -> "UIDPLUS"
  | UNSELECT -> "UNSELECT"
  | XLIST -> "XLIST"
  | AUTH_ANONYMOUS -> "AUTH=ANONYMOUS"
  | AUTH_LOGIN -> "AUTH=LOGIN"
  | AUTH_PLAIN -> "AUTH=PLAIN"
  | XOAUTH2 -> "XOAUTH2"
  | X_GM_EXT_1 -> "X-GM-EXT-1"
  | OTHER s -> s

let encode_capability s = Encoder.raw (capability_to_string s)

let flag_to_string = function
  | Answered -> "\\Answered"
  | Flagged -> "\\Flagged"
  | Deleted -> "\\Deleted"
  | Seen -> "\\Seen"
  | Draft -> "\\Draft"
  | Keyword s -> s
  | Extension s -> "\\" ^ s
  | Recent -> "\\Recent"
  | Any -> "\\*"

let encode_flag f = Encoder.raw (flag_to_string f)

type _ u = E : 'b * ('b -> untagged -> 'b) * ('b -> 'a) -> 'a u

type 'a cmd = { format : Encoder.t; u : 'a u }

let encode tag { format; _ } =
  let rec loop acc = function
    | Encoder.Wait :: k as k' ->
        if acc = [] then `Wait (loop [] k)
        else `Next (String.concat "" (List.rev acc), loop [] k')
    | Raw s :: k -> loop (s :: acc) k
    | Crlf :: k -> loop ("\r\n" :: acc) k
    | [] ->
        if acc = [] then `End else `Next (String.concat "" (List.rev acc), `End)
  in
  loop [] (Encoder.((raw tag ++ format) & crlf) [])

let process { format; u = E (res, process, finish) } = function
  | State { status = NO | BAD; message; _ } -> Error message
  | u -> Ok { format; u = E (process res u, process, finish) }

let finish { u = E (res, _, finish); _ } = finish res

(* let idle imap = *)
(*   let process = wrap_process (fun _ r _ -> r) in *)
(*   let tag = tag imap in *)
(*   let r = Encoder.(raw tag ++ raw "IDLE" & crlf) in *)
(*   let t, u = Lwt.wait () in *)
(*   let t = t >>= fun () -> send imap Encoder.(raw "DONE" & crlf) process () in *)
(*   let stop () = imap.stop_poll <- None; Lwt.wakeup u () in *)
(*   send imap r process () >>= fun () -> *)
(*   let rec loop () = *)
(*     recv imap >>= function *)
(*     | Response.Cont _ -> *)
(*         imap.stop_poll <- Some stop; *)
(*         loop () *)
(*     | Untagged _ -> *)
(*         stop (); *)
(*         loop () *)
(*     | Tagged (_t, _) -> (\* FIXME *\) *)
(*         imap.tag <- imap.tag + 1; *)
(*         Lwt.return_unit *)
(*   in *)
(*   Lwt.join [loop (); t] *)

(* let stop_poll imap = *)
(*   match imap.stop_poll with *)
(*   | Some f -> f () *)
(*   | None -> () *)

(* let poll imap = *)
(*   idle imap *)

let simple format v =
  let process res _ = res in
  let finish res = res in
  { format; u = E (v, process, finish) }

let login username password =
  let format = Encoder.(str "LOGIN" ++ str username ++ str password) in
  simple format ()

let _capability =
  let format = Encoder.(str "CAPABILITY") in
  let process caps = function CAPABILITY caps1 -> caps1 :: caps | _ -> caps in
  let finish l = List.rev l |> List.flatten in
  { format; u = E ([], process, finish) }

let create m =
  let format = Encoder.(str "CREATE" ++ mutf7 m) in
  simple format ()

let delete m =
  let format = Encoder.(str "DELETE" ++ mutf7 m) in
  simple format ()

let rename m1 m2 =
  let format = Encoder.(str "RENAME" ++ mutf7 m1 ++ mutf7 m2) in
  simple format ()

let logout =
  let format = Encoder.(str "LOGOUT") in
  simple format ()

let noop =
  let format = Encoder.(str "NOOP") in
  simple format ()

let list ?(ref = "") s =
  let format = Encoder.(str "LIST" ++ mutf7 ref ++ str s) in
  let process res = function
    | LIST (flags, delim, mbox) -> (flags, delim, mbox) :: res
    | _ -> res
  in
  let finish = List.rev in
  { format; u = E ([], process, finish) }

module Status_response = struct
  type t = {
    messages : int option;
    recent : int option;
    uidnext : Common.uid option;
    uidvalidity : Common.uid option;
    unseen : int option;
    highestmodseq : Common.modseq option;
  }

  let default =
    {
      messages = None;
      recent = None;
      uidnext = None;
      uidvalidity = None;
      unseen = None;
      highestmodseq = None;
    }
end

let status m att =
  let format = Encoder.(str "STATUS" ++ mutf7 m ++ Status.encode att) in
  let process res = function
    | STATUS (mbox, items) when m = mbox ->
        let aux res = function
          | (MESSAGES n : Status.MailboxAttribute.t) ->
              { res with Status_response.messages = Some n }
          | RECENT n -> { res with recent = Some n }
          | UIDNEXT n -> { res with uidnext = Some n }
          | UIDVALIDITY n -> { res with uidvalidity = Some n }
          | UNSEEN n -> { res with unseen = Some n }
          | HIGHESTMODSEQ n -> { res with highestmodseq = Some n }
        in
        let r = List.fold_left aux Status_response.default items in
        let rec go : type a. a Status.t -> a option = function
          | Status.MESSAGES -> r.Status_response.messages
          | RECENT -> r.recent
          | UIDNEXT -> r.uidnext
          | UIDVALIDITY -> r.uidvalidity
          | UNSEEN -> r.unseen
          | HIGHESTMODSEQ -> r.highestmodseq
          | PAIR (x, y) -> (
              match (go x, go y) with
              | Some x, Some y -> Some (x, y)
              | _ -> None )
          | MAP (f, x) -> (
              match go x with Some x -> Some (f x) | None -> None )
        in
        go att
    | _ -> res
  in
  let finish res = res in
  { format; u = E (None, process, finish) }

let copy_gen cmd nums mbox =
  let format =
    Encoder.(raw cmd ++ eset (Uint32.Set.of_list nums) ++ mutf7 mbox)
  in
  simple format ()

let copy = copy_gen "COPY"

let uid_copy = copy_gen "UID COPY"

let _check =
  let format = Encoder.(str "CHECK") in
  simple format ()

let _close = simple Encoder.(raw "CLOSE") ()

let expunge =
  let format = Encoder.(str "EXPUNGE") in
  simple format ()

let uid_expunge nums =
  let format = Encoder.(str "UID EXPUNGE" ++ eset (Uint32.Set.of_list nums)) in
  simple format ()

module Search = struct
  open Encoder

  type key = t

  let all = raw "ALL"

  let seq s = eset (Uint32.Set.of_list s)

  let answered = raw "ANSWERED"

  let bcc s = raw "BCC" ++ str s

  let before t = raw "BEFORE" ++ Date.encode t

  let body s = raw "BODY" ++ str s

  let cc s = raw "CC" ++ str s

  let deleted = raw "DELETED"

  let draft = raw "DRAFT"

  let flagged = raw "FLAGGED"

  let from s = raw "FROM" ++ str s

  let header s1 s2 = raw "HEADER" ++ str s1 ++ str s2

  let keyword s = raw "KEYWORD" ++ str s

  let larger n = raw "LARGER" ++ int n

  let new_ = raw "NEW"

  let not k = raw "NOT" ++ p k

  let old = raw "OLD"

  let on t = raw "ON" ++ Date.encode t

  let ( || ) k1 k2 = raw "OR" ++ p k1 ++ p k2

  let recent = raw "RECENT"

  let seen = raw "SEEN"

  let sent_before t = raw "SENTBEFORE" ++ Date.encode t

  let sent_on t = raw "SENTON" ++ Date.encode t

  let sent_since t = raw "SENTSINCE" ++ Date.encode t

  let since t = raw "SINCE" ++ Date.encode t

  let smaller n = raw "SMALLER" ++ int n

  let subject s = raw "SUBJECT" ++ str s

  let text s = raw "TEXT" ++ str s

  let to_ s = raw "TO" ++ str s

  let uid s = raw "UID" ++ eset (Uint32.Set.of_list s)

  let unanswered = raw "UNANSWERED"

  let undeleted = raw "UNDELETED"

  let undraft = raw "UNDRAFT"

  let unflagged = raw "UNFLAGGED"

  let unkeyword s = raw "UNKEYWORD" ++ str s

  let unseen = raw "UNSEEN"

  let ( && ) k1 k2 = p k1 ++ p k2

  let modseq n = raw "MODSEQ" ++ uint64 n

  let x_gm_raw s = raw "X-GM-RAW" ++ str s

  let x_gm_msgid n = raw "X-GM-MSGID" ++ uint64 n

  let x_gm_thrid n = raw "X-GM-THRID" ++ uint64 n

  let x_gm_labels l = raw "X-GM-LABELS" ++ list str l
end

let search_gen cmd sk =
  let format = Encoder.(raw cmd ++ sk) in
  let process res = function
    | SEARCH (ids, modseq) -> (ids :: fst res, modseq)
    | _ -> res
  in
  let finish (ids, modseq) = (List.(rev ids |> flatten), modseq) in
  { format; u = E (([], None), process, finish) }

let search = search_gen "SEARCH"

let uid_search = search_gen "UID SEARCH"

let select_gen cmd m =
  let arg =
    if false (* List.mem Capability.CONDSTORE imap.capabilities *) then
      " (CONDSTORE)"
    else ""
  in
  let format = Encoder.((raw cmd ++ mutf7 m) & raw arg) in
  simple format ()

let select = select_gen "SELECT"

let examine = select_gen "EXAMINE"

let append m ?flags ?internaldate data =
  let flags =
    match flags with
    | None -> Encoder.empty
    | Some l -> Encoder.(raw " " & p (list encode_flag l))
  in
  let internaldate =
    match internaldate with
    | None -> Encoder.empty
    | Some s -> Encoder.(raw " " & str s)
  in
  let format =
    Encoder.((raw "APPEND" ++ mutf7 m) & flags & (internaldate ++ literal data))
  in
  simple format ()

module Int32Map = Map.Make (Int32)

module Fetch = struct
  type 'a t =
    | FLAGS : flag list t
    | ENVELOPE : envelope t
    | INTERNALDATE : string t
    | UID : uid t
    | X_GM_MSGID : int64 t
    | X_GM_THRID : int64 t
    | X_GM_LABELS : string list t
    | RFC822 : string t
    | RFC822_TEXT : string t
    | RFC822_HEADER : string t
    | RFC822_SIZE : int t
    | BODY : mime t
    | BODYSTRUCTURE : mime t
    | MODSEQ : int64 t
    | PAIR : 'a t * 'b t -> ('a * 'b) t
    | MAP : ('a -> 'b) * 'a t -> 'b t

  let flags = FLAGS

  let envelope = ENVELOPE

  let internaldate = INTERNALDATE

  let uid = UID

  let x_gm_msgid = X_GM_MSGID

  let x_gm_thrid = X_GM_THRID

  let x_gm_labels = X_GM_LABELS

  let rfc822 = RFC822

  let rfc822_text = RFC822_TEXT

  let rfc822_header = RFC822_HEADER

  let rfc822_size = RFC822_SIZE

  let body = BODY

  let bodystructure = BODYSTRUCTURE

  let modseq = MODSEQ

  let map f x = MAP (f, x)

  let pair x y = PAIR (x, y)

  let rec encode : type a. a t -> Encoder.t = function
    | ENVELOPE -> Encoder.raw "ENVELOPE"
    | INTERNALDATE -> Encoder.raw "INTERNALDATE"
    | RFC822_HEADER -> Encoder.raw "RFC822.HEADER"
    | RFC822_SIZE -> Encoder.raw "RFC822.SIZE"
    | RFC822_TEXT -> Encoder.raw "RFC822.TEXT"
    | MODSEQ -> Encoder.raw "MODSEQ"
    | RFC822 -> Encoder.raw "RFC822"
    | BODY -> Encoder.raw "BODY"
    | BODYSTRUCTURE -> Encoder.raw "BODYSTRUCTURE"
    | UID -> Encoder.raw "UID"
    | FLAGS -> Encoder.raw "FLAGS"
    | X_GM_MSGID -> Encoder.raw "X-GM-MSGID"
    | X_GM_THRID -> Encoder.raw "X-GM-THRID"
    | X_GM_LABELS -> Encoder.raw "X-GM-LABELS"
    | PAIR _ as x ->
        let rec go : type a. _ -> a t -> _ =
         fun acc x ->
          match x with PAIR (x, y) -> go (go acc x) y | x -> encode x :: acc
        in
        Encoder.plist (fun x -> x) (List.rev (go [] x))
    | MAP (_, x) -> encode x

  (* let body_section ?(peek = true) ?section:(sec = [], None) () =
     raw (if peek then "BODY.PEEK" else "BODY") & raw "[" & MIME.Request.encode sec & raw "]" *)

  type u = {
    flags : flag list option;
    uid : uid option;
    envelope : envelope option;
    internaldate : string option;
    rfc822 : string option;
    rfc822_header : string option;
    rfc822_text : string option;
    rfc822_size : int option;
    body : mime option;
    bodystructure : mime option;
    modseq : int64 option;
    x_gm_msgid : int64 option;
    x_gm_thrid : int64 option;
    x_gm_labels : string list option;
  }

  let empty =
    {
      flags = None;
      uid = None;
      envelope = None;
      internaldate = None;
      rfc822 = None;
      rfc822_header = None;
      rfc822_text = None;
      rfc822_size = None;
      body = None;
      bodystructure = None;
      modseq = None;
      x_gm_msgid = None;
      x_gm_thrid = None;
      x_gm_labels = None;
    }

  let matches t a =
    let aux u = function
      | (FLAGS l : message_attribute) -> { u with flags = Some l }
      | UID n -> { u with uid = Some n }
      | ENVELOPE e -> { u with envelope = Some e }
      | INTERNALDATE x -> { u with internaldate = Some x }
      | RFC822 x -> { u with rfc822 = Some x }
      | RFC822_HEADER x -> { u with rfc822_header = Some x }
      | RFC822_TEXT x -> { u with rfc822_text = Some x }
      | RFC822_SIZE x -> { u with rfc822_size = Some x }
      | BODY x -> { u with body = Some x }
      | BODYSTRUCTURE x -> { u with bodystructure = Some x }
      | MODSEQ x -> { u with modseq = Some x }
      | X_GM_MSGID x -> { u with x_gm_msgid = Some x }
      | X_GM_THRID x -> { u with x_gm_thrid = Some x }
      | X_GM_LABELS l -> { u with x_gm_labels = Some l }
      | BODY_SECTION _ -> u
      (* TODO *)
    in
    let u = List.fold_left aux empty a in
    let rec go : type a. a t -> a option =
     fun t ->
      match t with
      | FLAGS -> u.flags
      | UID -> u.uid
      | ENVELOPE -> u.envelope
      | INTERNALDATE -> u.internaldate
      | RFC822 -> u.rfc822
      | RFC822_HEADER -> u.rfc822_header
      | RFC822_TEXT -> u.rfc822_text
      | RFC822_SIZE -> u.rfc822_size
      | BODY -> u.body
      | BODYSTRUCTURE -> u.bodystructure
      | MODSEQ -> u.modseq
      | X_GM_MSGID -> u.x_gm_msgid
      | X_GM_THRID -> u.x_gm_thrid
      | X_GM_LABELS -> u.x_gm_labels
      | MAP (f, t) -> ( match go t with Some x -> Some (f x) | None -> None )
      | PAIR (t1, t2) -> (
          match (go t1, go t2) with
          | Some x1, Some x2 -> Some (x1, x2)
          | _ -> None )
    in
    go t
end

let fetch_gen cmd ?changed_since nums att push =
  let open Encoder in
  let attx = Fetch.encode att in
  let changed_since =
    match changed_since with
    | None -> empty
    | Some m -> p (raw " CHANGEDSINCE" ++ uint64 m ++ raw "VANISHED")
  in
  let format =
    (raw cmd ++ eset (Uint32.Set.of_list nums) ++ attx) & changed_since
  in
  let process () = function
    | FETCH (_seq, items) -> (
        match Fetch.matches att items with None -> () | Some x -> push x )
    | _ -> ()
  in
  { format; u = E ((), process, ignore) }

let fetch ?changed_since nums att push =
  fetch_gen "FETCH" ?changed_since nums att push

let uid_fetch ?changed_since nums att push =
  fetch_gen "UID FETCH" ?changed_since nums att push

type store_mode = [ `Add | `Remove | `Set ]

type _ store_kind = Flags : flag store_kind | Labels : string store_kind

let store_gen (type a) cmd ?unchanged_since mode nums (att : a store_kind)
    (l : a list) =
  let open Encoder in
  let base =
    let mode = match mode with `Add -> "+" | `Set -> "" | `Remove -> "-" in
    let name = match att with Flags -> "FLAGS" | Labels -> "X-GM-LABELS" in
    Printf.sprintf "%s%s.SILENT" mode name
  in
  let att =
    match att with Flags -> list encode_flag l | Labels -> list label l
  in
  let unchanged_since =
    match unchanged_since with
    | None -> str ""
    | Some m -> p (raw "UNCHANGEDSINCE" ++ uint64 m)
  in
  let format =
    raw cmd
    ++ eset (Uint32.Set.of_list nums)
    ++ unchanged_since ++ raw base ++ p att
  in
  simple format ()

let store ?unchanged_since mode nums att l =
  store_gen "STORE" ?unchanged_since mode nums att l

let uid_store ?unchanged_since mode nums att l =
  store_gen "UID STORE" ?unchanged_since mode nums att l

let _enable caps =
  let format = Encoder.(str "ENABLE" ++ list encode_capability caps) in
  simple format ()

module L = struct
  type state = Begin | Int of int | Cr of int | Lf of int

  let is_complete s len =
    assert (len <= Bytes.length s);
    let rec loop state i =
      if i >= len then None
      else
        match (state, Bytes.get s i) with
        | Begin, '{' -> loop (Int 0) (i + 1)
        | Int n, ('0' .. '9' as c) ->
            loop (Int ((10 * n) + Char.code c - Char.code '0')) (i + 1)
        | Int n, '}' -> loop (Cr n) (i + 1)
        | Begin, '\r' -> loop (Lf (-1)) (i + 1)
        | Cr n, '\r' -> loop (Lf n) (i + 1)
        | Lf -1, '\n' -> Some (i + 1)
        | Lf n, '\n' -> loop Begin (i + 1 + n)
        | _ -> loop Begin (i + 1)
    in
    loop Begin 0
end
