(* The MIT License (MIT)

   Copyright (c) 2015-2017 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

module Flag = Flag

include Response

module Fetch = struct
  type section_msgtext = Response.section_msgtext =
    | HEADER
    | HEADER_FIELDS of string list
    | HEADER_FIELDS_NOT of string list
    | TEXT
    | MIME

  type section =
    int list * section_msgtext option

  open Encoder

  type t = rope

  let section_msgtext = function
    | HEADER -> raw "HEADER"
    | HEADER_FIELDS l -> raw "HEADER.FIELDS" ++ plist str l
    | HEADER_FIELDS_NOT l -> raw "HEADER.FIELDS.NOT" ++ plist str l
    | TEXT -> raw "TEXT"
    | MIME -> raw "MIME"

  let section_enc (nl, sec) =
    let sec = match sec with None -> empty | Some sec -> section_msgtext sec in
    match nl with
    | [] ->
        sec
    | _ :: _ ->
        list ~sep:'.' int nl & raw "." & sec

  let header ?(part = []) () = part, Some HEADER
  let header_fields ?(part = []) l = part, Some (HEADER_FIELDS l)
  let header_fields_not ?(part = []) l = part, Some (HEADER_FIELDS_NOT l)
  let text ?(part = []) () = part, Some TEXT
  let part ~part () = part, None
  let mime ~part () = part, Some MIME

  let envelope = raw "ENVELOPE"
  let internaldate = raw "INTERNALDATE"
  let rfc822_size = raw "RFC822.SIZE"
  let body = raw "BODY"
  let body_section ?(peek = true) ?section:(sec = [], None) () =
    raw (if peek then "BODY.PEEK" else "BODY") & raw "[" & section_enc sec & raw "]"
  let bodystructure = raw "BODYSTRUCTURE"
  let uid = raw "UID"
  let flags = raw "FLAGS"

  let all = [flags; internaldate; rfc822_size; envelope]
  let fast = [flags; internaldate; rfc822_size]
  let full = [flags; internaldate; rfc822_size; envelope; body]

  let x_gm_msgid = raw "X-GM-MSGID"
  let x_gm_thrid = raw "X-GM-THRID"
  let x_gm_labels = raw "X-GM-LABELS"

  type response =
    {
      flags: Flag.t list option;
      envelope: Response.envelope option;
      internaldate: (Response.date * Response.time) option;
      rfc822_size: int option;
      body: MIME.mime option;
      bodystructure: MIME.mime option;
      body_section: (section * string option) list;
      uid: Response.uid option;
      modseq: Response.modseq option;
      x_gm_msgid: Response.modseq option;
      x_gm_thrid: Response.modseq option;
      x_gm_labels: string list option;
    }

  let default =
    {
      flags = None;
      envelope = None;
      internaldate = None;
      rfc822_size = None;
      body = None;
      bodystructure = None;
      body_section = [];
      uid = None;
      modseq = None;
      x_gm_msgid = None;
      x_gm_thrid = None;
      x_gm_labels = None;
    }
end

module Status = struct
  open Encoder

  type t = rope

  let messages = raw "MESSAGES"

  let recent = raw "RECENT"

  let uidnext = raw "UIDNEXT"

  let uidvalidity = raw "UIDVALIDITY"

  let unseen = raw "UNSEEN"

  let highestmodseq = raw "HIGHESTMODSEQ"

  type response =
    {
      messages: int option;
      recent: int option;
      uidnext: Response.uid option;
      uidvalidity: Response.uid option;
      unseen: int option;
      highestmodseq: Response.modseq option;
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

module Search = struct
  open Encoder

  type key = rope

  let all = raw "ALL"
  let seq s = eset (Uint32.Set.of_list s)
  let answered = raw "ANSWERED"
  let bcc s = raw "BCC" ++ str s
  let before t = raw "BEFORE" ++ date t
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
  let on t = raw "ON" ++ date t
  let (||) k1 k2 = raw "OR" ++ p k1 ++ p k2
  let recent = raw "RECENT"
  let seen = raw "SEEN"
  let sent_before t = raw "SENTBEFORE" ++ date t
  let sent_on t = raw "SENTON" ++ date t
  let sent_since t = raw "SENTSINCE" ++ date t
  let since t = raw "SINCE" ++ date t
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
  let (&&) k1 k2 = p k1 ++ p k2
  let modseq n = raw "MODSEQ" ++ uint64 n
  let x_gm_raw s = raw "X-GM-RAW" ++ str s
  let x_gm_msgid n = raw "X-GM-MSGID" ++ uint64 n
  let x_gm_thrid n = raw "X-GM-THRID" ++ uint64 n
  let x_gm_labels l = raw "X-GM-LABELS" ++ list str l
end

module A = Angstrom.Buffered
module R = Response

type error =
  | Incorrect_tag of string * string
  | Decode_error of string * string list * string
  | Unexpected_cont
  | Bad_greeting
  | Auth_error of string
  | No of string
  | Bad of string

exception Error of error

open Lwt.Infix

type t =
  {
    sock: Lwt_ssl.socket;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;

    mutable debug: bool;

    mutable tag: int;
    mutable unconsumed: A.unconsumed;

    mutable uidnext: Response.uid option;
    mutable messages: int option;
    mutable recent: int option;
    mutable unseen: int option;
    mutable uidvalidity: Response.uid option;
    mutable highestmodseq: Response.modseq option;

    mutable capabilities: Capability.t list;

    mutable stop_poll: (unit -> unit) option;
  }

let create_connection sock unconsumed =
  let ic = Lwt_ssl.in_channel_of_descr sock in
  let oc = Lwt_ssl.out_channel_of_descr sock in
  {
    sock;
    ic;
    oc;

    debug = true;

    tag = 0;
    unconsumed;
    uidnext = None;
    uidvalidity = None;
    recent = None;
    messages = None;
    unseen = None;
    highestmodseq = None;
    capabilities = [];
    stop_poll = None;
  }

let tag {tag; _} =
  Printf.sprintf "%04d" tag

let uidnext {uidnext; _} =
  uidnext

let messages {messages; _} =
  messages

let recent {recent; _} =
  recent

let unseen {unseen; _} =
  unseen

let uidvalidity {uidvalidity; _} =
  uidvalidity

let highestmodseq {highestmodseq; _} =
  highestmodseq

let unconsumed_to_string {A.buf; off; len} =
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set b i (Bigarray.Array1.get buf (off + i))
  done;
  Bytes.unsafe_to_string b

let parse unconsumed p =
  let input = `String (unconsumed_to_string unconsumed) in
  A.feed (A.parse p) input

let recv imap =
  let rec loop = function
    | A.Partial f ->
        Lwt_io.read_line imap.ic >>= fun line ->
        (if imap.debug then Lwt_io.eprintlf "> %s" line else Lwt.return_unit) >>= fun () ->
        loop (f (`String (line ^ "\r\n")))
    | Done (unconsumed, r) ->
        imap.unconsumed <- unconsumed;
        (* begin if imap.debug then *)
        (*   Lwt_io.eprintl (Sexplib.Sexp.to_string_hum (R.sexp_of_response r)) *)
        (* else *)
          (* Lwt.return_unit end >>= fun () -> *)
        Lwt.return r
    | Fail (unconsumed, backtrace, f) ->
        Lwt_io.eprintlf "** Parse error at `%s':" f >>= fun () ->
        Lwt_list.iter_s Lwt_io.eprintl backtrace >>= fun () ->
        Lwt_io.eprintlf "** Unconsumed: %S" (unconsumed_to_string unconsumed) >>= fun () ->
        Lwt.fail (Error (Decode_error (unconsumed_to_string unconsumed, backtrace, f)))
  in
  loop (parse imap.unconsumed Decoder.response)

let rec send imap r process res =
  match r with
  | Encoder.Cat (r1, r2) ->
      send imap r1 process res >>= fun res ->
      send imap r2 process res
  | Literal s ->
      if List.mem Capability.LITERALPLUS imap.capabilities ||
         (List.mem Capability.LITERALMINUS imap.capabilities && String.length s <= 4096)
      then
        Lwt_io.fprintf imap.oc "{%d+}\r\n" (String.length s) >>= fun () ->
        Lwt_io.write imap.oc s >>= fun () ->
        Lwt.return res
      else
        let rec loop res =
          recv imap >>= function
          | R.Cont _ ->
              Lwt.return res
          | R.Untagged u ->
              loop (process imap res u)
          | R.Tagged _ ->
              Lwt.fail (Failure "not expected")
        in
        Lwt_io.fprintf imap.oc "{%d}\r\n" (String.length s) >>= fun () ->
        Lwt_io.flush imap.oc >>= fun () ->
        loop res
  | Raw s ->
      Lwt_io.write imap.oc s >>= fun () ->
      Lwt.return res

let send imap r process res =
  (* begin if imap.debug then *)
  (*   Lwt_io.eprintl (Sexplib.Sexp.to_string_hum (Encoder.sexp_of_rope r)) *)
  (* else *)
  (*   Lwt.return_unit end >>= fun () -> *)
  send imap r process res >>= fun res ->
  Lwt_io.flush imap.oc >>= fun () ->
  Lwt.return res

let wrap_process f imap res u =
  begin match u with
  | R.State (NO (_, s)) ->
      raise (Error (No s))
  | State (BAD (_, s)) ->
      raise (Error (Bad s))
  | State (OK (Some (UIDNEXT n), _)) ->
      imap.uidnext <- Some n
  | State (OK (Some (UIDVALIDITY n), _)) ->
      imap.uidvalidity <- Some n;
  | State (OK (Some (CAPABILITY caps), _)) ->
      imap.capabilities <- caps
  | State (OK (Some (HIGHESTMODSEQ n), _)) ->
      imap.highestmodseq <- Some n
  | RECENT n ->
      imap.recent <- Some n
  | EXISTS n ->
      imap.messages <- Some n
  | STATUS (_, items) ->
      List.iter (function
          | R.MESSAGES n ->
              imap.messages <- Some n
          | RECENT n ->
              imap.recent <- Some n
          | UIDNEXT n ->
              imap.uidnext <- Some n
          | UIDVALIDITY n ->
              imap.uidvalidity <- Some n
          | UNSEEN n ->
              imap.unseen <- Some n
          | HIGHESTMODSEQ n ->
              imap.highestmodseq <- Some n
        ) items
  | CAPABILITY caps ->
      imap.capabilities <- caps
  | _ ->
      ()
  end;
  f imap res u

let run imap format res process =
  let process = wrap_process process in
  let tag = tag imap in
  let r = Encoder.(raw tag ++ format & raw "\r\n") in
  send imap r process res >>= fun res ->
  let rec loop res =
    recv imap >>= function
    | R.Cont _ ->
        Lwt.fail (Failure "unexpected")
    | Untagged u ->
        begin match process imap res u with
        | exception e ->
            Lwt.fail e
        | res ->
            loop res
        end
    | Tagged _ ->
        imap.tag <- imap.tag + 1;
        Lwt.return res
  in
  loop res

let idle imap =
  let process = wrap_process (fun _ r _ -> r) in
  let tag = tag imap in
  let r = Encoder.(raw tag ++ raw "IDLE\r\n") in
  let t, u = Lwt.wait () in
  let t = t >>= fun () -> send imap Encoder.(raw "DONE\r\n") process () in
  let stop () = imap.stop_poll <- None; Lwt.wakeup u () in
  send imap r process () >>= fun () ->
  let rec loop () =
    recv imap >>= function
    | R.Cont _ ->
        imap.stop_poll <- Some stop;
        loop ()
    | R.Untagged _ ->
        stop ();
        loop ()
    | Tagged (_t, _) -> (* FIXME *)
        imap.tag <- imap.tag + 1;
        Lwt.return_unit
  in
  Lwt.join [loop (); t]

let stop_poll imap =
  match imap.stop_poll with
  | Some f -> f ()
  | None -> ()

let poll imap =
  if List.mem Capability.IDLE imap.capabilities then
    idle imap
  else
    Lwt.fail (Failure "IDLE not supported")

let login imap username password =
  let format = Encoder.(str "LOGIN" ++ str username ++ str password) in
  let process _ res _ = res in
  run imap format () process

let _capability imap =
  let format = Encoder.(str "CAPABILITY") in
  let process _ caps = function
    | R.CAPABILITY caps1 -> caps @ caps1
    | _ -> caps
  in
  run imap format [] process

let create imap m =
  let format = Encoder.(str "CREATE" ++ mutf7 m) in
  run imap format () (fun _ r _ -> r)

let delete imap m =
  let format = Encoder.(str "DELETE" ++ mutf7 m) in
  run imap format () (fun _ r _ -> r)

let rename imap m1 m2 =
  let format = Encoder.(str "RENAME" ++ mutf7 m1 ++ mutf7 m2) in
  run imap format () (fun _ r _ -> r)

let logout imap =
  let format = Encoder.(str "LOGOUT") in
  run imap format () (fun _ r _ -> r)

let noop imap =
  let format = Encoder.(str "NOOP") in
  run imap format () (fun _ r _ -> r)

let list imap ?(ref = "") s =
  let format = Encoder.(str "LIST" ++ mutf7 ref ++ str s) in
  let process _ r = function
    | R.LIST (flags, delim, mbox) -> (flags, delim, mbox) :: r
    | _ -> List.rev r
  in
  run imap format [] process

let status imap m att =
  let format = Encoder.(str "STATUS" ++ mutf7 m ++ p (list (fun x -> x) att)) in
  let process _ res = function
    | R.STATUS (mbox, items) when m = mbox ->
        let aux res = function
          | (MESSAGES n : R.mbx_att) -> {res with Status.messages = Some n}
          | RECENT n -> {res with recent = Some n}
          | UIDNEXT n -> {res with uidnext = Some n}
          | UIDVALIDITY n -> {res with uidvalidity = Some n}
          | UNSEEN n -> {res with unseen = Some n}
          | HIGHESTMODSEQ n -> {res with highestmodseq = Some n}
        in
        List.fold_left aux res items
    | _ ->
        res
  in
  run imap format Status.default process

let copy_gen cmd imap nums mbox =
  let format = Encoder.(raw cmd ++ eset (Uint32.Set.of_list nums) ++ mutf7 mbox) in
  run imap format () (fun _ r _ -> r)

let copy =
  copy_gen "COPY"

let uid_copy =
  copy_gen "UID COPY"

let _check imap =
  let format = Encoder.(str "CHECK") in
  run imap format () (fun _ r _ -> r)

let _close imap =
  run imap Encoder.(raw "CLOSE") () (fun _ r _ -> r)

let expunge imap =
  let format = Encoder.(str "EXPUNGE") in
  run imap format () (fun _ r _ -> r)

let uid_expunge imap nums =
  let format = Encoder.(str "UID EXPUNGE" ++ eset (Uint32.Set.of_list nums)) in
  run imap format () (fun _ r _ -> r)

let search_gen cmd imap sk =
  let format = Encoder.(raw cmd ++ sk) in
  let process _ (res, m) = function
    | R.SEARCH (ids, m1) -> ids @ res, m1
    | _ -> (res, m)
  in
  run imap format ([], None) process

let search =
  search_gen "SEARCH"

let uid_search =
  search_gen "UID SEARCH"

let select imap ?(read_only = false) m =
  let cmd = if read_only then "EXAMINE" else "SELECT" in
  let arg = if List.mem Capability.CONDSTORE imap.capabilities then " (CONDSTORE)" else "" in
  let format = Encoder.(raw cmd ++ mutf7 m & raw arg) in
  run imap format () (fun _ r _ -> r)

let append imap m ?(flags = []) data =
  let format = Encoder.(raw "APPEND" ++ mutf7 m ++ p (list flag flags) ++ literal data) in
  let process _ () _ = () in
  run imap format () process

module Int32Map = Map.Make (Int32)

let fetch_gen cmd imap ?changed_since nums att =
  let open Encoder in
  let att =
    match att with
    (* | `Fast -> raw "FAST" *)
    (* | `Full -> raw "FULL" *)
    (* | `All -> raw "ALL" *)
    | [x] -> x
    | xs -> p (list (fun x -> x) xs)
  in
  let changed_since =
    match changed_since with
    | None -> empty
    | Some m -> p (raw " CHANGEDSINCE" ++ uint64 m ++ raw "VANISHED")
  in
  let format = raw cmd ++ eset (Uint32.Set.of_list nums) ++ att & changed_since in
  let strm, push = Lwt_stream.create () in
  let process _ () = function
    | R.FETCH (num, items) ->
        let aux res = function
          | (Response.FLAGS l : R.msg_att) -> {res with Fetch.flags = Some l}
          | ENVELOPE e -> {res with envelope = Some e}
          | INTERNALDATE (d, t) -> {res with internaldate = Some (d, t)}
          (* | RFC822 (Some s) -> {res with rfc822 = Some s} *)
          (* | RFC822 None -> {res with rfc822 = Some ""} *)
          (* | RFC822_HEADER (Some s) -> {res with rfc822_header = Some s} *)
          (* | RFC822_HEADER None -> {res with rfc822_header = Some ""} *)
          (* | RFC822_TEXT (Some s) -> {res with rfc822_text = Some s} *)
          (* | RFC822_TEXT None -> {res with rfc822_text = Some ""} *)
          | RFC822_SIZE n -> {res with rfc822_size = Some n}
          | BODY x -> {res with body = Some x}
          | BODYSTRUCTURE x -> {res with bodystructure = Some x}
          | BODY_SECTION (sec, s) -> {res with body_section = (sec, s) :: res.body_section}
          | UID n -> {res with uid = Some n}
          | MODSEQ n -> {res with modseq = Some n}
          | X_GM_MSGID n -> {res with x_gm_msgid = Some n}
          | X_GM_THRID n -> {res with x_gm_thrid = Some n}
          | X_GM_LABELS n -> {res with x_gm_labels = Some n}
        in
        let info = List.fold_left aux Fetch.default items in
        push (Some (num, info))
    | _ ->
        ()
  in
  Lwt.ignore_result (run imap format () process >|= fun () -> push None);
  strm

let fetch =
  fetch_gen "FETCH"

let uid_fetch =
  fetch_gen "UID FETCH"

type store_mode =
  [ `Add
  | `Remove
  | `Set ]

type store_kind =
  [ `Flags of Flag.t list
  | `Labels of string list ]

let store_gen cmd imap ?unchanged_since mode nums att =
  let open Encoder in
  let base =
    let mode = match mode with `Add -> "+" | `Set -> "" | `Remove -> "-" in
    match att with
    | `Flags _ ->
        Printf.sprintf "%sFLAGS.SILENT" mode
    | `Labels _ ->
        Printf.sprintf "%sX-GM-LABELS.SILENT" mode
  in
  let att =
    match att with
    | `Flags flags -> list flag flags
    | `Labels labels -> list label labels
  in
  let unchanged_since =
    match unchanged_since with
    | None -> str ""
    | Some m -> p (raw "UNCHANGEDSINCE" ++ uint64 m)
  in
  let format = raw cmd ++ eset (Uint32.Set.of_list nums) ++ unchanged_since ++ raw base ++ p att in
  run imap format () (fun _ r _ -> r)

let store =
  store_gen "STORE"

let uid_store =
  store_gen "UID STORE"

let _enable imap caps =
  let format = Encoder.(str "ENABLE" ++ list capability caps) in
  run imap format () (fun _ r _ -> r)

let () =
  Ssl.init ()

let connect server ?(port = 993) username password ?read_only mailbox =
  let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname server >>= fun he ->
  let addr = Lwt_unix.ADDR_INET (he.Unix.h_addr_list.(0), port) in
  Lwt_unix.connect sock addr >>= fun () ->
  Lwt_ssl.ssl_connect sock ctx >>= fun sock ->
  let imap =
    create_connection sock {A.buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0; off = 0; len = 0}
  in
  recv imap >>= function
  | R.Untagged _ ->
      login imap username password >>= fun () ->
      select imap ?read_only mailbox >>= fun () ->
      Lwt.return imap
  | Tagged _ | Cont _ ->
      Lwt.fail (Failure "unexpected response")

let disconnect imap =
  logout imap >>= fun () -> Lwt_ssl.ssl_shutdown imap.sock
