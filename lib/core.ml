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

type error =
  | Incorrect_tag of string * string
  | Decode_error of string * int
  | Unexpected_cont
  | Bad_greeting
  | Auth_error of string
  | No of string
  | Bad of string [@@deriving sexp]

exception Error of error

let () =
  Printexc.register_printer (function
      | Error (Decode_error (s, pos)) ->
          Some (Printf.sprintf "Parsing error:\n%s\n%s^\n" s (String.make pos ' '))
      | _ ->
          None
    )

open Lwt.Infix

type t =
  {
    sock: Lwt_ssl.socket;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
    mutable debug: bool;
    mutable tag: int;
    mutable stop_poll: (unit -> unit) option;
  }

let create_connection sock =
  let ic = Lwt_ssl.in_channel_of_descr sock in
  let oc = Lwt_ssl.out_channel_of_descr sock in
  {
    sock;
    ic;
    oc;
    debug = (Sys.getenv_opt "IMAPDEBUG" <> None);
    tag = 0;
    stop_poll = None;
  }

let tag {tag; _} =
  Printf.sprintf "%04d" tag

let parse ic =
  let get_line k =
    let k s = prerr_string "> "; prerr_endline s; k s in
    Lwt.on_success (Lwt_io.read_line ic) k
  in
  let get_exactly n k =
    let k s = Printf.eprintf "> [%d bytes]\n" (String.length s); k s in
    let b = Bytes.create n in
    Lwt.on_success (Lwt_io.read_into_exactly ic b 0 n) (fun () -> k (Bytes.unsafe_to_string b))
  in
  let buf = {Parser.line = ""; pos = 0; get_line; get_exactly} in
  let t, u = Lwt.wait () in
  Parser.response buf (function
      | Ok x ->
          Lwt.wakeup u x
      | Pervasives.Error (s, pos) ->
          Lwt.wakeup_exn u (Error (Decode_error (s, pos)))
    );
  t

let recv {ic; _} =
  parse ic

let rec send imap r process =
  match r with
  | Encoder.End ->
      Lwt.return_unit
  | Wait r ->
      let rec loop () =
        recv imap >>= function
        | Response.Cont _ ->
            send imap r process
        | Untagged u ->
            process u;
            loop ()
        | Tagged _ ->
            Lwt.fail (Failure "not expected")
      in
      Lwt_io.flush imap.oc >>= loop
  | Crlf r ->
      Lwt_io.write imap.oc "\r\n" >>= fun () ->
      send imap r process
  | Raw (s, r) ->
      Lwt_io.write imap.oc s >>= fun () ->
      send imap r process

let send imap r process =
  let r = r Encoder.End in
  Printf.eprintf "%s\n%!" (Sexplib.Sexp.to_string_hum (Encoder.sexp_of_s r));
  send imap r process >>= fun () ->
  Lwt_io.flush imap.oc

let wrap_process f u =
  begin match u with
  | Response.Untagged.State (NO (_, s)) ->
      raise (Error (No s))
  | State (BAD (_, s)) ->
      raise (Error (Bad s))
  | _ ->
      ()
  end;
  f u

let run imap format process =
  let process = wrap_process process in
  let tag = tag imap in
  let r = Encoder.(raw tag ++ format & crlf) in
  let rec loop res =
    recv imap >>= function
    | Response.Cont _ ->
        Lwt.fail (Failure "unexpected")
    | Untagged u ->
        begin match process u with
        | exception e ->
            Lwt.fail e
        | () ->
            loop ()
        end
    | Tagged (_, NO (_code, s)) ->
        Lwt.fail (Error (No s))
    | Tagged (_, BAD (_code, s)) ->
        Lwt.fail (Error (Bad s))
    | Tagged (_, OK _) ->
        imap.tag <- imap.tag + 1;
        Lwt.return res
  in
  send imap r process >>= loop

let run imap format res process =
  let res = ref res in
  run imap format (fun u -> res := process !res u) >>= fun () ->
  Lwt.return !res

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

let login imap username password =
  let format = Encoder.(str "LOGIN" ++ str username ++ str password) in
  let process res _ = res in
  run imap format () process

let _capability imap =
  let format = Encoder.(str "CAPABILITY") in
  let process caps = function
    | Response.Untagged.CAPABILITY caps1 -> caps @ caps1
    | _ -> caps
  in
  run imap format [] process

let create imap m =
  let format = Encoder.(str "CREATE" ++ mutf7 m) in
  run imap format () (fun r _ -> r)

let delete imap m =
  let format = Encoder.(str "DELETE" ++ mutf7 m) in
  run imap format () (fun r _ -> r)

let rename imap m1 m2 =
  let format = Encoder.(str "RENAME" ++ mutf7 m1 ++ mutf7 m2) in
  run imap format () (fun r _ -> r)

let logout imap =
  let format = Encoder.(str "LOGOUT") in
  run imap format () (fun r _ -> r)

let noop imap =
  let format = Encoder.(str "NOOP") in
  run imap format () (fun r _ -> r)

let list imap ?(ref = "") s =
  let format = Encoder.(str "LIST" ++ mutf7 ref ++ str s) in
  let process r = function
    | Response.Untagged.LIST (flags, delim, mbox) -> (flags, delim, mbox) :: r
    | _ -> List.rev r
  in
  run imap format [] process

let status imap m att =
  let format = Encoder.(str "STATUS" ++ mutf7 m ++ p (list (fun x -> x) att)) in
  let process res = function
    | Response.Untagged.STATUS (mbox, items) when m = mbox ->
        let aux res = function
          | (MESSAGES n : Status.MailboxAttribute.t) -> {res with Status.Response.messages = Some n}
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
  run imap format Status.Response.default process

let copy_gen cmd imap nums mbox =
  let format = Encoder.(raw cmd ++ eset (Uint32.Set.of_list nums) ++ mutf7 mbox) in
  run imap format () (fun r _ -> r)

let copy =
  copy_gen "COPY"

let uid_copy =
  copy_gen "UID COPY"

let _check imap =
  let format = Encoder.(str "CHECK") in
  run imap format () (fun r _ -> r)

let _close imap =
  run imap Encoder.(raw "CLOSE") () (fun r _ -> r)

let expunge imap =
  let format = Encoder.(str "EXPUNGE") in
  run imap format () (fun r _ -> r)

let uid_expunge imap nums =
  let format = Encoder.(str "UID EXPUNGE" ++ eset (Uint32.Set.of_list nums)) in
  run imap format () (fun r _ -> r)

let search_gen cmd imap sk =
  let format = Encoder.(raw cmd ++ Search.encode sk) in
  let process (res, m) = function
    | Response.Untagged.SEARCH (ids, m1) -> ids @ res, m1
    | _ -> (res, m)
  in
  run imap format ([], None) process

let search =
  search_gen "SEARCH"

let uid_search =
  search_gen "UID SEARCH"

let select_gen cmd imap m =
  let arg = if false (* List.mem Capability.CONDSTORE imap.capabilities *) then " (CONDSTORE)" else "" in
  let format = Encoder.(raw cmd ++ mutf7 m & raw arg) in
  run imap format () (fun r _ -> r)

let select =
  select_gen "SELECT"

let examine =
  select_gen "EXAMINE"

let append imap m ?flags ?internaldate data =
  let flags =
    match flags with
    | None -> Encoder.empty
    | Some l -> Encoder.(raw " " & p (list Flag.encode l))
  in
  let internaldate =
    match internaldate with
    | None -> Encoder.empty
    | Some s -> Encoder.(raw " " & str s)
  in
  let format = Encoder.(raw "APPEND" ++ mutf7 m & flags & internaldate ++ literal data) in
  let process () _ = () in
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
  let process () = function
    | Response.Untagged.FETCH (num, items) ->
        let aux res = function
          | (FLAGS flags : Fetch.MessageAttribute.t) -> {res with Fetch.Response.flags}
          | ENVELOPE e -> {res with envelope = Some e}
          | INTERNALDATE internaldate -> {res with internaldate}
          | RFC822 rfc822 -> {res with rfc822}
          | RFC822_HEADER rfc822_header -> {res with rfc822_header}
          | RFC822_TEXT rfc822_text -> {res with rfc822_text}
          | RFC822_SIZE n -> {res with rfc822_size = Some n}
          | BODY x -> {res with body = Some x}
          | BODYSTRUCTURE x -> {res with bodystructure = Some x}
          | BODY_SECTION (sec, None) -> {res with body_section = (sec, "") :: res.body_section}
          | BODY_SECTION (sec, Some s) -> {res with body_section = (sec, s) :: res.body_section}
          | UID uid -> {res with uid}
          | MODSEQ modseq -> {res with modseq}
          | X_GM_MSGID x_gm_msgid -> {res with x_gm_msgid}
          | X_GM_THRID x_gm_thrid -> {res with x_gm_thrid}
          | X_GM_LABELS x_gm_labels -> {res with x_gm_labels}
        in
        let info = List.fold_left aux Fetch.Response.default items in
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
    | `Flags flags -> list Flag.encode flags
    | `Labels labels -> list label labels
  in
  let unchanged_since =
    match unchanged_since with
    | None -> str ""
    | Some m -> p (raw "UNCHANGEDSINCE" ++ uint64 m)
  in
  let format = raw cmd ++ eset (Uint32.Set.of_list nums) ++ unchanged_since ++ raw base ++ p att in
  run imap format () (fun r _ -> r)

let store =
  store_gen "STORE"

let uid_store =
  store_gen "UID STORE"

let _enable imap caps =
  let format = Encoder.(str "ENABLE" ++ list Capability.encode caps) in
  run imap format () (fun r _ -> r)

let () =
  Ssl.init ()

let connect ~host ?(port = 993) ~username ~password =
  let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  let addr = Lwt_unix.ADDR_INET (he.Unix.h_addr_list.(0), port) in
  Lwt_unix.connect sock addr >>= fun () ->
  Lwt_ssl.ssl_connect sock ctx >>= fun sock ->
  let imap = create_connection sock in
  recv imap >>= function
  | Response.Untagged _ ->
      login imap username password >|= fun () -> imap
  | Tagged _ | Cont _ ->
      Lwt.fail (Failure "unexpected response")

let disconnect imap =
  logout imap >>= fun () -> Lwt_ssl.ssl_shutdown imap.sock
