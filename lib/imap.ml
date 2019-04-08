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

type error =
  | Incorrect_tag of string * string
  | Decode_error of string * int
  | Unexpected_cont
  | Bad_greeting
  | Auth_error of string
  | Server_error of string

exception Error of error

let () =
  Printexc.register_printer (function
      | Error (Decode_error (s, pos)) ->
          Some (Printf.sprintf "Parsing error:\n%s\n%s^\n" s (String.make pos ' '))
      | _ ->
          None
    )

open Lwt.Infix

let id = ref (-1)

type t =
  {
    id: int;
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
    id = (incr id; !id);
    sock;
    ic;
    oc;
    debug = (Sys.getenv_opt "IMAPDEBUG" <> None);
    tag = 0;
    stop_poll = None;
  }

let tag {tag; _} =
  Printf.sprintf "%04d" tag

let parse {id; ic; _} =
  let get_line k =
    let k s = Printf.eprintf "{%03d} > %s\n%!" id s; k s in
    Lwt.on_success (Lwt_io.read_line ic) k
  in
  let get_exactly n k =
    let k s = Printf.eprintf "{%03d} > [%d bytes]\n%!" id (String.length s); k s in
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

let rec send imap r process res =
  match r with
  | Encoder.End ->
      Lwt.return res
  | Wait r ->
      let rec loop res =
        parse imap >>= function
        | Response.Cont _ ->
            send imap r process res
        | Untagged u ->
            loop (process res u)
        | Tagged _ ->
            Lwt.fail (Failure "not expected")
      in
      Lwt_io.flush imap.oc >>= fun () -> loop res
  | Crlf r ->
      Lwt_io.write imap.oc "\r\n" >>= fun () ->
      send imap r process res
  | Raw (s, r) ->
      Lwt_io.write imap.oc s >>= fun () ->
      send imap r process res

let send imap r process res =
  let r = r Encoder.End in
  (* Printf.eprintf "%s\n%!" (Sexplib.Sexp.to_string_hum (Encoder.sexp_of_s r)); *)
  send imap r process res >>= fun res ->
  Lwt_io.flush imap.oc >>= fun () ->
  Lwt.return res

let wrap_process f res = function
  | Response.Untagged.State (NO (_, s) | BAD (_, s)) ->
      raise (Error (Server_error s))
  | u ->
      f res u

type ('a, 'b) cmd =
  {
    format: Encoder.t;
    default: 'a;
    process: 'a -> Response.Untagged.t -> 'a;
    finish: 'a -> 'b;
  }

let run imap {format; default; process; finish} =
  let process = wrap_process process in
  let tag = tag imap in
  let r = Encoder.(raw tag ++ format & crlf) in
  let rec loop res =
    parse imap >>= function
    | Response.Cont _ ->
        Lwt.fail_with "unexpected"
    | Untagged u ->
        loop (process res u)
    | Tagged (_, (NO (_code, s) | BAD (_code, s))) ->
        Lwt.fail (Error (Server_error s))
    | Tagged (_, OK _) ->
        imap.tag <- imap.tag + 1;
        Lwt.return res
  in
  send imap r process default >>= loop >|= finish

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

let simple format default =
  let process res _ = res in
  let finish res = res in
  {format; process; default; finish}

let login username password =
  let format = Encoder.(str "LOGIN" ++ str username ++ str password) in
  simple format ()

let _capability =
  let format = Encoder.(str "CAPABILITY") in
  let process caps = function
    | Response.Untagged.CAPABILITY caps1 ->
        caps1 :: caps
    | _ ->
        caps
  in
  let finish l = List.rev l |> List.flatten in
  {format; process; default = []; finish}

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
    | Response.Untagged.LIST (flags, delim, mbox) -> (flags, delim, mbox) :: res
    | _ -> res
  in
  let finish = List.rev in
  {format; process; default = []; finish}

module Status_response = struct
  type t =
    {
      messages: int option;
      recent: int option;
      uidnext: Common.uid option;
      uidvalidity: Common.uid option;
      unseen: int option;
      highestmodseq: Common.modseq option;
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
    | Response.Untagged.STATUS (mbox, items) when m = mbox ->
        let aux res = function
          | (MESSAGES n : Status.MailboxAttribute.t) -> {res with Status_response.messages = Some n}
          | RECENT n -> {res with recent = Some n}
          | UIDNEXT n -> {res with uidnext = Some n}
          | UIDVALIDITY n -> {res with uidvalidity = Some n}
          | UNSEEN n -> {res with unseen = Some n}
          | HIGHESTMODSEQ n -> {res with highestmodseq = Some n}
        in
        let r = List.fold_left aux Status_response.default items in
        let rec go : type a. a Status.t -> a option = function
          | Status.MESSAGES -> r.Status_response.messages
          | RECENT -> r.recent
          | UIDNEXT -> r.uidnext
          | UIDVALIDITY -> r.uidvalidity
          | UNSEEN -> r.unseen
          | HIGHESTMODSEQ -> r.highestmodseq
          | PAIR (x, y) ->
              begin match go x, go y with
              | Some x, Some y -> Some (x, y)
              | _ -> None
              end
          | MAP (f, x) ->
              begin match go x with
              | Some x -> Some (f x)
              | None -> None
              end
        in
        go att
    | _ ->
        res
  in
  let finish res = res in
  {format; process; default = None; finish}

let copy_gen cmd nums mbox =
  let format = Encoder.(raw cmd ++ eset (Uint32.Set.of_list nums) ++ mutf7 mbox) in
  simple format ()

let copy =
  copy_gen "COPY"

let uid_copy =
  copy_gen "UID COPY"

let _check =
  let format = Encoder.(str "CHECK") in
  simple format ()

let _close =
  simple Encoder.(raw "CLOSE") ()

let expunge =
  let format = Encoder.(str "EXPUNGE") in
  simple format ()

let uid_expunge nums =
  let format = Encoder.(str "UID EXPUNGE" ++ eset (Uint32.Set.of_list nums)) in
  simple format ()

let search_gen cmd sk =
  let format = Encoder.(raw cmd ++ Search.encode sk) in
  let process res = function
    | Response.Untagged.SEARCH (ids, modseq) ->
        (ids :: fst res, modseq)
    | _ ->
        res
  in
  let finish (ids, modseq) = (List.(rev ids |> flatten), modseq) in
  {format; process; default = ([], None); finish}

let search =
  search_gen "SEARCH"

let uid_search =
  search_gen "UID SEARCH"

let select_gen cmd m =
  let arg = if false (* List.mem Capability.CONDSTORE imap.capabilities *) then " (CONDSTORE)" else "" in
  let format = Encoder.(raw cmd ++ mutf7 m & raw arg) in
  simple format ()

let select =
  select_gen "SELECT"

let examine =
  select_gen "EXAMINE"

let append m ?flags ?internaldate data =
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
  simple format ()

module Int32Map = Map.Make (Int32)

let fetch_gen cmd ?changed_since nums att push =
  let open Encoder in
  let attx = Fetch.encode att in
  let changed_since =
    match changed_since with
    | None -> empty
    | Some m -> p (raw " CHANGEDSINCE" ++ uint64 m ++ raw "VANISHED")
  in
  let format = raw cmd ++ eset (Uint32.Set.of_list nums) ++ attx & changed_since in
  let process () = function
    | Response.Untagged.FETCH (_seq, items) ->
        let rec choose f = function [] -> None | x :: xs -> begin match f x with Some _ as x -> x | None -> choose f xs end in
        let module FA = Fetch.MessageAttribute in
        let rec go : type a. a Fetch.t -> a option = function
          | Fetch.FLAGS ->
              choose (function FA.FLAGS l -> Some l | _ -> None) items
          | UID ->
              choose (function FA.UID x -> Some x | _ -> None) items
          | ENVELOPE ->
              choose (function FA.ENVELOPE e -> Some e | _ -> None) items
          | INTERNALDATE ->
              choose (function FA.INTERNALDATE s -> Some s | _ -> None) items
          | RFC822 ->
              choose (function FA.RFC822 x -> Some x | _ -> None) items
          | RFC822_HEADER ->
              choose (function FA.RFC822_HEADER x -> Some x | _ -> None) items
          | RFC822_TEXT ->
              choose (function FA.RFC822_TEXT x -> Some x | _ -> None) items
          | RFC822_SIZE ->
              choose (function FA.RFC822_SIZE n -> Some n | _ -> None) items
          | BODY ->
              choose (function FA.BODY x -> Some x | _ -> None) items
          | BODYSTRUCTURE ->
              choose (function FA.BODYSTRUCTURE x -> Some x | _ -> None) items
          | MODSEQ ->
              choose (function FA.MODSEQ n -> Some n | _ -> None) items
          | X_GM_MSGID ->
              choose (function FA.X_GM_MSGID l -> Some l | _ -> None) items
          | X_GM_THRID ->
              choose (function FA.X_GM_THRID l -> Some l | _ -> None) items
          | X_GM_LABELS ->
              choose (function FA.X_GM_LABELS l -> Some l | _ -> None) items
          | MAP (f, att) ->
              begin match go att with Some x -> Some (f x) | None -> None end
          | PAIR (att1, att2) ->
              begin match go att1, go att2 with Some x, Some y -> Some (x, y) | _ -> None end
        in
        begin match go att with Some x -> push x | None -> () end
    | _ ->
        ()
  in
  {format; process; default = (); finish = ignore}

let fetch ?changed_since nums att push =
  fetch_gen "FETCH" ?changed_since nums att push

let uid_fetch ?changed_since nums att push =
  fetch_gen "UID FETCH" ?changed_since nums att push

type store_mode =
  [ `Add
  | `Remove
  | `Set ]

type store_kind =
  [ `Flags of Flag.t list
  | `Labels of string list ]

let store_gen cmd ?unchanged_since mode nums att =
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
  simple format ()

let store =
  store_gen "STORE"

let uid_store =
  store_gen "UID STORE"

let _enable caps =
  let format = Encoder.(str "ENABLE" ++ list Capability.encode caps) in
  simple format ()

let () =
  Ssl.init ()

let connect ~host ~port ~username ~password =
  let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  let addr = Lwt_unix.ADDR_INET (he.Unix.h_addr_list.(0), port) in
  Lwt_unix.connect sock addr >>= fun () ->
  Lwt_ssl.ssl_connect sock ctx >>= fun sock ->
  let imap = create_connection sock in
  parse imap >>= function
  | Response.Untagged _ ->
      run imap (login username password) >|= fun () -> imap
  | Tagged _ | Cont _ ->
      Lwt.fail_with "unexpected response"

let disconnect imap =
  run imap logout >>= fun () -> Lwt_ssl.ssl_shutdown imap.sock
