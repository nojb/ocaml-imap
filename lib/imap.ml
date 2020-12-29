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

type _ u = E : 'b * ('b -> Response.untagged -> 'b) * ('b -> 'a) -> 'a u

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
  | Response.State { status = NO | BAD; message; _ } -> Error message
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
  let process caps = function
    | Response.CAPABILITY caps1 -> caps1 :: caps
    | _ -> caps
  in
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
    | Response.LIST (flags, delim, mbox) -> (flags, delim, mbox) :: res
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
    | Response.STATUS (mbox, items) when m = mbox ->
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

let search_gen cmd sk =
  let format = Encoder.(raw cmd ++ Search.encode sk) in
  let process res = function
    | Response.SEARCH (ids, modseq) -> (ids :: fst res, modseq)
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
    | Some l -> Encoder.(raw " " & p (list Flag.encode l))
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
    | Response.FETCH (_seq, items) -> (
        let rec choose f = function
          | [] -> None
          | x :: xs -> (
              match f x with Some _ as x -> x | None -> choose f xs )
        in
        let module FA = Fetch.MessageAttribute in
        let rec go : type a. a Fetch.t -> a option = function
          | Fetch.FLAGS ->
              choose (function FA.FLAGS l -> Some l | _ -> None) items
          | UID -> choose (function FA.UID x -> Some x | _ -> None) items
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
          | BODY -> choose (function FA.BODY x -> Some x | _ -> None) items
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
          | MAP (f, att) -> (
              match go att with Some x -> Some (f x) | None -> None )
          | PAIR (att1, att2) -> (
              match (go att1, go att2) with
              | Some x, Some y -> Some (x, y)
              | _ -> None )
        in
        match go att with Some x -> push x | None -> () )
    | _ -> ()
  in
  { format; u = E ((), process, ignore) }

let fetch ?changed_since nums att push =
  fetch_gen "FETCH" ?changed_since nums att push

let uid_fetch ?changed_since nums att push =
  fetch_gen "UID FETCH" ?changed_since nums att push

type store_mode = [ `Add | `Remove | `Set ]

type store_kind = [ `Flags of Flag.t list | `Labels of string list ]

let store_gen cmd ?unchanged_since mode nums att =
  let open Encoder in
  let base =
    let mode = match mode with `Add -> "+" | `Set -> "" | `Remove -> "-" in
    match att with
    | `Flags _ -> Printf.sprintf "%sFLAGS.SILENT" mode
    | `Labels _ -> Printf.sprintf "%sX-GM-LABELS.SILENT" mode
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
  let format =
    raw cmd
    ++ eset (Uint32.Set.of_list nums)
    ++ unchanged_since ++ raw base ++ p att
  in
  simple format ()

let store = store_gen "STORE"

let uid_store = store_gen "UID STORE"

let _enable caps =
  let format = Encoder.(str "ENABLE" ++ list Capability.encode caps) in
  simple format ()

module Parser = Parser
module Encoder = Encoder
module Response = Response

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
