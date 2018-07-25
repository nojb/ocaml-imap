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

open Lwt.Infix

type acc =
  {
    host: string;
    port: int;
    username: string;
    password: string;
  }

type mb =
  {
    account: acc;
    mutable mailbox: string;
  }

type state =
  | CONNECTED
  | SELECTED of string
  | EXAMINED of string
  | IN_PROGRESS
  | DISCONNECTED

type conn =
  {
    c: Core.t;
    mutable s: state;
  }

module H = Hashtbl.Make (struct type t = conn let equal = (==) let hash = Hashtbl.hash end)

let waiters = Lwt_condition.create ()
let max_conns = ref 5
let conns : unit H.t = H.create !max_conns

let wrap f c =
  let s = c.s in
  c.s <- IN_PROGRESS;
  Lwt.try_bind
    (fun () -> f c.c)
    (fun x -> c.s <- s; Lwt_condition.signal waiters (); Lwt.return x)
    (fun e -> c.s <- DISCONNECTED; H.remove conns c; Lwt.fail e)

exception Found of conn

let rec use ?examine ({account = {host; port; username; password}; mailbox} as state) f =
  match
    H.iter (fun c () ->
        match c.s, examine with
        | SELECTED m, (None | Some false) when m = mailbox ->
            raise (Found c)
        | EXAMINED m, (None | Some true) when m = mailbox ->
            raise (Found c)
        | _ ->
            ()
      ) conns
  with
  | exception Found c ->
      Printf.eprintf "[Reusing connection to %s]\n%!" host;
      wrap f c
  | () ->
      begin match
        H.iter (fun c () ->
            match c.s with
            | CONNECTED | SELECTED _ | EXAMINED _ ->
                raise (Found c)
            | _ ->
                ()
          ) conns
      with
      | exception Found c ->
          Printf.eprintf "[Reusing connection to %s]\n%!" host;
          (if examine = Some true then Core.examine else Core.select) c.c mailbox >>= fun () ->
          wrap f c
      | () ->
          let t = Lwt_condition.wait waiters >>= fun () -> use ?examine state f in
          if H.length conns < !max_conns then begin
            Printf.eprintf "[Starting new connection to %s]\n%!" host;
            Lwt.on_any (Core.connect ~host ~port ~username ~password)
              (fun c -> H.add conns {c; s = CONNECTED} (); Lwt_condition.signal waiters ())
              (fun _ -> decr max_conns) (* MAYBE try connecting again? *)
          end;
          t
      end

let uid_search mb query =
  use mb (fun imap -> Core.uid_search imap query)

let uid_fetch mb uids attrs push =
  use ~examine:true mb (fun imap -> Core.uid_fetch imap uids attrs push)

let append mb ?flags ?internaldate data =
  use ~examine:false mb (fun imap -> Core.append imap mb.mailbox ?flags ?internaldate data)
