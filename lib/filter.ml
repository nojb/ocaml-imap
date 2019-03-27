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

(* module Options = struct *)
(*   type t = *)
(*     { *)
(*       max_connections_by_host: int; *)
(*       total_max_connections: int; *)
(*     } *)
(* end *)

type account =
  {
    username: string;
    password: string;
    host: string;
    port: int;
  }

type mailbox =
  {
    account: account;
    mutable name: string;
  }

module MailboxMap = Map.Make (struct type t = mailbox let compare = Stdlib.compare end)

module Pool = struct
  let mutex = Lwt_mutex.create ()

  let connect_ {username; password; host; port} f =
    Lwt_mutex.with_lock mutex (fun () ->
        Core.connect ~host ~port ~username ~password >>= fun conn ->
        f conn >>= fun r ->
        Core.disconnect conn >>= fun () ->
        Lwt.return r
      )

  let connect mb f =
    connect_ mb.account (fun conn ->
        Core.select conn mb.name >>= fun () -> f conn
      )
end

module Message = struct
  type t =
    {
      mailbox: mailbox;
      uid: int32;
      labels: string list option ref;
    }

  let uid {uid; _} = uid

  let fetch_labels {mailbox; uid; labels} =
    match !labels with
    | None ->
        Pool.connect mailbox (fun conn ->
            let t, u = Lwt.wait () in
            Core.uid_fetch conn [uid] [Fetch.Request.x_gm_labels]
              (fun {Fetch.Response.x_gm_labels; _} ->
                 labels := Some x_gm_labels;
                 Lwt.wakeup u x_gm_labels
              ) >>= fun () -> t
          )
    | Some labels ->
        Lwt.return labels
end

module Message_set = struct
  type t = Search.key MailboxMap.t

  let create mb q =
    MailboxMap.singleton mb q

  let perform ms =
    let perform (mb, q) =
      let perform conn = Core.uid_search conn q >|= Stdlib.fst in
      Pool.connect mb perform >|= fun uids -> (mb, uids)
    in
    Lwt_list.map_p perform (MailboxMap.bindings ms)

  let count ms =
    perform ms >|=
    List.fold_left (fun acc l -> acc + List.length (Stdlib.snd l)) 0

  let uids ms =
    perform ms >|= fun l ->
    List.flatten
      (List.map (fun (mailbox, uids) ->
           List.map (fun uid -> {Message.mailbox; uid; labels = ref None}) uids
         ) l)

  let unseen ms =
    MailboxMap.map (fun q -> Search.(q && unseen)) ms

  let answered ms =
    MailboxMap.map (fun q -> Search.(q && answered)) ms

  let new_ ms =
    MailboxMap.map (fun q -> Search.(q && new_)) ms

  let store_flags ms mode kind =
    perform ms >>= fun l ->
    Lwt_list.iter_p (fun (mb, uids) ->
        Pool.connect mb (fun conn ->
            Core.uid_store conn mode uids kind
          )
      ) l

  let add_labels ms labels =
    store_flags ms `Add (`Labels labels)

  let remove_labels ms labels =
    store_flags ms `Remove (`Labels labels)

  let set_labels ms labels =
    store_flags ms `Set (`Labels labels)

  let add_flags ms flags =
    store_flags ms `Add (`Flags flags)

  let remove_flags ms flags =
    store_flags ms `Remove (`Flags flags)

  let set_flags ms flags =
    store_flags ms `Set (`Flags flags)

  let fetch_labels ms =
    let stream, push = Lwt_stream.create () in
    let th =
      perform ms >|= fun l ->
      Lwt_list.iter_p (fun (mailbox, uids) ->
          Pool.connect mailbox (fun conn ->
              Core.uid_fetch conn uids [Fetch.Request.x_gm_labels]
                (fun {Fetch.Response.x_gm_labels; _} ->
                   List.iter (fun uid ->
                       push (Some {Message.mailbox; uid; labels = ref (Some x_gm_labels)})
                     ) uids
                )
            )
        ) l >|= fun () -> push None
    in
    Lwt.ignore_result th;
    stream

  let mark_seen ms =
    add_flags ms [Flag.Seen]

  let mark_unseen ms =
    remove_flags ms [Flag.Seen]

  let delete ms =
    perform ms >>= fun l ->
    Lwt_list.iter_p
      (fun (mb, uids) ->
         Pool.connect mb
           (fun conn -> Core.uid_store conn `Add uids (`Flags [Flag.Deleted]))
      ) l >>= fun () ->
    Lwt_list.iter_p
      (fun (mb, uids) ->
         Pool.connect mb (fun conn -> Core.uid_expunge conn uids)
      ) l

  let union ms1 ms2 =
    MailboxMap.union (fun _ q1 q2 -> Some Search.(q1 || q2)) ms1 ms2

  let inter ms1 ms2 =
    MailboxMap.merge (fun _ q1 q2 ->
        match q1, q2 with
        | None, _ | _, None -> None
        | Some q1, Some q2 -> Some Search.(q1 && q2)
      ) ms1 ms2
end

module Mailbox = struct
  type t = mailbox

  let create account name =
    {account; name}

  module Map = MailboxMap

  (* let account mb = *)
  (*   mb.account *)

  let name mb =
    mb.name

  let all mb =
    Message_set.create mb Search.all

  let rename mb name =
    Pool.connect mb (fun conn -> Core.rename conn mb.name name) >|= fun () ->
    mb.name <- name

  let delete mb =
    Pool.connect mb (fun conn -> Core.delete conn mb.name)
end

module Account = struct
  type t = account

  let create ~username ~password ~host ~port () =
    {username; password; host; port}

  let inbox account =
    Mailbox.create account "INBOX"

  let select account name =
    Mailbox.create account name

  let all account =
    Pool.connect_ account (fun conn ->
        Core.list conn "%" >|= List.map (fun (_, _, name) -> Mailbox.create account name)
      )
end
