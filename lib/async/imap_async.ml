(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

open Core.Std
open Async.Std

module Ivar = Async_kernel.Raw_ivar
                
module Async_io = struct
  type 'a t = ('a, exn) Result.t Deferred.t
  type 'a t_repr = ('a, exn) Result.t Ivar.t
  external thread : 'a t_repr -> 'a t = "%identity"
  external thread_repr : 'a t -> 'a t_repr = "%identity"
    
  let fail exn =
    let ivar = Ivar.create_full (Error exn) in
    thread ivar

  let bind t' f =
    let t = thread_repr t' in
    match Ivar.peek t with
    | Some (Ok e)      -> f e
    | Some (Error exn) -> fail exn
    | None -> (* Sleep *)
      let bind_result = Ivar.create () in
      Ivar.upon t (
        function
        | Ok e ->
          let res = thread_repr (f e) in
          (try Ivar.connect ~bind_result ~bind_rhs:res (* TODO: monitor *)
           with e -> Exn.reraise e "Lwt.bind")
        | Error e ->
          (* [e] could be [Canceled] and [t] could have been canceled through [ret] *)
          if Ivar.is_empty bind_result then
            Ivar.fill bind_result (Error e)
      ) ;
      thread bind_result
  
  let return x =
    let ivar = Ivar.create_full (Ok x) in
    thread ivar

  let catch t_susp handler =
    let t = thread_repr (try t_susp () with e -> fail e) in
    match Ivar.peek t with
    | Some (Ok _)    -> thread t
    | Some (Error e) -> handler e
    | None ->
      let ivar = Ivar.create () in
      Ivar.upon t (fun result ->
          match Ivar.peek ivar with
          | None ->
            begin match result with
              | Ok _ as res -> Ivar.fill ivar res
              | Error exn ->
                let res =
                  thread_repr (
                    try handler exn
                    with exn -> fail exn
                  )
                in
                try Ivar.connect ~bind_result:ivar ~bind_rhs:res
                with e -> Exn.reraise e "Lwt.catch"
            end
          | _ ->
            (* At this point [ivar] is already filled, and it can only be filled by
               [Error Canceled].
               To match Lwt's semantic we would like to give [Canceled] to [handler]
               and then replace the content of the ivar.
               But we cannot replace the content of a filled ivar, also the other
               handlers might already have been run so there is no point. *)
            ()
        ) ;
      thread ivar

  (* let (>>=) = bind *)

  type mutex = unit
  let create_mutex () = ()
  let is_locked _ = false
  let with_lock _ f = f ()

  type input = Reader.t
  type output = Writer.t

  let check_debug f g = f

  let read_line =
    check_debug
      (fun ic ->
         Reader.read_line ic
         >>= function
         |`Ok s -> return s
         |`Eof -> fail End_of_file
      )
      (fun ic ->
         Reader.read_line ic
         >>= function
         |`Ok s -> eprintf "<<< %s\n" s; return s
         |`Eof -> eprintf "<<<EOF\n"; fail End_of_file
      )

  let read_exactly ic len =
    let buf = String.create len in
    Reader.really_read ic ~pos:0 ~len buf >>= function
    |`Ok -> return buf
    |`Eof _ -> fail End_of_file

  let write =
    check_debug
      (fun oc buf ->
         Writer.write oc buf;
         return ())
      (fun oc buf ->
         eprintf "\n%4d >>> %s" (Pid.to_int (Unix.getpid ())) buf;
         Writer.write oc buf;
         return ())

  let flush oc = Writer.flushed oc >>= fun () -> return ()
  let close _ = return ()
end

include Imap.Make (Async_io)

open Async_ssl.Std

let default_ssl_port = 993

let connect_simple s ?(port=default_ssl_port) host =
  Tcp.connect (Tcp.to_host_and_port host port) >>= fun (socket, net_to_ssl, ssl_to_net) ->
  let net_to_ssl = Reader.pipe net_to_ssl in
  let ssl_to_net = Writer.pipe ssl_to_net in
  let app_to_ssl, app_wr = Pipe.create () in
  let app_rd, ssl_to_app = Pipe.create () in
  don't_wait_for (Ssl.client ~app_to_ssl ~ssl_to_app ~net_to_ssl ~ssl_to_net ());
  Reader.of_pipe (Info.of_string "imap_client_reader") app_rd >>= fun app_rd ->
  Writer.of_pipe (Info.of_string "imap_client_writer") app_wr >>= fun (app_wr, _) ->
  connect s (app_rd, app_wr)
