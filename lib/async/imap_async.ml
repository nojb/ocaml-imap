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

(* module Ivar = Async_kernel.Raw_ivar *)
                
module Async_io = struct
  type 'a t = ('a, exn) Result.t Deferred.t
    
  let fail exn =
    Deferred.return (Error exn)

  let bind t' f =
    t' >>= function
    | Error _ as e -> Deferred.return e
    | Ok x -> f x
  
  let return x =
    Deferred.return (Ok x)

  let catch t_susp handler =
    t_susp () >>= function
    | Error e -> handler e
    | Ok _ as r -> Deferred.return r

  (* let (>>=) = bind *)

  type mutex = unit
  let create_mutex () = ()
  let is_locked _ = false
  let with_lock _ f = f ()

  type input = Reader.t
  type output = Writer.t

  let read_line ic =
    Reader.read_line ic >>= function
    | `Ok s ->
      if !Imap.debug then Imap_utils.log `Server (s ^ "\r\n");
      return s
    | `Eof ->
      fail End_of_file

  let read_exactly ic len =
    let buf = String.create len in
    Reader.really_read ic ~pos:0 ~len buf >>= function
    | `Ok ->
      if !Imap.debug then Imap_utils.log `Server buf;
      return buf
    | `Eof _ ->
      fail End_of_file

  let write oc buf =
    Writer.write oc buf;
    if !Imap.debug then Imap_utils.log `Client buf;
    return ()

  let flush oc = Writer.flushed oc >>= fun () -> return ()
  let close _ = return ()

  let compress _ = assert false
  let starttls _ = assert false
    
  open Async_ssl.Std

  let connect port host =
    assert false

  let connect_ssl _ ?ca_file port host =
    Tcp.connect (Tcp.to_host_and_port host port) >>= fun (socket, net_to_ssl, ssl_to_net) ->
    let net_to_ssl = Reader.pipe net_to_ssl in
    let ssl_to_net = Writer.pipe ssl_to_net in
    let app_to_ssl, app_wr = Pipe.create () in
    let app_rd, ssl_to_app = Pipe.create () in
    don't_wait_for (Ssl.client ~app_to_ssl ~ssl_to_app ~net_to_ssl ~ssl_to_net ());
    Reader.of_pipe (Info.of_string "imap_client_reader") app_rd >>= fun app_rd ->
    Writer.of_pipe (Info.of_string "imap_client_writer") app_wr >>= fun (app_wr, _) ->
    return (app_rd, app_wr)
end

include Imap.Make (Async_io)
