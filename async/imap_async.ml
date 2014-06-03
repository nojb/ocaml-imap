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

open Imap
  
(* module Ivar = Async_kernel.Raw_ivar *)
                
module Async_io = struct
  type 'a t = 'a Deferred.t
    
  let fail exn =
    raise exn

  let bind = Deferred.bind
  
  let return = Deferred.return

  let catch f handler =
    try_with ~extract_exn:true f >>= function
    | Ok x -> return x
    | Error e -> handler e

  type mutex = {
    mutable locked : bool;
    mutable waiters : unit Ivar.t Queue.t
  }
  let create_mutex () = {
    locked = false;
    waiters = Queue.create ()
  }
  let is_locked m = m.locked
  let lock m =
    if m.locked then begin
      let iv = Ivar.create () in
      Queue.enqueue m.waiters iv;
      Ivar.read iv
    end
    else begin
      m.locked <- true;
      return ()
    end
  let unlock m =
    if m.locked then begin
      match Queue.dequeue m.waiters with
      | Some iv -> Ivar.fill iv ()
      | None -> m.locked <- false
    end      
  let with_lock m f =
    lock m >>= fun () ->
    Monitor.protect f ~finally:(fun () -> unlock m; return ())

  type input = Reader.t
  type output = Writer.t

  let read_line ic =
    Reader.read_line ic >>= function
    | `Ok s ->
      if !Client.debug then begin
        Utils.log `Server s;
        Utils.log `Server "\r\n"
      end;
      return s
    | `Eof ->
      fail End_of_file

  let read_exactly ic len =
    let buf = String.create len in
    Reader.really_read ic ~pos:0 ~len buf >>= function
    | `Ok ->
      if !Client.debug then Utils.log `Server buf;
      return buf
    | `Eof _ ->
      fail End_of_file

  let write oc buf =
    Writer.write oc buf;
    if !Client.debug then Utils.log `Client buf;
    return ()

  let flush oc = Writer.flushed oc >>= fun () -> return ()
  
  let compress _ = assert false
  let starttls _ = assert false
    
  open Async_ssl.Std

  let connect port host =
    Tcp.connect (Tcp.to_host_and_port host port) >>= fun (_, net_to_app, app_to_net) ->
    return (net_to_app, app_to_net)

  let disconnect (rd, wr) =
    Deferred.all_ignore [Writer.close wr; Reader.close rd]
      (* FIXME This closes the underlying file descr twice. *)

  let connect_ssl version ?ca_file port host =
    Tcp.connect (Tcp.to_host_and_port host port) >>= fun (socket, net_to_ssl, ssl_to_net) ->
    let net_to_ssl = Reader.pipe net_to_ssl in
    let ssl_to_net = Writer.pipe ssl_to_net in
    let app_to_ssl, app_wr = Pipe.create () in
    let app_rd, ssl_to_app = Pipe.create () in
    let version = match version with
      | `TLSv1 -> Ssl.Version.Tlsv1
      | `SSLv23 -> Ssl.Version.Sslv23
      | `SSLv3 -> Ssl.Version.Sslv3
    in
    don't_wait_for (Ssl.client ~version ?ca_file ~app_to_ssl ~ssl_to_app ~net_to_ssl ~ssl_to_net ());
    Reader.of_pipe (Info.of_string "imap_client_reader") app_rd >>= fun app_rd ->
    Writer.of_pipe (Info.of_string "imap_client_writer") app_wr >>= fun (app_wr, _) ->
    return (app_rd, app_wr)
end

include Client.Make (Async_io)

let noop s = ignore (noop s)

let logout s = ignore (logout s)

let starttls ?version ?ca_file s = ignore (starttls ?version ?ca_file s)

let authenticate s a = ignore (authenticate s a)

let login s p l = ignore (login s p l)

let compress s = ignore (compress s)

let select s m = ignore (select s m)

let examine s m = ignore (examine s m)

let create s m = ignore (create s m)

let delete s m = ignore (delete s m)

let rename s m1 m2 = ignore (rename s m1 m2)

let subscribe s m = ignore (subscribe s m)

let unsubscribe s m = ignore (unsubscribe s m)

let append s m ?flags ?date d = ignore (append s m ?flags ?date d)

let idle s f = let _, stop = idle s f in stop

let check s = ignore (check s)

let close s = ignore (close s)

let expunge s = ignore (expunge s)

let uid_expunge s uids = ignore (uid_expunge s uids)

let fetch s seqs atts =
  let rd, wr = Pipe.create () in
  ignore (Monitor.protect
            (fun () -> fetch s seqs atts (Pipe.write_without_pushback wr))
            (fun () -> Pipe.close wr; return ()));
  rd

let fetch_changedsince s seqs modseq atts =
  let rd, wr = Pipe.create () in
  ignore (Monitor.protect
            (fun () -> fetch_changedsince s seqs modseq atts (Pipe.write_without_pushback wr))
            (fun () -> Pipe.close wr; return ()));
  rd

let uid_fetch s uids atts =
  let rd, wr = Pipe.create () in
  ignore (Monitor.protect
            (fun () -> uid_fetch s uids atts (Pipe.write_without_pushback wr))
            (fun () -> Pipe.close wr; return ()));
  rd

let uid_fetch_changedsince s uids modseq atts =
  let rd, wr = Pipe.create () in
  ignore (Monitor.protect
            (fun () -> uid_fetch_changedsince s uids modseq atts (Pipe.write_without_pushback wr))
            (fun () -> Pipe.close wr; return ()));
  rd

let store s seqs mode atts = ignore (store s seqs mode atts)

let uid_store s uids mode atts = ignore (uid_store s uids mode atts)

let copy s seqs m = ignore (copy s seqs m)

let uid_copy s uids m = ignore (uid_copy s uids m)
