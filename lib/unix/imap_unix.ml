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

module Sync = struct
  type 'a t = 'a
  let bind t f = f t
  let return x = x
  let fail e = raise e
  let catch f g = try f () with e -> g e
  type mutex = unit
  let create_mutex () = ()
  let is_locked () = false
  let with_lock () f = f ()
end

module Unix_IO = Imap_gen_io.Make (Sync)

let ssl_input sock =
  Unix_IO.create_in
    ~read:(Ssl.read sock)
    ~close:(fun () -> Ssl.shutdown sock)

let ssl_output sock =
  Unix_IO.create_out
    ~write:(Ssl.write sock)
    ~close:(fun () -> Ssl.shutdown sock)
    ~flush:(fun () -> Ssl.flush sock)

let input_of_file_descr fd =
  Unix_IO.create_in
    ~read:(Unix.read fd)
    ~close:(fun () -> Unix.close fd)

let output_of_file_descr fd =
  Unix_IO.create_out
    ~write:(Unix.write fd)
    ~close:(fun () -> Unix.close fd)
    ~flush:(fun () -> ())

include Imap.Make (struct
    include Sync
    type input = Unix_IO.input
    type output = unit Unix_IO.output
    let read_line ic =
      let s = Unix_IO.read_line ic in
      if !Imap.debug then begin
        Imap_utils.log `Server s;
        Imap_utils.log `Server "\r\n"
      end;
      s
    let read_exactly ic len =
      let buf = String.create len in
      Unix_IO.read_into_exactly ic buf 0 len;
      if !Imap.debug then Imap_utils.log `Server buf;
      buf
    let flush = Unix_IO.flush
    let write oc s =
      Unix_IO.write oc s;
      if !Imap.debug then Imap_utils.log `Client s
    let compress (ic, oc) =
      (* let ic = Unix_IO.underlying_in ic in *)
      (* let oc = Unix_IO.underlying_out oc in *)
      let ic = (* Unix_IO.buffered_input *) (Unix_IO.inflate_input ic) in
      let oc = (* Unix_IO.buffered_output *) (Unix_IO.deflate_output oc) in
      (ic, oc)
    let starttls _ = assert false

    let connect port host =
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let he = Unix.gethostbyname host in
      Unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port));
      let ic = Unix_IO.buffered_input (input_of_file_descr fd) in
      let oc = Unix_IO.buffered_output (output_of_file_descr fd) in
      (ic, oc)

    let _ = Ssl.init ()
        
    let connect_ssl version ?ca_file port host =
      let he = Unix.gethostbyname host in
      let version = match version with
        | `TLSv1 -> Ssl.TLSv1
        | `SSLv23 -> Ssl.SSLv23
        | `SSLv3 -> Ssl.SSLv3
      in
      let ctx = Ssl.create_context version Ssl.Client_context in
      begin match ca_file with
        | None -> ()
        | Some ca_file ->
          Ssl.load_verify_locations ctx ca_file "";
          Ssl.set_verify ctx [Ssl.Verify_peer] None
      end;
      let sock = Ssl.open_connection_with_context ctx
          (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port))
      in
      let ic = ssl_input sock in
      let oc = ssl_output sock in
      (* let ic = Unix_IO.buffered_input ic in *)
      (* let oc = Unix_IO.buffered_output oc in *)
      (ic, oc)
  end)

(* let starttls ?ssl_context s = *)
(*   let aux (ic, oc) = *)
(*     let fd = match Unix_io.Low.get_fd (Unix_io.get_low ic) with *)
(*       | None -> failwith "starttls: no file descriptor" *)
(*       | Some fd -> fd *)
(*     in *)
(*     let low, connect = Unix_io.Low.open_tls ?ssl_context fd in *)
(*     connect (); *)
(*     Unix_io.set_low ic low; *)
(*     Unix_io.set_low oc low; *)
(*     (ic, oc) *)
(*   in *)
(*   starttls s aux *)
