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

open Imap

module M = struct
  class virtual input_chan =
    object (self)
      method virtual read : string -> int -> int -> int
      method virtual close : unit

      method read_char =
        let buf = String.create 1 in
        match self#read buf 0 1 with
        | 0 -> raise End_of_file
        | _ -> buf.[0]

      method read_char_opt =
        try Some self#read_char with End_of_file -> None

      method read_line =
        let buf = Buffer.create 128 in
        let rec loop cr_read =
          match self#read_char_opt with
          | None ->
            if cr_read then Buffer.add_char buf '\r';
            Buffer.contents buf
          | Some '\n' ->
            Buffer.contents buf
          | Some '\r' ->
            if cr_read then Buffer.add_char buf '\r';
            loop true
          | Some ch ->
            if cr_read then Buffer.add_char buf '\r';
            Buffer.add_char buf ch;
            loop false
        in
        match self#read_char with
        | '\r' -> loop true
        | '\n' -> ""
        | ch -> Buffer.add_char buf ch; loop false

      method read_exactly n =
        let buf = String.create n in
        let rec loop n off len =
          if len <= 0 then buf
          else
            match self#read buf off len with
            | 0 -> raise End_of_file
            | m -> loop (n + m) (off + m) (len - m)
        in
        loop 0 0 n
    end

  class virtual output_chan =
    object (self)
      method virtual write : string -> int -> int -> int

      method virtual close : unit

      method virtual flush : unit

      method write_string s =
        let rec loop off len =
          if len <= 0 then
            ()
          else
            let n = self#write s off len in
            loop (off + n) (len - n)
        in
        loop 0 (String.length s)
    end

  let input_of_in_channel ic =
    object
      inherit input_chan
      method read = Pervasives.input ic
      method close = Pervasives.close_in ic
    end

  let output_of_out_channel oc =
    object
      inherit output_chan
      method write buf off len = Pervasives.output oc buf off len; len
      method close = Pervasives.close_out oc
      method flush = Pervasives.flush oc
    end

  let input_of_ssl sock =
    object
      inherit input_chan
      method read = Ssl.read sock
      method close = Ssl.shutdown sock
    end

  let output_of_ssl sock =
    object
      inherit output_chan
      method write = Ssl.write sock
      method close = Ssl.shutdown sock
      method flush = Ssl.flush sock
    end

  let inflate_input ic =
    let chunk_size = 1024 in
    let read_buf = String.create chunk_size in
    let tr = Cryptokit.Zlib.uncompress () in
    object (self)
      inherit input_chan
      method read buf off len =
        let avail = tr#available_output in
        if avail > 0 then
          if avail <= len then begin
            let zbuf, zoff, zlen = tr#get_substring in
            String.unsafe_blit zbuf zoff buf off zlen;
            zlen
          end else
            let rec loop n off len =
              if len <= 0 then n
              else begin
                buf.[off] <- tr#get_char;
                loop (n+1) (off+1) (len-1)
              end
            in
            loop 0 off len
        else begin
          let n = ic#read read_buf 0 chunk_size in
          tr#put_substring read_buf 0 n;
          self#read buf off len
        end
      method close =
        tr#finish
    end

  let deflate_output oc =
    let tr = Cryptokit.Zlib.compress () in
    object
      inherit output_chan
      method write buf off len =
        tr#put_substring buf off len;
        (* if tr#available_output > 0 then oc#write_string tr#get_string; *)
        len
      method close =
        tr#finish;
        tr#flush;
        if tr#available_output > 0 then oc#write_string tr#get_string;
        oc#flush;
        oc#close
      method flush =
        tr#flush;
        if tr#available_output > 0 then oc#write_string tr#get_string;
        oc#flush
    end

  type 'a t = 'a

  let bind t f = f t
  let fail e = raise e
  let return x = x
  let catch f g = try f () with e -> g e

  type mutex = unit
  let create_mutex () = ()
  let with_lock () f = f ()
  let is_locked () = false

  type input = input_chan
  type output = Unix.file_descr * output_chan

  let read_line ic =
    let s = ic#read_line in
    if !Client.debug then begin
      Utils.log `Server s;
      Utils.log `Server "\r\n"
    end;
    s

  let read_exactly ic n =
    let s = ic#read_exactly n in
    if !Client.debug then Utils.log `Server s;
    s   

  let write (_, oc) s =
    oc#write_string s;
    if !Client.debug then Utils.log `Client s

  let flush (_, oc) =
    oc#flush

  let connect port host =
    let he = Unix.gethostbyname host in
    let ic, oc = Unix.open_connection (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) in
    let fd = Unix.descr_of_in_channel ic in
    let ic = input_of_in_channel ic in
    let oc = output_of_out_channel oc in
    (ic, (fd, oc))

  let disconnect (ic, (_, oc)) =
    ic#close;
    oc#close
  (* FIXME This closes the underlying file descr twice. *)
    
  let _ = Ssl.init ()

  let ssl_context version ca_file =
    let version = match version with
      | `TLSv1 -> Ssl.TLSv1
      | `SSLv23 -> Ssl.SSLv23
      | `SSLv3 -> Ssl.SSLv3
    in
    let ctx = Ssl.create_context version Ssl.Client_context in
    match ca_file with
    | None ->
      ctx
    | Some ca_file ->
      Ssl.load_verify_locations ctx ca_file "";
      Ssl.set_verify ctx [Ssl.Verify_peer] None;
      ctx

  let connect_ssl version ?ca_file port host =
    let he = Unix.gethostbyname host in
    let sock = Ssl.open_connection_with_context (ssl_context version ca_file)
        (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port))
    in
    let fd = Ssl.file_descr_of_socket sock in
    let ic = input_of_ssl sock in
    let oc = output_of_ssl sock in
    (ic, (fd, oc))

  let starttls version ?ca_file (_, (fd, _)) =
    let sock = Ssl.embed_socket fd (ssl_context version ca_file) in
    Ssl.connect sock;
    let ic = input_of_ssl sock in
    let oc = output_of_ssl sock in
    (ic, (fd, oc))

  let compress (ic, (fd, oc)) =
    let ic = inflate_input ic in
    let oc = deflate_output oc in
    (ic, (fd, oc))
end

include Client.Make (M)

let idle s f =
  ignore (idle s f)
