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

module M = struct
  class virtual input =
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

  class virtual output =
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
      inherit input
      method read = Pervasives.input ic
      method close = Pervasives.close_in ic
    end

  let output_of_out_channel oc =
    object
      inherit output
      method write buf off len = Pervasives.output oc buf off len; len
      method close = Pervasives.close_out oc
      method flush = Pervasives.flush oc
    end

  let input_of_ssl sock =
    object
      inherit input
      method read = Ssl.read sock
      method close = Ssl.shutdown sock
    end

  let output_of_ssl sock =
    object
      inherit output
      method write = Ssl.write sock
      method close = Ssl.shutdown sock
      method flush = Ssl.flush sock
    end

  let inflate_input ic =
    let chunk_size = 1024 in
    let read_buf = String.create chunk_size in
    let tr = Cryptokit.Zlib.uncompress () in
    object (self)
      inherit input
      method read buf off len =
        let avail = tr#available_output in
        if avail > 0 then
          if avail <= len then
            let zbuf, zoff, zlen = tr#get_substring in
            String.unsafe_blit zbuf zoff buf off zlen;
            zlen
          else
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
      inherit output
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

  let read_line ic =
    let s = ic#read_line in
    if !Imap.debug then begin
      Imap_utils.log `Server s;
      Imap_utils.log `Server "\r\n"
    end;
    s

  let read_exactly ic n =
    let s = ic#read_exactly n in
    if !Imap.debug then Imap_utils.log `Server s;
    s   

  let close_in ic =
    ic#close

  let write oc s =
    oc#write_string s;
    if !Imap.debug then Imap_utils.log `Client s

  let flush oc =
    oc#flush

  let close_out oc =
    oc#close

  let connect port host =
    let he = Unix.gethostbyname host in
    let ic, oc = Unix.open_connection (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) in
    let ic = input_of_in_channel ic in
    let oc = output_of_out_channel oc in
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
    let ic = input_of_ssl sock in
    let oc = output_of_ssl sock in
    (ic, oc)

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
  
  let starttls version ?ca_file (ic, oc) =
    assert false

  let compress (ic, oc) =
    let ic = inflate_input ic in
    let oc = deflate_output oc in
    (ic, oc)
end

include Imap.Make (M)

