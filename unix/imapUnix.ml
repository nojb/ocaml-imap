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

open ImapTypes

let string_of_error =
  function
    Bad -> "Server did not understand request"
  | BadTag -> "Incorrect tag in reply"
  | No -> "Server denied request"
  | Bye -> "Server closed the connection"
  | ParseError -> "Parser error"
  | Auth_error -> "Authentication error"

exception Error of error

type session = {
  sock : Ssl.socket;
  mutable imap_session : state;
  buffer : Buffer.t;
  mutable pos : int
}

let run_control s c =
  let rec loop =
    function
      ControlOk (x, st, i) ->
        s.imap_session <- st;
        s.pos <- i;
        x
    | ControlFail err ->
        raise (Error err)
    | ControlFlush (str, r) ->
        prerr_endline ">>>>";
        prerr_string str;
        prerr_endline ">>>>";
        Ssl.output_string s.sock str;
        Ssl.flush s.sock;
        loop r
    | ControlNeed (len, k) ->
        let buf = String.create 65536 in
        match Ssl.read s.sock buf 0 (String.length buf) with
          0 ->
            loop (k End)
        | _ as n ->
            prerr_endline "<<<<";
            prerr_string (String.sub buf 0 n);
            prerr_endline "<<<<";
            Buffer.add_substring s.buffer buf 0 n;
            loop (k More)
  in
  loop (c s.imap_session (Buffer.create 0) s.buffer s.pos)

let connect s =
  run_control s Commands.greeting 

let _ =
  prerr_endline "Initialising SSL...";
  Ssl.init ()

(* let ssl_context ssl_method ca_file = *)
(*   let version = *)
(*     match version with *)
(*       `TLSv1 -> Ssl.TLSv1 *)
(*     | `SSLv23 -> Ssl.SSLv23 *)
(*     | `SSLv3 -> Ssl.SSLv3 *)
(*   in *)
(*   let ctx = Ssl.create_context ssl_method Ssl.Client_context in *)
(*   match ca_file with *)
(*     None -> *)
(*       ctx *)
(*   | Some ca_file -> *)
(*       Ssl.load_verify_locations ctx ca_file ""; *)
(*       Ssl.set_verify ctx [Ssl.Verify_peer] None; *)
(*       ctx *)

let create_session ?(ssl_method = Ssl.TLSv1) ?(port=993) host =
  let he = Unix.gethostbyname host in
  let sockaddr = Unix.ADDR_INET (he.Unix.h_addr_list.(0), port) in
  let sock = Ssl.open_connection ssl_method sockaddr in
  {sock; imap_session = Commands.fresh_state; buffer = Buffer.create 0; pos = 0}

let next_tag s =
  let tag, st = Commands.next_tag s.imap_session in
  s.imap_session <- st;
  tag
      
let send_command s cmd =
  run_control s (cmd (next_tag s))
  
(* module M = struct *)
(*   class virtual input_chan = *)
(*     object (self) *)
(*   let inflate_input ic = *)
(*     let chunk_size = 1024 in *)
(*     let read_buf = String.create chunk_size in *)
(*     let tr = Cryptokit.Zlib.uncompress () in *)
(*     object (self) *)
(*       inherit input_chan *)
(*       method read buf off len = *)
(*         let avail = tr#available_output in *)
(*         if avail > 0 then *)
(*           if avail <= len then begin *)
(*             let zbuf, zoff, zlen = tr#get_substring in *)
(*             String.unsafe_blit zbuf zoff buf off zlen; *)
(*             zlen *)
(*           end else *)
(*             let rec loop n off len = *)
(*               if len <= 0 then n *)
(*               else begin *)
(*                 buf.[off] <- tr#get_char; *)
(*                 loop (n+1) (off+1) (len-1) *)
(*               end *)
(*             in *)
(*             loop 0 off len *)
(*         else begin *)
(*           let n = ic#read read_buf 0 chunk_size in *)
(*           tr#put_substring read_buf 0 n; *)
(*           self#read buf off len *)
(*         end *)
(*       method close = *)
(*         tr#finish *)
(*     end *)

(*   let deflate_output oc = *)
(*     let tr = Cryptokit.Zlib.compress () in *)
(*     object *)
(*       inherit output_chan *)
(*       method write buf off len = *)
(*         tr#put_substring buf off len; *)
(*         (\* if tr#available_output > 0 then oc#write_string tr#get_string; *\) *)
(*         len *)
(*       method close = *)
(*         tr#finish; *)
(*         tr#flush; *)
(*         if tr#available_output > 0 then oc#write_string tr#get_string; *)
(*         oc#flush; *)
(*         oc#close *)
(*       method flush = *)
(*         tr#flush; *)
(*         if tr#available_output > 0 then oc#write_string tr#get_string; *)
(*         oc#flush *)
(*     end *)

(*   let connect port host = *)
(*     let he = Unix.gethostbyname host in *)
(*     let ic, oc = Unix.open_connection (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) in *)
(*     let fd = Unix.descr_of_in_channel ic in *)
(*     let ic = input_of_in_channel ic in *)
(*     let oc = output_of_out_channel oc in *)
(*     (ic, (fd, oc)) *)

(*   let disconnect (ic, (_, oc)) = *)
(*     ic#close; *)
(*     oc#close *)
(*   (\* FIXME This closes the underlying file descr twice. *\) *)
    
(*   let _ = Ssl.init () *)

(*   let ssl_context version ca_file = *)
(*     let version = match version with *)
(*       | `TLSv1 -> Ssl.TLSv1 *)
(*       | `SSLv23 -> Ssl.SSLv23 *)
(*       | `SSLv3 -> Ssl.SSLv3 *)
(*     in *)
(*     let ctx = Ssl.create_context version Ssl.Client_context in *)
(*     match ca_file with *)
(*     | None -> *)
(*       ctx *)
(*     | Some ca_file -> *)
(*       Ssl.load_verify_locations ctx ca_file ""; *)
(*       Ssl.set_verify ctx [Ssl.Verify_peer] None; *)
(*       ctx *)

(*   let connect_ssl version ?ca_file port host = *)
(*     let he = Unix.gethostbyname host in *)
(*     let sock = Ssl.open_connection_with_context (ssl_context version ca_file) *)
(*         (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) *)
(*     in *)
(*     let fd = Ssl.file_descr_of_socket sock in *)
(*     let ic = input_of_ssl sock in *)
(*     let oc = output_of_ssl sock in *)
(*     (ic, (fd, oc)) *)

(*   let starttls version ?ca_file (_, (fd, _)) = *)
(*     let sock = Ssl.embed_socket fd (ssl_context version ca_file) in *)
(*     Ssl.connect sock; *)
(*     let ic = input_of_ssl sock in *)
(*     let oc = output_of_ssl sock in *)
(*     (ic, (fd, oc)) *)

(*   let compress (ic, (fd, oc)) = *)
(*     let ic = inflate_input ic in *)
(*     let oc = deflate_output oc in *)
(*     (ic, (fd, oc)) *)
(* end *)
