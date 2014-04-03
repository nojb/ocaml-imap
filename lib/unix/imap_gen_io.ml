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

module type S = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module Make (IO : S) = struct
  let (>>=) = IO.bind
  let (>|=) t f = IO.bind t (fun x -> IO.return (f x))

  type logger = [ `Read | `Write ] -> string Lazy.t -> unit

  let null_logger _ _ = ()

  type input = {
    in_read : string -> int -> int -> int IO.t;
    in_close : unit -> unit IO.t;
    in_underlying : input option;
    mutable in_logger : logger
  }
  
  type 'a output = {
    out_write : string -> int -> int -> int IO.t;
    out_close : unit -> 'a IO.t;
    out_flush : unit -> unit IO.t;
    out_underlying : 'a output option;
    mutable out_logger : logger
  }

  let set_logger_in inp log = inp.in_logger <- log

  let set_logger_out out log = out.out_logger <- log

  let underlying_in inp =
    match inp.in_underlying with
    | None -> inp
    | Some inp -> inp

  let underlying_out out =
    match out.out_underlying with
    | None -> out
    | Some out -> out

  let unsafe_read ic buf off len =
    ic.in_read buf off len >>= fun n ->
    if n > 0 then begin
      ic.in_logger `Read (lazy (String.sub buf off n));
      IO.return n
    end else
      IO.return 0

  let unsafe_write oc buf off len =
    oc.out_write buf off len >>= fun n ->
    if n > 0 then begin
      oc.out_logger `Write (lazy (String.sub buf off n));
      IO.return n
    end else
      IO.return 0

  let flush out =
    out.out_flush ()

  let close_in inp =
    inp.in_close ()

  let close_out out =
    out.out_close ()

  let create_out ?underlying ?(logger=null_logger) ~write ~close ~flush =
    {out_write=write; out_close=close; out_logger=logger; out_flush=flush; out_underlying=underlying}

  let create_in ?underlying ?(logger=null_logger) ~read ~close =
    {in_read=read; in_close=close; in_logger=logger; in_underlying=underlying}

  let null =
    create_out
      ?underlying:None
      ?logger:None
      ~write:(fun _ _ len -> IO.return len)
      ~close:(fun () -> IO.return ())
      ~flush:(fun () -> IO.return ())

  (* let read ic buf off len = *)
  (*   if off < 0 || len < 0 || off + len > String.length buf then *)
  (*     IO.fail (Failure (Printf.sprintf "read(off=%d,len=%d,buf_len=%d" *)
  (*                         off len (String.length buf))) *)
  (*   else *)
  (*     unsafe_read ic buf off len *)

  (* let write oc buf off len = *)
  (*   if off < 0 || len < 0 || off + len > String.length buf then *)
  (*     IO.fail (Failure (Printf.sprintf "write(off=%d,len=%d,buf_len=%d)" *)
  (*                         off len (String.length buf))) *)
  (*   else *)
  (*     unsafe_write oc buf off len *)

  let read_char =
    let buf = String.create 1 in
    fun ic ->
      unsafe_read ic buf 0 1 >>= function
      | 0 -> IO.fail End_of_file
      | _ -> IO.return buf.[0]

  let read_char_opt ic =
    IO.catch
      (fun () -> read_char ic >>= fun c -> IO.return (Some c))
      (function
        | End_of_file -> IO.return None
        | e -> IO.fail e)

  let read_line ic =
    let buf = Buffer.create 128 in
    let rec loop cr_read =
      read_char_opt ic >>= function
      | None ->
        if cr_read then Buffer.add_char buf '\r';
        IO.return (Buffer.contents buf)
      | Some '\n' ->
        IO.return (Buffer.contents buf)
      | Some '\r' ->
        if cr_read then Buffer.add_char buf '\r';
        loop true
      | Some ch ->
        if cr_read then Buffer.add_char buf '\r';
        Buffer.add_char buf ch;
        loop false
    in
    read_char ic >>= function
    | '\r' -> loop true
    | '\n' -> IO.return ""
    | ch -> Buffer.add_char buf ch; loop false

  let rec unsafe_read_into_exactly ic buf off len =
    unsafe_read ic buf off len >>= function
    | 0 ->
      IO.fail End_of_file
    | n ->
      let len = len - n in
      if len = 0 then
        IO.return ()
      else
        unsafe_read_into_exactly ic buf (off + n) len

  let read_into_exactly ic buf off len =
    if off < 0 || len < 0 || off + len > String.length buf then
      IO.fail (Invalid_argument (Printf.sprintf
                                   "read_into_exactly(off=%d,len=%d,buf_len=%d)"
                                   off len (String.length buf)))
    else
    if len = 0 then
      IO.return ()
    else
      unsafe_read_into_exactly ic buf off len

  let rec unsafe_write_from_exactly oc buf off len =
    if len <= 0 then
      IO.return ()
    else
      unsafe_write oc buf off len >>= fun n ->
      unsafe_write_from_exactly oc buf (off + n) (len - n)

  let write_from_exactly oc buf off len =
    if off < 0 || len < 0 || off + len > String.length buf then
      IO.fail
        (Invalid_argument
           (Printf.sprintf "write_from_exactly(off=%d,len=%d,buf_len=%d)"
              off len (String.length buf)))
    else
    if len = 0 then
      IO.return ()
    else
      unsafe_write_from_exactly oc buf off len

  let write oc s =
    unsafe_write_from_exactly oc s 0 (String.length s)

  let default_buffer_size = 16 * 1024

  let buffered_input ?(buffer_size=default_buffer_size) ic =
    let data = String.create buffer_size in
    let ptr = ref 0 in
    let max = ref 0 in
    let refill () =
      let size = !max - !ptr in
      if size > 0 then String.unsafe_blit data !ptr data 0 size;
      ptr := 0;
      max := size;
      let avail = buffer_size - size in
      unsafe_read ic data size avail >>= fun n ->
      if n < 0 || n > avail then
        IO.fail
          (Failure (Printf.sprintf
                      "buffered_input: refill: invalid result: read(request:%d,result:%d)"
                      avail n))
      else begin
        max := !max + n;
        IO.return n
      end
    in
    let unsafe_read buf off len =
      let avail = !max - !ptr in
      if avail > 0 then begin
        let len = min len avail in
        String.unsafe_blit data !ptr buf off len;
        ptr := !ptr + len;
        IO.return len
      end else begin
        refill () >>= fun n ->
        let len = min len n in
        String.unsafe_blit data 0 buf off len;
        ptr := len;
        max := n;
        IO.return len
      end
    in
    let close () = close_in ic in
    create_in ~underlying:ic ?logger:None ~read:unsafe_read ~close

  let buffered_output ?(buffer_size=default_buffer_size) oc =
    let data = String.create buffer_size in
    let ptr = ref 0 in
    let flush_once () =
      let len = !ptr in
      unsafe_write oc data 0 len >>= fun n ->
      if n < 0 || n > len then
        IO.fail (Failure (Printf.sprintf
                            "buffered_output: flush_once: invalid result: write(request=%d,result=%d"
                            len n))
      else begin
        let len = len - n in
        String.unsafe_blit data n data 0 len;
        ptr := len;
        IO.return n
      end
    in
    let unsafe_write buf off len =
      let rec loop n off len =
        let avail = buffer_size - !ptr in
        if avail >= len then begin
          String.unsafe_blit buf off data !ptr len;
          ptr := !ptr + len;
          IO.return (n + len)
        end else begin
          String.unsafe_blit buf off data !ptr avail;
          ptr := buffer_size;
          let n = n + avail in
          flush_once () >>= fun _ ->
          if !ptr = 0 then
            loop n (off + avail) (len - avail)
          else
            IO.return n
        end
      in
      loop 0 off len
    in
    let rec flush () =
      if !ptr > 0 then
        flush_once () >>= fun _ -> flush ()
      else
        IO.return ()
    in
    let close () =
      IO.catch flush (fun _ -> IO.return ()) >>= fun () -> close_out oc
    in
    create_out
      ~underlying:oc
      ?logger:None
      ~write:unsafe_write
      ~close
      ~flush

  let inflate_input ?(buffer_size=default_buffer_size) ic =
    let zstr : Zlib.stream = Zlib.inflate_init false in
    let data = String.create buffer_size in
    let avail_in = ref 0 in
    let next_in = ref 0 in
    let unsafe_inflate buf off len =
      let rec loop () =
        begin
          if !avail_in = 0 then begin
            next_in := 0;
            unsafe_read ic data 0 buffer_size
          end else
            IO.return !avail_in
        end >>= fun avail_in' ->
        if !avail_in = 0 then
          IO.return (0, 0)
        else begin
          avail_in := avail_in';
          let eof, used_in, used_out =
            Zlib.inflate zstr data !next_in !avail_in buf off len Zlib.Z_NO_FLUSH
          in
          next_in := !next_in + used_in;
          avail_in := !avail_in - used_in;
          if not eof && !avail_in = 0 && used_out = 0 then
            loop ()
          else
            IO.return (used_in, used_out)
        end
      in
      loop () >>= fun (_, used_out) ->
      IO.return used_out
    in
    let close () =
      (* XXX should flush the buffer *)
      IO.return ()
    in
    let log = ic.in_logger in
    ic.in_logger <- null_logger;
    create_in
      ?underlying:None
      ~logger:log
      ~read:unsafe_inflate
      ~close:close

  let deflate_output ?(buffer_size=default_buffer_size) oc =
    let zstr = Zlib.deflate_init 1 false in
    let data = String.create buffer_size in
    let unsafe_deflate buf off len =
      let len = min len buffer_size in
      let _, used_in, used_out =
        Zlib.deflate zstr buf off len data 0 buffer_size Zlib.Z_SYNC_FLUSH
      in
      let rec loop off len =
        if len <= 0 then
          IO.return ()
        else
          unsafe_write oc data off len >>= fun n ->
          loop (off + n) (len - n)
      in
      loop 0 used_out >>= fun () ->
      IO.return used_in
    in
    let close () =
      Zlib.deflate_end zstr;
      close_out oc
    in (* XXX close the zstream ? *)
    let flush () = flush oc in
    let log = oc.out_logger in
    oc.out_logger <- null_logger;
    create_out
      ?underlying:None
      ~logger:log
      ~write:unsafe_deflate
      ~close
      ~flush

  let string_input s =
    let ptr = ref 0 in
    let unsafe_read buf off len =
      let len = min len (String.length s - !ptr) in
      String.unsafe_blit s !ptr buf off len;
      ptr := !ptr + len;
      IO.return len
    in
    let close () = IO.return () in
    create_in
      ?underlying:None
      ?logger:None
      ~read:unsafe_read
      ~close

  let string_output () =
    let b = Buffer.create 80 in
    let unsafe_write buf off len =
      Buffer.add_substring b buf off len;
      IO.return len
    in
    let close () =
      IO.return (Buffer.contents b)
    in
    let flush () = IO.return () in
    create_out
      ?underlying:None
      ?logger:None
      ~write:unsafe_write
      ~close
      ~flush

  (* The default logger echoes both input and output to [stderr].  In order to
     make them look a little nicer, new lines are preceded by "S: " or "C: "
     depending on the case.  Here a new line means either "\r\n" or "\n". *)
  let default_logger =
    fun direction str ->
      match direction with
      | `Write ->
        Imap_utils.log `Client (Lazy.force str)
      | `Read ->
        Imap_utils.log `Server (Lazy.force str)
end
