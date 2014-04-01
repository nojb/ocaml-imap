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

module Make (U : Imap_io_low.UNIX) = struct
  module Low = Imap_io_low.Make (U)
  module IO = U.IO
  
  let (>>=) = IO.bind
  let (>|=) t f = IO.bind t (fun x -> IO.return (f x))

  type buf = {
    mutable data : string;
    mutable length : int;
    mutable ptr : int;
    mutable max : int
  }

  type input
  type output

  type _ mode =
    | Input : input mode
    | Output : output mode

  type 'a chan = {
    mode : 'a mode;
    buf : buf;
    mutable low : Low.t
  }

  type ic =
    input chan

  type oc =
    output chan

  (* type t = { *)
  (*   rd_buf : buf; *)
  (*   wr_buf : buf; *)
  (*   mutable low : Low.t *)
  (* } *)

  let create_buf bufsize =
    {data = String.create bufsize;
     ptr = 0;
     length = bufsize;
     max = 0}

  let default_buffer_size = 1024

  let of_low ?buffer_size:(bufsize=default_buffer_size) low =
    let rd_buf = create_buf bufsize in
    let wr_buf = create_buf bufsize in
    {mode = Input; buf = rd_buf; low},
    {mode = Output; buf = wr_buf; low}

  let get_low s =
    s.low

  let set_low s low =
    s.low <- low

  let perform_read io =
    let buf = io.buf in
    let size = buf.max - buf.ptr in
    if size > 0 then String.unsafe_blit buf.data buf.ptr buf.data 0 size;
    buf.ptr <- 0;
    buf.max <- size;
    let ptr = size in
    let len = buf.length - size in
    Low.read io.low buf.data ptr len >>= fun n ->
    if n < 0 || n > len then
      IO.fail
        (Failure
           (Printf.sprintf
              "ImapIO.perform_read: invalid result of the [read] function(request:%d,result:%d)"
              len n))
    else begin
      buf.max <- buf.max + n;
      IO.return n
    end

  let perform_write oc =
    let buf = oc.buf in
    let len = buf.ptr in
    Low.write oc.low buf.data 0 len >>= fun n ->
    if n < 0 || n > len then
      IO.fail
        (Failure
           (Printf.sprintf
              "ImapIO.perform_read: invalid result of the [write] function(request:%d,result:%d)"
              len n))
    else begin
      let len = len - n in
      String.unsafe_blit buf.data n buf.data 0 len;
      buf.ptr <- len;
      IO.return n
    end

  let rec read_char io =
    let buf = io.buf in
    if buf.ptr = buf.max then
      perform_read io >>= function
      | 0 -> IO.fail End_of_file
      | _ -> read_char io
    else begin
      let ch = String.unsafe_get buf.data buf.ptr in
      buf.ptr <- buf.ptr + 1;
      IO.return ch
    end

  let read_char_opt io =
    IO.catch
      (fun () -> read_char io >|= fun ch -> Some ch)
      (function
        | End_of_file -> IO.return None
        | exn -> IO.fail exn)

  let unsafe_read_into io str ofs len =
    let buf = io.buf in
    let avail = buf.max - buf.ptr in
    if avail > 0 then begin
      let len = min len avail in
      String.unsafe_blit buf.data buf.ptr str ofs len;
      buf.ptr <- buf.ptr + len;
      IO.return len
    end else begin
      perform_read io >>= fun n ->
      let len = min len n in
      String.unsafe_blit buf.data 0 str ofs len;
      buf.ptr <- len;
      buf.max <- n;
      IO.return len
    end

  let read_into io str ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length str then
      IO.fail
        (Invalid_argument (Printf.sprintf
                             "Imap_io.read_into(ofs=%d,len=%d,str_len=%d)"
                             ofs len (String.length str)))
    else begin
      if len = 0 then
        IO.return 0
      else
        unsafe_read_into io str ofs len
    end

  let rec unsafe_read_into_exactly io str ofs len =
    unsafe_read_into io str ofs len >>= function
    | 0 ->
      IO.fail End_of_file
    | n ->
      let len = len - n in
      if len = 0 then
        IO.return ()
      else
        unsafe_read_into_exactly io str (ofs + n) len

  let read_into_exactly io str ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length str then
      IO.fail (Invalid_argument (Printf.sprintf
                                    "Imap_io.read_into_exactly(ofs=%d,len=%d,str_len=%d)"
                                    ofs len (String.length str)))
    else begin
      if len = 0 then
        IO.return ()
      else
        unsafe_read_into_exactly io str ofs len
    end

  let rev_concat len l =
    let buf = String.create len in
    let _ =
      List.fold_left
        (fun ofs str ->
           let len = String.length str in
           let ofs = ofs - len in
           String.unsafe_blit str 0 buf ofs len;
           ofs)
        len l
    in
    buf

  let rec read_all io total_len acc =
    let buf = io.buf in
    let len = buf.max - buf.ptr in
    let str = String.create len in
    String.unsafe_blit buf.data buf.ptr str 0 len;
    buf.ptr <- buf.max;
    perform_read io >>= function
    | 0 ->
      IO.return (rev_concat (len + total_len) (str :: acc))
    | n ->
      read_all io (len + total_len) (str :: acc)

  let read ?count io =
    match count with
    | None ->
      read_all io 0 []
    | Some len ->
      let str = String.create len in
      unsafe_read_into io str 0 len >>= fun real_len ->
      if real_len < len then
        IO.return (String.sub str 0 real_len)
      else
        IO.return str

  let read_line io =
    let buf = Buffer.create 128 in
    let rec loop cr_read =
      read_char_opt io >>= function
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
    read_char io >>= function
    | '\r' -> loop true
    | '\n' -> IO.return ""
    | ch -> Buffer.add_char buf ch; loop false

  let rec flush io =
    if io.buf.ptr > 0 then
      perform_write io >>= fun _ ->
      flush io
    else
      IO.return ()

  let safe_flush io =
    IO.catch (fun () -> flush io) (fun _ -> IO.return ())

  let rec write_char io ch =
    let buf = io.buf in
    let ptr = buf.ptr in
    if ptr < buf.length then begin
      buf.ptr <- ptr + 1;
      String.unsafe_set buf.data ptr ch;
      IO.return ()
    end else
      perform_write io >>= fun _ ->
      write_char io ch

  let rec unsafe_write_from io str ofs len =
    let buf = io.buf in
    let avail = buf.length - buf.ptr in
    if avail >= len then begin
      String.unsafe_blit str ofs buf.data buf.ptr len;
      buf.ptr <- buf.ptr + len;
      IO.return 0
    end else begin
      String.unsafe_blit str ofs buf.data buf.ptr avail;
      buf.ptr <- buf.length;
      perform_write io >>= fun _ ->
      let len = len - avail in
      if buf.ptr = 0 then begin
        if len = 0 then
          IO.return 0
        else
          (* Everything has been written, try to write more: *)
          unsafe_write_from io str (ofs + avail) len
      end else
        (* Not everything has been written, just what is
           remaining: *)
        IO.return len
    end

  let write_from io str ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length str then
      IO.fail
        (Invalid_argument
           (Printf.sprintf "Imap_io.write_from(ofs=%d,len=%d,str_len=%d)"
              ofs len (String.length str)))
    else begin
      if len = 0 then
        IO.return 0
      else
        unsafe_write_from io str ofs len >>= fun remaining ->
        IO.return (len - remaining)
    end

  let rec unsafe_write_from_exactly io str ofs len =
    unsafe_write_from io str ofs len >>= function
    | 0 ->
      IO.return ()
    | n ->
      unsafe_write_from_exactly io str (ofs + len - n) n

  let write_from_exactly io str ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length str then
      IO.fail
        (Invalid_argument
           (Printf.sprintf "Imap_io.write_from_exactly(ofs=%d,len=%d,str_len=%d)"
              ofs len (String.length str)))
    else begin
      if len = 0 then
        IO.return ()
      else
        unsafe_write_from_exactly io str ofs len
    end

  let write io str =
    unsafe_write_from_exactly io str 0 (String.length str)

  let close (_, oc) =
    safe_flush oc >>= fun () ->
    Low.close oc.low
end
