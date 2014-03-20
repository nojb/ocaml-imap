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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type log_type =
  | Log_read
  | Log_write

type logger = log_type -> string -> unit Lwt.t

type t = {
  _read : string -> int -> int -> int Lwt.t;
  _write : string -> int -> int -> int Lwt.t;
  _close : unit -> unit Lwt.t;
  _fd : Lwt_unix.file_descr option;
  mutable logger : logger option
}

let null = {
  _read = (fun _ _ _ -> Lwt.return 0);
  _write = (fun _ _ len -> Lwt.return len);
  _close = (fun _ -> Lwt.return ());
  _fd = None;
  logger = None
}

let read low buf off len =
  low._read buf off len >>= fun n ->
  if n > 0 then
    match low.logger with
    | None ->
      Lwt.return n
    | Some log ->
      log Log_read (String.sub buf off n) >>= fun () ->
      Lwt.return n
  else
    Lwt.return 0

let write low buf off len =
  low._write buf off len >>= fun n ->
  if n > 0 then
    match low.logger with
    | None ->
      Lwt.return n
    | Some log ->
      log Log_write (String.sub buf off n) >>= fun () ->
      Lwt.return n
  else
    Lwt.return 0

let close low =
  low._close ()

let get_logger low =
  low.logger

let set_logger low logger =
  low.logger <- logger

let get_fd low =
  low._fd

let of_fd fd =
  {_read = Lwt_unix.read fd;
   _write = Lwt_unix.write fd;
   _close = (fun () -> Lwt_unix.close fd);
   _fd = Some fd;
   logger = None}

let string_read s ptr buf off len =
  let len = min (String.length s - !ptr) len in
  String.blit s !ptr buf off len;
  ptr := !ptr + len;
  Lwt.return len

let string_write s ptr buf off len =
  let len = min (String.length s - !ptr) len in
  String.blit buf off s !ptr len;
  ptr := !ptr + len;
  Lwt.return len

let of_string s =
  let ptr = ref 0 in
  {_read = string_read s ptr;
   _write = string_write s ptr;
   _close = (fun _ -> Lwt.return ());
   _fd = None;
   logger = None}

let wrap_call f () =
  try
    f ()
  with
  | Ssl.Connection_error err
  | Ssl.Accept_error err
  | Ssl.Read_error err
  | Ssl.Write_error err as e ->
    match err with
    | Ssl.Error_want_read ->
      raise Lwt_unix.Retry_read
    | Ssl.Error_want_write ->
      raise Lwt_unix.Retry_write
    | _ ->
      raise e

let repeat_call fd f =
  try
    Lwt_unix.check_descriptor fd;
    Lwt.return (wrap_call f ())
  with
  | Lwt_unix.Retry_read ->
    Lwt_unix.register_action Lwt_unix.Read fd (wrap_call f)
  | Lwt_unix.Retry_write ->
    Lwt_unix.register_action Lwt_unix.Write fd (wrap_call f)
  | e ->
    Lwt.fail e

let ssl_read fd sock buf off len =
  repeat_call fd (fun () ->
      try
        Ssl.read sock buf off len
      with
      | Ssl.Read_error Ssl.Error_zero_return -> 0)

let ssl_write fd sock buf off len =
  repeat_call fd (fun () -> Ssl.write sock buf off len)

let open_ssl ctx fd =
  let sock = Ssl.embed_socket (Lwt_unix.unix_file_descr fd) ctx in
  let connect () =
    repeat_call fd (fun () -> Ssl.connect sock)
  in
  {_read = ssl_read fd sock;
   _write = ssl_write fd sock;
   _close = (fun () -> Lwt_unix.close fd);
   _fd = Some fd;
   logger = None}, connect

let compress_chunk_size = 1024

type compress_data = {
  zistrm : Zlib.stream;
  zostrm : Zlib.stream;
  input_buf : string;
  output_buf : string;
  mutable avail_in : int;
  mutable next_in : int;
}

let compress_read strm cdata buf off len =
  let rec loop () =
    begin if cdata.avail_in = 0 then begin
        cdata.next_in <- 0;
        strm._read cdata.input_buf 0 compress_chunk_size
      end else
        Lwt.return cdata.avail_in
    end >>= fun avail_in ->
    if avail_in = 0 then
      Lwt.return (0, 0)
    else begin
      cdata.avail_in <- avail_in;
      let eof, used_in, used_out =
        Zlib.inflate cdata.zistrm cdata.input_buf cdata.next_in cdata.avail_in buf off len
          Zlib.Z_NO_FLUSH
      in
      cdata.next_in <- cdata.next_in + used_in;
      cdata.avail_in <- cdata.avail_in - used_in;
      if not eof && cdata.avail_in = 0 && used_out = 0 then
        loop ()
      else
        Lwt.return (used_in, used_out)
    end
  in
  loop () >>= fun (used_in, used_out) ->
  Lwt.return used_out

let compress_write strm cdata buf off len =
  let compress_len = min len compress_chunk_size in
  let _, used_in, used_out =
    Zlib.deflate cdata.zostrm buf off compress_len cdata.output_buf 0 compress_chunk_size
      Zlib.Z_SYNC_FLUSH
  in
  let rec loop off len =
    if len <= 0 then
      Lwt.return ()
    else
      strm._write cdata.output_buf off len >>= fun len' ->
      loop (off + len') (len - len')
  in
  loop 0 used_out >>= fun () ->
  Lwt.return used_in

let compress_close strm cdata =
  (* Zlib.inflate_end cdata.zistrm; *)
  (* Zlib.deflate_end cdata.zostrm; *)
  strm._close ()

let compress strm =
  let input_buf = String.create compress_chunk_size in
  let output_buf = String.create compress_chunk_size in
  let zistrm = Zlib.inflate_init false in
  let zostrm = Zlib.deflate_init 1 false in
  let cdata =
    {input_buf; output_buf; zistrm; zostrm; avail_in = 0; next_in = 0}
  in
  {_read = compress_read strm cdata;
   _write = compress_write strm cdata;
   _close = (fun () -> compress_close strm cdata); 
   _fd = strm._fd;
   logger = strm.logger}

type logger_state =
  | Last_was_cr
  | Last_was_nl
  | Last_was_other

(* The default logger echoes both input and output to [stderr].  In order to
   make them look a little nicer, new lines are preceded by "S: " or "C: "
   depending on the case.  Here a new line means either "\r\n" or "\n". *)
let default_logger =
  let rec aux r header str =
    let len = String.length str in
    let rec loop st off =
      if off >= len then
        Lwt.return st
      else
        let header = match st with
          | Last_was_nl -> header
          | Last_was_cr -> "\r"
          | Last_was_other -> ""
        in
        try
          let idx = String.index_from str off '\n' in
          if idx = off && st = Last_was_cr then
            Lwt_io.eprint "\n" >>= fun () -> loop Last_was_nl (off + 1)
          else
            let hascr = idx > 0 && str.[idx-1] = '\r' in
            let str' = String.sub str off (if hascr then idx-off-1 else idx-off) in
            Lwt_io.eprintf "%s%s\n" header str' >>= fun () ->
            loop Last_was_nl (idx+1)
        with
        | Not_found ->
          let hascr = len > 0 && str.[len-1] = '\r' in
          let str' = String.sub str off (if hascr then len-off-1 else len-off) in
          Lwt_io.eprintf "%s%s" header str' >>= fun () ->
          Lwt.return (if hascr then Last_was_cr else Last_was_other)
    in
    loop !r 0 >|= fun st -> r := st
  in
  let read_st = ref Last_was_nl in
  let write_st = ref Last_was_nl in
  fun direction str ->
    match direction with
    | Log_write ->
      aux write_st "C: " str
    | Log_read ->
      aux read_st "S: " str
