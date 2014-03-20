open OUnit2

let (>>=) = Lwt.(>>=)

let test_buf =
  "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

open Imap_io.Low

let write_all low buf =
  let rec loop off len =
    if len <= 0 then Lwt.return ()
    else
      low.write buf off len >>= fun n ->
      (* Lwt_log.error_f "Wrote %d bytes: %S\n%!" n (String.sub buf off n) >>= fun () -> *)
      loop (off + n) (len - n)
  in
  loop 0 (String.length buf)

let read_all low =
  let stepsize = 16 in
  let buf = String.create 0 in
  let rec loop buf off len = (* off + len = String.length buf *)
    if len <= 0 then begin
      let buf' = String.create (String.length buf + stepsize) in
      String.blit buf 0 buf' 0 off;
      (* Lwt_log.error_f "Resized buffer: old size: %d new size: %d\n" *)
        (* (String.length buf) (String.length buf') >>= fun () -> *)
      (* Lwt_log.error_f "old buf contents: %S\n%!" buf >>= fun () -> *)
      loop buf' off stepsize
    end
    else
      low.read buf off len >>= fun n ->
      (* Lwt_log.error_f "Read %d bytes: %S" n (String.sub buf off n) >>= fun () -> *)
      if n = 0 then
        Lwt.return (String.sub buf 0 off)
      else
        loop buf (off + n) (len - n)
  in
  loop buf 0 0

let with_tmp_file f =
  let name = Filename.temp_file "test_chan-" "" in
  (* Lwt_log.error_f "Temp file: %S\n%!" name >>= fun () -> *)
  Lwt_unix.openfile name [Unix.O_CREAT; Unix.O_RDWR] 0o600 >>= fun fd ->
  Lwt.finalize
    (fun () -> f fd)
    (fun () -> Lwt_unix.close fd >>= fun () -> Lwt_unix.unlink name)

let test_low f ctxt =
  with_tmp_file
    (fun fd ->
       let low = of_fd fd in
       let low = f low in
       write_all low test_buf >>= fun () ->
       (* Lwt_log.error "Write complete\n" >>= fun () -> *)
       Lwt_unix.lseek fd 0 Unix.SEEK_SET >>= fun _ ->
       read_all low >>= fun s ->
       (* Lwt_log.error_f "Read complete: %S\n" s >>= fun () -> *)
       assert_equal ~ctxt s test_buf;
       Lwt.return ())

let test_fd ctxt =
  test_low (fun low -> low) ctxt

let test_compress ctxt =
  test_low compress ctxt

let test_buffered ctxt =
  with_tmp_file
    (fun fd ->
       let low = of_fd fd in
       let io = Imap_io.of_low low in
       Imap_io.write io test_buf >>= fun () ->
       Imap_io.flush io >>= fun () ->
       Lwt_unix.lseek fd 0 Unix.SEEK_SET >>= fun _ ->
       Imap_io.read io >>= fun s ->
       assert_equal ~ctxt s test_buf;
       Lwt.return ())

let suite =
  let run f ctxt = Lwt_main.run (f ctxt) in
  "Chan" >:::
  [
    "test_fd" >:: run test_fd;
    "test_compress" >:: run test_compress;
    "test_buffered" >:: run test_buffered
  ]

let () =
  run_test_tt_main suite
