open OUnit2

(* let (>>=) = Lwt.(>>=) *)

let test_buf =
  "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

module U = struct
  type 'a t = 'a
  let bind t f = f t
  let return x = x
  let fail e = raise e
  let catch f g = try f () with e -> g e
end

module IO = Imap_gen_io.Make(U)
    
open IO

let input_of_file_descr fd =
  IO.create_in
    ~read:(Unix.read fd)
    ~close:(fun () -> Unix.close fd)

let output_of_file_descr fd =
  IO.create_out
    ~write:(Unix.write fd)
    ~close:(fun () -> Unix.close fd)
    ~flush:(fun () -> ())

(* let write_all low buf = *)
(*   let rec loop off len = *)
(*     if len <= 0 then Lwt.return () *)
(*     else *)
(*       write low buf off len >>= fun n -> *)
(*       (\* Lwt_log.error_f "Wrote %d bytes: %S\n%!" n (String.sub buf off n) >>= fun () -> *\) *)
(*       loop (off + n) (len - n) *)
(*   in *)
(*   loop 0 (String.length buf) *)

(* let read_all low = *)
(*   let stepsize = 16 in *)
(*   let buf = String.create 0 in *)
(*   let rec loop buf off len = (\* off + len = String.length buf *\) *)
(*     if len <= 0 then begin *)
(*       let buf' = String.create (String.length buf + stepsize) in *)
(*       String.blit buf 0 buf' 0 off; *)
(*       (\* Lwt_log.error_f "Resized buffer: old size: %d new size: %d\n" *\) *)
(*         (\* (String.length buf) (String.length buf') >>= fun () -> *\) *)
(*       (\* Lwt_log.error_f "old buf contents: %S\n%!" buf >>= fun () -> *\) *)
(*       loop buf' off stepsize *)
(*     end *)
(*     else *)
(*       read low buf off len >>= fun n -> *)
(*       (\* Lwt_log.error_f "Read %d bytes: %S" n (String.sub buf off n) >>= fun () -> *\) *)
(*       if n = 0 then *)
(*         Lwt.return (String.sub buf 0 off) *)
(*       else *)
(*         loop buf (off + n) (len - n) *)
(*   in *)
(*   loop buf 0 0 *)

let read_all ic =
  let b = Buffer.create 17 in
  let rec loop () =
    match read_char_opt ic with
    | Some ch ->
      Buffer.add_char b ch;
      loop ()
    | None ->
      Buffer.contents b
  in
  loop ()

let with_tmp_file f =
  let name = Filename.temp_file "test_chan-" "" in
  (* Lwt_log.error_f "Temp file: %S\n%!" name >>= fun () -> *)
  let fd = Unix.openfile name [Unix.O_CREAT; Unix.O_RDWR] 0o600 in
  let result = try `Ok (f fd) with e -> `Fail e in
  Unix.close fd;
  Unix.unlink name;
  match result with
  | `Ok x -> x
  | `Fail e -> raise e

let test_low f_in f_out ctxt =
  with_tmp_file
    (fun fd ->
       let ic = f_in (input_of_file_descr fd) in
       let oc = f_out (output_of_file_descr fd) in
       write oc test_buf;
       flush oc;
       (* Lwt_log.error "Write complete\n" >>= fun () -> *)
       ignore (Unix.lseek fd 0 Unix.SEEK_SET);
       let s = read_all ic in
       (* Lwt_log.error_f "Read complete: %S\n" s >>= fun () -> *)
       assert_equal ~ctxt s test_buf)

let test_fd ctxt =
  test_low (fun ic -> ic) (fun oc -> oc) ctxt

let test_compress ctxt =
  test_low inflate_input deflate_output ctxt

let test_buffered ctxt =
  test_low buffered_input buffered_output ctxt

let suite =
  (* let run f ctxt = Lwt_main.run (f ctxt) in *)
  "Chan" >:::
  [
    "test_fd" >:: test_fd;
    "test_compress" >:: test_compress;
    "test_buffered" >:: test_buffered
  ]

let () =
  run_test_tt_main suite
