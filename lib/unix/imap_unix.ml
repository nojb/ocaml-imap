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

module Unix_IO = Imap_io.Make (Sync)

include Imap.Make (struct
    include Sync
    type input = Unix_IO.input
    type output = unit Unix_IO.output
    let read_line = Unix_IO.read_line
    let read_exactly ic len =
      let buf = String.create len in
      Unix_IO.read_into_exactly ic buf 0 len;
      buf
    let flush = Unix_IO.flush
    let write = Unix_IO.write
  end)

let ssl_input sock =
  let unsafe_read buf off len =
    let rec loop () =
      try
        Ssl.read sock buf off len
      with
      | Ssl.Read_error Ssl.Error_want_read ->
        loop ()
    in
    loop ()
  in
  let close () = Ssl.shutdown sock in
  Unix_IO.create_in
    ?underlying:None
    ?logger:None
    ~read:unsafe_read
    ~close

let ssl_output sock =
  let unsafe_write buf off len =
    let rec loop () =
      try
        Ssl.write sock buf off len
      with
      | Ssl.Write_error Ssl.Error_want_write ->
        loop ()
    in
    loop ()
  in
  let close () = Ssl.shutdown sock in
  let flush () = Ssl.flush sock in
  Unix_IO.create_out
    ?underlying:None
    ?logger:None
    ~write:unsafe_write
    ~close
    ~flush

let ssl_connect fd context =
  let sock = Ssl.embed_socket fd context in
  let rec loop () =
    try
      Ssl.connect sock
    with
    | Ssl.Connection_error Ssl.Error_want_connect ->
      loop ()
  in
  loop ();
  sock

let default_ssl_context =
  let () = Ssl.init () in
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  Ssl.set_verify ctx [Ssl.Verify_peer] None;
  ctx

let connect_simple s ?(port=993) host =
  (* let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in *)
  let he = Unix.gethostbyname host in
  (* Unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)); *)
  let sock = Ssl.open_connection Ssl.TLSv1 (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) in
  (* ssl_connect fd default_ssl_context in *)
  (* let ic = Unix_IO.buffered_input (ssl_input sock) in *)
  let ic = ssl_input sock in
  let oc = ssl_output sock in
  Unix_IO.set_logger_in ic Unix_IO.default_logger;
  Unix_IO.set_logger_out oc Unix_IO.default_logger;
  (* let oc = Unix_IO.buffered_output (ssl_output sock) in *)
  connect s (ic, oc)

let compress s =
  let aux (ic, oc) =
    (* let ic = Unix_IO.underlying_in ic in *)
    (* let oc = Unix_IO.underlying_out oc in *)
    let ic = (* Unix_IO.buffered_input *) (Unix_IO.inflate_input ic) in
    let oc = (* Unix_IO.buffered_output *) (Unix_IO.deflate_output oc) in
    (ic, oc)
  in
  compress s aux

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
