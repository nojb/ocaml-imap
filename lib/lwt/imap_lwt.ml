(* module T = struct *)
(* end *)

(* module U : Imap_io_low.UNIX with module IO = T = struct *)
(*   module IO = T *)
(*   include Lwt_unix *)
(*   let prerr_string = Lwt_io.eprint *)
(* end *)

module Lwtio = struct
  type 'a t = 'a Lwt.t
  let bind = Lwt.bind
  let return = Lwt.return
  let fail = Lwt.fail
  let catch = Lwt.catch
  module Mutex = struct
    include Lwt_mutex
    type mutex = t
  end
  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel
  let read_line = Lwt_io.read_line
  let read_into_exactly = Lwt_io.read_into_exactly
  let write = Lwt_io.write
  let flush = Lwt_io.flush
  (* let close = Lwt_io.close *)
  (* include Imap_io.Make (U) *)
end

include Imap.Make (Lwtio)

let (>>=) = Lwt.bind


let default_ssl_context =
  let () = Ssl.init () in
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  Ssl.set_verify ctx [Ssl.Verify_peer] None;
  ctx

let default_ssl_port = 993

let connect_simple s ?(port=default_ssl_port) host =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  Lwt_unix.connect fd (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >>= fun () ->
  Lwt_ssl.ssl_connect fd default_ssl_context >>= fun ssl_sock ->
  let ic = Lwt_ssl.in_channel_of_descr ssl_sock in
  let oc = Lwt_ssl.out_channel_of_descr ssl_sock in
  connect s (ic, oc)

(* let compress s = *)
(*   let aux (ic, oc) = *)
(*     let low = Lwtio.get_low ic in *)
(*     let low = Lwtio.Low.compress low in *)
(*     Lwtio.set_low ic low; *)
(*     Lwtio.set_low oc low; *)
(*     Lwt.return (ic, oc) *)
(*   in *)
(*   compress s aux *)

(* let starttls ?ssl_context s = *)
(*   let aux (ic, oc) = *)
(*     let fd = match Lwtio.Low.get_fd (Lwtio.get_low ic) with *)
(*       | None -> failwith "starttls: no file descriptor" *)
(*       | Some fd -> fd *)
(*     in *)
(*     let low, connect = Lwtio.Low.open_tls ?ssl_context fd in *)
(*     connect () >>= fun () -> *)
(*     Lwtio.set_low ic low; *)
(*     Lwtio.set_low oc low; *)
(*     Lwt.return (ic, oc) *)
(*   in *)
(*   starttls s aux *)
