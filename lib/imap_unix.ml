module T = struct
  type 'a t = 'a
  let bind t f = f t
  let return x = x
  let fail e = raise e
  let catch f g = try f () with e -> g e
  module Mutex = struct
    type mutex = unit
    let create () = ()
    let is_locked () = false
    let with_lock () f = f ()
  end
end

module U : Imap_io_low.UNIX with module IO = T = struct
  module IO = T
  include Unix
  let unix_file_descr fd = fd
  let check_descriptor _ = ()
  exception Retry_read
  exception Retry_write
  type io_event =
    | Read
    | Write
  let register_action _ _ f = f ()
  let prerr_string = prerr_string
end

module Unix_io = struct
  include T
  include Imap_io.Make (U)
end

include Imap.Make (Unix_io)

let connect_simple s ?port host =
  let low, connect_ssl = Unix_io.Low.open_ssl () in
  connect_ssl ?port host;
  let io = Unix_io.of_low low in
  connect s io

let compress s =
  let aux (ic, oc) =
    let low = Unix_io.get_low ic in
    let low = Unix_io.Low.compress low in
    Unix_io.set_low ic low;
    Unix_io.set_low oc low;
    (ic, oc)
  in
  compress s aux

let starttls ?ssl_context s =
  let aux (ic, oc) =
    let fd = match Unix_io.Low.get_fd (Unix_io.get_low ic) with
      | None -> failwith "starttls: no file descriptor"
      | Some fd -> fd
    in
    let low, connect = Unix_io.Low.open_tls ?ssl_context fd in
    connect ();
    Unix_io.set_low ic low;
    Unix_io.set_low oc low;
    (ic, oc)
  in
  starttls s aux
