module T = struct
  type 'a t = 'a Lwt.t
  let bind = Lwt.bind
  let return = Lwt.return
  let fail = Lwt.fail
  let catch = Lwt.catch
  module Mutex = struct
    include Lwt_mutex
    type mutex = t
  end
end

module U : Imap_io_low.UNIX with module IO = T = struct
  module IO = T
  include Lwt_unix
  let prerr_string = Lwt_io.eprint
end

module Lwtio = struct
  include T
  include Imap_io.Make (U)
end

include Imap.Make (Lwtio)

let (>>=) = Lwt.bind

let connect_simple s ?port host =
  let low, connect_ssl = Lwtio.Low.open_ssl () in
  connect_ssl ?port host >>= fun () ->
  let io = Lwtio.of_low low in
  connect s io

let compress s =
  let aux (ic, oc) =
    let low = Lwtio.get_low ic in
    let low = Lwtio.Low.compress low in
    Lwtio.set_low ic low;
    Lwtio.set_low oc low;
    Lwt.return (ic, oc)
  in
  compress s aux

let starttls ?ssl_context s =
  let aux (ic, oc) =
    let fd = match Lwtio.Low.get_fd (Lwtio.get_low ic) with
      | None -> failwith "starttls: no file descriptor"
      | Some fd -> fd
    in
    let low, connect = Lwtio.Low.open_tls ?ssl_context fd in
    connect () >>= fun () ->
    Lwtio.set_low ic low;
    Lwtio.set_low oc low;
    Lwt.return (ic, oc)
  in
  starttls s aux
