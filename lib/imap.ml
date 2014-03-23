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

open Imap_utils
open Imap_types
open Imap_uint

let debug =
  try let s = Sys.getenv "IMAP_DEBUG" in ref (s <> "0")
  with Not_found -> ref false

let fresh_response_info = {
  rsp_alert = "";
  rsp_parse = "";
  rsp_badcharset = [];
  rsp_trycreate = false;
  rsp_mailbox_list = [];
  (* rsp_mailbox_lsub = []; *)
  rsp_search_results = [];
  rsp_search_results_modseq = Uint64.zero;
  rsp_status = {st_mailbox = ""; st_info_list = []};
  rsp_expunged = [];
  rsp_fetch_list = [];
  rsp_appenduid = (Uint32.zero, Uint32.zero);
  rsp_copyuid = (Uint32.zero, Imap_set.empty, Imap_set.empty);
  rsp_compressionactive = false;
  rsp_id = [];
  rsp_modified = Imap_set.empty;
  rsp_namespace = ([], [], []);
  rsp_enabled = [];
  rsp_other = ("", "")
}

let fresh_selection_info = {
  sel_perm_flags = [];
  sel_perm = `READ_ONLY;
  sel_uidnext = Uint32.zero;
  sel_uidvalidity = Uint32.zero;
  sel_first_unseen = Uint32.zero;
  sel_flags = [];
  sel_exists = None;
  sel_recent = None;
  sel_uidnotsticky = false;
  sel_highestmodseq = Uint64.zero
}

let fresh_capability_info =
  []

type connection_info = {
  chan : Imap_io.t;
  sock : Lwt_unix.file_descr;
  mutable next_tag : int;
  mutable imap_response : string;
  mutable rsp_info : response_info;
  mutable sel_info : selection_info;
  mutable cap_info : capability_info;
  mutable compress_deflate : bool;
  send_lock : Lwt_mutex.t
}

type connection_state =
  | Connected of connection_info
  | Disconnected

type session = {
  port : int option;
  host : string;
  mutable conn_state : connection_state
}

exception NO
exception BAD
exception BYE
exception Parse_error of string * int
exception Bad_tag
exception Not_connected
exception Io_error of exn
exception Auth_error of exn
    
let connection_info s =
  match s.conn_state with
  | Connected ci -> ci
  | Disconnected -> raise Not_connected

let default_port = 143
let default_ssl_port = 993

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

(* let test_ssl_context = *)
(*   let _ = Ssl.init () in *)
(*   let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in *)
(*   Ssl.set_verify ctx [Ssl.Verify_peer] None; *)
(*   ctx *)

let literal_re = Str.regexp "{\\([0-9]+\\)}$"

let is_literal s =
  try
    Str.search_backward literal_re s (String.length s - 1) |> ignore;
    Some (Str.matched_group 1 s |> int_of_string)
  with
  | Not_found -> None

let read_line io =
  let b = Buffer.create 64 in
  let rec loop () =
    lwt s = Imap_io.read_line io in
    Buffer.add_string b s;
    Buffer.add_string b "\r\n";
    match is_literal s with
    | Some len ->
      let buf = String.create len in
      Imap_io.read_into_exactly io buf 0 len >>= fun () ->
      Buffer.add_string b buf;
      loop ()
    | None ->
      Lwt.return (Buffer.contents b)
  in
  Lwt.catch loop (fun exn -> raise_lwt (Io_error exn))

let resp_text_store s (c, text) =
  match c with
  | `ALERT ->
    s.rsp_info <- {s.rsp_info with rsp_alert = text}
  | `BADCHARSET csets ->
    s.rsp_info <- {s.rsp_info with rsp_badcharset = csets}
  | `CAPABILITY caps ->
    s.cap_info <- caps
  | `PARSE ->
    s.rsp_info <- {s.rsp_info with rsp_parse = text}
  | `PERMANENTFLAGS flags ->
    s.sel_info <- {s.sel_info with sel_perm_flags = flags}
  | `READ_ONLY ->
    s.sel_info <- {s.sel_info with sel_perm = `READ_ONLY}
  | `READ_WRITE ->
    s.sel_info <- {s.sel_info with sel_perm = `READ_WRITE}
  | `TRYCREATE ->
    s.rsp_info <- {s.rsp_info with rsp_trycreate = true}
  | `UIDNEXT uid ->
    s.sel_info <- {s.sel_info with sel_uidnext = uid}
  | `UIDVALIDITY uid ->
    s.sel_info <- {s.sel_info with sel_uidvalidity = uid}
  | `UNSEEN unseen ->
    s.sel_info <- {s.sel_info with sel_first_unseen = unseen}
  | `APPENDUID (uidvalidity, uid) ->
    s.rsp_info <- {s.rsp_info with rsp_appenduid = (uidvalidity, uid)}
  | `COPYUID (uidvalidity, src_uids, dst_uids) ->
    s.rsp_info <- {s.rsp_info with rsp_copyuid = (uidvalidity, src_uids, dst_uids)}
  | `UIDNOTSTICKY ->
    s.sel_info <- {s.sel_info with sel_uidnotsticky = true}
  | `COMPRESSIONACTIVE ->
    s.rsp_info <- {s.rsp_info with rsp_compressionactive = true}
  | `HIGHESTMODSEQ modseq ->
    s.sel_info <- {s.sel_info with sel_highestmodseq = modseq}
  | `NOMODSEQ ->
    s.sel_info <- {s.sel_info with sel_highestmodseq = Uint64.zero}
  | `MODIFIED set ->
    s.rsp_info <- {s.rsp_info with rsp_modified = set}
  | `OTHER other ->
    s.rsp_info <- {s.rsp_info with rsp_other = other}
  | `NONE ->
    ()

let mailbox_data_store s = function
  | `FLAGS flags ->
    s.sel_info <- {s.sel_info with sel_flags = flags}
  | `LIST mb ->
    s.rsp_info <- {s.rsp_info with
                   rsp_mailbox_list = s.rsp_info.rsp_mailbox_list @ [mb]}
  | `LSUB mb ->
    s.rsp_info <- {s.rsp_info with
                    rsp_mailbox_list = s.rsp_info.rsp_mailbox_list @ [mb]}
                    (* rsp_mailbox_lsub = s.rsp_info.rsp_mailbox_lsub @ [mb] } *)
  | `SEARCH (results, modseq) ->
    s.rsp_info <- {s.rsp_info with
                   rsp_search_results = s.rsp_info.rsp_search_results @ results;
                   rsp_search_results_modseq =
                     Uint64.max modseq s.rsp_info.rsp_search_results_modseq}
  | `STATUS status ->
    s.rsp_info <- {s.rsp_info with rsp_status = status}
  | `EXISTS n ->
    s.sel_info <- {s.sel_info with sel_exists = Some n}
  | `RECENT n ->
    s.sel_info <- {s.sel_info with sel_recent = Some n}

let message_data_store s = function
  | `EXPUNGE n ->
    s.rsp_info <- {s.rsp_info with rsp_expunged = s.rsp_info.rsp_expunged @ [n]};
    begin match s.sel_info.sel_exists with
      | Some n ->
        s.sel_info <- {s.sel_info with sel_exists = Some (n-1)}
      | None ->
        ()
    end
  | `FETCH att ->
    s.rsp_info <- {s.rsp_info with rsp_fetch_list = s.rsp_info.rsp_fetch_list @ [att]}

let resp_cond_state_store s = function
  | `OK rt
  | `NO rt
  | `BAD rt ->
    resp_text_store s rt

let resp_cond_bye_store s = function
  | `BYE rt ->
    resp_text_store s rt

let response_data_store s = function
  | #Imap_response.resp_cond_state as resp ->
    resp_cond_state_store s resp
  | #Imap_response.resp_cond_bye as resp ->
    resp_cond_bye_store s resp
  | #Imap_response.mailbox_data as resp ->
    mailbox_data_store s resp
  | #Imap_response.message_data as resp ->
    message_data_store s resp
  | `CAPABILITY caps ->
    s.cap_info <- caps
  | `ID params ->
    s.rsp_info <- {s.rsp_info with rsp_id = params}
  | `NAMESPACE (pers, other, shared) ->
    s.rsp_info <- {s.rsp_info with rsp_namespace = pers, other, shared}
  | `ENABLED caps ->
    s.rsp_info <- {s.rsp_info with rsp_enabled = caps}

let response_tagged_store s (_, rcs) =
  resp_cond_state_store s rcs

let response_fatal_store s r =
  resp_cond_bye_store s r

let text_of_response_done = function
  | `TAGGED (_, `OK (_, txt))
  | `TAGGED (_, `BAD (_, txt))
  | `TAGGED (_, `NO (_, txt))
  | `BYE (_, txt) ->
    txt

let response_done_store s resp =
  s.imap_response <- text_of_response_done resp;
  match resp with
  | `TAGGED tagged ->
    response_tagged_store s tagged
  | #Imap_response.response_fatal as resp ->
    response_fatal_store s resp

let resp_data_or_resp_done_store s resp =
  match resp with
  | #Imap_response.response_data as resp ->
    response_data_store s resp
  | #Imap_response.response_done as resp ->
    response_done_store s resp

let resp_cond_auth_store s = function
  | `OK rt
  | `PREAUTH rt ->
    resp_text_store s rt

let greetings_store s = function
  | #Imap_response.resp_cond_auth as resp ->
    resp_cond_auth_store s resp
  | #Imap_response.resp_cond_bye as resp ->
    resp_cond_bye_store s resp

let cont_req_or_resp_data_or_resp_done_store s = function
  | `CONT_REQ _ ->
    ()
  | #Imap_response.response_data
  | #Imap_response.response_done as resp ->
    resp_data_or_resp_done_store s resp

let read ci p store =
  lwt s = read_line ci.chan in
  match Imap_parser.parse p s with
  | `Ok x -> store ci x; Lwt.return x
  | `Fail i -> raise_lwt (Parse_error (s, i))
  | `Exn exn -> raise_lwt exn

let read_greeting ci =
  read ci Imap_response.greeting greetings_store

let read_resp_data_or_resp_done ci =
  read ci
    Imap_response.resp_data_or_resp_done
    resp_data_or_resp_done_store

let read_cont_req_or_resp_data_or_resp_done ci =
  read ci
    Imap_response.cont_req_or_resp_data_or_resp_done
    cont_req_or_resp_data_or_resp_done_store

let get_response ci tag : Imap_response.resp_text Lwt.t =
  ci.rsp_info <- fresh_response_info;
  let rec loop () =
    match_lwt read_resp_data_or_resp_done ci with
    | `BYE _ -> (* FIXME change mode *)
      raise_lwt BYE
    | #Imap_response.response_data ->
      loop ()
    | `TAGGED (tag', `OK rt) ->
      if tag <> tag' then raise_lwt Bad_tag
      else Lwt.return rt
    | `TAGGED (_, `BAD rt) ->
      raise_lwt BAD
    | `TAGGED (_, `NO rt) ->
      raise_lwt NO
  in
  loop ()

let get_idle_response ci tag f stop =
  ci.rsp_info <- fresh_response_info;
  let rec loop () =
    read_cont_req_or_resp_data_or_resp_done ci >>= function
    | `BYE _ -> (* FIXME change mode *)
      raise_lwt BYE
    | #Imap_response.response_data ->
      begin match f () with
        | `Continue -> loop ()
        | `Stop -> stop (); loop ()
      end
    | `TAGGED (tag', `OK _) ->
      if tag <> tag' then raise_lwt Bad_tag
      else Lwt.return ()
    | `TAGGED (_, `BAD rt) ->
      raise_lwt BAD
    | `TAGGED (_, `NO rt) ->
      raise_lwt NO
    | `CONT_REQ _ ->
      loop ()
  in
  loop ()

let get_auth_response step ci tag =
  ci.rsp_info <- fresh_response_info;
  let rec loop needs_more =
    read_cont_req_or_resp_data_or_resp_done ci >>= function
    | `BYE _ -> (* FIXME change mode *)
      raise_lwt BYE
    | #Imap_response.response_data ->
      loop needs_more
    | `TAGGED (tag', `OK _) ->
      begin if needs_more then step "" else Lwt.return `OK end >>=
      begin function
        | `OK ->
          if tag <> tag' then raise_lwt Bad_tag
          else Lwt.return ()
        | `NEEDS_MORE ->
          raise_lwt (Auth_error (Failure "Insufficient data for SASL authentication"))
      end
    | `TAGGED (_, `BAD rt) ->
      raise_lwt BAD
    | `TAGGED (_, `NO rt) ->
      raise_lwt NO
    | `CONT_REQ data ->
      let data = match data with
        | `BASE64 data -> data
        | `TEXT _ -> ""
      in
      step data >>= function
      | `OK -> loop false
      | `NEEDS_MORE -> loop true
  in
  loop true

let get_continuation_request ci =
  read ci Imap_response.continue_req (fun _ _ -> ()) >|= function (`CONT_REQ x) -> x

let make ?port host =
  {port; host; conn_state = Disconnected}

let connect_lwt port host =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname host >>= fun he ->
  Lwt_unix.connect sock (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >|= fun () ->
  (Imap_io.of_low (Imap_io_low.of_fd sock), sock)

let ssl_connect_lwt io ssl_context =
  Imap_io.flush io >>= fun () ->
  let low = Imap_io.get_low io in
  match Imap_io_low.get_fd low with
  | None -> assert false
  | Some fd ->
    let low, connect = Imap_io_low.open_ssl ssl_context fd in
    connect () >>= fun () ->
    Imap_io_low.set_logger low (if !debug then Some Imap_io_low.default_logger else None);
    Imap_io.set_low io low;
    Lwt.return ()

let connect ?ssl_context s =
  match s.conn_state with
  | Disconnected ->
    let port = match ssl_context, s.port with
      | None, None -> default_port
      | _, Some port -> port
      | Some _, None -> default_ssl_port
    in
    connect_lwt port s.host >>= fun (io, sock) ->
    begin match ssl_context with
      | Some ssl_context ->
        ssl_connect_lwt io ssl_context
      | None ->
        Lwt.return ()
    end >>= fun () ->
    let ci = {
      chan = io; sock; next_tag = 1;
      imap_response = "";
      rsp_info = fresh_response_info;
      sel_info = fresh_selection_info;
      cap_info = fresh_capability_info;
      compress_deflate = false;
      send_lock = Lwt_mutex.create ()
    }
    in
    s.conn_state <- Connected ci;
    begin match_lwt read_greeting ci with
      | `BYE _ ->
        Lwt_unix.close sock >>= fun () ->
        s.conn_state <- Disconnected;
        raise_lwt BYE
      | `OK _ ->
        Lwt.return `Needsauth
      | `PREAUTH _ ->
        Lwt.return `Preauth
    end
  | _ ->
    raise_lwt (Failure "Imap.connect: already connected")

let disconnect s =
  match s.conn_state with
  | Disconnected -> ()
  | Connected ci ->
    Lwt.async (fun () -> Lwt.catch (fun () -> Imap_io.close ci.chan) (fun _ -> Lwt.return ()));
    s.conn_state <- Disconnected

let generate_tag s =
  let tag = s.next_tag in
  s.next_tag <- s.next_tag + 1;
  string_of_int tag

module S = Imap_sender

let run_sender ci (f : S.t) =
  let rec loop f =
    f (Imap_io.write ci.chan) >>= function
    | `Ok ->
      Imap_io.flush ci.chan
    | `Cont_req cont ->
      Imap_io.flush ci.chan >>= fun () ->
      get_continuation_request ci >>= fun _ ->
      loop cont
  in
  Lwt.catch (fun () -> loop f) (fun exn -> raise_lwt (Io_error exn))

let send_command' ci f =
  let tag = generate_tag ci in
  let f = S.(raw tag @> space @> f @> crlf) in
  run_sender ci f >|= fun () -> tag

let send_command ci f =
  send_command' ci f >>= fun tag ->
  get_response ci tag >|= fun _ -> ()
  
let capability s =
  let ci = connection_info s in
  let cmd = S.raw "CAPABILITY" in
  let aux () =
    send_command ci cmd >|= fun () -> ci.cap_info
  in
  Lwt_mutex.with_lock ci.send_lock aux

let noop s =
  let ci = connection_info s in
  let cmd = S.raw "NOOP" in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let logout s =
  let ci = connection_info s in
  let cmd = S.raw "LOGOUT" in
  let aux () =
    Lwt.catch
      (fun () -> send_command ci cmd)
      (function
        | BYE -> s.conn_state <- Disconnected; Lwt.return ()
        | exn -> raise_lwt exn)
  in
  Lwt_mutex.with_lock ci.send_lock aux

let id s params =
  let ci = connection_info s in
  let cmd = match params with
    | [] -> S.(raw "ID" @> space @> nil)
    | _ -> S.(raw "ID" @> space @> list (separated_pair string space string) params)
  in
  let aux () =
    send_command ci cmd >>= fun () ->
    Lwt.return ci.rsp_info.rsp_id
  in
  Lwt_mutex.with_lock ci.send_lock aux

let enable s caps =
  let ci = connection_info s in
  let string_of_capability = function
    | `AUTH_TYPE s -> "AUTH=" ^ s
    | `NAME s -> s
  in
  let cmd =
    S.(raw "ENABLE " @> List.fold_right
         (fun cap rest -> raw (string_of_capability cap) @> rest) caps null)
  in
  let aux () =
    send_command ci cmd >>= fun () ->
    Lwt.return ci.rsp_info.rsp_enabled
  in
  Lwt_mutex.with_lock ci.send_lock aux

let starttls s ssl_context =
  let ci = connection_info s in
  let cmd = S.raw "STARTTLS" in
  let aux () =
    if ci.compress_deflate then
      raise_lwt (Failure "starttls: compression active")
    else
      send_command ci cmd >>= fun () ->
      ssl_connect_lwt ci.chan ssl_context >|= fun () ->
      ci.cap_info <- fresh_capability_info (* See 6.2.1 in RFC 3501 *)
  in
  Lwt_mutex.with_lock ci.send_lock aux

let authenticate s auth =
  let ci = connection_info s in
  let cmd = S.(raw "AUTHENTICATE" @> space @> string auth.Imap_auth.name) in
  let step data =
    let data = Imap_utils.base64_decode data in
    begin
      try Lwt.return (auth.Imap_auth.step data)
      with e -> Lwt.fail (Auth_error e)
    end >>= fun (rc, data) ->
    let data = Imap_utils.base64_encode data in
    run_sender ci S.(raw data @> crlf) >>= fun () ->
    Lwt.return rc
  in
  let aux () =
    send_command' ci cmd >>= get_auth_response step ci
  in
  Lwt_mutex.with_lock ci.send_lock aux

let login s user pass =
  let ci = connection_info s in
  let cmd = S.(raw "LOGIN" @> space @> string user @> space @> string pass) in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let compress s =
  let ci = connection_info s in
  let cmd = S.raw "COMPRESS DEFLATE" in
  let aux () =
    send_command ci cmd >>= fun () ->
    let low = Imap_io.get_low ci.chan in
    let low = Imap_io_low.compress low in
    Imap_io.set_low ci.chan low;
    ci.compress_deflate <- true;
    Lwt.return ()
  in
  Lwt_mutex.with_lock ci.send_lock aux

let select_aux s cmd ?use_condstore:(use_condstore=false) mbox =
  let ci = connection_info s in
  let send_condstore =
    if use_condstore then S.(space @> raw "(CONDSTORE)") else S.null
  in
  let cmd = S.(raw cmd @> space @> mailbox mbox @> send_condstore) in
  let aux () =
    ci.sel_info <- fresh_selection_info;
    send_command ci cmd >|= fun () -> ci.sel_info.sel_highestmodseq
  in
  Lwt_mutex.with_lock ci.send_lock aux

let select_condstore s mbox =
  select_aux s "SELECT" ~use_condstore:true mbox

let select s mbox =
  select_aux s "SELECT" ~use_condstore:false mbox >>= fun _ ->
  Lwt.return ()

let examine_condstore s mbox =
  select_aux s "EXAMINE" ~use_condstore:true mbox

let examine s mbox =
  select_aux s "EXAMINE" ~use_condstore:false mbox >>= fun _ ->
  Lwt.return ()

let create s mbox =
  let ci = connection_info s in
  let cmd = S.(raw "CREATE" @> space @> mailbox mbox) in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let delete s mbox =
  let ci = connection_info s in
  let cmd = S.(raw "DELETE" @> space @> mailbox mbox) in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let rename s oldbox newbox =
  let ci = connection_info s in
  let cmd = S.(raw "RENAME" @> space @> mailbox oldbox @> space @> mailbox newbox) in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let subscribe s mbox =
  let ci = connection_info s in
  let cmd = S.(raw "SUBSCRIBE" @> space @> mailbox mbox) in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let unsubscribe s mbox =
  let ci = connection_info s in
  let cmd = S.(raw "UNSUBSCRIBE" @> space @> mailbox mbox) in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let list_aux s cmd mbox list_mb =
  let ci = connection_info s in
  let cmd = S.(raw cmd @> space @> mailbox mbox @> space @> mailbox list_mb) in
  let aux () =
    send_command ci cmd >|= fun () -> ci.rsp_info.rsp_mailbox_list
  in
  Lwt_mutex.with_lock ci.send_lock aux

let list s mbox list_mb =
  list_aux s "LIST" mbox list_mb

let lsub s mbox list_mb =
  list_aux s "LSUB" mbox list_mb

let status s mbox attrs =
  let ci = connection_info s in
  let cmd =
    S.(raw "STATUS" @> space @> mailbox mbox @> space @> list status_att attrs)
  in
  let aux () =
    send_command ci cmd >|= fun () -> ci.rsp_info.rsp_status
  in
  Lwt_mutex.with_lock ci.send_lock aux

let append_uidplus s mbox ?flags ?date data =
  let ci = connection_info s in
  let flags = match flags with
    | None | Some [] -> S.null
    | Some flags -> S.(list flag flags @> space)
  in
  let date = match date with
    | None -> S.null
    | Some dt -> S.(date_time dt @> space)
  in
  let cmd =
    S.(raw "APPEND" @> space @> mailbox mbox @> space @>
       flags @> date @> literal data)
  in
  let aux () =
    send_command ci cmd >|= fun () -> ci.rsp_info.rsp_appenduid
  in
  Lwt_mutex.with_lock ci.send_lock aux

let append s mbox ?flags ?date data =
  append_uidplus s mbox ?flags ?date data >>= fun _ ->
  Lwt.return ()

let idle s f =
  let ci = connection_info s in
  let cmd = S.raw "IDLE" in
  let idling = ref false in
  let stop () =
    if !idling then
      idling := false;
      Lwt.async (fun () ->
          try_lwt
            run_sender ci S.(raw "DONE" @> crlf)
          with
          | _ -> Lwt.return ())
  in
  let aux () =
    ci.sel_info <- {ci.sel_info with sel_exists = None; sel_recent = None};
    send_command' ci cmd >>= fun tag ->
    idling := true;
    get_idle_response ci tag f stop
  in
  Lwt_mutex.with_lock ci.send_lock aux, stop

let namespace s =
  let ci = connection_info s in
  let cmd = S.raw "NAMESPACE" in
  let aux () =
    send_command ci cmd >>= fun () ->
    Lwt.return ci.rsp_info.rsp_namespace
  in
  Lwt_mutex.with_lock ci.send_lock aux

let check s =
  let ci = connection_info s in
  let cmd = S.raw "CHECK" in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let close s =
  let ci = connection_info s in
  let cmd = S.raw "CLOSE" in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let expunge s =
  let ci = connection_info s in
  let cmd = S.raw "EXPUNGE" in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let uid_expunge s set =
  lwt () = assert_lwt (not (Imap_set.mem_zero set)) in
  let ci = connection_info s in
  let cmd = S.(raw "UID EXPUNGE" @> space @> message_set set) in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let search_aux s cmd ?charset query =
  let ci = connection_info s in
  let charset_opt = match charset with
    | None -> S.null
    | Some charset -> S.(string charset @> space)
  in
  let cmd = S.(raw cmd @> space @> charset_opt @> search_key query) in
  let aux () =
    send_command ci cmd >|= fun () -> ci.rsp_info.rsp_search_results
  in
  Lwt_mutex.with_lock ci.send_lock aux

let search s ?charset query =
  search_aux s "SEARCH" ?charset query

let uid_search s ?charset query =
  search_aux s "UID SEARCH" ?charset query

let fetch_aux cmd s set changedsince attrs =
  let ci = connection_info s in
  let changedsince = match changedsince with
    | None -> S.null
    | Some modseq -> S.(space @> raw "(CHANGEDSINCE " @> uint64 modseq @> raw ")")
  in
  let cmd =
    S.(raw cmd @> space @> message_set set @> space @>
       list fetch_att attrs @> changedsince)
  in
  let aux () =
    send_command ci cmd >|= fun () -> ci.rsp_info.rsp_fetch_list
  in
  Lwt_mutex.with_lock ci.send_lock aux

let fetch s set ?changedsince attrs =
  fetch_aux "FETCH" s set changedsince attrs

let uid_fetch s set ?changedsince attrs =
  fetch_aux "UID FETCH" s set changedsince attrs

let store_aux cmd s set unchangedsince mode att =
  let ci = connection_info s in
  let unchangedsince = match unchangedsince with
    | None -> S.null
    | Some modseq -> S.(raw "(UNCHANGEDSINCE " @> uint64 modseq @> raw ") ")
  in
  let mode = match mode with
    | `Add -> S.raw "+"
    | `Set -> S.null
    | `Remove -> S.raw "-"
  in
  let cmd =
    S.(raw cmd @> space @> message_set set @> space @>
       unchangedsince @> mode @> store_att att)
  in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let store s set ?unchangedsince mode flags =
  store_aux "STORE" s set unchangedsince mode flags

let uid_store s set ?unchangedsince mode flags =
  store_aux "UID STORE" s set unchangedsince mode flags

let copy_aux cmd s set destbox =
  let ci = connection_info s in
  let cmd = S.(raw cmd @> space @> message_set set @> space @> mailbox destbox) in
  let aux () = send_command ci cmd in
  Lwt_mutex.with_lock ci.send_lock aux

let copy s set destbox =
  copy_aux "COPY" s set destbox

let uid_copy s set destbox =
  copy_aux "UID COPY" s set destbox

let has_capability_name s name =
  let ci = connection_info s in
  List.exists (function
      | `NAME x -> Imap_utils.compare_ci x name = 0
      | `AUTH_TYPE _ -> false) ci.cap_info

let has_uidplus s =
  has_capability_name s "UIDPLUS"

let has_compress_deflate s =
  has_capability_name s "COMPRESS=DEFLATE"

let has_id s =
  has_capability_name s "ID"

let has_condstore s =
  has_capability_name s "CONDSTORE"

let has_x_gm_ext_1 s =
  has_capability_name s "X-GM-EXT-1"

let has_namespace s =
  has_capability_name s "NAMESPACE"

let has_enable s =
  has_capability_name s "ENABLE"

let last_response s =
  let ci = connection_info s in
  ci.imap_response

let response_info s =
  let ci = connection_info s in
  ci.rsp_info

let selection_info s =
  let ci = connection_info s in
  ci.sel_info

let capability_info s =
  let ci = connection_info s in
  ci.cap_info

let is_busy s =
  let ci = connection_info s in
  Lwt_mutex.is_locked ci.send_lock
