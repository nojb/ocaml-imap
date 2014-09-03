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

open ImapUtils
open ImapTypes
  
let debug =
  try let s = Sys.getenv "IMAP_DEBUG" in ref (s <> "0")
  with Not_found -> ref false

external uint32_set_to_seq_set : Uint32_set.t -> Seq_set.t = "%identity"
external seq_set_to_uint32_set : Seq_set.t -> Uint32_set.t = "%identity"
external uint32_set_to_uid_set : Uint32_set.t -> Uid_set.t = "%identity"
external uid_set_to_uint32_set : Uid_set.t -> Uint32_set.t = "%identity"
external uint32_list_to_seq_list : Uint32.t list -> Seq.t list = "%identity"
external uint32_list_to_uid_list : Uint32.t list -> Uid.t list = "%identity"

module Make (IO : IO.S) = struct
  module IO = IO

  open ImapState

  type connection_info = {
    mutable chan : IO.input * IO.output;
    (* reader : ImapReader.reader; *)
    mutable next_tag : int;
    mutable compress_deflate : bool;
    mutable state : state;
    send_lock : IO.mutex;
    disconnect : unit -> unit IO.t;
    buffer : Buffer.t;
    mutable i : int
  }

  type connection_state =
    | Connected of connection_info
    | Disconnected

  type session = {
    mutable conn_state : connection_state
  }

  exception NO
  exception BAD
  exception BYE
  exception Parse_error of string * int
  exception Bad_tag
  exception Io_error of exn
  exception Auth_error of exn

  let connection_info s =
    match s.conn_state with
    | Connected ci -> ci
    | Disconnected -> failwith "Imap.Client: not connected"

  let (>>=) = IO.bind
  let (>|=) t f = IO.bind t (fun x -> IO.return (f x))

  (* let parse_response ci = *)
  (*   let rec loop = *)
  (*     function *)
  (*       ImapParser.Ok (r, i) -> *)
  (*         ci.i <- i; *)
  (*         IO.return r *)
  (*     | ImapParser.Fail i -> *)
  (*         IO.fail (Parse_error (Buffer.contents ci.buffer, i)) *)
  (*     | ImapParser.Need (len, k) -> *)
  (*         let ic, _  = ci.chan in *)
  (*         IO.read 65536 ic >>= *)
  (*         begin *)
  (*           function *)
  (*             "" -> *)
  (*               loop (k ImapParser.End) *)
  (*           | _ as data -> *)
  (*               Buffer.add_string ci.buffer data; *)
  (*               loop (k ImapParser.More) *)
  (*         end *)
  (*   in *)
  (*   loop (ImapParser.response ci.buffer ci.i) >>= fun r -> *)
  (*   ci.state <- ImapState.response_store ci.state r; *)
  (*   let t = *)
  (*     match r.rsp_resp_done with *)
  (*       RESP_DONE_TAGGED {rsp_cond_state = {rsp_text = t}} *)
  (*     | RESP_DONE_FATAL t -> t *)
  (*   in *)
  (*   ci.imap_response <- t; *)
  (*   match r.rsp_resp_done with *)
  (*     RESP_DONE_TAGGED {rsp_tag = tag} -> *)
  (*       if tag = ci.imap_tag then failwith "bad tag"; *)
  (*       Lwt.return r *)
  (*   | RESP_DONE_FATAL _ -> *)
  (*       failwith "fatal!" *)

  let read ci p store =
    let rec loop =
      function
        ImapParser.Ok (x, i) ->
          ci.i <- i;
          store ci.state x;
          IO.return x
      | ImapParser.Fail i ->
          IO.fail (Parse_error (Buffer.contents ci.buffer, i))
      | ImapParser.Need (len, k) ->
          let ic, _  = ci.chan in
          IO.read 65536 ic >>=
          begin
            function
              "" ->
                loop (k ImapParser.End)
            | _ as data ->
                Buffer.add_string ci.buffer data;
                loop (k ImapParser.More)
          end
    in
    loop (p ci.buffer ci.i)

  let read_greeting ci =
    assert false
  (*   read ci ImapResponse.greeting greetings_store *)

  let get_response ci tag : ImapResponse.resp_text IO.t =
    assert false
  (*   ci.state <- {ci.state with rsp_info = fresh_response_info}; *)
  (*   let rec loop () = *)
  (*     read_resp_data_or_resp_done ci >>= *)
  (*     function *)
  (*       `BYE _ -> *)
  (*         ci.disconnect () >>= fun () -> *)
  (*         IO.fail BYE *)
  (*     | #ImapResponse.response_data -> *)
  (*         loop () *)
  (*     | `TAGGED (tag', `OK rt) -> *)
  (*         if tag <> tag' then IO.fail Bad_tag *)
  (*         else IO.return rt *)
  (*     | `TAGGED (_, `BAD rt) -> *)
  (*         IO.fail BAD *)
  (*     | `TAGGED (_, `NO rt) -> *)
  (*         IO.fail NO *)
  (*   in *)
  (*   loop () *)

  let get_idle_response ci tag f stop =
    assert false
  (*   ci.state <- {ci.state with rsp_info = fresh_response_info}; *)
  (*   let rec loop () = *)
  (*     read_cont_req_or_resp_data_or_resp_done ci >>= *)
  (*     function *)
  (*       `BYE _ -> *)
  (*         ci.disconnect () >>= fun () -> *)
  (*         IO.fail BYE *)
  (*     | #ImapResponse.response_data -> *)
  (*         begin *)
  (*           match f () with *)
  (*             `Continue -> loop () *)
  (*           | `Stop -> stop (); loop () *)
  (*         end *)
  (*     | `TAGGED (tag', `OK _) -> *)
  (*         if tag <> tag' then IO.fail Bad_tag *)
  (*         else IO.return () *)
  (*     | `TAGGED (_, `BAD rt) -> *)
  (*         IO.fail BAD *)
  (*     | `TAGGED (_, `NO rt) -> *)
  (*         IO.fail NO *)
  (*     | `CONT_REQ _ -> *)
  (*         loop () *)
  (*   in *)
  (*   loop () *)

  let get_auth_response step ci tag =
    assert false
  (*   ci.state <- {ci.state with rsp_info = fresh_response_info}; *)
  (*   let rec loop needs_more = *)
  (*     read_cont_req_or_resp_data_or_resp_done ci >>= *)
  (*     function *)
  (*       `BYE _ -> *)
  (*         ci.disconnect () >>= fun () -> *)
  (*         IO.fail BYE *)
  (*     | #ImapResponse.response_data -> *)
  (*         loop needs_more *)
  (*     | `TAGGED (tag', `OK _) -> *)
  (*         begin *)
  (*           if needs_more then step "" else IO.return `OK end >>= begin *)
  (*           function *)
  (*             `OK -> *)
  (*               if tag <> tag' then IO.fail Bad_tag *)
  (*               else IO.return () *)
  (*           | `NEEDS_MORE -> *)
  (*               IO.fail (Auth_error (Failure "Insufficient data for SASL authentication")) *)
  (*         end *)
  (*     | `TAGGED (_, `BAD rt) -> *)
  (*         IO.fail BAD *)
  (*     | `TAGGED (_, `NO rt) -> *)
  (*         IO.fail NO *)
  (*     | `CONT_REQ data -> *)
  (*         let data = *)
  (*           match data with *)
  (*             `BASE64 data -> data *)
  (*           | `TEXT _ -> "" *)
  (*         in *)
  (*         step data >>= *)
  (*         function *)
  (*           `OK -> loop false *)
  (*         | `NEEDS_MORE -> loop true *)
  (*   in *)
  (*   loop true *)

  let get_continuation_request ci =
    assert false
    (* read ci ImapResponse.continue_req (fun _ _ -> ()) >|= function (`CONT_REQ x) -> x *)

  let make () =
    { conn_state = Disconnected }

  let connect' s chan =
    let disconnect' chan =
      s.conn_state <- Disconnected;
      IO.catch (fun () -> IO.disconnect chan) (fun _ -> IO.return ())
    in
    match s.conn_state with
    | Disconnected ->
      let ci = {
        chan; next_tag = 1;
        buffer = Buffer.create 0;
        i = 0;
        (* reader = ImapReader.create (); *)
        state = {
          imap_response = "";
          rsp_info = fresh_response_info;
          sel_info = fresh_selection_info;
          cap_info = []
        };
        compress_deflate = false;
        send_lock = IO.create_mutex ();
        disconnect = (fun () -> disconnect' chan)
      }
      in
      s.conn_state <- Connected ci;
      read_greeting ci >>= begin function
        | `BYE _ ->
          ci.disconnect () >>= fun () ->
          IO.fail BYE
        | `OK _ ->
          IO.return `Needsauth
        | `PREAUTH _ ->
          IO.return `Preauth
      end
    | _ ->
      IO.fail (Failure "Imap.connect: already connected")

  let connect s ?(port=143) host =
    IO.connect port host >>= connect' s

  let connect_ssl s ?(version=`TLSv1) ?ca_file ?(port=993) host =
    IO.connect_ssl version ?ca_file port host >>= connect' s

  let disconnect s =
    match s.conn_state with
    | Disconnected -> ()
    | Connected ci ->
      let (_ : unit IO.t) = ci.disconnect () in
      ()

  let generate_tag s =
    let tag = s.next_tag in
    s.next_tag <- s.next_tag + 1;
    string_of_int tag

  module S = ImapWriter

  let run_sender ci (f : S.t) =
    let _, oc = ci.chan in
    IO.catch
      (fun () ->
         S.fold (fun io a ->
             match a with
             | `Raw s ->
               io >>= fun () -> IO.write oc s
             | `Cont_req ->
               io >>= fun () -> IO.flush oc >>= fun () ->
               get_continuation_request ci >>= fun _ ->
               IO.return ()) (IO.return ()) f >>= fun () ->
         IO.flush oc)
      (fun e -> ci.disconnect () >>= fun () -> IO.fail (Io_error e))

  let send_command' ci f =
    let tag = generate_tag ci in
    let f = S.(raw tag ++ space ++ f ++ crlf) in
    run_sender ci f >|= fun () -> tag

  let send_command ci f =
    send_command' ci f >>= fun tag ->
    get_response ci tag >|= fun _ -> ()

  let capability s =
    let ci = connection_info s in
    let cmd = S.raw "CAPABILITY" in
    let aux () =
      send_command ci cmd >|= fun () -> ci.state.cap_info
    in
    IO.with_lock ci.send_lock aux

  let noop s =
    let ci = connection_info s in
    let cmd = S.raw "NOOP" in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let logout s =
    let ci = connection_info s in
    let cmd = S.raw "LOGOUT" in
    let aux () =
      IO.catch
        (fun () -> send_command ci cmd)
        (function
          | BYE -> ci.disconnect ()
          | exn -> IO.fail exn)
    in
    IO.with_lock ci.send_lock aux

  let id s params =
    let ci = connection_info s in
    let cmd = match params with
      | [] -> S.(raw "ID" ++ space ++ nil)
      | _ -> S.(raw "ID" ++ space ++ list (separated_pair string space string) params)
    in
    let aux () =
      send_command ci cmd >>= fun () ->
      IO.return ci.state.rsp_info.rsp_id
    in
    IO.with_lock ci.send_lock aux

  let enable s caps =
    let ci = connection_info s in
    let string_of_capability =
      function
        CAPABILITY_AUTH_TYPE s -> "AUTH=" ^ s
      | CAPABILITY_NAME s -> s
    in
    let cmd =
      S.(raw "ENABLE " ++ List.fold_right
           (fun cap rest -> raw (string_of_capability cap) ++ rest) caps null)
    in
    let aux () =
      send_command ci cmd >>= fun () ->
      IO.return ci.state.rsp_info.rsp_enabled
    in
    IO.with_lock ci.send_lock aux

  let starttls ?(version = `TLSv1) ?ca_file s =
    let ci = connection_info s in
    let cmd = S.raw "STARTTLS" in
    let aux () =
      if ci.compress_deflate then
        IO.fail (Failure "starttls: compression active")
      else
        send_command ci cmd >>= fun () ->
        IO.starttls version ?ca_file ci.chan >>= begin fun chan ->
          ci.chan <- chan;
          ci.state <- {ci.state with cap_info = []}; (* See 6.2.1 in RFC 3501 *)
          IO.return ()
        end
    in
    IO.with_lock ci.send_lock aux

  let authenticate s auth =
    let ci = connection_info s in
    let cmd = S.(raw "AUTHENTICATE" ++ space ++ string auth.ImapAuth.name) in
    let step data =
      let data = ImapUtils.base64_decode data in
      begin
        try IO.return (auth.ImapAuth.step data)
        with e -> IO.fail (Auth_error e)
      end >>= fun (rc, data) ->
      let data = ImapUtils.base64_encode data in
      run_sender ci S.(raw data ++ crlf) >>= fun () ->
      IO.return rc
    in
    let aux () =
      send_command' ci cmd >>= get_auth_response step ci
    in
    IO.with_lock ci.send_lock aux

  let login s user pass =
    let ci = connection_info s in
    let cmd = S.(raw "LOGIN" ++ space ++ string user ++ space ++ string pass) in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let compress s =
    let ci = connection_info s in
    let cmd = S.raw "COMPRESS DEFLATE" in
    let aux () =
      send_command ci cmd >>= fun () ->
      let chan = IO.compress ci.chan in
      ci.chan <- chan;      
      ci.compress_deflate <- true;
      IO.return ()
    in
    IO.with_lock ci.send_lock aux

  let select_aux s cmd ?use_condstore:(use_condstore=false) mbox =
    let ci = connection_info s in
    let send_condstore =
      if use_condstore then S.(space ++ raw "(CONDSTORE)") else S.null
    in
    let cmd = S.(raw cmd ++ space ++ mailbox mbox ++ send_condstore) in
    let aux () =
      ci.state <- {ci.state with sel_info = fresh_selection_info};
      send_command ci cmd >|= fun () -> ci.state.sel_info.sel_highestmodseq
    in
    IO.with_lock ci.send_lock aux

  let select_condstore s mbox =
    select_aux s "SELECT" ~use_condstore:true mbox

  let select s mbox =
    select_aux s "SELECT" ~use_condstore:false mbox >>= fun _ ->
    IO.return ()

  let examine_condstore s mbox =
    select_aux s "EXAMINE" ~use_condstore:true mbox

  let examine s mbox =
    select_aux s "EXAMINE" ~use_condstore:false mbox >>= fun _ ->
    IO.return ()

  let create s mbox =
    let ci = connection_info s in
    let cmd = S.(raw "CREATE" ++ space ++ mailbox mbox) in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let delete s mbox =
    let ci = connection_info s in
    let cmd = S.(raw "DELETE" ++ space ++ mailbox mbox) in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let rename s oldbox newbox =
    let ci = connection_info s in
    let cmd = S.(raw "RENAME" ++ space ++ mailbox oldbox ++ space ++ mailbox newbox) in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let subscribe s mbox =
    let ci = connection_info s in
    let cmd = S.(raw "SUBSCRIBE" ++ space ++ mailbox mbox) in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let unsubscribe s mbox =
    let ci = connection_info s in
    let cmd = S.(raw "UNSUBSCRIBE" ++ space ++ mailbox mbox) in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let list_aux s cmd mbox list_mb =
    let ci = connection_info s in
    let cmd = S.(raw cmd ++ space ++ mailbox mbox ++ space ++ mailbox list_mb) in
    let aux () =
      send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_mailbox_list
    in
    IO.with_lock ci.send_lock aux

  let list s mbox list_mb =
    list_aux s "LIST" mbox list_mb

  let lsub s mbox list_mb =
    list_aux s "LSUB" mbox list_mb

  let status s mbox attrs =
    let ci = connection_info s in
    let cmd =
      S.(raw "STATUS" ++ space ++ mailbox mbox ++ space ++ list status_att attrs)
    in
    let aux () =
      send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_status
    in
    IO.with_lock ci.send_lock aux

  let append_uidplus s mbox ?flags ?date data =
    let ci = connection_info s in
    let flags = match flags with
      | None | Some [] -> S.null
      | Some flags -> S.(list flag flags ++ space)
    in
    let date = match date with
      | None -> S.null
      | Some dt -> S.(date_time dt ++ space)
    in
    let cmd =
      S.(raw "APPEND" ++ space ++ mailbox mbox ++ space ++ flags ++ date ++ literal data)
    in
    let aux () =
      send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_appenduid
    in
    IO.with_lock ci.send_lock aux

  let append s mbox ?flags ?date data =
    append_uidplus s mbox ?flags ?date data >>= fun _ ->
    IO.return ()

  let idle s f =
    let ci = connection_info s in
    let cmd = S.raw "IDLE" in
    let idling = ref false in
    let stop () =
      if !idling then begin
        idling := false;
        ignore (IO.catch
                  (fun () -> run_sender ci S.(raw "DONE" ++ crlf))
                  (fun _ -> IO.return ()))
      end
    in
    let aux () =
      ci.state <- {ci.state with sel_info = {ci.state.sel_info with sel_exists = None; sel_recent = None}};
      send_command' ci cmd >>= fun tag ->
      idling := true;
      get_idle_response ci tag f stop
    in
    IO.with_lock ci.send_lock aux, stop

  let namespace s =
    assert false
    (* let ci = connection_info s in *)
    (* let cmd = S.raw "NAMESPACE" in *)
    (* let aux () = *)
    (*   send_command ci cmd >>= fun () -> *)
    (*   IO.return ci.state.rsp_info.rsp_namespace *)
    (* in *)
    (* IO.with_lock ci.send_lock aux *)

  let check s =
    let ci = connection_info s in
    let cmd = S.raw "CHECK" in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let close s =
    let ci = connection_info s in
    let cmd = S.raw "CLOSE" in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let expunge s =
    let ci = connection_info s in
    let cmd = S.raw "EXPUNGE" in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let uid_expunge s set =
    assert (not (Uid_set.mem_zero set));
    let ci = connection_info s in
    let cmd = S.(raw "UID EXPUNGE" ++ space ++ message_set (uid_set_to_uint32_set set)) in
    let aux () = send_command ci cmd in
    IO.with_lock ci.send_lock aux

  let search_aux s cmd ?charset query =
    let ci = connection_info s in
    let charset_opt = match charset with
      | None -> S.null
      | Some charset -> S.(string charset ++ space)
    in
    let cmd = S.(raw cmd ++ space ++ charset_opt ++ search_key query) in
    let aux () =
      send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_search_results
    in
    IO.with_lock ci.send_lock aux

  let search s ?charset query =
    search_aux s "SEARCH" ?charset query >|= uint32_list_to_seq_list

  let uid_search s ?charset query =
    search_aux s "UID SEARCH" ?charset query >|= uint32_list_to_uid_list

  type msg_att_handler =
    Seq.t * msg_att list -> unit

  let fetch_aux cmd s set changedsince attrs =
    let ci = connection_info s in
    let changedsince =
      match changedsince with
        None ->
        S.null
      | Some modseq ->
        S.(space ++ raw "(CHANGEDSINCE " ++ raw (Modseq.to_string modseq) ++ raw ")")
    in
    let cmd =
      S.(raw cmd ++ space ++ message_set set ++ space ++ list fetch_att attrs ++ changedsince)
    in
    let aux () =
      send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_fetch_list
    in
    IO.with_lock ci.send_lock aux

  let fetch_changedsince s set modseq atts =
    fetch_aux "FETCH" s (seq_set_to_uint32_set set) (Some modseq) atts

  let fetch s set attrs =
    fetch_aux "FETCH" s (seq_set_to_uint32_set set) None attrs

  let uid_fetch_changedsince s set modseq atts =
    fetch_aux "UID FETCH" s (uid_set_to_uint32_set set) (Some modseq) atts

  let uid_fetch s set attrs =
    fetch_aux "UID FETCH" s (uid_set_to_uint32_set set) None attrs

  let store_aux cmd s set unchangedsince mode att =
    assert false
    (* let ci = connection_info s in *)
    (* let unchangedsince = match unchangedsince with *)
    (*   | None -> S.null *)
    (*   | Some modseq -> S.(raw "(UNCHANGEDSINCE " ++ raw (Modseq.to_string modseq) ++ raw ") ") *)
    (* in *)
    (* let mode = match mode with *)
    (*   | `Add -> S.raw "+" *)
    (*   | `Set -> S.null *)
    (*   | `Remove -> S.raw "-" *)
    (* in *)
    (* let cmd = *)
    (*   S.(raw cmd ++ space ++ message_set set ++ space ++ unchangedsince ++ mode ++ store_att att) *)
    (* in *)
    (* let aux () = send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_modified in *)
    (* IO.with_lock ci.send_lock aux *)

  let store s set mode flags =
    store_aux "STORE" s (seq_set_to_uint32_set set) None mode flags >>= fun _ ->
    IO.return ()

  let uid_store s set mode flags =
    store_aux "UID STORE" s (uid_set_to_uint32_set set) None mode flags >>= fun _ ->
    IO.return ()

  let store_unchangedsince s set unchangedsince mode flags =
    store_aux "STORE" s (seq_set_to_uint32_set set) (Some unchangedsince) mode flags >|=
    uint32_set_to_seq_set

  let uid_store_unchangedsince s set unchangedsince mode flags =
    store_aux "UID STORE" s (uid_set_to_uint32_set set) (Some unchangedsince) mode flags >|=
    uint32_set_to_uid_set

  let copy_aux cmd s set destbox =
    let ci = connection_info s in
    let cmd = S.(raw cmd ++ space ++ message_set set ++ space ++ mailbox destbox) in
    let aux () = send_command ci cmd >|= fun () -> ci.state.rsp_info.rsp_copyuid in
    IO.with_lock ci.send_lock aux

  let copy s set destbox =
    copy_aux "COPY" s (seq_set_to_uint32_set set) destbox >>= fun _ ->
    IO.return ()

  let uidplus_copy s (set : Seq_set.t) destbox =
    copy_aux "COPY" s (seq_set_to_uint32_set set) destbox

  let uid_copy s set destbox =
    copy_aux "UID COPY" s (uid_set_to_uint32_set set) destbox >>= fun _ ->
    IO.return ()

  let uidplus_uid_copy s set destbox =
    copy_aux "UID COPY" s (uid_set_to_uint32_set set) destbox

  let state s =
    let ci = connection_info s in
    ci.state

  let is_busy s =
    let ci = connection_info s in
    IO.is_locked ci.send_lock
end
