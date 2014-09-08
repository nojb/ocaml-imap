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

open Types
open Extension
  
(* resp-text-code   =/ "HIGHESTMODSEQ" SP mod-sequence-value / *)
(*                     "NOMODSEQ" / *)
(*                     "MODIFIED" SP set *)

type condstore_resptextcode =
   | CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ of Uint64.t
   | CONDSTORE_RESPTEXTCODE_NOMODSEQ
   | CONDSTORE_RESPTEXTCODE_MODIFIED of ImapSet.t
       
type extension_data +=
     CONDSTORE_FETCH_DATA_MODSEQ of Uint64.t
   | CONDSTORE_RESP_TEXT_CODE of condstore_resptextcode
   | CONDSTORE_SEARCH_DATA of Uint32.t list * Uint64.t
   | CONDSTORE_STATUS_INFO_HIGHESTMODSEQ of Uint64.t

let fetch_att_modseq = FETCH_ATT_EXTENSION "MODSEQ"

let condstore_printer =
  let open Format in
  let condstore_resptextcode_print ppf =
    function
      CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ n ->
        fprintf ppf "(highest-mod-seq %s)" (Uint64.to_string n)
    | CONDSTORE_RESPTEXTCODE_NOMODSEQ ->
        fprintf ppf "(no-mod-seq)"
    | CONDSTORE_RESPTEXTCODE_MODIFIED ns ->
        fprintf ppf "(modified@ ?)"
  in
  function
    CONDSTORE_FETCH_DATA_MODSEQ n ->
      Some (fun ppf -> fprintf ppf "(mod-seq %s)" (Uint64.to_string n))
  | CONDSTORE_RESP_TEXT_CODE r ->
      Some (fun ppf -> condstore_resptextcode_print ppf r)
  | CONDSTORE_SEARCH_DATA (ns, n) ->
      let loop ppf =
        function
          [] -> ()
        | [x] -> fprintf ppf "%s" (Uint32.to_string x)
        | x :: xs ->
            fprintf ppf "%s" (Uint32.to_string x);
            List.iter (fun x -> fprintf ppf "@ %s" (Uint32.to_string x)) xs
      in
      Some (fun ppf -> fprintf ppf "@[<2>(%a@ (mod-seq %s))@]" loop ns (Uint64.to_string n))
  | CONDSTORE_STATUS_INFO_HIGHESTMODSEQ n ->
      Some (fun ppf -> fprintf ppf "(highest-mod-seq %s)" (Uint64.to_string n))
  | _ ->
      None

(* [RFC 4551]
mod-sequence-value  = 1*DIGIT
                          ;; Positive unsigned 64-bit integer
                          ;; (mod-sequence)
                          ;; (1 <= n < 18,446,744,073,709,551,615)
*)

(*
search-sort-mod-seq = "(" "MODSEQ" SP mod-sequence-value ")"
*)

(*
permsg-modsequence  = mod-sequence-value
                          ;; per message mod-sequence
*)
let condstore_parse =
  let open Parser in
  let mod_sequence_value =
    accum (function '0' .. '9' -> true | _ -> false) >>= fun s ->
    try ret (Uint64.of_string s) with _ -> fail
  in
  let search_sort_mod_seq =
    char '(' >> str "MODSEQ" >> char ' ' >> mod_sequence_value >>= fun x -> char ')' >> ret x
  in
  let permsg_modsequence = mod_sequence_value in
  let condstore_resptextcode =
    let highestmodseq =
      str "HIGHESTMODSEQ" >> char ' ' >> mod_sequence_value >>= fun modseq ->
      ret (CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ modseq)
    in
    let nomodseq = str "NOMODSEQ" >> ret CONDSTORE_RESPTEXTCODE_NOMODSEQ in
    let modified =
      str "MODIFIED" >> char ' ' >> sequence_set >>= fun set ->
      ret (CONDSTORE_RESPTEXTCODE_MODIFIED set)
    in
    altn [ highestmodseq; nomodseq; modified ]
  in
  function
    EXTENDED_PARSER_RESP_TEXT_CODE ->
      condstore_resptextcode >>= fun r ->
      ret (CONDSTORE_RESP_TEXT_CODE r)
  | EXTENDED_PARSER_FETCH_DATA ->
(* fetch-mod-resp      = "MODSEQ" SP "(" permsg-modsequence ")" *)
      str "MODSEQ" >> char ' ' >> char '(' >> permsg_modsequence >>= fun m -> char ')' >>
      ret (CONDSTORE_FETCH_DATA_MODSEQ m)
(* status-att          =/ "HIGHESTMODSEQ" *)
(*                           ;; extends non-terminal defined in RFC 3501. *)
  | EXTENDED_PARSER_STATUS_ATT ->
(* msg-att-dynamic     =/ fetch-mod-resp *)
      str "HIGHESTMODSEQ" >> char ' ' >> mod_sequence_value >>= fun n ->
      ret (CONDSTORE_STATUS_INFO_HIGHESTMODSEQ n)
  | EXTENDED_PARSER_MAILBOX_DATA ->
(* mailbox-data        =/ "SEARCH" [1*(SP nz-number) SP *)
(*                           search-sort-mod-seq] *)
      str "SEARCH" >> char ' ' >>
      opt (rep1 (char ' ' >> nz_number) >>= fun ns ->
           char ' ' >> search_sort_mod_seq >>= fun m -> ret (ns, m)) ([], Uint64.zero) >>=
      fun (ns, m) ->
      ret (CONDSTORE_SEARCH_DATA (ns, m))
  | _ ->
      fail

open Control

let search_modseq_aux cmd ?charset key =
  let sender =
    let open Sender in
    raw cmd >>
    begin
      match charset with
        Some charset when search_key_need_to_send_charset key ->
          char ' ' >> string charset
      | _ ->
          ret ()
    end >>
    char ' ' >>
    search_key key
  in
  let cmd_handler s =
    let rec loop =
      function
        [] ->
          let res = s.rsp_info.rsp_search_results in
          (* modify (fun s -> {s with rsp_info = {s.rsp_info with rsp_search_results = []}}) >> *)
          (* ret (res, Uint64.zero) *)
          (* FIXME *)
          (res, Uint64.zero)
      | CONDSTORE_SEARCH_DATA (res, m) :: _ ->
          (* ret (res, m) *)
          (res, m)
      | _ :: rest ->
          loop rest
    in
    loop s.rsp_info.rsp_extension_list
  in
  Core.std_command sender cmd_handler

let search_modseq ?charset key =
  search_modseq_aux "SEARCH" ?charset key

let uid_search_modseq ?charset key =
  search_modseq_aux "UID SEARCH" ?charset key

let search ?charset key tag =
  search_modseq_aux "SEARCH" ?charset key tag >>= fun (res, _) -> ret res

let uid_search ?charset key tag =
  search_modseq_aux "UID SEARCH" ?charset key tag >>= fun (res, _) -> ret res

let select_condstore_optional mb cmd use_condstore =
  let cmd_sender =
    let open Sender in
    raw cmd >> char ' ' >> mailbox mb >> if use_condstore then raw " (CONDSTORE)" else ret ()
  in
  let cmd_handler s =
    let rec loop =
      function
        [] ->
          Uint64.zero
      | CONDSTORE_RESP_TEXT_CODE (CONDSTORE_RESPTEXTCODE_HIGHESTMODSEQ m) :: _ ->
          m
      | CONDSTORE_RESP_TEXT_CODE CONDSTORE_RESPTEXTCODE_NOMODSEQ :: _ ->
          Uint64.zero
      | _ :: rest ->
          loop rest
    in
    loop s.rsp_info.rsp_extension_list
  in
  fun tag ->
    modify (fun s -> {s with sel_info = Core.fresh_selection_info}) >>
    Core.std_command cmd_sender cmd_handler tag

let select_condstore mb =
  select_condstore_optional mb "SELECT" true

let select mb =
  fun tag ->
    select_condstore_optional mb "SELECT" false tag >>
    ret ()

let examine_condstore mb =
  select_condstore_optional mb "EXAMINE" true

let examine mb =
  fun tag ->
    select_condstore_optional mb "EXAMINE" false tag >>
    ret ()

let fetch_aux cmd set changedsince attrs =
  let cmd =
    let open Sender in
    let changedsince =
      match changedsince with
        None ->
          ret ()
      | Some modseq ->
          char ' ' >> raw "(CHANGEDSINCE " >> raw (Uint64.to_string modseq) >> char ')'
    in
    raw cmd >> char ' ' >> message_set set >> char ' ' >> list fetch_att attrs >> changedsince
  in
  let cmd_handler s = s.rsp_info.rsp_fetch_list in
  Core.std_command cmd cmd_handler

let fetch_changedsince_optional =
  fetch_aux "FETCH"

let uid_fetch_changedsince_optional =
  fetch_aux "UID FETCH"

let fetch_changedsince set modseq attrs =
  fetch_changedsince_optional set (Some modseq) attrs

let uid_fetch_changedsince set modseq attrs =
  uid_fetch_changedsince_optional set (Some modseq) attrs

let store_aux cmd set unchangedsince flags =
  let sender =
    let open Sender in
    let unchangedsince =
      match unchangedsince with
        None -> ret ()
      | Some modseq -> raw "(UNCHANGEDSINCE " >> raw (Uint64.to_string modseq) >> raw ") "
    in
    raw cmd >> char ' ' >> message_set set >> char ' ' >> unchangedsince >> store_att_flags flags
  in
  let handler s =
    let rec loop =
      function
        [] -> ImapSet.empty
      | CONDSTORE_RESP_TEXT_CODE (CONDSTORE_RESPTEXTCODE_MODIFIED uids) :: _ ->
          uids
      | _ :: rest ->
          loop rest
    in
    loop s.rsp_info.rsp_extension_list
  in
  Core.std_command sender handler

let store set flags tag =
  store_aux "STORE" set None flags tag >> ret ()

let uid_store set flags tag =
  store_aux "UID STORE" set None flags tag >> ret ()

let store_unchangedsince set unchangedsince flags =
  store_aux "STORE" set (Some unchangedsince) flags

let uid_store_unchangedsince set unchangedsince flags =
  store_aux "UID STORE" set (Some unchangedsince) flags
    
let _ =
  register_extension {ext_parser = condstore_parse; ext_printer = condstore_printer}
