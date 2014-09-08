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
  
(* resp-code-apnd  = "APPENDUID" SP nz-number SP append-uid *)

(* resp-code-copy  = "COPYUID" SP nz-number SP uid-set SP uid-set *)

(* resp-text-code  =/ resp-code-apnd / resp-code-copy / "UIDNOTSTICKY" *)
(*                   ; incorporated before the expansion rule of *)
(*                   ;  atom [SP 1*<any TEXT-CHAR except "]">] *)
(*                   ; that appears in [IMAP] *)

(*
uid-set         = (uniqueid / uid-range) *("," uid-set)
*)
(*
uid-range       = (uniqueid ":" uniqueid)
                  ; two uniqueid values and all values
                  ; between these two regards of order.
                  ; Example: 2:4 and 4:2 are equivalent.
*)

type extension_data +=
     UIDPLUS_RESP_CODE_APND of Uint32.t * Uint32.t
   | UIDPLUS_RESP_CODE_COPY of Uint32.t * ImapSet.t * ImapSet.t
   | UIDPLUS_RESP_CODE_UIDNOTSTICKY

let uidplus_printer =
  let open Format in
  function
    UIDPLUS_RESP_CODE_APND (uid1, uid2) ->
      Some (fun ppf -> fprintf ppf "@[<2>(uidplus-append %s %s)@]"
               (Uint32.to_string uid1) (Uint32.to_string uid2))
  | UIDPLUS_RESP_CODE_COPY (uid, uidset1, uidset2) ->
      Some (fun ppf -> fprintf ppf "@[<2>(uidplus-copy %s ?)@]" (Uint32.to_string uid))
  | UIDPLUS_RESP_CODE_UIDNOTSTICKY ->
      Some (fun ppf -> fprintf ppf "(uid-not-sticky)")
  | _ ->
      None

let uidplus_parser =
  let open Parser in
  let uid_range =
    uniqueid >>= fun x ->
    char ':' >>
    uniqueid >>= fun y ->
    ret (x, y)
  in
  let uid_set =
    let elem =
      alt
        (uniqueid >>= fun id -> ret (ImapSet.single id))
        (uid_range >>= fun (x, y) -> ret (ImapSet.interval x y))
    in
    elem >>= fun x ->
    rep (char ',' >> elem) >>= fun xs ->
    ret (List.fold_left ImapSet.union x xs)
  in
  let resp_code_apnd =
    str "APPENDUID" >>
    char ' ' >>
    nz_number >>= fun uid ->
    char ' ' >>
    nz_number >>= fun uid2 ->
    ret (UIDPLUS_RESP_CODE_APND (uid, uid2))
  in
  let resp_code_copy =
    str "COPYUID" >>
    char ' ' >>
    nz_number >>= fun uidvalidity ->
    char ' ' >>
    uid_set >>= fun src_uids ->
    char ' ' >>
    uid_set >>= fun dst_uids ->
    ret (UIDPLUS_RESP_CODE_COPY (uidvalidity, src_uids, dst_uids))
  in
  let resp_code_uidnotsticky =
    str "UIDNOTSTICKY" >> ret UIDPLUS_RESP_CODE_UIDNOTSTICKY
  in
  function
    EXTENDED_PARSER_RESP_TEXT_CODE ->
      altn [ resp_code_apnd; resp_code_copy; resp_code_uidnotsticky ]
  | _ ->
      fail

open Control

let uid_expunge set =
  let sender =
    let open Sender in
    raw "UID EXPUNGE" >> char ' ' >> message_set set
  in
  Core.std_command sender (fun _ -> ())

let copy_aux cmd set destbox =
  let sender =
    let open Sender in
    raw cmd >> char ' ' >> message_set set >> char ' ' >> mailbox destbox
  in
  let handler s =
    let rec loop =
      function
        [] ->
          Uint32.zero, ImapSet.empty, ImapSet.empty
      | UIDPLUS_RESP_CODE_COPY (uid, src, dst) :: _ ->
          uid, src, dst
      | _ :: rest ->
          loop rest
    in
    loop s.rsp_info.rsp_extension_list
  in
  Core.std_command sender handler

let copy set destbox tag =
  copy_aux "COPY" set destbox tag >> ret ()

let uid_copy set destbox tag =
  copy_aux "UID COPY" set destbox tag >> ret ()

let uidplus_copy =
  copy_aux "COPY"

let uidplus_uid_copy =
  copy_aux "UID COPY"

let _ =
  register_extension {ext_parser = uidplus_parser; ext_printer = uidplus_printer}
