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

open ImapTypes

type msg_att_extension +=
     XGMLABELS_XGMLABELS of string list

let fetch_att_xgmlabels =
  FETCH_ATT_EXTENSION "X-GM-LABELS"

let xgmlabels_printer : type a. a extension_kind -> a -> _ option =
  let open Format in
  function
    FETCH_DATA ->
      begin
        function
          XGMLABELS_XGMLABELS labels ->
            let p ppf = List.iter (fun x -> fprintf ppf "@ %S" x) in
            Some (fun ppf -> fprintf ppf "@[<2>(x-gm-labels%a)@]" p labels)
        | _ ->
            None
      end
  | _ ->
      function _ -> None

let xgmlabels_parser : type a. a extension_kind -> a ImapParser.t = fun kind ->
  let open ImapParser in
  match kind with
    FETCH_DATA ->
      str "X-GM-LABELS" >> char ' ' >>
      char '(' >> sep (char ' ') astring >>= fun labels -> char ')' >>
      ret (XGMLABELS_XGMLABELS labels)
  | _ ->
      fail

open ImapControl

let store_xgmlabels_aux cmd set fl_sign fl_silent labels =
  let sender =
    let open ImapSend in
    send cmd >> char ' ' >> message_set set >> char ' ' >>
    begin
      match fl_sign with
        STORE_ATT_FLAGS_SET -> ret ()
      | STORE_ATT_FLAGS_ADD -> char '+'
      | STORE_ATT_FLAGS_REMOVE -> char '-'
    end
    >> send "X-GM-LABELS" >> (if fl_silent then send ".SILENT" else ret ()) >>
    char ' ' >>
    list string labels
  in
  ImapCore.std_command sender

let store_xgmlabels =
  store_xgmlabels_aux "STORE"

let uid_store_xgmlabels =
  store_xgmlabels_aux "UID STORE"

let _ =
  ImapPrint.(register_printer {print = xgmlabels_printer});
  ImapParser.(register_parser {parse = xgmlabels_parser})
