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
     XGMMSGID_MSGID of Uint64.t

let fetch_att_xgmmsgid = FETCH_ATT_EXTENSION "X-GM-MSGID"

let xgmmsgid_printer : type a. a extension_kind -> a -> _ option =
  let open Format in
  function
    FETCH_DATA ->
      begin
        function
          XGMMSGID_MSGID id ->
            Some (fun ppf -> fprintf ppf "(x-gm-msgid %s)" (Uint64.to_string id))
        | _ ->
            None
      end
  | _ ->
      function _ -> None

let xgmmsgid_parser : type a. a extension_kind -> a parser = fun kind ->
  let open ImapParser in
  match kind with
    FETCH_DATA ->
      str "X-GM-MSGID" >> char ' ' >> uint64 >>= fun n ->
      ret (XGMMSGID_MSGID n)
  | _ ->
      fail

let _ =
  ImapPrint.(register_printer {print = xgmmsgid_printer});
  ImapParser.(register_parser {parse = xgmmsgid_parser})
