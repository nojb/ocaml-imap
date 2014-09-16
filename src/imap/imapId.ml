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
open ImapExtension
  
type extension_data +=
     ID_PARAMS of (string * string option) list

let id_printer =
  let open Format in
  function
    ID_PARAMS params ->
      let p ppf =
        List.iter (function (k, None) -> fprintf ppf "@ (%s nil)" k
                          | (k, Some v) -> fprintf ppf "@ (%s %S)" k v)
      in
      Some (fun ppf -> fprintf ppf "@[<2>(id%a)@]" p params)
  | _ ->
      None

open ImapControl

let id_sender params =
  let open ImapSend in
  let param_sender =
    function
      (k, None) ->
        string k >> char ' ' >> nil
    | (k, Some v) ->
        string k >> char ' ' >> string v
  in
  raw "ID" >> char ' ' >>
  match params with
    [] -> nil
  | xs -> list param_sender params

let id_parser =
  let open ImapParser in
  let id_params =
    astring >>= fun k ->
    char ' ' >>
    nstring >>= fun v ->
    ret (k, v)
  in
  function
    EXTENDED_PARSER_RESPONSE_DATA ->
      str "ID" >> char ' ' >>
      char '(' >> sep (char ' ') id_params >>= fun params_list -> char ')' >>
      ret (ID_PARAMS params_list)
  | _ ->
      fail

let id_handler s =
  let rec loop =
    function
      [] -> []
    | ID_PARAMS params :: _ ->
        params
    | _ :: rest ->
        loop rest
  in
  loop s.rsp_info.rsp_extension_list

let id params =
  ImapCore.std_command (id_sender params) id_handler

let id_basic name version =
  let get l n =
    try
      snd (List.find (fun (k, _) -> String.lowercase k = String.lowercase n) l)
    with
      Not_found -> None
  in
  let cmd = id ["name", Some name; "version", Some version] in
  cmd >>= fun params ->
  ret (get params "name", get params "version")
  
let _ =
  register_extension {ext_printer = id_printer; ext_parser = id_parser}
