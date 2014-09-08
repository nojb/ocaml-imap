(* The MIT License (MIT) *)

(* Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com> *)

(* Permission is hereby granted, free of charge, to any person obtaining a copy *)
(* of this software and associated documentation files (the "Software"), to deal *)
(* in the Software without restriction, including without limitation the rights *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell *)
(* copies of the Software, and to permit persons to whom the Software is *)
(* furnished to do so, subject to the following conditions: *)

(* The above copyright notice and this permission notice shall be included in *)
(* all copies or substantial portions of the Software. *)

(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE *)
(* SOFTWARE. *)

open ImapTypes
  
type extension_data += EXTENSION_ENABLE of capability list

open Parser

(*
response-data =/ "*" SP enable-data CRLF
enable-data   = "ENABLED" *(SP capability)
*)
let enable_parser =
  function
    EXTENDED_PARSER_RESPONSE_DATA ->
      str "ENABLED" >>
      rep (char ' ' >> capability) >>= fun caps ->
      ret (EXTENSION_ENABLE caps)
  | _ ->
      fail


open Print
open Format

let enable_printer =
  function
    EXTENSION_ENABLE caps ->
      let p ppf = List.iter (fun x -> fprintf ppf "@ %a" capability_print x) in
      Some (fun ppf -> fprintf ppf "@[<2>(enabled%a)@]" p caps)
  | _ ->
      None

open Sender
open Control

let send_capability =
  function
    CAPABILITY_AUTH_TYPE t ->
      raw "AUTH=" >> raw t
  | CAPABILITY_NAME t ->
      raw t

let enable caps =
  Commands.std_command
    (raw "ENABLE" >> char ' ' >> separated (char ' ') send_capability caps)
    (fun s ->
       let rec loop =
         function [] -> []
                | EXTENSION_ENABLE caps :: _ -> caps
                | _ :: rest -> loop rest
       in
       loop s.rsp_info.rsp_extension_list)

let _ =
  Extension.register_extension {ext_parser = enable_parser; ext_printer = enable_printer}
