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

let extension_list = ref []

let extension_data_store st e =
  {st with rsp_info = {st.rsp_info with rsp_extension_list = st.rsp_info.rsp_extension_list @ [e]}}

let register_extension ext =
  extension_list := !extension_list @ [ext]

let unregister_all () =
  extension_list := []

(* (\* *)
(* msg-att-dynamic = "FLAGS" SP "(" [flag-fetch *(SP flag-fetch)] ")" *)
(*                     ; MAY change for a message *)

(* fetch-mod-resp      = "MODSEQ" SP "(" permsg-modsequence ")" *)

(* msg-att-dynamic     =/ fetch-mod-resp *)

(* *\) *)
(* let msg_att_dynamic = *)
(*   str "FLAGS" >> char ' ' >> char '(' >> *)
(*   sep (char ' ') flag_fetch >>= fun flags -> *)
(*   char ')' >> *)
(*   ret flags *)
(*   (\* let flags = char ' ' >> flag_list >>= fun flags -> ret (`FLAGS flags) in *\) *)
(*   (\* let modseq = char ' ' >> char '(' >> permsg_modsequence >>= fun n -> char ')' >> ret (`MODSEQ n) in *\) *)
(*   (\* let x_gm_labels = *\) *)
(*   (\*   char ' ' >> char '(' >> *\) *)
(*   (\*   sep (char ' ') astring >>= fun labs -> *\) *)
(*   (\*   char ')' >> *\) *)
(*   (\*   ret (`X_GM_LABELS (List.map ImapUtils.decode_mutf7 labs)) *\) *)
(*   (\* in *\) *)
(*   (\* altn [ *\) *)
(*   (\*   str "FLAGS" >> flags; *\) *)
(*   (\*   str "MODSEQ" >> modseq; *\) *)
(*   (\*   str "X-GM-LABELS" >> x_gm_labels *\) *)
(*   (\* ] *\) *)

(* (\* *)
(* id_params_list ::= "(" #(string SPACE nstring) ")" / nil *)
(*          ;; list of field value pairs *)
(* *\) *)
(* let id_params_list = *)
(*   let param = *)
(*     imap_string >>= fun k -> *)
(*     char ' ' >> *)
(*     nstring >>= fun v -> *)
(*     ret (k, v) *)
(*   in *)
(*   alt *)
(*     begin *)
(*       char '(' >> *)
(*       sep1 (char ' ') param >>= fun xs -> *)
(*       char ')' >> *)
(*       ret (ImapUtils.option_map (function (k, Some v) -> Some (k, v) | (_, None) -> None) xs) *)
(*     end *)
(*     (nil >> ret []) *)
  
(* (\* *)
(* id_response ::= "ID" SPACE id_params_list *)
(* *\) *)
(* (\* let id_response = *\) *)
(* (\*   str "ID" >> char ' ' >> id_params_list >>= fun params -> ret (`ID params) *\) *)

(* (\* *)
(* Namespace_Response_Extension = SP string SP "(" string *(SP string) ")" *)
(* *\) *)
(* (\* let namespace_response_extension = *\) *)
(* (\*   char ' ' >> *\) *)
(* (\*   imap_string >>= fun n -> *\) *)
(* (\*   char ' ' >> *\) *)
(* (\*   char '(' >> *\) *)
(* (\*   sep1 (char ' ') imap_string >>= fun xs -> *\) *)
(* (\*   char ')' >> *\) *)
(* (\*   ret (n, xs) *\) *)
  
(* (\* *)
(* Namespace = nil / "(" 1*( "(" string SP  (<"> QUOTED_CHAR <"> / *)
(*       nil) *(Namespace_Response_Extension) ")" ) ")" *)
(* *\) *)
(* (\* let namespace = *\) *)
(* (\*   alt *\) *)
(* (\*     (nil >> ret []) *\) *)
(* (\*     begin *\) *)
(* (\*       char '(' >> *\) *)
(* (\*       rep1 *\) *)
(* (\*         begin *\) *)
(* (\*           char '(' >> imap_string >>= fun ns_prefix -> *\) *)
(* (\*           char ' ' >> *\) *)
(* (\*           alt quoted_char (nil >> ret '\000') >>= fun ns_delimiter -> *\) *)
(* (\*           rep namespace_response_extension >>= fun ns_extensions -> *\) *)
(* (\*           char ')' >> *\) *)
(* (\*           ret {ns_prefix; ns_delimiter; ns_extensions} *\) *)
(* (\*         end >>= fun x -> *\) *)
(* (\*       char ')' >> *\) *)
(* (\*       ret x *\) *)
(* (\*     end *\) *)

(* (\* *)
(* Namespace_Response = "*" SP "NAMESPACE" SP Namespace SP Namespace SP *)
(*       Namespace *)

(*       ; The first Namespace is the Personal Namespace(s) *)
(*       ; The second Namespace is the Other Users' Namespace(s) *)
(*       ; The third Namespace is the Shared Namespace(s) *)
(* *\) *)
(* (\* let namespace_response = *\) *)
(* (\*   str "NAMESPACE" >> *\) *)
(* (\*   char ' ' >> namespace >>= fun personal -> *\) *)
(* (\*   char ' ' >> namespace >>= fun others -> *\) *)
(* (\*   char ' ' >> namespace >>= fun shared -> *\) *)
(* (\*   ret (`NAMESPACE (personal, others, shared)) *\) *)

(* (\** {2 NAMESPACE command} *\) *)

(* (\* type namespace = { *\) *)
(* (\*   ns_prefix : string; *\) *)
(* (\*   ns_delimiter : char; *\) *)
(* (\*   ns_extensions : (string * string list) list *\) *)
(* (\* } with sexp *\) *)

(* (\* type id_response = *\) *)
(* (\*   [ `ID of (string * string) list ] with sexp *\) *)

(* (\* type namespace_response = *\) *)
(* (\*   [ `NAMESPACE of namespace list * namespace list * namespace list ] with sexp *\) *)

(* (\* type enable_response = *\) *)
(* (\*   [ `ENABLED of capability list ] with sexp *\) *)
