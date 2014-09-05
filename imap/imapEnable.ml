open ImapTypes
  
type extension_data += EXTENSION_ENABLE of capability list

open ImapParser

let enable_parser =
  function
    EXTENDED_PARSER_RESPONSE_DATA ->
      str "ENABLED" >>
      rep (char ' ' >> capability) >>= fun caps ->
      ret (EXTENSION_ENABLE caps)
  | _ ->
      fail


open ImapPrint
open Format

let enable_printer =
  function
    EXTENSION_ENABLE caps ->
      let p ppf = List.iter (fun x -> fprintf ppf "@ %a" capability_print x) in
      Some (fun ppf -> fprintf ppf "@[<2>(enabled%a)@]" p caps)
  | _ ->
      None

open ImapWriter

let send_capability =
  function
    CAPABILITY_AUTH_TYPE t ->
      raw "AUTH=" ++ raw t
  | CAPABILITY_NAME t ->
      raw t

let enable caps = {
  cmd_sender = ImapWriter.(raw "ENABLE" ++ char ' ' ++ separated (char ' ') send_capability caps);
  cmd_parser = ImapParser.response;
  cmd_handler =
    fun s ->
      let rec loop =
        function [] -> []
               | EXTENSION_ENABLE caps :: _ -> caps
               | _ :: rest -> loop rest
      in
      loop s.rsp_info.rsp_extension_list
}

let _ =
  ImapExtension.register_extension {ext_parser = enable_parser; ext_printer = enable_printer}
