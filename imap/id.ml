open ImapTypes
  
type extension_data += ID_PARAMS of (string * string option) list

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

open Control

let id_sender params =
  let open Sender in
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
  let open Parser in
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
  Commands.std_command (id_sender params) id_handler

let id_basic name version =
  let get l n =
    try
      snd (List.find (fun (k, _) -> String.lowercase k = String.lowercase n) l)
    with
      Not_found -> None
  in
  let cmd = id ["name", Some name; "version", Some version] in
  fun tag ->
    cmd tag >>= fun params ->
    ret (get params "name", get params "version")
  
let _ =
  Extension.register_extension {ext_printer = id_printer; ext_parser = id_parser}
