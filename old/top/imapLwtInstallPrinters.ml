let printers =
  [ "ImapLwt.Uid.printer"; "ImapLwt.Seq.printer";
    "ImapLwt.Modseq.printer"; "ImapLwt.Gmsgid.printer"; "ImapLwt.Gthrid.printer";
    "ImapLwt.UidSet.printer"; "ImapLwt.SeqSet.printer" ]

let eval_string ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let rec install_printers = function
  | [] -> true
  | printer :: printers ->
      let cmd = Printf.sprintf "#install_printer %s;;" printer in
      eval_string cmd && install_printers printers
      
let () =
  if not (install_printers printers) then
    Format.eprintf "ocaml.lwt.top: there was a problem installing some of the toplevel printers@."
