#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let cond_lwt = Env.bool "lwt"
let cond_ssl = Env.bool "ssl"
let cond_lwt_ssl = cond_lwt && cond_ssl

let imap_units =
  [ "Commands"; "Control"; "Auth"; "Parser"; "Print"; "Set"; "Utils"; "Send"; "Core" ]

let () =
  let my_ext = List.filter (fun s -> s <> ".mli" && s <> ".cmx" && s <> ".cmi") Exts.module_library in
  Pkg.describe "imap" ~builder:`OCamlbuild
    begin
      Pkg.lib "pkg/META" ::
      Pkg.lib ~exts:my_ext "imap/imap" ::
      Pkg.lib ~cond:cond_lwt_ssl ~exts:Exts.module_library "lwt/imapLwt" ::
      Pkg.doc "README.md" ::
      Pkg.doc "CHANGES" ::
      Pkg.lib ~exts:Exts.interface "imap/imapTypes" ::
      (List.map (fun u -> Pkg.lib ~exts:Exts.interface_opt (Printf.sprintf "imap/imap%s" u)) imap_units)
    end
