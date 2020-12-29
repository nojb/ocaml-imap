(lang dune 2.7)

(formatting (enabled_for ocaml dune))

(name imap)

(license MIT)

(authors
 "Nicolás Ojeda Bär <n.oje.bar@gmail.com>")

(source
 (github nojb/ocaml-imap))

(package
 (name imap)
 (synopsis "Lwt-compatible IMAP4rev1 client library")
 (depends
  dune
  (base64 (>= 2.0.0))
  uutf))

(package
 (name imap-unix)
 (depends ssl))

(package
 (name imap-lwt)
 (depends
  lwt
  lwt_ssl))

(generate_opam_files true)
