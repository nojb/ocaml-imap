(lang dune 2.7)

(formatting (enabled_for ocaml dune))

(name imap)

(license MIT)

(authors
 "Nicolas Ojeda Bar")

(source
 (github nojb/ocaml-imap))

(package
 (name imap)
 (synopsis "Lwt-compatible IMAP4rev1 client library")
 (depends
  (dune (>= 2.7))
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
