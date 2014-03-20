# ocaml-imap

OCaml-IMAP is an OCaml client library for the
[IMAP4rev1](http://tools.ietf.org/html/rfc3501) protocol. It uses
[Lwt](http://ocsigen.org/lwt/) to support concurrency.  It aims to be correct,
easy to use and complete.

This is a preliminary release.  While the library has seen some limited testing,
more testing is required before it can be considered ready.

The library consists of a low-level interface (module `Imap` and its dependencies) and
a higher level interface (module `Imap_client`).  In this release only the low-level interface
is available, but it is already enough to do interesting things.

This library has been greatly influenced by the
[MailCore2](https://github.com/MailCore/mailcore2) library and Ruby's
[Net::IMAP](http://ruby-doc.org/stdlib-2.0/libdoc/net/imap/rdoc/Net/IMAP.html)
module.

## Requirements and installation

The library has several dependencies and the easiest way to install everything
is to use [OPAM](http://opam.ocaml.org).

1. Add my personal OPAM repository to OPAM:
   ```sh
   opam repo add nojb-personal https://github.com/nojb/opam-personal
   ```

2. Install the `imap` packages:
   ```sh
   opam install imap
   ```

3. (Optional) Install the `utop` toplevel:
   ```sh
   opam install utop
   ```

Once we are done with the package, it is easy to uninstall and remove the added repository:
```sh
opam remove imap
opam repo remove nojb-personal
```

## Usage

Since the library is written using Lwt, it is easier to explore using the
[utop](https://github.com/diml/utop) toplevel, which has auto-evaluation of Lwt
threads.  All the examples below are done using the `utop` toplevel.

### Connection, Authentication, and Disconnection

1. Load the library and install the relevant printers.
   ```ocaml
   # #require "imap";;
   # #install_printer Imap_uint.Uint32.printer;;
   # #install_printer Imap_uint.Uint64.printer;;
   ```

2. Set up a SSL context.
   ```ocaml
   # Ssl.init ();;
   - : unit = ()
   # let ssl_context = Ssl.create_context Ssl.TLSv1 Ssl.Client_context;;
   val ssl_context : Ssl.context = <abstr>
   ```

3. Create an IMAP session object and connect to the server.
   ```ocaml
   # let imap = Imap.make "imap.gmail.com";;
   val imap : Imap.session = <abstr>
   # Imap.connect ~ssl_context imap;;
   - : [ `Needsauth | `Preauth ] = `Needsauth
   ```
   This means that the SSL connection has been established successfully and the
   server requires authentication.

4. Authenticate with login and password.
   ```ocaml
   # Imap.login imap "<login>" "<password>";;
   - : unit = ()
   ```
   At this point we have successfully authenticated and we are ready to interact
   with the server.

5. Logout
   ```ocaml
   # Imap.logout imap;;
   - : unit = ()
   ```

### Debug Output

One can turn on debug output by setting the variable `Imap.debug` to true:
```ocaml
# Imap.debug := true;;
- : unit = ()
```
This makes it very easy to see what is going on behind the scenes:
```ocaml
# Imap.connect imap;;
S: * OK Gimap ready for requests from 131.111.16.20 u46mb29270479wec
- : [ `Needsauth | `Preauth ] = `Needsauth
# Imap.login imap "<login>" "<password>";;
C: 1 LOGIN <login> <password>
S: * CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE QUOTA ID XLIST CHILDREN X-GM-EXT-1 UIDPLUS COMPRESS=DEFLATE ENABLE MOVE CONDSTORE ESEARCH
S: 1 OK <login> <user name> authenticated (Success)
- : unit = ()
# Imap.logout imap;;
C: 2 LOGOUT
S: * BYE LOGOUT Requested
S: 2 OK 73 good day (Success)
- : unit = ()
```

### Some example manipulations

1. We login as before.
   ```ocaml
   # Imap.connect ~ssl_context imap;;
   - : unit = ()
   # Imap.login imap "<login>" "<password>";;
   - : unit = ()
   ```
   It is going to be convenient to open the module `Imap_types`.
   ```ocaml
   opan Imap_types;;
   ```

2. Select the INBOX mailbox in order to look at its messages.
   ```ocaml
   # Imap.select imap "inbox";;
   - : unit = ()
   ```

3. Get a list of the UID (unique identification number) and flags of all the messages in INBOX.
   ```ocaml
   # Imap.fetch;;
   - : Imap.session -> Imap_set.t -> ?changedsince:Imap_uint.uint64 -> fetch_att list -> msg_att list Lwt.t = <fun>
   # Imap.fetch imap Imap_set.all [`UID; `FLAGS];;
   - : msg_att list =
   [(1Ul, [`UID 1Ul; `FLAGS [`Seen]]); (2Ul, [`UID 2Ul; `FLAGS []]); (3Ul, [`UID 3Ul; `FLAGS []]); 
    (4Ul, [`UID 4Ul; `FLAGS []]); (5Ul, [`UID 5Ul; `FLAGS []]); (6Ul, [`UID 6Ul; `FLAGS [`Seen]]); 
    (7Ul, [`UID 7Ul; `FLAGS []]); (8Ul, [`UID 8Ul; `FLAGS []]); (9Ul, [`UID 9Ul; `FLAGS []]); 
    (10Ul, [`UID 10Ul; `FLAGS []]); (11Ul, [`UID 11Ul; `FLAGS []]); (12Ul, [`UID 12Ul; `FLAGS []])]
    ```
   Here the answer consists of an association list.  The keys are the sequence
   numbers of the messages and the values are the requested information.

4. Mark message with UID 12 as `Seen`.
   ```ocaml
   # let single12 = Imap_set.singleton (Imap_uint.Uint32.of_int 12);;
   val single12 : Imap_set.t = [(12Ul, 12Ul)]
   # Imap.uid_store imap single12 `Add (`FLAGS [`Seen]);;
   - : unit = ()
   # Imap.uid_fetch imap single12 [`FLAGS];;
   - : msg_att list = [(12Ul, [`UID 12Ul; `FLAGS [`Seen]])]
   ```

5. Close the mailbox.
   ```ocaml
   # Imap.close imap;;
   - : unit = ()
   # Imap.last_response imap;;
   - : string = "Returned to authenticated state. (Success)"
   ```

The full IMAP4rev1 protocol is supported in addition of some common extensions
(see below).

## Documentation

Documentation of the main interface file `imap.mli` is complete and am working
in adding documentation to the rest of the files.

## Supported IMAP extensions

- COMPRESS=DEFLATE [(RFC 4978)](https://tools.ietf.org/html/rfc4978)
- IDLE [(RFC 2177)](https://tools.ietf.org/html/rfc2177)
- CONDSTORE [(RFC 4551)](https://tools.ietf.org/html/rfc4551)
- ID [(RFC 2971)](https://tools.ietf.org/html/rfc2971)
- UIDPLUS [(RFC 4315)](https://tools.ietf.org/html/rfc4315)
- X-GM-EXT-1 [(Google)](https://developers.google.com/gmail/imap_extensions)
- NAMESPACE [(RFC 2342)](http://www.ietf.org/rfc/rfc2342.txt)
- ENABLE [(RFC 5161)](https://tools.ietf.org/html/rfc5161)
- AUTH=XOAUTH2 [(Google)](https://developers.google.com/gmail/xoauth2_protocol)

## Comments

Comments, bug reports and feature requests are very welcome: n.oje.bar@gmail.com.
