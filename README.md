# ocaml-imap

OCaml-IMAP is an OCaml client library for the
[IMAP4rev1](http://tools.ietf.org/html/rfc3501) protocol.  It is written in a
monadic style and can be used with [Lwt](http://ocsigen.org/lwt/) or
[Async](https://github.com/janestreet/async) to support concurrency.  It aims to
be correct, easy to use and complete.

This is a preliminary release.

The full IMAP4rev1 protocol is supported in addition of some common extensions
(see below).

## Acknowledgements

This library has been greatly influenced by

- [MailCore2](https://github.com/MailCore/mailcore2)
- Ruby's [Net::IMAP](http://ruby-doc.org/stdlib-2.0/libdoc/net/imap/rdoc/Net/IMAP.html)
  module.

## Requirements and installation

The library has several dependencies and the easiest way to install everything
is to use [OPAM](http://opam.ocaml.org).

1. Install `ocaml-imap` from OPAM. For the `Lwt` backend, just do:
   ```sh
   opam install ssl lwt imap
   ```
   For the `Async` backend, do:
   ```sh
   opam install async_ssl imap
   ```
   Finally, simply doing
   ```sh
   opam install imap
   ```
   will install only the `unix` (synchronous) backend.

   Optionally, if you would like to use GNU SASL library to authenticate
   with the server, you should include in each case the dependency `gsasl`:
   `opam install gsasl imap`, or `opam install ssl lwt gsasl imap`, or
   `opam install async_ssl gsasl imap`. See `gsasl/imap_gsasl.mli` for the interface.
   
2. Read the documentation in `imap/client.mli`, `imap/response.mli`, and
   `imap/imap_types.mli`.

3. (Optional) Install the `utop` toplevel:
   ```sh
   opam install utop
   ```

## Usage

Since we will be using the Lwt backend for demonstration purposes, it is easier
to explore using the [utop](https://github.com/diml/utop) toplevel, which has
auto-evaluation of Lwt threads.  All the examples below are done using the
`utop` toplevel.

### Connection, Authentication, and Disconnection

1. Load the library and install the relevant printers.
   ```ocaml
   # #require "imap.lwt";;
   # open Imap;;
   # open Imap_types;;
   # #install_printer Uid.printer;;
   # #install_printer Seq.printer;;
   # #install_printer Gmsgid.printer;;
   # #install_printer Gthrid.printer;;
   # #install_printer Modseq.printer;;
   ```

2. Set up and open a SSL connection to the IMAP server
   ```ocaml
   # let imap = Imap_lwt.make ();;
   val imap : Imap_lwt.session = <abstr>
   # Imap_lwt.connect_ssl imap "imap.gmail.com";;
   - : [ `Needsauth | `Preauth ] = `Needsauth
   ```
   This means that the SSL connection has been established successfully and the
   server requires authentication.

3. Authenticate with login and password.
   ```ocaml
   # Imap_lwt.login imap "<login>" "<password>";;
   - : unit = ()
   ```
   At this point we have successfully authenticated and we are ready to interact
   with the server.

4. Logout
   ```ocaml
   # Imap_lwt.logout imap;;
   - : unit = ()
   ```

### Selecting and closing mailbox

1. Select the INBOX mailbox in order to look at its messages.
   ```ocaml
   # Imap_lwt.select imap "inbox";;
   - : unit = ()
   ```

2. Get the number of unread messages and the total number of messages in the
   mailbox:
   ```ocaml
   # Imap_lwt.status imap "inbox" [`UNSEEN; `MESSAGES];;
   - : mailbox_data_status = {st_mailbox = "inbox"; st_info_list = [`MESSAGES 11; `UNSEEN 8]}
   ```
   So there are 11 messages and 8 unread messages.

3. Close the mailbox.
   ```ocaml
   # Imap_lwt.close imap;;
   - : unit = ()
   # Imap_lwt.last_response imap;;
   - : string = "Returned to authenticated state. (Success)"
   ```
   
### Fetch message MIME information

1. Get the UIDs of unread messages in the mailbox.
   ```ocaml
   # Imap_lwt.uid_search imap `UNSEEN;;
   - : Uid.t list = [2; 3; 4; 5; 7; 8; 9; 10]
   ```
   
2. Get envelope information for message with UID 2.
   ```ocaml
   # let s = Imap_lwt.uid_fetch (Uid_set.single' 2) [`ENVELOPE];;
   val s : (Seq.t * msg_att) Lwt_stream.t = <abstr>
   # Lwt_stream.map snd s |> Lwt_stream.to_list;;
   - : msg_at list =
   [`UID 2;
    `ENVELOPE
     {env_date = "Tue, 4 Feb 2014 07:52:19 -0800";
      env_subject = "The best of Gmail, wherever you are";
      env_from = [{addr_name = "Gmail Team"; addr_adl = ""; addr_mailbox = "mail-noreply"; addr_host = "google.com"}];
      env_sender = [{addr_name = "Gmail Team"; addr_adl = ""; addr_mailbox = "mail-noreply"; addr_host = "google.com"}];
      env_reply_to = [{addr_name = "Gmail Team"; addr_adl = ""; addr_mailbox = "mail-noreply"; addr_host = "google.com"}];
      env_to = [{addr_name = "imaplib test"; addr_adl = ""; addr_mailbox = "imaplibtest"; addr_host = "gmail.com"}];
      env_cc = [];
      env_bcc = [];
      env_in_reply_to = "";
      env_message_id = "<CAFo37G3POBokcVK2ZDKMXi35iOnXH5XKyAztFFFeUVy2J1QrgA@mail.gmail.com>"}]
   ```

3. Get MIME body structure
   ```ocaml
   # let s = Imap_lwt.uid_fetch (Uid_set.single' 2) [`BODYSTRUCTURE];;
   val s : (Seq.t * msg_att) Lwt_stream.t = <abstr>
   # Lwt_stream.to_list s;;
   [(2, `UID 2);
    (2,
     `BODYSTRUCTURE
       (Imap.Mime.Multi_part
         {Imap.Mime.bd_subtype = "ALTERNATIVE";
          bd_parts =
           [Imap.Mime.Text
             {Imap.Mime.bd_param = [("CHARSET", "ISO-8859-1")]; bd_id = None; 
              bd_desc = None; bd_enc = `QUOTED_PRINTABLE; bd_octets = 656; 
              bd_other = {Imap.Mime.text_subtype = "PLAIN"; text_lines = 23}; 
              bd_ext = {Imap.Mime.ext_md5 = None; ext_dsp = None; ext_lang = []; ext_exts = []}};
            Imap.Mime.Text
             {Imap.Mime.bd_param = [("CHARSET", "ISO-8859-1")]; bd_id = None; 
              bd_desc = None; bd_enc = `QUOTED_PRINTABLE; bd_octets = 3374; 
              bd_other = {Imap.Mime.text_subtype = "HTML"; text_lines = 46}; 
              bd_ext = {Imap.Mime.ext_md5 = None; ext_dsp = None; ext_lang = []; ext_exts = []}}];
          bd_mexts = {Imap.Mime.mext_param = [("BOUNDARY", "089e0122f720083c1b04f196a0a7")]; mext_dsp = None; mext_lang = []; mext_exts = []}}))]
   ```
            
### Inspect and change flags and labels

1. Get flags for message with UID 11.
   ```ocaml
   # let s = Imap_lwt.uid_fetch imap (Uid_set.single' 11) [`FLAGS];;
   val s : (Seq.t * msg_att) Lwt_stream.t = <abstr>
   # Lwt_stream.map snd s |> Lwt_stream.to_list;;
   - : msg_att list = [`FLAGS []]
   ```

2. Get Gmail labels for message with UID 11.
   ```ocaml
   # let s = Imap_lwt.uid_fetch imap (Uid_set.single' 11) [`X_GM_LABELS];;
   val s : (Seq.t * msg_att) Lwt_stream.t = <abstr>
   # Lwt_stream.map snd s |> Lwt_stream.to_list;;
   - : msg_att list = [`X_GM_LABELS ["\\Important"]]
   ```

1. Mark message with UID 11 as `Seen`.
   ```ocaml
   # Imap_lwt.uid_store imap (Uid_set.single' 11) `Add (`FLAGS [`Seen]);;
   - : unit = ()
   # let s = Imap_lwt.uid_fetch imap (Uid_set.single' 11) [`FLAGS];;
   val s : (Seq.t * msg_att) Lwt_stream.t = <abstr>
   # Lwt_stream.map snd s |> Lwt_stream.to_list;;
   - : msg_att list = [`FLAGS [`Seen]]
   ```

### Debug trace

One can turn on debug output by setting the variable `Imap.Client.debug` to true:
```ocaml
# Imap.Client.debug := true;;
- : unit = ()
```
The same effect is achieved by setting the environment variable `IMAP_DEBUG`
to anything other than `0`.  This makes it very easy to see what is going on
behind the scenes:
```ocaml
# Imap_lwt.connect_ssl imap "imap.gmail.com";;
S: * OK Gimap ready for requests from 131.111.16.20 u46mb29270479wec
- : [ `Needsauth | `Preauth ] = `Needsauth
# Imap_lwt.login imap "<login>" "<password>";;
C: 1 LOGIN <login> <password>
S: * CAPABILITY IMAP4rev1 UNSELECT IDLE NAMESPACE QUOTA ID XLIST CHILDREN X-GM-EXT-1 UIDPLUS COMPRESS=DEFLATE ENABLE MOVE CONDSTORE ESEARCH
S: 1 OK <login> <user name> authenticated (Success)
- : unit = ()
# Imap_lwt.logout imap;;
C: 2 LOGOUT
S: * BYE LOGOUT Requested
S: 2 OK 73 good day (Success)
- : unit = ()
```

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
