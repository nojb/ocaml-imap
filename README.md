# ocaml-imap

OCaml-IMAP is an OCaml client library for the
[IMAP4rev1](http://tools.ietf.org/html/rfc3501) protocol. It uses
[Lwt](http://ocsigen.org/lwt/) to support concurrency.  It aims to be correct,
easy to use and complete.

This is a preliminary release.  While the library has seen some limited testing,
more testing is required before it can be considered ready.

The library consists mainly of a low-level interface to the IMAP protocol and
some of its extensions. It does not try to keep track of things such as what is
the currently selected folder, but instead concentrates in the coding/decoding
of the IMAP protocol.  There is also a higher-level interface that does many of
these other things, but this interface is not yet ready for release.

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

1. Install the dependencies using OPAM:
   ```sh
   opam install ocamlfind uint ssl cryptokit ucorelib lwt sexplib camlzip
   ```

2. Install the preliminary GNU SASL bindings from https://github.com/nojb/ocaml-gsasl.
   You can look there for the detailed instructions, but briefly, the steps are:
   - Install the [GNU SASL library](http://www.gnu.org/software/gsasl/) (in OS
     X, you can just do `brew install gsasl` for this).
   - Install ctypes (using OPAM you can do `opam install ctypes`)
   - Build and install `ocaml-gsasl`
   ```sh
   cd ~/tmp
   git clone https://github.com/nojb/ocaml-gsasl
   cd ocaml-gsasl
   make
   make install
   ```
   (NB: in order to uninstall these bindings do `ocamlfind remove gsasl`.)

3. (Optional) Install the `utop` toplevel:
   ```sh
   opam install utop
   ```

4. Install `ocaml-imap` also from git:
   ```sh
   cd ~/tmp
   git clone https://github.com/nojb/ocaml-imap
   cd ocaml-imap
   make
   make install
   ```
   (NB: in order to uninstall this package do `ocamlfind remove imap`.)

5. Read the documentation in `lib/imap.mli`, `lib/imap_response.mli`, and
   `lib/imap_types.mli`.

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
   # open Imap_types;;
   # open Imap_body;;
   # open Imap_envelope;;
   # open Imap_uint;;
   ```

2. Set up and open a SSL connection to the IMAP server
   ```ocaml
   # let imap = Imap.make ();;
   val imap : Imap.session = <abstr>
   # Imap.connect_simple imap "imap.gmail.com";;
   - : [ `Needsauth | `Preauth ] = `Needsauth
   ```
   This means that the SSL connection has been established successfully and the
   server requires authentication.

3. Authenticate with login and password.
   ```ocaml
   # Imap.login imap "<login>" "<password>";;
   - : unit = ()
   ```
   At this point we have successfully authenticated and we are ready to interact
   with the server.

4. Logout
   ```ocaml
   # Imap.logout imap;;
   - : unit = ()
   ```

### Selecting and closing mailbox

1. Select the INBOX mailbox in order to look at its messages.
   ```ocaml
   # Imap.select imap "inbox";;
   - : unit = ()
   ```

2. Get the number of unread messages and the total number of messages in the
   mailbox:
   ```ocaml
   # Imap.status imap "inbox" [`UNSEEN; `MESSAGES];;
   - : mailbox_data_status = {st_mailbox = "inbox"; st_info_list = [`MESSAGES 11; `UNSEEN 8]}
   ```
   So there are 11 messages and 8 unread messages.

3. Close the mailbox.
   ```ocaml
   # Imap.close imap;;
   - : unit = ()
   # Imap.last_response imap;;
   - : string = "Returned to authenticated state. (Success)"
   ```
   
### Fetch message MIME information

1. Get the UIDs of unread messages in the mailbox.
   ```ocaml
   # let uids = Imap.uid_search imap `UNSEEN;;
   val uids : uint32 list Lwt.t = <abstr>
   # uids;;
   - : uint32 list = [2Ul; 3Ul; 4Ul; 5Ul; 7Ul; 8Ul; 9Ul; 10Ul]
   ```
   
2. Get envelope information for message with UID 2.
   ```ocaml
   # Imap.uid_fetch (Imap_set.single' 2) [`ENVELOPE];;
   - : msg_att list =
   [(2Ul,
     [`ENVELOPE
       {env_date = "Tue, 4 Feb 2014 07:52:19 -0800";
        env_subject = "The best of Gmail, wherever you are";
        env_from = [{addr_name = "Gmail Team"; addr_adl = ""; addr_mailbox = "mail-noreply"; addr_host = "google.com"}];
        env_sender = [{addr_name = "Gmail Team"; addr_adl = ""; addr_mailbox = "mail-noreply"; addr_host = "google.com"}];
        env_reply_to = [{addr_name = "Gmail Team"; addr_adl = ""; addr_mailbox = "mail-noreply"; addr_host = "google.com"}];
        env_to = [{addr_name = "imaplib test"; addr_adl = ""; addr_mailbox = "imaplibtest"; addr_host = "gmail.com"}];
        env_cc = [];
        env_bcc = [];
        env_in_reply_to = "";
        env_message_id = "<CAFo37G3POBokcVK2ZDKMXi35iOnXH5XKyAztFFFeUVy2J1QrgA@mail.gmail.com>"}])]
   ```

3. Get MIME body structure
   ```ocaml
   # Imap.uid_fetch (Imap_set.single' 2) [`BODYSTRUCTURE];;
   - : msg_att list =
   [(2Ul,
     [`BODYSTRUCTURE
        (Multi_part
          {bd_subtype = "ALTERNATIVE";
           bd_parts =
            [Text
              {bd_param = [("CHARSET", "ISO-8859-1")]; bd_id = None; bd_desc = None; 
               bd_enc = `QUOTED_PRINTABLE; bd_octets = 656; bd_other = {text_subtype = "PLAIN"; text_lines = 23};
               bd_ext = {ext_md5 = None; ext_dsp = None; ext_lang = []; ext_exts = []}};
             Text
              {bd_param = [("CHARSET", "ISO-8859-1")]; bd_id = None; bd_desc = None; 
               bd_enc = `QUOTED_PRINTABLE; bd_octets = 3374; bd_other = {text_subtype = "HTML"; text_lines = 46}; 
               bd_ext = {ext_md5 = None; ext_dsp = None; ext_lang = []; ext_exts = []}}];
               bd_mexts = {mext_param = [("BOUNDARY", "089e0122f720083c1b04f196a0a7")]; mext_dsp = None; mext_lang = []; mext_exts = []}})])]
   ```
            
### Inspect and change flags and labels

1. Get flags for message with UID 11.
   ```ocaml
   # Imap.uid_fetch imap (Imap_set.single' 11) [`FLAGS];;
   - : msg_att list = [(11Ul, [`FLAGS []])]
   ```

2. Get Gmail labels for message with UID 11.
   ```ocaml
   # Imap.uid_fetch imap (Imap_set.single' 11) [`X_GM_LABELS];;
   - : msg_att list = [(11Ul, [`X_GM_LABELS ["\\Important"]])]
   ```

1. Mark message with UID 11 as `Seen`.
   ```ocaml
   # Imap.uid_store imap (Imap_set.single' 11) `Add (`FLAGS [`Seen]);;
   - : unit = ()
   # Imap.uid_fetch imap (Imap_set.single' 11) [`FLAGS];;
   - : msg_att list = [(11Ul, [`FLAGS [`Seen]])]
   ```

### Debug trace

One can turn on debug output by setting the variable `Imap.debug` to true:
```ocaml
# Imap.debug := true;;
- : unit = ()
```
The same effect is achieved by setting the environment variable `IMAP_DEBUG`
to anything other than `0`.  This makes it very easy to see what is going on
behind the scenes:
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
