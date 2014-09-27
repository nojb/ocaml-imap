# ocaml-imap - an IMAP4 client library

OCaml-IMAP is a fully featured OCaml client library for the [IMAP4rev1][]
protocol.  It consists of two parts.  Firstly, there is a low-level API that
follows the specification closely and is completely independent of any
particular I/O, error handling, or control flow mechanisms.  Secondly, there is
a high-level API built on top of the low-level one and [Lwt][] that liberates
the user from dealing with much of the low-level details necessary to interact
with an IMAP server.  Additionally it can automatically open more than one
connection in the background for greater performance.  Other concucurrency
frameworks (such as [Async][]) could be supported with not too much effort.

This is a preliminary release.

The full IMAP4rev1 protocol is supported in addition of some common extensions
(see below).

Contact: [Nicolas Ojeda Bar][]

[IMAP4rev1]: http://tools.ietf.org/html/rfc3501
[Lwt]: http://ocsigen.org/lwt/
[Async]: https://github.com/janestreet/async 
[Nicolas Ojeda Bar]: n.oje.bar@gmail.com

## Acknowledgements

This library (particularly the interface) has been greatly influenced by
[MailCore2][].

[MailCore2]: https://github.com/MailCore/mailcore2

## Requirements and installation

Use [OPAM][].

    opam install ssl lwt imap

[OPAM]: http://opam.ocaml.org

<!-- ## Review: SEQs, UIDs, and MODSEQs -->

<!-- There are two main issues to keep in mind with dealing with IMAP protocol. -->
<!-- Firstly is that it is possible for many clients to access/modify one mailbox -->
<!-- (even the same folder) concurrently.  Secondly, since mailboxes can contain -->
<!-- thousands or more messages, it is important to minimize the transfer of unneeded -->
<!-- information between the client and the server.  In particular, information that -->
<!-- has already been fetched should not be refetched again unless it has changed. -->

<!-- The purpose of this section is to review the three main technical concepts -->
<!-- needed to understand the interface afforded by this library. -->

## UIDs and Sequence numbers

Emails in a mail folder are numbered in an increasing manner from one upwards
(in order of arrival).  The number corresponding to each message is called its
*sequence number*.

These numbers are not very useful because they are not intrinsic to the message.
For example, if the first message in the mail folder is deleted the number of
*every* other message in the folder gets decreased by one.  Since many clients
may be accessing the mail folder at the same time, if you delete a message by
specifying its sequence number you may end up deleting the wrong message.
Clearly this is not very convenient.  See below for how to deal with this.

In the high-level API there is only function that uses sequence numbers:
`fetch_messages_by_number`.

A better way to specify messages in an IMAP folder is to use *unique
identification numbers* (from now on, UIDs).  As the name implies, they are
supposed to be unique (but only within the folder the message is residing in).
More importantly they are intrinsic to the message.  This means that no matter
what operation you do, a message will never change its UID (actually I am lying
a little here.  If you want to know how find out about **UIDVALIDITY**).

One downside of UIDs is that they are only valid within a folder, so if you copy
an email to another folder there is no reliable way in the base IMAP protocol to
know what is the UID of the copy.  Similarly if you upload (append) a new
message to the folder.  There is a common extension to the protocol
(**UIDPLUS**) that gives you this information.

Most of the high level API deals with UIDs and sets of UIDs to find, fetch, and
modify messages.

<!-- ### MODSEQs -->

<!-- The other type of number important in the IMAP universe is the *modification -->
<!-- sequence number* (from now on, MODSEQ).  It plays a role in trying to identify -->
<!-- information (such as flags, deleted messages, etc.) that has not changed and -->
<!-- therefore does not need to be fetched again by the client. **NOTE**: Only IMAP -->
<!-- servers supporting the **CONDSTORE** and/or **QRESYNC** extensions will actually -->
<!-- use MODSEQs. -->

<!-- The MODSEQ of a message is a timestamp that indicates the last time that its -->
<!-- information was changed. -->

## Usage

All the examples below are done using the [utop][] toplevel using the high-level
Lwt API.

[utop]: https://github.com/diml/utop

### Setup the toplevel

    # #require "imap.lwt";;
    # #require "imap.lwt.top";;
    # open ImapLwt;;

### Creating and Authenticating a Session

    # let s = create_session ~host:"imap.gmail.com" ~username:"myuser" ~password:"mysecret";;
    val s : session = <abstr>

Note that you never need to connect or authenticate explicitly.  It is all
taken care of automatically.  The session will automatically disconnect
after a period of inactivity (default: 1min) and then reconnect as needed.

To terminate the session gracefully, use

    # logout s;;
    - : unit = ()

### Finding out message UIDs

Before one can do anything with a message using IMAP it is often necessary to
know its UID.  The easiest way to do this is use the imap **SEARCH** command.
In the library this is done with `search command':

    # #show_val search;;
    val search : session -> folder:string -> key:search_key -> UidSet.t Lwt.t

The only interesting argument is labeled `key` and specifies which messages one
is looking for.  For example to find all messages from an email
`"important@email.com"` in the **INBOX** folder, one can simply do:

    # lwt uids = search s ~folder:"INBOX" ~key:(From "important@email.com")
    val uids : UidSet.t = <abstr>

### Fetching message data

Once one has the UIDs of the messages one intends to manipulate, the actual
fetching of data is done with the function `fetch_messages_by_uid`.  The following
will find out the size of all messages in **INBOX**.

    # lwt msgs = fetch_messages_by_uid s ~folder:"inbox" ~request:[Size] ~uids:UidSet.all;;
    val msgs : message list = [ ... ]

This function returns a list of `message` values.  The `message` type is given by

    # #show_type message;;
    type message = {
      uid : UidSet.elt;
      size : int;
      mod_seq_value : Modseq.t;
      gmail_labels : string list;
      gmail_message_id : Gmsgid.t;
      gmail_thread_id : Gthrid.t;
      flags : message_flag list;
      internal_date : float;
      main_part : part option;
      envelope : envelope option;
    }

Only the fields that were actually requested should be used (in this case, only
`size` should be used).  The different information that can be requested like
this is specified by the type `messages_request_type`:

    # #show_type messages_request_type;;
    type messages_request_kind =
    | Flags
    | Headers
    | Structure
    | InternalDate
    | FullHeaders
    | HeaderSubject
    | GmailLabels
    | GmailMessageID
    | GmailThreadID
    | ExtraHeaders of string list
    | Size

To fetch the full message corresponding to a given UID, one can use `fetch_message_by_uid`:

    # fetch_message_by_uid s ~uid;;
    - : string = "..."
            
### Changing flags and labels

Suppose we have an UID set `uids` and we want to label (in the Gmail sense) the
corresponding messages with label `MyLabel`.  It is very easy.

    # add_labels s ~folder:"INBOX" ~uids ~labels:["MyLabel"]

There is also `remove_labels` to remove a label, and `set_labels` to completely
replace the existing labels.

There is a parallel set of function for flags: `add_flags`, `remove_flags`, `set_flags` that
allow to modify the flags of a message.

    # #show_type message_flag;;
    type message_flag = Seen | Answered | Flagged | Deleted | Draft | MDNSent | Forwarded | SubmitPending | Submitted

### Debugging

To see a debug trace of everything that gets sent and received, set the Lwt log level to `Debug`.

    # Lwt_log.Section.set_level Lwt_log.Sectin.main Lwt_log.Debug;;

**NOTE**: This will print your username and password on the terminal.

## Supported IMAP extensions

- [COMPRESS=DEFLATE][]
- [IDLE][]
- [CONDSTORE][]
- [ID][]
- [UIDPLUS][]
- [X-GM-EXT-1][]
- [NAMESPACE][]
- [QRESYNC][]
- [ENABLE][]

[COMPRESS=DEFLATE]: http://tools.ietf.org/html/rfc4978
[IDLE]: http://tools.ietf.org/html/rfc2177
[CONDSTORE]: http://tools.ietf.org/html/rfc4551
[ID]: https://www.ietf.org/rfc/rfc2971.txt
[UIDPLUS]: http://tools.ietf.org/html/rfc4315
[X-GM-EXT-1]: https://developers.google.com/gmail/imap_extensions
[NAMESPACE]: http://tools.ietf.org/html/rfc2342
[QRESYNC]: http://tools.ietf.org/html/rfc5162
[ENABLE]: http://tools.ietf.org/html/rfc5161

## Comments

Any comments/suggestions/bug reports/etc are greatly appreciated!  Please email [me].

[me]: n.oje.bar@gmail.com
