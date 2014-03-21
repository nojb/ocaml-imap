(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** IMAP protocol interface *)

open Imap_types
open Imap_uint
  
val debug : bool ref

(** Type of IMAP sessions. *)
type session

(** {2 Error handling} *)

(** Raised when the server returns a [NO] response. *)
exception NO

(** Raised when the server returns a [BAD] response. *)
exception BAD

(** Raised when the server closes the connection abruptly by sending a [BYE] message. *)
exception BYE

(** Raised when a server response cannot be parsed. *)
exception Parse_error of string * int

(** Raised if the session is not connected. *)
exception Not_connected

(** I/O error *)
exception Io_error of exn

(** Authentication error *)
exception Auth_error of exn

(** {2 IMAP sessions} *)

val make : ?port : int -> string -> session
(** [make ?port host] creates a new IMAP session with the server [host] using
    port [?port] (using the default port if omitted).  The session is initially
    disconnected and has to be connected using {!connect}. *)

val connect : ?ssl_context : Ssl.context -> session -> [ `Needsauth | `Preauth ] Lwt.t
(** Connects to the IMAP server.  It returns [`Needsauth] if the server requires
    authentication and [`Preauth] if the session has already been authenticated
    in some other fashion. *)

val disconnect : session -> unit
(** Disconnects from the server forcefully.  For a graceful exit, {!logout} is
    preferred. *)

(** {2 Commands valid in any state} *)

val capability : session -> capability list Lwt.t
(** Queries the IMAP server for its capabilities by sending a {b CAPABILITY}
    command. *)

val noop : session -> unit Lwt.t
(** Polls the server for an event by sending a {b NOOP} command. *)

val logout : session -> unit Lwt.t
(** Logs out from the server by sending a {b LOGOUT} command. *)

val id : session -> (string * string) list -> (string * string) list Lwt.t
(** Sends an {b ID} command with an association list of identification
    parameters [params] and returns whatever identification parameters the
    server sends back.

    Some common values for the identification keys are:
    - {b name} Name of the program
    - {b version} Version number of the program
    - {b support-url} URL to contact for support

    This command requires support for the ID extension. *)

(** {2 Commands valid in {b Non-authenticated} state} *)

val enable : session -> capability list -> capability list Lwt.t
(** Tells the server to enable the given list of capabilities.  Returns the list
    of capabilities actually enabled.  This requires support for the ENABLE
    extension. *)

val starttls : session -> Ssl.context -> unit Lwt.t
(** Start a TLS session in a given SSL context. *)

val authenticate : session -> Imap_auth.t -> unit Lwt.t
(** [authenticate s auth] authenticates the client using the SASL mechanism
    [auth]. *)

val login : session -> string -> string -> unit Lwt.t
(** [login s user pass] authenticates the client using login [user] and password
    [pass]. *)

(** {2 Commands valid in {b Authenticated} or {b Selected} state} *)

val compress : session -> unit Lwt.t
(** Enables compression.  This command requires supports for the
    COMPRESS=DEFLATE extension. *)

val select : session -> string -> unit Lwt.t
(** Selects a mailbox so that the messages inside can be accessed and modified.
    Mailbox names are assumed to be encoded using UTF8. *)

val select_condstore : session -> string -> uint64 Lwt.t
(** Like {!select}, but returns the higest modification sequence of the
    mailbox.  This command requires support for the CONDSTORE extension. *)

val examine : session -> string -> unit Lwt.t
(** Like {!select}, but opens the mailbox in read-only mode. *)

val examine_condstore : session -> string -> uint64 Lwt.t
(** Like {!select_condstore}, but opens the mailbox in read-only mode. *)
    
val create : session -> string -> unit Lwt.t
(** Creates a mailbox.  Mailbox names are assumed to be encoded using UTF8. *)

val delete : session -> string -> unit Lwt.t
(** Deletes a mailbox.  Mailbox names are assumed to be encoded using UTF8. *)

val rename : session -> string -> string -> unit Lwt.t
(** [rename s oldname newname] renames a mailbox from [oldname] to [newname].
    Mailbox names are assumed to be encoded using UTF8. *)
 
val subscribe : session -> string -> unit Lwt.t
(** Adds the specified mailbox name to the server's set of "active" or
    "subscribed" mailboxes.  Mailbox names are assumed to be encoded using
    UTF8. *)

val unsubscribe : session -> string -> unit Lwt.t
(** Removes the specified mailbox name from the server's set of "active" or
    "subscribed" mailboxes.  Mailbox names are assumed to be encoded using
    UTF8. *)

val list : session -> string -> string -> mailbox_list list Lwt.t
(** [list s base names] returns a list of all the mailbox based at [base]
    and matching [names].  Here [names] can contain wildcards like '*' and '%' (see RFC 3501). *)

val lsub : session -> string -> string -> mailbox_list list Lwt.t
(** Like {!list} but only return those mailboxes which are "subscribed" (see {!subscribe}). *)

val status : session -> string -> status_att list -> mailbox_data_status Lwt.t
(** [status s mbox atts] returns the value of the attributes [atts] of the
    mailbox [mbox]. *)

val append : session -> string -> ?flags:flag list -> ?date:float -> string -> (uint32 * uint32) Lwt.t
(** [append s mbox ?flags ?date msg] appends a message [msg] to the mailbox
    [mbox].  The flags of the new message will be set to [?flags] (or the empty
    list if omitted).  The timestamp of the message will be set to [?date] (or
    current time and date if omitted).

    If the server supports the UIDPLUS extension it returns the updated value of
    UIDVALIDITY for [mbox] and the UID of the newly appended message.
    Otherwise, it returns zero for both of them. *)

val idle : session -> (unit -> [`Continue | `Stop]) -> unit Lwt.t * (unit -> unit)
(** [idle s f] indicates the server that we are ready to accept real-time
    notifications by sending an {b IDLE} command.  It returns a pair [(idle,
    stop)] of a lwt thread [idle] which only returns once the IDLE command ends.
    The function [stop] can be used to end the IDLE command at any time.  If
    [stop] is invoked after the [IDLE] command has ended nothing will happen.
    Each time the server sends a response the function [f] is invoked.  If it
    returns [`Continue] we keep listening.  If it returns [`Stop], the IDLE
    command is stopped.

    This command requires support for the IDLE extension. *)

val namespace : session -> (namespace list * namespace list * namespace list) Lwt.t
(** Returns the three types of namespaces (personal, other users, shared) in the server.

    This command requires support for the NAMESPACE extension. *)
    
(** {2 Commands valid only in {b Selected} state} *)

val check : session -> unit Lwt.t
(** Request a checkpoint of the currently selected mailbox.  The exact meaning
    of this is implementation-dependant. *)

val close : session -> unit Lwt.t
(** Closes the currently selected mailbox. *)
    
val expunge : session -> unit Lwt.t
(** Permanently removes the messages from the selected mailbox that have the
    [\Deleted] flag set. *)
    
val uid_expunge : session -> Imap_set.t -> unit Lwt.t
(** [uid_expunge s uids] is like {!expunge} but only removes those messages
    whose unique identificatio number belongs to the set [uids].

    This command requires support for the UIDPLUS extension. *)
    
val search : session -> ?charset:string -> search_key -> uint32 list Lwt.t
(** [search s ?charset query] return the sequence numbers of all the messages
    that match the given criteria [query].  The parameter [?charset] specifies
    which encoding is used to encode whatever text is present in [query]. *)

val uid_search : session -> ?charset:string -> search_key -> uint32 list Lwt.t
(** Like {!search}, but returns the unique identification numbers of the
    matching messages. *)

val fetch : session -> Imap_set.t ->
  ?changedsince:uint64 -> fetch_att list -> msg_att list Lwt.t
(** [fetch s set atts] retrieve flags and/or other attributes [att] for those
    messages whose message sequence numbers belong to [set].  The most common
    attribytes are:

    - [`BODYSECTION `ALL] - this returns the full message: headers
      and body,
    - [`BODYSECTION `TEXT] - this returns just the the text of the body
      of the message, not the header,
    - [`BODY] - this returns a {!Imap_body.t} describing the structure of the message,
    - [`ENVELOPE] - this parses the header and returns a {!Imap_envelope.t} with this
      information,
    - [`FLAGS] - the flags in the message,
    - [`UID] - the unique identifier of the message.

    If the [?changedsince] argument is given, only those messages that have a
    modification sequence number at least the argument are fetched.  This
    requires support for the CONDSTORE extension. *)

val uid_fetch : session -> Imap_set.t ->
  ?changedsince:uint64 -> fetch_att list -> msg_att list Lwt.t
(** Like {!fetch}, but the elements of the set are taken to be unique
    identification numbers. *)
    
val store : session -> Imap_set.t -> ?unchangedsince:uint64 ->
  [`Add | `Set | `Remove] -> store_att -> unit Lwt.t
(** [store s set mode silent flags] modifies the flags and/or other attributes
    for those messages whose sequence numbers belong to [set].  The attribute is
    added, remove, or changed (regardless of its original value) according to
    [mode].

    If the argument [?unchangedsince] is given, only those messages that have a
    modification sequence number not greater than the argument are affected.  This
    requires support for the CONDSTORE extension. *)

val uid_store : session -> Imap_set.t -> ?unchangedsince:uint64 ->
  [`Add | `Set | `Remove] -> store_att -> unit Lwt.t
(** Like {!store} but the elements of the set are taken to be unique
    identification numbers. *)
    
val copy : session -> Imap_set.t -> string -> unit Lwt.t
(** Copies the given set of messages from the selected mailbox to the given
    mailbox.  The set elements are assumed to be sequence numbers. *)

val uid_copy : session -> Imap_set.t -> string -> unit Lwt.t
(** Like {!copy} but the set elements are assumed to be unique identification numbers. *)

(** {2 Session information} *)
    
val has_uidplus : session -> bool
(** Whether the IMAP server supports the UIDPLUS extension. *)
  
val has_compress_deflate : session -> bool
(** Whether the IMAP server supports the COMPRESS=DEFLATE extension. *)
  
val has_id : session -> bool
(** Whether the IMAP server supports the ID extension. *)
  
val has_condstore : session -> bool
(** Whether the IMAP server supports the CONDSTORE extension. *)
  
val has_x_gm_ext_1 : session -> bool
(** Whether the IMAP server supports the X-GM-EXT-1 extension (probably only
    Google supports this). *)

val has_namespace : session -> bool
(** Whether the IMAP server supports the NAMESPACE extension. *)

val has_enable : session -> bool
(** Whether the IMAP server supports the ENABLE extension. *)
  
val last_response : session -> string
(** The descriptive text of the last tagged response (or the last BYE
    (untagged) response from the server. *)
  
val response_info : session -> response_info
(** Returns information about the last server response. *)
  
val selection_info : session -> selection_info
(** Returns information about the last selected mailbox. *)
  
val capability_info : session -> capability_info
(** Returns the last known list of server capabilities.  It does not actually
    contact the server. *)

val is_busy : session -> bool
(** Whether some command is in progress. *)
