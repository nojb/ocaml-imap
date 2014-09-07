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

open ImapTypes

val greeting_store : state -> greeting -> state

val cont_req_or_resp_data_store : state -> cont_req_or_resp_data -> state

val response_store : state -> response -> state

(* val debug : bool ref *)

val next_tag : state -> string * state

(** {2 Error handling} *)

(** Raised when the server returns a [NO] response. *)
(* exception NO *)

(** Raised when the server returns a [BAD] response. *)
(* exception BAD *)

(** Raised when the server closes the connection abruptly by sending a [BYE] message. *)
(* exception BYE *)

(** Raised when a server response cannot be parsed. *)
(* exception Parse_error of string * int *)

(** I/O error *)
(* exception Io_error of exn *)

(** Authentication error *)
(* exception Auth_error of exn *)

(** {2 IMAP sessions} *)

val fresh_selection_info : selection_info

val fresh_state : state

val greeting : resp_cond_auth_type control

val std_command : unit control -> (state -> 'a) -> 'a command

(** Creates a new IMAP session.  The session is initially disconnected and has
    to be connected using {!connect}. *)

(* val connect : session -> ?port:int -> string -> resp_cond_auth_type IO.t *)
(* (\** Connects to the IMAP server on the given port and hostname.  Returns *)
(*     [`Needsauth] if the server requires authentication and [`Preauth] if the *)
(*     session has already been authenticated in some other fashion. *\) *)

(* val connect_ssl : session -> ?version:[`TLSv1 | `SSLv23 | `SSLv3 ] -> ?ca_file : string -> *)
(*   ?port : int -> string -> resp_cond_auth_type IO.t *)
(* (\** Connects to the IMAP server using a SSL connection on the given port and *)
(*     hostname. [?version] is the version of the SSL protocol to use (default is *)
(*     TLSv1).  If [?ca_file] is not omitted, then it should point to a file in *)
(*     PEM format used to verify the server certificate.  Returns [`Needsauth] if *)
(*     the server requires authentication and [`Preauth] if the session has *)
(*     already been authenticated in some other fashion. *\) *)

(* val disconnect : session -> unit *)
(* (\** Disconnects from the server forcefully.  For a graceful exit, {!logout} is *)
(*     preferred. *\) *)

(* (\** {2 Commands valid in any state} *\) *)

val capability : capability list command

(* val capability : session -> capability list IO.t *)
(* (\** Queries the IMAP server for its capabilities by sending a {b CAPABILITY} *)
(*     command. *\) *)

val noop : unit command

(* val logout : session -> unit IO.t *)
(* (\** Logs out from the server by sending a {b LOGOUT} command. *\) *)

(* val id : session -> (string * string) list -> (string * string) list IO.t *)
(* (\** Sends an {b ID} command with an association list of identification *)
(*     parameters [params] and returns whatever identification parameters the *)
(*     server sends back. *)

(*     Some common values for the identification keys are: *)
(*     - {b name} Name of the program *)
(*     - {b version} Version number of the program *)
(*     - {b support-url} URL to contact for support *)

(*     This command requires the ID extension. *\) *)

(* (\** {2 Commands valid in {b Non-authenticated} state} *\) *)

(* val enable : session -> capability list -> capability list IO.t *)
(* (\** Tells the server to enable the given list of capabilities.  Returns the list *)
(*     of capabilities actually enabled. *)

(*     This command requires the ENABLE extension. *\) *)

(* val starttls : ?version : [ `TLSv1 | `SSLv23 | `SSLv3 ] -> ?ca_file : string -> *)
(*   session -> unit IO.t *)
(* (\** Start a TLS session using the given protocol.  If [?ca_file] is given, *)
(*     then it must point to a certificate in PEM format and will be used to *)
(*     validate the server identity. *\) *)

(* val authenticate : session -> ImapAuth.t -> unit IO.t *)
(* (\** [authenticate s auth] authenticates the client using the SASL mechanism *)
(*     [auth]. *\) *)

val login : string -> string -> unit command

(* (\** {2 Commands valid in {b Authenticated} or {b Selected} state} *\) *)

(* val compress : session -> unit IO.t *)
(* (\** Enables compression. *)

(*     This command requires the COMPRESS=DEFLATE extension. *\) *)

(* val select : session -> string -> unit IO.t *)
(* (\** Selects a mailbox so that the messages inside can be accessed and modified. *)
(*     Mailbox names are assumed to be encoded using UTF8. *\) *)

(* val select_condstore : session -> string -> Modseq.t IO.t *)
(* (\** Like {!select}, but returns the higest modification sequence of the *)
(*     mailbox. *)

(*     This command requires the CONDSTORE extension. *\) *)

(* val examine : session -> string -> unit IO.t *)
(* (\** Like {!select}, but opens the mailbox in read-only mode. *\) *)

(* val examine_condstore : session -> string -> Modseq.t IO.t *)
(* (\** Like {!select_condstore}, but opens the mailbox in read-only mode. *\) *)

val create : string -> unit command

val delete : string -> unit command

val rename : string -> string -> unit command

val subscribe : string -> unit command

val unsubscribe : string -> unit command

val list : string -> string -> mailbox_list list command

val lsub : string -> string -> mailbox_list list command

val status : string -> status_att list -> mailbox_data_status command

(* val append : session -> string -> ?flags:flag list -> ?date:float -> string -> unit IO.t *)
(* (\** [append s mbox ?flags ?date msg] appends a message [msg] to the mailbox *)
(*     [mbox].  The flags of the new message will be set to [?flags] (or the empty *)
(*     list if omitted).  The timestamp of the message will be set to [?date] (or *)
(*     current time and date if omitted). *\) *)

(* val append_uidplus : session -> string -> ?flags:flag list -> ?date:float -> string -> *)
(*   (Uid.t * Uid.t) IO.t *)
(* (\** Like {!append} but returns a pair with the updated value of UIDVALIDITY and *)
(*     the UID of the newly appended message. *)

(*     This command requires the UIDPLUS extension. *\) *)

(* val idle : session -> (unit -> [`Continue | `Stop]) -> unit IO.t * (unit -> unit) *)
(* (\** [idle s f] indicates the server that we are ready to accept real-time *)
(*     notifications by sending an {b IDLE} command.  It returns a pair [(idle, *)
(*     stop)] of a lwt thread [idle] which only returns once the IDLE command ends. *)
(*     The function [stop] can be used to end the IDLE command at any time.  If *)
(*     [stop] is invoked after the [IDLE] command has ended nothing will happen. *)
(*     Each time the server sends a response the function [f] is invoked.  If it *)
(*     returns [`Continue] we keep listening.  If it returns [`Stop], the IDLE *)
(*     command is stopped. *)

(*     This command requires the IDLE extension. *\) *)

(* (\* val namespace : session -> (namespace list * namespace list * namespace list) IO.t *\) *)
(* (\** Returns the three types of namespaces (personal, other users, shared) in the server. *)

(*     This command requires the NAMESPACE extension. *\) *)

(* (\** {2 Commands valid only in {b Selected} state} *\) *)

val check : unit command

val close : unit command

val expunge : unit command

(* val uid_expunge : session -> Uid_set.t -> unit IO.t *)
(* (\** [uid_expunge s uids] is like {!expunge} but only removes those messages *)
(*     whose unique identificatio number belongs to the set [uids]. *)

(*     This command requires the UIDPLUS extension. *\) *)

(* val search : session -> ?charset:string -> search_key -> Seq.t list IO.t *)
(* (\** [search s ?charset query] return the sequence numbers of all the messages *)
(*     that match the given criteria [query].  The parameter [?charset] specifies *)
(*     which encoding is used to encode whatever text is present in [query]. *\) *)

(* val uid_search : session -> ?charset:string -> search_key -> Uid.t list IO.t *)
(* (\** Like {!search}, but returns the unique identification numbers of the *)
(*     matching messages. *\) *)

(* val fetch : session -> Seq_set.t -> fetch_att list -> msg_att list IO.t *)
(* (\** [fetch s set atts h] retrieve flags and/or other attributes [att] for those *)
(*     messages whose message sequence numbers belong to [set].  The most common *)
(*     attribytes are: *)

(*     - [`BODYSECTION `ALL] - this returns the full message: headers *)
(*       and body, *)
(*     - [`BODYSECTION `TEXT] - this returns just the the text of the body *)
(*       of the message, not the header, *)
(*     - [`BODY] - this returns a {!Imap_body.t} describing the structure of the message, *)
(*     - [`ENVELOPE] - this parses the header and returns a {!Imap_envelope.t} with this *)
(*       information, *)
(*     - [`FLAGS] - the flags in the message, *)
(*     - [`UID] - the unique identifier of the message. *)

(*     The function [h] is called with each pair [(n, att)] consisting of a sequence *)
(*     number [n] and a message attribute [att]. *\) *)

(* val fetch_changedsince : session -> Seq_set.t -> Modseq.t -> fetch_att list -> msg_att list IO.t *)
(* (\** [fetch_changedsince s set modseq atts] is like {!fetch}, but only those *)
(*     messages that have a modification sequence number at least [modseq] are *)
(*     fetched. *)

(*     This command requires the CONDSTORE extension. *\) *)

(* val uid_fetch : session -> Uid_set.t -> fetch_att list -> msg_att list IO.t *)
(* (\** Like {!fetch}, but the elements of the set are taken to be unique *)
(*     identification numbers. *\) *)

(* val uid_fetch_changedsince : session -> Uid_set.t -> Modseq.t -> fetch_att list -> msg_att list IO.t *)
(* (\** Like {!fetch_changedsince}, but the elements fo the set are taken to be *)
(*     unique identification numbers. *)

(*     This command requires the CONDSTORE extension. *\) *)

(* (\* val store : session -> Seq_set.t -> [`Add | `Set | `Remove] -> store_att -> unit IO.t *\) *)
(* (\** [store s set mode silent flags] modifies the flags and/or other attributes *)
(*     for those messages whose sequence numbers belong to [set].  The attribute is *)
(*     added, remove, or changed (regardless of its original value) according to *)
(*     [mode]. *\) *)

(* (\* val store_unchangedsince : session -> Seq_set.t -> Modseq.t -> [`Add | `Set | `Remove] -> *\) *)
(* (\* store_att -> Seq_set.t IO.t *\) *)
(* (\** [store_unchangedsince s set modseq mode att] is like {!store}, but only *)
(*     those messages that have a modification sequence number not greater than *)
(*     [modseq] are affected.  Retruns the set of message numbers that failed the *)
(*     UNCHANGEDSINCE test. *)

(*     This command requires the CONDSTORE extension. *\) *)

(* (\* val uid_store : session -> Uid_set.t -> [`Add | `Set | `Remove] -> store_att -> unit IO.t *\) *)
(* (\** Like {!store} but the elements of the set are taken to be unique *)
(*     identification numbers. *\) *)

(* (\* val uid_store_unchangedsince : session -> Uid_set.t -> Modseq.t -> [`Add | `Set | `Remove] -> *\) *)
(* (\* store_att -> Uid_set.t IO.t *\) *)
(* (\** [uid_store_unchangedsince s set modseq mode att] is like {!uid_store}, but *)
(*     only those messages that have a modification sequence number not greater than *)
(*     [modseq] are affected.  Retruns the set of unique identification numbers that *)
(*     failed the UNCHANGEDSINCE test. *)

(*     This command requires the CONDSTORE extension. *\) *)

(* val copy : session -> Seq_set.t -> string -> unit IO.t *)
(* (\** Copies the given set of messages from the selected mailbox to the given *)
(*     mailbox.  The set elements are assumed to be sequence numbers. *\) *)

(* val uidplus_copy : session -> Seq_set.t -> string -> (Uid.t * Uid_set.t * Uid_set.t) IO.t *)
(* (\** Like {!copy}, but returns a tuple [(uid, srcuids, dstuids)] where [uid] is *)
(*     the updated UIDVALIDITY of the destination mailbox, [srcuids] is the set of *)
(*     message UIDs that where copied and [dstuids] is the sequence set of message UIDs *)
(*     assigned to the copied messages, in the same order. *)

(*     This command requires the UIDPLUS extension. *\) *)

(* val uid_copy : session -> Uid_set.t -> string -> unit IO.t *)
(* (\** Like {!copy} but the set elements are assumed to be unique identification numbers. *\) *)

(* val uidplus_uid_copy : session -> Uid_set.t -> string -> (Uid.t * Uid_set.t * Uid_set.t) IO.t *)
(* (\** Like {!uidplus_copy}, but the set elements are assumed to be unique *)
(*     identification numbers. *)

(*     This command requires the UIDPLUS extension. *\) *)

(* (\** {2 Session information} *\) *)

(* val state : session -> ImapState.state *)
(* (\** Returns the current known imap information. *\) *)

(* val is_busy : session -> bool *)
(* (\** Whether some command is in progress. *\) *)
