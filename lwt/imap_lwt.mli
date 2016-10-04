(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 Nicolas Ojeda Bar <n.oje.bar@gmail.com>            *)

open Imap

type connection

val connect: int -> string -> connection Lwt.t

val login: connection -> string -> string -> unit Lwt.t
(** [login user pass] identifies the client to the server and carries the
    plaintext password authenticating this [user] with password [pass].  A
    server MAY include a [`Capability] response {{!code}code} in the tagged
    [`Ok] response to a successful [login] command in order to send capabilities
    automatically. *)

val authenticate: connection -> Auth.authenticator -> unit Lwt.t

val capability: connection -> Capability.capability list Lwt.t
(** [capability] returns the list of capabilities supported by the server.  The
    server must send a single untagged [`Capability] {{!untagged}response}
    with "IMAP4rev1" as one of the listed capabilities before the (tagged) [`Ok]
    response.  See the type describing the possible
    {{!capability}capabilities}. *)

val create: connection -> string -> unit Lwt.t
(** [create m] creates a mailbox named [m].  An [`Ok] response is returned only
    if a new mailbox with that name has been created.  It is an error to attempt
    to create "INBOX" or a mailbox with a name that refers to an existent mailbox.
    Any error in creation will return a tagged [`No] response. *)

val delete: connection -> string -> unit Lwt.t
(** [delete m] deletes a mailbox named [m].  An [`Ok] response is returned only
    if the mailbox with that name has been deleted.
    Any error in deletion will return a tagged [`No] response. *)

val rename: connection -> string -> string -> unit Lwt.t
(** [rename oldname newname] command changes the name of a mailbox from
    [oldname] to [newname].  A tagged [`Ok] response is returned only if the
    mailbox has been renamed.  It is an error to attempt to rename from a
    mailbox name that does not exist or to a mailbox name that already exists.
    Any error in renaming will return a tagged [`No] response. *)

val logout: connection -> unit Lwt.t
(** [logout] gracefully terminates a connection.  The server MUST send an untagged
    [`Bye] {{!untagged}response} before the (tagged) [`Ok] response. *)

val noop: connection -> unit Lwt.t
(** [noop] does nothing.  Since any command can return a status update as
    untagged data, the [noop] command can be used as a periodic poll for new
    messages or message status updates during a period of inactivity (this is
    the preferred method to do this). *)

val subscribe: connection -> string -> unit Lwt.t
(** [subscribe m] adds the mailbox [m] to the server's set of "active" or
    "subscribed" mailboxes as returned by the {!lsub} command. *)

val unsubscribe: connection -> string -> unit Lwt.t
(** [unsubcribe m] removes the mailbox [m] from the server's set of "active" or
    "subscribed" mailboxes as returned by the {!lsub} command. *)

val list: connection -> ?ref:string -> string -> (MbxFlag.mbx_flag list * char option * string) list Lwt.t
(** [list ref m] returns a subset of names from the complete set of all names
    available to the client.  Zero or more untagged [`List]
    {{!untagged}replies} are returned, containing the name attributes,
    hierarchy delimiter.  The optional argument [ref] is the name of a mailbox
    or a level of mailbox hierarchy, and indicates the context in which the
    mailbox name is interpreted.*)

val lsub: connection -> ?ref:string -> string -> (MbxFlag.mbx_flag list * char option * string) list Lwt.t
(** [lsub ref m] is identical to {!list}, except that it returns a subset of
    names from the set of names that the user has declared as being "active" or
    "subscribed". *)

val status: connection -> string -> StatusRequest.t list -> StatusData.t Lwt.t
(** [status] requests {{!status_query}status information} of the indicated
    mailbox.  An untagged [`Status] {{!untagged}response} is returned with
    the requested information. *)

val copy: connection -> SeqSet.t -> string -> unit Lwt.t
val uid_copy: connection -> UidSet.t -> string -> unit Lwt.t
(** [copy uid set m] copies the messages in [set] to the end of the specified
    mailbox [m].  [set] is understood as a set of message UIDs if [uid] is
    [true] (the default) or sequence numbers if [uid] is [false]. *)

val check: connection -> unit Lwt.t
(** [check] requests a checkpoint of the currently selected mailbox.  A
    checkpoint refers to any implementation-dependent housekeeping associated
    with the mailbox. *)

val close: connection -> unit Lwt.t
(** [close] permanently removes all messages that have the [`Deleted]
    {!flag} set from the currently selected mailbox, and returns to
    the authenticated state from the selected state. *)

val expunge: connection -> Seq.t list Lwt.t
(** [expunge] permanently removes all messages that have the [`Deleted]
    {!flag} set from the currently selected mailbox.  Before
    returning an [`Ok] to the client, an untagged [`Expunge]
    {{!untagged}response} is sent for each message that is removed. *)

val uid_search: connection -> Search.key -> (Uid.t list * Modseq.t option) Lwt.t
val search: connection -> Search.key -> (Seq.t list * Modseq.t option) Lwt.t
(** [search uid sk] searches the mailbox for messages that match the given
    searching criteria.  If [uid] is [true] (the default), then the matching
    messages' unique identification numbers are returned.  Otherwise, their
    sequence numbers are.  The untagged [`Search] {{!untagged}response}
    from the server contains a listing of message numbers corresponding to those
    messages that match the searching criteria. *)

val select: connection -> string -> unit Lwt.t
val condstore_select: connection -> string -> Modseq.t Lwt.t
(** [select condstore m] selects the mailbox [m] so that its messages can be
    accessed.  If [condstore] (default value [false]) is [true], then the server
    will return the [`Modseq] data item in all subsequent untagged [`Fetch]
    {{!untagged}responses}. *)

val examine: connection -> string -> unit Lwt.t
val condstore_examine: connection -> string -> Modseq.t Lwt.t
(** [examine condstore m] is identical to [select condstore m] and returns the
    same output; however, the selected mailbox is identified as read-only. *)

val append: connection -> string -> ?flags:Flag.flag list -> string -> unit Lwt.t
(** [append m flags id data] appends [data] as a new message to the end of the
    mailbox [m].  This argument should be in the format of an [RFC-2822]
    message.

    If a flag list is specified, the flags should be set in the resulting
    message; otherwise, the flag list of the resulting message is set to empty
    by default.  In either case, the [`Recent] flag is also set. *)

(** {2 Fetch commands}

    The IMAP [FETCH] command is used to retrieve the data associated to a
    message (or a set of messages).  Messages are identified either by their
    sequence number (i.e., their position in the mailbox), or by their unique
    identification number (UID).

    For those servers that support the [CONDSTORE] extension, one can pass a
    mod-sequence value [?changed] to restrict the set of affected messages to
    those that are not yet known by the client.  One can further use the
    [?vanished] argument to learn of recently expunged messages.  It is a
    programmer error to set [?vanished] to [true] but not to pass a value for
    [?changed]. *)

val fetch: connection -> ?changed:Modseq.t -> ?vanished:bool -> SeqSet.t -> FetchRequest.t list -> FetchData.t list Lwt.t
val uid_fetch: connection -> ?changed:Modseq.t -> ?vanished:bool -> UidSet.t -> FetchRequest.t list -> FetchData.t list Lwt.t
(** [fetch uid changed vanished set att] retrieves data associated with the
    message set [set] in the current mailbox.  [set] is interpeted as being a
    set of UIDs or sequence numbers depending on whether [uid] is [true] (the
    default) or [false].  Specifying a [?changed] argument will further reduce
    the set of returned messages to those whose [CHANGEDSINCE] mod-sequence
    value is at least the passed value (requires the [CONDSTORE] extension).
    The [vanished] optional parameter specifies whether one wants to receive
    [`Vanished] responses as well. *)

(** {2 Store commands} *)

val add_flags: connection -> ?silent:bool -> ?unchanged:Modseq.t -> SeqSet.t -> Flag.flag list -> FetchData.t list Lwt.t
val set_flags: connection -> ?silent:bool -> ?unchanged:Modseq.t -> SeqSet.t -> Flag.flag list -> FetchData.t list Lwt.t
val remove_flags: connection -> ?silent:bool -> ?unchanged:Modseq.t -> SeqSet.t -> Flag.flag list -> FetchData.t list Lwt.t
val uid_add_flags: connection -> ?silent:bool -> ?unchanged:Modseq.t -> UidSet.t -> Flag.flag list -> FetchData.t list Lwt.t
val uid_set_flags: connection -> ?silent:bool -> ?unchanged:Modseq.t -> UidSet.t -> Flag.flag list -> FetchData.t list Lwt.t
val uid_remove_flags: connection -> ?silent:bool -> ?unchanged:Modseq.t -> UidSet.t -> Flag.flag list -> FetchData.t list Lwt.t
(** [store_add_flags uid silent unchanged set flags] adds flags [flags] to the
    message set [set].  [set] is interpreter as being a set of UIDs or sequence
    numbers depending on whether [uid] is [true] (the default) or [false].  The
    server will return the updated flags for the affected messages in untagged
    [`Fetch] {{!untagged}responses} depending on whether [silent] is [true] (the
    default) or [false].  Specifying a [?unchanged] argument will further reduce
    the set of affected messages to those whose [UNCHANGEDSINCE] mod-sequence
    value is at least the passed value (requires the [CONDSTORE] extension). *)
(** [store_set_flags] is like {!store_add_flags} but replaces the set of flags
    instead of adding to it. *)
(** [store_remove_flags] is like {!store_add_flags} but removes flags instead of
    adding them. *)

val add_labels: connection -> ?silent:bool -> ?unchanged:Modseq.t -> SeqSet.t -> string list -> FetchData.t list Lwt.t
val set_labels: connection -> ?silent:bool -> ?unchanged:Modseq.t -> SeqSet.t -> string list -> FetchData.t list Lwt.t
val remove_labels: connection -> ?silent:bool -> ?unchanged:Modseq.t -> SeqSet.t -> string list -> FetchData.t list Lwt.t
val uid_add_labels: connection -> ?silent:bool -> ?unchanged:Modseq.t -> UidSet.t -> string list -> FetchData.t list Lwt.t
val uid_set_labels: connection -> ?silent:bool -> ?unchanged:Modseq.t -> UidSet.t -> string list -> FetchData.t list Lwt.t
val uid_remove_labels: connection -> ?silent:bool -> ?unchanged:Modseq.t -> UidSet.t -> string list -> FetchData.t list Lwt.t
(** [store_add_labels] is like {!store_add_flags} but adds
    {{:https://developers.google.com/gmail/imap_extensions}Gmail} {e labels}
    instead of regular flags. *)
(** [store_set_labels] is like {!store_add_labels} but replaces the set of
    labels instead of adding to it. *)
(** [store_remove_labels] is like {!store_add_labels} but removes labels instead
    of adding them. *)

val enable: connection -> Capability.capability list -> Capability.capability list Lwt.t
