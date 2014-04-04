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

(** IMAP queries and response data *)

open Sexplib.Std
open Imap_uint
  
module type S = sig
  type t with sexp
  val zero : t
  val of_int : int -> t
  val of_string : string -> t
  val to_string : t -> string
  val max : t -> t -> t
  val is_zero : t -> bool
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val succ : t -> t
  val printer : Format.formatter -> t -> unit
end

module type S32 = sig
  include S
  val of_uint32 : Uint32.t -> t
end

module type S64 = sig
  include S
  val of_uint64 : Uint64.t -> t
end

module Uid : S32
module Seq : S32
module Gmsgid : S64
module Gthrid : S64
module Modseq : S64

module Uid_set : module type of Imap_set.Make (Uid)
module Seq_set : module type of Imap_set.Make (Seq)

(** {2 Message flags} *)
                
type flag =
  [ `Answered
  (** [\Answered] flag *)
  | `Flagged
  (** [\Flagged] flag *)
  | `Deleted
  (** [\Deleted] flag *)
  | `Seen
  (** [\Seen] flag *)
  | `Draft
  (** [\Draft] flag *)
  | `Recent
  (** [\Recent] flag *)
  | `Keyword of string
  (** keyword flag *)
  | `Extension of string
  (** \extension flag *) ] with sexp

(** Flags returned with a PERMANENTFLAG response code *)
type flag_perm =
  [ flag
  | `All
  (** New flags can be created *) ] with sexp

(** {2 SEARCH command} *)

type day_month_year =
  int * int * int

(** Search keys *)
type search_key =
  [ `ALL
  (** All messages in the mailbox. *)
  | `ANSWERED
  (** Messages with the [\Answered] flag set. *)
  | `BCC of string
  (** Messages that contain the specified string in the envelope's [BCC] field. *)
  | `BEFORE of day_month_year
  (** Messages whose internal date (disregarding time and timezone) is earlier
      than the specified date. *)
  | `BODY of string
  (** Messages that contain the specified string in the body of the message. *)
  | `CC of string
  (** Messages that contain the specified string in the envelope's [CC] field. *)
  | `DELETED
  (** Messages with the [\Deleted] flag set. *)
  | `FLAGGED
  (** Messages with the [\Flagged] flag set. *)
  | `FROM of string
  (** Messages that contain the specified string in the envelope's [FROM]
      field. *)
  | `KEYWORD of string
  (** Messages with the specified keyword flag set. *)
  | `NEW
  (** Messages that have the \Recent flag set but not the \Seen flag.  This is
      functionally equivalent to [`AND (`RECENT, `UNSEEN)]. *)
  | `OLD
  (** Messages that do not have the [\Recent] flag set.  This is functionally
      equivalent to [`NOT `RECENT] (as opposed to [`NOT `NEW]). *)
  | `ON of day_month_year
  (** Messages whose internal date (disregarding time and timezone) is within
      the specified date. *)
  | `RECENT
  (** Messages that have the [\Recent] flag set. *)
  | `SEEN
  (** Messages that have the [\Seen] flag set. *)
  | `SINCE of day_month_year
  (** Messages whose internal date (disregarding time and timezone) is within or
      later than the specified date. *)
  | `SUBJECT of string
  (** Messages that contain the specified string in the envelope's [SUBJECT]
      field. *)
  | `TEXT of string
  (** Messages that contain the specified string in the header or body of the
     message. *)
  | `TO of string
  (** Messages that contain the specified string in the envelope's [TO]
      field. *)
  | `UNANSWERED
  (** Messages that do not have the [\Answered] flag set. *)
  | `UNDELETED
  (** Messages that do not have the [\Deleted] flag set. *)
  | `UNFLAGGED
  (** Messages that do not have the [\Flagged] flag set. *)
  | `UNKEYWORD of string
  (** Messages that do not have the specified keyword flag set. *)
  | `UNSEEN
  (** Messages that do not have the [\Seen] flag set. *)
  | `DRAFT
  (** Messages with the [\Draft] flag set. *)
  | `HEADER of string * string
  (** Messages that have a header with the specified field-name (as defined in
      [RFC-2822]) and that contains the specified string in the text of the header
      (what comes after the colon). *)
  | `LARGER of int
  (** Messages with an [RFC-2822] size larger than the specified number of
      bytes. *)
  | `NOT of search_key
  (** Messages that do not match the specified search key. *)
  | `OR of search_key * search_key
  (** Messages that match either search key. *)
  | `SENTBEFORE of day_month_year
  (** Messages whose [Date:] header (disregarding time and timezone) is earlier
      than the specified date. *)
  | `SENTON of day_month_year
  (** Messages whose [Date:] header (disregarding time and timezone) is within
      the specified date. *)
  | `SENTSINCE of day_month_year
  (** Messages whose [Date:] header (disregarding time and timezone) is within
      or later than the specified date. *)
  | `SMALLER of int
  (** Messages with an [RFC-2822] size smaller than the specified number of
      bytes. *)
  | `UID of Uid_set.t
  (** Messages with unique identifiers corresponding to the specified unique
      identifier set. *)
  | `UNDRAFT
  (** Messages that do not have the [\Draft] flag set. *)
  | `INSET of Seq_set.t
  (** Messages with message sequence numbers corresponding to the specified
      message sequence number set. *)
  | `AND of search_key * search_key
  (** Messages that match both search keys. *)
  | `MODSEQ of (flag * [`Shared | `Priv | `All]) option * Modseq.t
  | `X_GM_RAW of string
  (** Messages that satisfy Gmail search expression. *)
  | `X_GM_MSGID of Gmsgid.t
  (** Message with given Gmail Message ID. *)
  | `X_GM_THRID of Gthrid.t
  (** Messages with given Gmail Thread ID. *)
  | `X_GM_LABELS of string
  (** Messages with given Gmail labels. *) ]

(** {2 FETCH command} *)

(** {3 Queries} *)

type section_msgtext =
  [ `HEADER
  (** All header fields *)
  | `HEADER_FIELDS of string list
  (** Header fields in the given list. *)
  | `HEADER_FIELDS_NOT of string list
  (** Header fields not in the given list. *)
  | `TEXT
  (** Text body of the message, omitting headers. *)
  | `ALL
  (** Everything *) ] with sexp

type section_spec =
  [ section_msgtext
  | `MIME
  (** MIME header for the corresponding part *)
  | `PART of int * section_spec ] with sexp

type section =
  [ section_msgtext
  | `PART of int * section_spec
  (** Part in a multipart message *) ] with sexp

type fetch_att_section =
  [ section
  | `PARTIAL of section * int * int
  (** Partial section, offset, length *) ] with sexp

(** FETCH queries *)
type fetch_att =
  [ `ENVELOPE
  (** MIME envelope information *)
  | `INTERNALDATE
  (** The message date kept by the server *)
  | `RFC822_HEADER
  (** The message header *)
  | `RFC822_TEXT
  (** The message text part *)
  | `RFC822_SIZE
  (** The size of the message content *)
  | `RFC822
  (** The message content (header and body) *)
  | `BODY
  (** The MIME description of the message *)
  | `BODYSECTION of fetch_att_section
  (** A MIME part content *)
  | `BODYPEEK of fetch_att_section
  (** Like [`BODYSECTION], but does not set the [`Seen] flag. *)
  | `BODYSTRUCTURE
  (** The MIME description of the message with additional information *)
  | `UID
  (** Unique identification number. *)
  | `FLAGS
  (** Message flags *)
  | `MODSEQ
  (** Modification sequence number.  This requires support for the CONDSTORE
      extension. *)
  | `X_GM_MSGID
  (** Gmail message ID. *)
  | `X_GM_THRID
  (** Gmail thread ID. *)
  | `X_GM_LABELS
  (** Gmail labels *) ] with sexp

(** {3 Responses} *)
  
type date_time = {
  dt_day : int;
  dt_month : int;
  dt_year : int;
  dt_hour : int;
  dt_min : int;
  dt_sec : int;
  dt_zone : int
} with sexp

type msg_att_section =
  [ section
  | `PARTIAL of section * int ] with sexp

type msg_att_static =
  [ `ENVELOPE of Mime.envelope
  | `INTERNALDATE of date_time
  | `RFC822 of string
  | `RFC822_HEADER of string
  | `RFC822_TEXT of string
  | `RFC822_SIZE of int
  | `BODY of Mime.body
  | `BODYSTRUCTURE of Mime.body
  | `BODYSECTION of msg_att_section * string
  | `UID of Uid.t
  | `X_GM_MSGID of Gmsgid.t
  | `X_GM_THRID of Gthrid.t ] with sexp

type msg_att_dynamic =
  [ `FLAGS of flag list
  | `MODSEQ of Modseq.t
  | `X_GM_LABELS of string list ] with sexp
  
type msg_att =
  [ msg_att_static | msg_att_dynamic ] with sexp

(** {2 STORE command} *)

type store_att =
  [ `FLAGS of flag list
  (** Message flags *)
  | `FLAGS_SILENT of flag list
  (** Silent message flags (server will not respond with the updated list of flags). *)
  | `X_GM_LABELS of string list
  (** Gmail labels *)
  | `X_GM_LABELS_SILENT of string list
  (** Silent Gmail labels (server will not respond with the updated list of labels). *) ] with sexp

(** {2 STATUS command} *)

(** STATUS queries: the different mailbox status attributes that can be fetched
    via {!Imap.status}. *)
type status_att =
  [ `MESSAGES
  (** Number of messages in the mailbox. *)
  | `RECENT
  (** Number of new messages in the mailbox. *)
  | `UIDNEXT
  (** The probable unique identification number of the next message to arrive. *)
  | `UIDVALIDITY
  (** The UID validity value of the mailbox. *)
  | `UNSEEN
  (** The number of unseen messages in the mailbox. *)
  | `HIGHESTMODSEQ
  (** The highest modification sequence of the mailbox.  This requires support
      for the CONDSTORE extension. *) ] with sexp

(** STATUS replies. See {!status_att}. *)
type status_info =
  [ `MESSAGES of int
  | `RECENT of int
  | `UIDNEXT of Uid.t
  | `UIDVALIDITY of Uid.t
  | `UNSEEN of int
  | `HIGHESTMODSEQ of Modseq.t ] with sexp

(** {2 LIST/LSUB commands} *)

type mailbox_data_status = {
  st_mailbox : string;
  st_info_list : status_info list
} with sexp

(** The type of mailbox single flags *)
type sflag =
  [ `Noselect
  (** [\Noselect] flag *)
  | `Marked
  (** [\Marked] flag *)
  | `Unmarked
  (** [\Unmarked] flag *) ] with sexp

(** The type of mailbox other flags *)
type oflag =
  [ `Noinferiors
  (** [\Noinferiors] flag *)
  | `Extension of string
  (** other flag *) ] with sexp

(** Mailbox flag *)
type mbx_list_flags = {
  mbf_sflag : sflag option;
  (** Mailbox single flag *)
  mbf_oflags : oflag list
  (** List of "mailbox other flag" *)
} with sexp

(** List of mailbox flags *)
type mailbox_list = {
  mb_flag : mbx_list_flags;
  (** List of mailbox flags *)
  mb_delimiter : char;
  (** Delimiter of the mailbox path, '\000' if not present *)
  mb_name : string
  (** Name of the mailbox *)
} with sexp

(** {2 CAPABILITY command} *)

type capability =
  [ `AUTH_TYPE of string
  (** Authentication type *)
  | `NAME of string
  (** Other type of capability *) ] with sexp

(** {2 NAMESPACE command} *)

type namespace = {
  ns_prefix : string;
  ns_delimiter : char;
  ns_extensions : (string * string list) list
} with sexp

(** {2 Untagged information sent by the server} *)

type selection_info = {
  sel_perm_flags : flag_perm list;
  sel_perm : [ `READ_ONLY | `READ_WRITE ];
  sel_uidnext : Uid.t;
  sel_uidvalidity : Uid.t;
  sel_first_unseen : Seq.t;
  sel_flags : flag list;
  sel_exists : int option;
  sel_recent : int option;
  sel_uidnotsticky : bool;
  sel_highestmodseq : Modseq.t
}
