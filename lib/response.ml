type modseq = int64

type uid = int32

type seq = int32

type date =
  {
    day: int;
    month: int;
    year: int;
  }

type time =
  {
    hours: int;
    minutes: int;
    seconds: int;
    zone: int;
  }


type address =
  {
    ad_name: string;
    ad_adl: string;
    ad_mailbox: string;
    ad_host: string;
  }

type envelope =
  {
    env_date: string;
    env_subject: string;
    env_from: address list;
    env_sender: address list;
    env_reply_to: address list;
    env_to: address list;
    env_cc: address list;
    env_bcc: address list;
    env_in_reply_to: string;
    env_message_id: string;
  }

module MIME = struct
  type fields =
    {
      fld_params : (string * string) list;
      fld_id : string option;
      fld_desc : string option;
      fld_enc : string;
      fld_octets : int;
    }

  type body_extension =
    | List of body_extension list
    | Number of int32
    | String of string

  type part_extension =
    {
      ext_dsp: (string * (string * string) list) option;
      ext_lang: string list;
      ext_loc: string option;
      ext_ext: body_extension list;
    }

  type mime =
    | Text of string * fields * int
    | Message of fields * envelope * mime * int
    | Basic of string * string * fields
    | Multipart of mime list * string
end

module MbxFlag = struct
  type t =
    | Noselect
    | Marked
    | Unmarked
    | Noinferiors
    | HasChildren
    | HasNoChildren
    | All
    | Archive
    | Drafts
    | Flagged
    | Junk
    | Sent
    | Trash
    | Extension of string
end

type state =
  | OK of Code.t option * string
  | NO of Code.t option * string
  | BAD of Code.t option * string

type section_msgtext =
  | HEADER
  | HEADER_FIELDS of string list
  | HEADER_FIELDS_NOT of string list
  | TEXT
  | MIME

type section =
  int list * section_msgtext option

type msg_att =
  | FLAGS of Flag.t list
  | ENVELOPE of envelope
  | INTERNALDATE of date * time
  (* | RFC822 of string option *)
  (* | RFC822_HEADER of string option *)
  (* | RFC822_TEXT of string option *)
  | RFC822_SIZE of int
  | BODY of MIME.mime
  | BODYSTRUCTURE of MIME.mime
  | BODY_SECTION of section * string option
  | UID of int32
  | MODSEQ of int64
  | X_GM_MSGID of int64
  | X_GM_THRID of int64
  | X_GM_LABELS of string list

type mbx_att =
  | MESSAGES of int
  | RECENT of int
  | UIDNEXT of int32
  | UIDVALIDITY of int32
  | UNSEEN of int
  | HIGHESTMODSEQ of int64

type untagged =
  | State of state
  | BYE of Code.t option * string
  | PREAUTH of Code.t option * string
  | FLAGS of Flag.t list
  | LIST of MbxFlag.t list * char option * string
  | LSUB of MbxFlag.t list * char option * string
  | SEARCH of int32 list * int64 option
  | STATUS of string * mbx_att list
  | EXISTS of int
  | RECENT of int
  | EXPUNGE of int32
  | FETCH of int32 * msg_att list
  | CAPABILITY of Capability.t list
  | VANISHED of Uint32.Set.t
  | VANISHED_EARLIER of Uint32.Set.t
  | ENABLED of Capability.t list

type response =
  | Untagged of untagged
  | Cont of string
  | Tagged of string * state
