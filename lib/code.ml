type t =
  | ALERT
  | BADCHARSET of string list
  | CAPABILITY of Capability.t list
  | PARSE
  | PERMANENTFLAGS of Flag.t list
  | READ_ONLY
  | READ_WRITE
  | TRYCREATE
  | UIDNEXT of int32
  | UIDVALIDITY of int32
  | UNSEEN of int32
  | OTHER of string * string option
  | CLOSED
  | HIGHESTMODSEQ of int64
  | NOMODSEQ
  | MODIFIED of (int32 * int32) list
  | APPENDUID of int32 * int32
  | COPYUID of int32 * (int32 * int32) list * (int32 * int32) list
  | UIDNOTSTICKY
  | COMPRESSIONACTIVE
  | USEATTR
