type t =
  | Answered
  | Flagged
  | Deleted
  | Seen
  | Draft
  | Keyword of string
  | Extension of string
  | Recent
  | Any
