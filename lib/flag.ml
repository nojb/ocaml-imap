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

let to_string = function
  | Answered -> "\\Answered"
  | Flagged -> "\\Flagged"
  | Deleted -> "\\Deleted"
  | Seen -> "\\Seen"
  | Draft -> "\\Draft"
  | Keyword s -> s
  | Extension s -> "\\" ^ s
  | Recent -> "\\Recent"
  | Any -> "\\*"
