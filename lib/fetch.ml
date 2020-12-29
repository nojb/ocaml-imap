(* The MIT License (MIT)

   Copyright (c) 2015-2018 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

open Common

module Date = struct
  type t = { day : int; month : int; year : int }

  let to_string { day; month; year } =
    let months =
      [|
        "Jan";
        "Feb";
        "Mar";
        "Apr";
        "May";
        "Jun";
        "Jul";
        "Aug";
        "Sep";
        "Oct";
        "Nov";
        "Dec";
      |]
    in
    Printf.sprintf "%2d-%s-%4d" day months.(month) year

  let encode d = Encoder.raw (to_string d)
end

module Time = struct
  type t = { hours : int; minutes : int; seconds : int; zone : int }

  let to_string { hours; minutes; seconds; zone } =
    Printf.sprintf "%02d:%02d:%02d %c%04d" hours minutes seconds
      (if zone >= 0 then '+' else '-')
      (abs zone)
end

module MessageAttribute = struct
  type t =
    | FLAGS of Flag.t list
    | ENVELOPE of Envelope.t
    | INTERNALDATE of string (* Date.t * Time.t *)
    | RFC822 of string
    | RFC822_HEADER of string
    | RFC822_TEXT of string
    | RFC822_SIZE of int
    | BODY of MIME.Response.t
    | BODYSTRUCTURE of MIME.Response.t
    | BODY_SECTION of MIME.Section.t * string option
    | UID of int32
    | MODSEQ of int64
    | X_GM_MSGID of int64
    | X_GM_THRID of int64
    | X_GM_LABELS of string list
end

type 'a t =
  | FLAGS : Flag.t list t
  | ENVELOPE : Envelope.t t
  | INTERNALDATE : string t
  | UID : uid t
  | X_GM_MSGID : int64 t
  | X_GM_THRID : int64 t
  | X_GM_LABELS : string list t
  | RFC822 : string t
  | RFC822_TEXT : string t
  | RFC822_HEADER : string t
  | RFC822_SIZE : int t
  | BODY : MIME.Response.t t
  | BODYSTRUCTURE : MIME.Response.t t
  | MODSEQ : int64 t
  | PAIR : 'a t * 'b t -> ('a * 'b) t
  | MAP : ('a -> 'b) * 'a t -> 'b t

let flags = FLAGS

let map f x = MAP (f, x)

let pair x y = PAIR (x, y)

let rec encode : type a. a t -> Encoder.t = function
  | ENVELOPE -> Encoder.raw "ENVELOPE"
  | INTERNALDATE -> Encoder.raw "INTERNALDATE"
  | RFC822_HEADER -> Encoder.raw "RFC822.HEADER"
  | RFC822_SIZE -> Encoder.raw "RFC822.SIZE"
  | RFC822_TEXT -> Encoder.raw "RFC822.TEXT"
  | MODSEQ -> Encoder.raw "MODSEQ"
  | RFC822 -> Encoder.raw "RFC822"
  | BODY -> Encoder.raw "BODY"
  | BODYSTRUCTURE -> Encoder.raw "BODYSTRUCTURE"
  | UID -> Encoder.raw "UID"
  | FLAGS -> Encoder.raw "FLAGS"
  | X_GM_MSGID -> Encoder.raw "X-GM-MSGID"
  | X_GM_THRID -> Encoder.raw "X-GM-THRID"
  | X_GM_LABELS -> Encoder.raw "X-GM-LABELS"
  | PAIR _ as x ->
      let rec go : type a. _ -> a t -> _ =
       fun acc x ->
        match x with PAIR (x, y) -> go (go acc x) y | x -> encode x :: acc
      in
      Encoder.plist (fun x -> x) (List.rev (go [] x))
  | MAP (_, x) -> encode x

(* let body_section ?(peek = true) ?section:(sec = [], None) () =
   raw (if peek then "BODY.PEEK" else "BODY") & raw "[" & MIME.Request.encode sec & raw "]" *)
