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
open Response

type 'a t =
  | FLAGS : Flag.t list t
  | ENVELOPE : envelope t
  | INTERNALDATE : string t
  | UID : uid t
  | X_GM_MSGID : int64 t
  | X_GM_THRID : int64 t
  | X_GM_LABELS : string list t
  | RFC822 : string t
  | RFC822_TEXT : string t
  | RFC822_HEADER : string t
  | RFC822_SIZE : int t
  | BODY : mime t
  | BODYSTRUCTURE : mime t
  | MODSEQ : int64 t
  | PAIR : 'a t * 'b t -> ('a * 'b) t
  | MAP : ('a -> 'b) * 'a t -> 'b t

let flags = FLAGS

let envelope = ENVELOPE

let internaldate = INTERNALDATE

let uid = UID

let x_gm_msgid = X_GM_MSGID

let x_gm_thrid = X_GM_THRID

let x_gm_labels = X_GM_LABELS

let rfc822 = RFC822

let rfc822_text = RFC822_TEXT

let rfc822_header = RFC822_HEADER

let rfc822_size = RFC822_SIZE

let body = BODY

let bodystructure = BODYSTRUCTURE

let modseq = MODSEQ

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

type u = {
  flags : Flag.t list option;
  uid : uid option;
  envelope : envelope option;
  internaldate : string option;
  rfc822 : string option;
  rfc822_header : string option;
  rfc822_text : string option;
  rfc822_size : int option;
  body : mime option;
  bodystructure : mime option;
  modseq : int64 option;
  x_gm_msgid : int64 option;
  x_gm_thrid : int64 option;
  x_gm_labels : string list option;
}

let empty =
  {
    flags = None;
    uid = None;
    envelope = None;
    internaldate = None;
    rfc822 = None;
    rfc822_header = None;
    rfc822_text = None;
    rfc822_size = None;
    body = None;
    bodystructure = None;
    modseq = None;
    x_gm_msgid = None;
    x_gm_thrid = None;
    x_gm_labels = None;
  }

let matches t a =
  let aux u = function
    | (FLAGS l : message_attribute) -> { u with flags = Some l }
    | UID n -> { u with uid = Some n }
    | ENVELOPE e -> { u with envelope = Some e }
    | INTERNALDATE x -> { u with internaldate = Some x }
    | RFC822 x -> { u with rfc822 = Some x }
    | RFC822_HEADER x -> { u with rfc822_header = Some x }
    | RFC822_TEXT x -> { u with rfc822_text = Some x }
    | RFC822_SIZE x -> { u with rfc822_size = Some x }
    | BODY x -> { u with body = Some x }
    | BODYSTRUCTURE x -> { u with bodystructure = Some x }
    | MODSEQ x -> { u with modseq = Some x }
    | X_GM_MSGID x -> { u with x_gm_msgid = Some x }
    | X_GM_THRID x -> { u with x_gm_thrid = Some x }
    | X_GM_LABELS l -> { u with x_gm_labels = Some l }
    | BODY_SECTION _ -> u
    (* TODO *)
  in
  let u = List.fold_left aux empty a in
  let rec go : type a. a t -> a option =
   fun t ->
    match t with
    | FLAGS -> u.flags
    | UID -> u.uid
    | ENVELOPE -> u.envelope
    | INTERNALDATE -> u.internaldate
    | RFC822 -> u.rfc822
    | RFC822_HEADER -> u.rfc822_header
    | RFC822_TEXT -> u.rfc822_text
    | RFC822_SIZE -> u.rfc822_size
    | BODY -> u.body
    | BODYSTRUCTURE -> u.bodystructure
    | MODSEQ -> u.modseq
    | X_GM_MSGID -> u.x_gm_msgid
    | X_GM_THRID -> u.x_gm_thrid
    | X_GM_LABELS -> u.x_gm_labels
    | MAP (f, t) -> ( match go t with Some x -> Some (f x) | None -> None )
    | PAIR (t1, t2) -> (
        match (go t1, go t2) with
        | Some x1, Some x2 -> Some (x1, x2)
        | _ -> None )
  in
  go t
