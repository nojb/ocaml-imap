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

module MailboxAttribute = struct
  type t =
    | MESSAGES of int
    | RECENT of int
    | UIDNEXT of int32
    | UIDVALIDITY of int32
    | UNSEEN of int
    | HIGHESTMODSEQ of int64
end

type 'a t =
  | MESSAGES : int t
  | RECENT : int t
  | UIDNEXT : int32 t
  | UIDVALIDITY : int32 t
  | UNSEEN : int t
  | HIGHESTMODSEQ : int64 t
  | PAIR : 'a t * 'b t -> ('a * 'b) t
  | MAP : ('a -> 'b) * 'a t -> 'b t

module E = Encoder

let rec encode : type a. a t -> E.t = function
  | MESSAGES -> E.raw "MESSAGES"
  | RECENT -> E.raw "RECENT"
  | UIDNEXT -> E.raw "UIDNEXT"
  | UIDVALIDITY -> E.raw "UIDVALIDITY"
  | UNSEEN -> E.raw "UNSEEN"
  | HIGHESTMODSEQ -> E.raw "HIGHESTMODSEQ"
  | MAP (_, x) -> encode x
  | PAIR _ as x ->
      let rec go : type a. _ -> a t -> _ =
       fun acc x ->
        match x with PAIR (x, y) -> go (go acc x) y | x -> encode x :: acc
      in
      E.list (fun x -> x) (List.rev (go [] x))
