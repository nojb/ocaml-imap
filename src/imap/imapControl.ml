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

open ImapTypes
open ImapTypesPrivate
  
type ('a, 'state, 'err) result =
  | Ok of 'a * 'state
  | Fail of 'err * 'state
  | Need of (input -> ('a, 'state, 'err) result)
  | Flush of string * (unit -> ('a, 'state, 'err) result)

type ('a, 'state, 'err) control =
  'state -> ('a, 'state, 'err) result

let flush st =
  let str = String.concat "" (List.rev st.out_buf) in
  let st = {st with out_buf = []} in
  Flush (str, fun () -> Ok ((), st))

let bind m f st =
  let rec loop = function
    | Ok (x, st) -> f x st
    | Fail _ as x -> x
    | Need k -> Need (fun inp -> loop (k inp))
    | Flush (str, k) -> Flush (str, fun () -> loop (k ()))
  in
  loop (m st)

let fail err st =
  Fail (err, st)
    
let liftP p st =
  let rec loop = function
    | ImapTypesPrivate.Ok (x, i) ->
        let rest = Buffer.sub st.in_buf i (Buffer.length st.in_buf - i) in
        Buffer.clear st.in_buf;
        Buffer.add_string st.in_buf rest;
        let st = {st with in_pos = 0} in
        Ok (x, st)
    | ImapTypesPrivate.Fail _ ->
        Fail (ParseError, st)
    | ImapTypesPrivate.Need k ->
        Need (fun inp -> loop (k inp))
  in
  loop (p st.in_buf st.in_pos)
      
let send s st =
  Ok ((), {st with out_buf = s :: st.out_buf})

let ret x st =
  Ok (x, st)

let gets f st =
  Ok (f st, st)

let modify f st =
  Ok ((), f st)

let get st =
  Ok (st, st)

let put st _ =
  Ok ((), st)

let catch f g st =
  let rec loop = function
    | Ok _ as ok -> ok
    | Fail (err, st) -> g err st
    | Need k -> Need (fun inp -> loop (k inp))
    | Flush (str, k) -> Flush (str, fun () -> loop (k ()))
  in
  loop (f st)

(* let try_bind m f g st = *)
(*   let rec loop = function *)
(*     | Ok (x, st) -> f x st *)
(*     | Fail err -> g err st *)
(*     | Need k -> Need (fun inp -> loop (k inp)) *)
(*     | Flush k -> Flush (fun () -> loop (k ())) *)
(*   in *)
(*   loop (m st) *)

let (>>=) = bind

let (>>) m1 m2 = m1 >>= fun _ -> m2

(* let lift f g m st = *)
(*   let rec loop = function *)
(*     | Ok (x, state) -> Ok (x, g st state) *)
(*     | Fail err -> Fail err *)
(*     | Need k -> Need (fun inp -> loop (k inp)) *)
(*     | Flush k -> Flush (fun () -> loop (k ())) *)
(*   in *)
(*   loop (m (f st)) *)

let run m st =
  assert (List.length st.out_buf = 0);
  m st

(* module type CONTROL = sig *)
(*   type error *)
(*   type 'a t *)
(*   val ret : 'a -> 'a t *)
(*   val bind : 'a t -> ('a -> 'b t) -> 'b t *)
(*   val fail : error -> _ t *)

(*   type io *)

(*   val input : io -> string -> int -> int -> int t *)
(*   val output : io -> string -> int -> int -> int t *)
(* end *)

(* module MakeRun (IO : CONTROL) = struct *)
(*   let really_output io buf pos len = *)
(*     let rec loop pos len = *)
(*       if len <= 0 then IO.ret () *)
(*       else IO.bind (IO.output io buf pos len) (fun n -> loop (pos + n) (len - n)) *)
(*     in *)
(*     loop pos len *)
(*   let run c io st = *)
(*     let buf = Bytes.create 65536 in *)
(*     let rec loop = function *)
(*       | Ok (x, st) -> *)
(*           IO.ret (x, st) *)
(*       | Fail err -> *)
(*           IO.fail err *)
(*       | Flush k -> *)
(*           let str = Buffer.contents st.out_buf in *)
(*           Buffer.clear st.out_buf; *)
(*           prerr_endline ">>>>"; *)
(*           prerr_string str; *)
(*           prerr_endline ">>>>"; *)
(*           IO.bind (really_output io str 0 (String.length str)) (fun () -> loop (k ())) *)
(*       | Need k -> *)
(*           IO.bind *)
(*             (IO.input io buf 0 (String.length buf)) *)
(*             (function *)
(*               | 0 -> *)
(*                   loop (k End) *)
(*               | _ as n -> *)
(*                   prerr_endline "<<<<"; *)
(*                   prerr_string (String.sub buf 0 n); *)
(*                   prerr_endline "<<<<"; *)
(*                   Buffer.add_subbytes st.in_buf buf 0 n; *)
(*                   loop (k More)) *)
(*     in *)
(*     loop (run c st) *)
(* end *)
