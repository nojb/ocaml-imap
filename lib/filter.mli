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

type mb

class type message =
  object
    method fetch_headers: (string * string) list Lwt.t
    method fetch_body: string Lwt.t
    method rep: mb
  end

class type message_set =
  object
    method count: int Lwt.t
    method get: int32 -> message
    method uids: int32 list Lwt.t

    method contain_from: string -> message_set
    method is_unseen: message_set

    method copy: mailbox -> unit Lwt.t

    method rep: mb
  end

and mailbox =
  object
    inherit message_set
    method name: string
  end

class account: host:string -> ?port:int -> username:string -> password:string -> unit ->
  object
    method inbox: mailbox
    method list_all: mailbox list Lwt.t
  end
