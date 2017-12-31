(* The MIT License (MIT)
  
   Copyright (c) 2015-2017 Nicolas Ojeda Bar <n.oje.bar@gmail.com>
   Copyright (c) 2017 Anil Madhavapeddy <anil@recoil.org>

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


open Cmdliner

type args = {
  server: string;
  port: int option;
  username: string;
  password: string;
  tls: bool;
  mailbox: string;
}

let opts server port username password tls mailbox =
  { server; port; username; password; tls; mailbox}

let server =
  let doc = Arg.info ~docv:"SERVER" ~doc:"Server hostname" [] in
  Arg.(required & pos 0 (some string) None & doc)

let port =
  let doc = Arg.info ~docv:"PORT" ~doc:"Server port" ["port"; "p"] in
  Arg.(value & opt (some int) None & doc)

let username =
  let doc = Arg.info ~docv:"USERNAME" ~doc:"Username" [] in
  Arg.(required & pos 1 (some string) None & doc)

let password =
  let doc = Arg.info ~docv:"PASSWORD" ~doc:"Password" [] in
  Arg.(required & pos 2 (some string) None & doc)

let tls =
  let doc = "Connect via TLS" in
  Arg.(value & flag & info ["tls"] ~doc)

let mailbox =
  let doc = Arg.info ~docv:"MAILBOX" ~doc:"Mailbox to connect to" [] in
  Arg.(required & pos 3 (some string) None & doc)

let client =
  Term.(const opts $ server $ port $ username $ password $ tls $ mailbox)

let connect ?read_only {server; port; username; password; tls; mailbox} =
  Imap.connect server ~tls ?port username password ?read_only mailbox
