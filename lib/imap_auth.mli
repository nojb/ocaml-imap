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

(** IMAP Authenticators.

    Used with the AUTHENTICATE command to perform SASL authentication.  See
    {!Imap.authenticate}. *)

type t = {
  name : string;
  (** The name of the authentication method. *)
  step : string -> [`OK | `NEEDS_MORE] * string
  (** The authentication function.  In order to perform SASL authentication,
      this function is called with each of the challenges from the server, in
      turn.  The challenges are not encoded.  For each challenge, it returns a
      pair [(rc, s)].  [s] is the string that needs to be sent back to the
      server.  [rc] is [`OK] if the mechanism does not need to be called again
      with more data from the server, and [`NEEDS_MORE] if it does.  This does
      not mean that the server will not send more data (for example, in case of
      authentication error), just that the mechanism does not need more data.
      Conversely, if the server behaves as if authentication is done, but this
      function has returned [`NEEDS_MORE], then something is amiss. *)
}

val gsasl : string -> (Gsasl.property * string) list -> t
(** [gsasl name props] performs authentication using the GNU SASL library.
    [name] is the gsasl method name, and [props] is a list of properties and
    values used to authenticate with that method.  See the
    {{:http://www.gnu.org/software/gsasl/manual/gsasl.html}manual} for more
    information. *)

val plain : string -> string -> t
(** [plain user pass] authenticates with the PLAIN SASL mechanism with username
    [user] and password [pass]. *)

val xoauth2 : string -> string -> t
(** [xoauth2 user token] authenticates using the XOAUTH2 mehchanism with
    username [user] and access token [token]. *)
