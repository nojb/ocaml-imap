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

open Response

module Request = struct
  open Encoder

  let section_msgtext = function
    | HEADER -> raw "HEADER"
    | HEADER_FIELDS l -> raw "HEADER.FIELDS" ++ plist str l
    | HEADER_FIELDS_NOT l -> raw "HEADER.FIELDS.NOT" ++ plist str l
    | TEXT -> raw "TEXT"
    | MIME -> raw "MIME"

  let encode (nl, sec) =
    let sec =
      match sec with None -> empty | Some sec -> section_msgtext sec
    in
    match nl with [] -> sec | _ :: _ -> list ~sep:'.' int nl & raw "." & sec

  let header ?(part = []) () = (part, Some HEADER)

  let header_fields ?(part = []) l = (part, Some (HEADER_FIELDS l))

  let header_fields_not ?(part = []) l = (part, Some (HEADER_FIELDS_NOT l))

  let text ?(part = []) () = (part, Some TEXT)

  let part ~part () = (part, None)

  let mime ~part () = (part, Some MIME)
end
