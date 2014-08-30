(* Check if [b] ends in "{[0-9]+}\r\n".
   Assume that it ends in "\r\n". *)
let scan_buffer_for_literal b =
  let len = Buffer.length b in
  let nth = Buffer.nth b in
  if len < 5 then
    None
  else if nth (len-3) = '}' then
    let rec loop acc exp i =
      if i < 0 then
        None
      else
        match nth i with
        | '0' .. '9' as c ->
            let c = Char.code c - Char.code '0' in
            loop (acc + c * exp) (exp * 10) (i - 1)
        | '{' ->
            Some acc
        | _ ->
            None
    in
    loop 0 1 (len - 4)
  else
    None

type step_state = {
  buf : Buffer.t;
  mutable lit : string list;
  mutable cur : [ `AwaitLit of int * Buffer.t | `AwaitCr | `AwaitLf | `CheckLit ]
}

(** [`Await] means that all the input [data] has been consumed. *)
let rec step data i st : [ `Await | `Ok of string * string list * int ] =
  match st.cur with
  | `AwaitCr ->
      prerr_endline "AwaitCr";
      let rec loop i =
        try
          let r = String.index_from data i '\r' in
          if r < String.length data - 1 then
            if data.[r+1] = '\n' then
              begin
                Buffer.add_substring st.buf data i (r+2-i);
                st.cur <- `CheckLit;
                step data (r+2) st
              end
            else
              loop (r+1)
          else
            begin
              Buffer.add_substring st.buf data i (r+1-i);
              st.cur <- `AwaitLf;
              `Await
            end
        with
        | Not_found ->
            Buffer.add_substring st.buf data i (String.length data - i);
            `Await
      in
      loop i
  | `AwaitLf ->
      prerr_endline "AwaitLf";
      if i >= String.length data then
        `Await
      else if data.[i] = '\n' then
        begin
          Buffer.add_char st.buf '\n';
          st.cur <- `CheckLit;
          step data (i+1) st
        end
      else
        begin
          st.cur <- `AwaitCr;
          step data i st
        end
  | `CheckLit ->
      begin
        prerr_endline "CheckLit";
        match scan_buffer_for_literal st.buf with
        | Some len ->
            st.cur <- `AwaitLit (len, Buffer.create 0);
            step data i st
        | None -> (* bingo ! *)
            let str = Buffer.contents st.buf in
            let lit = List.rev st.lit in
            Buffer.clear st.buf;
            st.lit <- [];
            `Ok (str, lit, i)
      end
  | `AwaitLit (0, chunks) ->
      prerr_endline "AwaitLit 0";
      st.lit <- Buffer.contents chunks :: st.lit;
      st.cur <- `AwaitCr;
      step data i st
  | `AwaitLit (len, chunks) ->
      prerr_endline "AwaitLit";
      let len1 = min len (String.length data - i) in
      if len1 = 0 then
        `Await
      else
        begin
          Buffer.add_substring chunks data i len1;
          st.cur <- `AwaitLit (len - len1, chunks);
          step data (i+len1) st
        end

type reader = {
  state : step_state;
  mutable current : string * int;
  mutable pending : [ `Data of string | `End ] Queue.t
}

let create () = {
  state = { buf = Buffer.create 0; lit = []; cur = `AwaitCr };
  current = ("", 0);
  pending = Queue.create ()
}

let read r =
  let rec loop = function
    | `Ok (str, lit, i) ->
        let d, _ = r.current in
        r.current <- (d, i);
        `Ok (str, lit)
    | `Await ->
        if Queue.is_empty r.pending then
          `Await
        else
          begin
            match Queue.pop r.pending with
            | `Data d ->
                r.current <- (d, 0);
                loop (step d 0 r.state)
            | `End ->
                `End
          end
  in
  let d, i = r.current in
  loop (step d i r.state)

let src r x =
  Queue.push x r.pending
