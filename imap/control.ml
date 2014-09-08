open ImapTypes
  
let flush st buf _ i =
  let str = Buffer.contents buf in
  Buffer.clear buf;
  ControlFlush (str, ControlOk ((), st, i))

let bind m f st buf b i =
  let rec loop =
    function
      ControlOk (x, st, i) -> f x st buf b i
    | ControlFail _ as x -> x
    | ControlNeed (len, k) -> ControlNeed (len, fun inp -> loop (k inp))
    | ControlFlush (buf, r) -> ControlFlush (buf, loop r)
  in
  loop (m st buf b i)

let fail err _ _ _ _ =
  ControlFail err
    
let liftP p st _ b i =
  let rec loop =
    function
      Ok (x, i) -> ControlOk (x, st, i)
    | Fail _ -> ControlFail ParseError
    | Need (len, k) -> ControlNeed (len, fun inp -> loop (k inp))
  in
  loop (p b i)
      
let send s st buf _ i =
  Buffer.add_string buf s;
  ControlOk ((), st, i)

let ret x st _ _ i =
  ControlOk (x, st, i)

let gets f st _ _ i =
  ControlOk (f st, st, i)

let modify f st _ _ i =
  ControlOk ((), f st, i)

let get st _ _ i =
  ControlOk (st, st, i)

let put st _ _ _ i =
  ControlOk ((), st, i)

let (>>=) = bind
let (>>) m1 m2 = m1 >>= fun _ -> m2
