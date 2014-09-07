open ImapTypes
  
let flush st buf _ i =
  ControlFlush (List.rev buf, ControlOk ((), st, [], i))

let bind m f st buf b i =
  let rec loop =
    function
      ControlOk (x, st, buf, i) -> f x st buf b i
    | ControlFail _ as x -> x
    | ControlNeed (len, k) -> ControlNeed (len, fun inp -> loop (k inp))
    | ControlFlush (buf, r) -> ControlFlush (buf, loop r)
  in
  loop (m st buf b i)

let fail err _ _ _ _ =
  ControlFail err
    
let liftP p st buf b i =
  let rec loop : 'a parse_result -> 'a result =
    function
      Ok (x, i) -> ControlOk (x, st, buf, i)
    | Fail i -> ControlFail ParseError
    | Need (len, k) -> ControlNeed (len, fun inp -> loop (k inp))
  in
  loop (p b i)
      
let send s st buf _ i =
  ControlOk ((), st, s :: buf, i)

let ret x st buf _ i =
  ControlOk (x, st, buf, i)

let gets f st buf _ i =
  ControlOk (f st, st, buf, i)

let modify f st buf _ i =
  ControlOk ((), f st, buf, i)

let get st buf _ i =
  ControlOk (st, st, buf, i)

let put st _ buf _ i =
  ControlOk ((), st, buf, i)

let (>>=) = bind
let (>>) m1 m2 = m1 >>= fun _ -> m2
