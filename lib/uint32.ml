type t = int32

let msb n = Int32.(logand n (shift_left 1l 31)) <> 0l

let compare n1 n2 =
  match msb n1, msb n2 with
  | true, true -> Int32.(compare (logand n1 0x7fffffffl) (logand n2 0x7fffffffl))
  | true, false -> 1
  | false, true -> -1
  | false, false -> Int32.compare n1 n2

let succ n = Int32.succ n

let (<) n m = compare n m < 0

let (<=) n m = compare n m <= 0

let min n m = if n <= m then n else m

let max n m = if n <= m then m else n

module Set = struct
  type t =
    (int32 * int32) list (* disjoint, sorted intervals *)

  let empty = []

  let singleton n = [(n, n)]

  let rec union s1 s2 =
    let rec loop s1 s2 =
      match s1, s2 with
      | [], s2 -> s2
      | s1, [] -> s1
      | (a, b) :: x1, (c, d) :: x2 ->
          if succ b < c then (a, b) :: loop x1 s2
          else if succ d < a then (c, d) :: loop s1 x2
          else union [min a c, max b d] (union x1 x2)
    in
    loop s1 s2

  let add n s =
    union (singleton n) s

  let _interval n m =
    if n <= m then [n, m] else [m, n]

  let of_list l =
    List.fold_left (fun s n -> add n s) empty l
end
