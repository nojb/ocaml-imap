open Sexplib.Std
open OUnit2

(* module I = Imap_set.Make *)
(*     (struct *)
(*       type t = int with sexp *)
(*       let zero = 0 *)
(*       let pred n = n - 1 *)
(*       let succ n = n + 1 *)
(*       let compare n m = n - m *)
(*       let max_int = max_int *)
(*     end) *)

(* let test_singletons ctxt = *)
(*   let max = 10000 in *)
(*   let rec loop acc n = *)
(*     if n > max then acc *)
(*     else loop (I.add n acc) (n + 1) *)
(*   in *)
(*   let s = loop I.empty 1 in *)
(*   assert_equal ~ctxt (I.to_list (I.optimise s)) [(1, max)] *)

(* let test_ranges ctxt = *)
(*   let s = I.interval (2, 4) in *)
(*   let s = I.add_interval (5, 7) s in *)
(*   let s = I.add_interval (6, 9) s in *)
(*   let s = I.add_interval (11, 23) s in *)
(*   assert_equal ~ctxt (I.to_list (I.optimise s)) [(2, 9); (11, 23)]; *)
(*   let s = I.add 10 s in *)
(*   assert_equal ~ctxt (I.to_list (I.optimise s)) [(2, 23)] *)

(* let test_top ctxt = *)
(*   let s = I.interval (3, 43) in *)
(*   let s = I.(union (from 100) s) in *)
(*   assert_equal ~ctxt (I.to_list (I.optimise s)) [(3, 43); (100, 0)]; *)
(*   let s = I.(union (from 5) s) in *)
(*   assert_equal ~ctxt (I.to_list (I.optimise s)) [(3, 0)] *)

let suite =
  "Index_set" >:::
  [
    (* "test_singletons" >:: test_singletons; *)
    (* "test_ranges" >:: test_ranges; *)
    (* "test_top" >:: test_top *)
  ]

let () =
  run_test_tt_main suite
