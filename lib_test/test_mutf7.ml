open OUnit2
open Utils

let utf8_mutf7_pairs =
  [
    "Entwürfe", "Entw&APw-rfe";
    "François", "Fran&AOc-ois";
    "Nicolás", "Nicol&AOE-s";
    "heißt \"Hallo\"", "hei&AN8-t \"Hallo\"";
    "Hot & Spicy & Fruity", "Hot &- Spicy &- Fruity"
  ]

let test_utf8_to_mutf7 =
  List.map (fun (utf8, mutf7) ->
      test_case (fun ctxt ->
          assert_equal ~ctxt ~msg:mutf7 mutf7 (encode_mutf7 utf8)))
    utf8_mutf7_pairs

let test_mutf7_to_utf8 =
  List.map (fun (utf8, mutf7) ->
      test_case (fun ctxt ->
          assert_equal ~ctxt ~msg:mutf7 (decode_mutf7 mutf7) utf8))
    utf8_mutf7_pairs

let suite =
  "Mutf7" >:::
  [
    "test_utf8_to_mutf7" >::: test_utf8_to_mutf7;
    "test_mutf7_to_utf8" >::: test_mutf7_to_utf8
  ]

let () =
  run_test_tt_main suite
