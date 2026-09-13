open Core
open Expect_test_helpers_core
open Chatml
module Lang = Chatml_lang
module Codec = Chatml_value_codec
module Builtin_modules = Chatml_builtin_modules

let show_value = Builtin_modules.value_to_string

let%expect_test
    "JSON numbers use valid round-trippable syntax and reject non-finite values"
  =
  let number f = Lang.VVariant ("Number", [ Lang.VFloat f ]) in
  List.iter
    [ 0.; -0.; 1.; -2.; 1.25; 1e-300; Float.max_finite_value; 1.0000000000000002 ]
    ~f:(fun value ->
      let json = Codec.value_to_jsonaf_result (number value) |> Result.ok_or_failwith in
      let parsed = Jsonaf.to_string json |> Jsonaf.of_string |> Jsonaf.float_exn in
      [%test_eq: float] value parsed);
  print_endline "finite values survive strict JSON serialization/parsing";
  let pair = Lang.VVariant ("Array", [ Lang.VArray [| number 1.; number (-2.) |] ]) in
  print_endline
    (Codec.value_to_jsonaf_result pair |> Result.ok_or_failwith |> Jsonaf.to_string);
  List.iter [ Float.nan; Float.infinity; Float.neg_infinity ] ~f:(fun value ->
    match Codec.value_to_jsonaf_result (number value) with
    | Error message -> print_endline message
    | Ok _ -> failwith "non-finite value escaped through the JSON codec");
  [%expect
    {|
    finite values survive strict JSON serialization/parsing
    [1,-2]
    JSON numbers must be finite
    JSON numbers must be finite
    JSON numbers must be finite
    |}]
;;

let print_snapshot_round_trip (value : Lang.value) =
  match Codec.Snapshot.of_value value with
  | Error msg -> print_endline ("ERR: " ^ msg)
  | Ok snapshot ->
    print_s [%sexp (snapshot : Codec.Snapshot.t)];
    (match Codec.Snapshot.to_value snapshot with
     | Error msg -> print_endline ("DECODE ERR: " ^ msg)
     | Ok value -> print_endline (show_value value))
;;

let dummy_position = { Source.line = 1; column = 0; offset = 0 }
let dummy_span = { Source.left = dummy_position; right = dummy_position }
let node value : _ Lang.node = { value; span = dummy_span }

let dummy_closure () =
  Lang.VClosure
    { params = []
    ; body = node Lang.REUnit
    ; env = Lang.create_env ()
    ; frames = []
    ; param_slots = []
    }
;;

let print_snapshot_error (value : Lang.value) =
  match Codec.Snapshot.of_value value with
  | Ok snapshot -> print_s [%sexp (snapshot : Codec.Snapshot.t)]
  | Error msg -> print_endline msg
;;

let%expect_test "snapshot codec round-trips supported data values" =
  let nested_record =
    Lang.VRecord
      (Map.of_alist_exn
         (module String)
         [ "flags", Lang.VArray [| Lang.VBool true; Lang.VBool false |]
         ; "message", Lang.VVariant ("Ok", [ Lang.VString "ready"; Lang.VUnit ])
         ; ( "metrics"
           , Lang.VRecord
               (Map.of_alist_exn
                  (module String)
                  [ "count", Lang.VInt 3; "score", Lang.VFloat 1.5 ]) )
         ])
  in
  print_snapshot_round_trip nested_record;
  [%expect
    {|
    (Record (
      (flags (
        Array (
          (Bool true)
          (Bool false))))
      (message (Variant Ok ((String ready) Unit)))
      (metrics (
        Record (
          (count (Int   3))
          (score (Float 1.5)))))))
    { flags = [|true, false|]; message = `Ok(ready, ()); metrics = { count = 3; score = 1.5 } }
    |}]
;;

let%expect_test "snapshot codec rejects runtime-only values with descriptive errors" =
  print_snapshot_error (Lang.VArray [| Lang.VInt 1; Lang.VRef (ref Lang.VUnit) |]);
  print_snapshot_error (dummy_closure ());
  print_snapshot_error (Lang.VModule (Lang.create_env ()));
  print_snapshot_error (Lang.VBuiltin (fun _ -> Lang.VUnit));
  print_snapshot_error (Lang.VTask (Lang.TPure Lang.VUnit));
  [%expect
    {|
    root[1]: refs are not serializable in ChatML snapshots
    root: closures are not serializable in ChatML snapshots
    root: modules are not serializable in ChatML snapshots
    root: builtins are not serializable in ChatML snapshots
    root: tasks are not serializable in ChatML snapshots
    |}]
;;

let%expect_test "snapshot codec rejects duplicate record fields when decoding" =
  let snapshot =
    Codec.Snapshot.Record
      [ "name", Codec.Snapshot.String "first"; "name", Codec.Snapshot.String "second" ]
  in
  (match Codec.Snapshot.to_value snapshot with
   | Ok value -> print_endline (show_value value)
   | Error msg -> print_endline msg);
  [%expect {| root: duplicate record field "name" in ChatML snapshot |}]
;;

let%expect_test "snapshot JSON encoding preserves arbitrary ChatML variants" =
  let snapshot =
    Codec.Snapshot.Variant
      ( "Tick"
      , [ Record [ "count", Int 3; "ready", Bool true ]
        ; Array [ String "payload"; Unit; Float 1.5 ]
        ] )
  in
  let encoded = Codec.Snapshot.to_jsonaf snapshot in
  let decoded = Codec.Snapshot.of_jsonaf encoded in
  print_s
    [%sexp
      { encoded_is_object =
          ((match encoded with
            | `Object _ -> true
            | `Null | `True | `False | `String _ | `Number _ | `Array _ -> false)
           : bool)
      ; round_trip = (decoded : (Codec.Snapshot.t, string) result)
      }];
  [%expect
    {|
    ((encoded_is_object true)
     (round_trip (
       Ok (
         Variant Tick (
           (Record (
             (count (Int  3))
             (ready (Bool true))))
           (Array ((String payload) Unit (Float 1.5))))))))
    |}]
;;
