open Core
open Expect_test_helpers_core
open Chatml

module Lang = Chatml_lang
module Codec = Chatml_value_codec
module Builtin_modules = Chatml_builtin_modules

let show_value = Builtin_modules.value_to_string

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
      [ "name", Codec.Snapshot.String "first"
      ; "name", Codec.Snapshot.String "second"
      ]
  in
  (match Codec.Snapshot.to_value snapshot with
   | Ok value -> print_endline (show_value value)
   | Error msg -> print_endline msg);
  [%expect {| root: duplicate record field "name" in ChatML snapshot |}]
;;
