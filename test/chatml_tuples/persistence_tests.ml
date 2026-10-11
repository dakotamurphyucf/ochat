open! Core
module L = Chatml.Chatml_lang
open L
module Codec = Chatml.Chatml_value_codec
module S = Codec.Snapshot
module D = Document_schema

let tuple_wire =
  {|{"kind":"tuple","values":[{"kind":"int","value":7},{"kind":"tuple","values":[{"kind":"string","value":"x"},{"kind":"bool","value":true}]}]}|}
;;

let expected = S.Tuple (Int 7, Tuple (String "x", Bool true, []), [])
let decoded () = S.of_jsonaf (Jsonaf.of_string tuple_wire) |> Result.ok_or_failwith

let%expect_test "tuple persistence has its own kind and preserves nested product shape" =
  [%test_eq: int] (S.compare (decoded ()) expected) 0;
  [%test_eq: string] (S.to_jsonaf expected |> Jsonaf.to_string) tuple_wire;
  let value = S.to_value expected |> Result.ok_or_failwith in
  assert (L.equal_value value (Support.run {|let result = (|7, (|"x", true|)|)|}));
  [%test_eq: int] (S.compare (S.of_value value |> Result.ok_or_failwith) expected) 0;
  print_endline tuple_wire;
  let two =
    S.of_jsonaf
      (Jsonaf.of_string
         {|{"kind":"variant","tag":"Pair","payload":[{"kind":"int","value":7},{"kind":"string","value":"x"}]}|})
    |> Result.ok_or_failwith
  in
  let one =
    S.of_jsonaf
      (Jsonaf.of_string
         {|{"kind":"variant","tag":"Pair","payload":[{"kind":"tuple","values":[{"kind":"int","value":7},{"kind":"string","value":"x"}]}]}|})
    |> Result.ok_or_failwith
  in
  [%test_eq: int] (S.compare two (Variant ("Pair", [ Int 7; String "x" ]))) 0;
  [%test_eq: int] (S.compare one (Variant ("Pair", [ Tuple (Int 7, String "x", []) ]))) 0;
  assert (not (Int.equal (S.compare two one) 0));
  [%expect
    {| {"kind":"tuple","values":[{"kind":"int","value":7},{"kind":"tuple","values":[{"kind":"string","value":"x"},{"kind":"bool","value":true}]}]} |}]
;;

let%test_unit
    "handwritten malformed tuple snapshots reject before invalid child conversion"
  =
  List.iter
    [ {|{"kind":"tuple","values":[]}|}
    ; {|{"kind":"tuple","values":[{"kind":"unit"}]}|}
    ; {|{"kind":"tuple","values":null}|}
    ; {|{"kind":"tuple","values":{}}|}
    ; {|{"kind":"tuple"}|}
    ; {|{"kind":"tuple","values":[{"kind":"unit"},null]}|}
    ; {|{"kind":"tuple","values":[{"kind":"unit"},{"kind":"tuple","values":[]}]}|}
    ]
    ~f:(fun wire ->
      let json = Jsonaf.of_string wire in
      assert (Result.is_error (S.of_jsonaf json));
      assert (Result.is_error (Session.Snapshot.of_jsonaf json)));
  (match S.of_jsonaf (Jsonaf.of_string {|{"kind":"tuple","values":[null]}|}) with
   | Error error -> assert (String.is_substring error ~substring:"at least two")
   | Ok _ -> failwith "Singleton tuple admitted");
  let runtime_only =
    L.VTuple (Chatml.Chatml_tuple.create L.VUnit (VRef (ref (VInt 7))) ~rest:[])
  in
  (match S.of_value runtime_only with
   | Error error -> assert (String.is_substring error ~substring:"root[1]")
   | Ok _ -> failwith "Runtime-only tuple child serialized");
  assert (Result.is_error (Codec.value_to_jsonaf_result (S.to_value_exn expected)));
  List.iter
    [ S.Tuple (Unit, Float Float.nan, [])
    ; Tuple (Unit, Record [ "same", Int 1; "same", Int 2 ], [])
    ]
    ~f:(fun snapshot ->
      assert (Result.is_error (Session.Snapshot.of_jsonaf (S.to_jsonaf snapshot))))
;;

(* This edit helper addresses a known wire field, independently of Session's
   ownership schema. Production encoding supplies only the unrelated envelope. *)
type step =
  | Field of string
  | Index of int

let rec edit tree path ~f =
  match path, tree with
  | [], _ -> f tree
  | Field name :: path, `Object fields ->
    assert (List.count fields ~f:(fun (key, _) -> String.equal key name) = 1);
    `Object
      (List.map fields ~f:(fun (key, value) ->
         key, if String.equal key name then edit value path ~f else value))
  | Index index :: path, `Array values ->
    assert (index < List.length values);
    `Array
      (List.mapi values ~f:(fun i value ->
         if Int.equal i index then edit value path ~f else value))
  | _ -> failwith "Missing fixture wire path"
;;

let add name value = function
  | `Object fields ->
    assert (not (List.Assoc.mem fields name ~equal:String.equal));
    `Object (fields @ [ name, value ])
  | _ -> failwith "Expected fixture object"
;;

let schema_exn = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let json session = Session.Document.encode session |> schema_exn |> D.Document.json

let tuple_session () =
  let base = Session.create ~id:"tuple-session" ~prompt_file:"fixture.chatmd" () in
  { base with
    moderator_state = { base.moderator_state with extensions = [ "tuple", expected ] }
  }
;;

let tuple_location =
  [ Field "payload"; Field "moderator_state"; Field "extensions"; Index 0; Field "value" ]
;;

let with_unknowns tree =
  edit tree tuple_location ~f:(add "future_tuple" (`String "container"))
  |> fun tree ->
  edit
    tree
    (tuple_location @ [ Field "values"; Index 0 ])
    ~f:(add "future_leaf" (`String "leaf"))
;;

let restore tree =
  D.Document.inspect ~limits:D.Limits.default tree
  |> schema_exn
  |> Session.Document.decode
  |> schema_exn
;;

let require_snapshot session snapshot =
  [%test_eq: int] (List.length session.Session.moderator_state.extensions) 1;
  [%test_eq: int]
    (S.compare (List.hd_exn session.moderator_state.extensions |> snd) snapshot)
    0
;;

let change_tuple session =
  let changed = S.Tuple (Int 8, Tuple (String "x", Bool true, []), []) in
  ( { session with
      Session.moderator_state =
        { session.Session.moderator_state with extensions = [ "tuple", changed ] }
    }
  , changed )
;;

let%test_unit "unrelated Session edits preserve exact tuple and nested unknown fields" =
  let inserted = json (tuple_session ()) |> with_unknowns in
  let restored = restore inserted in
  require_snapshot restored expected;
  let updated = { restored with kv_store = [ "unrelated", "edit" ] } in
  let encoded = json updated in
  let wanted =
    Jsonaf.of_string tuple_wire
    |> add "future_tuple" (`String "container")
    |> fun tree ->
    edit tree [ Field "values"; Index 0 ] ~f:(add "future_leaf" (`String "leaf"))
  in
  ignore
    (edit encoded tuple_location ~f:(fun actual ->
       assert (D.Json.equal actual wanted);
       actual));
  let roundtrip =
    Session.Document.to_string updated
    |> schema_exn
    |> Session.Document.of_string
    |> schema_exn
  in
  require_snapshot roundtrip expected;
  assert (D.Json.equal encoded (json roundtrip))
;;

let%test_unit "Session tuple child edits without unknown child fields succeed" =
  let restored = json (tuple_session ()) |> restore in
  let updated, changed = change_tuple restored in
  let encoded = json updated in
  ignore
    (edit encoded tuple_location ~f:(fun actual ->
       assert (D.Json.equal actual (S.to_jsonaf changed));
       actual));
  require_snapshot (restore encoded) changed;
  require_snapshot restored expected
;;

let%test_unit "ambiguous tuple child edits reject and leave original Session reusable" =
  let inserted = json (tuple_session ()) |> with_unknowns in
  let restored = restore inserted in
  let updated, _ = change_tuple restored in
  (match Session.Document.encode updated with
   | Error (D.Error.Extension_conflict path) ->
     [%test_eq: string list]
       path
       [ "payload"; "moderator_state"; "extensions"; "tuple"; "value"; "values" ]
   | Error error -> raise_s [%sexp "Unexpected error category", (error : D.Error.t)]
   | Ok _ -> failwith "Ambiguous child edit silently reassigned unknown fields");
  require_snapshot restored expected;
  assert (D.Json.equal inserted (json restored));
  let reused = { restored with kv_store = [ "after-rejection", "edit" ] } in
  require_snapshot (json reused |> restore) expected;
  assert (D.Json.equal inserted (json restored))
;;
