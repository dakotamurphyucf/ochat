open! Core
open Fixtures
module A = Agent_session
module D = Document_schema
module E = A.Session_decode_evidence
module S = A.Session_state_document

let%expect_test "decoder evidence reuses exact entries at their original positions" =
  let calls = ref 0 in
  let decode json =
    incr calls;
    Agent_protocol.Json_codec.string json
  in
  let array values = `Array (List.map values ~f:(fun value -> `String value)) in
  let original = E.Sequence.admit ~decode (array [ "a"; "b"; "c" ]) |> protocol_ok in
  let run previous json =
    calls := 0;
    let next = E.Sequence.readmit previous ~json in
    print_s
      [%sexp
        { calls = (!calls : int)
        ; result =
            (Result.map next ~f:E.Sequence.values
             |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
             : (string list, string) Result.t)
        }];
    next
  in
  let same = run original (array [ "a"; "b"; "c" ]) |> protocol_ok in
  let changed = run same (array [ "a"; "changed"; "c"; "added" ]) |> protocol_ok in
  let reordered = run changed (array [ "changed"; "a"; "c"; "added" ]) |> protocol_ok in
  let shortened = run reordered (array [ "changed" ]) |> protocol_ok in
  ignore (run shortened (`Array [ `True; `Null ]) : (_, _) Result.t);
  [%expect
    {|
    ((calls 0) (result (Ok (a b c))))
    ((calls 2) (result (Ok (a changed c added))))
    ((calls 2) (result (Ok (changed a c added))))
    ((calls 0) (result (Ok (changed))))
    ((calls 2) (result (Error "expected a JSON string")))
    |}]
;;

let%expect_test "field evidence retains number spellings and object ordering" =
  let calls = ref 0 in
  let decode json =
    incr calls;
    Ok json
  in
  let first = `Object [ "a", `Number "1.00"; "b", `Null ] in
  let original = E.Field.admit ~decode first |> protocol_ok in
  calls := 0;
  E.Field.readmit original ~json:first |> protocol_ok |> ignore;
  E.Field.readmit original ~json:(`Object [ "a", `Number "1"; "b", `Null ])
  |> protocol_ok
  |> ignore;
  E.Field.readmit original ~json:(`Object [ "b", `Null; "a", `Number "1.00" ])
  |> protocol_ok
  |> ignore;
  print_s [%sexp (!calls : int)];
  [%expect {| 2 |}]
;;

let%expect_test "identity hints reuse moved exact records without authorizing collisions" =
  let calls = ref 0 in
  let decode json =
    incr calls;
    let open Result.Let_syntax in
    let%bind fields = Agent_protocol.Json_codec.fields json in
    let%bind id =
      Agent_protocol.Json_codec.required_as fields "id" Agent_protocol.Json_codec.string
    in
    let%map value =
      Agent_protocol.Json_codec.required_as
        fields
        "value"
        Agent_protocol.Json_codec.string
    in
    id ^ ":" ^ value
  in
  let record id value = `Object [ "id", `String id; "value", `String value ] in
  let a = record "a" "one" in
  let b = record "b" "two" in
  let c = record "c" "three" in
  let changed = record "b" "changed" in
  let collision = record "b" "collision" in
  let original =
    E.Sequence.admit ~identity_field:"id" ~decode (`Array [ a; b ]) |> protocol_ok
  in
  let run previous values =
    calls := 0;
    let json = `Array values in
    let cached = E.Sequence.readmit previous ~json in
    let observed_calls = !calls in
    let cold = Agent_protocol.Json_codec.list decode json in
    assert (
      Result.equal
        (List.equal String.equal)
        (fun first second ->
           Sexp.equal
             (Agent_protocol.Error.sexp_of_t first)
             (Agent_protocol.Error.sexp_of_t second))
        (Result.map cached ~f:E.Sequence.values)
        cold);
    print_s
      [%sexp { calls = (observed_calls : int); accepted = (Result.is_ok cached : bool) }];
    cached
  in
  let prepended = run original [ c; b; a ] |> protocol_ok in
  let modified = run prepended [ changed; c; a ] |> protocol_ok in
  let duplicated = run modified [ changed; collision; c; a ] |> protocol_ok in
  let reordered = run duplicated [ collision; changed; c; a ] |> protocol_ok in
  let shortened = run reordered [ c ] |> protocol_ok in
  let reintroduced = run shortened [ a; c ] |> protocol_ok in
  ignore
    (run
       reintroduced
       [ `Object [ "id", `String "c"; "value", `Null ]
       ; `Object [ "id", `True; "value", `String "malformed" ]
       ]
     : (_, _) Result.t);
  [%expect
    {|
    ((calls 1) (accepted true))
    ((calls 1) (accepted true))
    ((calls 1) (accepted true))
    ((calls 1) (accepted true))
    ((calls 0) (accepted true))
    ((calls 1) (accepted true))
    ((calls 2) (accepted false))
    |}]
;;

let replace json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal key name then value else old))
  | _ -> raise_s [%sexp "expected fixture object"]
;;

let field json name =
  match D.Json.field json ~name with
  | Value json -> json
  | Absent | Null -> raise_s [%sexp "missing fixture field", (name : string)]
;;

let%expect_test "cached session admission agrees with cold decoding and preservation" =
  with_actor_workspace (fun _env workspace_instance ->
    let _, state, _, _, _ = extension_fixture workspace_instance in
    let document = state_document state in
    let previous = S.decode ~limits:document_limits document |> document_ok in
    let modified_native =
      { state with
        invocations = []
      ; conversation = { state.conversation with canonical_history = [] }
      }
    in
    let owners = [ previous; S.authored state; S.with_value previous modified_native ] in
    let payload = D.Document.payload document in
    let conversation = field payload "conversation" in
    let invocation =
      match field payload "invocations" with
      | `Array (first :: _) -> first
      | _ -> raise_s [%sexp "expected fixture invocation"]
    in
    let updated_invocation = replace invocation "input" (`Number "1.00") in
    let add_unknown = function
      | `Object fields -> `Object (fields @ [ "future", `Number "1.00" ])
      | _ -> raise_s [%sexp "expected fixture object"]
    in
    let candidates =
      [ payload
      ; (match payload with
         | `Object fields -> `Object (List.rev fields)
         | _ -> payload)
      ; replace payload "invocations" (`Array [ updated_invocation ])
      ; replace payload "invocations" (`Array [ add_unknown invocation ])
      ; replace payload "invocations" (`Array [ invocation; invocation ])
      ; replace payload "invocations" (`Array [ `Null; `True ])
      ; replace payload "moderator_executions" (`Array [ `Null; `True ])
      ; replace payload "inference_ledger" `Null
      ; replace
          payload
          "conversation"
          (replace conversation "canonical_history" (`Array [ `Null; `True ]))
      ; replace (replace payload "invocations" (`Array [ `Null ])) "identity" `Null
      ; (match payload with
         | `Object fields ->
           `Object
             (List.filter fields ~f:(fun (name, _) -> not (String.equal name "run_state")))
         | _ -> payload)
      ]
    in
    let limits =
      [ document_limits
      ; D.Limits.create
          ~max_bytes:(D.Limits.max_bytes document_limits)
          ~max_depth:(D.Limits.max_depth document_limits)
          ~max_fields:(D.Limits.max_fields document_limits + 1)
          ~max_nodes:(D.Limits.max_nodes document_limits)
        |> document_ok
      ; D.Limits.create ~max_bytes:128 ~max_depth:8 ~max_fields:20 ~max_nodes:100
        |> document_ok
      ]
    in
    let cases = ref 0 in
    List.iter candidates ~f:(fun payload ->
      let document =
        replace (D.Document.json document) "payload" payload
        |> D.Document.inspect ~limits:document_limits
        |> document_ok
      in
      List.iter limits ~f:(fun limits ->
        let encode result =
          Result.bind result ~f:(fun state -> S.encode state ~limits)
          |> Result.map ~f:D.Document.to_string
        in
        let cold = S.decode ~limits document |> encode in
        List.iter owners ~f:(fun previous ->
          let cached = S.decode_after previous ~limits document |> encode in
          assert (Result.equal String.equal D.Error.equal cold cached);
          incr cases)));
    print_s [%sexp (!cases : int)]);
  [%expect {| 99 |}]
;;
