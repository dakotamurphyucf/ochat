open! Core
open Fixtures
module P = Agent_protocol
module I = P.Invocation
module H = Agent_session.Invocation_history
module Codec = Agent_session.History_codec
module Payload = History_entry.Payload

let id sequence =
  History_entry.Id.create ~namespace:"retained-validation" ~sequence
  |> Result.ok_or_failwith
;;

let entry sequence provider_id view =
  Payload.Semantic.create
    view
    ~metadata:{ Payload.Metadata.empty with call_id = Value provider_id }
  |> Result.ok_or_failwith
  |> Payload.authored
  |> History_entry.create_with_id ~id:(id sequence)
  |> Codec.to_protocol
;;

let call ?(name = "read_file") sequence kind =
  entry
    sequence
    "same"
    (Call { kind; name; namespace = Absent; input_bytes = "null"; async = Absent })
;;

let result ?(relation = Payload.Call_relation.Bound (id 0)) sequence kind text =
  entry sequence "same" (Result { relation; kind; output = Text text })
;;

let invocation ?routing () =
  I.create
    ?routing
    { (invocation_fixture ()).context with
      origin = Model
    ; provider_call_id = Some "same"
    ; call_entry_id = Some (id 0)
    }
  |> protocol_ok
  |> I.dispatch
  |> protocol_ok
  |> fun invocation ->
  I.resolve invocation ~session_id ~generation:0 (Complete (`String "saved"))
  |> protocol_ok
;;

let outcome_text = Jsonaf.to_string (I.outcome_to_json (Complete (`String "saved")))

let assert_same_result left right =
  match left, right with
  | Ok (), Ok () -> ()
  | Error left, Error right ->
    assert (Jsonaf.exactly_equal (P.Error.to_json left) (P.Error.to_json right))
  | Ok (), Error error | Error error, Ok () ->
    raise_s [%sexp "validation paths disagree", (error : P.Error.t)]
;;

let validate history invocation =
  let snapshot = H.Validated_history.create history |> protocol_ok in
  let expected = H.validate_retained ~history invocation in
  let actual = H.Validated_history.validate_retained snapshot invocation in
  assert_same_result expected actual;
  actual
;;

let assert_rejected result =
  match result with
  | Error _ -> ()
  | Ok () -> failwith "invalid retained occurrence was accepted"
;;

let%expect_test "decoded snapshots preserve occurrence, receipt and provenance checks" =
  List.iter [ Payload.Call_kind.Function; Custom ] ~f:(fun kind ->
    let original = call 0 kind in
    let output = result 2 kind outcome_text in
    let resolved = invocation () in
    let published =
      I.publish_with_history resolved ~output_entry_id:(id 2) |> protocol_ok
    in
    validate [ original; output ] published |> protocol_ok;
    let unrelated =
      Codec.user_text ~id:(id 4) (String.make 65536 'x') |> Codec.to_protocol
    in
    validate [ original; unrelated; output ] published |> protocol_ok;
    (* Compaction can remove either retained occurrence independently. *)
    List.iter [ []; [ original ]; [ output ] ] ~f:(fun history ->
      validate history published |> protocol_ok);
    validate [ output; original ] published |> assert_rejected;
    validate [ original; call 1 kind; output ] published |> assert_rejected;
    (* Reuse after the retained output does not invalidate this receipt. *)
    validate [ original; output; call 3 kind ] published |> protocol_ok;
    validate [ call ~name:"other" 0 kind; output ] published |> assert_rejected;
    validate [ original; result 2 kind "wrong outcome" ] published |> assert_rejected;
    validate [ original; result ~relation:(Bound (id 9)) 2 kind outcome_text ] published
    |> assert_rejected;
    let other_kind =
      match kind with
      | Function -> Payload.Call_kind.Custom
      | Custom -> Function
    in
    validate [ original; result 2 other_kind outcome_text ] published |> assert_rejected;
    validate [ { original with provenance = Moderator_inserted }; output ] published
    |> assert_rejected;
    validate [ original; { output with provenance = Moderator_inserted } ] published
    |> assert_rejected;
    let discarded = I.discard_publication resolved ~reason:"compacted" |> protocol_ok in
    validate [] discarded |> protocol_ok;
    validate [ original ] discarded |> assert_rejected;
    let digest = Chatmd_shell_spec.Source_ref.digest in
    let fingerprint : I.payload_fingerprint =
      { sha256 = digest "null"; byte_length = 4 }
    in
    let routing : I.routing =
      { kind =
          (match kind with
           | Function -> I.Function
           | Custom -> I.Custom)
      ; original_name = "read_file"
      ; original_payload = fingerprint
      ; final_payload = fingerprint
      ; canonical_payload = Some fingerprint
      ; preparation = Passed
      }
    in
    let routed = invocation ~routing () in
    validate [ original ] routed |> protocol_ok;
    let wrong = { fingerprint with sha256 = digest "other" } in
    validate
      [ original ]
      (invocation ~routing:{ routing with canonical_payload = Some wrong } ())
    |> assert_rejected;
    let wrong = { fingerprint with byte_length = 5 } in
    validate
      [ original ]
      (invocation ~routing:{ routing with canonical_payload = Some wrong } ())
    |> assert_rejected;
    let guidance =
      P.Authoring_guidance.create
        ~context_identity:(digest "foreign scope")
        ~policy_fingerprint:(digest "policy")
        ~purpose:Reference
        ~payload:output.payload
        ~topics:
          [ { id = "chatml.tasks"
            ; document_sha256 = digest "document"
            ; source = Installed (digest "corpus")
            ; complete = true
            }
          ]
      |> protocol_ok
    in
    (* Well-formed guidance is not a matching invocation reference receipt. *)
    validate
      [ original; { output with provenance = Runtime_authoring guidance } ]
      published
    |> assert_rejected);
  print_endline
    "function/custom ordering, compaction, reuse, routing and provenance preserved";
  [%expect
    {| function/custom ordering, compaction, reuse, routing and provenance preserved |}]
;;

let%expect_test
    "snapshot and legacy checks agree across retained subsets and output mutations"
  =
  let checks = ref 0 in
  List.iter [ Payload.Call_kind.Function; Custom ] ~f:(fun kind ->
    let published =
      I.publish_with_history (invocation ()) ~output_entry_id:(id 2) |> protocol_ok
    in
    List.iter [ outcome_text; "wrong" ] ~f:(fun text ->
      let candidates = [ call 0 kind; call 1 kind; result 2 kind text; call 3 kind ] in
      for mask = 0 to 15 do
        let history =
          List.filteri candidates ~f:(fun index _ -> mask land (1 lsl index) <> 0)
        in
        ignore (validate history published : (unit, P.Error.t) result);
        Int.incr checks
      done));
  print_s [%sexp (!checks : int), "exact results agree for retained subsets"];
  [%expect {| (64 "exact results agree for retained subsets") |}]
;;

let%expect_test "snapshot construction rejects malformed history before reuse" =
  let original = call 0 Payload.Call_kind.Function in
  List.iter
    [ { original with payload = `Null }
    ; { original with redacted = true }
    ; { original with role = User }
    ]
    ~f:(fun malformed ->
      match
        Codec.all_of_protocol [ malformed ], H.Validated_history.create [ malformed ]
      with
      | Error expected, Error actual ->
        assert (Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json actual))
      | Ok _, _ | _, Ok _ -> failwith "malformed snapshot was accepted");
  let guidance =
    let digest = Chatmd_shell_spec.Source_ref.digest in
    P.Authoring_guidance.create
      ~context_identity:(digest "context")
      ~policy_fingerprint:(digest "policy")
      ~purpose:Reference
      ~payload:original.payload
      ~topics:
        [ { id = "chatml.tasks"
          ; document_sha256 = digest "document"
          ; source = Installed (digest "corpus")
          ; complete = true
          }
        ]
    |> protocol_ok
  in
  (* The generated sexp decoder can construct invalid guidance; payload decoding
     must still run its validating invariant checks before recording the proof. *)
  let malformed_guidance =
    match P.Authoring_guidance.sexp_of_t guidance with
    | Sexp.List fields ->
      Sexp.List
        (List.map fields ~f:(function
           | Sexp.List [ Atom "context_identity"; _ ] ->
             Sexp.List [ Atom "context_identity"; Atom "invalid" ]
           | field -> field))
      |> P.Authoring_guidance.t_of_sexp
    | _ -> failwith "guidance record did not encode as fields"
  in
  let malformed = { original with provenance = Runtime_authoring malformed_guidance } in
  (match
     Codec.all_of_protocol [ malformed ], H.Validated_history.create [ malformed ]
   with
   | Error expected, Error actual ->
     assert (Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json actual))
   | Ok _, _ | _, Ok _ -> failwith "malformed guidance was accepted");
  (match H.Validated_history.create [ original; original ] with
   | Error error -> [%test_eq: string] "duplicate history identity" error.message
   | Ok _ -> failwith "duplicate identity allowed ambiguous decoded reuse");
  print_endline
    "malformed payload, guidance, redaction, classification and duplicate identity \
     rejected";
  [%expect
    {| malformed payload, guidance, redaction, classification and duplicate identity rejected |}]
;;
