open! Core
module P = Agent_protocol
module W = P.Instruction_control

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let rejected result =
  match result with
  | Error _ -> ()
  | Ok _ -> failwith "malformed instruction control value admitted"
;;

let replace_field json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal key name then value else old))
  | _ -> failwith "expected instruction object"
;;

let add_field json field =
  match json with
  | `Object fields -> `Object (fields @ [ field ])
  | _ -> failwith "expected instruction object"
;;

let%expect_test "instruction wire methods are bounded consistency data and method affine" =
  let session_id = P.Id.Session.of_string "ses_instruction_wire" |> ok in
  let attachment_id = P.Id.Attachment.of_string "att_instruction_wire" |> ok in
  let principal_id = P.Id.Principal.of_string "pri_instruction_wire" |> ok in
  let root = W.Root_id.of_string "configured-root" |> ok in
  let member = Source.Portable_path.create "AGENTS.md" |> Or_error.ok_exn in
  let digest = Chatml_source.Digest.of_contents "WIRE DATA ONLY" ~checkpoint:ignore in
  let observed = W.Member.create ~root ~member ~digest in
  let selection = W.Selection.Project { root; scope = None } in
  let source = W.Source_request.create ~selection ~members:[ observed ] |> ok in
  let capture = W.Capture.create ~source ~capture_id:digest in
  let key = P.Idempotency_key.of_string "instruction-wire" |> ok in
  let requests =
    [ W.Request.Inspect { session_id; attachment_id = Some attachment_id }
    ; Refresh { session_id; selection = Some selection }
    ; Capture { session_id; source }
    ; Activate
        { session_id
        ; attachment_id
        ; expected_generation = 0
        ; expected_revision = 0L
        ; source
        ; capture_id = digest
        ; key
        }
    ; Remove
        { session_id
        ; attachment_id
        ; expected_generation = 0
        ; expected_revision = 1L
        ; change = All
        ; key
        }
    ]
  in
  let summary =
    P.Instruction_status.create
      ~revision:1L
      ~project_selected:true
      ~project_files:1
      ~skills:0
      ~retained_captures:1
      ~retained_receipts:1
    |> ok
  in
  let view =
    W.View.create
      ~session_id
      ~generation:0
      ~summary
      ~project:(Some source)
      ~skills:[]
      ~permitted_actions:[ Inspect; Refresh; Capture; Activate; Remove ]
    |> ok
  in
  let discovery =
    W.Discovery.create ~roots:[ root ] ~members:[ observed ] ~issues:[] |> ok
  in
  let receipt =
    W.Receipt.create
      ~principal_id
      ~key
      ~request_sha256:(Chatml_source.Digest.to_hex digest)
      ~session_id
      ~generation:0
      ~revision:1L
      ~accepted_at:(P.Timestamp.of_string "2026-01-01T00:00:00Z" |> ok)
    |> ok
  in
  let results =
    [ W.Result.Inspect view
    ; Refresh discovery
    ; Capture capture
    ; Activate receipt
    ; Remove receipt
    ]
  in
  List.iter2_exn requests results ~f:(fun request result ->
    let command = P.Command.Instructions request in
    let method_ = P.Command.method_name command in
    let decoded =
      P.Command.of_method_and_params ~method_ ~params:(P.Command.params command) |> ok
    in
    assert (Jsonaf.exactly_equal (P.Command.params command) (P.Command.params decoded));
    assert (
      Jsonaf.exactly_equal
        (W.Request.to_json request)
        (W.Request.to_json (W.Request.of_json (W.Request.to_json request) |> ok)));
    let result = P.Method_result.Instructions result in
    [%test_eq: string] method_ (P.Method_result.method_name result);
    let public = P.Public.Result.Non_history.of_internal result |> ok in
    let decoded =
      P.Public.Result.of_json ~method_ (P.Public.Result.to_json (Non_history public))
      |> ok
    in
    assert (
      Jsonaf.exactly_equal
        (P.Method_result.to_json result)
        (P.Public.Result.to_json decoded)));
  let activation = List.nth_exn requests 3 in
  let decode params =
    W.Request.of_method_and_params ~method_:(W.Request.method_name activation) ~params
  in
  let params = W.Request.params activation in
  rejected (decode (add_field params ("source_body", `String "forbidden")));
  rejected (decode (add_field params ("key", `String "duplicate")));
  rejected (decode (replace_field params "expected_generation" (`Number "-1")));
  rejected (decode (replace_field params "expected_revision" (`String "01")));
  rejected (decode (replace_field params "expected_revision" (`Number "0")));
  let source_json = Jsonaf.member_exn "source" params in
  rejected
    (decode
       (replace_field
          params
          "source"
          (add_field source_json ("native_token", `String "not authority"))));
  rejected
    (decode
       (replace_field
          params
          "source"
          (replace_field
             source_json
             "members"
             (`Array
                 (List.init 65 ~f:(fun _ ->
                    Jsonaf.member_exn "members" source_json
                    |> Jsonaf.list_exn
                    |> List.hd_exn))))));
  rejected (decode (add_field params ("oversized", `String (String.make 131073 'x'))));
  let different_root = W.Root_id.of_string "other-root" |> ok in
  rejected
    (W.Source_request.create
       ~selection:(W.Selection.Project { root = different_root; scope = None })
       ~members:[ observed ]);
  rejected
    (W.View.create
       ~session_id
       ~generation:0
       ~summary
       ~project:None
       ~skills:[]
       ~permitted_actions:[]);
  let json = W.Result.payload (W.Result.Inspect view) in
  rejected
    (W.Result.of_method_and_result
       ~method_:"session.instructions_get"
       ~result:(add_field json ("captured_body", `String "forbidden")));
  (* The existing summary owns compatible unknowns. The new view deliberately
     preserves that lower codec instead of widening or replacing its schema. *)
  let compatible_summary =
    P.Instruction_status.to_json summary
    |> fun json -> add_field json ("future_summary", `True)
  in
  ignore
    (W.Result.of_method_and_result
       ~method_:"session.instructions_get"
       ~result:(replace_field json "summary" compatible_summary)
     |> ok);
  rejected
    (W.Result.of_method_and_result ~method_:"session.instructions_activate" ~result:json);
  print_endline
    "five typed wire methods; exact public round trips; strict new fields; duplicate, \
     bounds, CAS and inconsistent view rejection; existing summary compatibility";
  [%expect
    {| five typed wire methods; exact public round trips; strict new fields; duplicate, bounds, CAS and inconsistent view rejection; existing summary compatibility |}]
;;
