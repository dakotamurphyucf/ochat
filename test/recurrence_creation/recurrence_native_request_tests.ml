open! Core
module A = Agent_session
module P = Agent_protocol
module Due = Due_support
module Request = A.Recurrence_native_request
module Effect = A.Recurrence_native_admission.Effect
module Command = A.Recurrence_command

let ok = Due.protocol_ok
let field = Due.field
let set = Due.set
let definition = Due.definition "native_request"
let id = A.Recurrence_entry.id (A.Recurrence_entry.create definition |> ok)
let source = Due.source 1L

let existing =
  `Object
    [ "kind", `String "existing_session"
    ; "session", P.Session_ref.to_json Due.session
    ; ( "input"
      , P.Session.Message_content.to_json
          { kind = Chatmd; text = "**retain exactly**"; attachments = [] } )
    ]
;;

let authored =
  `Object
    [ "kind", `String "authored_child"
    ; "name", `String "weekly_worker"
    ; "mode", `String "workflow"
    ]
;;

let basis ~create ~source =
  `Object
    [ "generation", `Number "0"
    ; "expected_catalog_revision", `String "0"
    ; ("expected_control_revision", if create then `Null else `String "0")
    ; ("source", if source then P.Run_source.to_json (Due.source 1L) else `Null)
    ; ("expected_lifecycle_revision", if source then `String "0" else `Null)
    ; "key", `String "native-original"
    ]
;;

let capture target =
  `Object
    [ ( "schedule"
      , A.Recurrence_definition.Schedule.to_json
          (A.Recurrence_definition.schedule definition) )
    ; "target", target
    ; "policy", A.Recurrence_policy.to_json A.Recurrence_policy.default
    ]
;;

let request effect_kind target =
  let with_id extra = `Object (("id", P.Id.Recurrence.to_json id) :: extra) in
  match effect_kind with
  | Effect.List -> `Object []
  | Get | Status | History -> with_id []
  | Pause | Cancel -> with_id [ "basis", basis ~create:false ~source:false ]
  | Resume -> with_id [ "basis", basis ~create:false ~source:true ]
  | Create | Edit ->
    with_id
      [ "basis", basis ~create:(Effect.equal effect_kind Create) ~source:true
      ; "capture", capture target
      ]
;;

let%expect_test "native recurrence requests keep both selectors and strict operation CAS" =
  List.iter Effect.all ~f:(fun effect_kind ->
    List.iter [ existing; authored ] ~f:(fun target ->
      let raw = request effect_kind target in
      let module Schema = Chatmd_shell_spec.Tool_schema in
      let checked_schema =
        Result.bind
          (Schema.compile (Request.parameters effect_kind))
          ~f:(fun schema -> Schema.validate schema raw)
      in
      (match checked_schema with
       | Ok () -> ()
       | Error diagnostics -> raise_s [%sexp (diagnostics : Schema.diagnostic list)]);
      ignore (Request.decode ~effect_kind raw |> ok : Request.t);
      assert (
        Result.is_error
          (Request.decode
             ~effect_kind
             (Due.append raw "controller" (P.Session_ref.to_json Due.session))));
      assert (
        Result.is_error (Request.decode ~effect_kind (Due.append raw "controller" `Null)));
      match raw with
      | `Object ((name, value) :: _) ->
        assert (Result.is_error (Request.decode ~effect_kind (Due.append raw name value)))
      | `Object [] -> ()
      | _ -> failwith "fixture request is not an object"));
  let original = request Create authored in
  let bad_basis name value =
    set original "basis" (set (field original "basis") name value)
  in
  List.iter
    [ bad_basis "generation" (`Number "-1")
    ; bad_basis "generation" (`String "0")
    ; bad_basis "expected_catalog_revision" (`String "00")
    ; bad_basis "expected_catalog_revision" (`Number "0")
    ; bad_basis "source" `Null
    ; bad_basis "expected_control_revision" (`String "0")
    ; set
        original
        "capture"
        (set
           (field original "capture")
           "target"
           (Due.append authored "path" (`String "workers/weekly.chatmd")))
    ; set
        original
        "capture"
        (set (field original "capture") "target" (set authored "mode" `Null))
    ]
    ~f:(fun json -> assert (Result.is_error (Request.decode ~effect_kind:Create json)));
  let oversized =
    request Create existing
    |> fun raw ->
    let capture = field raw "capture" in
    let target = field capture "target" in
    let input = field target "input" in
    set
      raw
      "capture"
      (set
         capture
         "target"
         (set target "input" (set input "text" (`String (String.make 393217 'x')))))
  in
  assert (Result.is_error (Request.decode ~effect_kind:Create oversized));
  let deep =
    List.init 40 ~f:Fn.id |> List.fold ~init:`Null ~f:(fun value _ -> `Array [ value ])
  in
  assert (
    Result.is_error (Request.decode ~effect_kind:Create (Due.append original "deep" deep)));
  print_endline
    "nine strict operations; both target selectors; explicit generation/CAS; no erased \
     path/options";
  [%expect
    {| nine strict operations; both target selectors; explicit generation/CAS; no erased path/options |}]
;;

let command raw =
  match Request.decode ~effect_kind:Create raw |> ok with
  | Create { id; basis; capture } ->
    let target =
      match capture.target with
      | Request.Target.Existing_session { session; input } ->
        A.Recurrence_target.create_existing
          ~session
          ~source
          ~configuration_revision:0L
          ~input
        |> ok
      | Authored_child _ ->
        failwith "pure CAS law uses an existing target, not a fabricated authored capture"
    in
    Command.create
      ~session_id:(P.Session_ref.session_id Due.session)
      ~generation:basis.generation
      ~source:basis.source
      ~expected_catalog_revision:basis.expected_catalog_revision
      ~expected_control_revision:basis.expected_control_revision
      ~expected_lifecycle_revision:basis.expected_lifecycle_revision
      ~key:basis.key
      ~mutation:
        (Create (id, { schedule = capture.schedule; target; policy = capture.policy }))
    |> ok
  | _ -> failwith "expected decoded Create"
;;

let%expect_test
    "echoed native generation preserves lower exact receipt replay without refreshing CAS"
  =
  let raw = request Create existing in
  let original = command raw in
  let prepare catalog request ~generation ~source =
    A.Recurrence_command_transition.prepare
      catalog
      ~request
      ~principal_id:Due.principal_id
      ~session:Due.session
      ~generation
      ~current_source:(Some source)
      ~current_lifecycle_revision:(Some P.Session_lifecycle.Revision.zero)
      ~now:Due.timestamp
  in
  let catalog, receipt =
    match prepare A.Recurrence_catalog.empty original ~generation:0 ~source |> ok with
    | Commit { catalog; receipt } -> catalog, receipt
    | Retained _ -> failwith "new fixture unexpectedly retained"
  in
  let next_source =
    P.Run_source.create ~observer:source.observer ~generation:1 ~installation_epoch:1L
    |> ok
  in
  (match prepare catalog original ~generation:1 ~source:next_source |> ok with
   | Retained same -> assert (A.Recurrence_command_receipt.equal receipt same)
   | Commit _ -> failwith "an exact old-generation retry recaptured state");
  let changed_basis =
    field raw "basis"
    |> fun basis ->
    set (set basis "generation" (`Number "1")) "source" (P.Run_source.to_json next_source)
  in
  let changed = command (set raw "basis" changed_basis) in
  let error =
    match prepare catalog changed ~generation:1 ~source:next_source with
    | Error error -> error
    | Ok _ -> failwith "same key accepted changed original generation/source"
  in
  assert (P.Error.equal_code error.code Conflict);
  assert (not error.retryable);
  let changed_input =
    let capture = field raw "capture" in
    let target = field capture "target" in
    let input = field target "input" in
    set
      raw
      "capture"
      (set capture "target" (set target "input" (set input "text" (`String "changed"))))
    |> command
  in
  let error =
    match prepare catalog changed_input ~generation:0 ~source with
    | Error error -> error
    | Ok _ -> failwith "same native key accepted a different original input"
  in
  assert (P.Error.equal_code error.code Conflict);
  assert (not error.retryable);
  let stale_fresh =
    set raw "basis" (set (field raw "basis") "key" (`String "fresh-stale")) |> command
  in
  let error =
    match prepare catalog stale_fresh ~generation:1 ~source:next_source with
    | Error error -> error
    | Ok _ -> failwith "fresh request accepted stale original generation"
  in
  assert (P.Error.equal_code error.code Conflict);
  print_endline
    "old exact receipt retained; changed-generation same key conflicts; fresh stale \
     basis conflicts";
  [%expect
    {| old exact receipt retained; changed-generation same key conflicts; fresh stale basis conflicts |}]
;;

let%expect_test
    "native recurrence registration does not grant an unbound caller authority"
  =
  Eio_main.run (fun _env ->
    let registration = A.Recurrence_native_tool.registration Effect.List in
    let output =
      registration.Chat_response.Agent_runtime.implementation.Ochat_function.run "{}"
    in
    let text =
      match output with
      | Openai.Responses.Tool_output.Output.Text text -> text
      | _ -> failwith "native recurrence returned nontext"
    in
    match Jsonaf.of_string text |> P.Invocation.outcome_of_json |> ok with
    | Fail failure ->
      assert (String.equal failure.code "invalid_request");
      assert (not failure.retryable)
    | Complete _ | Pending _ | Cancelled _ ->
      failwith "registration fabricated an admitted native scope");
  print_endline "unbound native call rejected; registration creates no effect authority";
  [%expect {| unbound native call rejected; registration creates no effect authority |}]
;;
