open! Core
open Agent_protocol

let get = function
  | Ok value -> value
  | Error (error : Agent_protocol.Error.t) -> failwith error.message
;;

let generator char = Id.Generator.create ~bytes:(fun length -> String.make length char)
let ids = generator 'a'
let server_id = Id.Server.create_with ids
let session_id = Id.Session.create_with ids
let session = Session_ref.create ~server_id ~session_id
let goal_id = Id.Goal.create_with ids
let principal_id = Id.Principal.create_with ids
let run_id = Id.Run.create_with ids
let key = Idempotency_key.of_string "goal-operator-original-key" |> get

let source =
  Run_source.create
    ~observer:{ script_id = "main"; source_sha256 = String.make 64 'a' }
    ~generation:3
    ~installation_epoch:2L
  |> get
;;

let print_code = function
  | Ok _ -> print_endline "ok"
  | Error (error : Agent_protocol.Error.t) ->
    print_endline (Error.code_to_string error.code)
;;

let%expect_test "replacement writer keeps the original persisted Goal command digest" =
  let intent =
    Goal_command.create
      ~session_id
      ~generation:3
      ~source:None
      ~expected_catalog_revision:7L
      ~expected_goal_revision:(Some 5L)
      ~key
      ~mutation:(Hold (goal_id, Operator_pause))
    |> get
  in
  let legacy =
    `Object
      [ "kind", `String "goal_command_v1"
      ; "session_id", Id.Session.to_json session_id
      ; "generation", `String "3"
      ; "source", `Null
      ; "expected_catalog_revision", `String "7"
      ; "expected_goal_revision", `String "5"
      ; "key", Idempotency_key.to_json key
      ; ( "mutation"
        , `Object
            [ "kind", `String "hold"
            ; "goal_id", Id.Goal.to_json goal_id
            ; "reason", `Object [ "kind", `String "operator_pause" ]
            ] )
      ]
  in
  let original_digest =
    let canonical = Json_codec.canonical_string legacy |> get in
    Digestif.SHA256.(digest_string canonical |> to_hex)
  in
  let round_trip generator =
    let attachment_id = Id.Attachment.create_with generator in
    let envelope = Goal_control.Request.command ~session ~attachment_id intent |> get in
    let method_ = Goal_control.Request.method_name envelope in
    let params = Goal_control.Request.params envelope in
    let decoded = Command.of_method_and_params ~method_ ~params |> get in
    match decoded with
    | Goal_control (Command request) ->
      Id.Attachment.equal attachment_id request.attachment_id
      && String.equal original_digest (Goal_command.digest request.intent |> get)
    | _ -> false
  in
  print_s [%sexp (round_trip ids : bool), (round_trip (generator 'b') : bool)];
  let foreign =
    Session_ref.create ~server_id ~session_id:(Id.Session.create_with (generator 'b'))
  in
  print_code
    (Goal_control.Request.command
       ~session:foreign
       ~attachment_id:(Id.Attachment.create_with ids)
       intent);
  [%expect
    {|
    (true true)
    invalid_request
    |}]
;;

let%expect_test "Start keeps explicit source and revision fences when decoded" =
  let intent =
    Goal_control.Start.create
      ~generation:3
      ~expected_revision:11L
      ~source
      ~goal_id
      ~expected_goal_revision:5L
      ~expected_catalog_revision:7L
      ~input:Authored_start
      ~key
    |> get
  in
  let envelope =
    Goal_control.Request.start
      ~session
      ~attachment_id:(Id.Attachment.create_with ids)
      intent
    |> get
  in
  let method_ = Goal_control.Request.method_name envelope in
  let params = Goal_control.Request.params envelope in
  let replace_intent ~f =
    match params with
    | `Object fields ->
      `Object
        (List.map fields ~f:(function
           | "intent", `Object intent -> "intent", `Object (f intent)
           | field -> field))
    | _ -> failwith "Goal control request is not an object"
  in
  Command.of_method_and_params ~method_ ~params |> print_code;
  let remove_revision fields =
    List.filter fields ~f:(fun (field, _) -> not (String.equal "expected_revision" field))
  in
  let corrupt_generation fields =
    List.Assoc.add fields ~equal:String.equal "generation" (`String "2")
  in
  let add_extra fields = ("receipt", `Null) :: fields in
  List.iter [ remove_revision; corrupt_generation; add_extra ] ~f:(fun f ->
    Command.of_method_and_params ~method_ ~params:(replace_intent ~f) |> print_code);
  [%expect
    {|
    ok
    invalid_request
    invalid_request
    invalid_request
    |}]
;;

let%expect_test "Goal transcript summaries omit retained Run receipts" =
  let created_at = Timestamp.of_string "2026-11-17T10:00:00Z" |> get in
  let run =
    Run.create
      ~id:run_id
      ~session
      ~principal_id
      ~source
      ~mode:Workflow
      ~lifecycle:Admitted
      ~revision:0L
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at
      ~updated_at:created_at
    |> get
  in
  let admission =
    Run_receipt.create
      ~run_id
      ~principal_id
      ~source
      ~key
      ~request_sha256:(String.make 64 'b')
      ~kind:Admission
      ~run_revision:0L
      ~session_revision:10L
      ~committed_at:created_at
    |> get
  in
  let episode = Goal_episode.create ~run ~admission |> get in
  let objective = Goal.Objective.of_string "Keep customer records current" |> get in
  let goal =
    Goal.create
      ~id:goal_id
      ~session
      ~creator:principal_id
      ~source
      ~objective
      ~budget:Goal.Budget.unlimited
    |> get
  in
  let goal = Goal.activate_episode goal episode ~previous:None |> get in
  let row : Goal_query.View.row =
    { goal = Goal_query.Goal_summary.of_goal goal
    ; run = Unavailable
    ; accounting = None
    ; actions = []
    ; execution_blockers = [ Unavailable ]
    }
  in
  let view =
    Goal_query.View.create
      ~session
      ~generation:3
      ~session_revision:10L
      ~catalog_revision:2L
      ~source:(Some source)
      ~availability:Stopped
      ~can_create:false
      ~goals:[ row ]
    |> get
  in
  let result = Goal_query.Result.Get (Available view) in
  let method_ = Goal_query.Result.method_name result in
  let payload = Goal_query.Result.payload result in
  let encoded = Jsonaf.to_string payload in
  print_s
    [%sexp
      { decoded = (Public.Result.of_json ~method_ payload |> Result.is_ok : bool)
      ; goal_present =
          (String.is_substring encoded ~substring:(Id.Goal.to_string goal_id) : bool)
      ; principal_present =
          (String.is_substring encoded ~substring:(Id.Principal.to_string principal_id)
           : bool)
      ; key_present =
          (String.is_substring encoded ~substring:(Idempotency_key.to_string key) : bool)
      ; run_present =
          (String.is_substring encoded ~substring:(Id.Run.to_string run_id) : bool)
      }];
  [%expect
    {|
    ((decoded true) (goal_present true) (principal_present false)
     (key_present false) (run_present false))
    |}]
;;

let%expect_test
    "Resume acknowledgement carries the new receipt and compact terminal evidence"
  =
  let accepted_at = Timestamp.of_string "2026-11-17T10:00:00Z" |> get in
  let receipt =
    Goal_command_receipt.create
      ~principal_id
      ~key
      ~request_sha256:(String.make 64 'c')
      ~goal_id
      ~goal_revision:6L
      ~catalog_revision:8L
      ~source:(Some source)
      ~accepted_at
    |> get
  in
  let result =
    Goal_control.Result.Resume
      (Resumed { receipt; continuation = Settled { run_id; run_revision = 12L } })
  in
  let method_ = Goal_control.Result.method_name result in
  let payload = Goal_control.Result.payload result in
  let verified =
    match Method_result.of_json ~method_ payload |> get with
    | Goal_control
        (Resume (Resumed { receipt = actual; continuation = Settled terminal })) ->
      Goal_command_receipt.equal receipt actual
      && Id.Run.equal terminal.run_id run_id
      && Int64.equal terminal.run_revision 12L
    | _ -> false
  in
  print_s [%sexp (verified : bool)];
  let fields = Json_codec.fields payload |> get in
  let continuation =
    Json_codec.required_as fields "continuation" Json_codec.fields |> get
  in
  print_s [%sexp (List.map (Json_codec.to_alist continuation) ~f:fst : string list)];
  [%expect
    {|
    true
    (kind run_id run_revision)
    |}]
;;

let%expect_test "nested Goal wire fields are closed without changing persisted Goal JSON" =
  let field key f = function
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name key then f value else value))
    | _ -> failwith "expected object"
  in
  let extra = function
    | `Object fields -> `Object (("unexpected", `Null) :: fields)
    | _ -> failwith "expected object"
  in
  let observed_tokens =
    Goal.Budget.Token_stop.create ~component:Input ~ceiling:10L |> get
  in
  let budget =
    Goal.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:None
      ~observed_tokens:(Some observed_tokens)
      ~deadline:None
    |> get
  in
  let objective = Goal.Objective.of_string "Check nested JSON members" |> get in
  let goal =
    Goal.create ~id:goal_id ~session ~creator:principal_id ~source ~objective ~budget
    |> get
  in
  let row : Goal_query.View.row =
    { goal = Goal_query.Goal_summary.of_goal goal
    ; run = No_episode
    ; accounting = None
    ; actions = []
    ; execution_blockers = []
    }
  in
  let view =
    Goal_query.View.create
      ~session
      ~generation:3
      ~session_revision:2L
      ~catalog_revision:1L
      ~source:(Some source)
      ~availability:Stopped
      ~can_create:false
      ~goals:[ row ]
    |> get
  in
  let result = Goal_query.Result.Get (Available view) in
  let method_ = Goal_query.Result.method_name result in
  let payload = Goal_query.Result.payload result in
  let decode_result json = Goal_query.Result.of_method_and_result ~method_ ~result:json in
  let in_goal f =
    field
      "view"
      (field "goals" (function
         | `Array [ row ] -> `Array [ field "goal" f row ]
         | _ -> failwith "expected one Goal row"))
      payload
  in
  decode_result payload |> print_code;
  in_goal (field "budget" extra) |> decode_result |> print_code;
  in_goal (field "budget" (field "observed_tokens" extra)) |> decode_result |> print_code;
  in_goal (field "state" (field "reason" extra)) |> decode_result |> print_code;
  let decode_mutation ?revision mutation =
    let intent =
      Goal_command.create
        ~session_id
        ~generation:3
        ~source:(Some source)
        ~expected_catalog_revision:1L
        ~expected_goal_revision:revision
        ~key
        ~mutation
      |> get
    in
    let json = Goal_command.to_json intent in
    Goal_command.of_json json |> print_code;
    field "mutation" (field "budget" (field "observed_tokens" extra)) json
    |> Goal_command.of_json
    |> print_code
  in
  decode_mutation (Create { id = goal_id; objective; budget });
  decode_mutation ~revision:0L (Change_budget (goal_id, budget));
  assert (
    Result.is_ok
      (Goal.Budget.to_json budget |> field "observed_tokens" extra |> Goal.Budget.of_json));
  assert (Result.is_ok (Goal.Hold.to_json Source_review |> extra |> Goal.Hold.of_json));
  [%expect
    {|
    ok
    invalid_request
    invalid_request
    invalid_request
    ok
    invalid_request
    ok
    invalid_request
    |}]
;;
