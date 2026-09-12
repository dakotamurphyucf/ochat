open! Core
open Agent_server_test_support
module Q = Chat_response.Authoring_context
module V = Chat_response.Authoring_validation
module C = Chat_response.Tool_capability

let host ?(surface = V.Ordinary) ?(runtime = "query-fixture-v1") () =
  V.create_host
    ~runtime_identity:runtime
    ~targets:[ One_off_script; Standalone_tool; Moderator; Generated_chatmd ]
    ~moderator_surface:surface
    ~compilation:Chatml_compilation.default_limits
  |> Result.ok_or_failwith
;;

let request ?task ?query ?topic_id ?features ?cursor ?max_tokens operation =
  let string = function
    | None -> `Null
    | Some s -> `String s
  in
  `Object
    [ "version", `Number "1"
    ; "operation", `String operation
    ; "task", string task
    ; "query", string query
    ; "topic_id", string topic_id
    ; ( "features"
      , match features with
        | None -> `Null
        | Some xs -> `Array (List.map xs ~f:(fun s -> `String s)) )
    ; "cursor", string cursor
    ; ( "max_tokens"
      , match max_tokens with
        | None -> `Null
        | Some n -> `Number (Int.to_string n) )
    ]
;;

let field json name =
  match json with
  | `Object fields -> List.Assoc.find_exn fields name ~equal:String.equal
  | _ -> failwith "expected response object"
;;

let require_json expected actual =
  match Jsonaf.exactly_equal expected actual with
  | true -> ()
  | false ->
    raise_s
      [%sexp
        "unexpected JSON"
      , (Jsonaf.to_string expected : string)
      , (Jsonaf.to_string actual : string)]
;;

let items json =
  match field json "items" with
  | `Array xs -> xs
  | _ -> failwith "expected items"
;;

let has_error json =
  match json with
  | `Object fields -> List.Assoc.mem fields "error" ~equal:String.equal
  | _ -> false
;;

let capabilities () =
  C.create
    ~owner:"query-fixture"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "empty query tools")
    []
  |> Result.map_error ~f:(fun error -> error.C.message)
  |> Result.ok_or_failwith
;;

let%expect_test
    "documentation queries preserve strict shape, source pages and caller context"
  =
  let service =
    Q.create ~secret:"query-fixture-secret-not-a-credential" ~max_tokens:1_000_000 ()
    |> Result.ok_or_failwith
  in
  let capabilities = capabilities () in
  let query ?(host = host ()) ?(scope = "parent-generation-1") request =
    Q.query service ~host ~capabilities ~scope request
  in
  let topic =
    request
      ~task:"moderator_tool"
      ~topic_id:"runtime.jobs.shell-example"
      ~max_tokens:6000
      "topic"
  in
  let first = query topic in
  assert (not (has_error first));
  let first_cursor =
    match field first "next_cursor" with
    | `String s -> s
    | _ -> failwith "expected multiple source pages"
  in
  let rec collect count reversed response =
    assert (count < 100);
    let budget = field response "budget" in
    require_json
      (`Number (Int.to_string ((String.length (Jsonaf.to_string response) + 2) / 3)))
      (field budget "token_estimate");
    assert ((String.length (Jsonaf.to_string response) + 2) / 3 <= 6000);
    let page = items response in
    assert (not (List.is_empty page));
    let reversed = List.rev_append page reversed in
    match field response "next_cursor" with
    | `Null ->
      assert (Jsonaf.exactly_equal (field response "complete") `True);
      List.rev reversed
    | `String cursor ->
      collect (count + 1) reversed (query (request ~cursor ~max_tokens:6000 "continue"))
    | _ -> failwith "invalid cursor"
  in
  let paged = collect 0 [] first in
  let whole =
    query
      (request
         ~task:"moderator_tool"
         ~topic_id:"runtime.jobs.shell-example"
         ~max_tokens:1_000_000
         "topic")
  in
  assert (Jsonaf.exactly_equal (field whole "complete") `True);
  assert (List.equal Jsonaf.exactly_equal paged (items whole));
  let shown =
    List.map paged ~f:(fun item ->
      match field item "text" with
      | `String s -> s
      | _ -> assert false)
  in
  assert (
    List.exists shown ~f:(fun text ->
      String.is_substring text ~substring:"Notification.publish"
      && String.is_substring text ~substring:"```ocaml"));
  assert (
    not
      (List.exists shown ~f:(fun text ->
         String.is_substring text ~substring:"<!-- ochat-authoring-example")));
  let continue = request ~cursor:first_cursor ~max_tokens:6000 "continue" in
  List.iter
    [ query ~scope:"child-generation-1" continue
    ; query ~host:(host ~runtime:"changed" ()) continue
    ; query ~host:(host ~surface:V.Delegated ()) continue
    ; query (request ~cursor:(first_cursor ^ "tampered") ~max_tokens:6000 "continue")
    ]
    ~f:(fun result -> assert (has_error result));
  assert (
    has_error
      (query (request ~task:"one_off_script" ~topic_id:"runtime.jobs.timers" "topic")));
  assert (
    has_error
      (query
         (request
            ~task:"moderator_tool"
            ~query:"timers"
            ~topic_id:"runtime.jobs.timers"
            "search")));
  let incomplete_shape =
    match topic with
    | `Object xs ->
      `Object (List.filter xs ~f:(fun (name, _) -> not (String.equal name "cursor")))
    | _ -> assert false
  in
  assert (has_error (query incomplete_shape));
  let prepared =
    query
      (request
         ~task:"background_workflow"
         ~features:[ "timers"; "notifications" ]
         ~max_tokens:6000
         "prepare")
  in
  assert (not (has_error prepared));
  require_json `False (field prepared "package_complete");
  List.iter
    [ "one_off_script"
    ; "standalone_tool"
    ; "moderator_tool"
    ; "child_agent"
    ; "background_workflow"
    ]
    ~f:(fun task ->
      let response = query (request ~task ~max_tokens:1_000_000 "prepare") in
      require_json `True (field response "complete");
      let task_guidance =
        List.find_exn (items response) ~f:(fun item ->
          match Jsonaf.member "topic_id" item with
          | Some (`String id) -> String.equal id "chatml.task-effects"
          | _ -> false)
      in
      let text = field task_guidance "text" |> Jsonaf.string_exn in
      assert (String.is_substring text ~substring:"not a general exception boundary");
      assert (String.is_substring text ~substring:"inside map");
      require_json `False (field response "package_complete"));
  let orientation = List.hd_exn (items prepared) in
  require_json (`String "orientation") (field orientation "kind");
  let content = field orientation "content" in
  require_json (`Array []) (field content "selected_tools");
  let guides = field content "guides" |> Jsonaf.list_exn in
  (* Follow every advertised direct route through real surface/dependency checks. *)
  List.iter guides ~f:(fun guide ->
    let task = field guide "suggested_task" |> Jsonaf.string_exn in
    field guide "topic_ids"
    |> Jsonaf.list_exn
    |> List.iter ~f:(fun id ->
      let response = query (request ~task ~topic_id:(Jsonaf.string_exn id) "topic") in
      match has_error response with
      | false -> ()
      | true -> failwith (Jsonaf.to_string response)));
  let one_off_host =
    V.create_host
      ~runtime_identity:"one-off-only"
      ~targets:[ One_off_script ]
      ~moderator_surface:Ordinary
      ~compilation:Chatml_compilation.default_limits
    |> Result.ok_or_failwith
  in
  let limited = query ~host:one_off_host (request ~task:"one_off_script" "prepare") in
  let limited_map = field (List.hd_exn (items limited)) "content" in
  require_json
    (`Array [ `String "one_off_script" ])
    (field limited_map "enabled_authoring_tasks");
  field limited_map "guides"
  |> Jsonaf.list_exn
  |> List.iter ~f:(fun guide ->
    match Jsonaf.string_exn (field guide "suggested_task") with
    | "one_off_script" -> ()
    | _ -> require_json `False (field guide "suggested_task_enabled"));
  assert (
    has_error
      (query
         (request
            ~task:"one_off_script"
            ~query:"no-matching-feature"
            ~max_tokens:1
            "search")));
  let symbol =
    query
      (request ~task:"moderator_tool" ~query:"Subscription.arm" ~max_tokens:6000 "search")
  in
  require_json
    (`String "runtime.jobs.subscriptions")
    (field (List.hd_exn (items symbol)) "topic_id");
  let signatures =
    query
      (request
         ~task:"one_off_script"
         ~topic_id:"reference.signatures"
         ~max_tokens:1_000_000
         "topic")
  in
  assert (not (has_error signatures));
  require_json `True (field signatures "complete");
  require_json
    (`String "signature_legend")
    (field (List.hd_exn (items signatures)) "kind");
  let declarations =
    List.tl_exn (items signatures)
    |> List.concat_map ~f:(fun item -> field item "declarations" |> Jsonaf.list_exn)
  in
  let names =
    List.map declarations ~f:(fun item -> field item "name" |> Jsonaf.string_exn)
  in
  assert (List.mem names "main" ~equal:String.equal);
  assert (List.mem names "json" ~equal:String.equal);
  assert (not (List.mem names "Process.run" ~equal:String.equal));
  assert (not (List.mem names "Notification.publish" ~equal:String.equal));
  let language_symbol =
    query (request ~task:"one_off_script" ~query:"Array.map" "search")
  in
  let reference_hit =
    List.find_exn (items language_symbol) ~f:(fun item ->
      Jsonaf.exactly_equal (field item "topic_id") (`String "reference.signatures"))
  in
  assert (
    List.mem
      (field reference_hit "matching_symbols" |> Jsonaf.list_exn)
      (`String "Array.map")
      ~equal:Jsonaf.exactly_equal);
  let restricted =
    query
      (request ~task:"one_off_script" ~query:"Subscription.arm" ~max_tokens:6000 "search")
  in
  assert (List.is_empty (items restricted));
  print_endline
    "strict request fields; complete atomic source pagination; stale/foreign cursor \
     rejection; target-aware symbol search; honest package coverage";
  [%expect
    {| strict request fields; complete atomic source pagination; stale/foreign cursor rejection; target-aware symbol search; honest package coverage |}]
;;

let%expect_test
    "model-visible documentation tool retrieves installed text through the actual \
     invocation path"
  =
  let host = host () in
  Mirage_crypto_rng_unix.use_default ();
  let registration =
    Agent_session.Authoring_context_tool.registration ~host |> Result.ok_or_failwith
  in
  assert registration.implementation.info.function_.strict;
  let schema = registration.implementation.info.function_.parameters in
  require_json `False (field schema "additionalProperties");
  assert (
    not
      (List.Assoc.mem
         (match schema with
          | `Object fields -> fields
          | _ -> assert false)
         "anyOf"
         ~equal:String.equal));
  let requested = request ~task:"child_agent" "prepare" in
  let received = ref None in
  let schema_response = ref None in
  Fixtures.with_daemon
    ~validation_host:host
    ~sources:
      [ ( "agent.chatmd"
        , {|<developer>Use the documentation query tool to learn ChatML.</developer><tool name="ochat_authoring_context"/>|}
        )
      ]
    ~calls:
      [ "documentation", "ochat_authoring_context", requested
      ; ( "schemas"
        , "ochat_authoring_context"
        , request ~task:"child_agent" ~topic_id:"reference.tools" "topic" )
      ]
    ~inspect_request:(fun number inputs ->
      match number with
      | 2 ->
        List.iter inputs ~f:(function
          | Openai.Responses.Item.Function_call_output
              { call_id = "documentation"; output = Text text; _ } ->
            received := Some text
          | Openai.Responses.Item.Function_call_output
              { call_id = "schemas"; output = Text text; _ } ->
            schema_response := Some text
          | _ -> ())
      | _ -> ())
    (fun state ->
       let response =
         Option.value_exn !received
         |> Jsonaf.of_string
         |> Agent_protocol.Invocation.outcome_of_json
         |> protocol_ok
         |> function
         | Agent_protocol.Invocation.Complete (`String text) -> Jsonaf.of_string text
         | other -> raise_s [%sexp (other : Agent_protocol.Invocation.outcome)]
       in
       (match has_error response with
        | true -> failwith (Jsonaf.to_string response)
        | false -> ());
       let first = List.hd_exn (items response) in
       require_json (`String "orientation") (field first "kind");
       let selected = field (field first "content") "selected_tools" |> Jsonaf.list_exn in
       require_json
         (`Array [ `String "ochat_authoring_context" ])
         (`Array (List.map selected ~f:(fun tool -> field tool "name")));
       require_json (`String "child_agent") (field response "task");
       let schemas =
         Option.value_exn !schema_response
         |> Jsonaf.of_string
         |> Agent_protocol.Invocation.outcome_of_json
         |> protocol_ok
         |> function
         | Agent_protocol.Invocation.Complete (`String text) -> Jsonaf.of_string text
         | other -> raise_s [%sexp (other : Agent_protocol.Invocation.outcome)]
       in
       let schema_item = List.hd_exn (items schemas) in
       [%test_eq: int] 1 (List.length (items schemas));
       require_json Q.parameters (field schema_item "input_schema");
       require_json `True (field schema_item "strict");
       require_json (`String "native_output") (field schema_item "result_contract");
       let module R = Agent_protocol.Authoring_reference in
       let module I = Agent_protocol.Invocation in
       let module State = Agent_session.Session_state in
       let alter invocation name replacement =
         match I.sexp_of_t invocation with
         | Sexp.List fields ->
           let fields =
             List.filter fields ~f:(function
               | Sexp.List (Sexp.Atom key :: _) -> not (String.equal key name)
               | _ -> true)
           in
           I.t_of_sexp (Sexp.List (fields @ Option.to_list replacement))
         | _ -> assert false
       in
       let without_reference invocation = alter invocation "authoring_reference" None in
       List.iter state.invocations ~f:(fun invocation ->
         let reference = Option.value_exn invocation.I.authoring_reference in
         let output_id = Option.value_exn invocation.output_entry_id in
         let output =
           List.find_exn state.conversation.canonical_history ~f:(fun entry ->
             Agent_protocol.History.Id.equal entry.id output_id)
         in
         (match output.provenance with
          | Runtime_authoring guidance ->
            assert (guidance.version = 2);
            assert (
              Agent_protocol.Authoring_guidance.equal_purpose guidance.purpose Reference);
            assert (
              Agent_protocol.Authoring_guidance.matches_payload guidance output.payload);
            assert (
              List.equal
                Agent_protocol.Authoring_guidance.equal_topic
                guidance.topics
                (List.map reference.topics ~f:(fun topic -> topic.R.topic)))
          | _ -> failwith "published reference has no trusted history provenance");
         let value =
           match invocation.status with
           | Published (Complete value) -> value
           | _ -> failwith "reference invocation was not published"
         in
         assert (R.matches_output reference value);
         assert (
           not (R.matches_output reference (`String "disclosure replaced the result")));
         assert (
           String.equal
             reference.scope
             (R.scope_for
                ~session_id:state.identity.session_id
                ~generation:state.identity.generation));
         assert (I.equal invocation (I.of_json (I.to_json invocation) |> protocol_ok));
         assert (
           Result.is_error
             (I.validate
                (alter
                   invocation
                   "status"
                   (Some
                      (Sexp.List
                         [ Sexp.Atom "status"
                         ; I.sexp_of_status (Published (Complete `Null))
                         ])))));
         assert (
           Result.is_error
             (I.validate_transition
                ~previous:(Some invocation)
                (without_reference invocation))));
       let restored =
         State.sexp_of_t state
         |> Sexp.to_string_mach
         |> Agent_session.Session_persistence.restore_snapshot
         |> Result.map_error ~f:(fun error ->
           Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
         |> Result.ok_or_failwith
       in
       State.validate restored |> protocol_ok;
       Authoring_publication_checks.verify restored;
       assert (Option.is_some restored.conversation.authoring_publication);
       let index = State.authoring_references restored |> protocol_ok in
       assert (
         List.length (Chat_response.Authoring_reference_index.receipts index)
         = List.length state.invocations);
       assert (
         Result.is_error (State.upgrade_schema { restored with schema_version = 19 }));
       assert (List.equal I.equal state.invocations restored.invocations);
       assert (
         Result.is_error (State.upgrade_schema { restored with schema_version = 18 }));
       let legacy =
         { restored with
           schema_version = 18
         ; invocations = List.map restored.invocations ~f:without_reference
         ; conversation = { restored.conversation with authoring_publication = None }
         }
       in
       assert (
         (State.upgrade_schema legacy |> protocol_ok).schema_version
         = State.current_schema_version);
       match Fixtures.result state "documentation" with
       | Complete (`String text) -> assert (not (has_error (Jsonaf.of_string text)))
       | other -> raise_s [%sexp (other : Agent_protocol.Invocation.outcome)]);
  print_endline
    "strict native registration; fake provider receives actual installed reference \
     result; no documentation provider call";
  [%expect
    {| strict native registration; fake provider receives actual installed reference result; no documentation provider call |}]
;;
