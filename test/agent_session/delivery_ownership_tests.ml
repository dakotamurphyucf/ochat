open Core
open Fixtures
module P = Agent_protocol
module D = P.Delivery
module E = P.Moderator_execution

let%expect_test
    "owned delivery envelopes retain authority through codecs, transitions and journal \
     state"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let initial, _, _, subscription, original = extension_fixture workspace_instance in
    let source : P.Invocation.observer =
      { script_id = "publisher"; source_sha256 = String.make 64 'a' }
    in
    let event =
      E.create
        { id = P.Id.Moderator_execution.of_string "mex_publisher" |> protocol_ok
        ; session_id
        ; generation = 0
        ; source
        ; operation_id = None
        ; job = None
        ; phase = Internal_event
        ; event = `String "result ready"
        ; checkpoint_sha256 = String.make 64 'b'
        ; created_at = timestamp
        }
      |> protocol_ok
    in
    let legacy =
      D.create { original.context with invocation_id = None; work = None } |> protocol_ok
    in
    let owned =
      D.create
        { legacy.context with
          ownership = Some { source; creator = Moderator_event event.context.id }
        }
      |> protocol_ok
    in
    List.iter [ legacy; owned ] ~f:(fun value ->
      let json = D.to_json value in
      assert (Jsonaf.exactly_equal json (D.to_json (D.of_json json |> protocol_ok)));
      assert (Jsonaf.exactly_equal json (D.to_json (D.t_of_sexp (D.sexp_of_t value)))));
    assert (
      not
        (String.is_substring
           (Sexp.to_string_mach (D.sexp_of_t legacy))
           ~substring:"ownership"));
    let envelope = D.to_json owned in
    let fields = P.Json_codec.fields envelope |> protocol_ok in
    assert (Result.is_error (P.Json_codec.required_as fields "id" P.Id.Delivery.of_json));
    let alter name replacement =
      match envelope with
      | `Object fields ->
        `Object
          (List.filter_map fields ~f:(fun (key, value) ->
             if String.equal key name
             then Option.map replacement ~f:(fun value -> key, value)
             else Some (key, value)))
      | _ -> failwith "expected envelope"
    in
    List.iter
      [ alter "ownership" None
      ; alter "schema_version" None
      ; alter "schema_version" (Some (`Number "1"))
      ; alter "schema_version" (Some (`Number "99"))
      ]
      ~f:(fun invalid -> assert (Result.is_error (D.of_json invalid)));
    assert (Result.is_error (D.validate_transition ~previous:(Some legacy) owned));
    assert (Result.is_error (D.validate_transition ~previous:(Some owned) legacy));
    let other_source = { source with source_sha256 = String.make 64 'c' } in
    let rebound =
      D.create
        { owned.context with
          ownership =
            Some { source = other_source; creator = Moderator_event event.context.id }
        }
      |> protocol_ok
    in
    assert (Result.is_error (D.validate_transition ~previous:(Some owned) rebound));
    let completed =
      E.complete
        event
        ~checkpoint_sha256:(String.make 64 'b')
        ~requests:{ request_turn = false; request_compaction = false; end_session = None }
      |> protocol_ok
    in
    let delta =
      Agent_session.Session_delta.Batch
        [ Moderator_execution_changed event
        ; Moderator_execution_changed completed
        ; Delivery_changed owned
        ]
    in
    let replay =
      Agent_session.Session_delta.t_of_sexp (Agent_session.Session_delta.sexp_of_t delta)
    in
    let committed =
      Agent_session.Session_transition.apply
        ~now:timestamp
        initial
        ~delta:replay
        ~payloads:[]
      |> protocol_ok
    in
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t committed.state))
      |> store_ok
    in
    assert (Jsonaf.exactly_equal envelope (D.to_json (List.hd_exn restored.deliveries)));
    assert (
      Result.is_error
        (Agent_session.Session_state.validate { restored with moderator_executions = [] }));
    assert (
      Result.is_error
        (Agent_session.Session_state.validate { restored with deliveries = [ rebound ] }));
    let foreign =
      D.create
        { owned.context with
          session_id = P.Id.Session.of_string "ses_foreign" |> protocol_ok
        }
      |> protocol_ok
    in
    assert (
      Result.is_error
        (Agent_session.Delivery_ownership.validate
           ~invocations:[]
           ~events:[ completed ]
           ~subscriptions:[]
           foreign));
    let invocation =
      let context = (invocation_fixture ()).context in
      P.Invocation.create
        ~observer:source
        { context with
          origin = Moderator
        ; provider_call_id = None
        ; parent_invocation = Some (P.Id.Invocation.of_string "inv_parent" |> protocol_ok)
        }
      |> protocol_ok
    in
    let by_invocation =
      D.create
        { owned.context with
          ownership = Some { source; creator = Invocation invocation.context.id }
        }
      |> protocol_ok
    in
    Agent_session.Delivery_ownership.validate
      ~invocations:[ invocation ]
      ~events:[]
      ~subscriptions:[]
      by_invocation
    |> protocol_ok;
    let changed_observer =
      P.Invocation.create ~observer:other_source invocation.context |> protocol_ok
    in
    assert (
      Result.is_error
        (Agent_session.Delivery_ownership.validate
           ~invocations:[ changed_observer ]
           ~events:[]
           ~subscriptions:[]
           by_invocation));
    let bound =
      P.Subscription.create
        { subscription.context with
          id = P.Id.Subscription.create ()
        ; source = Some source
        }
      |> protocol_ok
    in
    let correlated =
      D.create
        { owned.context with
          invocation_id = Some bound.context.invocation_id
        ; work = Some (Subscription bound.context.id)
        }
      |> protocol_ok
    in
    Agent_session.Delivery_ownership.validate
      ~invocations:[]
      ~events:[ completed ]
      ~subscriptions:[ bound ]
      correlated
    |> protocol_ok;
    let other =
      P.Subscription.create { bound.context with source = Some other_source }
      |> protocol_ok
    in
    assert (
      Result.is_error
        (Agent_session.Delivery_ownership.validate
           ~invocations:[]
           ~events:[ completed ]
           ~subscriptions:[ other ]
           correlated));
    print_endline
      "legacy preserved; downgrade/rebinding rejected; owned creator survives journal \
       and snapshot");
  [%expect
    {| legacy preserved; downgrade/rebinding rejected; owned creator survives journal and snapshot |}]
;;
