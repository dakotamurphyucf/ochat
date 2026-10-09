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
          ownership =
            Some
              { source
              ; creator = Moderator_event event.context.id
              ; subscription_binding = None
              }
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
            Some
              { source = other_source
              ; creator = Moderator_event event.context.id
              ; subscription_binding = None
              }
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
    (* Persisted owned deliveries from before the epoch field preserve absent
       versus explicit null through an unrelated real state-document write. *)
    let module Schema = Document_schema in
    let field json name =
      match Schema.Json.field json ~name with
      | Value value -> value
      | Absent | Null -> failwith "missing state fixture field"
    in
    let set json name value =
      match json with
      | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
      | _ -> failwith "expected state fixture object"
    in
    let state_payload =
      Agent_session.Session_state_document.authored committed.state
      |> fun document ->
      Agent_session.Session_state_document.encode document ~limits:document_limits
      |> document_ok
      |> Schema.Document.payload
    in
    let delivery =
      match field state_payload "deliveries" with
      | `Array [ delivery ] -> delivery
      | _ -> failwith "expected one owned delivery"
    in
    let ownership = field delivery "ownership" in
    let absent_ownership =
      match ownership with
      | `Object fields ->
        `Object (List.Assoc.remove fields ~equal:String.equal "subscription_binding")
      | _ -> failwith "expected owned delivery"
    in
    List.iter
      [ absent_ownership; set ownership "subscription_binding" `Null ]
      ~f:(fun ownership ->
        let payload =
          set state_payload "deliveries" (`Array [ set delivery "ownership" ownership ])
          |> Fixtures.legacy_pending_payload
        in
        let original =
          Schema.Document.create
            ~limits:document_limits
            ~kind:"session.state"
            ~version:8
            ~payload
          |> document_ok
        in
        let bytes = Schema.Document.to_string original in
        let decoded =
          Agent_session.Session_state_document.decode ~limits:document_limits original
          |> document_ok
        in
        let state = Agent_session.Session_state_document.value decoded in
        let rewritten =
          Agent_session.Session_state_document.with_value
            decoded
            { state with stop_epoch = 1L }
          |> fun document ->
          Agent_session.Session_state_document.encode document ~limits:document_limits
          |> document_ok
          |> Schema.Document.payload
        in
        let rewritten_delivery =
          match field rewritten "deliveries" with
          | `Array [ delivery ] -> delivery
          | _ -> failwith "expected retained delivery"
        in
        assert (String.equal bytes (Schema.Document.to_string original));
        match
          ( Schema.Json.field ownership ~name:"subscription_binding"
          , Schema.Json.field
              (field rewritten_delivery "ownership")
              ~name:"subscription_binding" )
        with
        | Absent, Absent | Null, Null -> ()
        | (Absent | Null | Value _), (Absent | Null | Value _) ->
          failwith "delivery epoch presence changed");
    let restored = restore_state committed.state |> store_ok in
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
          ownership =
            Some
              { source
              ; creator = Invocation invocation.context.id
              ; subscription_binding = None
              }
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

let%expect_test "subscription delivery retains exact epoch and rejects rebound provenance"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let _, _, _, previous, original = extension_fixture workspace_instance in
    let source : P.Invocation.observer =
      { script_id = "subscription-publisher"; source_sha256 = String.make 64 'a' }
    in
    let event =
      E.create
        { id =
            P.Id.Moderator_execution.of_string "mex_subscription_binding" |> protocol_ok
        ; session_id
        ; generation = 0
        ; source
        ; operation_id = None
        ; job = None
        ; phase = Internal_event
        ; event = `Null
        ; checkpoint_sha256 = String.make 64 'b'
        ; created_at = timestamp
        }
      |> protocol_ok
    in
    let subscription =
      P.Subscription.create { previous.context with source = Some source } |> protocol_ok
    in
    let subscription, _ =
      P.Subscription.finish
        subscription
        ~expected_epoch:0
        ~now:timestamp
        original.context.completion
      |> protocol_ok
    in
    let binding epoch =
      D.Subscription_binding.create ~subscription_id:subscription.context.id ~epoch
      |> protocol_ok
    in
    let delivery epoch source =
      D.create
        { original.context with
          ownership =
            Some
              { source
              ; creator = Moderator_event event.context.id
              ; subscription_binding = Some (binding epoch)
              }
        }
      |> protocol_ok
    in
    let owned = delivery subscription.epoch source in
    let validate value =
      Agent_session.Delivery_ownership.validate
        ~invocations:[]
        ~events:[ event ]
        ~subscriptions:[ subscription ]
        value
    in
    validate owned |> protocol_ok;
    let json = D.to_json owned in
    let fields = P.Json_codec.fields json |> protocol_ok in
    let version =
      P.Json_codec.required_as
        fields
        "schema_version"
        (P.Json_codec.bounded_int ~min:1 ~max:Int.max_value)
      |> protocol_ok
    in
    let decoded = D.of_json json |> protocol_ok in
    let wrong_epoch = delivery (subscription.epoch + 1) source in
    let wrong_source =
      delivery subscription.epoch { source with source_sha256 = String.make 64 'c' }
    in
    let module Schema = Document_schema in
    let field json name =
      match Schema.Json.field json ~name with
      | Value value -> value
      | Absent | Null -> failwith "missing binding fixture field"
    in
    let set json name value =
      match json with
      | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
      | _ -> failwith "expected binding fixture object"
    in
    let codec =
      Schema.Domain_codec.create_validated
        ~limits:document_limits
        ~kind:"fixture.delivery"
        ~version:1
        ~shape:Agent_session.Session_record_shapes.delivery
        ~supported_semantics:[]
        ~validate:(fun value ->
          D.validate value
          |> Result.map_error ~f:(fun error -> Schema.Error.Malformed error.message))
        ~decode:(fun json ->
          D.Storage.of_json json
          |> Result.map_error ~f:(fun error -> Schema.Error.Malformed error.message))
        ~encode:(fun value -> Ok (D.Storage.to_json value))
      |> document_ok
    in
    let status_write payload =
      let document =
        Schema.Document.create
          ~limits:document_limits
          ~kind:"fixture.delivery"
          ~version:1
          ~payload
        |> document_ok
      in
      let carrier = Schema.Domain_codec.decode codec document |> document_ok in
      let committed =
        D.commit
          (Schema.Extension_carrier.value carrier)
          ~history_id:
            (History_entry.Id.create ~namespace:"binding-test" ~sequence:0
             |> Result.ok_or_failwith)
          ~now:timestamp
        |> protocol_ok
      in
      Schema.Domain_codec.encode
        codec
        (Schema.Extension_carrier.with_value carrier committed)
      |> document_ok
      |> Schema.Document.payload
    in
    let payload = D.Storage.to_json owned in
    let owner = field payload "ownership" in
    let raw_binding =
      field owner "subscription_binding"
      |> fun value -> set value "future_binding" (`String "preserved")
    in
    let future =
      set payload "ownership" (set owner "subscription_binding" raw_binding)
      |> status_write
    in
    let future_preserved =
      Schema.Json.equal
        (field (field (field future "ownership") "subscription_binding") "future_binding")
        (`String "preserved")
    in
    let malformed_binding =
      `Object
        [ "subscription_id", P.Id.Subscription.to_json subscription.context.id
        ; "epoch", `Number "-1"
        ]
    in
    print_s
      [%sexp
        { version : int
        ; future_preserved : bool
        ; roundtrip = (D.equal owned decoded : bool)
        ; wrong_epoch_rejected = (Result.is_error (validate wrong_epoch) : bool)
        ; wrong_source_rejected = (Result.is_error (validate wrong_source) : bool)
        ; rebound_rejected =
            (Result.is_error (D.validate_transition ~previous:(Some owned) wrong_epoch)
             : bool)
        ; negative_epoch_rejected =
            (Result.is_error (D.Subscription_binding.of_json malformed_binding) : bool)
        ; sexp_validated = (D.equal owned (D.t_of_sexp (D.sexp_of_t owned)) : bool)
        }]);
  [%expect
    {|
    ((version 6) (future_preserved true) (roundtrip true)
     (wrong_epoch_rejected true) (wrong_source_rejected true)
     (rebound_rejected true) (negative_epoch_rejected true)
     (sexp_validated true))
    |}]
;;
