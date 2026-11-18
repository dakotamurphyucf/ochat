open Core
open Fixtures
module P = Agent_protocol
module C = Chat_response.Tool_capability

let routing_hook_registry ~observe ~revoke ~explode =
  let registration name run =
    let module Definition = struct
      type input = Jsonaf.t

      let name = name
      let description = Some "owned routing fixture hook"
      let type_ = "function"
      let parameters = `Object [ "type", `String "null" ]
      let input_of_string = Jsonaf.of_string
    end
    in
    let implementation =
      Ochat_function.create_function
        (module Definition)
        (fun input ->
           assert (Jsonaf.exactly_equal input `Null);
           run ();
           Openai.Responses.Tool_output.Output.Text "null")
    in
    Chatmd_shell_spec.Source_ref.digest ("routing hook v1:" ^ name), implementation
  in
  C.create
    ~owner:"routing-hooks"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "routing hooks v1")
    [ registration "observe" observe
    ; registration "revoke" revoke
    ; registration "explode" explode
    ]
  |> Result.map_error ~f:(fun error -> error.C.message)
  |> Result.ok_or_failwith
;;

let routing_hook_tools actor (input : Agent_session.Operation_worker.Input.t) registry =
  let policy =
    permission_policy
      ~tool_default:Allow
      ~fallback:Fallback_deny
      ~evaluator:None
      ~reviewer:None
  in
  Agent_session.Script_tool_calls.create
    ~registry:(fun () -> registry)
    ~moderator_names:String.Set.empty
    ~now:P.Timestamp.now
    ~is_halted:(fun () ->
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      state.halted
      || Option.is_some state.failure
      || P.Session.equal_desired_state state.lifecycle.desired Stopped)
    ~requires_active_moderator:(fun _ -> false)
    ~authorize:(fun (invocation : P.Invocation.t) binding ->
      let open Result.Let_syntax in
      let%bind state = Agent_session.Session_actor.state actor in
      let reference = C.reference binding in
      let%bind _ =
        C.resolve registry ~id:reference.id ~fingerprint:reference.fingerprint
        |> Result.map_error ~f:(fun error -> handoff_error error.C.message)
      in
      let parent =
        Option.bind invocation.parent_event ~f:(fun id ->
          List.find
            state.moderator_executions
            ~f:(fun (event : P.Moderator_execution.t) ->
              P.Id.Moderator_execution.equal id event.context.id))
      in
      let%bind () =
        match state.active_operation, parent with
        | Some operation, Some event
          when P.Id.Operation.equal operation.id input.operation.id
               && P.Id.Session.equal invocation.context.session_id input.session_id
               && Int.equal invocation.context.generation input.session_generation
               && Option.exists
                    event.context.operation_id
                    ~f:(P.Id.Operation.equal operation.id)
               && P.Id.Session.equal
                    event.context.session_id
                    invocation.context.session_id
               && Int.equal event.context.generation invocation.context.generation ->
          Ok ()
        | _ -> Error (handoff_error "routing native hook owner changed")
      in
      let request : Agent_session.Permission_policy.invocation =
        { tool_name = reference.name
        ; identity_digest = C.permission_fingerprint binding
        ; invocation_display = Jsonaf.to_string invocation.context.input
        ; effects = []
        }
      in
      match
        Agent_session.Permission_policy.decide policy ~responder_available:false request
      with
      | Allow_now -> Ok ()
      | Deny_now _ | Request_permission | Request_review ->
        Error (handoff_error "routing native hook was not authorized"))
    ~prepare_output:(function
      | Openai.Responses.Tool_output.Output.Text "null" -> Ok `Null
      | _ -> Error (handoff_error "unexpected routing native hook output"))
    ~defer_observation:(fun _ -> Ok ())
;;

let routing_model_invocations (state : Agent_session.Session_state.t) =
  List.filter state.invocations ~f:(fun (invocation : P.Invocation.t) ->
    match invocation.context.origin, invocation.context.call_entry_id with
    | Model, Some _ -> true
    | _ -> false)
;;

let assert_routing_hooks
      (state : Agent_session.Session_state.t)
      ~registry
      ~observe
      ~revoke
      ~explode
  =
  let hooks =
    List.filter state.invocations ~f:(fun (invocation : P.Invocation.t) ->
      match invocation.context.origin, invocation.context.call_entry_id with
      | Model, Some _ -> false
      | _ -> true)
  in
  assert (
    List.length state.invocations
    = List.length (routing_model_invocations state) + observe + revoke + explode);
  List.iter
    [ "observe", observe; "revoke", revoke; "explode", explode ]
    ~f:(fun (name, expected) ->
      assert (
        List.count hooks ~f:(fun (invocation : P.Invocation.t) ->
          String.equal invocation.context.tool_name name)
        = expected));
  List.iter hooks ~f:(fun (invocation : P.Invocation.t) ->
    let binding =
      C.find registry ~name:invocation.context.tool_name
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
    in
    let reference = C.reference binding in
    assert (
      String.equal
        invocation.context.implementation_revision
        reference.implementation_revision);
    assert (
      String.equal invocation.context.capability_fingerprint (C.fingerprint registry));
    assert (P.Invocation.equal_origin invocation.context.origin Moderator);
    assert (Option.is_none invocation.context.call_entry_id);
    assert (Option.is_none invocation.context.provider_call_id);
    assert (Option.is_none invocation.context.parent_invocation);
    assert (Option.is_none invocation.output_entry_id);
    let event_id = Option.value_exn invocation.parent_event in
    let event =
      List.find_exn
        state.moderator_executions
        ~f:(fun (event : P.Moderator_execution.t) ->
          P.Id.Moderator_execution.equal event.context.id event_id)
    in
    assert (P.Id.Session.equal event.context.session_id invocation.context.session_id);
    assert (Int.equal event.context.generation invocation.context.generation);
    assert (Option.is_some event.context.operation_id);
    assert (
      P.Moderator_execution.equal_phase
        event.context.phase
        (if String.equal invocation.context.tool_name "observe"
         then Post_tool_response
         else Pre_tool_call));
    assert (
      Option.exists invocation.observation ~f:(fun observation ->
        P.Invocation.equal_observer observation.observer event.context.source));
    match invocation.status with
    | Resolved (Complete `Null) ->
      assert (not (String.equal invocation.context.tool_name "explode"))
    | Resolved (Fail error) ->
      assert (String.equal invocation.context.tool_name "explode");
      assert (String.equal error.code "invocation.handler_failed");
      assert (not error.retryable);
      assert (Jsonaf.exactly_equal error.details `Null);
      assert (not (String.is_substring error.message ~substring:"private diagnostic"))
    | _ -> assert false)
;;

let assert_routing_queue (state : Agent_session.Session_state.t) ~emitted ~consumed =
  let module Queue = Agent_session.Moderator_queue in
  let snapshot =
    Agent_session.Moderator_checkpoint.decode state.moderator
    |> protocol_ok
    |> Option.value_exn
  in
  let metadata =
    Agent_session.Moderator_checkpoint.queue state.moderator
    |> protocol_ok
    |> Option.value_exn
  in
  Agent_session.Moderator_checkpoint.validate_encoded_capacity state.moderator
  |> protocol_ok;
  Queue.validate_queue metadata snapshot.queued_internal_events |> protocol_ok;
  let index = Option.value_exn state.run_state in
  let source =
    Agent_session.Run_source_installation.captured
      (Agent_session.Run_state.installation index)
      ~generation:state.identity.generation
    |> protocol_ok
  in
  assert (P.Run_source.equal (Queue.source metadata) source);
  assert (Int64.equal source.installation_epoch 1L);
  assert (Int64.equal (Queue.head_ordinal metadata) (Int64.of_int consumed));
  assert (Int64.equal (Queue.next_ordinal metadata) (Int64.of_int emitted));
  assert (List.length (Queue.entries metadata) = emitted - consumed);
  assert (List.length snapshot.queued_internal_events = emitted - consumed);
  let receipts =
    List.filter state.moderator_executions ~f:(fun (event : P.Moderator_execution.t) ->
      Option.is_some event.queue_occurrence)
  in
  assert (List.length receipts = consumed);
  List.iter receipts ~f:(fun (receipt : P.Moderator_execution.t) ->
    let occurrence = Option.value_exn receipt.queue_occurrence in
    assert (Int64.equal (P.Moderator_execution.Queue_occurrence.ordinal occurrence) 0L);
    assert (
      P.Run_source.equal (P.Moderator_execution.Queue_occurrence.source occurrence) source);
    match receipt.status with
    | Completed _ -> ()
    | Running | Failed _ | Interrupted _ -> assert false)
;;

let diagnose_terminal_failure backend ~case_index ~actual ~expected =
  let failures =
    Agent_session.Memory_backend.events_after backend 0L
    |> protocol_ok
    |> List.filter_map ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
      match event.kind with
      | Operation_failed ->
        let operation = Agent_protocol.Operation.of_json event.payload |> protocol_ok in
        (match operation.state with
         | Failed error ->
           Some
             [%sexp
               (error.code : Agent_protocol.Error.code)
             , (String.prefix error.message 8_192 : string)]
         | Starting | Running | Cancelling | Completed | Cancelled | Interrupted _ -> None)
      | _ -> None)
    |> Fn.flip List.take 8
  in
  print_s
    [%sexp
      "moderator routing failure diagnostic"
    , (case_index : int)
    , (actual : int)
    , (expected : int)
    , (failures : Sexp.t list)]
;;

let%test_unit "streamed native and moderator services share pre and post routing" =
  List.iteri
    [ `Success
    ; `Custom
    ; `Invalid
    ; `Rewrite_invalid
    ; `Redirect
    ; `Deny
    ; `Pre_reject
    ; `Pre_fail
    ; `Post_fail
    ; `Revoked
    ; `Halt_wait
    ; `Disclosure
    ; `Mixed
    ; `Pre_end
    ; `Pre_reject_end
    ; `Revoked_before
    ; `Kind_mismatch
    ; `Invalid_json
    ; `Redacted
    ; `Publish_rejected
    ; `Call_save_rejected
    ; `Dispatch_rejected
    ; `Observer_failed
    ; `Moderator_call_save_rejected
    ; `Moderator_dispatch_rejected
    ; `Moderator_observer_failed
    ; `Custom_call_save_rejected
    ; `Custom_dispatch_rejected
    ; `Custom_observer_failed
    ]
    ~f:(fun case_index mode ->
      let calls = ref 0
      and admitted = ref 0
      and post_calls = ref 0
      and requests = ref 0 in
      let halted = ref false in
      let custom =
        match mode with
        | `Custom
        | `Custom_call_save_rejected
        | `Custom_dispatch_rejected
        | `Custom_observer_failed -> true
        | _ -> false
      in
      let mixed =
        match mode with
        | `Mixed -> true
        | _ -> false
      in
      let publication_rejected =
        match mode with
        | `Publish_rejected -> true
        | _ -> false
      in
      let redacted =
        match mode with
        | `Redacted -> true
        | _ -> false
      in
      let post_failed =
        match mode with
        | `Post_fail -> true
        | _ -> false
      in
      let call_save_rejected =
        match mode with
        | `Call_save_rejected | `Moderator_call_save_rejected | `Custom_call_save_rejected
          -> true
        | _ -> false
      in
      let dispatch_rejected =
        match mode with
        | `Dispatch_rejected | `Moderator_dispatch_rejected | `Custom_dispatch_rejected ->
          true
        | _ -> false
      in
      let observer_failed =
        match mode with
        | `Observer_failed | `Moderator_observer_failed | `Custom_observer_failed -> true
        | _ -> false
      in
      let moderator_target =
        match mode with
        | `Moderator_call_save_rejected
        | `Moderator_dispatch_rejected
        | `Moderator_observer_failed -> true
        | _ -> false
      in
      let original_name =
        match mode with
        | `Redirect -> "counter"
        | _ -> if moderator_target then "counter" else "read_file"
      in
      let before_execution_failure =
        call_save_rejected || dispatch_rejected || observer_failed
      in
      let registry = ref (native_registry ~custom calls ~raises:false) in
      let installed_hooks = ref None in
      with_handoff_runtime
        ~reject:(fun next ->
          List.exists
            next.Agent_session.Session_transition.state.invocations
            ~f:(fun (inv : P.Invocation.t) ->
              match inv.context.origin, inv.context.call_entry_id with
              | Model, Some _ ->
                (publication_rejected && Option.is_some inv.output_entry_id)
                ||
                  (match inv.status with
                  | Admitted -> call_save_rejected
                  | Dispatching -> dispatch_rejected
                  | Resolved _ | Published _ -> false)
              | _ -> false))
        ~make_runtime:(fun env actor_ready ->
          let pre =
            match mode with
            | `Invalid | `Invalid_json | `Kind_mismatch ->
              "Task.fail(\"invalid input reached pre hook\")"
            | `Revoked_before ->
              "Task.bind(Tool.call(\"revoke\", `Null), fun ignored -> Task.pure(state))"
            | `Rewrite_invalid ->
              "Task.bind(Tool.rewrite_args(`Null), fun ignored -> Task.pure(state))"
            | `Redirect ->
              "Task.bind(Tool.redirect(\"read_file\", `Object([])), fun ignored -> \
               Task.pure(state))"
            | `Pre_reject ->
              "Task.bind(Tool.reject(\"private rejection\"), fun ignored -> \
               Task.pure(state))"
            | `Pre_reject_end ->
              "Task.bind(Tool.reject(\"private rejection\"), fun ignored -> \
               Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
               Task.pure(state)))"
            | `Pre_fail -> "Task.fail(\"private pre failure\")"
            | `Pre_end ->
              "Task.bind(Runtime.end_session(\"done\"), fun ignored -> Task.pure(state))"
            | _ -> "Task.pure(state)"
          in
          let events =
            "| `Pre_tool_call(c) -> "
            ^ pre
            ^ " | `Post_tool_response(r) -> Task.bind(Tool.call(\"observe\", `Null), fun \
               ignored -> "
            ^ (if post_failed
               then "Task.fail(\"private post failure\")"
               else "Task.pure(state)")
            ^ ")"
            ^ (if observer_failed
               then
                 " | `Item_appended(item) -> (match Json.get_field(item.value, \"type\") \
                  with | `Some(`String(\"function_call\")) -> Task.fail(\"private \
                  observer failure\") | `Some(`String(\"custom_tool_call\")) -> \
                  Task.fail(\"private observer failure\") | _ -> Task.pure(state))"
               else "")
            ^ " | _ -> Task.pure(state)"
          in
          let hooks =
            routing_hook_registry
              ~observe:(fun () -> Int.incr post_calls)
              ~revoke:(fun () ->
                registry
                := C.select !registry ~names:[]
                   |> Result.map_error ~f:(fun error -> error.C.message)
                   |> Result.ok_or_failwith)
              ~explode:(fun () -> assert false)
            |> fun hooks ->
            C.select hooks ~names:[ "observe"; "revoke" ]
            |> Result.map_error ~f:(fun error -> error.C.message)
            |> Result.ok_or_failwith
          in
          installed_hooks := Some hooks;
          let manager, _, definition =
            handoff_definition
              ~events
              ~capability_registry:hooks
              ~moderator_capabilities:
                { Chat_response.Moderation.Capabilities.default with
                  on_tool_call = (fun ~name:_ ~args:_ -> assert false)
                }
              env
          in
          let post_stream ~sw:_ ~inputs =
            Int.incr requests;
            if !requests > 1
            then (
              assert (
                List.exists inputs ~f:(function
                  | Openai.Responses.Item.Function_call_output _
                  | Custom_tool_call_output _ -> true
                  | _ -> false));
              Stdlib.Seq.empty)
            else
              let open Openai.Responses.Response_stream in
              let call ~custom ~name ~payload ~index =
                let item_id = "native-item-" ^ Int.to_string index in
                let call_id = "native-call-" ^ Int.to_string index in
                [ Output_item_added
                    { item =
                        (if custom
                         then
                           Custom_function
                             { name
                             ; input = ""
                             ; call_id
                             ; _type = "custom_tool_call"
                             ; id = Some item_id
                             }
                         else
                           Function_call
                             { name
                             ; arguments = ""
                             ; call_id
                             ; _type = "function_call"
                             ; id = Some item_id
                             ; status = None
                             })
                    ; output_index = index
                    ; type_ = "response.output_item.added"
                    }
                ; (if custom
                   then
                     Custom_tool_call_input_done
                       { input = payload
                       ; item_id
                       ; output_index = index
                       ; type_ = "response.custom_tool_call_input.done"
                       }
                   else
                     Function_call_arguments_done
                       { arguments = payload
                       ; item_id
                       ; output_index = index
                       ; type_ = "response.function_call_arguments.done"
                       })
                ; Output_item_done
                    { item =
                        (if custom
                         then
                           Custom_function
                             { name
                             ; input = payload
                             ; call_id
                             ; _type = "custom_tool_call"
                             ; id = Some item_id
                             }
                         else
                           Function_call
                             { name
                             ; arguments = payload
                             ; call_id
                             ; _type = "function_call"
                             ; id = Some item_id
                             ; status = Some "completed"
                             })
                    ; output_index = index
                    ; type_ = "response.output_item.done"
                    }
                ]
              in
              let initial =
                call
                  ~custom:
                    (match mode with
                     | `Kind_mismatch -> true
                     | _ -> custom)
                  ~name:original_name
                  ~payload:
                    (match mode with
                     | `Invalid -> "null"
                     | `Invalid_json -> "{"
                     | _ -> "{}")
                  ~index:0
              in
              Stdlib.List.to_seq
                (initial
                 @
                 if mixed
                 then call ~custom:false ~name:"counter" ~payload:"null" ~index:1
                 else [])
          in
          let inference =
            Inference_ports.create ~post_stream ~config:Chat_response.Config.default ()
          in
          Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let state = Agent_session.Session_actor.state actor |> protocol_ok in
            let response_dir =
              Eio.Path.(
                Eio.Stdenv.fs env
                / state.spec.workspace_instance.canonical_root.native_path
                / "response")
            in
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
            let script_tools = routing_hook_tools actor input hooks in
            let declared = !registry in
            let dispatch_tool ~input ~capabilities =
              let native =
                Agent_session.Native_tool_dispatch.create
                  ~input
                  ~capabilities
                  ~declared
                  ~registry:(fun () -> !registry)
                  ~now:Agent_protocol.Timestamp.now
                  ~is_halted:(fun () ->
                    !halted
                    || Chat_response.Moderator_manager.is_halted manager
                       |> Result.ok_or_failwith)
                  ~admit:(fun _ _ ->
                    Int.incr admitted;
                    Eio.Fiber.yield ();
                    (match mode with
                     | `Revoked ->
                       registry
                       := Chat_response.Tool_capability.select !registry ~names:[]
                          |> Result.map_error ~f:(fun error ->
                            error.Chat_response.Tool_capability.message)
                          |> Result.ok_or_failwith
                     | `Halt_wait -> halted := true
                     | _ -> ());
                    Ok ())
                  ~prepare_output:(fun _ ->
                    match mode with
                    | `Disclosure -> Error (handoff_error "private disclosure")
                    | _ -> Ok (`String "disclosed"))
              in
              let moderator =
                Agent_session.Moderator_tool_dispatch.create
                  ~script_tools
                  ~definition
                  ~manager
                  ~input
                  ~capabilities
                  ~available_tools:[]
                  ~session_meta:`Null
                  ~now:Agent_protocol.Timestamp.now
                  ~validate_work:(fun _ -> Error "pending disabled")
                  ~admit:(fun _ -> Ok ())
                  ~prepare_outcome:(fun _ -> Ok ())
                  ()
              in
              Chat_response.In_memory_stream.Tool_dispatch.chain [ moderator; native ]
            in
            let tool_tbl = String.Table.create () in
            Hashtbl.set tool_tbl ~key:"read_file" ~data:(fun ~invocation:_ _ ->
              failwith "native adapter fell through");
            let worker =
              Agent_session.Turn_worker.create
                ~moderator_events:(fun ~input:_ ~capabilities ->
                  Agent_session.Moderator_event.foreground_handlers
                    ~script_tools
                    ~capabilities
                    ~manager
                    ~session_meta:`Null
                    ~now:P.Timestamp.now
                    ())
                ~dispatch_tool
                { env
                ; inference_context = inference.context
                ; inference_identity = (fun _ -> inference.identity)
                ; on_inference_attempt = ignore
                ; on_inference_observation = ignore
                ; on_inference_completion = ignore
                ; response_dir
                ; tools = []
                ; tool_tbl
                ; moderator =
                    Some
                      { manager
                      ; session_id = Agent_protocol.Id.Session.to_string input.session_id
                      ; session_meta = `Null
                      ; runtime_policy = Chat_response.Runtime_semantics.default_policy
                      ; event_handlers = None
                      }
                ; permission_profile =
                    permission_policy
                      ~tool_default:
                        (match mode with
                         | `Deny -> Deny
                         | _ -> Allow)
                      ~fallback:Fallback_deny
                      ~evaluator:None
                      ~reviewer:None
                ; review_permission = (fun _ -> assert false)
                ; history_compaction = false
                ; parallel_tool_calls = true
                ; agent_page_classifications = []
                ; delegated_permission_tools = String.Set.empty
                ; redact_tool_payload =
                    (fun ~name:_ value -> if redacted then "\"hidden\"" else value)
                }
            in
            Agent_session.Operation_worker.run worker ~sw ~input caps)
          |> fun worker ->
          Handoff_runtime.Moderator
            { worker; manager; inference = Inference_ports.execution inference })
        (fun _env actor _writer backend ->
           let rec finished () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if Option.is_none state.active_operation
             then state
             else (
               Eio.Fiber.yield ();
               finished ())
           in
           let state = finished () in
           let model_invocations = routing_model_invocations state in
           let expected_count =
             if call_save_rejected then 0 else if mixed then 2 else 1
           in
           if List.length model_invocations <> expected_count
           then
             failwithf
               "invocation count: expected %d, got %d (save=%b dispatch=%b observer=%b \
                moderator=%b requests=%d)"
               expected_count
               (List.length model_invocations)
               call_save_rejected
               dispatch_rejected
               observer_failed
               moderator_target
               !requests
               ();
           if call_save_rejected
           then (
             assert (!calls = 0 && !admitted = 0 && !post_calls = 0 && !requests = 1);
             assert_routing_hooks
               state
               ~registry:(Option.value_exn !installed_hooks)
               ~observe:0
               ~revoke:0
               ~explode:0;
             assert (List.length state.conversation.canonical_history = 1);
             assert_same_session_snapshot
               state
               (Agent_session.Memory_backend.state backend))
           else (
             let native =
               List.find_exn state.invocations ~f:(fun invocation ->
                 String.equal
                   invocation.context.tool_name
                   (if moderator_target then "counter" else "read_file"))
             in
             let expected =
               match mode with
               | `Success
               | `Custom
               | `Redirect
               | `Post_fail
               | `Mixed
               | `Redacted
               | `Publish_rejected -> None
               | `Invalid | `Rewrite_invalid | `Invalid_json | `Kind_mismatch ->
                 Some "invocation.invalid_input"
               | `Deny -> Some "invocation.permission_denied"
               | `Pre_reject | `Pre_reject_end -> Some "invocation.pre_tool_rejected"
               | `Pre_fail -> Some "invocation.pre_tool_failed"
               | `Revoked | `Revoked_before -> Some "invocation.stale_binding"
               | `Halt_wait | `Pre_end -> Some "invocation.session_ended"
               | `Disclosure -> Some "invocation.disclosure_rejected"
               | `Call_save_rejected
               | `Moderator_call_save_rejected
               | `Custom_call_save_rejected -> assert false
               | `Dispatch_rejected
               | `Observer_failed
               | `Moderator_dispatch_rejected
               | `Moderator_observer_failed
               | `Custom_dispatch_rejected
               | `Custom_observer_failed -> Some "interrupted"
             in
             (match native.status, expected with
              | Published (Complete (`String "disclosed")), None -> ()
              | Resolved (Complete (`String "disclosed")), None ->
                assert publication_rejected
              | Published (Fail error), Some code -> assert (String.equal error.code code)
              | Published (Cancelled _), Some "interrupted" -> ()
              | _ -> assert false);
             let executed =
               match mode with
               | `Disclosure -> true
               | _ -> Option.is_none expected
             in
             assert (!calls = if executed then 1 else 0);
             assert (
               !admitted
               =
               if
                 before_execution_failure
                 ||
                 match mode with
                 | `Invalid
                 | `Invalid_json
                 | `Kind_mismatch
                 | `Revoked_before
                 | `Rewrite_invalid
                 | `Pre_reject
                 | `Pre_reject_end
                 | `Pre_fail
                 | `Pre_end -> true
                 | _ -> false
               then 0
               else 1);
             let expected_post_calls =
               if
                 before_execution_failure
                 ||
                 match mode with
                 | `Pre_end | `Pre_reject_end | `Publish_rejected -> true
                 | _ -> false
               then 0
               else if mixed
               then 2
               else 1
             in
             if !post_calls <> expected_post_calls
             then
               diagnose_terminal_failure
                 backend
                 ~case_index
                 ~actual:!post_calls
                 ~expected:expected_post_calls;
             assert (!post_calls = expected_post_calls);
             assert_routing_hooks
               state
               ~registry:(Option.value_exn !installed_hooks)
               ~observe:expected_post_calls
               ~revoke:
                 (match mode with
                  | `Revoked_before -> 1
                  | _ -> 0)
               ~explode:0;
             if mixed then assert_routing_queue state ~emitted:1 ~consumed:1;
             assert (
               !requests
               =
               if
                 before_execution_failure
                 ||
                 match mode with
                 | `Post_fail | `Publish_rejected | `Pre_end | `Pre_reject_end -> true
                 | _ -> false
               then 1
               else 2);
             assert (
               List.length state.conversation.canonical_history
               = if mixed then 5 else if publication_rejected then 2 else 3);
             let routing = Option.value_exn native.routing in
             assert (String.equal routing.original_name original_name);
             if redacted
             then (
               let canonical = Option.value_exn routing.canonical_payload in
               assert (
                 String.equal
                   canonical.sha256
                   (Chatmd_shell_spec.Source_ref.digest "\"hidden\""));
               assert (
                 String.equal
                   routing.final_payload.sha256
                   (Chatmd_shell_spec.Source_ref.digest "{}")));
             let failures =
               Agent_session.Memory_backend.events_after backend 0L
               |> protocol_ok
               |> List.count ~f:(fun (event : P.Event.Durable.t) ->
                 Agent_protocol.Event.Durable.equal_kind event.kind Operation_failed)
             in
             assert (
               failures
               =
               if before_execution_failure || post_failed || publication_rejected
               then 1
               else 0);
             assert (Bool.equal (Option.is_some state.failure) publication_rejected);
             assert_same_session_snapshot
               state
               (Agent_session.Memory_backend.state backend))))
;;

let%test_unit
    "streamed moderator tools use actor publication and preserve post-hook failures"
  =
  List.iteri
    [ `Success
    ; `Deny
    ; `Disclosure
    ; `Post_fail
    ; `Publish_rejected
    ; `Invalid_json
    ; `Redirect
    ; `Redirect_bad
    ; `Revoked
    ; `End_session
    ; `Unhandled
    ; `Duplicate
    ; `Wrong_id
    ; `Invalid_output
    ; `Forged_error
    ; `Result_rejected
    ; `Pre_reject
    ; `Pre_reject_end
    ; `Pre_reject_post_fail
    ; `Custom_success
    ; `Pre_reject_custom
    ; `Original_invalid
    ; `Custom_invalid
    ; `Rewrite_bad
    ; `Rewrite_ok
    ; `Redacted_input
    ; `Pre_fail
    ; `Pre_host_exception
    ; `Pre_invalid_action
    ; `Pre_custom_fail
    ; `Original_array_limit
    ; `Original_depth_limit
    ; `Original_bytes_limit
    ; `Custom_bytes_limit
    ; `Rewrite_limit
    ; `Redirect_limit
    ; `Pre_end_multi
    ; `Pre_reject_end_multi
    ; `End_session_multi
    ]
    ~f:(fun case_index mode ->
      let request_count = ref 0 in
      let admitted = ref 0 in
      let host_calls = ref 0 in
      let live_snapshot = ref None in
      let native_calls = ref 0 in
      let multi =
        List.mem
          [ `Pre_end_multi; `Pre_reject_end_multi; `End_session_multi ]
          mode
          ~equal:Poly.equal
      in
      let pre_end = Poly.equal mode `Pre_end_multi in
      let implementation_end =
        Poly.equal mode `End_session || Poly.equal mode `End_session_multi
      in
      let pre_failed =
        List.mem
          [ `Pre_fail; `Pre_host_exception; `Pre_invalid_action; `Pre_custom_fail ]
          mode
          ~equal:Poly.equal
      in
      let redirected =
        List.mem [ `Redirect; `Redirect_bad; `Redirect_limit ] mode ~equal:Poly.equal
      in
      let rewritten =
        List.mem [ `Rewrite_bad; `Rewrite_ok; `Rewrite_limit ] mode ~equal:Poly.equal
      in
      let original_limit =
        List.mem
          [ `Original_array_limit
          ; `Original_depth_limit
          ; `Original_bytes_limit
          ; `Custom_bytes_limit
          ]
          mode
          ~equal:Poly.equal
      in
      let final_limit =
        Poly.equal mode `Rewrite_limit || Poly.equal mode `Redirect_limit
      in
      let over_array = `Array (List.init 257 ~f:(fun _ -> `Null)) in
      let array_expression =
        "`Array([" ^ String.concat ~sep:"," (List.init 257 ~f:(fun _ -> "`Null")) ^ "])"
      in
      let original_payload =
        match mode with
        | `Invalid_json -> "[broken"
        | `Original_invalid -> "\"wrong\""
        | `Original_array_limit -> Jsonaf.to_string over_array
        | `Original_depth_limit ->
          Jsonaf.to_string
            (List.fold (List.init 17 ~f:Fn.id) ~init:`Null ~f:(fun value _ ->
               `Array [ value ]))
        | `Original_bytes_limit ->
          Jsonaf.to_string (`String (String.make (256 * 1024) 'x'))
        | `Custom_bytes_limit -> String.make (256 * 1024) 'x'
        | _ -> if redirected then "{}" else "null"
      in
      let invalid_original =
        original_limit
        || List.mem
             [ `Invalid_json; `Original_invalid; `Custom_invalid ]
             mode
             ~equal:Poly.equal
      in
      let pre_rejected =
        List.mem
          [ `Pre_reject
          ; `Pre_reject_end
          ; `Pre_reject_post_fail
          ; `Pre_reject_custom
          ; `Pre_reject_end_multi
          ]
          mode
          ~equal:Poly.equal
      in
      let ends_session = implementation_end || Poly.equal mode `Pre_reject_end || multi in
      let custom =
        Poly.equal mode `Custom_success
        || Poly.equal mode `Pre_reject_custom
        || Poly.equal mode `Custom_invalid
        || Poly.equal mode `Pre_custom_fail
        || Poly.equal mode `Custom_bytes_limit
      in
      let post_fails =
        Poly.equal mode `Post_fail || Poly.equal mode `Pre_reject_post_fail
      in
      let installed_hooks = ref None in
      with_handoff_runtime
        ~reject:(fun next ->
          List.exists
            next.Agent_session.Session_transition.state.invocations
            ~f:(fun (inv : P.Invocation.t) ->
              match inv.context.origin, inv.context.call_entry_id with
              | Model, Some _ ->
                (Poly.equal mode `Publish_rejected && Option.is_some inv.output_entry_id)
                || (Poly.equal mode `Result_rejected
                    &&
                      (match inv.status with
                      | Resolved (Complete _) -> true
                      | _ -> false))
              | _ -> false))
        ~make_runtime:(fun env actor_ready ->
          let events =
            if pre_end
            then
              "| `Pre_tool_call(c) -> Task.bind(Runtime.end_session(\"done\"), fun \
               ignored -> Task.pure(state)) | _ -> Task.pure(state)"
            else if pre_failed
            then
              "| `Pre_tool_call(c) -> let ignored = state[0] <- 99 in "
              ^ "Task.bind(Runtime.emit(`String(\"uncommitted\")), fun ignored -> "
              ^ "Task.bind(Turn.prepend_system(\"uncommitted\"), fun ignored -> "
              ^ (if Poly.equal mode `Pre_host_exception
                 then
                   "Task.bind(Tool.call(\"explode\", `Null), fun result -> match result \
                    with | `Error(code) -> Task.fail(code) | `Ok(_) -> \
                    Task.fail(\"expected native failure\"))"
                 else if Poly.equal mode `Pre_invalid_action
                 then
                   "Task.bind(Tool.reject(\"rejected\"), fun ignored -> \
                    Task.bind(Tool.redirect(\"counter\", `Null), fun ignored -> \
                    Task.pure(state)))"
                 else "Task.fail(\"private diagnostic\")")
              ^ ")) | _ -> Task.pure(state)"
            else if invalid_original
            then
              "| `Pre_tool_call(c) -> Task.fail(\"invalid input reached pre handler\") | \
               _ -> Task.pure(state)"
            else if rewritten
            then
              "| `Pre_tool_call(c) -> Task.bind(Tool.rewrite_args("
              ^ (if final_limit
                 then array_expression
                 else if Poly.equal mode `Rewrite_bad
                 then "`String(\"wrong\")"
                 else "`Null")
              ^ "), fun ignored -> Task.pure(state)) | _ -> Task.pure(state)"
            else if pre_rejected
            then
              "| `Pre_tool_call(c) -> Task.bind(Tool.reject(\"private diagnostic\"), fun \
               ignored -> "
              ^ (if
                   Poly.equal mode `Pre_reject_end
                   || Poly.equal mode `Pre_reject_end_multi
                 then
                   "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                    Task.pure(state)))"
                 else "Task.pure(state))")
              ^ (if post_fails
                 then
                   " | `Post_tool_response(r) -> let ignored = state[0] <- 99 in \
                    Task.fail(\"post hook failed\")"
                 else "")
              ^ " | _ -> Task.pure(state)"
            else if Poly.equal mode `Post_fail
            then
              "| `Post_tool_response(r) -> let ignored = state[0] <- 99 in \
               Task.fail(\"post hook failed\") | _ -> Task.pure(state)"
            else if redirected
            then
              "| `Pre_tool_call(c) -> Task.bind(Tool.redirect(\"counter\", "
              ^ (if final_limit
                 then array_expression
                 else if Poly.equal mode `Redirect_bad
                 then "`String(\"wrong\")"
                 else "`Null")
              ^ "), fun ignored -> Task.pure(state)) | _ -> Task.pure(state)"
            else "| _ -> Task.pure(state)"
          in
          let hooks =
            routing_hook_registry
              ~observe:(fun () -> assert false)
              ~revoke:(fun () -> assert false)
              ~explode:(fun () ->
                Int.incr host_calls;
                failwith "private diagnostic from host")
            |> fun hooks ->
            C.select hooks ~names:[ "explode" ]
            |> Result.map_error ~f:(fun error -> error.C.message)
            |> Result.ok_or_failwith
          in
          installed_hooks := Some hooks;
          let manager, _, definition =
            handoff_definition
              ~capability_registry:hooks
              ~events
                (* The observing moderator must be able to inspect the original
                   oversized request and construct rewritten arguments. Keep the
                   callee's stricter declaration limits below so these cases
                   reach original/final input validation, rather than exhausting
                   the observer while projecting its history/event. *)
              ?execution_policy:
                (Option.some_if
                   (original_limit || final_limit)
                   (Chatml_execution.Bounded
                      { Chatml_execution.default_limits with
                        max_array_items = 512
                      ; max_depth = 128
                      ; max_value_bytes = 1024 * 1024
                      }))
              ~script_limits:
                (if original_limit || final_limit
                 then {|max_array_items="256" max_depth="32" max_value="256KiB"|}
                 else "")
              ~moderator_capabilities:
                { Chat_response.Moderation.Capabilities.default with
                  on_tool_call = (fun ~name:_ ~args:_ -> assert false)
                }
              ~schema:
                (if original_limit || final_limit
                 then "true"
                 else if
                   redirected
                   || rewritten
                   || invalid_original
                   || Poly.equal mode `Invalid_output
                 then "{\"type\":\"null\"}"
                 else "true")
              ~resolve:
                (match mode with
                 | `Unhandled -> "Task.pure(())"
                 | `Duplicate ->
                   "Task.bind(Invocation.resolve(p.context.invocation_id, \
                    `Complete(`Null)), fun ignored -> \
                    Invocation.resolve(p.context.invocation_id, `Complete(`Null)))"
                 | `Wrong_id -> "Invocation.resolve(\"other\", `Complete(`Null))"
                 | `Invalid_output ->
                   "Invocation.resolve(p.context.invocation_id, \
                    `Complete(`String(\"wrong\")))"
                 | `Forged_error ->
                   "Task.fail(\"invocation.unhandled: private diagnostic\")"
                 | _ -> "Invocation.resolve(p.context.invocation_id, `Complete(`Null))")
              ~finish:
                (if implementation_end
                 then
                   "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                    Task.pure(state))"
                 else "Task.pure(state)")
              env
          in
          live_snapshot
          := Some
               (fun () ->
                 Chat_response.Moderator_manager.identity_snapshot manager
                 |> Result.ok_or_failwith);
          let post_stream ~sw:_ ~inputs =
            Int.incr request_count;
            if !request_count = 1
            then (
              let initial =
                Stdlib.List.to_seq
                  Openai.Responses.Response_stream.
                    [ Output_item_added
                        { item =
                            (if custom
                             then
                               Custom_function
                                 { name = "counter"
                                 ; input = ""
                                 ; call_id = "counter-call"
                                 ; _type = "custom_tool_call"
                                 ; id = Some "counter-item"
                                 }
                             else
                               Function_call
                                 { name = (if redirected then "alias" else "counter")
                                 ; arguments = ""
                                 ; call_id = "counter-call"
                                 ; _type = "function_call"
                                 ; id = Some "counter-item"
                                 ; status = None
                                 })
                        ; output_index = 0
                        ; type_ = "response.output_item.added"
                        }
                    ; (if custom
                       then
                         Custom_tool_call_input_done
                           { input = original_payload
                           ; item_id = "counter-item"
                           ; output_index = 0
                           ; type_ = "response.custom_tool_call_input.done"
                           }
                       else
                         Function_call_arguments_done
                           { arguments = original_payload
                           ; item_id = "counter-item"
                           ; output_index = 0
                           ; type_ = "response.function_call_arguments.done"
                           })
                    ; Output_item_done
                        { item =
                            (if custom
                             then
                               Custom_function
                                 { name = "counter"
                                 ; input = original_payload
                                 ; call_id = "counter-call"
                                 ; _type = "custom_tool_call"
                                 ; id = Some "counter-item"
                                 }
                             else
                               Function_call
                                 { name = (if redirected then "alias" else "counter")
                                 ; arguments = original_payload
                                 ; call_id = "counter-call"
                                 ; _type = "function_call"
                                 ; id = Some "counter-item"
                                 ; status = Some "completed"
                                 })
                        ; output_index = 0
                        ; type_ = "response.output_item.done"
                        }
                    ]
              in
              if not multi
              then initial
              else
                Stdlib.Seq.append initial (fun () ->
                  (* Force later provider items to arrive after the first handler
                   has halted, including the asynchronous implementation path. *)
                  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                    let rec halted () =
                      if
                        Chat_response.Moderator_manager.is_halted manager
                        |> Result.ok_or_failwith
                      then ()
                      else (
                        Eio.Fiber.yield ();
                        halted ())
                    in
                    halted ());
                  let open Openai.Responses.Response_stream in
                  let message =
                    match worker_output_item with
                    | Openai.Responses.Item.Output_message message -> message
                    | _ -> assert false
                  in
                  Stdlib.List.to_seq
                    [ Output_item_added
                        { item =
                            Custom_function
                              { name = "counter"
                              ; input = ""
                              ; call_id = "later-custom"
                              ; _type = "custom_tool_call"
                              ; id = Some "later-custom-item"
                              }
                        ; output_index = 1
                        ; type_ = "response.output_item.added"
                        }
                    ; Custom_tool_call_input_done
                        { input = "null"
                        ; item_id = "later-custom-item"
                        ; output_index = 1
                        ; type_ = "response.custom_tool_call_input.done"
                        }
                    ; Output_item_done
                        { item =
                            Custom_function
                              { name = "counter"
                              ; input = "null"
                              ; call_id = "later-custom"
                              ; _type = "custom_tool_call"
                              ; id = Some "later-custom-item"
                              }
                        ; output_index = 1
                        ; type_ = "response.output_item.done"
                        }
                    ; Output_item_added
                        { item =
                            Function_call
                              { name = "native"
                              ; arguments = ""
                              ; call_id = "later-native"
                              ; _type = "function_call"
                              ; id = Some "later-native-item"
                              ; status = None
                              }
                        ; output_index = 2
                        ; type_ = "response.output_item.added"
                        }
                    ; Function_call_arguments_done
                        { arguments = "null"
                        ; item_id = "later-native-item"
                        ; output_index = 2
                        ; type_ = "response.function_call_arguments.done"
                        }
                    ; Output_item_done
                        { item =
                            Function_call
                              { name = "native"
                              ; arguments = "null"
                              ; call_id = "later-native"
                              ; _type = "function_call"
                              ; id = Some "later-native-item"
                              ; status = Some "completed"
                              }
                        ; output_index = 2
                        ; type_ = "response.output_item.done"
                        }
                    ; Output_item_done
                        { item = Output_message message
                        ; output_index = 3
                        ; type_ = "response.output_item.done"
                        }
                    ]
                    ()))
            else (
              assert (
                List.exists inputs ~f:(function
                  | Openai.Responses.Item.Function_call_output _ -> not custom
                  | Custom_tool_call_output _ -> custom
                  | _ -> false));
              Seq.empty)
          in
          let inference =
            Inference_ports.create ~post_stream ~config:Chat_response.Config.default ()
          in
          Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let state = Agent_session.Session_actor.state actor |> protocol_ok in
            let response_dir =
              Eio.Path.(
                Eio.Stdenv.fs env
                / state.spec.workspace_instance.canonical_root.native_path
                / "response")
            in
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
            let script_tools = routing_hook_tools actor input hooks in
            let dispatch_tool ~input ~capabilities =
              Agent_session.Moderator_tool_dispatch.create
                ~script_tools
                ~definition
                ~manager
                ~input
                ~capabilities
                ~available_tools:[]
                ~session_meta:`Null
                ~now:Agent_protocol.Timestamp.now
                ~validate_work:(fun _ -> Error "no pending work")
                ~admit:(fun request ->
                  Int.incr admitted;
                  assert (String.equal request.name "counter");
                  if redirected
                  then (
                    assert (String.equal request.original_name "alias");
                    assert (String.equal request.original_payload "{}");
                    assert (String.equal request.payload "null"));
                  if Poly.equal mode `Revoked
                  then Error "capability was revoked"
                  else Ok ())
                ~prepare_outcome:(fun _ ->
                  if Poly.equal mode `Disclosure then Error "blocked" else Ok ())
                ()
            in
            let worker =
              let tool_tbl = String.Table.create () in
              Hashtbl.set tool_tbl ~key:"native" ~data:(fun ~invocation:_ _ ->
                Int.incr native_calls;
                Openai.Responses.Tool_output.Output.Text "unexpected execution");
              Agent_session.Turn_worker.create
                ~moderator_events:(fun ~input:_ ~capabilities ->
                  Agent_session.Moderator_event.foreground_handlers
                    ~script_tools
                    ~capabilities
                    ~manager
                    ~session_meta:`Null
                    ~now:P.Timestamp.now
                    ())
                ~dispatch_tool
                { env
                ; inference_context = inference.context
                ; inference_identity = (fun _ -> inference.identity)
                ; on_inference_attempt = ignore
                ; on_inference_observation = ignore
                ; on_inference_completion = ignore
                ; response_dir
                ; tools = []
                ; tool_tbl
                ; moderator =
                    Some
                      { manager
                      ; session_id = Agent_protocol.Id.Session.to_string input.session_id
                      ; session_meta = `Null
                      ; runtime_policy = Chat_response.Runtime_semantics.default_policy
                      ; event_handlers = None
                      }
                ; permission_profile =
                    permission_policy
                      ~tool_default:(if Poly.equal mode `Deny then Deny else Allow)
                      ~fallback:Fallback_deny
                      ~evaluator:None
                      ~reviewer:None
                ; review_permission = (fun _ -> assert false)
                ; history_compaction = false
                ; parallel_tool_calls = true
                ; agent_page_classifications = []
                ; delegated_permission_tools = String.Set.empty
                ; redact_tool_payload =
                    (fun ~name:_ value ->
                      if Poly.equal mode `Redacted_input then "\"redacted\"" else value)
                }
            in
            Agent_session.Operation_worker.run worker ~sw ~input caps)
          |> fun worker ->
          Handoff_runtime.Moderator
            { worker; manager; inference = Inference_ports.execution inference })
        (fun _env actor writer backend ->
           let rec finished () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if Option.is_some state.active_operation
             then (
               Eio.Fiber.yield ();
               finished ())
             else state
           in
           let state = finished () in
           let model_invocations = routing_model_invocations state in
           assert (!native_calls = 0);
           if Poly.equal mode `Publish_rejected
           then (
             assert (Option.is_some state.failure);
             assert (
               match state.lifecycle.observed with
               | Failed _ -> true
               | _ -> false);
             let entry =
               let id =
                 History_entry.Id.create ~namespace:"rejected-next-turn" ~sequence:0
                 |> Result.ok_or_failwith
               in
               Agent_session.History_codec.user_text ~id "must not start"
               |> Agent_session.History_codec.to_protocol
             in
             assert (
               Result.is_error
                 (Agent_session.Session_actor.submit_message
                    ~submitting_principal:principal_id
                    actor
                    ~attachment_id:writer.id
                    entry));
             let after = Agent_session.Session_actor.state actor |> protocol_ok in
             assert_same_session_snapshot state after);
           let expected_invocations = if multi then 2 else 1 in
           if List.length model_invocations <> expected_invocations
           then
             diagnose_terminal_failure
               backend
               ~case_index
               ~actual:(List.length model_invocations)
               ~expected:expected_invocations;
           assert (List.length model_invocations = expected_invocations);
           let invocation =
             List.find_exn state.invocations ~f:(fun inv ->
               Option.equal
                 String.equal
                 inv.context.provider_call_id
                 (Some "counter-call"))
           in
           if multi
           then (
             let later =
               List.find_exn state.invocations ~f:(fun inv ->
                 Option.equal
                   String.equal
                   inv.context.provider_call_id
                   (Some "later-custom"))
             in
             assert (Option.is_some later.output_entry_id);
             assert (
               Poly.equal
                 (Option.value_exn later.routing).preparation
                 Agent_protocol.Invocation.Session_ended);
             match later.status with
             | Published (Fail error) ->
               assert (String.equal error.code "invocation.session_ended")
             | _ -> assert false);
           let routing = Option.value_exn invocation.routing in
           let final_payload =
             if final_limit
             then Jsonaf.to_string over_array
             else if Poly.equal mode `Redirect_bad || Poly.equal mode `Rewrite_bad
             then "\"wrong\""
             else if redirected || rewritten
             then "null"
             else original_payload
           in
           let fingerprint payload =
             Agent_protocol.Invocation.
               { sha256 = Chatmd_shell_spec.Source_ref.digest payload
               ; byte_length = String.length payload
               }
           in
           assert (
             String.equal
               routing.original_name
               (if redirected then "alias" else "counter"));
           assert (
             Poly.equal
               routing.kind
               (if custom then Agent_protocol.Invocation.Custom else Function));
           assert (Poly.equal routing.original_payload (fingerprint original_payload));
           assert (Poly.equal routing.final_payload (fingerprint final_payload));
           assert (
             Poly.equal
               routing.canonical_payload
               (Some
                  (fingerprint
                     (if Poly.equal mode `Redacted_input
                      then "\"redacted\""
                      else final_payload))));
           let expected_preparation =
             if pre_end
             then Agent_protocol.Invocation.Session_ended
             else if invalid_original
             then Agent_protocol.Invocation.Invalid_input
             else if pre_rejected
             then Pre_tool_rejected
             else if pre_failed
             then Pre_tool_failed
             else Passed
           in
           if not (Poly.equal routing.preparation expected_preparation)
           then
             failwithf
               "unexpected preparation for input %s/%d: %s, expected %s"
               (Chatmd_shell_spec.Source_ref.digest original_payload)
               (String.length original_payload)
               (Sexp.to_string
                  (Agent_protocol.Invocation.sexp_of_preparation routing.preparation))
               (Sexp.to_string
                  (Agent_protocol.Invocation.sexp_of_preparation expected_preparation))
               ();
           let expected_count =
             if
               Poly.equal mode `Success
               || Poly.equal mode `Custom_success
               || Poly.equal mode `Rewrite_ok
               || Poly.equal mode `Redacted_input
               || Poly.equal mode `Post_fail
               || Poly.equal mode `Publish_rejected
               || Poly.equal mode `Redirect
               || Poly.equal mode `End_session
               || Poly.equal mode `End_session_multi
             then 1
             else 0
           in
           let saved_snapshot =
             Agent_session.Moderator_checkpoint.decode state.moderator
             |> protocol_ok
             |> Option.value_exn
           in
           let live = (Option.value_exn !live_snapshot) () in
           assert (
             Sexp.equal
               (Session.Moderator_state.Identity_snapshot.sexp_of_t live)
               (Session.Moderator_state.Identity_snapshot.sexp_of_t saved_snapshot));
           let consumed =
             if
               expected_count = 1
               && (not ends_session)
               && (not post_fails)
               &&
               match mode with
               | `Publish_rejected -> false
               | _ -> true
             then 1
             else 0
           in
           assert_routing_queue state ~emitted:expected_count ~consumed;
           assert (!host_calls = if Poly.equal mode `Pre_host_exception then 1 else 0);
           assert_routing_hooks
             state
             ~registry:(Option.value_exn !installed_hooks)
             ~observe:0
             ~revoke:0
             ~explode:
               (match mode with
                | `Pre_host_exception -> 1
                | _ -> 0);
           if pre_failed
           then (
             assert (List.is_empty live.prepended_items);
             assert (List.is_empty live.queued_internal_events));
           assert (
             Poly.equal
               saved_snapshot.current_state
               (Session.Snapshot.Array [ Int expected_count ]));
           assert (
             Bool.equal
               (Option.is_some invocation.output_entry_id)
               (not (Poly.equal mode `Publish_rejected)));
           let valid_outcome =
             match invocation.status with
             | Published (Complete `Null) ->
               Poly.equal mode `Success
               || Poly.equal mode `Custom_success
               || Poly.equal mode `Rewrite_ok
               || Poly.equal mode `Redacted_input
               || Poly.equal mode `Post_fail
               || Poly.equal mode `Redirect
               || Poly.equal mode `End_session
               || Poly.equal mode `End_session_multi
             | Published (Fail error) ->
               let expected =
                 match mode with
                 | `Deny | `Revoked -> "invocation.permission_denied"
                 | `Disclosure -> "invocation.disclosure_rejected"
                 | `Invalid_json
                 | `Redirect_bad
                 | `Original_invalid
                 | `Custom_invalid
                 | `Original_array_limit
                 | `Original_depth_limit
                 | `Original_bytes_limit
                 | `Custom_bytes_limit
                 | `Rewrite_limit
                 | `Redirect_limit
                 | `Rewrite_bad -> "invocation.invalid_input"
                 | `Unhandled -> "invocation.unhandled"
                 | `Duplicate -> "invocation.duplicate_resolution"
                 | `Wrong_id -> "invocation.wrong_id"
                 | `Invalid_output -> "invocation.invalid_output"
                 | `Forged_error -> "invocation.handler_failed"
                 | `Result_rejected -> "invocation.commit_failed"
                 | `Pre_fail
                 | `Pre_host_exception
                 | `Pre_invalid_action
                 | `Pre_custom_fail -> "invocation.pre_tool_failed"
                 | `Pre_reject
                 | `Pre_reject_end
                 | `Pre_reject_post_fail
                 | `Pre_reject_custom -> "invocation.pre_tool_rejected"
                 | `Pre_reject_end_multi -> "invocation.pre_tool_rejected"
                 | `Pre_end_multi -> "invocation.session_ended"
                 | _ -> assert false
               in
               assert (not error.retryable);
               assert (Poly.equal error.details `Null);
               assert (
                 not (String.is_substring error.message ~substring:"private diagnostic"));
               if not (String.equal error.code expected)
               then
                 failwithf "moderator outcome: expected %s, got %s" expected error.code ();
               true
             | Resolved (Complete `Null) -> Poly.equal mode `Publish_rejected
             | _ -> false
           in
           if not valid_outcome
           then (
             diagnose_terminal_failure
               backend
               ~case_index
               ~actual:(List.length model_invocations)
               ~expected:expected_invocations;
             let event_failures =
               List.filter_map
                 state.moderator_executions
                 ~f:(fun (event : P.Moderator_execution.t) ->
                   match event.status with
                   | Failed error ->
                     Some
                       [%sexp
                         (event.context.phase : P.Moderator_execution.phase)
                       , (error.code : string)
                       , (String.prefix error.message 1024 : string)]
                   | Running | Completed _ | Interrupted _ -> None)
             in
             print_s [%sexp (event_failures : Sexp.t list)];
             failwithf
               "unexpected moderator outcome for input %d bytes: %s"
               (String.length original_payload)
               (Sexp.to_string
                  (Agent_protocol.Invocation.sexp_of_status invocation.status))
               ());
           assert (
             List.length state.conversation.canonical_history
             = if multi then 8 else if Poly.equal mode `Publish_rejected then 2 else 3);
           let failed = post_fails || Poly.equal mode `Publish_rejected in
           assert (!request_count = if failed || ends_session then 1 else 2);
           if ends_session then assert saved_snapshot.halted;
           assert (
             !admitted
             =
             if
               pre_end
               || pre_rejected
               || pre_failed
               || invalid_original
               || final_limit
               || Poly.equal mode `Rewrite_bad
               || Poly.equal mode `Redirect_bad
             then 0
             else 1);
           let events =
             Agent_session.Memory_backend.events_after backend 0L |> protocol_ok
           in
           let failures =
             List.filter events ~f:(fun e ->
               Agent_protocol.Event.Durable.equal_kind e.kind Operation_failed)
           in
           assert (List.length failures = if failed then 1 else 0);
           if post_fails
           then (
             let event = List.hd_exn failures in
             match
               Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
               |> protocol_ok
             with
             | Operation_failed { state = Failed error; _ } ->
               assert (not error.retryable);
               assert (
                 String.is_substring
                   (Jsonaf.to_string error.data)
                   ~substring:"post_tool_response");
               assert (
                 String.is_substring
                   (Jsonaf.to_string error.data)
                   ~substring:
                     (History_entry.Id.to_string
                        (Option.value_exn invocation.output_entry_id)))
             | _ -> assert false);
           assert (
             Poly.equal
               state.invocations
               (Agent_session.Memory_backend.state backend).invocations)))
;;

let%test_unit
    "routed calls recheck revoked policy and release cancelled handlers and waits"
  =
  List.iter [ `Revoked; `Cancelled; `Active_cancel; `Session_ended ] ~f:(fun mode ->
    let done_, done_u = Eio.Promise.create () in
    with_handoff_actor
      ~make_worker:(fun env actor_ready ->
        let manager, _, definition =
          handoff_definition
            ~finish:
              (if Poly.equal mode `Session_ended
               then
                 "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                  Task.pure(state))"
               else "Task.pure(state)")
            env
        in
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let first_held, first_held_u = Eio.Promise.create () in
          let release, release_u = Eio.Promise.create () in
          let cancel, cancel_u = Eio.Promise.create () in
          let cancelled, cancelled_u = Eio.Promise.create () in
          let attempted, attempted_u = Eio.Promise.create () in
          let admitted = ref [] in
          let revoked = ref false in
          let prepared = ref 0 in
          let history = ref input.history in
          let allocate item =
            let id =
              History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
            in
            Openai.Responses_history.create_with_id_exn ~id item
          in
          let request call_id =
            let call =
              allocate
                (Openai.Responses.Item.Function_call
                   { name = "counter"
                   ; arguments = "null"
                   ; call_id
                   ; _type = "function_call"
                   ; id = None
                   ; status = None
                   })
            in
            caps.commit_entry call |> protocol_ok;
            history := !history @ [ call ];
            Chat_response.In_memory_stream.Tool_dispatch.
              { kind = Function
              ; original_name = "counter"
              ; original_payload = "null"
              ; name = "counter"
              ; payload = "null"
              ; rejection = None
              ; call
              ; history = !history
              ; source = None
              ; parent_call_id = None
              }
          in
          let dispatch =
            Agent_session.Moderator_tool_dispatch.create
              ~definition
              ~manager
              ~input
              ~capabilities:caps
              ~available_tools:[]
              ~session_meta:`Null
              ~now:Agent_protocol.Timestamp.now
              ~validate_work:(fun _ -> Error "no pending work")
              ~admit:(fun request ->
                let call_id =
                  match Openai.Responses_history.item_exn request.call with
                  | Function_call c -> c.call_id
                  | _ -> assert false
                in
                admitted := !admitted @ [ call_id ];
                if !revoked then Error "revoked while queued" else Ok ())
              ~prepare_outcome:(fun _ ->
                Int.incr prepared;
                if !prepared = 1
                then (
                  Eio.Promise.resolve first_held_u ();
                  Eio.Promise.await release);
                Ok ())
              ()
          in
          let run request =
            let result = dispatch.run request ~authorize:ignore |> Option.value_exn in
            let call_id =
              match Openai.Responses_history.item_exn request.call with
              | Function_call c -> c.call_id
              | _ -> assert false
            in
            let output =
              allocate
                (Openai.Responses.Item.Function_call_output
                   { output = result.output
                   ; call_id
                   ; _type = "function_call_output"
                   ; id = None
                   ; status = None
                   })
            in
            (Option.value_exn result.commit_output) output;
            history := !history @ [ output ]
          in
          let first = request "first" in
          let second = request "second" in
          Eio.Switch.run (fun sw ->
            Eio.Fiber.fork ~sw (fun () ->
              if Poly.equal mode `Active_cancel
              then (
                let result =
                  Eio.Fiber.first
                    (fun () ->
                       run first;
                       `Completed)
                    (fun () ->
                       Eio.Promise.await cancel;
                       `Cancelled)
                in
                assert (Poly.equal result `Cancelled);
                Eio.Promise.resolve cancelled_u ())
              else run first);
            Eio.Promise.await first_held;
            Eio.Fiber.fork ~sw (fun () ->
              if Poly.equal mode `Cancelled
              then (
                let result =
                  Eio.Fiber.first
                    (fun () ->
                       Eio.Promise.resolve attempted_u ();
                       run second;
                       `Completed)
                    (fun () ->
                       Eio.Promise.await cancel;
                       `Cancelled)
                in
                assert (Poly.equal result `Cancelled);
                Eio.Promise.resolve cancelled_u ())
              else (
                Eio.Promise.resolve attempted_u ();
                run second));
            Eio.Promise.await attempted;
            Eio.Fiber.yield ();
            (* These mailbox requests must remain responsive while the first
               handler owns the moderator and the second call waits. *)
            let queued = Agent_session.Session_actor.state actor |> protocol_ok in
            assert (List.length queued.invocations = 2);
            assert (Poly.equal !admitted [ "first" ]);
            if Poly.equal mode `Cancelled
            then (
              Eio.Promise.resolve cancel_u ();
              Eio.Promise.await cancelled;
              let after = Agent_session.Session_actor.state actor |> protocol_ok in
              assert (List.length after.invocations = 2))
            else if Poly.equal mode `Active_cancel
            then (
              Eio.Promise.resolve cancel_u ();
              Eio.Promise.await cancelled)
            else if Poly.equal mode `Revoked
            then revoked := true;
            Eio.Promise.resolve release_u ());
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          assert (!prepared = if Poly.equal mode `Active_cancel then 2 else 1);
          let module Checkpoint = Agent_session.Moderator_checkpoint in
          let module Queue = Agent_session.Moderator_queue in
          let module P = Agent_protocol in
          let snapshot =
            Chat_response.Moderator_manager.identity_snapshot manager
            |> Result.ok_or_failwith
          in
          let recorded =
            Checkpoint.decode state.moderator |> protocol_ok |> Option.value_exn
          in
          assert (
            Sexp.equal
              (Session.Moderator_state.Identity_snapshot.sexp_of_t recorded)
              (Session.Moderator_state.Identity_snapshot.sexp_of_t snapshot));
          let metadata =
            Checkpoint.queue state.moderator |> protocol_ok |> Option.value_exn
          in
          Queue.validate_queue metadata snapshot.queued_internal_events |> protocol_ok;
          assert (
            Sexp.equal
              ([%sexp_of: Session.Snapshot.t list] snapshot.queued_internal_events)
              ([%sexp_of: Session.Snapshot.t list]
                 [ Variant
                     ("Internal_event", [ Variant ("String", [ String "committed" ]) ])
                 ]));
          let index = Option.value_exn state.run_state in
          let source =
            Agent_session.Run_source_installation.captured
              (Agent_session.Run_state.installation index)
              ~generation:state.identity.generation
            |> protocol_ok
          in
          assert (P.Run_source.equal (Queue.source metadata) source);
          assert (
            P.Invocation.equal_observer
              source.observer
              { script_id = snapshot.script_id
              ; source_sha256 = snapshot.script_source_hash
              });
          let emitted = List.hd_exn (Queue.entries metadata) in
          [%test_eq: int] 1 (List.length (Queue.entries metadata));
          let producer =
            List.find_exn state.invocations ~f:(fun (invocation : P.Invocation.t) ->
              match invocation.status with
              | Published (Complete `Null) -> true
              | Admitted | Dispatching | Resolved _ | Published _ -> false)
          in
          (* This dispatcher creates source-less invocations; the full actual
             installation belongs to their emitted queue occurrence. *)
          assert (Option.is_none producer.observation);
          assert (
            Option.equal
              Agent_session.Queue_producer.equal
              (Queue.Entry.producer emitted)
              (Some (Invocation_at_source { id = producer.context.id; source })));
          assert (
            Option.equal
              P.Inference_origin.equal
              (Queue.Entry.origin emitted)
              producer.inference_origin);
          assert (
            Poly.equal
              (Chat_response.Moderator_manager.identity_snapshot manager
               |> Result.ok_or_failwith)
                .current_state
              (Session.Snapshot.Array [ Int 1 ]));
          let failed =
            List.filter state.invocations ~f:(fun inv ->
              match inv.status with
              | Published (Fail error) ->
                assert (
                  String.equal
                    error.code
                    (if Poly.equal mode `Session_ended
                     then "invocation.session_ended"
                     else "invocation.permission_denied"));
                true
              | Published (Complete `Null) -> false
              | Resolved (Cancelled _) ->
                assert (Poly.equal mode `Active_cancel);
                false
              | Admitted ->
                (match mode with
                 | `Cancelled -> ()
                 | _ -> assert false);
                false
              | _ -> assert false)
          in
          assert (
            List.length failed
            = if Poly.equal mode `Revoked || Poly.equal mode `Session_ended then 1 else 0);
          assert (
            Poly.equal
              !admitted
              (if Poly.equal mode `Cancelled || Poly.equal mode `Session_ended
               then [ "first" ]
               else [ "first"; "second" ]));
          revoked := false;
          run (request "third");
          assert (
            !prepared
            =
            if Poly.equal mode `Session_ended
            then 1
            else if Poly.equal mode `Active_cancel
            then 3
            else 2);
          let snapshot =
            Chat_response.Moderator_manager.identity_snapshot manager
            |> Result.ok_or_failwith
          in
          assert (
            Poly.equal
              snapshot.current_state
              (Session.Snapshot.Array
                 [ Int (if Poly.equal mode `Session_ended then 1 else 2) ]));
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          Eio.Promise.resolve done_u ();
          Completed
            { final_history = !history
            ; runtime_requests = []
            ; moderator_snapshot = state.moderator
            }))
      (fun env actor _writer backend ->
         let state =
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
             await_idle actor)
         in
         let failures =
           Agent_session.Memory_backend.events_after backend 0L
           |> protocol_ok
           |> List.filter_map ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
             match event.kind with
             | Operation_failed ->
               Some (Agent_protocol.Operation.of_json event.payload |> protocol_ok)
             | _ -> None)
         in
         [%test_eq: Sexp.t]
           [%sexp ([] : Agent_protocol.Operation.t list)]
           [%sexp (failures : Agent_protocol.Operation.t list)];
         assert (Eio.Promise.is_resolved done_);
         Eio.Promise.await done_;
         assert (
           Poly.equal
             state.invocations
             (Agent_session.Memory_backend.state backend).invocations)))
;;
