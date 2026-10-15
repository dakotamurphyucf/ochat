open Core
open Fixtures
open Job_fixtures
module B = Chat_response.Background_request
module Calls = Agent_session.Script_tool_calls
module Jobs = Agent_session.Script_job_service
module P = Agent_protocol

let artifact () =
  B.Agent_artifact.create
    ~revision_id:(P.Id.Prompt_revision.create ())
    ~manifest_sha256:
      (Chatml_source.Digest.of_contents "selected-child" ~checkpoint:ignore)
    ~root_relative_path:(Source.Portable_path.create "child.chatmd" |> Or_error.ok_exn)
;;

let host actor ~stage : Jobs.host =
  { stage
  ; select = (fun owner ids -> A.select_background_jobs actor ~owner ~ids)
  ; abort = (fun owner id -> A.abort_background_job actor ~owner ~id |> protocol_ok)
  ; get = (fun owner id -> A.read_script_job actor ~owner ~id)
  ; materialize = (fun owner job -> A.read_script_job_result actor ~owner ~expected:job)
  ; cancel = (fun owner id -> A.cancel_script_job actor ~owner ~id)
  }
;;

(* The source receipt is a fixture reference; this checks the genuine queued
   dispatcher, not artifact installation or native file acquisition. *)
let dispatch env sw actor backend job request tools =
  let module Registry = Agent_server.Session_registry in
  let module Owner = Agent_server.Runtime_owner in
  let module Scheduler = Agent_server.Job_scheduler in
  let policy = Chat_response.One_off_request.default_policy in
  let executor : Agent_session.Runtime_builder.background_executor =
    { policy
    ; now = (fun () -> timestamp)
    ; run =
        (fun ~job
          ~deadline
          ~execute
          ~moderator_execute
          ~claim_event:_
          ~is_halted:_
          ~request ->
          Agent_session.Background_execution.run
            ~env
            ~job
            ~deadline
            ~execute
            ~moderator_execute
            ~request
            ~policy
            ~script_tools:tools
            ~now:(fun () -> timestamp)
            ~moderate_tool:(fun _ _ -> Ok None)
            ~prepare_outcome:(fun _ -> Ok ())
            ())
    }
  in
  let runtime =
    { (Runtime_lease_tests.runtime ~script_tools:tools ~close:ignore ()) with
      background_executor = Some executor
    }
  in
  let owner = Owner.create ~actor ~initial:(Some runtime) ~build:(fun () -> Ok runtime) in
  let registry = Registry.create () in
  let entry : Registry.entry =
    { actor
    ; history_ids =
        Agent_session.History_id_source.create
          ~namespace:"queued-source"
          ~block_size:8
          ~reserve:(fun ~count:_ -> failwith "unexpected history allocation")
        |> protocol_ok
    ; runtime = owner
    ; durable_events =
        Agent_session.Durable_event_log.create ~capacity:128 [] |> protocol_ok
    ; capacity = None
    ; store_handle = None
    ; expire_permissions = (fun ~now:_ -> ())
    ; collect_results = (fun () -> Ok None)
    ; close = ignore
    }
  in
  Registry.add registry ~session_id entry |> protocol_ok;
  let scheduler =
    Scheduler.start
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~registry
      ~developer_registry:None
      ~capacity:
        (Agent_server.Job_capacity.create
           ~limits:
             { daemon_total = 1
             ; per_principal = 1
             ; per_prompt = 1
             ; per_workspace = 1
             ; per_session = 1
             ; per_kind = 1
             ; max_nested_depth = 1
             })
      ~model_job_inference:(fun _ _ -> failwith "unexpected model job")
  in
  Exn.protect
    ~finally:(fun () ->
      Scheduler.close scheduler;
      Owner.close_and_wait owner)
    ~f:(fun () ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
        Background_scheduler_tests.until env (fun () ->
          Background_scheduler_tests.terminal backend job));
      let completed = Background_scheduler_tests.current backend job in
      [%test_eq: int] completed.attempt 1;
      [%test_eq: string]
        (Jsonaf.to_string completed.payload)
        (Jsonaf.to_string (B.to_json request));
      match completed.result with
      | Some result -> P.Completion.of_json result |> protocol_ok
      | None -> failwith "queued source completion was not saved")
;;

let%expect_test
    "selected source reference is staged once and dispatched without preparation"
  =
  with_actor (fun env sw actor _writer backend ->
    let selected = artifact () in
    let preparations = ref 0 in
    let executions = ref 0 in
    let descendant_rejected = ref false in
    let replacement_rejected = ref false in
    let delivery_cancelled = ref false in
    let reauthorization_cancel = ref None in
    let reason = Failure "selected-artifact-delivery-cancelled" in
    let current_registry = ref None in
    let substitute_registry = ref None in
    let escaped = ref None in
    let admission_borrow = ref None in
    let authorizations = ref 0 in
    let before_admission_rejected = ref false in
    let current_authority = ref true in
    let current_rejected = ref false in
    let delayed_reader = Eio.Fiber.create_key () in
    let delayed_checks = ref 0 in
    let delayed_waiting, delayed_waiting_u = Eio.Promise.create () in
    let delayed_release, delayed_release_u = Eio.Promise.create () in
    let delayed_result, delayed_result_u = Eio.Promise.create () in
    let module Definition = struct
      type input = string

      let name = "selected_agent"
      let description = None
      let type_ = "function"
      let parameters = `Object [ "type", `String "object" ]
      let input_of_string input = input
    end
    in
    let implementation =
      Ochat_function.create_function
        (module Definition)
        (fun _ ->
           let services = Calls.current_native_services () |> Result.ok_or_failwith in
           escaped := Some services;
           let admitted =
             Calls.agent_artifact services |> protocol_ok |> Option.value_exn
           in
           [%test_eq: string]
             (P.Id.Prompt_revision.to_string (B.Agent_artifact.revision_id admitted))
             (P.Id.Prompt_revision.to_string (B.Agent_artifact.revision_id selected));
           let borrowed = N.borrow () |> protocol_ok in
           admission_borrow := Some borrowed;
           let parent = N.borrowed_invocation borrowed in
           let ceiling = N.borrowed_capabilities borrowed |> protocol_ok in
           let child =
             I.create
               { parent.context with
                 id = P.Id.Invocation.create ()
               ; origin = Script
               ; parent_invocation = Some parent.context.id
               ; provider_call_id = None
               ; call_entry_id = None
               ; parent_job = None
               }
             |> protocol_ok
           in
           let execute = N.execute_borrowed borrowed in
           execute ~invocation:child (fun ~dispatched ->
             N.with_dispatched_scope
               ~execute
               ~selected:ceiling
               ~invocation:dispatched
               (fun () ->
                  let child_borrow = N.borrow () |> protocol_ok in
                  let binding =
                    C.find ceiling ~name:Definition.name |> Background_execution_tests.cap
                  in
                  assert (
                    Result.is_error (N.require_native_admission child_borrow ~binding));
                  descendant_rejected := Result.is_error (Calls.agent_artifact services);
                  Ok (I.Complete `Null)))
           |> protocol_ok
           |> ignore;
           let original = Option.value_exn !current_registry in
           let replacement = Option.value_exn !substitute_registry in
           let original_binding =
             C.find original ~name:Definition.name |> Background_execution_tests.cap
           in
           let replacement_binding =
             C.find replacement ~name:Definition.name |> Background_execution_tests.cap
           in
           [%test_eq: string]
             (C.permission_fingerprint original_binding)
             (C.permission_fingerprint replacement_binding);
           assert (
             not
               (phys_equal
                  (C.native_implementation original_binding |> Option.value_exn)
                  (C.native_implementation replacement_binding |> Option.value_exn)));
           assert (
             Result.is_error
               (N.require_native_admission borrowed ~binding:replacement_binding));
           current_registry := Some replacement;
           replacement_rejected := Result.is_error (Calls.agent_artifact services);
           current_registry := Some original;
           current_authority := false;
           current_rejected := Result.is_error (Calls.agent_artifact services);
           current_authority := true;
           (try
              Eio.Cancel.sub (fun context ->
                reauthorization_cancel := Some context;
                Calls.agent_artifact services |> ignore;
                failwith "cancelled selected artifact was delivered")
            with
            | Eio.Cancel.Cancelled actual when phys_equal actual reason ->
              delivery_cancelled := true);
           reauthorization_cancel := None;
           Eio.Fiber.fork ~sw (fun () ->
             Eio.Fiber.with_binding delayed_reader true (fun () ->
               let result = Calls.agent_artifact services in
               Eio.Promise.resolve delayed_result_u (Result.is_error result)));
           Eio.Promise.await delayed_waiting;
           Int.incr executions;
           Openai.Responses.Tool_output.Output.Text "selected child")
    in
    let replacement =
      Ochat_function.create_function
        (module Definition)
        (fun _ -> failwith "replacement must not execute")
    in
    substitute_registry
    := Some
         (C.create
            ~owner:"queued-source-owner"
            ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
            [ Chatmd_shell_spec.Source_ref.digest "selected-agent-v1", replacement ]
          |> Background_execution_tests.cap);
    let registry =
      C.create
        ~owner:"queued-source-owner"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
        [ Chatmd_shell_spec.Source_ref.digest "selected-agent-v1", implementation ]
      |> Background_execution_tests.cap
    in
    current_registry := Some registry;
    let staged = ref None in
    let stage owner request =
      let open Result.Let_syntax in
      assert (Int.equal !preparations 1);
      assert (Option.is_some (B.agent_artifact request));
      let%bind job = A.prepare_background_job_launch actor ~owner request in
      let%map () =
        A.stage_background_job actor ~job ~capacity:{ publish = ignore; abort = ignore }
      in
      staged := Some job;
      job
    in
    let source_host = host actor ~stage in
    let service =
      Jobs.create
        ~env
        ~policy:Chat_response.One_off_request.default_policy
        ~current_capabilities:(fun () -> registry)
        ~prepare_source:(fun ~binding ~owner:_ ~request ~checkpoint ->
          checkpoint ();
          assert (
            phys_equal
              (C.native_implementation binding |> Option.value_exn)
              implementation);
          assert (Option.is_none (B.agent_artifact request));
          Eio.Fiber.yield ();
          Int.incr preparations;
          Ok (Some selected))
        ~host:source_host
    in
    let issuing = add_claimed_job actor in
    let invocation = root issuing in
    with_job actor issuing (fun ~job:_ ~execute ->
      execute ~invocation (fun ~dispatched ->
        Jobs.with_scope
          service
          ~owner:(Invocation dispatched.context.id)
          ~selected:registry
          ~error:Agent_protocol.Error.invalid_request
          (fun scope ->
             let open Result.Let_syntax in
             let%bind id =
               (Jobs.moderator_transaction scope).handlers.start_tool
                 ~name:Definition.name
                 ~input:(`Object [])
               |> Result.map_error ~f:Agent_protocol.Error.invalid_request
             in
             let%map () = source_host.select (Invocation dispatched.context.id) [ id ] in
             I.Complete `Null)))
    |> protocol_ok
    |> ignore;
    let staged = Option.value_exn !staged in
    let persisted =
      List.find_exn (Agent_session.Memory_backend.state backend).jobs ~f:(fun job ->
        P.Id.Job.equal staged.id job.id)
    in
    let request =
      B.of_json ~policy:Chat_response.One_off_request.default_policy persisted.payload
      |> protocol_ok
    in
    let tools =
      Calls.create
        ~registry:(fun () -> Option.value_exn !current_registry)
        ~moderator_names:String.Set.empty
        ~now:(fun () -> timestamp)
        ~is_halted:(fun () -> false)
        ~requires_active_moderator:(fun _ -> false)
        ~authorize:(fun _ binding ->
          Int.incr authorizations;
          let borrowed = N.borrow () |> protocol_ok in
          before_admission_rejected
          := Result.is_error (N.require_native_admission borrowed ~binding);
          Ok ())
        ~prepare_output:(function
          | Openai.Responses.Tool_output.Output.Text text ->
            (* The native handler has returned; dispatcher scope/services are
               deliberately still live while output disclosure is held. *)
            Eio.Promise.resolve delayed_release_u ();
            assert (Eio.Promise.await delayed_result);
            Ok (`String text)
          | Content _ -> failwith "unexpected selected child output")
        ~defer_observation:(fun _ -> Ok ())
      |> fun tools ->
      Calls.with_admission_check tools ~check:(fun _ _ ->
        if Option.value (Eio.Fiber.get delayed_reader) ~default:false
        then (
          Int.incr delayed_checks;
          if Int.equal !delayed_checks 2
          then (
            Eio.Promise.resolve delayed_waiting_u ();
            Eio.Promise.await delayed_release));
        Option.iter !reauthorization_cancel ~f:(fun context ->
          Eio.Fiber.yield ();
          Eio.Cancel.cancel context reason);
        if !current_authority
        then Ok ()
        else
          Error
            (P.Error.create
               Permission_denied
               ~message:"current owner revoked"
               ~retryable:false
               ()))
    in
    (match dispatch env sw actor backend persisted request tools with
     | Succeeded (`String "selected child") -> ()
     | _ -> failwith "selected target dispatch failed");
    assert (Result.is_error (Calls.agent_artifact (Option.value_exn !escaped)));
    [%test_eq: int] 1 !authorizations;
    assert !before_admission_rejected;
    assert !current_rejected;
    let binding =
      C.find (Option.value_exn !current_registry) ~name:Definition.name
      |> Background_execution_tests.cap
    in
    assert (
      Result.is_error
        (N.require_native_admission (Option.value_exn !admission_borrow) ~binding));
    print_s
      [%sexp
        { preparations = (!preparations : int)
        ; executions = (!executions : int)
        ; descendant_rejected = (!descendant_rejected : bool)
        ; replacement_rejected = (!replacement_rejected : bool)
        ; delivery_cancelled = (!delivery_cancelled : bool)
        }]);
  [%expect
    {|
    ((preparations 1) (executions 1) (descendant_rejected true)
     (replacement_rejected true) (delivery_cancelled true))
    |}]
;;

let%test_unit "failed or revoked source preparation never enters protected staging" =
  List.iter [ `Rejected; `Revoked; `Cancelled ] ~f:(fun mode ->
    with_actor (fun env _sw actor _writer _backend ->
      let original = native_registry (ref 0) ~raises:false in
      let registry = ref original in
      let stages = ref 0 in
      let cancellation = ref None in
      let reason = Failure "source-preparation-cancelled" in
      let source_host =
        host actor ~stage:(fun _ _ ->
          Int.incr stages;
          failwith "staging entered")
      in
      let service =
        Jobs.create
          ~env
          ~policy:Chat_response.One_off_request.default_policy
          ~current_capabilities:(fun () -> !registry)
          ~prepare_source:(fun ~binding:_ ~owner:_ ~request:_ ~checkpoint ->
            checkpoint ();
            Eio.Fiber.yield ();
            match mode with
            | `Rejected -> Error (P.Error.invalid_request "source denied")
            | `Revoked ->
              registry := C.select original ~names:[] |> Background_execution_tests.cap;
              Ok (Some (artifact ()))
            | `Cancelled ->
              Eio.Cancel.cancel (Option.value_exn !cancellation) reason;
              Ok (Some (artifact ())))
          ~host:source_host
      in
      let result =
        try
          Eio.Cancel.sub (fun context ->
            cancellation := Some context;
            Jobs.with_scope
              service
              ~owner:(Invocation (P.Id.Invocation.create ()))
              ~selected:original
              ~error:Fn.id
              (fun scope ->
                 (Jobs.moderator_transaction scope).handlers.start_tool
                   ~name:"read_file"
                   ~input:(`Object [])))
        with
        | Eio.Cancel.Cancelled actual when phys_equal actual reason -> Error "cancelled"
      in
      assert (Result.is_error result);
      [%test_eq: int] !stages 0))
;;
