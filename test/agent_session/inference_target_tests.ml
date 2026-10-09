open! Core
open Fixtures
module A = Agent_session
module D = Document_schema
module P = Agent_protocol
module R = Inference.Request
module Selection = Inference.Selection

let inference_ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (R.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let selected selection =
  match Selection.view selection with
  | Captured target -> target
  | Unresolved -> assert false
;;

let job (state : A.Session_state.t) : P.Job.t =
  { id = P.Id.Job.create ()
  ; session_id = state.identity.session_id
  ; generation = state.identity.generation
  ; kind = Model_call
  ; payload = `Object [ "recipe", `String "agent_prompt.v1"; "payload", `Null ]
  ; status = Queued
  ; retry_policy = Never
  ; attempt = 0
  ; created_at = timestamp
  ; started_at = None
  ; next_run_at = None
  ; completed_at = None
  ; result = None
  ; delivery = Not_required
  ; launch = None
  ; progress = None
  }
;;

let transition state delta =
  A.Session_transition.apply ~now:timestamp state ~delta ~payloads:[] |> protocol_ok
;;

let state () workspace_instance =
  actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
;;

let capture delta =
  A.Session_delta_document.create
    delta
    ~limits:document_limits
    ~state_document:A.Session_state_document.authored
  |> document_ok
;;

let binding (state : A.Session_state.t) = List.hd_exn state.model_job_targets

let member json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> assert false
;;

let replace json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, previous) ->
         key, if String.equal name key then value else previous))
  | _ -> assert false
;;

let omit json names =
  match json with
  | `Object fields ->
    `Object
      (List.filter fields ~f:(fun (name, _) ->
         not (List.mem names name ~equal:String.equal)))
  | _ -> assert false
;;

let%test_unit
    "live model admission atomically captures source and recovery invents nothing"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let before = state () workspace_instance in
    let model_job = job before in
    let unbound = A.Session_delta.apply before (Job_changed model_job) |> protocol_ok in
    assert (Result.is_error (A.Session_state.validate unbound));
    assert (
      Result.is_error
        (A.Session_state_document.encode
           (A.Session_state_document.authored unbound)
           ~limits:document_limits));
    let admitted = transition before (Job_changed model_job) in
    A.Session_state.validate admitted.state |> protocol_ok;
    let captured_binding = binding admitted.state in
    assert (
      R.Target.equal
        (selected before.spec.inference_target)
        (selected (A.Model_job_target.source captured_binding)));
    (match Selection.view (A.Model_job_target.execution captured_binding) with
     | Unresolved -> ()
     | Captured _ -> assert false);
    let stored = capture admitted.delta in
    assert (Int.equal (D.Document.version (A.Session_delta_document.document stored)) 4);
    let replayed =
      A.Session_delta.apply before (A.Session_delta_document.value stored) |> protocol_ok
    in
    assert (
      D.Json.equal
        (A.Model_job_target.to_json captured_binding)
        (A.Model_job_target.to_json (binding replayed)));
    let unresolved = Selection.unresolved ~limits:document_limits |> inference_ok in
    let before =
      { before with spec = { before.spec with inference_target = unresolved } }
    in
    assert (
      Result.is_error
        (A.Session_transition.apply
           ~now:timestamp
           before
           ~delta:(Job_changed model_job)
           ~payloads:[])))
;;

let%test_unit "parent changes cannot mutate admitted job source or root recipe target" =
  with_actor_workspace (fun _ workspace_instance ->
    let before = state () workspace_instance in
    let admitted = (transition before (Job_changed (job before))).state in
    let old_binding = binding admitted in
    let source = selected (A.Model_job_target.source old_binding) in
    let next =
      R.Target.with_model source ~model:"approved-parent-change" ~limits:document_limits
      |> inference_ok
    in
    let changed = (transition admitted (Inference_target_changed next)).state in
    assert (R.Target.equal source (selected (A.Model_job_target.source (binding changed))));
    let recipe =
      R.Target.with_model
        source
        ~model:"actual-fetched-recipe-model"
        ~limits:document_limits
      |> inference_ok
    in
    let captured_binding =
      A.Model_job_target.capture_recipe
        (binding changed)
        ~target:recipe
        ~limits:document_limits
      |> protocol_ok
    in
    let captured =
      (transition changed (Model_job_recipe_target_captured captured_binding)).state
    in
    assert (
      R.Target.equal source (selected (A.Model_job_target.source (binding captured))));
    assert (
      R.Target.equal recipe (selected (A.Model_job_target.execution (binding captured))));
    assert (
      Result.is_error
        (A.Model_job_target.capture_recipe
           (binding captured)
           ~target:next
           ~limits:document_limits));
    assert (R.Target.equal source (selected (A.Model_job_target.source old_binding))))
;;

let%test_unit
    "v1 state and delta convert targetless model jobs to explicit unresolved bindings"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let before = state () workspace_instance in
    let model_job = job before in
    let authored = state_document before in
    let payload = D.Document.payload authored in
    let spec = member payload "spec" |> fun json -> omit json [ "inference_target" ] in
    let payload =
      payload
      |> fun json ->
      replace json "spec" spec
      |> fun json ->
      replace json "jobs" (`Array [ P.Job.to_json model_job ])
      |> fun json -> omit json [ "model_job_targets"; "runtime_initialization" ]
    in
    let raw =
      D.Document.create ~limits:document_limits ~kind:"session.state" ~version:1 ~payload
      |> document_ok
    in
    let original_bytes = D.Document.to_string raw in
    let restored =
      A.Session_state_document.decode ~limits:document_limits raw |> document_ok
    in
    let current = A.Session_state_document.value restored in
    (match current.runtime_initialization with
     | Ready -> ()
     | Pending _ -> assert false);
    (match
       ( Selection.view current.spec.inference_target
       , Selection.view (A.Model_job_target.source (binding current))
       , Selection.view (A.Model_job_target.execution (binding current)) )
     with
     | Unresolved, Unresolved, Unresolved -> ()
     | _ -> assert false);
    assert (String.equal original_bytes (D.Document.to_string raw));
    let delta =
      D.Document.create
        ~limits:document_limits
        ~kind:"session.delta"
        ~version:1
        ~payload:
          (`Object
              [ ( "changes"
                , `Array
                    [ `Object
                        [ "kind", `String "job_changed"
                        ; "value", P.Job.to_json model_job
                        ]
                    ] )
              ])
      |> document_ok
    in
    let decoded =
      A.Session_delta_document.decode ~limits:document_limits delta |> document_ok
    in
    let replayed =
      A.Session_delta.apply before (A.Session_delta_document.value decoded) |> protocol_ok
    in
    (match Selection.view (A.Model_job_target.source (binding replayed)) with
     | Unresolved -> ()
     | Captured _ -> assert false);
    assert (Int.equal (D.Document.version delta) 1))
;;

let%test_unit "state validates model binding kind ownership generation and uniqueness" =
  with_actor_workspace (fun _ workspace_instance ->
    let before = state () workspace_instance in
    let admitted = (transition before (Job_changed (job before))).state in
    let captured = binding admitted in
    assert (Result.is_error (A.Session_state.validate { admitted with jobs = [] }));
    assert (
      Result.is_error
        (A.Session_state.validate
           { admitted with model_job_targets = [ captured; captured ] }));
    let model_job = List.hd_exn admitted.jobs in
    assert (
      Result.is_error
        (A.Session_state.validate
           { admitted with
             jobs = [ { model_job with generation = model_job.generation + 1 } ]
           }));
    assert (
      Result.is_error
        (A.Session_state.validate
           { admitted with jobs = [ { model_job with kind = Async_tool } ] })))
;;

let%test_unit
    "pending initialization is explicit durable state with retained future fields"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let before = state () workspace_instance in
    let pending =
      { before with runtime_initialization = Pending { fresh_history = true } }
    in
    let authored = state_document pending in
    let payload = D.Document.payload authored in
    let initialization = member payload "runtime_initialization" in
    let initialization =
      match initialization with
      | `Object fields ->
        `Object (fields @ [ "future_initialization", `String "retained" ])
      | _ -> assert false
    in
    let payload = replace payload "runtime_initialization" initialization in
    let raw =
      D.Document.create
        ~limits:document_limits
        ~kind:"session.state"
        ~version:(D.Document.version authored)
        ~payload
      |> document_ok
    in
    let original = D.Document.to_string raw in
    let restored =
      A.Session_state_document.decode ~limits:document_limits raw |> document_ok
    in
    let current = A.Session_state_document.value restored in
    (match current.runtime_initialization with
     | Pending { fresh_history = true } -> ()
     | Ready | Pending { fresh_history = false } -> assert false);
    assert (
      Int.equal
        current.conversation.initial_prompt_entry_count
        before.conversation.initial_prompt_entry_count);
    assert (
      List.equal
        P.History.equal_entry
        current.conversation.canonical_history
        before.conversation.canonical_history);
    let encoded =
      A.Session_state_document.encode restored ~limits:document_limits |> document_ok
    in
    assert (String.equal original (D.Document.to_string encoded));
    let completed =
      A.Session_state_document.with_value
        restored
        { current with runtime_initialization = Ready }
    in
    assert (
      Result.is_error (A.Session_state_document.encode completed ~limits:document_limits));
    assert (String.equal original (D.Document.to_string raw)))
;;

let%test_unit "initialization preserves acknowledged shell grants before Ready" =
  with_actor_workspace (fun env workspace_instance ->
    let initial =
      { (state () workspace_instance) with
        runtime_initialization = Pending { fresh_history = true }
      }
    in
    let backend = A.Memory_backend.create ~event_capacity:32 ~initial_state:initial in
    Eio.Switch.run (fun sw ->
      let actor =
        A.Session_actor.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:initial
          ~operation_worker:None
          ~persistence:(A.Memory_backend.persistence backend)
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id = P.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "initialization-test")
            ; job_results = None
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; schedule_limits = A.Staged_schedules.default_limits
            ; notification_limits = A.Staged_notifications.default_limits
            ; ingress_limits = A.Staged_ingress.default_limits
            ; subscription_limits = A.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      Exn.protect
        ~finally:(fun () -> A.Session_actor.shutdown actor)
        ~f:(fun () ->
          let expected = A.Session_actor.state actor |> protocol_ok in
          let manifest : Session.Shell_state.Manifest_grant.persisted =
            { grant_id = "manifest-initialization"
            ; manifest_sha256 = "manifest-sha256"
            ; canonical_source_root = "/prompt"
            ; repository_identity = None
            ; source_sha256 = "source-sha256"
            ; signer = None
            ; issuer = Some "test"
            ; audience = []
            ; schema_version = 1
            ; builtin_versions = []
            ; imported_source_sha256 = []
            ; session_id = Some (P.Id.Session.to_string session_id)
            ; user_id = None
            ; host_id = None
            ; created_at_ns = 1L
            ; expires_at_ns = None
            ; revoked_at_ns = None
            ; revocation_reason = None
            }
          in
          let approval : Session.Shell_state.Approval_grant.persisted =
            { grant_id = "approval-initialization"
            ; manifest_sha256 = "manifest-sha256"
            ; runtime_id = "selected-runtime"
            ; request_kind = Structured
            ; command_sha256 = "command-sha256"
            ; executable_sha256 = "executable-sha256"
            ; argv = [ "inspect" ]
            ; argv_prefix = None
            ; cwd_sha256 = "cwd-sha256"
            ; environment_sha256 = "environment-sha256"
            ; stdin_sha256 = None
            ; stdin_bytes = 0
            ; script_sha256 = None
            ; scope = Exact_session
            ; session_id = Some (P.Id.Session.to_string session_id)
            ; user_id = None
            ; host_id = None
            ; created_at_ns = 1L
            ; expires_at_ns = None
            ; last_used_at_ns = None
            ; reviewer = { source = "host-policy"; reviewer_id = None }
            ; revoked_at_ns = None
            ; revocation_reason = None
            }
          in
          A.Session_actor.add_shell_manifest_grant actor manifest |> protocol_ok;
          A.Session_actor.replace_shell_approval_grants actor [ approval ] |> protocol_ok;
          let acknowledged = A.Session_actor.state actor |> protocol_ok in
          assert (List.is_empty expected.shell.manifest_grants);
          assert (List.is_empty expected.shell.approval_grants);
          let scope =
            A.Session_actor.begin_initialization actor ~expected |> protocol_ok
          in
          Exn.protect
            ~f:(fun () ->
              A.Session_actor.complete_initialization actor ~scope ~candidate:expected
              |> protocol_ok
              |> ignore)
            ~finally:(fun () ->
              A.Session_actor.end_initialization actor ~scope |> protocol_ok);
          let ready = A.Session_actor.state actor |> protocol_ok in
          (match ready.runtime_initialization with
           | Ready -> ()
           | Pending _ -> assert false);
          assert (
            Int64.(
              ready.counters.transaction_sequence
              > acknowledged.counters.transaction_sequence));
          assert (
            Sexp.equal
              (Session.Shell_state.sexp_of_t ready.shell)
              (Session.Shell_state.sexp_of_t acknowledged.shell));
          assert (
            Sexp.equal
              (Session.Shell_state.sexp_of_t (A.Memory_backend.state backend).shell)
              (Session.Shell_state.sexp_of_t acknowledged.shell)))))
;;

let create_pending_actor env sw initial =
  let backend = A.Memory_backend.create ~event_capacity:128 ~initial_state:initial in
  let actor =
    A.Session_actor.create
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~mailbox_capacity:32
      ~compaction_env:None
      ~initial_state:initial
      ~operation_worker:None
      ~persistence:(A.Memory_backend.persistence backend)
      ~services:
        { now = (fun () -> timestamp)
        ; monotonic_now = (fun () -> Mtime.min_stamp)
        ; create_attachment_id = P.Id.Attachment.create
        ; create_reclaim_token = (fun () -> "initialization-scope-test")
        ; job_results = None
        ; schedule_limits = A.Staged_schedules.default_limits
        ; notification_limits = A.Staged_notifications.default_limits
        ; ingress_limits = A.Staged_ingress.default_limits
        ; subscription_limits = A.Staged_subscriptions.default_limits
        ; state_committed = (fun _ _ -> ())
        }
  in
  actor, backend
;;

let pending_state workspace_instance =
  { (state () workspace_instance) with
    runtime_initialization = Pending { fresh_history = true }
  }
;;

let with_pending_actor ?(prepare = Fn.id) f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let actor, backend =
        create_pending_actor env sw (prepare (pending_state workspace_instance))
      in
      Exn.protect
        ~f:(fun () -> f actor backend)
        ~finally:(fun () -> A.Session_actor.shutdown actor)))
;;

let with_two_pending_actors f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let initial = pending_state workspace_instance in
      let actor, _ = create_pending_actor env sw initial in
      let other, _ = create_pending_actor env sw initial in
      Exn.protect
        ~f:(fun () -> f actor other)
        ~finally:(fun () ->
          A.Session_actor.shutdown other;
          A.Session_actor.shutdown actor)))
;;

let%test_unit "initialization capability admits and completes a stopped synchronous job" =
  with_pending_actor (fun actor backend ->
    let expected = A.Session_actor.state actor |> protocol_ok in
    let scope = A.Session_actor.begin_initialization actor ~expected |> protocol_ok in
    Exn.protect
      ~finally:(fun () -> A.Session_actor.end_initialization actor ~scope |> protocol_ok)
      ~f:(fun () ->
        let running =
          A.Session_actor.start_initialization_model_job actor ~scope (job expected)
          |> protocol_ok
        in
        assert (
          match running.status with
          | Running -> true
          | _ -> false);
        assert (Int.equal running.attempt 1);
        let current = A.Session_actor.state actor |> protocol_ok in
        assert (P.Session.equal_desired_state current.lifecycle.desired Stopped);
        assert (Int.equal (List.length current.jobs) 1);
        assert (Int.equal (List.length current.model_job_targets) 1);
        assert (
          A.Session_actor.initialization_model_job_is_current
            actor
            ~scope
            ~job_id:running.id
            ~generation:running.generation
            ~attempt:running.attempt
          |> protocol_ok);
        let target = selected expected.spec.inference_target in
        A.Session_actor.capture_initialization_recipe_target
          actor
          ~scope
          ~job_id:running.id
          ~generation:running.generation
          ~attempt:running.attempt
          ~target
          ~limits:document_limits
        |> protocol_ok;
        A.Session_actor.complete_initialization_model_job
          actor
          ~scope
          ~job_id:running.id
          ~generation:running.generation
          ~attempt:running.attempt
          (A.Runtime_builder.Model_succeeded (`String "initializer result"))
        |> protocol_ok
        |> ignore;
        A.Session_actor.complete_initialization actor ~scope ~candidate:expected
        |> protocol_ok
        |> ignore;
        let ready = A.Memory_backend.state backend in
        assert (
          A.Session_state.Runtime_initialization.equal ready.runtime_initialization Ready);
        assert (
          match (List.hd_exn ready.jobs).status with
          | Succeeded -> true
          | _ -> false);
        assert (
          D.Json.equal
            (Selection.to_json ready.spec.inference_target)
            (Selection.to_json expected.spec.inference_target))))
;;

let%test_unit
    "initialization token is actor owned exclusive and cannot authorize other jobs"
  =
  with_two_pending_actors (fun actor other ->
    let expected = A.Session_actor.state actor |> protocol_ok in
    let scope = A.Session_actor.begin_initialization actor ~expected |> protocol_ok in
    Exn.protect
      ~finally:(fun () -> A.Session_actor.end_initialization actor ~scope |> protocol_ok)
      ~f:(fun () ->
        assert (Result.is_error (A.Session_actor.begin_initialization actor ~expected));
        assert (Result.is_error (A.Session_actor.end_initialization other ~scope));
        assert (
          Result.is_error
            (A.Session_actor.start_initialization_model_job other ~scope (job expected)));
        let outsider = A.Session_actor.add_job actor (job expected) |> protocol_ok in
        let outsider =
          A.Session_actor.claim_job
            actor
            ~job_id:outsider.id
            ~generation:outsider.generation
          |> protocol_ok
          |> Option.value_exn
        in
        assert (
          not
            (A.Session_actor.initialization_model_job_is_current
               actor
               ~scope
               ~job_id:outsider.id
               ~generation:outsider.generation
               ~attempt:outsider.attempt
             |> protocol_ok));
        assert (
          Result.is_error
            (A.Session_actor.complete_initialization_model_job
               actor
               ~scope
               ~job_id:outsider.id
               ~generation:outsider.generation
               ~attempt:outsider.attempt
               (A.Runtime_builder.Model_succeeded `Null)));
        let owned =
          A.Session_actor.start_initialization_model_job actor ~scope (job expected)
          |> protocol_ok
        in
        assert (
          not
            (A.Session_actor.initialization_model_job_is_current
               actor
               ~scope
               ~job_id:owned.id
               ~generation:owned.generation
               ~attempt:(owned.attempt + 1)
             |> protocol_ok));
        assert (
          Result.is_error
            (A.Session_actor.capture_initialization_recipe_target
               actor
               ~scope
               ~job_id:owned.id
               ~generation:owned.generation
               ~attempt:(owned.attempt + 1)
               ~target:(selected expected.spec.inference_target)
               ~limits:document_limits))))
;;

let%test_unit "authorized stopped no-op revokes initialization but rejected stop does not"
  =
  with_pending_actor (fun actor backend ->
    let writer, _ =
      A.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
    in
    let reader, _ =
      A.Session_actor.attach actor ~mode:Read_only ~subscribe:false |> protocol_ok
    in
    let expected = A.Session_actor.state actor |> protocol_ok in
    let scope = A.Session_actor.begin_initialization actor ~expected |> protocol_ok in
    assert (
      Result.is_error (A.Session_actor.stop actor ~attachment_id:reader.id ~mode:Graceful));
    let queued =
      A.Session_actor.add_initialization_model_job
        actor
        ~scope
        { (job expected) with delivery = Pending }
      |> protocol_ok
    in
    (* Graceful stop preserves queued work. Clear it through ordinary cancellation
       so the next stop is the actual stopped/no-work no-op branch. *)
    A.Session_actor.cancel_job actor ~attachment_id:writer.id ~job_id:queued.id ()
    |> protocol_ok
    |> ignore;
    let before = A.Session_actor.state actor |> protocol_ok in
    A.Session_actor.stop actor ~attachment_id:writer.id ~mode:Graceful
    |> protocol_ok
    |> ignore;
    let after = A.Memory_backend.state backend in
    assert (
      Int64.equal before.counters.transaction_sequence after.counters.transaction_sequence);
    assert (
      Result.is_error
        (A.Session_actor.start_initialization_model_job actor ~scope (job after)));
    assert (
      Result.is_error
        (A.Session_actor.complete_initialization actor ~scope ~candidate:expected));
    A.Session_actor.end_initialization actor ~scope |> protocol_ok;
    A.Session_actor.end_initialization actor ~scope |> protocol_ok)
;;

let%test_unit
    "stop fences an already claimed initializer attempt and its target publication"
  =
  with_pending_actor (fun actor _ ->
    let writer, _ =
      A.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
    in
    let expected = A.Session_actor.state actor |> protocol_ok in
    let scope = A.Session_actor.begin_initialization actor ~expected |> protocol_ok in
    let running =
      A.Session_actor.start_initialization_model_job actor ~scope (job expected)
      |> protocol_ok
    in
    A.Session_actor.stop actor ~attachment_id:writer.id ~mode:Graceful
    |> protocol_ok
    |> ignore;
    assert (
      not
        (A.Session_actor.initialization_model_job_is_current
           actor
           ~scope
           ~job_id:running.id
           ~generation:running.generation
           ~attempt:running.attempt
         |> protocol_ok));
    assert (
      Result.is_error
        (A.Session_actor.capture_initialization_recipe_target
           actor
           ~scope
           ~job_id:running.id
           ~generation:running.generation
           ~attempt:running.attempt
           ~target:(selected expected.spec.inference_target)
           ~limits:document_limits));
    assert (
      Result.is_error
        (A.Session_actor.complete_initialization_model_job
           actor
           ~scope
           ~job_id:running.id
           ~generation:running.generation
           ~attempt:running.attempt
           (A.Runtime_builder.Model_succeeded `Null)));
    A.Session_actor.end_initialization actor ~scope |> protocol_ok;
    A.Session_actor.interrupt_job
      actor
      ~job_id:running.id
      ~generation:running.generation
      ~attempt:running.attempt
      ~reason:"constructor revoked"
    |> protocol_ok
    |> ignore;
    let current = A.Session_actor.state actor |> protocol_ok in
    assert (
      A.Session_state.Runtime_initialization.equal
        current.runtime_initialization
        (Pending { fresh_history = true }));
    assert (
      List.for_all current.jobs ~f:(fun job ->
        not
          (match job.status with
           | Running -> true
           | _ -> false))))
;;

let%test_unit "durable Pending alone cannot restore or revive constructor authority" =
  with_pending_actor
    ~prepare:(fun state ->
      { state with
        spec =
          { state.spec with
            inference_target =
              Selection.unresolved ~limits:document_limits |> inference_ok
          }
      })
    (fun actor _ ->
       let expected = A.Session_actor.state actor |> protocol_ok in
       assert (Result.is_error (A.Session_actor.begin_initialization actor ~expected)));
  with_pending_actor
    ~prepare:(fun state -> { state with runtime_initialization = Ready })
    (fun actor _ ->
       let expected = A.Session_actor.state actor |> protocol_ok in
       assert (Result.is_error (A.Session_actor.begin_initialization actor ~expected)));
  with_pending_actor (fun actor _ ->
    let expected = A.Session_actor.state actor |> protocol_ok in
    let retired = A.Session_actor.begin_initialization actor ~expected |> protocol_ok in
    A.Session_actor.end_initialization actor ~scope:retired |> protocol_ok;
    let active = A.Session_actor.begin_initialization actor ~expected |> protocol_ok in
    (* Ending the old token cannot retire the actual current owner. *)
    A.Session_actor.end_initialization actor ~scope:retired |> protocol_ok;
    assert (
      Result.is_error
        (A.Session_actor.start_initialization_model_job
           actor
           ~scope:retired
           (job expected)));
    let queued =
      A.Session_actor.add_initialization_model_job
        actor
        ~scope:active
        { (job expected) with delivery = Pending }
      |> protocol_ok
    in
    assert (P.Job.equal_kind queued.kind Model_call);
    A.Session_actor.end_initialization actor ~scope:active |> protocol_ok)
;;
