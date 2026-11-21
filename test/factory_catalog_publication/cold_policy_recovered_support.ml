open! Core
open Agent_server_test_support
open Retry_support
module Cold = Cold_policy_resource_tests
module Store = Agent_store
module B = A.Runtime_builder
module Resource = Factory.For_testing
module Recovery_owner = Agent_server.Session_recovery_owner
module Graph = Agent_server.Graph_tracking
module Scope = Owner.Policy_restoration_source
module Persistence = A.Session_persistence
module E = P.Moderator_execution
module Existing = Source_preparation_tests

module Product_source_selection = struct
  type t =
    | Selected
    | Legacy_missing
end

type hooks =
  { before_publish : unit -> unit
  ; observe_publication : Scope.Publication.t -> unit
  ; on_physical_release : unit -> unit
  }

let no_hooks =
  { before_publish = ignore; observe_publication = ignore; on_physical_release = ignore }
;;

type t =
  { env : Eio_unix.Stdenv.base
  ; sw : Eio.Switch.t
  ; root : string
  ; daemon : Daemon.t
  ; actor : Actor.t
  ; handle : Store.Session_store.Handle.t
  ; writer : Store.Commit_writer.t
  ; recovered_state : A.Session_state.t
  ; recovery_owner : Recovery_owner.t
  ; source_owner : A.Runtime_source_owner.t
  ; revision : A.Prompt_revision.t
  ; source : B.Source.t
  ; product_source_selection : Product_source_selection.t
  ; missing_product_source : P.Error.t option ref
  ; profile : A.Permission_policy.t
  ; cell : Resource.t
  ; runtime : Owner.t option ref
  ; phase : string ref
  ; counters : counters
  ; lifecycle_calls : int ref
  ; mutable hooks : hooks
  }

let owner t = Option.value_exn !(t.runtime)
let unavailable message = P.Error.create Invalid_state ~message ~retryable:false ()
let unexpected what = Error (unavailable ("634 recovered host unexpectedly used " ^ what))
let state t = Actor.state t.actor |> protocol_ok

let capture f =
  try Ok (f ()) with
  | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
;;

let resume = function
  | Ok x -> x
  | Error (exn, bt) -> Exn.raise_with_original_backtrace exn bt
;;

(* The native switch is an actual separately joined owner. Closing the Cell's
   physical runtime returns only after that switch and its release hooks exit. *)
let build_in_scope t ~on_release f =
  let ready, ready_u = Eio.Promise.create () in
  let stop, stop_u = Eio.Promise.create () in
  let exited, exited_u = Eio.Promise.create () in
  let delivered = ref false in
  let signal_stop () =
    if Option.is_none (Eio.Promise.peek stop) then Eio.Promise.resolve stop_u ()
  in
  let stop_and_join () =
    Eio.Cancel.protect (fun () ->
      signal_stop ();
      Eio.Promise.await exited |> resume)
  in
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    let outcome =
      capture (fun () ->
        Eio.Cancel.sub (fun context ->
          Eio.Switch.run (fun sw ->
            Eio.Switch.on_release sw on_release;
            Eio.Fiber.fork_daemon ~sw (fun () ->
              Eio.Promise.await stop;
              Eio.Cancel.cancel context Exit;
              `Stop_daemon);
            let built = f sw in
            delivered := true;
            Eio.Promise.resolve ready_u (Ok built);
            Eio.Promise.await stop)))
    in
    if not !delivered
    then (
      match outcome with
      | Error failure -> Eio.Promise.resolve ready_u (Error failure)
      | Ok () -> assert false);
    (* Exit is the intentional private-scope cancellation after its real join. *)
    let outcome =
      match outcome with
      | Error (Eio.Cancel.Cancelled Exit, _) when Option.is_some (Eio.Promise.peek stop)
        -> Ok ()
      | other -> other
    in
    Eio.Promise.resolve exited_u outcome);
  match capture (fun () -> Eio.Promise.await ready |> resume) with
  | Ok built -> built, stop_and_join
  | Error (exn, bt) ->
    ignore (capture stop_and_join : (unit, _) Result.t);
    Exn.raise_with_original_backtrace exn bt
;;

let services t : B.extension_services =
  { runtime_policy = Daemon.default_options.chatml_runtime_policy
  ; native_service_revision = None
  ; script_tools =
      (fun native ->
        let registry =
          Lazy.force native.Chat_response.Agent_runtime.capabilities
          |> Result.map_error ~f:(fun e -> e.Chat_response.Tool_capability.message)
          |> Result.ok_or_failwith
        in
        A.Script_tool_calls.create
          ~registry:(fun () -> registry)
          ~moderator_names:String.Set.empty
          ~now:P.Timestamp.now
          ~is_halted:(fun () -> (state t).halted)
          ~requires_active_moderator:(fun _ -> true)
          ~authorize:(fun _ _ -> unexpected "Tool.call")
          ~prepare_output:(fun _ -> unexpected "Tool output")
          ~defer_observation:(fun _ -> unexpected "Tool observation"))
  ; standalone_execution_limits = A.Standalone_tool_dispatch.declared_execution_limits
  ; one_off_policy = Chat_response.One_off_request.default_policy
  ; authoring_validation_host = None
  ; initial_agent_guidance = None
  ; claim_lifecycle =
      (fun ~event ->
        Int.incr t.lifecycle_calls;
        Actor.with_current_moderator_event t.actor ~operation_id:None ~event)
  ; run_actions = (fun _ -> unexpected "Run action")
  ; lifecycle_started =
      (fun observer ->
        let current = state t in
        let basis = A.Policy_restore_basis.of_state current |> protocol_ok in
        P.Invocation.equal_observer
          observer
          (A.Moderator_lifecycle.source (A.Policy_restore_basis.lifecycle basis)).observer)
  ; history =
      (fun () ->
        A.History_codec.all_of_protocol (state t).conversation.canonical_history
        |> protocol_ok)
  ; standalone_completion = (fun ~tools:_ _ -> unexpected "standalone completion")
  ; idle_notifications = (fun ~source:_ ~tools:_ () -> unexpected "idle notification")
  ; notification_input =
      (fun ~source:_ ~tools:_ ~consume:_ () -> unexpected "notification")
  ; initial_notification_input =
      (fun ~source:_ ~tools:_ ~consume:_ () -> unexpected "initial notification")
  }
;;

type construction =
  { restoration : Actor.Policy_restoration.t
  ; access : B.Restored_policy_access.t
  ; expected : A.Session_state.t
  ; ordinary : Resource.Ordinary.t
  }

type admission =
  | Actor_direct
  | Owner_stage of Scope.t

let model_jobs : A.Runtime_model_services.t =
  { spawn_model = (fun ~recipe:_ ~payload:_ -> failwith "644 unexpected Model.spawn")
  ; call_model =
      (fun ~recipe:_ ~payload:_ ~execute:_ -> failwith "644 unexpected Model.call")
  }
;;

let construction_inputs
      t
      ~restoration
      ~(expected : A.Session_state.t)
      ~check
      ~capture_tracking
  =
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr t.counters.providers;
      failwith "634 recovered component entered provider")
  in
  let upstream =
    policy.runtime_inference_ports t.actor
    |> Result.map_error ~f:(fun e ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t e))
    |> Result.ok_or_failwith
  in
  let graph =
    Graph.create_for_policy_restoration
      t.actor
      ~restoration
      ~source:
        (Transcript.Source_id.of_string
           ("634-restored/" ^ P.Id.Transaction.to_string (P.Id.Transaction.create ()))
         |> Result.ok_or_failwith)
      ~upstream
    |> protocol_ok
  in
  capture_tracking graph;
  let target =
    policy.capture_inference_target
      ~prompt_revision_id:(A.Prompt_revision.id t.revision)
      ~config:(Chat_response.Config.of_elements (A.Prompt_revision.elements t.revision))
    |> Result.map_error ~f:(fun e ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t e))
    |> Result.ok_or_failwith
  in
  let context =
    policy.resolve_inference_context target
    |> Result.map_error ~f:(fun e ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t e))
    |> Result.ok_or_failwith
  in
  let paths =
    A.Runtime_paths.create
      ~env:t.env
      ~tool_dir:t.root
      ~workspace:expected.spec.workspace_instance.canonical_root.native_path
      ~prompt_dir:(Eio.Path.native_exn (A.Prompt_revision.materialized_tree t.revision))
      ~session_dir:(Store.Session_store.Handle.directory t.handle)
      ~cache_dir:(Store.Session_store.Handle.cache_directory t.handle)
      ~home:t.root
    |> Existing.store_ok
  in
  ( graph
  , { B.Native_host.preparation = Original_execution
    ; check_source = check
    ; startup = None
    ; env = t.env
    ; inference_context = context
    ; inference_identity = Graph.identity graph
    ; bind_inference_identity = (fun allocate -> Graph.bind_identity graph ~allocate)
    ; on_inference_attempt = Graph.on_attempt graph
    ; on_inference_completion = Graph.on_completion graph
    ; on_inference_observation = Graph.on_observation graph
    ; paths
    ; storage_paths = paths
    ; session_id = expected.identity.session_id
    ; permission_profile = t.profile
    ; native_service_revision = None
    ; one_off_policy = Some Chat_response.One_off_request.default_policy
    ; authoring_validation_host = None
    ; initial_agent_guidance = None
    ; ancestor_capabilities = None
    ; ancestor_sources = None
    ; manifest_authorizer = Shell_runtime.Manifest_authorizer.deny
    ; approval_provider = Shell_runtime.Approval_broker.None_available
    ; approval_store = Shell_access.Approval.create_store ()
    } )
;;

let with_construction t (native : Cold.native_request) ~admission f =
  let open Result.Let_syntax in
  native.authorize () |> protocol_ok;
  let before = state t in
  B.Source.validate_target t.source before |> protocol_ok;
  let basis = A.Policy_restore_basis.of_state before |> protocol_ok in
  let restoration =
    Eio.Cancel.protect (fun () ->
      match admission with
      | Actor_direct ->
        Actor.begin_policy_restoration
          t.actor
          ~source_owner:t.source_owner
          ~source:t.source
          ~basis
          ~request:native.request
          ~governance:native.governance
          ~authorize:native.authorize
        |> protocol_ok
      | Owner_stage stage ->
        Scope.prepare stage ~source_owner:t.source_owner ~source:t.source ~basis
        |> protocol_ok)
  in
  protect_cleanup
    ~finally:(fun () ->
      match admission with
      | Actor_direct -> Actor.end_policy_restoration t.actor ~restoration |> protocol_ok
      | Owner_stage _ -> ())
    (fun () ->
       let check () =
         let%bind () = native.authorize () in
         Actor.Policy_restoration.check restoration
       in
       let reservation =
         Actor.reserve_policy_restoration_history_block t.actor ~restoration ~count:100
         |> protocol_ok
       in
       let expected = state t in
       A.Policy_restore_basis.check_construction basis ~state:expected |> protocol_ok;
       Resource.Ordinary.with_probe
         t.cell
         ~basis
         ~check
         ~expected
         ~build:(fun ordinary ~capture_tracking ->
           let graph, native_host =
             construction_inputs t ~restoration ~expected ~check ~capture_tracking
           in
           let paths = native_host.B.Native_host.paths in
           let context = native_host.inference_context in
           let jobs = model_jobs in
           let runtime, join =
             build_in_scope t ~on_release:t.hooks.on_physical_release (fun sw ->
               let product, binding =
                 B.build_product_with_extensions
                   ~selected_source:
                     (match t.product_source_selection with
                      | Selected -> Some t.source
                      | Legacy_missing -> None)
                   ~ancestor_capabilities:None
                   ~ancestor_sources:None
                   ~construction_checkpoint:(Some check)
                   ~startup:None
                   ~legacy_construction:None
                   ~agent_source:None
                   ~root_invocation:expected.spec.root_invocation
                   ~script_preparation:Daemon.default_options.script_preparation
                   ~native_registrations:[]
                   ~services:(services t)
                   ~sw
                   ~env:t.env
                   ~inference_context:context
                   ~inference_identity:(Graph.identity graph)
                   ~bind_inference_identity:(fun allocate ->
                     Graph.bind_identity graph ~allocate)
                   ~on_inference_attempt:(Graph.on_attempt graph)
                   ~on_inference_completion:(Graph.on_completion graph)
                   ~on_inference_observation:(Graph.on_observation graph)
                   ~paths
                   ~storage_paths:paths
                   ~revision:t.revision
                   ~session_id:expected.identity.session_id
                   ~history_namespace:
                     (P.Id.Session.to_string expected.identity.session_id)
                   ~next_history_sequence:(Int64.to_int_exn reservation.first_sequence)
                   ~existing_history:
                     (Some
                        (A.History_codec.all_of_protocol
                           expected.conversation.canonical_history
                         |> protocol_ok))
                   ~existing_moderator_snapshot:expected.moderator
                   ~moderator_reservation_size:100
                   ~manifest_authorizer:Shell_runtime.Manifest_authorizer.deny
                   ~approval_provider:Shell_runtime.Approval_broker.None_available
                   ~approval_store:(Shell_access.Approval.create_store ())
                   ~permission_profile:t.profile
                   ~review_permission:(fun _ ->
                     failwith "634 unexpected permission review")
                   ~schedule_services:
                     { after_ms =
                         (fun ~delay_ms:_ ~payload:_ ->
                           failwith "634 unexpected Schedule.after_ms")
                     ; cancel = (fun ~id:_ -> failwith "634 unexpected Schedule.cancel")
                     }
                   ~job_services:
                     { initialization = jobs; for_execution = (fun _ -> jobs) }
                 |> protocol_ok
               in
               Resource.Ordinary.capture_product ordinary ~product ~binding |> protocol_ok;
               B.Product.Binding.runtime binding)
           in
           let closed = ref false in
           let close () =
             if not !closed
             then (
               (* Record success only after all actual resource/factual legs join. *)
               let sealed = capture (fun () -> Graph.seal graph |> protocol_ok) in
               let primary = capture (fun () -> B.close runtime) in
               let joined = capture join in
               let finished = capture (fun () -> Graph.finish graph |> protocol_ok) in
               resume sealed;
               resume primary;
               resume joined;
               resume finished;
               closed := true)
           in
           let runtime =
             { runtime with
               B.close = B.Close.legacy close
             ; moderator_snapshot = (fun () -> expected.moderator)
             }
           in
           Ok runtime)
         ~f:(fun ordinary ->
           let expected = Resource.Ordinary.expected ordinary in
           let access = Resource.Ordinary.access ordinary |> protocol_ok in
           f { restoration; access; expected; ordinary }))
;;

(* Actual native-first Cell construction under the same genuine recovered-policy
   admission as with_construction. The qualified lifetime owns the real child,
   Construction and graph before preparation can yield. *)
type native_construction =
  { restoration : Actor.Policy_restoration.t
  ; basis : A.Policy_restore_basis.t
  ; expected : A.Session_state.t
  ; check : unit -> (unit, P.Error.t) result
  ; native : Resource.Native.t
  ; loan : Resource.Native_loan.t
  ; prepared : B.Prepared_native.t
  }

let with_native_construction t (native : Cold.native_request) f =
  t.phase := "644 admit native construction";
  native.authorize () |> protocol_ok;
  let expected = state t in
  B.Source.validate_target t.source expected |> protocol_ok;
  let basis = A.Policy_restore_basis.of_state expected |> protocol_ok in
  let restoration =
    Actor.begin_policy_restoration
      t.actor
      ~source_owner:t.source_owner
      ~source:t.source
      ~basis
      ~request:native.request
      ~governance:native.governance
      ~authorize:native.authorize
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Actor.end_policy_restoration t.actor ~restoration |> protocol_ok)
    (fun () ->
       let check () =
         let open Result.Let_syntax in
         let%bind () = native.authorize () in
         Actor.Policy_restoration.check restoration
       in
       Resource.Native.with_probe
         t.cell
         ~sw:t.sw
         ~expected
         ~host:(fun ~capture_tracking ->
           t.phase := "644 prepare actual native driver";
           let _, host =
             construction_inputs t ~restoration ~expected ~check ~capture_tracking
           in
           host)
         ~input:(fun () ->
           B.Native_input.create
             ~source:t.source
             ~family:
               (Authored
                  { revision = t.revision
                  ; root_invocation = expected.spec.root_invocation
                  ; agent_source = None
                  ; script_preparation = Daemon.default_options.script_preparation
                  ; native_registrations = []
                  }))
         ~on_physical_release:t.hooks.on_physical_release
         ~f:(fun actual ->
           t.phase := "644 publish prepared native unit";
           f
             { restoration
             ; basis
             ; expected
             ; check
             ; native = actual
             ; loan = Resource.Native.loan actual
             ; prepared = Resource.Native.prepared actual |> protocol_ok
             }))
;;

let with_native_promotion t constructed f =
  t.phase := "644 reserve actual native promotion";
  Resource.Attachment.with_probe
    constructed.native
    ~build:(fun prepared ~attachment ->
      let reservation =
        Actor.reserve_policy_restoration_history_block
          t.actor
          ~restoration:constructed.restoration
          ~count:100
        |> protocol_ok
      in
      t.phase := "644 build actual Product attachment";
      let product, binding =
        B.build_product_from_native
          prepared
          ~attachment
          ~source:t.source
          ~construction_checkpoint:constructed.check
          ~legacy_construction:None
          ~services:(Some (services t))
          ~history_namespace:
            (P.Id.Session.to_string constructed.expected.identity.session_id)
          ~next_history_sequence:(Int64.to_int_exn reservation.first_sequence)
          ~existing_history:
            (Some
               (A.History_codec.all_of_protocol
                  constructed.expected.conversation.canonical_history
                |> protocol_ok))
          ~existing_moderator_snapshot:constructed.expected.moderator
          ~moderator_reservation_size:100
          ~permission_profile:t.profile
          ~review_permission:(fun _ -> failwith "644 unexpected permission review")
          ~schedule_services:
            { after_ms =
                (fun ~delay_ms:_ ~payload:_ ->
                  failwith "644 unexpected Schedule.after_ms")
            ; cancel = (fun ~id:_ -> failwith "644 unexpected Schedule.cancel")
            }
          ~job_services:
            { initialization = model_jobs; for_execution = (fun _ -> model_jobs) }
        |> protocol_ok
      in
      let runtime = B.Product.Binding.runtime binding in
      let runtime =
        { runtime with B.moderator_snapshot = (fun () -> constructed.expected.moderator) }
      in
      Ok { Resource.Materialization.product; binding; physical = runtime })
    ~f
;;

let publish_cell (construction : construction) =
  Resource.Ordinary.publish construction.ordinary
;;

(* This real recovered host owns the first Product loan before publication.
   The extra native loan must expose that same selected Source and resource view,
   without another prepare/claim or closing the first loan's physical unit. *)
let check_loaded_selected_source t loan =
  let product = Resource.Loan.product loan ~cell:t.cell |> protocol_ok in
  let runtime = Resource.Loan.runtime loan ~cell:t.cell |> protocol_ok in
  let expected = state t in
  let ports = !(t.counters.inference_ports) in
  let providers = !(t.counters.providers) in
  let lifecycle_calls = !(t.lifecycle_calls) in
  let source = B.Product.selected_source product |> protocol_ok in
  assert (phys_equal source t.source);
  let native =
    Resource.pin_native
      t.cell
      ~expected_native:(B.Product.native_identity product)
      ~expected
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Resource.Native_loan.close native |> protocol_ok)
    (fun () ->
       Resource.Native_loan.with_resources native ~f:(fun original ->
         Resource.Native_loan.with_source_resources native ~f:(fun ~source resources ->
           assert (phys_equal source t.source);
           assert (
             phys_equal resources.B.native (Option.value_exn runtime.B.native_runtime));
           assert (phys_equal resources.native original.B.native);
           assert (Option.equal phys_equal resources.managed original.managed);
           assert (
             Option.equal
               Store.Prompt_source_receipt.equal
               (A.Prompt_source_lease.receipt resources.source_lease)
               (A.Prompt_source_lease.receipt runtime.source_lease));
           (* This fixture has no original-root receipt. Its real Extension source
            must not be mistaken for No_moderator because of that absence. *)
           assert (A.Prompt_source_lease.is_legacy resources.source_lease);
           (match B.Source.moderator source with
            | Extension _ -> ()
            | No_moderator | Legacy _ -> failwith "real recovered policy source was lost");
           Ok ()))
       |> protocol_ok);
  assert (phys_equal (B.Product.selected_source product |> protocol_ok) t.source);
  [%test_eq: int] ports !(t.counters.inference_ports);
  [%test_eq: int] providers !(t.counters.providers);
  [%test_eq: int] lifecycle_calls !(t.lifecycle_calls);
  Delegated_policy_claim_fault_tests.assert_same expected (state t)
;;

(* Legacy Product compatibility has actual physical custody but deliberately
   lacks the selected-Source disclosure. Keep every physical check explicit. *)
let check_loaded_legacy_source t loan =
  let product = Resource.Loan.product loan ~cell:t.cell |> protocol_ok in
  let runtime = Resource.Loan.runtime loan ~cell:t.cell |> protocol_ok in
  let expected = state t in
  let ports = !(t.counters.inference_ports) in
  let providers = !(t.counters.providers) in
  let lifecycle_calls = !(t.lifecycle_calls) in
  let missing_source () =
    match B.Product.selected_source product with
    | Error ({ code = Invalid_state; retryable = false; _ } as error) -> error
    | Error error -> raise_s [%sexp (error : P.Error.t)]
    | Ok _ -> failwith "legacy compatibility Product disclosed a selected Source"
  in
  let missing = missing_source () in
  t.missing_product_source := Some missing;
  let native =
    Resource.pin_native
      t.cell
      ~expected_native:(B.Product.native_identity product)
      ~expected
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Resource.Native_loan.close native |> protocol_ok)
    (fun () ->
       Resource.Native_loan.with_resources native ~f:(fun actual ->
         assert (phys_equal actual.B.native (Option.value_exn runtime.B.native_runtime));
         assert (
           Option.equal
             Store.Prompt_source_receipt.equal
             (A.Prompt_source_lease.receipt actual.source_lease)
             (A.Prompt_source_lease.receipt runtime.source_lease));
         assert (A.Prompt_source_lease.is_legacy actual.source_lease);
         Resource.Native_loan.with_resources native ~f:(fun repeated ->
           assert (phys_equal actual.native repeated.B.native);
           assert (Option.equal phys_equal actual.managed repeated.managed);
           let disclosures = ref 0 in
           (match
              Resource.Native_loan.with_source_resources native ~f:(fun ~source:_ _ ->
                Int.incr disclosures;
                Ok ())
            with
            | Error error ->
              [%test_eq: Sexp.t] (P.Error.sexp_of_t missing) (P.Error.sexp_of_t error)
            | Ok () -> failwith "legacy missing Source entered disclosure callback");
           [%test_eq: int] 0 !disclosures;
           Ok ()))
       |> protocol_ok);
  [%test_eq: Sexp.t] (P.Error.sexp_of_t missing) (P.Error.sexp_of_t (missing_source ()));
  [%test_eq: int] ports !(t.counters.inference_ports);
  [%test_eq: int] providers !(t.counters.providers);
  [%test_eq: int] lifecycle_calls !(t.lifecycle_calls);
  Delegated_policy_claim_fault_tests.assert_same expected (state t)
;;

let check_loaded_source t loan =
  match t.product_source_selection with
  | Selected -> check_loaded_selected_source t loan
  | Legacy_missing -> check_loaded_legacy_source t loan
;;

let prepare_owner_policy
      t
      native
      ~before_publish
      ~observe_publication
      ~on_physical_release
  =
  let previous = t.hooks in
  protect_cleanup
    ~finally:(fun () -> t.hooks <- previous)
    (fun () ->
       t.hooks <- { before_publish; observe_publication; on_physical_release };
       Owner.prepare_delegated_tool
         (owner t)
         ~request:native.Cold.request
         ~governance:native.governance
         ~delegation:native.delegation
         ~event:(Pre_tool_call native.request.call)
         ~authorize:native.authorize)
;;

let with_recovered_host
      ?(product_source_selection = Product_source_selection.Selected)
      ?wrap_env
      ~phase
      f
  =
  Cold.with_seed_boundary
    ?wrap_env
    ~phase
    Stopped
    (fun env sw root daemon target saved counters ->
       (* Cold deliberately returns the original environment to its callback.
          This host constructs a new physical unit after recovery, so its actual
          Runtime_paths must retain the requested filesystem fault boundary. *)
       let env = Option.value_map wrap_env ~default:env ~f:(fun wrap -> wrap env) in
       (match target with
        | Indexed_target -> ()
        | Existing_target _ ->
          failwith "634 lower host requires actual cold indexed target");
       let store = Daemon.store daemon in
       let handle =
         Store.Session_store.open_session
           store
           ~sw
           ~actor_lock_nonce:(P.Id.Transaction.to_string (P.Id.Transaction.create ()))
           saved.session_id
         |> Existing.store_ok
       in
       let job_capacity =
         Agent_server.Job_capacity.create
           ~limits:
             (config root root (Filename.concat root "parent.chatmd")).server.job_limits
       in
       let recovery_owner =
         Recovery_owner.create ~store ~handle ~job_capacity ~capacity:None |> protocol_ok
       in
       protect_cleanup
         ~finally:(fun () -> Recovery_owner.close recovery_owner |> protocol_ok)
         (fun () ->
            let snapshot =
              Store.Snapshot.load_current
                ~env
                ~directory:(Store.Session_store.Handle.snapshot_directory handle)
                ~max_payload_length:1048576
              |> Existing.store_ok
              |> Option.value_exn
            in
            let initial =
              Persistence.restore_snapshot
                ~limits:Document_schema.Limits.default
                snapshot.snapshot
              |> Existing.store_ok
            in
            let journal =
              Store.Journal.open_existing
                ~env
                ~directory:(Store.Session_store.Handle.journal_directory handle)
                ~max_payload_length:1048576
                ~max_segment_bytes:4194304L
                ~max_segment_frames:32
              |> Existing.store_ok
            in
            let recovered =
              Store.Recovery.read
                ~env
                ~journal
                ~snapshot_directory:(Store.Session_store.Handle.snapshot_directory handle)
                ~max_snapshot_payload_length:1048576
                ~session_id:saved.session_id
                ~initial
                ~restore_snapshot:
                  (Persistence.restore_snapshot ~limits:Document_schema.Limits.default)
                ~apply:
                  (Persistence.apply_transaction ~limits:Document_schema.Limits.default)
                ~validate_transaction:
                  (Persistence.validate_transaction
                     ~limits:Document_schema.Limits.default)
                ~validate:Persistence.validate
              |> Existing.store_ok
            in
            assert (not recovered.repaired_crash_tail);
            let recovered_state = Persistence.Restored.state recovered.state in
            assert (
              Jsonaf.exactly_equal
                saved.checkpoint
                (Option.value_exn recovered_state.moderator));
            assert (P.Run_source.equal saved.source (Cold.Warm.source recovered_state));
            assert (
              A.Moderator_lifecycle.equal
                saved.lifecycle
                (A.Run_state.moderator_lifecycle
                   (Option.value_exn recovered_state.run_state)
                 |> Option.value_exn));
            assert (List.is_empty recovered_state.jobs);
            (* The real seed/reopened daemon both use these unchanged host defaults.
            A named/custom policy needs its actual resolver, never a fallback. *)
            assert (Option.is_none recovered_state.spec.runtime_policy);
            assert (Option.is_none recovered_state.spec.delegation);
            let writer =
              Store.Commit_writer.create
                ~sw
                ~journal
                ~session_id:saved.session_id
                ~next_transaction_sequence:
                  Int64.(recovered.latest_transaction_sequence + 1L)
                ~previous_transaction_hash:recovered.latest_transaction_hash
                ~queue_capacity:8
              |> Existing.store_ok
            in
            Recovery_owner.adopt_writer_exn recovery_owner writer;
            let persistence =
              Persistence.create
                ~archive:
                  (A.Compaction_archive.write_document
                     ~env
                     ~handle
                     ~max_payload_length:1048576)
                ~pending_archive:None
                ~before_commit:
                  (Some
                     (fun current ->
                       Store.Session_store.prepare_canonical_projection
                         store
                         handle
                         ~metadata:(Factory.metadata current)
                         ~entry:(Factory.index_entry current)
                       |> Result.map_error ~f:Store.Store_error.to_protocol_error))
                ~command_accepted:(fun _ _ -> ())
                ~writer
                ~durability:Flush
                ~limits:Document_schema.Limits.default
                ~archive_limits:Document_schema.Limits.default
                ~retention_preflight:None
                ~restored:recovered.state
                ~previous_transaction_hash:recovered.latest_transaction_hash
            in
            let source_owner = A.Runtime_source_owner.create () in
            let goal_descendant_profile =
              A.Goal_descendants.Profile.create
                ~max_scopes:8
                ~max_roots:16
                ~max_branches:16
                ~max_encoded_bytes:(256 * 1024)
                ~child_max_encoded_bytes:(1024 * 1024)
              |> protocol_ok
            in
            let actor =
              Actor.create_with_runtime_source_owner
                ~runtime_source_owner:(A.Runtime_source_owner.identity source_owner)
                ~ordinary_execution_admission:(fun () -> unexpected "ordinary execution")
                ~schedule_permission_timeouts:false
                ~sw
                ~clock:env#clock
                ~mailbox_capacity:32
                ~owner_lease_duration_ms:1000
                ~max_attachments:8
                ~subscriber_capacity:32
                ~compaction_env:(Some env)
                ~initial_state:recovered_state
                ~persistence:(Persistence.actor_persistence persistence)
                ~operation_worker:None
                ~services:
                  { now = P.Timestamp.now
                  ; monotonic_now = (fun () -> Eio.Time.Mono.now env#mono_clock)
                  ; create_attachment_id = P.Id.Attachment.create
                  ; create_reclaim_token =
                      (fun () -> P.Id.Transaction.to_string (P.Id.Transaction.create ()))
                  ; job_results = None
                  ; goal_descendant_profile
                  ; subscription_limits = A.Staged_subscriptions.default_limits
                  ; schedule_limits = A.Staged_schedules.default_limits
                  ; notification_limits = A.Staged_notifications.default_limits
                  ; ingress_limits = A.Staged_ingress.default_limits
                  ; state_committed =
                      (fun current _ ->
                        Store.Session_store.write_metadata
                          store
                          handle
                          ~entry:(Factory.index_entry current)
                          (Factory.metadata current)
                        |> Existing.store_ok)
                  }
            in
            Recovery_owner.adopt_actor_exn recovery_owner actor;
            let catalog = Factory.developer_catalog (Daemon.factory daemon) in
            let profile =
              Factory.Developer_catalog.find_profile
                catalog
                recovered_state.spec.permission_profile
              |> Option.value_exn
            in
            assert (
              String.equal
                profile.revision_digest
                recovered_state.spec.permission_profile_digest);
            let revision =
              match
                (List.hd_exn (A.Prompt_catalog.entries (Daemon.prompts daemon)))
                  .availability
              with
              | Ready revision -> revision
              | Disabled | Unavailable _ ->
                failwith "634 recovered captured revision unavailable"
            in
            assert (
              P.Id.Prompt_revision.equal
                (A.Prompt_revision.id revision)
                recovered_state.spec.prompt_revision_id);
            Store.Prompt_artifact_store.verify_tree
              ~root:(A.Prompt_revision.materialized_tree revision)
              (A.Prompt_revision.artifact revision)
            |> Existing.store_ok;
            let source =
              B.Source.prepare
                revision
                ~script_preparation:Daemon.default_options.script_preparation
                ~invocation:
                  (A.Runtime_script_preparation.Invocation.of_root
                     recovered_state.spec.root_invocation)
                ~env
                ~checkpoint:Eio.Fiber.check
              |> protocol_ok
            in
            B.Source.validate_target source recovered_state |> protocol_ok;
            Resource.with_probe ~actor ~handle ~source ~profile (fun cell ->
              let t =
                { env
                ; sw
                ; root
                ; daemon
                ; actor
                ; handle
                ; writer
                ; recovered_state
                ; recovery_owner
                ; source_owner
                ; revision
                ; source
                ; product_source_selection
                ; missing_product_source = ref None
                ; profile
                ; cell
                ; runtime = ref None
                ; phase
                ; counters
                ; lifecycle_calls = ref 0
                ; hooks = no_hooks
                }
              in
              let active_native = ref None in
              let access =
                Resource.policy_access
                  cell
                  ~restore:(fun ~stage ~request ~governance ~authorize:_ ->
                    match !active_native with
                    | None -> unexpected "restoration outside genuine native extent"
                    | Some (native : Cold.native_request) ->
                      if
                        not
                          (phys_equal request native.request
                           && phys_equal governance native.governance)
                      then unexpected "foreign native extent"
                      else
                        with_construction
                          t
                          native
                          ~admission:(Owner_stage stage)
                          (fun constructed ->
                             let loan = publish_cell constructed |> protocol_ok in
                             let caller_loan = ref (Some loan) in
                             protect_cleanup
                               ~finally:(fun () ->
                                 Option.iter !caller_loan ~f:(fun loan ->
                                   Resource.Loan.close loan |> protocol_ok))
                               (fun () ->
                                  check_loaded_source t loan;
                                  t.hooks.before_publish ();
                                  let publication =
                                    Scope.publish
                                      stage
                                      ~restoration:constructed.restoration
                                      ~loan
                                      ~access:constructed.access
                                  in
                                  (match Scope.Publication.custody publication with
                                   | Caller_retains -> ()
                                   | Owner_retains -> caller_loan := None);
                                  t.hooks.observe_publication publication;
                                  Scope.Publication.resume publication)))
              in
              let runtime =
                Owner.create_with_deferred_run_unload
                  ~native_resource_access:None
                  ~source_owner
                  ~actor
                  ~initial:None
                  ~before_unload:(fun ~closing:_ ->
                    let current = Actor.state actor |> protocol_ok in
                    if
                      List.is_empty current.jobs && Option.is_none current.spec.delegation
                    then Ok ()
                    else unexpected "nonempty dependency closure")
                  ~policy_resource_cleanup:(Resource.cleanup_port cell)
                  ~policy_resource_access:(Some access)
                  ~build:(fun ~initialization_cleanup:_ ~original_actor:_ _ ->
                    unexpected "ordinary build")
                  ~build_deferred:
                    (fun
                      ~initialization_cleanup:_
                      ~source_consumer:_
                      ~source:_
                      ~expected_product:_
                      ~construction:_ -> unexpected "Run build")
                  ~ordinary_warm:None
                  ~delegated_load:None
              in
              t.runtime := Some runtime;
              Recovery_owner.adopt_runtime_exn recovery_owner runtime;
              (* The active record is lexical transport, not an authority issuer. *)
              let with_request ~original f =
                let callback_failure = ref None in
                let outcome =
                  capture (fun () ->
                    Cold.with_native_request
                      ~phase
                      env
                      daemon
                      ~target_actor:actor
                      recovered_state
                      ~original
                      ~f:(fun native ->
                        assert (Option.is_none !active_native);
                        active_native := Some native;
                        protect_cleanup
                          ~finally:(fun () -> active_native := None)
                          (fun () ->
                             match f native with
                             | value -> value
                             | exception exn ->
                               let bt = Stdlib.Printexc.get_raw_backtrace () in
                               callback_failure := Some (exn, bt);
                               (try
                                  Eio.traceln
                                    "644 native callback failed at %s: %s"
                                    !phase
                                    (Exn.to_string exn)
                                with
                                | _ -> ());
                               Exn.raise_with_original_backtrace exn bt)))
                in
                (* Native execution projects callback exceptions to handler_failed.
                 Keep its genuine success expectation and real caller cleanup,
                 then re-raise the original failing callback with its raw trace. *)
                match !callback_failure with
                | Some failure -> resume (Error failure)
                | None -> resume outcome
              in
              f t with_request;
              [%test_eq: int] 0 !(t.lifecycle_calls);
              [%test_eq: int] 0 !(counters.providers);
              [%test_eq: int]
                0
                (List.length (Cold.Warm.completed (state t) Session_resume)))))
;;
