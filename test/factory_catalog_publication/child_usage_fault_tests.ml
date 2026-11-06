open! Core
open Agent_server_test_support
module R = Retry_support
module A = Agent_session
module Actor = A.Session_actor
module Factory = Agent_server.Session_factory
module Daemon = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module P = Agent_protocol
module D = Agent_store.Delegation_store
module Existing = Source_preparation_tests

(* Faults select the actual parent's bounded association or paired GD leaf.
   Distinct frontier and initial-snapshot cuts prove no pre-import Init. *)
module Fault = struct
  module Target = struct
    type t =
      | Frontier
      | Initial_usage
      | Input of int64
  end

  type t =
    { mutable parent : P.Id.Session.t option
    ; key : P.Idempotency_key.t
    ; mutable reference : D.Reference.t option
    ; mutable target : Target.t
    ; mutable hits : int
    }

  let create () =
    { parent = None
    ; key = P.Idempotency_key.of_string "553-slot-unknown-ack" |> protocol_ok
    ; reference = None
    ; target = Target.Frontier
    ; hits = 0
    }
  ;;

  let rec leaves = function
    | A.Session_delta.Batch changes -> List.concat_map changes ~f:leaves
    | delta -> [ delta ]
  ;;

  let reached t env path =
    match t.parent with
    | None -> ()
    | Some parent ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Agent_store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
         let transaction =
           Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
           |> Existing.store_ok
         in
         if P.Id.Session.equal transaction.session_id parent
         then (
           let delta =
             A.Session_delta_document.decode
               ~limits:Document_schema.Limits.default
               transaction.delta
             |> R.document_ok
             |> A.Session_delta_document.value
           in
           let children_of_delta = function
             | A.Session_delta.Child_contributions_changed children ->
               (match t.target with
                | Target.Frontier | Input _ -> Some children
                | Initial_usage -> None)
             | Goal_descendants_changed update ->
               let matches =
                 match t.target, A.Goal_descendants.Update.reason update with
                 | Initial_usage, Import _ -> true
                 | ( Initial_usage
                   , ( Register_new_root _
                     | Register_first_unrelated_child _
                     | Register_child _
                     | Register_branch _
                     | Register_retained_branch _
                     | Reserve _
                     | Record_included _
                     | Close_before_writer _ ) ) -> false
                 | (Frontier | Input _), _ -> true
               in
               if not matches
               then None
               else (
                 match A.Goal_descendants.Update.children update with
                 | Preserve -> None
                 | Replace children -> Some children)
             | _ -> None
           in
           let reference =
             leaves delta
             |> List.filter_map ~f:children_of_delta
             |> List.find_map ~f:(fun children ->
               A.Child_contributions.references children
               |> List.find ~f:(fun reference ->
                 P.Id.Session.equal reference.D.Reference.key.parent_session_id parent
                 && P.Idempotency_key.equal reference.key.idempotency_key t.key
                 &&
                 match t.target with
                 | Frontier -> true
                 | Initial_usage ->
                   (match
                      A.Child_contributions.snapshot children ~reference |> protocol_ok
                    with
                    | None -> false
                    | Some snapshot ->
                      A.Child_usage_snapshot.Partition.equal
                        (A.Child_usage_snapshot.own snapshot)
                        A.Child_usage_snapshot.Partition.empty
                      && Option.is_some (A.Child_usage_snapshot.goal_scopes snapshot))
                 | Input input ->
                   let partition =
                     A.Child_contributions.aggregate children
                     |> protocol_ok
                     |> A.Child_usage_snapshot.Descendants.partition
                   in
                   let actual =
                     A.Child_usage_snapshot.Partition.usage partition
                     |> fun usage ->
                     A.Goal_usage.component usage Input |> A.Goal_usage.Metric.actual
                   in
                   P.Inference_query.Metric.equal_sum (Tokens input) actual))
           in
           Option.iter reference ~f:(fun reference ->
             t.parent <- None;
             t.reference <- Some reference;
             t.hits <- t.hits + 1;
             raise (Core_unix.Unix_error (EIO, "553 actual parent slot sync", path))))
       | Some _ | None -> ())
  ;;
end

let definition env root runtime =
  let native = Option.value_exn runtime.A.Runtime_builder.native_runtime in
  let capabilities =
    Lazy.force native.capabilities
    |> Result.map_error ~f:(fun error -> error.Chat_response.Tool_capability.message)
    |> Result.ok_or_failwith
  in
  let bundle =
    Chatmd_source_bundle.create
      ~root_file:"child.chatmd"
      ~sources:
        [ ( "child.chatmd"
          , "<config model=\"fixture-model\"/><developer>553 actual retained \
             child.</developer>" )
        ]
      ()
    |> Result.ok_or_failwith
  in
  A.Generated_definition.prepare
    ~env
    ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
    ~revision_id:(P.Id.Prompt_revision.create ())
    ~created_at:(P.Timestamp.now ())
    ~current_capabilities:(fun () -> capabilities)
    ~references:[]
    bundle
  |> Result.map_error ~f:(fun errors ->
    P.Error.invalid_request
      (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
       |> String.concat ~sep:"\n"))
  |> protocol_ok
;;

let create_child factory parent key definition =
  Factory.create_generated_session
    ~start_immediately:true
    factory
    ~parent_session_id:parent
    ~idempotency_key:key
    ~display_name:None
    definition
;;

(* Periodic idle work may hold the real parent's resource lease. Retry only
   explicit retryable contention, preserving this immutable request/key and all
   persistence/authority errors. The enclosing real fixture bounds this loop. *)
let rec create_after_idle factory parent key definition =
  match create_child factory parent key definition with
  | Ok _ as result -> result
  | Error { code = Conflict; retryable = true; _ } ->
    Eio.Fiber.yield ();
    create_after_idle factory parent key definition
  | Error _ as failure -> failure
;;

let has_slot state reference =
  Option.exists state.A.Session_state.child_contributions ~f:(fun children ->
    List.mem
      (A.Child_contributions.references children)
      reference
      ~equal:D.Reference.equal)
;;

let with_reopened env sw root daemon policy f =
  let store = Daemon.store daemon in
  Daemon.shutdown daemon |> protocol_ok;
  assert (Agent_store.Session_store.is_closed store);
  let recovered =
    Daemon.start
      ~sw
      ~env:(env :> Eio_unix.Stdenv.base)
      ~config:(config root root (Filename.concat root "parent.chatmd"))
      ~tool_dir:root
      ~home:root
      ~process_start_identity:None
      ~options:
        { Daemon.default_options with
          inference_policy = policy
        ; qualify_chatml_extensions = true
        }
      ()
    |> protocol_ok
  in
  R.protect_cleanup
    ~finally:(fun () -> Daemon.shutdown recovered |> protocol_ok)
    (fun () ->
       assert (not (phys_equal store (Daemon.store recovered)));
       f recovered)
;;

let reopen env sw root daemon policy before reference definition fault counters =
  with_reopened env sw root daemon policy (fun recovered ->
    let parent =
      Registry.load (Daemon.registry recovered) before.A.Session_state.identity.session_id
      |> protocol_ok
    in
    ignore (Retry_rotation_tests.await_recovered parent before : A.Session_state.t);
    let child =
      create_after_idle
        (Daemon.factory recovered)
        before.identity.session_id
        fault.Fault.key
        definition
      |> protocol_ok
    in
    assert (
      P.Id.Session.equal
        (R.state child).identity.session_id
        reference.D.Reference.child_session_id);
    (* Starting the recovered child loads its runtime; only this explicit,
          authenticated ordinary input may dispatch its first provider. Keep the
          actual writer lexical until the operation and retry oracles join. *)
    [%test_eq: int] 0 !(counters.R.providers);
    Goal_native_test_support.with_ordinary_owner
      recovered
      child
      ~v1:false
      (fun ordinary_receipt ->
         assert (Option.is_none ordinary_receipt);
         Dispatch_race_tests.await env (fun () ->
           let current = Actor.state child.actor |> protocol_ok in
           Option.some_if
             (Option.is_none current.active_operation
              && not (List.is_empty (A.Inference_ledger.rows current.inference_ledger)))
             ());
         let current = R.state parent in
         assert (has_slot current reference);
         let children = Option.value_exn current.child_contributions in
         let coverage =
           A.Child_contributions.aggregate children
           |> protocol_ok
           |> A.Child_usage_snapshot.Descendants.coverage
         in
         assert (
           not (A.Child_usage_snapshot.Coverage.historical_children_unknown coverage));
         [%test_eq: int64]
           0L
           (A.Child_usage_snapshot.Coverage.unobserved_children coverage);
         [%test_eq: int] 1 !(counters.R.providers);
         let child_before = R.state child in
         let retry =
           create_after_idle
             (Daemon.factory recovered)
             before.identity.session_id
             fault.key
             definition
           |> protocol_ok
         in
         assert (phys_equal child.actor retry.actor);
         Existing.assert_same child_before child;
         [%test_eq: int] 1 !(counters.providers);
         [%test_eq: int] 1 fault.hits))
;;

exception Observed_slot_failure of P.Error.t

let run boundary ~initial =
  let fault = Fault.create () in
  fault.target <- (if initial then Initial_usage else Frontier);
  match
    R.with_recoverable_fixture
      ~wrap_env:(fun env ->
        Crash_fault_io.wrap
          env
          ~boundary
          ~matches:(fun path ->
            String.is_substring path ~substring:"/journal/"
            && String.is_suffix path ~suffix:".log")
          ~reached:(Fault.reached fault env))
      (fun env sw root daemon policy parent runtime _revision counters ->
         let before = R.state parent in
         let definition = definition env root runtime in
         fault.parent <- Some before.identity.session_id;
         let failure =
           match
             create_after_idle
               (Daemon.factory daemon)
               before.identity.session_id
               fault.key
               definition
           with
           | Error failure -> failure
           | Ok _ -> failwith "failed parent slot ACK published/started its child"
         in
         [%test_eq: P.Error.code] Persistence_error failure.code;
         [%test_eq: int] 1 fault.hits;
         [%test_eq: int] 0 !(counters.providers);
         let reference = Option.value_exn fault.reference in
         let record =
           D.resolve
             (Agent_store.Session_store.delegations (Daemon.store daemon))
             reference
           |> Existing.store_ok
         in
         assert (
           D.equal_stage (if initial then Artifact_installed else Reserved) record.stage);
         assert (Option.is_none record.revocation);
         assert (
           Option.is_none
             (Registry.find (Daemon.registry daemon) reference.child_session_id));
         let before_unknown_ack =
           if not initial
           then (
             Existing.assert_same before parent;
             before)
           else (
             let current = R.state parent in
             [%test_eq: int64]
               Int64.(before.counters.revision + 1L)
               current.counters.revision;
             assert (has_slot current reference);
             let children = Option.value_exn current.child_contributions in
             assert (
               Option.is_none
                 (A.Child_contributions.snapshot children ~reference |> protocol_ok));
             current)
         in
         reopen
           env
           sw
           root
           daemon
           policy
           before_unknown_ack
           reference
           definition
           fault
           counters;
         raise (Observed_slot_failure failure))
  with
  | () -> failwith "actual parent slot fault was not reached"
  | exception Observed_slot_failure failure ->
    [%test_eq: P.Error.code] Persistence_error failure.code
;;

let%expect_test
    "actual parent slot unknown ACK retains child identity before link/start and \
     physical reopen"
  =
  List.iter [ false; true ] ~f:(fun initial ->
    run Before_sync ~initial;
    run After_sync ~initial);
  print_endline
    "frontier and actual zero-import cuts: no pre-ACK Init; physical recovery same \
     child, one first delivery, no retry replay";
  [%expect
    {| frontier and actual zero-import cuts: no pre-ACK Init; physical recovery same child, one first delivery, no retry replay |}]
;;

module Observation = Inference.Observation

let observed result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (Observation.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

(* Host correction of the actual retained provider row, never an invented
   allocation or a foreign Handle copied into the parent ledger. *)
let correction handle revision input =
  let actual = Observation.Count.create (Actual input) |> observed in
  let unknown = Observation.Count.create (Unknown Not_reported) |> observed in
  let usage =
    Observation.Usage.create
      ~counts:
        { input = actual
        ; output = unknown
        ; reported_total = unknown
        ; cached_input = unknown
        ; cache_write_input = unknown
        ; reasoning_output = unknown
        }
      ~inclusions:[]
    |> observed
  in
  Observation.create
    ~scope:(A.Inference_ledger.Handle.scope handle)
    ~id:(A.Inference_ledger.Handle.accounting_id handle)
    ~revision
    ~payload:(Usage usage)
    ~limits:Observation.Admission.observation
  |> observed
;;

let contributed_input (state : A.Session_state.t) =
  Option.value_exn state.child_contributions
  |> A.Child_contributions.aggregate
  |> protocol_ok
  |> A.Child_usage_snapshot.Descendants.partition
  |> A.Child_usage_snapshot.Partition.usage
  |> fun usage -> A.Goal_usage.component usage Input |> A.Goal_usage.Metric.actual
;;

let import daemon parent child =
  let authorize expected (current : A.Session_state.t) =
    if P.Id.Session.equal expected current.identity.session_id
    then Ok ()
    else Error (P.Error.invalid_request "child usage read targets another actual owner")
  in
  Factory.import_child_usage
    (Daemon.factory daemon)
    ~parent
    ~child
    ~actor:R.original
    ~authorize_parent:(authorize (R.state parent).identity.session_id)
    ~authorize_child:(authorize (R.state child).identity.session_id)
;;

let assert_input expected state =
  assert (P.Inference_query.Metric.equal_sum (Tokens expected) (contributed_input state))
;;

let run_import boundary =
  let fault = Fault.create () in
  match
    R.with_recoverable_fixture
      ~wrap_env:(fun env ->
        Crash_fault_io.wrap
          env
          ~boundary
          ~matches:(fun path ->
            String.is_substring path ~substring:"/journal/"
            && String.is_suffix path ~suffix:".log")
          ~reached:(Fault.reached fault env))
      (fun env sw root daemon policy parent runtime _revision counters ->
         let definition = definition env root runtime in
         let parent_id = (R.state parent).identity.session_id in
         let child =
           create_after_idle (Daemon.factory daemon) parent_id fault.key definition
           |> protocol_ok
         in
         (* This is the real child's only input and provider. Detach its writer
          BEFORE arming the import fault and closing the physical old Store. *)
         Goal_native_test_support.with_ordinary_owner
           daemon
           child
           ~v1:false
           (fun receipt ->
              assert (Option.is_none receipt);
              Dispatch_race_tests.await env (fun () ->
                let current = Actor.state child.actor |> protocol_ok in
                Option.some_if
                  (Option.is_none current.active_operation
                   && not
                        (List.is_empty (A.Inference_ledger.rows current.inference_ledger))
                  )
                  ()));
         [%test_eq: int] 1 !(counters.providers);
         let current = R.state child in
         let row =
           match A.Inference_ledger.rows current.inference_ledger with
           | [ row ] -> row
           | [] | _ :: _ :: _ ->
             failwith "child did not retain its one actual provider row"
         in
         let handle = A.Inference_ledger.Row.handle row in
         let usage_revision =
           A.Inference_ledger.Row.record row
           |> Observation.Attempt_record.observations
           |> List.filter_map ~f:(fun observation ->
             match Observation.payload observation with
             | Usage _ -> Some (Observation.revision observation)
             | Configuration _ | Context_estimate _ | Transport_selection _ | Diagnostic _
               -> None)
           |> List.fold ~init:0L ~f:Int64.max
         in
         let reference = Option.value_exn current.spec.delegation in
         Actor.observe_inference
           child.actor
           ~handle
           (correction handle Int64.(usage_revision + 1L) 7L)
         |> protocol_ok;
         (match import daemon parent child |> protocol_ok with
          | Imported Replaced -> ()
          | Imported (Added | Duplicate | Stale) | Historical_absence ->
            failwith "provider correction did not replace the actual initial zero");
         let seven = R.state parent in
         assert_input 7L seven;
         Actor.observe_inference
           child.actor
           ~handle
           (correction handle Int64.(usage_revision + 2L) 11L)
         |> protocol_ok;
         (* Arm only the typed replacement leaf containing the actual eleven-token
          snapshot. No provider, registration, or unrelated parent write matches. *)
         fault.target <- Input 11L;
         fault.parent <- Some parent_id;
         let failure =
           match import daemon parent child with
           | Error failure -> failure
           | Ok _ -> failwith "selected import ACK fault unexpectedly succeeded"
         in
         [%test_eq: P.Error.code] Persistence_error failure.code;
         [%test_eq: int] 1 fault.hits;
         assert (D.Reference.equal reference (Option.value_exn fault.reference));
         Existing.assert_same seven parent;
         [%test_eq: int] 1 !(counters.providers);
         with_reopened env sw root daemon policy (fun recovered ->
           let parent =
             Registry.load (Daemon.registry recovered) parent_id |> protocol_ok
           in
           ignore (Retry_rotation_tests.await_recovered parent seven : A.Session_state.t);
           let child =
             create_after_idle (Daemon.factory recovered) parent_id fault.key definition
             |> protocol_ok
           in
           let recovered_child = R.state child in
           assert (
             P.Id.Session.equal
               recovered_child.identity.session_id
               reference.child_session_id);
           assert (
             D.Reference.equal
               reference
               (Option.value_exn recovered_child.spec.delegation));
           (* Before_sync and After_sync are both unknown to the old caller. A
            recovered exact replacement may already exist; import actual current
            child evidence and prove eleven once, never seven+eleven or twice11. *)
           (match import recovered parent child |> protocol_ok with
            | Imported Duplicate -> ()
            | Imported (Added | Replaced | Stale) | Historical_absence ->
              failwith
                "Factory retained replay did not already import actual current child");
           let eleven = R.state parent in
           assert_input 11L eleven;
           (match import recovered parent child |> protocol_ok with
            | Imported Duplicate -> ()
            | Imported (Added | Replaced | Stale) | Historical_absence ->
              failwith "exact recovered child usage did not duplicate");
           Existing.assert_same eleven parent;
           let before_retry = R.state child in
           let same =
             create_after_idle (Daemon.factory recovered) parent_id fault.key definition
             |> protocol_ok
           in
           assert (phys_equal child.actor same.actor);
           Existing.assert_same before_retry same;
           [%test_eq: int] 1 !(counters.providers);
           [%test_eq: int] 1 fault.hits);
         raise (Observed_slot_failure failure))
  with
  | () -> failwith "actual parent replacement fault was not reached"
  | exception Observed_slot_failure failure ->
    [%test_eq: P.Error.code] Persistence_error failure.code
;;

let%expect_test
    "actual replacement unknown ACK reopens and replaces seven with eleven once"
  =
  run_import Before_sync;
  run_import After_sync;
  print_endline
    "both import cuts: strict physical reopen, 7 -> 11 once, duplicate and creation \
     retry no replay";
  [%expect
    {| both import cuts: strict physical reopen, 7 -> 11 once, duplicate and creation retry no replay |}]
;;

module Historical_unrelated = struct
  module S = Agent_store
  module J = Document_schema

  let admission record =
    S.Delegation_document.of_record record
    |> R.document_ok
    |> S.Delegation_document.to_document
    |> R.document_ok
    |> J.Document.payload
    |> fun payload ->
    match J.Json.field payload ~name:"admission" with
    | Value admission -> Jsonaf.to_string admission
    | Absent | Null -> failwith "actual private admission is missing"
  ;;

  let assert_frontier parent reference =
    let children = Option.value_exn parent.A.Session_state.child_contributions in
    assert (
      Option.exists
        (A.Child_contributions.origin children ~reference |> protocol_ok)
        ~f:(S.Delegation_goal_origin.equal S.Delegation_goal_origin.unrelated))
  ;;

  (* Only the first physical image is authored. All runtime authority below is
     issued later by the ordinary Factory recovery/source path. *)
  let install env sw store (parent : A.Session_state.t) definition record =
    let reference = D.reference record in
    let history, next =
      A.Generated_definition.initial_history
        definition
        ~session_id:record.D.admission.child_session_id
      |> protocol_ok
    in
    let inference_target =
      match record.admission.inference_target with
      | Some target -> Inference.Selection.captured target ~limits:J.Limits.default
      | None -> Inference.Selection.unresolved ~limits:J.Limits.default
    in
    let inference_target =
      inference_target
      |> Result.map_error ~f:(fun error ->
        Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
      |> Result.ok_or_failwith
    in
    let protocol =
      P.Session.Spec.create
        ~execution_host:Daemon
        ~prompt:(Generated reference.revision_id)
        ~workspace:parent.A.Session_state.spec.protocol.workspace
        ~liveness:Detached
        ~persistence:Durable
        ~permission_profile:parent.spec.permission_profile
        ~start_immediately:true
        ~labels:[]
        ()
      |> protocol_ok
    in
    let initial =
      A.Session_state.create
        ~identity:
          { session_id = reference.child_session_id
          ; display_name = None
          ; creating_principal = Some record.key.principal_id
          ; labels = []
          ; organization = P.Session_organization.Values.empty
          ; created_at = record.admission.created_at
          ; updated_at = record.admission.created_at
          ; generation = 0
          ; metadata_revision = 0L
          }
        ~spec:
          { parent.spec with
            protocol
          ; prompt_definition_id = None
          ; prompt_revision_id = reference.revision_id
          ; configuration_revision = 0L
          ; inference_target
          ; delegation = Some reference
          ; quota_key = None
          }
        ~initial_history:(List.map history ~f:A.History_codec.to_protocol)
    in
    let initial =
      { initial with
        parent_stop_epoch = record.admission.parent_stop_epoch
      ; conversation =
          { initial.conversation with
            next_history_sequence = Int64.of_int next
          ; reserved_history_through = Int64.of_int next
          }
      }
    in
    A.Session_state.validate initial |> protocol_ok;
    let old_state state =
      let payload =
        A.Session_state_document.authored state
        |> A.Session_state_document.encode ~limits:J.Limits.default
        |> R.document_ok
        |> J.Document.payload
        |> function
        | `Object fields ->
          `Object
            (List.filter_map fields ~f:(fun (name, value) ->
               if String.equal name "goal_descendants"
               then None
               else
                 Some
                   ( name
                   , if String.equal name "schema_version" then `Number "26" else value ))
             @ [ "future_historical_origin", `String "retained" ])
        | _ -> failwith "initial State is not an object"
      in
      J.Document.create
        ~limits:J.Limits.default
        ~kind:"session.state"
        ~version:11
        ~payload
      |> R.document_ok
    in
    S.Session_store.create_session_initialized
      store
      ~sw
      ~transaction_id:record.admission.transaction_id
      ~actor_lock_nonce:(P.Id.Transaction.create () |> P.Id.Transaction.to_string)
      ~initialize:(fun ~staging_directory ->
        let delta =
          J.Document.create
            ~limits:J.Limits.default
            ~kind:"session.delta"
            ~version:7
            ~payload:
              (`Object
                  [ ( "changes"
                    , `Array
                        [ `Object
                            [ "kind", `String "created"
                            ; "state", J.Document.json (old_state initial)
                            ]
                        ] )
                  ])
          |> R.document_ok
        in
        let first =
          S.Transaction.create
            ~limits:J.Limits.default
            ~session_id:reference.child_session_id
            ~generation:0
            ~transaction_sequence:1L
            ~previous_transaction_hash:None
            ~session_revision:1L
            ~first_event_sequence:None
            ~last_event_sequence:None
            ~accepted_at_ns:
              (P.Timestamp.to_time_ns record.admission.created_at
               |> Time_ns.to_int_ns_since_epoch
               |> Int64.of_int)
            ~command_audit:None
            ~delta
            ~durable_events:[]
          |> Existing.store_ok
        in
        let bytes = " \n" ^ S.Transaction.encode first ^ "\n" in
        let journal =
          S.Journal.create
            ~env
            ~directory:(Filename.concat staging_directory "journal")
            ~max_payload_length:1048576
            ~max_segment_bytes:4194304L
            ~max_segment_frames:16
          |> Existing.store_ok
        in
        ignore
          (S.Journal.append journal ~durability:Flush ~flags:0 ~payload:bytes
           |> Existing.store_ok
           : S.Journal.append_result);
        let state =
          { initial with
            counters = { initial.counters with revision = 1L; transaction_sequence = 1L }
          }
        in
        let snapshot =
          S.Snapshot.create
            ~limits:J.Limits.default
            ~session_id:reference.child_session_id
            ~transaction_sequence:1L
            ~transaction_hash:(Some (S.Document_record.digest bytes))
            ~event_sequence:0L
            ~created_at:record.admission.created_at
            ~prompt_artifact:(P.Id.Prompt_revision.to_string reference.revision_id)
            ~workspace_identity:state.spec.workspace_instance.conflict_domain
            ~payload:(old_state state)
          |> Existing.store_ok
        in
        (* Session_store owns and creates the staging snapshot directory. *)
        ignore
          (S.Snapshot.install
             snapshot
             ~env
             ~directory:(Filename.concat staging_directory "snapshot")
             ~max_payload_length:1048576
           |> Existing.store_ok
           : S.Snapshot.installed);
        let restored =
          A.Session_persistence.restore_snapshot ~limits:J.Limits.default snapshot
          |> Existing.store_ok
        in
        A.Session_persistence.validate restored |> Existing.store_ok;
        S.Durable_file.sync_directory ~env ~path:staging_directory |> Existing.store_ok;
        let current = A.Session_persistence.Restored.state restored in
        S.Session_store.Initial_projection.create
          ~metadata:(Agent_server.Session_projection.metadata current)
          ~entry:(Agent_server.Session_projection.index_entry current))
    |> Existing.store_ok
  ;;

  let assert_saved env directory =
    let saved =
      S.Snapshot.load_current ~env ~directory ~max_payload_length:(64 * 1024 * 1024)
      |> Existing.store_ok
      |> Option.value_exn
    in
    let payload = J.Document.payload saved.snapshot.payload in
    (match J.Json.field payload ~name:"goal_descendants" with
     | Absent -> ()
     | Null | Value _ -> failwith "historical own GD omission was rewritten");
    match J.Json.field payload ~name:"future_historical_origin" with
    | Value (`String "retained") -> ()
    | Absent | Null | Value _ -> failwith "historical unknown field was lost"
  ;;
end

let%expect_test
    "actual private Unrelated restores historical own omission and admits one Turn"
  =
  let fault = Fault.create () in
  match
    R.with_recoverable_fixture
      ~wrap_env:(fun env ->
        Crash_fault_io.wrap
          env
          ~boundary:Before_sync
          ~matches:(fun path ->
            String.is_substring path ~substring:"/journal/"
            && String.is_suffix path ~suffix:".log")
          ~reached:(Fault.reached fault env))
      (fun env sw root daemon policy parent runtime _revision counters ->
         let before = R.state parent in
         let definition = definition env root runtime in
         fault.parent <- Some before.identity.session_id;
         let failure =
           create_after_idle
             (Daemon.factory daemon)
             before.identity.session_id
             fault.key
             definition
           |> R.rejected
         in
         [%test_eq: P.Error.code] Persistence_error failure.code;
         let reference = Option.value_exn fault.reference in
         let resolve daemon =
           D.resolve
             (Agent_store.Session_store.delegations (Daemon.store daemon))
             reference
           |> Existing.store_ok
         in
         let original = resolve daemon in
         assert (
           D.equal_stage original.stage Reserved && Option.is_none original.revocation);
         assert (
           Option.exists
             original.admission.goal_origin
             ~f:
               (Agent_store.Delegation_goal_origin.equal
                  Agent_store.Delegation_goal_origin.unrelated));
         let original_admission = Historical_unrelated.admission original in
         with_reopened env sw root daemon policy (fun recovered ->
           let store = Daemon.store recovered in
           let parent =
             Registry.load (Daemon.registry recovered) before.identity.session_id
             |> protocol_ok
           in
           Agent_server.Runtime_owner.drain_idle_moderator parent.runtime
           |> protocol_ok
           |> ignore;
           let current = Retry_rotation_tests.await_recovered parent before in
           Historical_unrelated.assert_frontier current reference;
           let artifacts =
             Agent_store.Prompt_artifact_store.create
               ~env
               ~root:
                 (Agent_store.Data_root.prompt_artifacts_path
                    (Agent_store.Session_store.data_root store))
             |> Existing.store_ok
           in
           let generated result =
             result
             |> Result.map_error ~f:(fun diagnostics ->
               List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
               |> String.concat ~sep:"\n")
             |> Result.ok_or_failwith
           in
           let definition =
             A.Generated_definition.with_identity
               definition
               ~revision_id:reference.revision_id
               ~created_at:(resolve recovered).admission.created_at
             |> generated
           in
           let record =
             A.Generated_definition.install_reserved
               ~delegations:(Agent_store.Session_store.delegations store)
               ~reservation:(resolve recovered)
               ~artifact_store:artifacts
               definition
             |> generated
           in
           let handle =
             Historical_unrelated.install env sw store current definition record
           in
           let directory = Agent_store.Session_store.Handle.snapshot_directory handle in
           Historical_unrelated.assert_saved env directory;
           let metadata =
             Agent_store.Session_store.Handle.metadata_checked handle |> Existing.store_ok
           in
           assert (P.Id.Session.equal metadata.session.id reference.child_session_id);
           [%test_eq: string]
             metadata.prompt_artifact
             (P.Id.Prompt_revision.to_string reference.revision_id);
           let record =
             D.advance
               (Agent_store.Session_store.delegations store)
               (resolve recovered)
               Child_installed
             |> Existing.store_ok
           in
           let moderator =
             Agent_server.Runtime_owner.with_loaded_runtime parent.runtime (fun runtime ->
               A.Runtime_builder.moderation_source runtime)
             |> protocol_ok
           in
           Actor.checkpoint parent.actor ~persist:(fun current ->
             let open Result.Let_syntax in
             let%bind fingerprint =
               A.Delegation_authority.fingerprint ?moderator current
             in
             Historical_unrelated.assert_frontier current reference;
             assert (
               Int.equal current.identity.generation record.key.parent_generation
               && Int64.equal current.stop_epoch before.stop_epoch
               && P.Session.equal_desired_state current.lifecycle.desired Running
               && (not current.halted)
               && Option.is_none current.failure);
             [%test_eq: string] record.admission.authority_sha256 fingerprint;
             D.advance (Agent_store.Session_store.delegations store) record Linked
             |> Result.map ~f:ignore
             |> Result.map_error ~f:(fun error ->
               P.Error.invalid_request
                 (Sexp.to_string_hum [%sexp (error : Agent_store.Store_error.t)])))
           |> protocol_ok;
           Agent_store.Session_store.close_session store handle |> Existing.store_ok;
           (* Physical installation occurred after daemon startup indexed its
              inventory. Publish only that actual Store projection; the normal
              Registry loader still reconstructs and authenticates the owner. *)
           let indexed =
             Agent_store.Session_index.find
               (Agent_store.Session_store.session_index store)
               reference.child_session_id
             |> Option.value_exn
           in
           Registry.index (Daemon.registry recovered) indexed;
           let child =
             Registry.load (Daemon.registry recovered) reference.child_session_id
             |> protocol_ok
           in
           assert (Option.is_none (R.state child).goal_descendants);
           [%test_eq: int] 0 !(counters.providers);
           Goal_native_test_support.with_ordinary_owner
             recovered
             child
             ~v1:false
             (fun receipt ->
                assert (Option.is_none receipt);
                Dispatch_race_tests.await env (fun () ->
                  let current = R.state child in
                  Option.some_if
                    (Option.is_none current.active_operation
                     && not
                          (List.is_empty
                             (A.Inference_ledger.rows current.inference_ledger)))
                    ()));
           [%test_eq: int] 1 !(counters.providers);
           let completed = R.state child in
           assert (Option.is_none completed.goal_descendants);
           (match A.Inference_ledger.rows completed.inference_ledger with
            | [ row ] ->
              (match
                 Inference.Observation.Attempt_record.state
                   (A.Inference_ledger.Row.record row)
               with
               | Terminal terminal ->
                 assert (
                   Inference.Event.Terminal.equal_outcome
                     (Inference.Event.Terminal.outcome terminal)
                     Completed)
               | Prepared | Running | Interrupted _ ->
                 failwith "historical child Turn did not complete")
            | [] | _ :: _ :: _ ->
              failwith "historical child did not admit exactly one provider");
           [%test_eq: string]
             original_admission
             (Historical_unrelated.admission (resolve recovered));
           assert (D.Reference.equal reference (D.reference (resolve recovered)));
           with_reopened env sw root recovered policy (fun reopened ->
             Historical_unrelated.assert_saved env directory;
             ignore
               (Registry.load (Daemon.registry reopened) before.identity.session_id
                |> protocol_ok
                : Registry.entry);
             let child =
               Registry.load (Daemon.registry reopened) reference.child_session_id
               |> protocol_ok
             in
             assert (Option.is_none (R.state child).goal_descendants);
             [%test_eq: string]
               original_admission
               (Historical_unrelated.admission (resolve reopened));
             assert (D.Reference.equal reference (D.reference (resolve reopened)));
             [%test_eq: int] 1 !(counters.providers)));
         raise (Observed_slot_failure failure))
  with
  | () -> failwith "actual Factory frontier fault was not reached"
  | exception Observed_slot_failure failure ->
    [%test_eq: P.Error.code] Persistence_error failure.code;
    print_endline
      "actual private Unrelated, historical omission, one Turn, physical reopen";
    [%expect
      {| actual private Unrelated, historical omission, one Turn, physical reopen |}]
;;
