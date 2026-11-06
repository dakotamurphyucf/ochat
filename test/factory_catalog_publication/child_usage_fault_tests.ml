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

(* Faults select the actual parent's new bounded association leaf, not child
   creation, unrelated startup, or an inferred permission/custody record. *)
module Fault = struct
  type t =
    { mutable parent : P.Id.Session.t option
    ; key : P.Idempotency_key.t
    ; mutable reference : D.Reference.t option
    ; mutable input : int64 option
    ; mutable hits : int
    }

  let create () =
    { parent = None
    ; key = P.Idempotency_key.of_string "553-slot-unknown-ack" |> protocol_ok
    ; reference = None
    ; input = None
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
           let reference =
             leaves delta
             |> List.find_map ~f:(function
               | Child_contributions_changed children ->
                 A.Child_contributions.references children
                 |> List.find ~f:(fun reference ->
                   P.Id.Session.equal reference.D.Reference.key.parent_session_id parent
                   && P.Idempotency_key.equal reference.key.idempotency_key t.key
                   && Option.value_map t.input ~default:true ~f:(fun input ->
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
               | _ -> None)
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
           1L
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

let run boundary =
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
         Existing.assert_same before parent;
         let reference = Option.value_exn fault.reference in
         let record =
           D.resolve
             (Agent_store.Session_store.delegations (Daemon.store daemon))
             reference
           |> Existing.store_ok
         in
         assert (D.equal_stage Child_installed record.stage);
         assert (Option.is_none record.revocation);
         assert (
           Option.is_none
             (Registry.find (Daemon.registry daemon) reference.child_session_id));
         reopen env sw root daemon policy before reference definition fault counters;
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
  run Before_sync;
  run After_sync;
  print_endline
    "both actual slot cuts: no pre-ACK start; physical recovery same child, one first \
     delivery, no retry replay";
  [%expect
    {| both actual slot cuts: no pre-ACK start; physical recovery same child, one first delivery, no retry replay |}]
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
          | Imported Added -> ()
          | Imported (Replaced | Duplicate | Stale) | Historical_absence ->
            failwith "first actual import was not Added");
         let seven = R.state parent in
         assert_input 7L seven;
         Actor.observe_inference
           child.actor
           ~handle
           (correction handle Int64.(usage_revision + 2L) 11L)
         |> protocol_ok;
         (* Arm only the typed replacement leaf containing the actual eleven-token
          snapshot. No provider, registration, or unrelated parent write matches. *)
         fault.input <- Some 11L;
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
            | Imported (Replaced | Duplicate) -> ()
            | Imported (Added | Stale) | Historical_absence ->
              failwith "recovered observed slot lost its replacement identity");
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
