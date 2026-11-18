open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Goal = Goal_service_tests
module G = P.Goal
module GD = A.Goal_descendants
module S = A.Child_usage_snapshot
module Included = A.Goal_descendant_included
module Pending = A.Goal_descendant_pending
module Occurrence = A.Goal_descendant_occurrence

(* Only the daemon receives this clock. Creation.with_daemon passes the original
   environment to the test, preserving independent real-clock supervision. *)
module Deadline_cut = struct
  type selected =
    { parent : P.Id.Session.t
    ; reference : Agent_store.Delegation_store.Reference.t
    ; deadline : P.Timestamp.t
    }

  type t =
    { mutable selected : selected option
    ; mutable included : Included.t option
    ; mutable hits : int
    ; mutable pause : unit -> unit
    ; mutable resume : unit -> unit
    ; mutable advance : float -> unit
    }

  let create () =
    { selected = None
    ; included = None
    ; hits = 0
    ; pause = ignore
    ; resume = ignore
    ; advance = ignore
    }
  ;;

  let inspect selected transaction =
    if
      not
        (P.Id.Session.equal
           selected.parent
           transaction.Agent_store.Transaction.session_id)
    then None
    else
      A.Session_delta_document.decode
        ~limits:Document_schema.Limits.default
        transaction.delta
      |> document_ok
      |> A.Session_delta_document.value
      |> Child_usage_fault_tests.Fault.leaves
      |> List.find_map ~f:(function
        | A.Session_delta.Goal_descendants_changed update ->
          (match GD.Update.reason update, GD.Update.children update with
           | Import reference, Replace children
             when Agent_store.Delegation_store.Reference.equal
                    reference
                    selected.reference ->
             A.Child_contributions.snapshot children ~reference
             |> protocol_ok
             |> Option.bind ~f:(fun snapshot ->
               S.goal_scopes snapshot
               |> Option.bind ~f:(fun scopes ->
                 List.find_map scopes ~f:(fun scope ->
                   S.Goal_scope.included scope
                   |> Option.bind ~f:(fun included ->
                     match
                       Occurrence.kind (Pending.occurrence (Included.pending included))
                     with
                     | Attempt _ -> Some included
                     | Host_turn _ -> None))))
           | _ -> None)
        | _ -> None)
  ;;

  let wrap t (env : Eio_unix.Stdenv.base) =
    let clock, pause, resume, advance =
      controlled_wall_clock env#clock ~initial:(Eio.Time.now env#clock)
    in
    t.pause <- pause;
    t.resume <- resume;
    t.advance <- advance;
    let host =
      object
        method clock = clock
        method mono_clock = env#mono_clock
        method domain_mgr = env#domain_mgr
        method stdin = env#stdin
        method stdout = env#stdout
        method stderr = env#stderr
        method net = env#net
        method process_mgr = env#process_mgr
        method fs = env#fs
        method cwd = env#cwd
        method secure_random = env#secure_random
        method debug = env#debug
        method backend_id = env#backend_id
      end
    in
    Crash_fault_io.wrap
      host
      ~boundary:After_sync
      ~matches:(fun path ->
        String.is_substring path ~substring:"/journal/"
        && String.is_suffix path ~suffix:".log")
      ~reached:(fun path ->
        Option.iter t.selected ~f:(fun selected ->
          let scan =
            Eio.Path.load Eio.Path.(env#fs / path)
            |> Agent_store.Journal_segment.scan_contents
                 ~max_payload_length:(64 * 1024 * 1024)
            |> Source_preparation_tests.store_ok
          in
          match List.last scan.entries with
          | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
            let transaction =
              Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
              |> Source_preparation_tests.store_ok
            in
            Option.iter (inspect selected transaction) ~f:(fun included ->
              t.selected <- None;
              t.included <- Some included;
              t.hits <- t.hits + 1;
              (* No Actor RPC inside its writer. Its import ACK completes, but
                 the subsequent admitted guard observes the expired deadline. *)
              P.Timestamp.to_time_ns selected.deadline
              |> Time_ns.to_span_since_epoch
              |> Time_ns.Span.to_sec
              |> fun seconds -> t.advance (seconds +. 1.))
          | Some _ | None -> ()))
  ;;
end

let%expect_test
    "inherited deadline after canonical Attempt preserves facts and denies provider"
  =
  let cut = Deadline_cut.create () in
  Birth.with_native_goal_child
    ~wrap_env:(Deadline_cut.wrap cut)
    ~child_sources:Birth.attempt_sources
    (fun env _sw daemon parent child scope reference counters ->
       protect_cleanup ~finally:cut.resume (fun () ->
         let within f = Eio.Time.with_timeout_exn env#clock 30. f in
         Actor.with_moderator_checkpoint parent.actor (fun () -> Ok ()) |> protocol_ok;
         cut.pause ();
         let before = state parent in
         let deadline =
           P.Timestamp.add_ms before.identity.updated_at 3_600_000 |> protocol_ok
         in
         let budget =
           G.Budget.create
             ~admitted_turns:None
             ~admitted_attempts:None
             ~observed_tokens:None
             ~deadline:(Some deadline)
           |> protocol_ok
         in
         Goal_admitted_guard_tests.command
           daemon
           parent
           ~key:"554-descendant-deadline"
           (Change_budget (Goal.goal_id, budget))
         |> protocol_ok
         |> ignore;
         Agent_server.Session_factory.prepare_session_start (Daemon.factory daemon) child
         |> protocol_ok;
         Owner.ensure_loaded_authenticated child.runtime ~actor:original |> protocol_ok;
         let writer, subscriber =
           Actor.attach child.actor ~mode:Read_write ~subscribe:true |> protocol_ok
         in
         protect_cleanup
           ~finally:(fun () -> Actor.detach child.actor writer.id |> protocol_ok)
           (fun () ->
              Actor.start child.actor ~actor:original ~attachment_id:writer.id
              |> protocol_ok
              |> ignore;
              let idle = state child in
              assert (Option.is_none idle.moderator);
              assert (Option.is_none idle.goal_catalog);
              assert (List.is_empty idle.jobs);
              assert (Option.is_none idle.active_operation);
              let id = A.History_id_source.allocate child.history_ids |> protocol_ok in
              let message =
                Owner.parse_user_content
                  child.runtime
                  ~id
                  { kind = Plain_text
                  ; text = "Check the actual inherited deadline."
                  ; attachments = []
                  }
                |> protocol_ok
                |> A.History_codec.to_protocol
              in
              cut.selected
              <- Some { parent = before.identity.session_id; reference; deadline };
              let submitted =
                Actor.submit_message_authenticated
                  child.actor
                  ~actor:original
                  ~attachment_id:writer.id
                  message
                |> protocol_ok
              in
              let operation_id = Option.value_exn submitted.operation_id in
              let subscriber = Option.value_exn subscriber in
              let rec await_failure () =
                match A.Subscriber.take subscriber with
                | Some (Ok (Durable event))
                  when P.Event.Durable.equal_kind event.kind Operation_failed ->
                  (match
                     P.Event.Durable.Payload.of_json ~kind:event.kind event.payload
                     |> protocol_ok
                   with
                   | Operation_failed operation ->
                     assert (P.Id.Operation.equal operation.id operation_id);
                     (match operation.state with
                      | Failed error -> error
                      | _ -> failwith "failed event did not contain a failed operation")
                   | _ -> failwith "wrong typed operation failure payload")
                | Some (Ok (Durable _)) | Some (Ok (Recoverable _)) -> await_failure ()
                | Some (Error error) -> raise_s [%sexp (error : P.Error.t)]
                | None -> failwith "subscriber closed before deadline rejection"
              in
              let failure = within await_failure in
              [%test_eq: P.Error.code] Permission_denied failure.code;
              [%test_eq: string] "descendant root Goal deadline reached" failure.message;
              assert (not failure.retryable);
              let current =
                within (fun () -> Goal_native_test_support.await_quiet child)
              in
              [%test_eq: int] 1 cut.hits;
              [%test_eq: int] 2 !(counters.providers);
              let included = Option.value_exn cut.included in
              assert (
                List.equal
                  Included.equal
                  [ included ]
                  (GD.local_included (Option.value_exn current.goal_descendants)));
              let branch = Descendant_turn_adapter_tests.branch parent scope reference in
              assert (Option.is_none (GD.Branch.pending branch));
              [%test_eq: int64] 2L (GD.Branch.confirmed_prefix branch);
              (match GD.Branch.last branch with
               | Some (Included actual) -> assert (Included.equal actual included)
               | None | Some (No_writer_closed _) ->
                 failwith "deadline erased root confirmation");
              let snapshot =
                A.Child_contributions.snapshot
                  (Option.value_exn (state parent).child_contributions)
                  ~reference
                |> protocol_ok
                |> Option.value_exn
              in
              let marks = S.Partition.marks (S.own snapshot) in
              [%test_eq: int64] 1L marks.turn;
              [%test_eq: int64] 1L marks.attempt;
              [%test_eq: int64] 0L marks.untracked_attempts;
              [%test_eq: int64]
                2L
                (A.Goal_accounting.admitted_attempts (Goal.accounting (state parent)));
              let occurrence = Pending.occurrence (Included.pending included) in
              let ordinal =
                match Occurrence.kind occurrence with
                | Attempt { ordinal; _ } -> ordinal
                | Host_turn _ -> failwith "deadline cut did not follow an Attempt"
              in
              let row =
                A.Inference_ledger.find current.inference_ledger ~ordinal
                |> Option.value_exn
              in
              assert (
                P.Id.Session.equal
                  (A.Inference_ledger.Handle.session_id
                     (A.Inference_ledger.Row.handle row))
                  current.identity.session_id))));
  print_endline
    "actual Attempt and root confirmation retained; expired inherited Goal prevents \
     provider";
  [%expect
    {| actual Attempt and root confirmation retained; expired inherited Goal prevents provider |}]
;;
