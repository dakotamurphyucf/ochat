open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module K = Session_commit_attempt_test_support.Session_commit_attempt
module O = A.Session_commit_observation

let same_error result expected =
  match result with
  | Error actual -> assert (phys_equal actual expected)
  | Ok _ -> failwith "retained primary Error was lost"
;;

let refuses_seal attempt =
  match K.No_writer.seal attempt with
  | Error error -> assert (P.Error.equal_code error.code Conflict)
  | Ok _ -> failwith "writer uncertainty acquired a no-writer seal"
;;

let invalid_finalization f =
  match f () with
  | () -> failwith "foreign or duplicate finalization was accepted"
  | exception Invalid_argument _ -> ()
;;

let%expect_test "causal no-writer seals preserve elected cleanup and exact writer facts" =
  let prior_backtrace_status = Stdlib.Printexc.backtrace_status () in
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace prior_backtrace_status)
    ~f:(fun () ->
      (* This isolated trajectory requires a nonempty backtrace oracle. Restore
         the exact process flag even if an assertion or cancellation escapes. *)
      Stdlib.Printexc.record_backtrace true;
      with_actor_workspace (fun _ workspace_instance ->
        let previous =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        in
        let patch =
          P.Session_metadata.Patch.create
            ~name:(Set "kernel candidate")
            ~set_labels:[]
            ~remove_labels:[]
          |> protocol_ok
        in
        let delta =
          A.Session_metadata_transition.apply
            previous
            ~expected_metadata_revision:0L
            ~patch
          |> protocol_ok
          |> Option.value_exn
        in
        let candidate =
          A.Session_transition.apply ~now:timestamp previous ~delta ~payloads:[]
          |> protocol_ok
        in
        let slot = ref ()
        and preparation = ref () in
        let context = K.Context.Causal_canonical { slot; preparation } in
        let make () = K.create ~context ~previous ~candidate in
        let failure =
          P.Error.create
            Conflict
            ~message:"primary pre-writer failure"
            ~retryable:false
            ()
        in
        let commit attempt f =
          K.perform attempt (fun () ->
            O.Owner.commit (K.observation attempt) ~commit:(fun ~observation ->
              let host =
                O.Host.observe_entry observation ~previous ~candidate |> protocol_ok
              in
              f host))
        in
        let attempt = make () in
        let invocation = K.begin_invocation attempt |> protocol_ok in
        K.check_invocation attempt invocation |> protocol_ok;
        same_error (commit attempt (fun _ -> Error failure)) failure;
        let closed = K.No_writer.seal attempt |> protocol_ok in
        let matches
              ?(attempt = attempt)
              ?(slot = slot)
              ?(preparation = preparation)
              ?(previous = previous)
              ?(candidate = candidate)
              ()
          =
          K.No_writer.matches_canonical
            closed
            ~attempt
            ~slot
            ~preparation
            ~previous
            ~candidate
        in
        assert (matches ());
        assert (not (matches ~attempt:(make ()) ()));
        assert (not (matches ~slot:(ref ()) ()));
        assert (not (matches ~preparation:(ref ()) ()));
        let equal_previous = { previous with counters = previous.counters } in
        let equal_candidate = { candidate with events = candidate.events } in
        assert (not (phys_equal equal_previous previous));
        assert (not (phys_equal equal_candidate candidate));
        assert (not (matches ~previous:equal_previous ()));
        assert (not (matches ~candidate:equal_candidate ()));
        assert (
          not
            (K.No_writer.matches_reserve
               closed
               ~attempt
               ~reservation_attempt:slot
               ~set:preparation
               ~previous
               ~candidate));
        assert (
          not
            (K.No_writer.matches_terminal
               closed
               ~attempt
               ~operation:slot
               ~reservation:preparation
               ~previous
               ~candidate));
        same_error (K.begin_invocation attempt) failure;
        same_error (K.check_invocation attempt invocation) failure;
        let foreign = make () |> K.begin_invocation |> protocol_ok in
        invalid_finalization (fun () -> K.finish_invocation attempt foreign);
        K.finish_invocation attempt invocation;
        assert (matches ());
        ignore (K.No_writer.seal attempt |> protocol_ok : K.No_writer.t);
        invalid_finalization (fun () -> K.finish_invocation attempt invocation);
        same_error (K.begin_invocation attempt) failure;
        print_endline
          "original attempt/context/candidate pinned; same cleanup once; never reopened";
        (* The same order with a raised primary preserves the exact exception and its
       retained observation backtrace through the same sealed cleanup. *)
        let raised = make () in
        let elected = K.begin_invocation raised |> protocol_ok in
        let primary = Failure "primary raised pre-writer failure" in
        (match commit raised (fun _ -> raise primary) with
         | _ -> failwith "primary exception was swallowed"
         | exception exn -> assert (phys_equal exn primary));
        let retained_backtrace =
          match O.outcome (K.observation raised) with
          | Raised (exn, backtrace) ->
            assert (phys_equal exn primary);
            Stdlib.Printexc.raw_backtrace_to_string backtrace
          | Not_called | In_progress | Returned _ ->
            failwith "raised outcome was not retained"
        in
        assert (not (String.is_empty retained_backtrace));
        ignore (K.No_writer.seal raised |> protocol_ok : K.No_writer.t);
        K.finish_invocation raised elected;
        (match K.unresolved raised with
         | _ -> failwith "sealed primary exception was swallowed"
         | exception exn ->
           let actual = Stdlib.Printexc.get_raw_backtrace () in
           assert (phys_equal exn primary);
           assert (
             String.is_prefix
               (Stdlib.Printexc.raw_backtrace_to_string actual)
               ~prefix:retained_backtrace));
        invalid_finalization (fun () -> K.finish_invocation raised elected);
        print_endline "raised primary and retained backtrace survive seal-before-cleanup";
        (* Follow the actual observation adapter through successively stronger facts.
       These are private kernel host simulations, never journal/execution grants. *)
        let payload = "bounded kernel writer fixture" in
        let hash = Digestif.SHA256.(to_hex (digest_string payload)) in
        let transaction : O.Transaction.t =
          { session_id = previous.identity.session_id
          ; generation = candidate.state.identity.generation
          ; sequence = candidate.state.counters.transaction_sequence
          ; revision = candidate.state.counters.revision
          ; previous_hash = None
          ; payload
          ; hash
          ; max_payload_length = String.length payload
          }
        in
        let unknown = make () in
        let unknown_invocation = K.begin_invocation unknown |> protocol_ok in
        same_error
          (K.perform unknown (fun () ->
             O.Owner.commit (K.observation unknown) ~commit:(fun ~observation:_ ->
               Error failure)))
          failure;
        refuses_seal unknown;
        K.finish_invocation unknown unknown_invocation;
        let entered = make () in
        let entered_invocation = K.begin_invocation entered |> protocol_ok in
        same_error
          (commit entered (fun host ->
             O.Host.with_writer host ~transaction ~commit:(fun () -> Error failure)
             |> Result.map ~f:ignore))
          failure;
        (match O.progress (K.observation entered) with
         | Writer_entered _ -> ()
         | Unobserved | Before_writer | Journal_acknowledged _ ->
           failwith "writer did not enter");
        refuses_seal entered;
        K.finish_invocation entered entered_invocation;
        let acknowledged = make () in
        let acknowledged_invocation = K.begin_invocation acknowledged |> protocol_ok in
        same_error
          (commit acknowledged (fun host ->
             O.Host.with_writer host ~transaction ~commit:(fun () ->
               Ok O.Returned.{ sequence = transaction.sequence; hash })
             |> protocol_ok
             |> ignore;
             Error failure))
          failure;
        (match O.progress (K.observation acknowledged) with
         | Journal_acknowledged _ -> ()
         | Unobserved | Before_writer | Writer_entered _ ->
           failwith "ACK was not retained");
        refuses_seal acknowledged;
        K.finish_invocation acknowledged acknowledged_invocation;
        List.iter [ unknown; entered; acknowledged ] ~f:(fun attempt ->
          same_error (K.begin_invocation attempt) failure);
        print_endline
          "unobserved Error, writer-entered Error and post-ACK Error cannot refund"));
  [%expect
    {|
    original attempt/context/candidate pinned; same cleanup once; never reopened
    raised primary and retained backtrace survive seal-before-cleanup
    unobserved Error, writer-entered Error and post-ACK Error cannot refund |}]
;;
