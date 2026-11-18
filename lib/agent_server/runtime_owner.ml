open! Core

module Scope_outcome = struct
  module Cleanup = struct
    module Failure = struct
      type t =
        | Protocol of Agent_protocol.Error.t
        | Raised of exn * Stdlib.Printexc.raw_backtrace
    end

    type status =
      | Never_acquired
      | Joined
      | Failed of Failure.t * Failure.t list

    type t = status

    let status t = t
    let failed failure = Failed (failure, [])

    let combine first second =
      match first, second with
      | Failed (failure, rest), Failed (next, remaining) ->
        Failed (failure, rest @ (next :: remaining))
      | Failed _, (Never_acquired | Joined) -> first
      | (Never_acquired | Joined), Failed _ -> second
      | Never_acquired, Never_acquired -> Never_acquired
      | (Never_acquired | Joined), (Never_acquired | Joined) -> Joined
    ;;

    let of_result ~acquired = function
      | Ok (Ok ()) -> if acquired then Joined else Never_acquired
      | Ok (Error failure) -> Failed (Protocol failure, [])
      | Error (exn, backtrace) -> Failed (Raised (exn, backtrace), [])
    ;;
  end

  type 'a primary =
    | Returned of ('a, Agent_protocol.Error.t) result
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  type 'a t =
    { primary : 'a primary
    ; cleanup : Cleanup.t
    }

  let primary t = t.primary
  let cleanup t = t.cleanup
  let prepend_cleanup t ~cleanup = { t with cleanup = Cleanup.combine cleanup t.cleanup }

  let capture f =
    match f () with
    | result -> Returned result
    | exception exn -> Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  ;;

  let resume_primary = function
    | Returned result -> result
    | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  ;;

  let resume t =
    match t.primary with
    | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
    | Returned (Error _ as failure) -> failure
    | Returned (Ok _ as result) ->
      (match t.cleanup with
       | Never_acquired | Joined -> result
       | Failed (Protocol failure, _) -> Error failure
       | Failed (Raised (exn, backtrace), _) ->
         Exn.raise_with_original_backtrace exn backtrace)
  ;;
end

module Original_diagnostic = struct
  module Failure = Agent_session.Callback_cleanup.Failure
  module Segment = Original_cleanup_report.Segment

  let failures = function
    | [] -> None
    | failures -> Some (Segment.create failures)
  ;;

  let primary = function
    | Scope_outcome.Returned (Ok _) -> None
    | Returned (Error error) -> failures [ Failure.Protocol error ]
    | Raised (exn, trace) -> failures [ Failure.Raised (exn, trace) ]
  ;;

  let cleanup = function
    | Ok () -> None
    | Error (exn, trace) -> failures [ Failure.Raised (exn, trace) ]
  ;;

  let native = function
    | Ok result -> primary (Scope_outcome.Returned result)
    | Error (exn, trace) -> primary (Scope_outcome.Raised (exn, trace))
  ;;
end

module Physical_origin = struct
  module CO = Agent_session.Close_observation
  module D = Agent_session.Cleanup_diagnostic

  type t =
    { diagnostics : D.t list
    ; primary : D.t option
    }

  let of_observation observation =
    { diagnostics = CO.diagnostics observation; primary = CO.primary observation }
  ;;

  let with_boundary_failure observation failure =
    match failure with
    | None -> observation
    | Some failure ->
      let declared =
        Option.exists (CO.primary observation) ~f:(fun primary ->
          not (List.is_empty (D.failures primary)))
      in
      if declared
      then observation
      else (
        let diagnostic = D.create [ failure ] in
        CO.create
          (CO.exit observation)
          ~diagnostics:(CO.diagnostics observation @ [ diagnostic ])
          ~primary:diagnostic
          ())
  ;;

  (* Reported ports may omit provenance. Such an actual negative owns this
     opaque boundary once. Known incoming Cell groups are never re-minted;
     an absent/empty declared primary does not permit guessing from other
     supplied groups. Preserve them and own this unattributed boundary once. *)
  let capture_raw f =
    match f () with
    | observation -> observation
    | exception exn ->
      let trace = Stdlib.Printexc.get_raw_backtrace () in
      CO.create (Raised (exn, trace)) ~diagnostics:[] ()
  ;;

  let capture (f : unit -> unit CO.t) =
    let observation = capture_raw f in
    let failure =
      match CO.exit observation with
      | Returned () -> None
      | Raised (exn, trace) -> Some (D.Failure.Raised (exn, trace))
    in
    with_boundary_failure observation failure
  ;;

  let capture_result (f : unit -> (unit, Agent_protocol.Error.t) result CO.t) =
    let observation = capture_raw f in
    let failure =
      match CO.exit observation with
      | Returned (Ok ()) -> None
      | Returned (Error error) -> Some (D.Failure.Protocol error)
      | Raised (exn, trace) -> Some (D.Failure.Raised (exn, trace))
    in
    with_boundary_failure observation failure
  ;;

  let opaque (f : unit -> unit) =
    capture (fun () -> CO.create (Returned (f ())) ~diagnostics:[] ())
  ;;

  let opaque_result (f : unit -> (unit, Agent_protocol.Error.t) result) =
    capture_result (fun () -> CO.create (Returned (f ())) ~diagnostics:[] ())
  ;;

  let primary observation =
    match CO.exit observation with
    | Returned result -> Scope_outcome.Returned result
    | Raised (exn, trace) -> Scope_outcome.Raised (exn, trace)
  ;;

  let native observation =
    match CO.exit observation with
    | Returned result -> Ok result
    | Raised (exn, trace) -> Error (exn, trace)
  ;;

  let cleanup observation =
    match CO.exit observation with
    | Returned () -> Ok ()
    | Raised (exn, trace) -> Error (exn, trace)
  ;;
end

(* The actual natural claim finalizer owns this write-once pin report. A
   retirement may retain it before the live holder clears; absence/in-flight is
   never proof that a pin was released. No Owner or executable callback escapes. *)
module Policy_claim_release = struct
  module Original = struct
    type t =
      { observation :
          (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
      ; origin : Physical_origin.t
      }
  end

  module Phase = struct
    type t =
      | Unstarted
      | Recording
      | Recorded of Original.t
  end

  type t = { mutable phase : Phase.t }

  let create () = { phase = Unstarted }

  let original t =
    match t.phase with
    | Unstarted | Recording -> None
    | Recorded original -> Some original
  ;;

  let begin_release t =
    match t.phase with
    | Unstarted -> t.phase <- Recording
    | Recording | Recorded _ -> invalid_arg "natural claim pin release already started"
  ;;

  let record t observation =
    match t.phase with
    | Recording ->
      t.phase
      <- Recorded { observation; origin = Physical_origin.of_observation observation }
    | Unstarted | Recorded _ -> invalid_arg "natural claim pin release is not recording"
  ;;

  let positive = function
    | Some (Scope_outcome.Returned (Ok ())) -> true
    | None | Some (Returned (Error _) | Raised _) -> false
  ;;

  let released t ~current ~attempts =
    positive current
    || List.exists attempts ~f:positive
    || Option.exists (original t) ~f:(fun original ->
      positive (Some (Physical_origin.primary original.Original.observation)))
  ;;
end

(* First-error selection travels with its actual producer origin. Combining
   reports never searches diagnostics, compares payloads or chooses a new primary. *)
module Cleanup_projection = struct
  type t =
    { cleanup : Scope_outcome.Cleanup.t
    ; origin : Physical_origin.t option
    ; boundary_diagnostics : Original_cleanup_report.Segment.t list
    }

  let create ?(boundary_diagnostics = []) cleanup ~origin =
    { cleanup; origin; boundary_diagnostics }
  ;;

  let empty = create Scope_outcome.Cleanup.Never_acquired ~origin:None
  let cleanup t = t.cleanup
  let origin t = t.origin

  let combine first second =
    let origin =
      match first.cleanup, second.cleanup with
      | Scope_outcome.Cleanup.Failed _, _ -> first.origin
      | (Never_acquired | Joined), Failed _ -> second.origin
      | (Never_acquired | Joined), (Never_acquired | Joined) -> None
    in
    let diagnostics =
      Option.value_map first.origin ~default:[] ~f:(fun origin -> origin.diagnostics)
      @ Option.value_map second.origin ~default:[] ~f:(fun origin -> origin.diagnostics)
    in
    let origin =
      match origin, diagnostics with
      | Some selected, _ -> Some { selected with diagnostics }
      | None, [] -> None
      | None, _ :: _ -> Some { Physical_origin.diagnostics; primary = None }
    in
    { cleanup = Scope_outcome.Cleanup.combine first.cleanup second.cleanup
    ; origin
    ; boundary_diagnostics = first.boundary_diagnostics @ second.boundary_diagnostics
    }
  ;;

  let of_primary ?origin ~acquired primary =
    let cleanup =
      match primary with
      | Scope_outcome.Returned result ->
        Scope_outcome.Cleanup.of_result ~acquired (Ok result)
      | Raised (exn, trace) ->
        Scope_outcome.Cleanup.of_result ~acquired (Error (exn, trace))
    in
    create cleanup ~origin
  ;;

  let of_diagnostic cleanup diagnostic =
    let origin =
      Option.map diagnostic ~f:(fun primary ->
        { Physical_origin.diagnostics = [ primary ]; primary = Some primary })
    in
    create cleanup ~origin
  ;;

  (* These projections only resume captured DATA; neither body nor finalizer
     effects run twice. Keep Exn.protect's exact raw composite when both raised,
     while diagnostics retain the two actual causes without minting a wrapper. *)
  let protect_observed body finally =
    let module CO = Agent_session.Close_observation in
    let outcome =
      Scope_outcome.capture (fun () ->
        Exn.protect ~f:(fun () -> CO.resume body) ~finally:(fun () -> CO.resume finally))
    in
    let primary =
      match CO.exit body, CO.exit finally with
      | Raised _, _ -> CO.primary body
      | Returned _, Raised _ -> CO.primary finally
      | Returned _, Returned () -> CO.primary body
    in
    CO.create
      (match outcome with
       | Scope_outcome.Returned result -> Returned result
       | Raised (exn, trace) -> Raised (exn, trace))
      ~diagnostics:(CO.diagnostics body @ CO.diagnostics finally)
      ?primary
      ()
  ;;

  let protect_unit_observed body finally =
    let module CO = Agent_session.Close_observation in
    let body =
      CO.create
        (match CO.exit body with
         | Returned () -> Returned (Ok ())
         | Raised (exn, trace) -> Raised (exn, trace))
        ~diagnostics:(CO.diagnostics body)
        ?primary:(CO.primary body)
        ()
    in
    let observed = protect_observed body finally in
    CO.create
      (match CO.exit observed with
       | Returned (Ok ()) -> Returned ()
       | Returned (Error _) -> invalid_arg "unit finalizer returned a protocol error"
       | Raised (exn, trace) -> Raised (exn, trace))
      ~diagnostics:(CO.diagnostics observed)
      ?primary:(CO.primary observed)
      ()
  ;;
end

(* Pure evidence aggregation. These values carry no execution authority. The
   typed observer classifies an unfinished original dependency as Blocked and an
   unattempted tail behind a terminal prerequisite as Terminal [], never as an
   eternally live producer. *)
module Cleanup_duty = struct
  module Failure = Scope_outcome.Cleanup.Failure

  type t =
    | Succeeded
    | Retryable of Failure.t list
    | Blocked of Failure.t list
    | Terminal of Failure.t list

  let failures = function
    | Succeeded -> []
    | Retryable failures | Blocked failures | Terminal failures -> failures
  ;;
end

module Cleanup_snapshot = struct
  type progress =
    | Joined
    | Retryable
    | Blocked
    | Terminal
  [@@deriving equal, sexp_of]

  type t =
    { progress : progress
    ; failures : Scope_outcome.Cleanup.Failure.t list
    ; cleanup : Scope_outcome.Cleanup.t option
    ; observation :
        ( (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
          , Agent_protocol.Error.t )
          result
    }

  let progress t = t.progress
  let failures t = t.failures
  let cleanup t = t.cleanup
  let observation t = t.observation
  let with_observation t observation = { t with observation }

  let unavailable_observation () =
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"cleanup observation has unfinished or unavailable producer evidence"
         ~retryable:false
         ())
  ;;

  (* Next permitted action has precedence over eventual recoverability. In
     particular a retryable leg cannot erase an unrelated immutable negative. *)
  let of_duties duties =
    let progress =
      List.fold duties ~init:Joined ~f:(fun progress duty ->
        match duty, progress with
        | Cleanup_duty.Retryable _, _ -> Retryable
        | Blocked _, (Joined | Terminal | Blocked) -> Blocked
        | Terminal _, Joined -> Terminal
        | (Succeeded | Blocked _ | Terminal _), _ -> progress)
    in
    let failures = List.concat_map duties ~f:Cleanup_duty.failures in
    let cleanup =
      match failures, progress with
      | first :: rest, _ -> Some (Scope_outcome.Cleanup.Failed (first, rest))
      | [], Joined -> Some Scope_outcome.Cleanup.Joined
      | [], (Retryable | Blocked | Terminal) -> None
    in
    { progress; failures; cleanup; observation = unavailable_observation () }
  ;;
end

(* One original Retirement_cleanup selection, with distinct original call slots.
   A successful initial observation does not consume the final retained-cell
   refresh. *)
module Actor_cleanup_step = struct
  module Cleanup = Agent_session.Session_actor.Retirement_cleanup

  type phase =
    | Initial_drain
    | Same_foreground
    | Final_refresh
    | Complete

  type t =
    { cleanup : Cleanup.t
    ; mutable initial : Cleanup.Snapshot.t Scope_outcome.primary option
    ; mutable foreground : unit Scope_outcome.primary option
    ; mutable final : Cleanup.Snapshot.t Scope_outcome.primary option
    ; mutable initial_diagnostic : Original_cleanup_report.Segment.t option
    ; mutable foreground_diagnostic : Original_cleanup_report.Segment.t option
    ; mutable final_diagnostic : Original_cleanup_report.Segment.t option
    }

  let create cleanup =
    { cleanup
    ; initial = None
    ; foreground = None
    ; final = None
    ; initial_diagnostic = None
    ; foreground_diagnostic = None
    ; final_diagnostic = None
    }
  ;;

  let diagnostic = function
    | Scope_outcome.Returned (Ok snapshot) ->
      Original_diagnostic.failures
        (List.map (Cleanup.Snapshot.failures snapshot) ~f:(function
           | Cleanup.Failure.Protocol error ->
             Agent_session.Callback_cleanup.Failure.Protocol error
           | Raised (exn, trace) -> Raised (exn, trace)))
    | Returned (Error error) ->
      Original_diagnostic.primary (Scope_outcome.Returned (Error error))
    | Raised (exn, trace) ->
      Original_diagnostic.primary (Scope_outcome.Raised (exn, trace))
  ;;

  let phase t =
    match t.initial, t.foreground, t.final with
    | None, _, _ -> Initial_drain
    | Some _, None, _ -> Same_foreground
    | Some _, Some _, None -> Final_refresh
    | Some _, Some _, Some _ -> Complete
  ;;
end

module Runtime_binding = struct
  type t =
    | Legacy of Agent_session.Runtime_builder.t
    | Attached of Agent_session.Runtime_builder.Attachment.t

  let runtime = function
    | Legacy runtime -> runtime
    | Attached attachment -> Agent_session.Runtime_builder.Attachment.runtime attachment
  ;;

  let product = function
    | Legacy _ -> None
    | Attached attachment -> Agent_session.Runtime_builder.Attachment.product attachment
  ;;

  let close_reported = function
    | Legacy runtime -> Agent_session.Runtime_builder.close_reported runtime
    | Attached attachment ->
      Physical_origin.capture (fun () ->
        Agent_session.Runtime_builder.Attachment.close_reported attachment)
  ;;

  let matches t expected = phys_equal (runtime t) expected

  let native_identity = function
    | Legacy _ -> None
    | Attached attachment ->
      Agent_session.Runtime_builder.Attachment.native_identity attachment
  ;;
end

module Resource_handoff = struct
  type t =
    { binding : Runtime_binding.t
    ; product : Agent_session.Runtime_builder.Product_identity.t
    }

  let of_binding binding =
    Option.map (Runtime_binding.product binding) ~f:(fun product -> { binding; product })
  ;;

  let runtime t = Runtime_binding.runtime t.binding
  let product t = t.product
end

module Lease_purpose = struct
  type t =
    | Exclusive
    | Shared_resource of Agent_session.Runtime_builder.Product_identity.t
    | Shared_native of Agent_session.Runtime_builder.Native_identity.t

  module Selection = struct
    type t =
      | Exclusive
      | Delegation_resource
      | Policy_resource of Agent_session.Runtime_builder.Product_identity.t
      | Native_resource of Agent_session.Runtime_builder.Native_identity.t
  end
end

module Policy_resource_cleanup = struct
  type t =
    | Callback of
        { pending : unit -> bool
        ; close_pending :
            unit
            -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
        }
    | Cell of Session_policy_resource.t

  let create_reported ~pending ~close_pending = Callback { pending; close_pending }
  let of_cell cell = Cell cell

  let cell = function
    | Callback _ -> None
    | Cell cell -> Some cell
  ;;

  let create ~pending ~close_pending =
    create_reported ~pending ~close_pending:(fun () ->
      Physical_origin.opaque_result close_pending)
  ;;

  let pending = function
    | Callback callbacks -> callbacks.pending ()
    | Cell cell -> Session_policy_resource.pending cell
  ;;

  let close_pending_reported t =
    Physical_origin.capture_result (fun () ->
      match t with
      | Callback callbacks -> callbacks.close_pending ()
      | Cell cell -> Session_policy_resource.close_pending_reported cell)
  ;;

  let close_pending t = close_pending_reported t |> Agent_session.Close_observation.resume
end

module Restored_policy_anchor = struct
  type phase =
    | Preparing
    | Available of Agent_session.Session_actor.Policy_restoration.Published.t
    | Retiring

  type t =
    { loan : Session_policy_resource.Lease.t
    ; access : Agent_session.Runtime_builder.Restored_policy_access.t
    ; mutable phase : phase
    }

  let runtime t = Session_policy_resource.Lease.runtime t.loan

  let product t =
    Agent_session.Runtime_builder.Restored_policy_access.product_identity t.access
  ;;

  let revoke t =
    (match t.phase with
     | Available published ->
       Agent_session.Session_actor.Policy_restoration.Published.close published
     | Preparing | Retiring -> ());
    t.phase <- Retiring
  ;;

  let available t =
    match t.phase with
    | Available published ->
      Result.map
        (Agent_session.Session_actor.Policy_restoration.Published.check published)
        ~f:(fun () -> published)
    | Preparing | Retiring ->
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"restored policy resource is preparing or retiring"
           ~retryable:true
           ())
  ;;
end

(* Actual references remain in this Owner until the SAME loan's release joins.
   A Cleanup report is diagnostic evidence, not custody for a failed release. *)
module Native_loan = struct
  module Attempt = struct
    type outcome =
      ((unit, Agent_protocol.Error.t) result, exn * Stdlib.Printexc.raw_backtrace) result

    type t =
      { finished : outcome Eio.Promise.t
      ; finish : outcome Eio.Promise.u
      ; mutable origin : Physical_origin.t option
      }

    let create () =
      let finished, finish = Eio.Promise.create () in
      { finished; finish; origin = None }
    ;;
  end

  type phase =
    | Available
    | Retiring
    | Closing of Attempt.t
    | Released

  type t =
    { loan : Session_policy_resource.Native_lease.t
    ; identity : Agent_session.Runtime_builder.Native_identity.t
    ; mutable phase : phase
    }

  let create loan =
    { loan
    ; identity = Session_policy_resource.Native_lease.identity loan
    ; phase = Available
    }
  ;;

  let retire t =
    match t.phase with
    | Available -> t.phase <- Retiring
    | Retiring | Closing _ | Released -> ()
  ;;
end

type cleanup_outcome = (unit, exn * Stdlib.Printexc.raw_backtrace) result

module Retired_failure_receipt = struct
  type t =
    { producer_finished : cleanup_outcome Eio.Promise.t
    ; failure : exn * Stdlib.Printexc.raw_backtrace
    ; origin : Physical_origin.t
    }
end

module Product_selection = Runtime_callback_data.Product_selection
module Lease_callback = Runtime_callback_data.Lease_callback

module Retired_resource = struct
  module Attempt = struct
    type failure = exn * Stdlib.Printexc.raw_backtrace

    type t =
      { finished : cleanup_outcome Eio.Promise.t
      ; finish : cleanup_outcome Eio.Promise.u
      ; prior_failure : Retired_failure_receipt.t option
      ; mutable origin : Physical_origin.t option
      ; mutable close_origin : Physical_origin.t option
      ; mutable product_selection : Product_selection.t option
      }

    let create ~prior_failure =
      let finished, finish = Eio.Promise.create () in
      { finished
      ; finish
      ; prior_failure
      ; origin = None
      ; close_origin = None
      ; product_selection = None
      }
    ;;
  end

  type phase =
    | Retained of Retired_failure_receipt.t option
    | Closing of Attempt.t
    | Closed

  type t =
    { binding : Runtime_binding.t
    ; product_callbacks : Agent_session.Runtime_builder.Product_callbacks.t option
    ; mutable product_selection : Product_selection.t option
    ; mutable phase : phase
    }

  module Receipt = struct
    type resource = t

    module Requirement = struct
      type t =
        | Initial
        | Required
    end

    module Borrower = struct
      type route =
        | Direct_resource
        | Native_retention of Agent_session.Runtime_builder.Native_identity.t

      type t =
        { finished : unit Eio.Promise.t
        ; binding : Runtime_binding.t
        ; route : route
        }
    end

    module Custody = struct
      type t =
        { borrowers : Borrower.t list
        ; factual_pending : bool
        }
    end

    type blocked =
      { custody : Custody.t
      ; previous_failure : Retired_failure_receipt.t option
      ; producer : Attempt.t option
      }

    type disposition =
      | Unattempted
      | Already_closed
      | Transferred of
          { borrowers : Borrower.t * Borrower.t list
          ; producer : Attempt.t option
          }
      | Blocked of blocked
      | Attempted of
          { attempt : Attempt.t
          ; outcome : cleanup_outcome
          }

    type t =
      { resource : resource
      ; disposition : disposition
      }

    let deferred ~requirement ~custody ~previous_failure ~producer =
      match requirement, previous_failure, custody.Custody.borrowers with
      | Requirement.Initial, None, first :: rest ->
        Transferred { borrowers = first, rest; producer }
      | (Initial | Required), (Some _ | None), _ ->
        Blocked { custody; previous_failure; producer }
    ;;
  end

  type selection =
    | Skip of Receipt.disposition
    | Join of Attempt.t
    | Close of Attempt.t

  let create binding =
    { binding
    ; product_callbacks = (Runtime_binding.runtime binding).product_callbacks
    ; product_selection = None
    ; phase = Retained None
    }
  ;;

  let capture_product t attempt =
    let selection =
      match t.product_selection with
      | Some _ as selected -> selected
      | None ->
        let selected = Option.map t.product_callbacks ~f:Product_selection.capture in
        t.product_selection <- selected;
        selected
    in
    attempt.Attempt.product_selection <- selection
  ;;

  let runtime t = Runtime_binding.runtime t.binding
end

(* A batch is the immutable result of the SAME actual request, including an
   unforeseen selection/commit escape and exact unattempted original tail. *)
module Cleanup_batch = struct
  module Original = struct
    type t =
      | Returned of cleanup_outcome
      | Raised of exn * Stdlib.Printexc.raw_backtrace
  end

  type t =
    { original : Original.t
    ; receipts : Retired_resource.Receipt.t list
    ; diagnostic : Original_cleanup_report.Segment.t option
    }

  let resume t =
    match t.original with
    | Returned outcome -> outcome
    | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  ;;

  let cleanup t =
    let captured =
      List.fold
        t.receipts
        ~init:Scope_outcome.Cleanup.Never_acquired
        ~f:(fun report receipt ->
          match receipt.Retired_resource.Receipt.disposition with
          | Attempted { outcome = Error (exn, backtrace); _ } ->
            Scope_outcome.Cleanup.combine
              report
              (Scope_outcome.Cleanup.failed
                 (Scope_outcome.Cleanup.Failure.Raised (exn, backtrace)))
          | Attempted { outcome = Ok (); _ }
          | Unattempted | Already_closed | Transferred _ | Blocked _ -> report)
    in
    match t.original with
    | Returned _ -> captured
    | Raised (exn, backtrace) ->
      Scope_outcome.Cleanup.combine
        captured
        (Scope_outcome.Cleanup.failed
           (Scope_outcome.Cleanup.Failure.Raised (exn, backtrace)))
  ;;

  let attempted_projection ~origin = function
    | Error (exn, trace) ->
      Cleanup_projection.create
        (Scope_outcome.Cleanup.failed (Raised (exn, trace)))
        ~origin
    | Ok () ->
      (* Keep the original flat positive status. Diagnostics on a positive exit
         still belong to that actual producer, but select no raw failure. *)
      Cleanup_projection.create
        Scope_outcome.Cleanup.Never_acquired
        ~origin:(Option.map origin ~f:(fun origin -> { origin with primary = None }))
  ;;

  let projection t =
    let captured =
      List.fold t.receipts ~init:Cleanup_projection.empty ~f:(fun report receipt ->
        match receipt.Retired_resource.Receipt.disposition with
        | Attempted { attempt; outcome } ->
          Cleanup_projection.combine
            report
            (attempted_projection
               ~origin:
                 (Option.first_some
                    attempt.Retired_resource.Attempt.close_origin
                    attempt.origin)
               outcome)
        | Already_closed ->
          let diagnostics =
            Option.value_map
              receipt.resource.product_selection
              ~default:[]
              ~f:Product_selection.diagnostics
          in
          Cleanup_projection.combine
            report
            (Cleanup_projection.create
               Scope_outcome.Cleanup.Never_acquired
               ~origin:
                 (if List.is_empty diagnostics
                  then None
                  else Some { Physical_origin.diagnostics; primary = None }))
        | Unattempted | Transferred _ | Blocked _ -> report)
    in
    match t.original with
    | Returned _ -> captured
    | Raised (exn, trace) ->
      Cleanup_projection.combine
        captured
        (Cleanup_projection.of_diagnostic
           (Scope_outcome.Cleanup.failed (Raised (exn, trace)))
           t.diagnostic)
  ;;

  (* [cleanup] selects physical negatives first. [resume] instead raises an
     actual control escape before returning that flat result. Keep its exact
     selected carrier without changing the independent diagnostic order. *)
  let resume_origin t =
    let origin = Cleanup_projection.origin (projection t) in
    match t.original with
    | Returned _ -> origin
    | Raised _ ->
      Some
        { Physical_origin.diagnostics =
            Option.value_map origin ~default:[] ~f:(fun origin -> origin.diagnostics)
        ; primary = t.diagnostic
        }
  ;;
end

module Owner_affinity = struct
  type t =
    { domain : int
    ; tag : unit ref
    ; actor : Agent_session.Session_actor.t
    ; source_owner : Agent_session.Runtime_source_owner.Identity.t option
    }

  let create ~actor ~source_owner =
    { domain = (Stdlib.Domain.self () :> int); tag = ref (); actor; source_owner }
  ;;

  let same (t : t) (other : t) =
    Int.equal t.domain other.domain
    && phys_equal t.tag other.tag
    && phys_equal t.actor other.actor
    && Option.equal
         Agent_session.Runtime_source_owner.Identity.equal
         t.source_owner
         other.source_owner
  ;;

  let check_domain_exn t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then invalid_arg "cleanup holder belongs to another Eio domain"
  ;;
end

(* Required original source tail only. A missing outcome is a genuinely deferred
   duty; it is not inferred successful from a cleared stage or an empty queue. *)
module Source_cleanup = struct
  type target =
    | Policy_end of Agent_session.Session_actor.Policy_restoration.t
    | Native_end of Agent_session.Session_actor.Native_setup.t
    | Native_release of Native_loan.t
    | Restored_release of Restored_policy_anchor.t
    | Delegated_end of Agent_session.Session_actor.Delegated_load.Preparation.t

  type t =
    { target : target
    ; mutable original : unit Scope_outcome.primary option
    ; mutable diagnostic : Original_cleanup_report.Segment.t option
    }

  let same_target first second =
    match first, second with
    | Policy_end first, Policy_end second -> phys_equal first second
    | Native_end first, Native_end second -> phys_equal first second
    | Native_release first, Native_release second -> phys_equal first second
    | Restored_release first, Restored_release second -> phys_equal first second
    | Delegated_end first, Delegated_end second -> phys_equal first second
    | ( ( Policy_end _
        | Native_end _
        | Native_release _
        | Restored_release _
        | Delegated_end _ )
      , ( Policy_end _
        | Native_end _
        | Native_release _
        | Restored_release _
        | Delegated_end _ ) ) -> false
  ;;

  let order = function
    | Policy_end _ -> 0
    | Native_end _ -> 1
    | Native_release _ -> 2
    | Restored_release _ -> 3
    | Delegated_end _ -> 4
  ;;

  let compare_order first second = Int.compare (order first.target) (order second.target)

  let capture t f =
    match t.original with
    | Some _ -> invalid_arg "source cleanup original duty already ran"
    | None ->
      let original = Scope_outcome.capture f in
      t.original <- Some original;
      t.diagnostic <- Original_diagnostic.primary original;
      original
  ;;
end

(* The local construction step is performed BEFORE Local binding close. Its
   original receipt is not an outer-tail command and cannot replay an End. *)
module Construction_cleanup = struct
  module End = struct
    type t =
      | Unattempted
      | Admission_retained
      | Attempted of unit Scope_outcome.primary
  end

  type phase =
    | Awaiting_close
    | Close_failed of Scope_outcome.Cleanup.Failure.t
    | Closed of End.t

  type t =
    { construction : Agent_session.Session_actor.Runtime_construction.t
    ; mutable phase : phase
    ; mutable close_diagnostic : Original_cleanup_report.Segment.t option
    ; mutable end_diagnostic : Original_cleanup_report.Segment.t option
    ; mutable startup_release :
        Agent_session.Session_actor.Failed_startup_disposal.t option
    }

  let create construction =
    { construction
    ; phase = Awaiting_close
    ; close_diagnostic = None
    ; end_diagnostic = None
    ; startup_release = None
    }
  ;;

  let construction t = t.construction

  let close_original t =
    match t.phase with
    | Close_failed _ | Closed _ -> invalid_arg "construction cleanup close already ran"
    | Awaiting_close ->
      let original =
        Scope_outcome.capture (fun () ->
          Agent_session.Session_actor.Runtime_construction.close t.construction;
          Ok ())
      in
      t.close_diagnostic <- Original_diagnostic.primary original;
      t.phase
      <- (match original with
          | Returned (Ok ()) -> Closed Unattempted
          | Returned (Error error) -> Close_failed (Protocol error)
          | Raised (exn, backtrace) -> Close_failed (Raised (exn, backtrace)));
      original
  ;;

  let retain_admission t =
    match t.phase with
    | Closed Unattempted -> t.phase <- Closed Admission_retained
    | Awaiting_close | Close_failed _ | Closed (Admission_retained | Attempted _) ->
      invalid_arg "construction cleanup cannot transfer this End obligation"
  ;;

  let record_end t original =
    match t.phase with
    | Closed Unattempted ->
      t.end_diagnostic <- Original_diagnostic.primary original;
      t.phase <- Closed (Attempted original)
    | Awaiting_close | Close_failed _ | Closed (Admission_retained | Attempted _) ->
      invalid_arg "construction cleanup End was displaced or already attempted"
  ;;
end

module Source_transfer = struct
  type proof =
    | Handed_off of Construction_cleanup.t
    | Ordinary_installed
    | Warm_installed

  type t =
    { binding : Runtime_binding.t
    ; proof : proof
    }
end

module Source_origin = struct
  type t =
    | Stage
    | Administration_transition
end

module Recovery_batch = struct
  module Role = struct
    type t =
      | Lease_admission
      | Lease_detachment
      | Source_entry
      | Construction_local of Construction_cleanup.t
      | Construction_exit
      | Ordinary_warm
      | Source_exit
      | Administration_prefix
      | Administration_before_commit
      | Administration_exit
      | Retirement_ordinary
      | Retirement_complete
  end

  type t =
    { role : Role.t
    ; batch : Cleanup_batch.t
    }
end

(* Factual positions on original objects, never cleanup authority or keys for a
   registry. Equal-role actual occurrences retain their original list order. *)
module Physical_cleanup_position = struct
  type t =
    | Lease_pin
    | Administration_precommit
    | Ordinary_suffix
    | Complete_suffix
    | Source_tail of Source_cleanup.t

  let same first second =
    match first, second with
    | Lease_pin, Lease_pin
    | Administration_precommit, Administration_precommit
    | Ordinary_suffix, Ordinary_suffix
    | Complete_suffix, Complete_suffix -> true
    | Source_tail first, Source_tail second -> phys_equal first second
    | ( ( Lease_pin
        | Administration_precommit
        | Ordinary_suffix
        | Complete_suffix
        | Source_tail _ )
      , ( Lease_pin
        | Administration_precommit
        | Ordinary_suffix
        | Complete_suffix
        | Source_tail _ ) ) -> false
  ;;
end

module Selected_native_release = struct
  type t =
    { loan : Native_loan.t
    ; position : Physical_cleanup_position.t
    }
end

module Selected_restored_release = struct
  type t =
    { anchor : Restored_policy_anchor.t
    ; position : Physical_cleanup_position.t
    }
end

module Retirement_cleanup_phase = struct
  type t =
    | Ordinary
    | Complete_resources
  [@@deriving equal]
end

(* These original-only calls have no retained concrete repair authority. Keep
   their actual required/attempted boundary separately from copied target reports;
   never retain the original host callback as a retry operation. *)
module Original_cleanup_step = struct
  module Administration_unload_position = struct
    type t =
      | Source_admission
      | Source_prefix
      | Transition
  end

  type target =
    | Dependency_stop of { closing : bool }
    | Policy_resource of Retirement_cleanup_phase.t
    | Lease_retire
    | Lease_detach
    | Lease_completion_check
    | Lease_cleanup_escape
    | Source_retire
    | Source_physical_selection
    | Source_prefix_selection
    | Source_installed_selection
    | Source_clear
    | Source_exclusion_complete
    | Administration_unload_selection of Administration_unload_position.t
    | Administration_retire_selection
    | Administration_clear
    | Administration_batch_registration
    | Construction_retention_check of Construction_cleanup.t
    | Construction_residency_check of Construction_cleanup.t
    | Local_release_selection of Construction_cleanup.t
    | Warm_release_selection
    | Actor_retirement_selection
    | Complete_constructor_drain
    | Complete_callback_drain
    | Complete_descendant_drain
    | Complete_inference_drain
    | Retirement_detach_selection
    | Selected_lease_cancel of
        { finished : unit Eio.Promise.t
        ; phase : Retirement_cleanup_phase.t
        }
    | Complete_resource_selection
    | Complete_resource_check
    | Retirement_coverage_check
    | Released_resource_check of Session_policy_resource.Cleanup_target.t

  type t =
    { target : target
    ; mutable original : unit Scope_outcome.primary option
    ; mutable diagnostic : Original_cleanup_report.Segment.t option
    ; mutable forwarded_origin : Physical_origin.t option
    }

  let create target =
    { target; original = None; diagnostic = None; forwarded_origin = None }
  ;;

  let record_projection t primary =
    match t.original with
    | Some _ -> invalid_arg "original cleanup step already ran"
    | None ->
      let original =
        match primary with
        | Scope_outcome.Returned (Ok _) -> Scope_outcome.Returned (Ok ())
        | Returned (Error error) -> Returned (Error error)
        | Raised (exn, backtrace) -> Raised (exn, backtrace)
      in
      t.original <- Some original;
      t.diagnostic <- Original_diagnostic.primary original
  ;;

  let capture t f =
    match t.original with
    | Some _ -> invalid_arg "original cleanup step already ran"
    | None ->
      let original = Scope_outcome.capture f in
      record_projection t original;
      original
  ;;

  let capture_reported t f =
    match t.original with
    | Some _ -> invalid_arg "original cleanup step already ran"
    | None ->
      let observed = Physical_origin.capture_result f in
      let original = Physical_origin.primary observed in
      t.original <- Some original;
      t.forwarded_origin <- Some (Physical_origin.of_observation observed);
      original
  ;;

  let capture_unit t f =
    match t.original with
    | Some _ -> invalid_arg "original cleanup step already ran"
    | None ->
      (match f () with
       | () -> record_projection t (Scope_outcome.Returned (Ok ()))
       | exception exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         record_projection t (Scope_outcome.Raised (exn, backtrace));
         Exn.raise_with_original_backtrace exn backtrace)
  ;;

  let projection t ~acquired =
    match t.original with
    | None -> Cleanup_projection.empty
    | Some primary ->
      let origin =
        match t.forwarded_origin with
        | Some _ as origin -> origin
        | None ->
          Option.map t.diagnostic ~f:(fun primary ->
            { Physical_origin.diagnostics = [ primary ]; primary = Some primary })
      in
      Cleanup_projection.of_primary ?origin ~acquired primary
  ;;
end

(* A claim invocation has distinct Actor-drain, pin-release and custody-check
   boundaries. A pin retry cannot erase an unrelated drain/publication failure. *)
module Policy_claim_attempt = struct
  module Slot = Retirement_cleanup_phase

  type phase =
    | Checking_custody
    | Draining_capture
    | Checking_release
    | Releasing_loan
    | Clearing_custody
    | Complete

  type t =
    { mutable phase : phase
    ; mutable drain : unit Scope_outcome.primary option
    ; mutable release : unit Scope_outcome.primary option
    ; mutable original : unit Scope_outcome.primary option
    ; mutable drain_diagnostic : Original_cleanup_report.Segment.t option
    ; mutable release_origin : Physical_origin.t option
    ; mutable control_diagnostic : Original_cleanup_report.Segment.t option
    }

  let create () =
    { phase = Checking_custody
    ; drain = None
    ; release = None
    ; original = None
    ; drain_diagnostic = None
    ; release_origin = None
    ; control_diagnostic = None
    }
  ;;
end

module Original_completion = struct
  module Lease = struct
    type t =
      | No_borrow
      | Borrow_published
  end

  module Stage = struct
    type t =
      | Not_reserved
      | Reserved
  end

  module Retirement = struct
    module Required = struct
      type t =
        | Ordinary
        | All_resources
    end

    module Entered = struct
      type t =
        | Not_entered
        | Ordinary
        | Complete_resources
        | Released_before_physical
    end

    type t =
      { required : Required.t
      ; entered : Entered.t
      }
  end

  type t =
    | Lease of Lease.t
    | Stage of Stage.t
    | Administration_transition
    | Construction
    | Retirement of Retirement.t
end

(* Pure canonical evidence. Source identity only suppresses another view of the
   SAME actual failed producer; it never skips a duty or changes cleanup readiness. *)
module Cleanup_evidence = struct
  module Failure = struct
    type source =
      | Original
      | Retired_actual of cleanup_outcome Eio.Promise.t
      | Retired_prior of cleanup_outcome Eio.Promise.t
      | Native_actual of Native_loan.Attempt.outcome Eio.Promise.t
      | Reported_actual of Agent_session.Cleanup_diagnostic.t * int
      | Reported_prior of Agent_session.Cleanup_diagnostic.t * int
      | Initializer_end of Initialization_capture_data.boundary

    type t =
      { source : source
      ; failure : Scope_outcome.Cleanup.Failure.t
      ; diagnostic : Agent_session.Cleanup_diagnostic.t option
      }

    type key =
      | Retired of cleanup_outcome Eio.Promise.t
      | Native of Native_loan.Attempt.outcome Eio.Promise.t
      | Reported of Agent_session.Cleanup_diagnostic.t * int
      | Initializer of Initialization_capture_data.boundary

    let same_key first second =
      match first, second with
      | Retired first, Retired second -> phys_equal first second
      | Native first, Native second -> phys_equal first second
      | Reported (first, first_index), Reported (second, second_index) ->
        Agent_session.Cleanup_diagnostic.equal first second
        && Int.equal first_index second_index
      | Initializer first, Initializer second -> phys_equal first second
      | ( (Retired _ | Native _ | Reported _ | Initializer _)
        , (Retired _ | Native _ | Reported _ | Initializer _) ) -> false
    ;;
  end

  type t =
    { progress : Cleanup_snapshot.progress
    ; failures : Failure.t list
    ; callbacks :
        (Initialization_capture_data.boundary
        * Agent_session.Callback_cleanup.Completion_observation.t)
          list
    ; custody_joined : bool
    ; original_pieces : Original_cleanup_report.Piece.t list
    ; diagnostics : Agent_session.Cleanup_diagnostic.t list
    }

  let make ?custody_joined ~progress failures =
    let custody_joined =
      Option.value
        custody_joined
        ~default:(Cleanup_snapshot.equal_progress progress Joined)
    in
    { progress
    ; failures
    ; callbacks = []
    ; custody_joined
    ; original_pieces = []
    ; diagnostics =
        List.filter_map failures ~f:(fun failure -> failure.Failure.diagnostic)
    }
  ;;

  let callbacks ~boundary ~custody_joined ~progress callbacks =
    { progress
    ; failures = []
    ; callbacks = List.map callbacks ~f:(fun observation -> boundary, observation)
    ; custody_joined
    ; original_pieces = []
    ; diagnostics = []
    }
  ;;

  let with_diagnostics t diagnostics =
    { t with diagnostics = t.diagnostics @ diagnostics }
  ;;

  let custody_joined duties = List.for_all duties ~f:(fun duty -> duty.custody_joined)
  let with_custody_joined t custody_joined = { t with custody_joined }

  let original_pieces duties =
    List.concat_map duties ~f:(fun duty -> duty.original_pieces)
  ;;

  let with_original t pieces = { t with original_pieces = pieces }

  let own t segment =
    with_original
      (with_diagnostics t (Option.to_list segment))
      (List.map (Option.to_list segment) ~f:(fun actual ->
         Original_cleanup_report.Piece.Own actual))
  ;;

  let own_list duties segment =
    match segment, duties with
    | None, _ -> duties
    | Some segment, [] -> [ own (make ~progress:Joined []) (Some segment) ]
    | Some segment, first :: rest ->
      { first with
        original_pieces =
          Original_cleanup_report.Piece.Own segment :: first.original_pieces
      }
      :: rest
  ;;

  let own_origin t origin =
    with_original
      (with_diagnostics
         t
         (Option.value_map origin ~default:[] ~f:(fun origin ->
            origin.Physical_origin.diagnostics)))
      (Option.value_map origin ~default:[] ~f:(fun origin ->
         List.map origin.Physical_origin.diagnostics ~f:(fun actual ->
           Original_cleanup_report.Piece.Own actual)))
  ;;

  let reported_failures origin ~copy =
    List.concat_map origin.Physical_origin.diagnostics ~f:(fun diagnostic ->
      Agent_session.Cleanup_diagnostic.failures diagnostic
      |> List.mapi ~f:(fun index failure ->
        let source =
          if copy
          then Failure.Reported_prior (diagnostic, index)
          else Failure.Reported_actual (diagnostic, index)
        in
        let failure =
          match failure with
          | Agent_session.Callback_cleanup.Failure.Protocol error ->
            Scope_outcome.Cleanup.Failure.Protocol error
          | Raised (exn, trace) -> Raised (exn, trace)
        in
        { Failure.source; failure; diagnostic = Some diagnostic }))
  ;;

  (* Inventory order describes diagnostic encounter history. Current failure
     order instead starts with the actual producer's declared primary. Keep
     both views paired so selecting H does not reorder historical G metadata. *)
  module Origin_failures = struct
    module D = Agent_session.Cleanup_diagnostic

    type t =
      { failures : Failure.t list
      ; diagnostics : D.t list
      }

    let empty = { failures = []; diagnostics = [] }

    let create origin ~copy ~fallback =
      let inventory =
        Option.value_map origin ~default:[] ~f:(fun origin ->
          reported_failures origin ~copy)
      in
      let diagnostics =
        Option.value_map origin ~default:[] ~f:(fun origin ->
          origin.Physical_origin.diagnostics)
      in
      let diagnostics =
        List.fold fallback ~init:diagnostics ~f:(fun diagnostics failure ->
          match failure.Failure.diagnostic with
          | None -> diagnostics
          | Some diagnostic ->
            if List.mem diagnostics diagnostic ~equal:D.equal
            then diagnostics
            else diagnostics @ [ diagnostic ])
      in
      let failures =
        match fallback with
        | [] -> []
        | _ :: _ ->
          (match Option.bind origin ~f:(fun origin -> origin.Physical_origin.primary) with
           | Some primary
             when (not (List.is_empty (D.failures primary)))
                  && Option.exists origin ~f:(fun origin ->
                    List.mem origin.Physical_origin.diagnostics primary ~equal:D.equal) ->
             let selected, remaining =
               List.partition_tf inventory ~f:(fun failure ->
                 Option.exists failure.Failure.diagnostic ~f:(D.equal primary))
             in
             selected @ remaining
           | Some _ | None -> fallback @ inventory)
      in
      { failures; diagnostics }
    ;;

    let prepend_inventory t failures =
      { failures = failures @ t.failures
      ; diagnostics =
          List.filter_map failures ~f:(fun failure -> failure.Failure.diagnostic)
          @ t.diagnostics
      }
    ;;
  end

  let make_origin_failures ?custody_joined ~progress selected =
    { (make ?custody_joined ~progress selected.Origin_failures.failures) with
      diagnostics = selected.diagnostics
    }
  ;;

  let child_view duties ~diagnostics_complete =
    Original_cleanup_report.create ~diagnostics_complete (original_pieces duties)
  ;;

  let succeeded = make ~progress:Joined []
  let original ?diagnostic failure = { Failure.source = Original; failure; diagnostic }

  let primary_failure = function
    | Scope_outcome.Returned (Ok _) -> None
    | Returned (Error error) -> Some (Scope_outcome.Cleanup.Failure.Protocol error)
    | Raised (exn, backtrace) ->
      Some (Scope_outcome.Cleanup.Failure.Raised (exn, backtrace))
  ;;

  module Selection = struct
    type source =
      | Evidence of Failure.source
      | Callback of
          Initialization_capture_data.boundary
          * Agent_session.Callback_cleanup.Completion.t
          * Agent_session.Callback_cleanup.Completion_snapshot.Failure_slot.kind

    type t =
      { failure : Scope_outcome.Cleanup.Failure.t
      ; source : source
      ; diagnostic : Agent_session.Cleanup_diagnostic.t option
      }
  end

  let snapshot_with_selection duties =
    let actual_retired =
      List.concat_map duties ~f:(fun duty ->
        List.filter_map duty.failures ~f:(fun failure ->
          match failure.Failure.source with
          | Retired_actual producer -> Some producer
          | Original
          | Retired_prior _
          | Native_actual _
          | Reported_actual _
          | Reported_prior _
          | Initializer_end _ -> None))
    in
    let actual_reported =
      List.concat_map duties ~f:(fun duty ->
        List.filter_map duty.failures ~f:(fun failure ->
          match failure.Failure.source with
          | Reported_actual (diagnostic, index) ->
            Some (Failure.Reported (diagnostic, index))
          | Original
          | Retired_actual _
          | Retired_prior _
          | Native_actual _
          | Reported_prior _
          | Initializer_end _ -> None))
    in
    let callback_seen = ref [] in
    let selections_rev = ref [] in
    let _, reversed =
      List.fold duties ~init:([], []) ~f:(fun (seen, reversed) duty ->
        let callback_failures =
          List.concat_map duty.callbacks ~f:(fun (boundary, observation) ->
            let module C = Agent_session.Callback_cleanup in
            let completion = C.Completion_observation.completion observation in
            if List.mem !callback_seen completion ~equal:phys_equal
            then []
            else (
              callback_seen := completion :: !callback_seen;
              C.Completion_snapshot.failure_slots
                (C.Completion_observation.snapshot observation)
              |> List.map ~f:(fun selected ->
                let slot = C.Completion_snapshot.Failure_slot.kind selected in
                match C.Completion_snapshot.Failure_slot.failure selected with
                | C.Failure.Protocol error ->
                  { Selection.failure = Scope_outcome.Cleanup.Failure.Protocol error
                  ; source = Callback (boundary, completion, slot)
                  ; diagnostic = None
                  }
                | Raised (exn, trace) ->
                  { Selection.failure = Raised (exn, trace)
                  ; source = Callback (boundary, completion, slot)
                  ; diagnostic = None
                  })))
        in
        let seen, failures =
          List.fold duty.failures ~init:(seen, []) ~f:(fun (seen, failures) view ->
            let keep key =
              if List.mem seen key ~equal:Failure.same_key
              then seen, failures
              else key :: seen, view :: failures
            in
            match view.source with
            | Original -> seen, view :: failures
            | Retired_actual producer -> keep (Failure.Retired producer)
            | Retired_prior producer ->
              if List.mem actual_retired producer ~equal:phys_equal
              then seen, failures
              else keep (Failure.Retired producer)
            | Native_actual producer -> keep (Failure.Native producer)
            | Reported_actual (diagnostic, index) ->
              keep (Failure.Reported (diagnostic, index))
            | Reported_prior (diagnostic, index) ->
              let key = Failure.Reported (diagnostic, index) in
              if List.mem actual_reported key ~equal:Failure.same_key
              then seen, failures
              else keep key
            | Initializer_end boundary -> keep (Failure.Initializer boundary))
        in
        let failures =
          List.rev_map failures ~f:(fun view ->
            { Selection.failure = view.Failure.failure
            ; source = Evidence view.source
            ; diagnostic = view.diagnostic
            })
          @ callback_failures
        in
        (match duty.progress with
         | Cleanup_snapshot.Joined -> ()
         | Retryable | Blocked | Terminal ->
           selections_rev := List.rev_append failures !selections_rev);
        let failures = List.map failures ~f:(fun failure -> failure.Selection.failure) in
        let duty =
          match duty.progress with
          | Cleanup_snapshot.Joined -> Cleanup_duty.Succeeded
          | Retryable -> Retryable failures
          | Blocked -> Blocked failures
          | Terminal -> Terminal failures
        in
        seen, duty :: reversed)
    in
    Cleanup_snapshot.of_duties (List.rev reversed), List.rev !selections_rev
  ;;

  let snapshot duties = fst (snapshot_with_selection duties)
end

(* Pure projection of the SAME natural producer into the existing claim duty.
   Current repair may settle custody; its original aliases remain immutable. *)
module Policy_claim_pin = struct
  module E = Cleanup_evidence

  module Ownership = struct
    type t =
      | Original
      | Alias
  end

  module Selection = struct
    type t =
      | Missing
      | Current
      | Natural of E.t
  end

  type t =
    { released : bool
    ; selection : Selection.t
    ; origin : Physical_origin.t option
    }

  let observe release ~current ~attempts =
    let released = Policy_claim_release.released release ~current ~attempts in
    let original = Policy_claim_release.original release in
    let selection =
      match current, original with
      | Some _, _ -> Selection.Current
      | None, None -> Missing
      | None, Some original ->
        let observed =
          if released
          then E.succeeded
          else
            E.make_origin_failures
              ~progress:Retryable
              (E.Origin_failures.create
                 (Some original.Policy_claim_release.Original.origin)
                 ~copy:false
                 ~fallback:
                   (List.map
                      (Option.to_list
                         (E.primary_failure
                            (Physical_origin.primary original.observation)))
                      ~f:(fun failure -> E.original failure)))
        in
        Natural observed
    in
    { released
    ; selection
    ; origin = Option.map original ~f:(fun original -> original.origin)
    }
  ;;

  (* A current pin's actual duty is emitted by the existing deferred_pin path.
     Historical natural failure supplies aliases only in that case. None means
     genuinely missing release evidence, not a fabricated successful pin. *)
  let without_attempt t ~custody =
    if t.released
    then Some [ custody ]
    else (
      match t.selection with
      | Selection.Current -> Some [ custody ]
      | Natural observed -> Some [ custody; observed ]
      | Missing -> None)
  ;;

  let attach_original t ~ownership duties =
    match t.origin, duties with
    | None, _ -> duties
    | Some _, [] -> invalid_arg "claim pin origin has no actual custody duty"
    | Some origin, first :: rest ->
      let original_pieces =
        match ownership with
        | Ownership.Alias -> first.E.original_pieces
        | Original ->
          List.map origin.Physical_origin.diagnostics ~f:(fun diagnostic ->
            Original_cleanup_report.Piece.Own diagnostic)
          @ first.E.original_pieces
      in
      { first with
        E.original_pieces
      ; diagnostics = first.diagnostics @ origin.Physical_origin.diagnostics
      }
      :: rest
  ;;
end

module Callback_evidence = struct
  module E = Cleanup_evidence
  module C = Agent_session.Callback_cleanup

  let progress ~pending ~failures =
    if pending
    then Cleanup_snapshot.Blocked
    else if List.is_empty failures
    then Joined
    else Terminal
  ;;

  let diagnostic diagnostic =
    let origin =
      { Physical_origin.diagnostics = [ diagnostic ]; primary = Some diagnostic }
    in
    E.own
      (E.make ~progress:Terminal (E.reported_failures origin ~copy:false))
      (Some diagnostic)
  ;;

  let completion observation ~resolve =
    let completion = C.Completion_observation.completion observation in
    let snapshot = C.Completion_observation.snapshot observation in
    let failures =
      List.concat_map (C.Completion_snapshot.failure_slots snapshot) ~f:(fun selected ->
        let diagnostic =
          resolve
            ~completion
            ~slot:(C.Completion_snapshot.Failure_slot.kind selected)
            ~failure:(C.Completion_snapshot.Failure_slot.failure selected)
        in
        let origin =
          { Physical_origin.diagnostics = [ diagnostic ]; primary = Some diagnostic }
        in
        E.reported_failures origin ~copy:false)
    in
    let custody_joined = Callback_disposal.completion_settled snapshot in
    E.make
      ~custody_joined
      ~progress:(progress ~pending:(not custody_joined) ~failures)
      failures
  ;;

  let pieces diagnostics =
    List.map diagnostics ~f:(fun diagnostic ->
      Original_cleanup_report.Piece.Own diagnostic)
  ;;

  let direct selected =
    match Lease_callback.observe selected with
    | Error failure ->
      let original =
        Original_cleanup_report.create
          ~diagnostics_complete:false
          (pieces (Lease_callback.diagnostics selected))
      in
      [ E.with_original
          (diagnostic failure)
          [ Original_cleanup_report.Piece.Child original ]
      ]
    | Ok observation ->
      let observed =
        completion observation ~resolve:(fun ~completion:_ ~slot ~failure ->
          Lease_callback.diagnostic selected ~slot ~failure)
      in
      let prior_read =
        Option.to_list
          (Option.map (Lease_callback.read_failure selected) ~f:(fun failure ->
             E.with_custody_joined
               (diagnostic failure)
               (Callback_disposal.completion_settled
                  (C.Completion_observation.snapshot observation))))
      in
      let report =
        Original_cleanup_report.create
          ~diagnostics_complete:
            (C.Completion_snapshot.original_finish_settled
               (C.Completion_observation.snapshot observation))
          (pieces (Lease_callback.diagnostics selected))
      in
      E.with_original observed [ Original_cleanup_report.Piece.Child report ]
      :: List.map prior_read ~f:(fun duty -> E.with_original duty [])
  ;;

  let product_operation operation =
    match Product_selection.refresh operation with
    | Error failure ->
      let original =
        Original_cleanup_report.create
          ~diagnostics_complete:false
          (pieces
             (Product_selection.P.Operation_snapshot.diagnostics
                (Product_selection.latest operation)
              @ Option.to_list (Product_selection.read_failure operation)))
      in
      [ E.with_original
          (diagnostic failure)
          [ Original_cleanup_report.Piece.Child original ]
      ]
    | Ok snapshot ->
      let module P = Product_selection.P.Operation_snapshot in
      let callbacks = P.callbacks snapshot in
      let claims = Option.value_map callbacks ~default:[] ~f:C.Cleanup_snapshot.claims in
      let callback_duties =
        List.map claims ~f:(fun observation ->
          completion observation ~resolve:(fun ~completion ~slot ~failure:_ ->
            match P.completion_diagnostic snapshot ~completion ~slot with
            | Some diagnostic -> diagnostic
            | None -> invalid_arg "selected Product callback slot has no original carrier"))
      in
      let terminal =
        match P.terminal snapshot, P.terminal_diagnostic snapshot with
        | None, _ -> E.make ~progress:Blocked []
        | Some (Ok (Ok ())), _ -> E.succeeded
        | Some (Ok (Error _) | Error _), Some failure ->
          E.with_custody_joined (diagnostic failure) true
        | Some (Ok (Error _) | Error _), None ->
          invalid_arg "selected Product terminal failure has no original carrier"
      in
      let parent =
        E.make
          ~custody_joined:(P.registration_closed snapshot)
          ~progress:(if P.registration_closed snapshot then Joined else Blocked)
          []
      in
      let callback_custody =
        Option.for_all callbacks ~f:Callback_disposal.parent_settled
      in
      let terminal =
        E.with_custody_joined
          terminal
          (Option.is_some (P.terminal snapshot) && callback_custody)
      in
      let read =
        Option.to_list
          (Option.map (Product_selection.read_failure operation) ~f:(fun failure ->
             E.with_custody_joined
               (diagnostic failure)
               (P.registration_closed snapshot
                && Option.is_some (P.terminal snapshot)
                && callback_custody)))
      in
      let duties = parent :: (callback_duties @ [ terminal ] @ read) in
      let complete =
        P.registration_closed snapshot
        && Option.is_some (P.terminal snapshot)
        && Option.for_all callbacks ~f:Callback_disposal.parent_diagnostics_sealed
      in
      let original =
        Original_cleanup_report.create
          ~diagnostics_complete:complete
          (pieces
             (P.diagnostics snapshot
              @ Option.to_list (Product_selection.read_failure operation)))
      in
      (match duties with
       | [] -> assert false
       | first :: rest ->
         E.with_original first [ Original_cleanup_report.Piece.Child original ]
         :: List.map rest ~f:(fun duty -> E.with_original duty []))
  ;;

  let product = function
    | Product_selection.Read_failed failure ->
      let original =
        Original_cleanup_report.create ~diagnostics_complete:false (pieces [ failure ])
      in
      [ E.with_original
          (diagnostic failure)
          [ Original_cleanup_report.Piece.Child original ]
      ]
    | Selected operations -> List.concat_map operations ~f:product_operation
  ;;
end

let%expect_test "cleanup evidence preserves producer identity and owned failure order" =
  Eio_main.run (fun _ ->
    let module E = Cleanup_evidence in
    let producer_a, _ = Eio.Promise.create () in
    let producer_b, _ = Eio.Promise.create () in
    let sentinel = Failure "shared close exception" in
    let trace_a = Stdlib.Printexc.get_callstack 8 in
    let trace_b = Stdlib.Printexc.get_callstack 8 in
    assert (not (phys_equal trace_a trace_b));
    let view source trace =
      E.make
        ~progress:Retryable
        [ { E.Failure.source
          ; failure = Scope_outcome.Cleanup.Failure.Raised (sentinel, trace)
          ; diagnostic = None
          }
        ]
    in
    let actual_a = view (Retired_actual producer_a) trace_a in
    let prior_a = view (Retired_prior producer_a) trace_a in
    let actual_b = view (Retired_actual producer_b) trace_b in
    let check duties expected_traces =
      let snapshot = E.snapshot duties in
      assert (
        Cleanup_snapshot.equal_progress (Cleanup_snapshot.progress snapshot) Retryable);
      List.iter2_exn
        (Cleanup_snapshot.failures snapshot)
        expected_traces
        ~f:(fun actual expected_trace ->
          match actual with
          | Raised (actual, trace) ->
            assert (phys_equal actual sentinel);
            assert (phys_equal trace expected_trace)
          | Protocol _ -> failwith "close exception became a protocol failure")
    in
    (* Pairing follows failures retained by the produced duty, including
       Succeeded dropping incidental data. Identity dedup still runs before that
       projection exactly as in the original fold. *)
    List.iter
      [ Cleanup_snapshot.Joined; Retryable; Blocked; Terminal ]
      ~f:(fun progress ->
        let duties =
          [ E.make
              ~progress
              [ { E.Failure.source = Original
                ; failure = Raised (sentinel, trace_a)
                ; diagnostic = None
                }
              ]
          ; actual_b
          ]
        in
        let snapshot, selections = E.snapshot_with_selection duties in
        List.iter2_exn
          (Cleanup_snapshot.failures snapshot)
          selections
          ~f:(fun failure selected ->
            match failure, selected.E.Selection.failure with
            | Raised (first, first_trace), Raised (second, second_trace) ->
              assert (phys_equal first second);
              assert (phys_equal first_trace second_trace)
            | Protocol _, Protocol _ | Raised _, Protocol _ | Protocol _, Raised _ ->
              failwith "paired failure selection changed"));
    (* Expected order follows owned operations, not a copied earlier view. *)
    check [ prior_a; actual_a; actual_b ] [ trace_a; trace_b ];
    check [ prior_a; actual_b; actual_a ] [ trace_b; trace_a ];
    check [ actual_a; actual_a; prior_a ] [ trace_a ];
    check [ prior_a ] [ trace_a ];
    (* A lower producer may retain multiple independent failures. Preserve each
       index, aliases and actual-owned order without equating identical payloads. *)
    let module D = Agent_session.Cleanup_diagnostic in
    let module CO = Agent_session.Close_observation in
    let shared = [ D.Failure.Raised (sentinel, trace_a); Raised (sentinel, trace_b) ] in
    let group_a = D.create shared in
    let group_b = D.create shared in
    let trace_c = Stdlib.Printexc.get_callstack 8 in
    let group_c = D.create [ Raised (sentinel, trace_c) ] in
    let origin group =
      CO.create (Raised (sentinel, trace_a)) ~diagnostics:[ group ] ~primary:group ()
      |> Physical_origin.of_observation
    in
    let reported group ~copy =
      E.make ~progress:Retryable (E.reported_failures (origin group) ~copy)
    in
    let actual_a = reported group_a ~copy:false in
    let actual_b = reported group_b ~copy:false in
    let copied_a = reported group_a ~copy:true in
    let actual_c = reported group_c ~copy:false in
    check
      [ copied_a; actual_b; actual_c; actual_a ]
      [ trace_a; trace_b; trace_c; trace_a; trace_b ];
    check [ actual_a; actual_a; copied_a ] [ trace_a; trace_b ];
    check [ copied_a ] [ trace_a; trace_b ];
    (* Current first-error order and original observation order are distinct.
       A late release H precedes old lexical G in the fresh projection; original
       End already owned G before H existed. Explicit primary, not first original
       group, drives legacy projection. Both views retain the SAME carriers. *)
    let g = D.create [ Raised (sentinel, trace_a) ] in
    let h = D.create [ Raised (sentinel, trace_b) ] in
    let original =
      Original_cleanup_report.create
        ~diagnostics_complete:true
        [ Own g; Own h; Dependency_copy g ]
    in
    assert (List.equal D.equal (Original_cleanup_report.diagnostics original) [ g; h ]);
    let current first trace second =
      Cleanup_projection.create
        (Scope_outcome.Cleanup.Failed
           (Raised (sentinel, trace), [ Raised (sentinel, second) ]))
        ~origin:
          (Some { Physical_origin.diagnostics = [ first; g ]; primary = Some first })
    in
    let selected = current h trace_b trace_a in
    (match Cleanup_projection.origin selected with
     | Some { diagnostics; primary = Some primary } ->
       assert (List.equal D.equal diagnostics [ h; g ]);
       assert (D.equal primary h)
     | None | Some { primary = None; _ } -> failwith "fresh primary lost its carrier");
    let positive = Cleanup_projection.create Scope_outcome.Cleanup.Joined ~origin:None in
    (* A successful actual reported exit may carry independent diagnostics.
       Preserve their identities/order without turning success into a failure. *)
    let positive_groups groups =
      let observed = CO.create (Returned (Ok ())) ~diagnostics:groups () in
      Cleanup_projection.of_primary
        ~origin:(Physical_origin.of_observation observed)
        ~acquired:true
        (Physical_origin.primary observed)
    in
    let positives =
      Cleanup_projection.combine (positive_groups [ g ]) (positive_groups [ h ])
    in
    (match Cleanup_projection.cleanup positives with
     | Scope_outcome.Cleanup.Joined -> ()
     | Never_acquired | Failed _ -> failwith "positive diagnostics changed the raw exit");
    (match Cleanup_projection.origin positives with
     | Some { diagnostics; primary = None } ->
       assert (List.equal D.equal diagnostics [ g; h ])
     | None | Some { primary = Some _; _ } ->
       failwith "positive producer groups gained a primary or disappeared");
    let positive_receipt =
      Cleanup_batch.attempted_projection
        ~origin:(Some { Physical_origin.diagnostics = [ g; h ]; primary = Some g })
        (Ok ())
    in
    (match Cleanup_projection.cleanup positive_receipt with
     | Scope_outcome.Cleanup.Never_acquired -> ()
     | Joined | Failed _ -> failwith "positive batch receipt changed flat cleanup");
    (match Cleanup_projection.origin positive_receipt with
     | Some { diagnostics; primary = None } ->
       assert (List.equal D.equal diagnostics [ g; h ])
     | None | Some { primary = Some _; _ } ->
       failwith "positive batch receipt lost groups or retained a failure selection");
    let positive_then_negative = Cleanup_projection.combine positives selected in
    (match Cleanup_projection.origin positive_then_negative with
     | Some { diagnostics; primary = Some primary } ->
       assert (List.equal D.equal diagnostics [ g; h; h; g ]);
       assert (D.equal primary h)
     | None | Some { primary = None; _ } ->
       failwith "positive diagnostic displaced the later actual error");
    (* The paired current boundary must honor H even when positive earlier
       evidence leaves G first in the encounter inventory. Failure keys and
       the original G,H inventory remain unchanged. *)
    let current_evidence =
      E.Origin_failures.create
        (Cleanup_projection.origin positive_then_negative)
        ~copy:false
        ~fallback:[ E.original (Raised (sentinel, trace_b)) ]
      |> E.make_origin_failures ~progress:Retryable
    in
    assert (List.equal D.equal current_evidence.diagnostics [ g; h; h; g ]);
    check [ current_evidence ] [ trace_b; trace_a ];
    let _, selections = E.snapshot_with_selection [ current_evidence ] in
    assert (
      List.equal
        (Option.equal D.equal)
        (List.map selections ~f:(fun selected -> selected.E.Selection.diagnostic))
        [ Some h; Some g ]);
    let current_evidence =
      E.with_original current_evidence [ Original_cleanup_report.Piece.Child original ]
    in
    assert (
      List.equal
        D.equal
        (Original_cleanup_report.diagnostics
           (E.child_view [ current_evidence ] ~diagnostics_complete:true))
        [ g; h ]);
    let selected = Cleanup_projection.combine positive selected in
    let selected = Cleanup_projection.combine selected (current g trace_a trace_b) in
    (match Cleanup_projection.origin selected with
     | Some { primary = Some primary; _ } -> assert (D.equal primary h)
     | None | Some { primary = None; _ } -> failwith "combine changed first error");
    let calls = ref 0 in
    let unreported =
      Physical_origin.capture_result (fun () ->
        Int.incr calls;
        CO.create (Raised (sentinel, trace_a)) ~diagnostics:[] ())
    in
    [%test_eq: int] 1 !calls;
    let boundary_origin = Physical_origin.of_observation unreported in
    let boundary =
      E.make ~progress:Retryable (E.reported_failures boundary_origin ~copy:false)
    in
    check [ boundary; boundary ] [ trace_a ];
    (match CO.primary unreported, CO.diagnostics unreported with
     | Some primary, [ diagnostic ] -> assert (D.equal primary diagnostic)
     | _ -> failwith "unreported actual negative lost its one boundary carrier");
    let known =
      CO.create (Raised (sentinel, trace_a)) ~diagnostics:[ group_a ] ~primary:group_a ()
    in
    assert (phys_equal known (Physical_origin.capture_result (fun () -> known)));
    let unattributed incoming =
      let original = CO.create (Raised (sentinel, trace_c)) ~diagnostics:incoming () in
      let observed = Physical_origin.capture_result (fun () -> original) in
      (match CO.primary observed with
       | Some primary ->
         assert (not (List.mem incoming primary ~equal:D.equal));
         assert (
           List.equal D.equal (List.drop_last_exn (CO.diagnostics observed)) incoming)
       | None -> failwith "unattributed negative guessed no boundary primary");
      E.make
        ~progress:Retryable
        (E.reported_failures (Physical_origin.of_observation observed) ~copy:false)
    in
    check [ unattributed [ group_a ] ] [ trace_a; trace_b; trace_c ];
    check [ unattributed [ D.create [] ] ] [ trace_c ];
    let unreported_protocol =
      let error = Agent_protocol.Error.invalid_request "actual opaque protocol close" in
      let observed =
        Physical_origin.capture_result (fun () ->
          CO.create (Returned (Error error)) ~diagnostics:[] ())
      in
      match CO.diagnostics observed with
      | [ diagnostic ] ->
        (match D.failures diagnostic with
         | [ Protocol actual ] -> assert (phys_equal actual error)
         | _ -> failwith "opaque protocol failure changed actual payload")
      | _ -> failwith "opaque protocol failure lost actual boundary carrier"
    in
    unreported_protocol;
    print_endline
      "one failure per producer; shared exception preserves distinct owned order");
  [%expect
    {| one failure per producer; shared exception preserves distinct owned order |}]
;;

let%expect_test "cleanup evidence suppresses copied failures without suppressing duties" =
  Eio_main.run (fun _ ->
    let module E = Cleanup_evidence in
    let producer, _ = Eio.Promise.create () in
    let sentinel = Failure "retained close failed" in
    let trace = Stdlib.Printexc.get_callstack 8 in
    let failure source =
      { E.Failure.source
      ; failure = Scope_outcome.Cleanup.Failure.Raised (sentinel, trace)
      ; diagnostic = None
      }
    in
    let terminal = E.make ~progress:Terminal [ failure (Retired_actual producer) ] in
    let blocked = E.make ~progress:Blocked [ failure (Retired_prior producer) ] in
    let repairable = E.make ~progress:Retryable [] in
    let check duties progress =
      let snapshot = E.snapshot duties in
      assert (
        Cleanup_snapshot.equal_progress (Cleanup_snapshot.progress snapshot) progress);
      match Cleanup_snapshot.failures snapshot, Cleanup_snapshot.cleanup snapshot with
      | ( [ Raised (actual, actual_trace) ]
        , Some (Failed (Raised (cleanup, cleanup_trace), [])) ) ->
        assert (phys_equal actual sentinel);
        assert (phys_equal cleanup sentinel);
        assert (phys_equal actual_trace trace);
        assert (phys_equal cleanup_trace trace)
      | _ -> failwith "next-action priority erased or duplicated the actual failure"
    in
    check [ terminal ] Terminal;
    check [ terminal; blocked ] Blocked;
    check [ terminal; blocked; repairable ] Retryable;
    let unfinished = E.snapshot [ E.make ~progress:Blocked [] ] in
    assert (Cleanup_snapshot.equal_progress (Cleanup_snapshot.progress unfinished) Blocked);
    assert (Option.is_none (Cleanup_snapshot.cleanup unfinished));
    let joined = E.snapshot [ E.succeeded ] in
    assert (Cleanup_snapshot.equal_progress (Cleanup_snapshot.progress joined) Joined);
    (match Cleanup_snapshot.cleanup joined with
     | Some Joined -> ()
     | Some (Never_acquired | Failed _) | None -> failwith "positive duty did not join");
    print_endline
      "retryable then blocked then terminal; copied failures never settle a duty");
  [%expect
    {| retryable then blocked then terminal; copied failures never settle a duty |}]
;;

(* Captured DATA only. Execution/retry remains in the later exact Owner
   interpreter; no arbitrary callback/closer or public success setter. *)
(* Actual dependency-report DATA. Identity stays on the admitted background
   lease; only this finite report slot is retained by an enclosing retirement.
   A consumer snapshot never becomes a recursive consumer-recovery edge. *)
module Relation_evidence = struct
  module CO = Agent_session.Close_observation

  type observation = (unit, Agent_protocol.Error.t) result CO.t

  module Native_original = struct
    type t =
      { observation : observation
      ; outer_escape : observation option
      }
  end

  module Mode = struct
    type t =
      | Ordinary
      | Native
  end

  module Join_mode = struct
    type t =
      | Original
      | Recovery_retry
      | Reobserve
  end

  module Join_outcome = struct
    type t =
      | Canonical_consumer of observation
      | Join_control of observation
      | Continue_provider

    let observation = function
      | Canonical_consumer observation | Join_control observation -> observation
      | Continue_provider ->
        (* Scheduling continuation, not evidence that a consumer was joined. *)
        CO.create (Returned (Ok ())) ~diagnostics:[] ()
    ;;
  end

  module Cancellation = struct
    type t =
      | Skip
      | Attempt
  end

  module Run_result = struct
    type t =
      { join : Join_outcome.t
      ; cancellation : observation option
      }

    let observation t =
      let module CO = Agent_session.Close_observation in
      let joined = Join_outcome.observation t.join in
      match t.cancellation with
      | None -> joined
      | Some cancellation ->
        let selected =
          match CO.exit joined with
          | Returned (Error _) | Raised _ -> joined
          | Returned (Ok ()) -> cancellation
        in
        CO.create
          (CO.exit selected)
          ~diagnostics:(CO.diagnostics joined @ CO.diagnostics cancellation)
          ?primary:(CO.primary selected)
          ()
    ;;
  end

  module Join_control = struct
    type t =
      | Blocking of observation
      | Settled of observation

    let observation = function
      | Blocking observation | Settled observation -> observation
    ;;
  end

  type transport =
    | Ordinary_scope
    | Native_scope of Native_original.t option

  type 'guard consumer =
    | Unfinished
    | Observed of
        { guard : 'guard
        ; evidence : Cleanup_evidence.t list
        }

  module Selection = struct
    type t =
      | Active_revocation
      | Detached_handoff
  end

  module Operation = struct
    type t =
      { selection : Selection.t
      ; run : Join_mode.t -> cancellation:Cancellation.t -> Run_result.t
      }
  end

  type 'guard t =
    { mutable transport : transport
    ; mutable consumer : 'guard consumer
    ; mutable operation : Operation.t option
    }

  let ordinary () =
    { transport = Ordinary_scope; consumer = Unfinished; operation = None }
  ;;

  let native () =
    { transport = Native_scope None; consumer = Unfinished; operation = None }
  ;;

  (* First actual Retirement_lease enrollment installs under the provider lock.
     Selection is immutable: an active revocation cannot later be downgraded to
     historical report handoff. Reenrollment shares this SAME operation. *)
  let install_operation t operation =
    match t.operation with
    | None ->
      t.operation <- Some operation;
      operation
    | Some actual -> actual
  ;;

  let seal_native t ~observation ~outer_escape =
    match t.transport with
    | Native_scope None ->
      t.transport <- Native_scope (Some { Native_original.observation; outer_escape })
    | Native_scope (Some _) -> invalid_arg "native relation receipt was already sealed"
    | Ordinary_scope -> invalid_arg "ordinary relation cannot own a native receipt"
  ;;

  let observe_consumer t ~guard ~evidence = t.consumer <- Observed { guard; evidence }

  module Native_evidence = struct
    type t =
      { duties : Cleanup_evidence.t list
      ; complete : bool
      }
  end

  let native_evidence t =
    let module E = Cleanup_evidence in
    match t.transport with
    | Ordinary_scope -> Some { Native_evidence.duties = []; complete = true }
    | Native_scope None -> None
    | Native_scope (Some receipt) ->
      let aliases =
        E.own_origin
          E.succeeded
          (Some (Physical_origin.of_observation receipt.observation))
      in
      let control, complete =
        match receipt.outer_escape with
        | None -> [], true
        | Some escape ->
          (match CO.exit escape with
           | Returned (Ok ()) ->
             invalid_arg "native outer escape was reported as positive"
           | Returned (Error _) | Raised _ ->
             let origin = Physical_origin.of_observation escape in
             let declared =
               Option.exists origin.primary ~f:(fun primary ->
                 not (List.is_empty (Agent_session.Cleanup_diagnostic.failures primary)))
             in
             if declared
             then
               ( [ E.make ~progress:Terminal (E.reported_failures origin ~copy:false) ]
               , true )
             else [ E.make ~progress:Terminal [] ], false)
      in
      Some { Native_evidence.duties = control @ [ aliases ]; complete }
  ;;

  let native_handoff_evidence t =
    match t.transport, native_evidence t with
    | Ordinary_scope, Some actual -> Some actual
    | Native_scope (Some receipt), Some { Native_evidence.complete; _ } ->
      (* The original returned bracket owns this exact old control. Historical
         handoff forwards its aliases without newly retiring the downstream
         consumer or selecting that old control as provider-current failure. *)
      Some
        { Native_evidence.duties =
            [ Cleanup_evidence.own_origin
                Cleanup_evidence.succeeded
                (Some (Physical_origin.of_observation receipt.observation))
            ]
        ; complete
        }
    | (Ordinary_scope | Native_scope _), None -> None
    | Native_scope None, Some _ -> invalid_arg "unsealed native receipt has evidence"
  ;;

  let selected_native_evidence t =
    match t.operation with
    | None -> None
    | Some { Operation.selection = Active_revocation; _ } -> native_evidence t
    | Some { selection = Detached_handoff; _ } -> native_handoff_evidence t
  ;;
end

(* Exact Source selection facts survive on the SAME recovery after the lexical
   extent clears. This slot is DATA, not another recovery/retirement edge. *)
module Source_resource_association = struct
  type basis =
    { binding : Runtime_binding.t option
    ; handoff : Resource_handoff.t option
    ; restored : Restored_policy_anchor.t option
    ; native : Native_loan.t option
    }

  type t =
    { mutable target : Session_policy_resource.Cleanup_target.t option
    ; mutable basis : basis option
    }

  let create () = { target = None; basis = None }

  let capture t ~binding ~handoff ~restored ~native =
    match t.basis with
    | None -> t.basis <- Some { binding; handoff; restored; native }
    | Some _ -> invalid_arg "Source resource basis was already captured"
  ;;
end

module Recovery_data = struct
  module Native_release_report = struct
    module Selection = struct
      type t =
        | Released
        | Missing of Agent_protocol.Error.t
        | Join of Native_loan.Attempt.t
        | Close of Native_loan.Attempt.t
    end

    type t =
      { loan : Native_loan.t
      ; position : Physical_cleanup_position.t
      ; mutable attempt : Native_loan.Attempt.t option
      ; mutable selection : Selection.t Scope_outcome.primary option
      ; mutable commit : unit Scope_outcome.primary option
      ; mutable operation : Native_loan.Attempt.outcome option
      ; mutable original : Native_loan.Attempt.outcome option
      ; original_registration : bool
      ; mutable selection_diagnostic : Original_cleanup_report.Segment.t option
      ; mutable commit_diagnostic : Original_cleanup_report.Segment.t option
      }
  end

  module Restored_release_report = struct
    type t =
      { anchor : Restored_policy_anchor.t
      ; position : Physical_cleanup_position.t
      ; mutable selection_revoke : unit Scope_outcome.primary option
      ; mutable helper_revoke : unit Scope_outcome.primary option
      ; mutable clear : unit Scope_outcome.primary option
      ; mutable operation : unit Scope_outcome.primary option
      ; mutable original : unit Scope_outcome.primary option
      ; original_registration : bool
      ; mutable selection_diagnostic : Original_cleanup_report.Segment.t option
      ; mutable revoke_diagnostic : Original_cleanup_report.Segment.t option
      ; mutable operation_origin : Physical_origin.t option
      ; mutable clear_diagnostic : Original_cleanup_report.Segment.t option
      }
  end

  module Kind = struct
    type t =
      | Source of Source_origin.t
      | Lease
      | Construction
      | Retirement
  end

  module Completion_requirement = struct
    type t =
      | Legacy
      | Canonical_custody
  end

  type t =
    { affinity : Owner_affinity.t
    ; kind : Kind.t
    ; completion_requirement : Completion_requirement.t
    ; source_association : Source_resource_association.t option
    ; mutable phase : phase
    ; mutable original_initializers_sealed : bool
    }

  and captured =
    { initializations_rev : Initialization_capture_data.boundary list
    ; callbacks_rev : Lease_callback.t list
    ; disposal_check : disposal_check option
    ; transfers_rev : Source_transfer.t list
    ; observing : current option
    ; batches_rev : Recovery_batch.t list
    ; leases_rev : lease list
    ; source_tail_rev : Source_cleanup.t list
    ; prefix : t option
    ; actor_cleanup : Actor_cleanup_step.t option
    ; native_releases_rev : Selected_native_release.t list
    ; native_release_reports_rev : Native_release_report.t list
    ; restored_releases_rev : Selected_restored_release.t list
    ; constructions_rev : Construction_cleanup.t list
    ; original_steps_rev : Original_cleanup_step.t list
    ; policy_claims_rev : policy_claim list
    ; restored_release_reports_rev : Restored_release_report.t list
    ; pending_source : t option
    ; preparation_sources_rev : t list
    }

  (* Identity is retained for the actual completed guard only. It is not a new
     dependency/execution edge and the observer never traverses it recursively. *)
  and disposal_check =
    { source : t option
    ; guard : Original_cleanup_step.t
    ; physical_ready : bool
    ; source_custody_joined : bool
    }

  (* Only an actual reported release stores [startup_settlement]. A speculative
     prerequisite is passed explicitly through pure observation, never stored. *)
  and startup_candidate =
    { retirement : t
    ; check : disposal_check
    ; source : t
    ; construction : Construction_cleanup.t
    }

  and startup_settlement =
    { candidate : startup_candidate
    ; release : Agent_session.Session_actor.Failed_startup_disposal.t
    }

  and lease =
    { finished : unit Eio.Promise.t
    ; recovery : t
    ; role : lease_role
    ; relation : relation_guard Relation_evidence.t option
    ; mutable join_original : Relation_evidence.Join_outcome.t option
    ; mutable join_current_control : Relation_evidence.Join_control.t option
    ; mutable retry_cancel : Relation_evidence.observation option
    ; mutable operation : Relation_evidence.Operation.t option
    }

  and relation_guard = { scopes : (t * completed) list }

  and lease_role =
    | Source_lexical
    | Retirement_lease of
        { phase : Retirement_cleanup_phase.t
        ; cancel : Original_cleanup_step.t
        }

  and policy_claim =
    { capture : Agent_session.Session_actor.Delegated_policy_claim.Capture.t
    ; loan : Session_policy_resource.Lease.t
    ; runtime : Agent_session.Runtime_builder.t
    ; wrapper_finished : unit Eio.Promise.t
    ; wrapper_recovery : t
    ; natural_release : Policy_claim_release.t
    ; mutable selected_phases : Retirement_cleanup_phase.t list
    ; mutable ordinary_attempt : Policy_claim_attempt.t option
    ; mutable complete_attempt : Policy_claim_attempt.t option
    }

  and completed =
    { original : Scope_outcome.Cleanup.t
    ; original_projection : Cleanup_projection.t
    ; captured : captured
    ; completion : Original_completion.t
    ; current : current
    ; latest : Cleanup_snapshot.t
    }

  and current =
    { owner : t
    ; mutable startup_settlement : startup_settlement option
    ; retired : retired_current list
    ; native : native_current list
    ; restored : restored_current list
    ; source_ends : source_end_current list
    ; constructions : construction_current list
    ; actor : actor_current option
    ; claims : claim_current list
    }

  and retired_current =
    { resource : Retired_resource.t
    ; origins : Retired_resource.Receipt.t list
    ; mutable retired_latest : reported_batch option
    }

  and native_current =
    { loan : Native_loan.t
    ; mutable native_latest : reported_native option
    }

  and restored_current =
    { anchor : Restored_policy_anchor.t
    ; mutable restored_latest : reported_restored option
    }

  and reported_batch =
    { after : Cleanup_snapshot.t
    ; owner : t
    ; batch : Cleanup_batch.t
    }

  and reported_native =
    { after : Cleanup_snapshot.t
    ; owner : t
    ; report : Native_release_report.t
    }

  and reported_restored =
    { after : Cleanup_snapshot.t
    ; owner : t
    ; report : Restored_release_report.t
    }

  and source_end_current =
    { duty : Source_cleanup.t
    ; mutable end_latest : unit Scope_outcome.primary option
    ; mutable end_diagnostic : Agent_session.Cleanup_diagnostic.t option
    }

  and construction_current =
    { receipt : Construction_cleanup.t
    ; mutable construction_end : unit Scope_outcome.primary option
    ; mutable construction_end_diagnostic : Agent_session.Cleanup_diagnostic.t option
    }

  and actor_current =
    { step : Actor_cleanup_step.t
    ; mutable initial : reported_actor option
    ; mutable foreground : unit Scope_outcome.primary option
    ; mutable foreground_diagnostic : Agent_session.Cleanup_diagnostic.t option
    ; mutable final : reported_actor option
    }

  and reported_actor =
    { after : Cleanup_snapshot.t
    ; owner : t
    ; result : Actor_cleanup_step.Cleanup.Snapshot.t Scope_outcome.primary
    ; diagnostic : Agent_session.Cleanup_diagnostic.t option
    }

  and claim_current =
    { claim : policy_claim
    ; mutable claim_pin : unit Scope_outcome.primary option
    ; mutable claim_pin_origin : Physical_origin.t option
    ; mutable claim_clear : unit Scope_outcome.primary option
    ; mutable claim_clear_diagnostic : Agent_session.Cleanup_diagnostic.t option
    }

  and reserved =
    { holder : t
    ; before : completed
    ; mutable executed : bool
    }

  and phase =
    | Original_running of captured
    | Complete of completed
    | Retrying of reserved

  (* The same original dependencies as retry reservation. A finite guard checks
     their actual completion identities without traversing evidence recursively. *)
  let captured_children (captured : captured) =
    captured.preparation_sources_rev
    @ Option.to_list captured.pending_source
    @ List.map captured.leases_rev ~f:(fun lease -> lease.recovery)
    @ List.map captured.policy_claims_rev ~f:(fun claim -> claim.wrapper_recovery)
    @ Option.to_list captured.prefix
  ;;

  module Relation_guard = struct
    type recovery = t
    type t = relation_guard

    type status =
      | Current
      | Busy
      | Stale

    let check t =
      List.fold t.scopes ~init:Current ~f:(fun status (holder, completed) ->
        Owner_affinity.check_domain_exn holder.affinity;
        match holder.phase with
        | Original_running _ | Retrying _ -> Busy
        | Complete current ->
          if phys_equal current completed
          then status
          else (
            match status with
            | Busy -> Busy
            | Current | Stale -> Stale))
    ;;

    let capture (root : recovery) =
      let selected = ref [] in
      let rec visit path (holder : recovery) =
        Owner_affinity.check_domain_exn holder.affinity;
        if List.mem path holder ~equal:phys_equal
        then invalid_arg "original cleanup dependencies contain a cycle"
        else if List.exists !selected ~f:(fun (actual, _) -> phys_equal actual holder)
        then Ok ()
        else (
          match holder.phase with
          | Original_running _ | Retrying _ ->
            Error
              (Agent_protocol.Error.create
                 Conflict
                 ~message:"consumer cleanup is not a completed original observation"
                 ~retryable:true
                 ())
          | Complete completed ->
            selected := (holder, completed) :: !selected;
            List.fold_result
              (captured_children completed.captured)
              ~init:()
              ~f:(fun () child -> visit (holder :: path) child))
      in
      Result.map (visit [] root) ~f:(fun () -> { scopes = List.rev !selected })
    ;;
  end

  let create_with_completion_requirement ~affinity ~kind ~completion_requirement =
    { affinity
    ; kind
    ; completion_requirement
    ; source_association =
        (match kind with
         | Source _ -> Some (Source_resource_association.create ())
         | Lease | Construction | Retirement -> None)
    ; original_initializers_sealed = true
    ; phase =
        Original_running
          { initializations_rev = []
          ; callbacks_rev = []
          ; disposal_check = None
          ; transfers_rev = []
          ; observing = None
          ; batches_rev = []
          ; leases_rev = []
          ; source_tail_rev = []
          ; prefix = None
          ; actor_cleanup = None
          ; native_releases_rev = []
          ; native_release_reports_rev = []
          ; restored_releases_rev = []
          ; constructions_rev = []
          ; original_steps_rev = []
          ; policy_claims_rev = []
          ; restored_release_reports_rev = []
          ; pending_source = None
          ; preparation_sources_rev = []
          }
    }
  ;;

  let create ~affinity ~kind =
    create_with_completion_requirement ~affinity ~kind ~completion_requirement:Legacy
  ;;

  let record_disposal_check (t : t) check =
    Owner_affinity.check_domain_exn t.affinity;
    match t.phase with
    | Original_running captured ->
      if Option.is_some captured.disposal_check
      then invalid_arg "permanent disposal check was observed twice";
      if not (List.mem captured.original_steps_rev check.guard ~equal:phys_equal)
      then invalid_arg "permanent disposal check belongs to another original scope";
      (match check.guard.target with
       | Complete_resource_check -> ()
       | _ -> invalid_arg "permanent disposal check has the wrong original target");
      t.phase <- Original_running { captured with disposal_check = Some check }
    | Complete _ | Retrying _ ->
      invalid_arg "permanent disposal check requires its original producer"
  ;;

  let callback_registration_status (t : t) =
    let refused message =
      Error (Agent_protocol.Error.create Conflict ~message ~retryable:false ())
    in
    if not (Int.equal t.affinity.domain (Stdlib.Domain.self () :> int))
    then refused "callback lease belongs to another creating domain"
    else (
      match t.kind, t.phase with
      | Lease, Original_running _ -> Ok ()
      | Lease, (Complete _ | Retrying _) ->
        refused "callback registration requires its original running holder"
      | ( (Source _ | Retirement | Construction)
        , (Original_running _ | Complete _ | Retrying _) ) ->
        refused "direct callback registration requires its actual lease")
  ;;

  let check_callback_registration_open t =
    match callback_registration_status t with
    | Ok () -> ()
    | Error error -> invalid_arg error.message
  ;;

  let register_callback (t : t) completion =
    check_callback_registration_open t;
    match t.phase with
    | Complete _ | Retrying _ -> assert false
    | Original_running captured ->
      if
        not
          (List.exists captured.callbacks_rev ~f:(fun prior ->
             phys_equal (Lease_callback.completion prior) completion))
      then (
        let observation =
          match
            Agent_session.Callback_cleanup.Completion_observation.observe completion
          with
          | Ok observation -> observation
          | Error _ -> invalid_arg "callback receipt belongs to another creating domain"
        in
        t.phase
        <- Original_running
             { captured with
               callbacks_rev = Lease_callback.create observation :: captured.callbacks_rev
             })
  ;;

  let register_initialization (t : t) boundary =
    Owner_affinity.check_domain_exn t.affinity;
    match t.phase with
    | Complete _ | Retrying _ ->
      invalid_arg "initializer registration requires its original running holder"
    | Original_running captured ->
      if
        List.exists captured.initializations_rev ~f:(fun prior ->
          Initialization_capture_data.same_scope prior boundary)
      then invalid_arg "original initialization scope was enrolled twice";
      t.phase
      <- Original_running
           { captured with
             initializations_rev = boundary :: captured.initializations_rev
           }
  ;;

  let record_batch (t : t) ~role batch =
    Owner_affinity.check_domain_exn t.affinity;
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      t.phase
      <- Original_running
           { captured with
             batches_rev = { Recovery_batch.role; batch } :: captured.batches_rev
           }
  ;;

  let record_transfer (t : t) ~binding ~proof =
    Owner_affinity.check_domain_exn t.affinity;
    match t.kind, t.phase with
    | (Source _ | Construction | Lease), Original_running captured ->
      t.phase
      <- Original_running
           { captured with
             transfers_rev = { Source_transfer.binding; proof } :: captured.transfers_rev
           }
    | Retirement, _ | (Source _ | Construction | Lease), (Complete _ | Retrying _) ->
      invalid_arg "runtime transfer requires its original running source"
  ;;

  module Lease_position = struct
    type t =
      | Source_lexical
      | Retirement of Retirement_cleanup_phase.t

    let matches_role (t : t) (role : lease_role) =
      match t, role with
      | Source_lexical, Source_lexical -> true
      | Retirement Ordinary, Retirement_lease { phase = Ordinary; _ }
      | Retirement Complete_resources, Retirement_lease { phase = Complete_resources; _ }
        -> true
      | (Source_lexical | Retirement _), (Source_lexical | Retirement_lease _) -> false
    ;;
  end

  let register_lease ?relation (t : t) ~position ~finished ~(recovery : t) =
    Owner_affinity.check_domain_exn t.affinity;
    if phys_equal t recovery || not (Owner_affinity.same t.affinity recovery.affinity)
    then invalid_arg "cleanup holder cannot attach a foreign or self lease";
    (match position, relation with
     | Lease_position.Source_lexical, Some _ ->
       invalid_arg "source lexical lease cannot acquire consumer relation evidence"
     | Source_lexical, None | Retirement _, (Some _ | None) -> ());
    (match t.kind, position, recovery.kind with
     | Source _, Lease_position.Source_lexical, Lease | Retirement, Retirement _, Lease ->
       ()
     | ( (Source _ | Lease | Construction | Retirement)
       , (Source_lexical | Retirement _)
       , (Source _ | Lease | Construction | Retirement) ) ->
       invalid_arg "cleanup holder requires its actual lexical lease position");
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      let matching =
        List.find captured.leases_rev ~f:(fun selected ->
          if phys_equal selected.finished finished
          then (
            if not (phys_equal selected.recovery recovery)
            then invalid_arg "cleanup lexical witness was rebound";
            if not (Option.equal phys_equal selected.relation relation)
            then invalid_arg "cleanup lexical relation was rebound";
            Lease_position.matches_role position selected.role)
          else false)
      in
      (match matching with
       | Some selected -> selected
       | None ->
         let role =
           match position with
           | Source_lexical -> Source_lexical
           | Retirement phase ->
             Retirement_lease
               { phase
               ; cancel =
                   Original_cleanup_step.create
                     (Selected_lease_cancel { finished; phase })
               }
         in
         let selected =
           { finished
           ; recovery
           ; role
           ; relation
           ; join_original = None
           ; join_current_control = None
           ; retry_cancel = None
           ; operation = None
           }
         in
         t.phase
         <- Original_running
              { captured with leases_rev = selected :: captured.leases_rev };
         selected)
  ;;

  let record_consumer_join
        (selected : lease)
        ~mode
        (actual : Relation_evidence.Run_result.t)
    =
    let outcome = actual.join in
    (match selected.role, selected.relation with
     | Retirement_lease _, (Some _ | None) -> ()
     | Source_lexical, _ ->
       invalid_arg "consumer join requires its actual retirement enrollment");
    (match outcome with
     | Relation_evidence.Join_outcome.Canonical_consumer _ | Continue_provider -> ()
     | Join_control observation ->
       (match Agent_session.Close_observation.exit observation with
        | Returned (Ok ()) ->
          invalid_arg "positive consumer check cannot establish blocking control"
        | Returned (Error _) | Raised _ -> ()));
    (match actual.cancellation with
     | None -> ()
     | Some cancellation ->
       (match selected.role, selected.retry_cancel with
        | Retirement_lease { cancel; _ }, None when Option.is_none cancel.original ->
          selected.retry_cancel <- Some cancellation
        | Retirement_lease _, Some previous when phys_equal previous cancellation -> ()
        | Source_lexical, _ | Retirement_lease _, _ ->
          invalid_arg "retirement cancellation receipt was rebound or replayed"));
    (match mode with
     | Relation_evidence.Join_mode.Original ->
       if Option.is_none selected.join_original
       then selected.join_original <- Some outcome
     | Recovery_retry | Reobserve -> ());
    match outcome with
    | Relation_evidence.Join_outcome.Join_control observation ->
      selected.join_current_control <- Some (Blocking observation)
    | Continue_provider -> ()
    | Canonical_consumer _ ->
      selected.join_current_control
      <- Option.map selected.join_current_control ~f:(fun control ->
           Relation_evidence.Join_control.Settled
             (Relation_evidence.Join_control.observation control))
  ;;

  let register_construction (t : t) construction =
    Owner_affinity.check_domain_exn t.affinity;
    (match t.kind with
     | Source _ -> ()
     | Lease | Construction | Retirement ->
       invalid_arg "construction cleanup requires its actual source holder");
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      (match
         List.find captured.constructions_rev ~f:(fun receipt ->
           phys_equal (Construction_cleanup.construction receipt) construction)
       with
       | Some receipt -> receipt
       | None ->
         let receipt = Construction_cleanup.create construction in
         t.phase
         <- Original_running
              { captured with constructions_rev = receipt :: captured.constructions_rev };
         receipt)
  ;;

  let register_source_target (t : t) target =
    Owner_affinity.check_domain_exn t.affinity;
    (match t.kind with
     | Source _ -> ()
     | Lease | Construction | Retirement ->
       invalid_arg "source tail requires its original source holder");
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      if
        not
          (List.exists captured.source_tail_rev ~f:(fun duty ->
             Source_cleanup.same_target duty.Source_cleanup.target target))
      then
        t.phase
        <- Original_running
             { captured with
               source_tail_rev =
                 { Source_cleanup.target; original = None; diagnostic = None }
                 :: captured.source_tail_rev
             }
  ;;

  let ordered_source_tail (t : t) =
    Owner_affinity.check_domain_exn t.affinity;
    let captured =
      match t.phase with
      | Original_running captured
      | Complete { captured; _ }
      | Retrying { before = { captured; _ }; _ } -> captured
    in
    List.rev captured.source_tail_rev
    |> List.stable_sort ~compare:Source_cleanup.compare_order
  ;;

  let register_prefix (t : t) ~(prefix : t) =
    Owner_affinity.check_domain_exn t.affinity;
    if phys_equal t prefix || not (Owner_affinity.same t.affinity prefix.affinity)
    then invalid_arg "source cleanup prefix has different original ownership";
    (match t.kind, prefix.kind with
     | Source _, Retirement -> ()
     | ( (Source _ | Lease | Construction | Retirement)
       , (Source _ | Lease | Construction | Retirement) ) ->
       invalid_arg "source cleanup requires an actual ordinary retirement prefix");
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      (match captured.prefix with
       | Some current when not (phys_equal current prefix) ->
         invalid_arg "source cleanup prefix was displaced"
       | Some _ -> ()
       | None -> t.phase <- Original_running { captured with prefix = Some prefix })
  ;;

  let register_actor_cleanup (t : t) cleanup =
    Owner_affinity.check_domain_exn t.affinity;
    (match t.kind with
     | Retirement -> ()
     | Source _ | Lease | Construction ->
       invalid_arg "Actor selection requires its actual retirement holder");
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      (match captured.actor_cleanup with
       | Some current when not (phys_equal current.Actor_cleanup_step.cleanup cleanup) ->
         invalid_arg "retirement cleanup selection was displaced"
       | Some _ -> ()
       | None ->
         t.phase
         <- Original_running
              { captured with actor_cleanup = Some (Actor_cleanup_step.create cleanup) })
  ;;

  (* Selected by the actual close helper, BEFORE the first revoke/close can
     yield or fail. Prefix-created duties belong here, not a later source sweep.
     No future current binding/anchor lookup can replace these exact targets. *)
  let register_native_release (t : t) ~position loan =
    Owner_affinity.check_domain_exn t.affinity;
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      if
        not
          (List.exists captured.native_releases_rev ~f:(fun selected ->
             phys_equal selected.Selected_native_release.loan loan
             && Physical_cleanup_position.same selected.position position))
      then
        t.phase
        <- Original_running
             { captured with
               native_releases_rev =
                 { Selected_native_release.loan; position }
                 :: captured.native_releases_rev
             }
  ;;

  let begin_native_release (t : t) ~position loan =
    register_native_release t ~position loan;
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      let report =
        { Native_release_report.loan
        ; position
        ; attempt = None
        ; selection = None
        ; commit = None
        ; operation = None
        ; original = None
        ; original_registration = true
        ; selection_diagnostic = None
        ; commit_diagnostic = None
        }
      in
      t.phase
      <- Original_running
           { captured with
             native_release_reports_rev = report :: captured.native_release_reports_rev
           };
      report
  ;;

  let register_restored_release (t : t) ~position anchor =
    Owner_affinity.check_domain_exn t.affinity;
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      if
        not
          (List.exists captured.restored_releases_rev ~f:(fun selected ->
             phys_equal selected.Selected_restored_release.anchor anchor
             && Physical_cleanup_position.same selected.position position))
      then
        t.phase
        <- Original_running
             { captured with
               restored_releases_rev =
                 { Selected_restored_release.anchor; position }
                 :: captured.restored_releases_rev
             }
  ;;

  let register_original_step (t : t) target =
    Owner_affinity.check_domain_exn t.affinity;
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      let step = Original_cleanup_step.create target in
      t.phase
      <- Original_running
           { captured with original_steps_rev = step :: captured.original_steps_rev };
      step
  ;;

  let register_policy_claim
        (t : t)
        ~(phase : Retirement_cleanup_phase.t)
        ~capture
        ~loan
        ~runtime
        ~wrapper_finished
        ~(wrapper_recovery : t)
        ~natural_release
    =
    Owner_affinity.check_domain_exn t.affinity;
    if
      phys_equal t wrapper_recovery
      || not (Owner_affinity.same t.affinity wrapper_recovery.affinity)
    then invalid_arg "policy claim wrapper has different original ownership";
    (match t.kind, wrapper_recovery.kind with
     | Retirement, Lease -> ()
     | ( (Source _ | Lease | Construction | Retirement)
       , (Source _ | Lease | Construction | Retirement) ) ->
       invalid_arg "policy claim requires its actual retirement and lexical wrapper");
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      (match
         List.find captured.policy_claims_rev ~f:(fun claim ->
           phys_equal claim.capture capture)
       with
       | Some claim ->
         if
           not
             (phys_equal claim.loan loan
              && phys_equal claim.runtime runtime
              && phys_equal claim.wrapper_finished wrapper_finished
              && phys_equal claim.wrapper_recovery wrapper_recovery
              && phys_equal claim.natural_release natural_release)
         then invalid_arg "policy claim capture was rebound to another physical holder";
         if
           not
             (List.mem claim.selected_phases phase ~equal:Retirement_cleanup_phase.equal)
         then claim.selected_phases <- phase :: claim.selected_phases;
         claim
       | None ->
         let claim =
           { capture
           ; loan
           ; runtime
           ; wrapper_finished
           ; wrapper_recovery
           ; natural_release
           ; selected_phases = [ phase ]
           ; ordinary_attempt = None
           ; complete_attempt = None
           }
         in
         t.phase
         <- Original_running
              { captured with policy_claims_rev = claim :: captured.policy_claims_rev };
         claim)
  ;;

  let begin_restored_release (t : t) ~position anchor =
    register_restored_release t ~position anchor;
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      let report =
        { Restored_release_report.anchor
        ; position
        ; selection_revoke = None
        ; helper_revoke = None
        ; clear = None
        ; operation = None
        ; original = None
        ; original_registration = true
        ; selection_diagnostic = None
        ; revoke_diagnostic = None
        ; operation_origin = None
        ; clear_diagnostic = None
        }
      in
      t.phase
      <- Original_running
           { captured with
             restored_release_reports_rev =
               report :: captured.restored_release_reports_rev
           };
      report
  ;;

  (* Retain only the SAME original source association. The source may own its
     ordinary prefix; it must never point back to this pending retirement. *)
  let register_source (t : t) ~(source : t) =
    Owner_affinity.check_domain_exn t.affinity;
    if phys_equal t source || not (Owner_affinity.same t.affinity source.affinity)
    then invalid_arg "pending cleanup source has different original ownership";
    (match t.kind, source.kind with
     | Retirement, Source _ -> ()
     | ( (Source _ | Lease | Construction | Retirement)
       , (Source _ | Lease | Construction | Retirement) ) ->
       invalid_arg "pending cleanup requires its actual original source holder");
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      (match captured.pending_source with
       | Some actual when not (phys_equal actual source) ->
         invalid_arg "pending cleanup source association was displaced"
       | Some _ -> ()
       | None ->
         t.phase <- Original_running { captured with pending_source = Some source })
  ;;

  let register_preparation_source (t : t) ~(source : t) =
    Owner_affinity.check_domain_exn t.affinity;
    if phys_equal t source || not (Owner_affinity.same t.affinity source.affinity)
    then invalid_arg "preparation source has different original ownership";
    (match t.kind, source.kind with
     | Lease, Source _ -> ()
     | ( (Source _ | Lease | Construction | Retirement)
       , (Source _ | Lease | Construction | Retirement) ) ->
       invalid_arg "preparation source requires its original outer lexical scope");
    match t.phase with
    | Complete _ | Retrying _ -> invalid_arg "cleanup holder registration is complete"
    | Original_running captured ->
      if not (List.mem captured.preparation_sources_rev source ~equal:phys_equal)
      then
        t.phase
        <- Original_running
             { captured with
               preparation_sources_rev = source :: captured.preparation_sources_rev
             }
  ;;

  let original_preparation_projection (t : t) =
    Owner_affinity.check_domain_exn t.affinity;
    let captured =
      match t.phase with
      | Original_running captured
      | Complete { captured; _ }
      | Retrying { before = { captured; _ }; _ } -> captured
    in
    List.fold
      (List.rev captured.preparation_sources_rev)
      ~init:Cleanup_projection.empty
      ~f:(fun cleanup source ->
        Owner_affinity.check_domain_exn source.affinity;
        match source.phase with
        | Original_running _ ->
          (* No completed diagnostic report exists. The exact dependency remains
             unsettled for canonical recovery; never manufacture Joined here. *)
          cleanup
        | Complete { original_projection; _ }
        | Retrying { before = { original_projection; _ }; _ } ->
          Cleanup_projection.combine cleanup original_projection)
  ;;

  let initialize_current (owner : t) (captured : captured) =
    let receipts =
      List.concat_map (List.rev captured.batches_rev) ~f:(fun selected ->
        selected.Recovery_batch.batch.receipts)
    in
    let retired =
      List.fold receipts ~init:[] ~f:(fun entries receipt ->
        let resource = receipt.Retired_resource.Receipt.resource in
        match
          List.find entries ~f:(fun (entry : retired_current) ->
            phys_equal entry.resource resource)
        with
        | Some _ -> entries
        | None ->
          let origins =
            List.filter receipts ~f:(fun receipt ->
              phys_equal receipt.Retired_resource.Receipt.resource resource)
          in
          { resource; origins; retired_latest = None } :: entries)
      |> List.rev
    in
    let native_targets =
      List.map (List.rev captured.native_releases_rev) ~f:(fun selected ->
        selected.Selected_native_release.loan)
      @ List.filter_map (List.rev captured.source_tail_rev) ~f:(fun duty ->
        match duty.Source_cleanup.target with
        | Native_release loan -> Some loan
        | Policy_end _ | Native_end _ | Restored_release _ | Delegated_end _ -> None)
    in
    let native =
      List.fold native_targets ~init:[] ~f:(fun entries loan ->
        if
          List.exists entries ~f:(fun (entry : native_current) ->
            phys_equal entry.loan loan)
        then entries
        else { loan; native_latest = None } :: entries)
      |> List.rev
    in
    let restored_targets =
      List.map (List.rev captured.restored_releases_rev) ~f:(fun selected ->
        selected.Selected_restored_release.anchor)
      @ List.filter_map (List.rev captured.source_tail_rev) ~f:(fun duty ->
        match duty.Source_cleanup.target with
        | Restored_release anchor -> Some anchor
        | Policy_end _ | Native_end _ | Native_release _ | Delegated_end _ -> None)
    in
    let restored =
      List.fold restored_targets ~init:[] ~f:(fun entries anchor ->
        if
          List.exists entries ~f:(fun (entry : restored_current) ->
            phys_equal entry.anchor anchor)
        then entries
        else { anchor; restored_latest = None } :: entries)
      |> List.rev
    in
    let source_ends =
      List.filter_map (List.rev captured.source_tail_rev) ~f:(fun duty ->
        match duty.Source_cleanup.target with
        | Policy_end _ | Native_end _ | Delegated_end _ ->
          Some { duty; end_latest = None; end_diagnostic = None }
        | Native_release _ | Restored_release _ -> None)
    in
    { owner
    ; startup_settlement = None
    ; retired
    ; native
    ; restored
    ; source_ends
    ; constructions =
        List.map (List.rev captured.constructions_rev) ~f:(fun receipt ->
          { receipt; construction_end = None; construction_end_diagnostic = None })
    ; actor =
        Option.map captured.actor_cleanup ~f:(fun step ->
          { step
          ; initial = None
          ; foreground = None
          ; foreground_diagnostic = None
          ; final = None
          })
    ; claims =
        List.map (List.rev captured.policy_claims_rev) ~f:(fun claim ->
          { claim
          ; claim_pin = None
          ; claim_pin_origin = None
          ; claim_clear = None
          ; claim_clear_diagnostic = None
          })
    }
  ;;

  let current_of_complete = function
    | Complete completed -> Some completed.current
    | Retrying reserved -> Some reserved.before.current
    | Original_running _ -> None
  ;;

  let captured_of_phase = function
    | Original_running captured
    | Complete { captured; _ }
    | Retrying { before = { captured; _ }; _ } -> captured
  ;;

  let begin_current_native (current : native_current) ~after ~owner ~position =
    let report =
      { Native_release_report.loan = current.loan
      ; position
      ; attempt = None
      ; selection = None
      ; commit = None
      ; operation = None
      ; original = None
      ; original_registration = false
      ; selection_diagnostic = None
      ; commit_diagnostic = None
      }
    in
    current.native_latest <- Some { after; owner; report };
    report
  ;;

  let begin_current_restored (current : restored_current) ~after ~owner ~position =
    let report =
      { Restored_release_report.anchor = current.anchor
      ; position
      ; selection_revoke = None
      ; helper_revoke = None
      ; clear = None
      ; operation = None
      ; original = None
      ; original_registration = false
      ; selection_diagnostic = None
      ; revoke_diagnostic = None
      ; operation_origin = None
      ; clear_diagnostic = None
      }
    in
    current.restored_latest <- Some { after; owner; report };
    report
  ;;

  module Observation = struct
    module E = Cleanup_evidence

    module Prerequisite = struct
      type t =
        | Ready
        | Live
        | Repairable
        | Unfulfillable

      let of_evidence evidence =
        if
          List.for_all evidence ~f:(fun (duty : E.t) ->
            Cleanup_snapshot.equal_progress duty.progress Joined)
        then Ready
        else if
          List.exists evidence ~f:(fun (duty : E.t) ->
            Cleanup_snapshot.equal_progress duty.progress Blocked)
        then Live
        else if
          List.exists evidence ~f:(fun (duty : E.t) ->
            Cleanup_snapshot.equal_progress duty.progress Retryable)
        then Repairable
        else Unfulfillable
      ;;
    end

    module Step_position = struct
      type t =
        | Dependency_stop
        | Policy_ordinary
        | Policy_complete
        | Lease_retire
        | Lease_detach
        | Lease_check
        | Lease_escape
        | Source_retire
        | Source_physical_selection
        | Source_prefix_selection
        | Source_installed_selection
        | Source_clear
        | Source_exclusion_complete
        | Administration_unload_admission
        | Administration_unload_prefix
        | Administration_unload_transition
        | Administration_retire
        | Administration_clear
        | Administration_registration
        | Construction_retention
        | Construction_residency
        | Local_release
        | Warm_release
        | Actor_selection
        | Complete_constructor
        | Complete_callback
        | Complete_descendant
        | Complete_inference
        | Retirement_detach
        | Selected_cancel
        | Complete_selection
        | Complete_check
        | Coverage_check
        | Released_check
      [@@deriving equal]

      let of_target = function
        | Original_cleanup_step.Dependency_stop _ -> Dependency_stop
        | Policy_resource Ordinary -> Policy_ordinary
        | Policy_resource Complete_resources -> Policy_complete
        | Lease_retire -> Lease_retire
        | Lease_detach -> Lease_detach
        | Lease_completion_check -> Lease_check
        | Lease_cleanup_escape -> Lease_escape
        | Source_retire -> Source_retire
        | Source_physical_selection -> Source_physical_selection
        | Source_prefix_selection -> Source_prefix_selection
        | Source_installed_selection -> Source_installed_selection
        | Source_clear -> Source_clear
        | Source_exclusion_complete -> Source_exclusion_complete
        | Administration_unload_selection Source_admission ->
          Administration_unload_admission
        | Administration_unload_selection Source_prefix -> Administration_unload_prefix
        | Administration_unload_selection Transition -> Administration_unload_transition
        | Administration_retire_selection -> Administration_retire
        | Administration_clear -> Administration_clear
        | Administration_batch_registration -> Administration_registration
        | Construction_retention_check _ -> Construction_retention
        | Construction_residency_check _ -> Construction_residency
        | Local_release_selection _ -> Local_release
        | Warm_release_selection -> Warm_release
        | Actor_retirement_selection -> Actor_selection
        | Complete_constructor_drain -> Complete_constructor
        | Complete_callback_drain -> Complete_callback
        | Complete_descendant_drain -> Complete_descendant
        | Complete_inference_drain -> Complete_inference
        | Retirement_detach_selection -> Retirement_detach
        | Selected_lease_cancel _ -> Selected_cancel
        | Complete_resource_selection -> Complete_selection
        | Complete_resource_check -> Complete_check
        | Retirement_coverage_check -> Coverage_check
        | Released_resource_check _ -> Released_check
      ;;
    end

    let deferred prerequisite ~owned =
      match prerequisite with
      | Prerequisite.Ready -> E.make ~progress:(if owned then Retryable else Terminal) []
      | Live -> E.make ~progress:Blocked []
      | Repairable | Unfulfillable -> E.make ~progress:Terminal []
    ;;

    let original ?diagnostic ?origin ~prerequisite primary =
      let observed =
        match primary with
        | None -> deferred prerequisite ~owned:false
        | Some primary ->
          (match E.primary_failure primary with
           | None -> E.succeeded
           | Some failure ->
             E.make_origin_failures
               ~progress:Terminal
               (E.Origin_failures.create
                  origin
                  ~copy:false
                  ~fallback:[ E.original ?diagnostic failure ]))
      in
      match origin with
      | Some _ -> E.own_origin observed origin
      | None -> E.own observed diagnostic
    ;;

    let current_original ?diagnostic ~prerequisite primary =
      E.with_original (original ?diagnostic ~prerequisite primary) []
    ;;

    let steps captured position =
      List.filter (List.rev captured.original_steps_rev) ~f:(fun step ->
        Step_position.equal
          (Step_position.of_target step.Original_cleanup_step.target)
          position)
    ;;

    let observe_steps captured position ~prerequisite =
      List.map (steps captured position) ~f:(fun step ->
        original
          ?diagnostic:step.Original_cleanup_step.diagnostic
          ?origin:step.forwarded_origin
          ~prerequisite
          step.original)
    ;;

    let required_step captured position ~prerequisite =
      match steps captured position with
      | [] -> [ deferred prerequisite ~owned:false ]
      | selected ->
        List.map selected ~f:(fun step ->
          original
            ?diagnostic:step.Original_cleanup_step.diagnostic
            ?origin:step.forwarded_origin
            ~prerequisite
            step.original)
    ;;

    let construction_steps captured receipt position ~prerequisite =
      List.filter_map (steps captured position) ~f:(fun step ->
        let same =
          match step.Original_cleanup_step.target with
          | Construction_retention_check actual
          | Construction_residency_check actual
          | Local_release_selection actual -> phys_equal actual receipt
          | Dependency_stop _
          | Policy_resource _
          | Lease_retire
          | Lease_detach
          | Lease_completion_check
          | Lease_cleanup_escape
          | Source_retire
          | Source_physical_selection
          | Source_prefix_selection
          | Source_installed_selection
          | Source_clear
          | Source_exclusion_complete
          | Administration_unload_selection _
          | Administration_retire_selection
          | Administration_clear
          | Administration_batch_registration
          | Warm_release_selection
          | Actor_retirement_selection
          | Complete_constructor_drain
          | Complete_callback_drain
          | Complete_descendant_drain
          | Complete_inference_drain
          | Retirement_detach_selection
          | Selected_lease_cancel _
          | Complete_resource_selection
          | Complete_resource_check
          | Retirement_coverage_check
          | Released_resource_check _ -> false
        in
        if same
        then
          Some
            (original
               ?diagnostic:step.Original_cleanup_step.diagnostic
               ?origin:step.forwarded_origin
               ~prerequisite
               step.original)
        else None)
    ;;

    let raised (exn, backtrace) = Scope_outcome.Cleanup.Failure.Raised (exn, backtrace)

    let current_retired captured resource =
      Option.bind captured.observing ~f:(fun current ->
        List.find current.retired ~f:(fun entry -> phys_equal entry.resource resource))
    ;;

    let current_native captured loan =
      Option.bind captured.observing ~f:(fun current ->
        List.find current.native ~f:(fun entry -> phys_equal entry.loan loan))
    ;;

    let current_restored captured anchor =
      Option.bind captured.observing ~f:(fun current ->
        List.find current.restored ~f:(fun entry -> phys_equal entry.anchor anchor))
    ;;

    let owns_current captured owner =
      Option.exists captured.observing ~f:(fun current -> phys_equal current.owner owner)
    ;;

    let effective_receipt captured (receipt : Retired_resource.Receipt.t) =
      match
        Option.bind (current_retired captured receipt.resource) ~f:(fun entry ->
          entry.retired_latest)
      with
      | None -> receipt
      | Some latest ->
        (match latest.batch.receipts with
         | [ current ]
           when phys_equal current.Retired_resource.Receipt.resource receipt.resource ->
           current
         | [] | [ _ ] | _ :: _ :: _ ->
           invalid_arg "retired retry receipt changed its exact entry")
    ;;

    let current_batch_control captured (batch : Cleanup_batch.t) ~prerequisite =
      List.concat_map batch.receipts ~f:(fun original_receipt ->
        match
          current_retired captured original_receipt.Retired_resource.Receipt.resource
        with
        | None -> []
        | Some entry ->
          (match entry.origins, entry.retired_latest with
           | first :: _, Some latest
             when phys_equal first original_receipt && owns_current captured latest.owner
             ->
             (match latest.batch.original with
              | Returned _ -> []
              | Raised (exn, backtrace) ->
                [ current_original
                    ?diagnostic:latest.batch.diagnostic
                    ~prerequisite
                    (Some (Raised (exn, backtrace)))
                ])
           | [], _ | _ :: _, None | _ :: _, Some _ -> []))
    ;;

    let batch captured (batch : Cleanup_batch.t) ~prerequisite =
      let physical =
        List.concat_map batch.receipts ~f:(fun receipt ->
          let original_receipt = receipt in
          let receipt = effective_receipt captured receipt in
          let resource = receipt.Retired_resource.Receipt.resource in
          let observed =
            match resource.Retired_resource.phase with
            | Closed -> E.succeeded
            | Retained _ | Closing _ ->
              (match receipt.disposition with
               | Already_closed | Transferred _ -> E.succeeded
               | Unattempted ->
                 (match resource.phase with
                  | Closing producer
                    when Option.is_none (Eio.Promise.peek producer.finished) ->
                    deferred Prerequisite.Live ~owned:true
                  | Retained _ | Closing _ | Closed -> deferred prerequisite ~owned:true)
               | Blocked { previous_failure; producer; custody } ->
                 let prior =
                   Option.value_map previous_failure ~default:[] ~f:(fun prior ->
                     E.reported_failures prior.Retired_failure_receipt.origin ~copy:true)
                 in
                 let producer_failures =
                   Option.value_map
                     producer
                     ~default:E.Origin_failures.empty
                     ~f:(fun attempt ->
                       match
                         Eio.Promise.peek attempt.Retired_resource.Attempt.finished
                       with
                       | None -> E.Origin_failures.empty
                       | Some (Ok ()) ->
                         E.Origin_failures.create attempt.origin ~copy:false ~fallback:[]
                       | Some (Error failure) ->
                         E.Origin_failures.create
                           attempt.origin
                           ~copy:false
                           ~fallback:
                             [ { E.Failure.source = Retired_actual attempt.finished
                               ; failure = raised failure
                               ; diagnostic = None
                               }
                             ])
                 in
                 let live =
                   Option.exists producer ~f:(fun producer ->
                     Option.is_none
                       (Eio.Promise.peek producer.Retired_resource.Attempt.finished))
                   || List.exists custody.borrowers ~f:(fun borrower ->
                     Option.is_none
                       (Eio.Promise.peek
                          borrower.Retired_resource.Receipt.Borrower.finished))
                   ||
                   match resource.phase with
                   | Closing current -> Option.is_none (Eio.Promise.peek current.finished)
                   | Retained _ | Closed -> false
                 in
                 E.make_origin_failures
                   ~progress:(if live then Blocked else Retryable)
                   (E.Origin_failures.prepend_inventory producer_failures prior)
               | Attempted { attempt; outcome } ->
                 let failures =
                   match outcome with
                   | Ok () ->
                     E.Origin_failures.create attempt.origin ~copy:false ~fallback:[]
                   | Error failure ->
                     E.Origin_failures.create
                       attempt.origin
                       ~copy:false
                       ~fallback:
                         [ { E.Failure.source = Retired_actual attempt.finished
                           ; failure = raised failure
                           ; diagnostic = None
                           }
                         ]
                 in
                 let progress : Cleanup_snapshot.progress =
                   match resource.phase, outcome with
                   | Closing current, _
                     when Option.is_none (Eio.Promise.peek current.finished)
                          && not (phys_equal current attempt) -> Blocked
                   | (Retained _ | Closing _), Error _ ->
                     if Option.is_some (Eio.Promise.peek attempt.finished)
                     then Retryable
                     else Terminal
                   | (Retained _ | Closing _), Ok () -> Terminal
                   | Closed, _ -> Joined
                 in
                 E.make_origin_failures ~progress failures)
          in
          let terminal_current =
            Option.exists
              (Option.bind (current_retired captured resource) ~f:(fun current ->
                 current.retired_latest))
              ~f:(fun current ->
                match current.batch.original with
                | Raised _ -> true
                | Returned _ -> false)
          in
          let observed =
            if
              terminal_current
              && not (Cleanup_snapshot.equal_progress observed.progress Joined)
            then { observed with progress = Terminal; custody_joined = false }
            else observed
          in
          let pieces origin ~copy =
            Option.value_map origin ~default:[] ~f:(fun origin ->
              List.map origin.Physical_origin.diagnostics ~f:(fun actual ->
                if copy
                then Original_cleanup_report.Piece.Dependency_copy actual
                else Original_cleanup_report.Piece.Own actual))
          in
          let own origin = pieces origin ~copy:false in
          let pieces =
            match original_receipt.Retired_resource.Receipt.disposition with
            | Unattempted | Already_closed -> []
            | Transferred { producer; _ } ->
              Option.value_map producer ~default:[] ~f:(fun attempt -> own attempt.origin)
            | Blocked { previous_failure; producer; _ } ->
              Option.value_map previous_failure ~default:[] ~f:(fun prior ->
                pieces (Some prior.Retired_failure_receipt.origin) ~copy:true)
              @ Option.value_map producer ~default:[] ~f:(fun attempt ->
                own attempt.origin)
            | Attempted { attempt; _ } -> own attempt.origin
          in
          let product =
            match original_receipt.Retired_resource.Receipt.disposition with
            | Attempted { attempt; _ } ->
              Option.value_map
                attempt.product_selection
                ~default:[]
                ~f:Callback_evidence.product
            | Blocked { producer = Some attempt; _ } ->
              Option.value_map
                attempt.product_selection
                ~default:[]
                ~f:Callback_evidence.product
            | Already_closed ->
              Option.value_map
                original_receipt.resource.product_selection
                ~default:[]
                ~f:Callback_evidence.product
            | Unattempted | Transferred _ | Blocked { producer = None; _ } -> []
          in
          let diagnostics =
            let from_origin = function
              | None -> []
              | Some origin -> origin.Physical_origin.diagnostics
            in
            match receipt.disposition with
            | Unattempted | Already_closed -> []
            | Transferred { producer; _ } ->
              Option.value_map producer ~default:[] ~f:(fun attempt ->
                from_origin attempt.origin)
            | Blocked { previous_failure; producer; _ } ->
              Option.value_map previous_failure ~default:[] ~f:(fun prior ->
                prior.Retired_failure_receipt.origin.diagnostics)
              @ Option.value_map producer ~default:[] ~f:(fun attempt ->
                from_origin attempt.origin)
            | Attempted { attempt; _ } -> from_origin attempt.origin
          in
          product @ [ E.with_original (E.with_diagnostics observed diagnostics) pieces ])
      in
      let control = current_batch_control captured batch ~prerequisite in
      match batch.original with
      | Returned _ -> physical @ control
      | Raised (exn, backtrace) ->
        physical
        @ [ E.own
              (E.make
                 ~progress:Terminal
                 [ E.original ?diagnostic:batch.diagnostic (Raised (exn, backtrace)) ])
              batch.diagnostic
          ]
        @ control
    ;;

    let batches captured role_matches ~prerequisite =
      List.concat_map (List.rev captured.batches_rev) ~f:(fun selected ->
        if role_matches selected.Recovery_batch.role
        then batch captured selected.batch ~prerequisite
        else [])
    ;;

    let operation_failure outcome =
      match outcome with
      | Ok (Ok ()) -> None
      | Ok (Error error) -> Some (Scope_outcome.Cleanup.Failure.Protocol error)
      | Error failure -> Some (raised failure)
    ;;

    module Native_evidence = struct
      type t =
        { selection : E.t
        ; physical : E.t
        ; commit : E.t
        }

      let ordered t = [ t.selection; t.physical; t.commit ]
    end

    let native_original_report (report : Native_release_report.t) ~prerequisite =
      let selection =
        match report.selection with
        | None -> deferred prerequisite ~owned:false
        | Some (Returned (Ok (Missing error))) ->
          E.make
            ~progress:Terminal
            [ E.original ?diagnostic:report.selection_diagnostic (Protocol error) ]
        | Some (Returned (Ok (Released | Join _ | Close _))) -> E.succeeded
        | Some (Returned (Error error)) ->
          E.make
            ~progress:Terminal
            [ E.original ?diagnostic:report.selection_diagnostic (Protocol error) ]
        | Some (Raised (exn, backtrace)) ->
          E.make
            ~progress:Terminal
            [ E.original ?diagnostic:report.selection_diagnostic (Raised (exn, backtrace))
            ]
      in
      let physical =
        match report.loan.Native_loan.phase with
        | Released -> E.succeeded
        | Available | Retiring | Closing _ ->
          (match report.operation with
           | None ->
             (match report.loan.phase with
              | Closing current when Option.is_none (Eio.Promise.peek current.finished) ->
                E.make ~progress:Blocked []
              | Available | Retiring | Closing _ | Released ->
                deferred (Prerequisite.of_evidence [ selection ]) ~owned:true)
           | Some outcome ->
             let failures =
               Option.to_list
                 (Option.map (operation_failure outcome) ~f:(fun failure ->
                    match report.attempt with
                    | Some attempt ->
                      { E.Failure.source = Native_actual attempt.finished
                      ; failure
                      ; diagnostic = None
                      }
                    | None -> invalid_arg "native operation lost its actual attempt"))
             in
             let failures =
               match report.attempt with
               | Some attempt ->
                 E.Origin_failures.create attempt.origin ~copy:false ~fallback:failures
               | None -> E.Origin_failures.create None ~copy:false ~fallback:failures
             in
             let progress : Cleanup_snapshot.progress =
               match report.loan.phase, report.attempt with
               | Closing current, Some observed
                 when (not (phys_equal current observed))
                      && Option.is_none (Eio.Promise.peek current.finished) -> Blocked
               | (Available | Retiring), Some _ -> Retryable
               | Closing _, Some _ ->
                 if Option.is_none report.commit && Option.is_none report.original
                 then Blocked
                 else Terminal
               | Released, Some _ -> Joined
               | (Available | Retiring | Closing _ | Released), None ->
                 invalid_arg "native operation lost its actual attempt"
             in
             E.make_origin_failures ~progress failures)
      in
      let commit =
        match report.selection with
        | Some (Returned (Ok (Close _))) ->
          original
            ?diagnostic:report.commit_diagnostic
            ~prerequisite:(Prerequisite.of_evidence [ physical ])
            report.commit
        | None
        | Some (Returned (Ok (Released | Missing _ | Join _)))
        | Some (Returned (Error _) | Raised _) -> E.succeeded
      in
      let terminal_control =
        Option.exists report.selection ~f:(fun primary ->
          Option.is_some (E.primary_failure primary))
        || Option.exists report.commit ~f:(fun primary ->
          Option.is_some (E.primary_failure primary))
        ||
        match report.selection with
        | Some (Returned (Ok (Missing _))) -> true
        | None
        | Some
            (Returned (Ok (Released | Join _ | Close _)) | Returned (Error _) | Raised _)
          -> false
      in
      let physical =
        if terminal_control && Cleanup_snapshot.equal_progress physical.progress Retryable
        then { physical with progress = Terminal; custody_joined = false }
        else physical
      in
      let physical =
        E.with_diagnostics
          physical
          (Option.value_map report.attempt ~default:[] ~f:(fun attempt ->
             Option.value_map
               attempt.Native_loan.Attempt.origin
               ~default:[]
               ~f:(fun origin -> origin.Physical_origin.diagnostics)))
      in
      let control evidence diagnostic =
        if report.original_registration
        then E.own evidence diagnostic
        else E.with_original (E.with_diagnostics evidence (Option.to_list diagnostic)) []
      in
      { Native_evidence.selection = control selection report.selection_diagnostic
      ; physical =
          E.own_origin
            physical
            (if report.original_registration
             then
               Option.bind report.attempt ~f:(fun attempt ->
                 attempt.Native_loan.Attempt.origin)
             else None)
      ; commit = control commit report.commit_diagnostic
      }
    ;;

    let current_native_controls captured (latest : reported_native) report position =
      owns_current captured latest.owner
      && Physical_cleanup_position.same latest.report.position position
      &&
      match
        List.find (List.rev captured.native_release_reports_rev) ~f:(fun original ->
          phys_equal original.Native_release_report.loan report.Native_release_report.loan
          && Physical_cleanup_position.same original.position position)
      with
      | Some first -> phys_equal first report
      | None -> phys_equal latest.report report
    ;;

    let native_report captured ?position report ~prerequisite =
      let position =
        Option.value position ~default:report.Native_release_report.position
      in
      let original = native_original_report report ~prerequisite in
      match
        Option.bind (current_native captured report.loan) ~f:(fun entry ->
          entry.native_latest)
      with
      | None -> Native_evidence.ordered original
      | Some latest ->
        let current = native_original_report latest.report ~prerequisite in
        let control_failed =
          Option.exists latest.report.selection ~f:(fun primary ->
            Option.is_some (E.primary_failure primary))
          || Option.exists latest.report.commit ~f:(fun primary ->
            Option.is_some (E.primary_failure primary))
          ||
          match latest.report.selection with
          | Some (Returned (Ok (Missing _))) -> true
          | None
          | Some
              (Returned (Ok (Released | Join _ | Close _)) | Returned (Error _) | Raised _)
            -> false
        in
        let current =
          if
            control_failed
            && not (Cleanup_snapshot.equal_progress current.physical.progress Joined)
          then
            { current with
              physical =
                { current.physical with progress = Terminal; custody_joined = false }
            }
          else current
        in
        let original_view =
          if phys_equal report latest.report
          then [ current.physical ]
          else
            [ original.selection
            ; E.with_original current.physical original.physical.original_pieces
            ; original.commit
            ]
        in
        let controls =
          if current_native_controls captured latest report position
          then [ current.selection; current.commit ]
          else []
        in
        original_view @ controls
    ;;

    let restored_released captured anchor =
      Option.exists
        (Option.bind (current_restored captured anchor) ~f:(fun entry ->
           entry.restored_latest))
        ~f:(fun latest ->
          match latest.report.operation with
          | Some (Returned (Ok ())) -> true
          | None | Some (Returned (Error _) | Raised _) -> false)
      || List.exists captured.restored_release_reports_rev ~f:(fun report ->
        phys_equal report.Restored_release_report.anchor anchor
        &&
        match report.operation with
        | Some (Returned (Ok ())) -> true
        | None | Some (Returned (Error _) | Raised _) -> false)
    ;;

    let restored_report
          captured
          ?position
          (report : Restored_release_report.t)
          ~prerequisite
      =
      let position = Option.value position ~default:report.position in
      let latest =
        Option.bind (current_restored captured report.anchor) ~f:(fun entry ->
          entry.restored_latest)
      in
      let effective =
        Option.value_map latest ~default:report ~f:(fun latest -> latest.report)
      in
      let control (report : Restored_release_report.t) ?diagnostic ~prerequisite primary =
        if report.original_registration
        then original ?diagnostic ~prerequisite primary
        else current_original ?diagnostic ~prerequisite primary
      in
      let selection report =
        Option.to_list
          (Option.map report.Restored_release_report.selection_revoke ~f:(fun primary ->
             control
               report
               ?diagnostic:report.selection_diagnostic
               ~prerequisite
               (Some primary)))
      in
      let controls report =
        selection report
        @ [ control
              report
              ?diagnostic:report.revoke_diagnostic
              ~prerequisite
              report.Restored_release_report.helper_revoke
          ]
      in
      let original_controls =
        match latest with
        | Some latest when phys_equal latest.report report -> []
        | None | Some _ -> controls report
      in
      let physical =
        if restored_released captured report.anchor
        then E.succeeded
        else (
          match effective.operation with
          | None -> deferred (Prerequisite.of_evidence (controls effective)) ~owned:true
          | Some primary ->
            E.make_origin_failures
              ~progress:Retryable
              (E.Origin_failures.create
                 effective.operation_origin
                 ~copy:false
                 ~fallback:
                   (List.map
                      (Option.to_list (E.primary_failure primary))
                      ~f:(fun failure -> E.original failure))))
      in
      let physical =
        E.own_origin
          (E.with_diagnostics
             physical
             (Option.value_map effective.operation_origin ~default:[] ~f:(fun origin ->
                origin.Physical_origin.diagnostics)))
          (if report.original_registration then report.operation_origin else None)
      in
      let old_clear =
        match latest, report.clear with
        | Some latest, Some primary when not (phys_equal latest.report report) ->
          [ control
              report
              ?diagnostic:report.clear_diagnostic
              ~prerequisite
              (Some primary)
          ]
        | None, _ | Some _, (Some _ | None) -> []
      in
      let include_latest =
        Option.exists latest ~f:(fun latest ->
          owns_current captured latest.owner
          && Physical_cleanup_position.same latest.report.position position
          &&
          match
            List.find (List.rev captured.restored_release_reports_rev) ~f:(fun original ->
              phys_equal original.Restored_release_report.anchor report.anchor
              && Physical_cleanup_position.same original.position position)
          with
          | Some first -> phys_equal first report
          | None -> phys_equal latest.report report)
      in
      let current_controls = if include_latest then controls effective else [] in
      let clear =
        if Option.is_some latest && not include_latest
        then []
        else
          [ control
              effective
              ?diagnostic:effective.clear_diagnostic
              ~prerequisite:(Prerequisite.of_evidence [ physical ])
              effective.clear
          ]
      in
      original_controls @ [ physical ] @ old_clear @ current_controls @ clear
    ;;

    let physical captured position ~prerequisite =
      let restored =
        List.concat_map (List.rev captured.restored_releases_rev) ~f:(fun selected ->
          if
            not
              (Physical_cleanup_position.same
                 selected.Selected_restored_release.position
                 position)
          then []
          else (
            let reports =
              List.filter
                (List.rev captured.restored_release_reports_rev)
                ~f:(fun report ->
                  phys_equal report.Restored_release_report.anchor selected.anchor
                  && Physical_cleanup_position.same report.position position)
            in
            match reports with
            | [] ->
              (match
                 Option.bind (current_restored captured selected.anchor) ~f:(fun entry ->
                   entry.restored_latest)
               with
               | Some latest ->
                 restored_report captured ~position latest.report ~prerequisite
               | None ->
                 [ (if restored_released captured selected.anchor
                    then E.succeeded
                    else deferred prerequisite ~owned:true)
                 ])
            | reports ->
              List.concat_map reports ~f:(fun report ->
                restored_report captured report ~prerequisite)))
      in
      let native =
        List.concat_map (List.rev captured.native_releases_rev) ~f:(fun selected ->
          if
            not
              (Physical_cleanup_position.same
                 selected.Selected_native_release.position
                 position)
          then []
          else (
            let reports =
              List.filter (List.rev captured.native_release_reports_rev) ~f:(fun report ->
                phys_equal report.Native_release_report.loan selected.loan
                && Physical_cleanup_position.same report.position position)
            in
            match reports with
            | [] ->
              (match
                 Option.bind (current_native captured selected.loan) ~f:(fun entry ->
                   entry.native_latest)
               with
               | Some latest ->
                 native_report captured ~position latest.report ~prerequisite
               | None ->
                 [ (match selected.loan.Native_loan.phase with
                    | Released -> E.succeeded
                    | Closing attempt
                      when Option.is_none (Eio.Promise.peek attempt.finished) ->
                      E.make ~progress:Blocked []
                    | Available | Retiring | Closing _ ->
                      deferred prerequisite ~owned:true)
                 ])
            | reports ->
              List.concat_map reports ~f:(fun report ->
                native_report captured report ~prerequisite)))
      in
      restored @ native
    ;;

    let actor captured (step : Actor_cleanup_step.t) ~prerequisite =
      let original_step = step in
      let current = Option.bind captured.observing ~f:(fun current -> current.actor) in
      let step =
        match current with
        | None -> step
        | Some current ->
          { step with
            initial =
              Option.value_map current.initial ~default:step.initial ~f:(fun report ->
                Some report.result)
          ; initial_diagnostic =
              Option.value_map
                current.initial
                ~default:step.initial_diagnostic
                ~f:(fun report -> report.diagnostic)
          ; foreground =
              (match current.foreground with
               | None -> step.foreground
               | Some _ as result -> result)
          ; foreground_diagnostic =
              (match current.foreground with
               | None -> step.foreground_diagnostic
               | Some _ -> current.foreground_diagnostic)
          ; final =
              Option.value_map current.final ~default:step.final ~f:(fun report ->
                Some report.result)
          ; final_diagnostic =
              Option.value_map
                current.final
                ~default:step.final_diagnostic
                ~f:(fun report -> report.diagnostic)
          }
      in
      let module C = Agent_session.Session_actor.Retirement_cleanup in
      let selected =
        match C.snapshot step.Actor_cleanup_step.cleanup with
        | Error _ -> invalid_arg "Actor cleanup observation has foreign ownership"
        | Ok snapshot ->
          (* The Actor returns the SAME immutable snapshot that it publishes.
             Only a named actual pass returning THIS object can supply its
             aggregate boundary carrier; a fresh unmatched read stays unknown. *)
          let diagnostic =
            List.find_map
              [ step.final, step.final_diagnostic; step.initial, step.initial_diagnostic ]
              ~f:(function
                | Some (Scope_outcome.Returned (Ok actual)), diagnostic
                  when phys_equal actual snapshot -> diagnostic
                | (None | Some (Returned _ | Raised _)), _ -> None)
          in
          let failures =
            List.map (C.Snapshot.failures snapshot) ~f:(function
              | C.Failure.Protocol error -> E.original ?diagnostic (Protocol error)
              | Raised (exn, backtrace) ->
                E.original ?diagnostic (Raised (exn, backtrace)))
          in
          let progress : Cleanup_snapshot.progress =
            match C.Snapshot.progress snapshot with
            | Joined -> Cleanup_snapshot.Joined
            | Pending -> Blocked
            | Retryable -> Retryable
            | Terminal -> Terminal
          in
          E.make ~progress failures
      in
      let call ?diagnostic = function
        | None -> []
        | Some (Scope_outcome.Returned (Ok _)) -> []
        | Some (Returned (Error error)) ->
          [ E.make ~progress:Terminal [ E.original ?diagnostic (Protocol error) ] ]
        | Some (Raised (exn, backtrace)) ->
          [ E.make ~progress:Terminal [ E.original ?diagnostic (Raised (exn, backtrace)) ]
          ]
      in
      let selected =
        match step.initial with
        | Some (Scope_outcome.Returned (Ok _)) -> selected
        | None | Some (Returned (Error _) | Raised _) ->
          { selected with
            progress = (deferred prerequisite ~owned:true).progress
          ; custody_joined = false
          }
      in
      let initial = call ?diagnostic:step.initial_diagnostic step.initial in
      let foreground =
        match step.foreground with
        | Some primary ->
          [ current_original
              ?diagnostic:step.foreground_diagnostic
              ~prerequisite
              (Some primary)
          ]
        | None ->
          let prerequisite = Prerequisite.of_evidence (selected :: initial) in
          [ (match prerequisite with
             | Ready ->
               if
                 Agent_session.Session_actor.Runtime_retirement.is_finished
                   (C.retirement step.cleanup)
               then E.make ~progress:Retryable []
               else E.make ~progress:Blocked []
             | Live | Repairable | Unfulfillable -> deferred prerequisite ~owned:true)
          ]
      in
      let final = call ?diagnostic:step.final_diagnostic step.final in
      let required_final =
        match step.final with
        | Some (Returned (Ok _)) -> E.succeeded
        | Some (Returned (Error _) | Raised _) -> E.make ~progress:Terminal []
        | None ->
          deferred
            (Prerequisite.of_evidence ((selected :: initial) @ foreground))
            ~owned:true
      in
      let replaced_control before after diagnostic =
        if Option.is_some after then call ?diagnostic before else []
      in
      let fixed =
        match current with
        | None -> []
        | Some current ->
          replaced_control
            original_step.initial
            current.initial
            original_step.initial_diagnostic
          @ (if Option.is_some current.foreground
             then
               Option.to_list
                 (Option.map original_step.foreground ~f:(fun primary ->
                    current_original
                      ?diagnostic:original_step.foreground_diagnostic
                      ~prerequisite
                      (Some primary)))
             else [])
          @ replaced_control
              original_step.final
              current.final
              original_step.final_diagnostic
      in
      (selected :: E.own_list initial original_step.initial_diagnostic)
      @ E.own_list foreground original_step.foreground_diagnostic
      @ E.own_list final original_step.final_diagnostic
      @ [ required_final ]
      @ fixed
    ;;

    let phase_matches = Retirement_cleanup_phase.equal

    let startup_matches_construction
          (candidate : startup_candidate)
          (receipt : Construction_cleanup.t)
      =
      phys_equal candidate.construction receipt
    ;;

    let released_construction (receipt : Construction_cleanup.t) =
      Option.exists receipt.Construction_cleanup.startup_release ~f:(fun release ->
        Agent_session.Session_actor.Failed_startup_disposal.same_construction
          release
          ~construction:receipt.construction)
    ;;

    let construction ?startup_prerequisite captured receipt ~prerequisite =
      let close, ending =
        match receipt.Construction_cleanup.phase with
        | Awaiting_close -> [ deferred prerequisite ~owned:true ], []
        | Close_failed failure ->
          ( [ E.own
                (E.make
                   ~progress:Terminal
                   [ E.original ?diagnostic:receipt.close_diagnostic failure ])
                receipt.close_diagnostic
            ]
          , [] )
        | Closed ending ->
          let retention =
            construction_steps captured receipt Construction_retention ~prerequisite
          in
          let ending =
            match ending with
            | Admission_retained -> E.succeeded
            | Attempted primary ->
              original ?diagnostic:receipt.end_diagnostic ~prerequisite (Some primary)
            | Unattempted ->
              let current =
                Option.bind captured.observing ~f:(fun current ->
                  Option.bind
                    (List.find current.constructions ~f:(fun ending ->
                       phys_equal ending.receipt receipt))
                    ~f:(fun ending ->
                      Option.map ending.construction_end ~f:(fun primary ->
                        primary, ending.construction_end_diagnostic)))
              in
              (match current with
               | Some (primary, diagnostic) ->
                 current_original ?diagnostic ~prerequisite (Some primary)
               | None -> deferred (Prerequisite.of_evidence retention) ~owned:false)
          in
          let ending =
            if
              released_construction receipt
              || Option.exists startup_prerequisite ~f:(fun candidate ->
                startup_matches_construction candidate receipt)
            then E.with_custody_joined ending true
            else ending
          in
          E.succeeded :: retention, [ ending ]
      in
      let local =
        construction_steps captured receipt Local_release ~prerequisite
        @ batches
            captured
            (function
              | Recovery_batch.Role.Construction_local actual -> phys_equal actual receipt
              | Construction_exit -> false
              | Lease_admission
              | Lease_detachment
              | Source_entry
              | Ordinary_warm
              | Source_exit
              | Administration_prefix
              | Administration_before_commit
              | Administration_exit
              | Retirement_ordinary
              | Retirement_complete -> false)
            ~prerequisite
      in
      close
      @ ending
      @ local
      @ construction_steps captured receipt Construction_residency ~prerequisite
    ;;

    let initializations captured =
      List.concat_map (List.rev captured.initializations_rev) ~f:(fun boundary ->
        let callback =
          match Initialization_capture_data.snapshot boundary with
          | Error error -> E.make ~progress:Terminal [ E.original (Protocol error) ]
          | Ok snapshot ->
            let module C = Agent_session.Callback_cleanup in
            List.iter (C.Cleanup_snapshot.claims snapshot) ~f:(fun observation ->
              ignore
                (Initialization_capture_data.observe_completion boundary observation
                 : Original_cleanup_report.Segment.t list));
            let progress =
              Callback_evidence.progress
                ~pending:(C.Cleanup_snapshot.pending snapshot)
                ~failures:(C.Cleanup_snapshot.failures snapshot)
            in
            E.callbacks
              ~boundary
              ~custody_joined:(Callback_disposal.parent_settled snapshot)
              ~progress
              (C.Cleanup_snapshot.claims snapshot)
        in
        let ending =
          match Initialization_capture_data.original_end boundary with
          | None -> E.make ~progress:Blocked []
          | Some (Ok (Ok ())) -> E.succeeded
          | Some (Ok (Error error)) ->
            E.make
              ~progress:Terminal
              [ { E.Failure.source = Initializer_end boundary
                ; failure = Protocol error
                ; diagnostic = None
                }
              ]
          | Some (Error (exn, trace)) ->
            E.make
              ~progress:Terminal
              [ { E.Failure.source = Initializer_end boundary
                ; failure = Raised (exn, trace)
                ; diagnostic = None
                }
              ]
        in
        [ E.with_original
            callback
            [ Original_cleanup_report.Piece.Child
                (Initialization_capture_data.original_diagnostics boundary)
            ]
        ; ending
        ])
    ;;

    let original_child evidence ~diagnostics_complete =
      let report = E.child_view evidence ~diagnostics_complete in
      match evidence with
      | [] ->
        [ E.with_original
            { E.succeeded with custody_joined = diagnostics_complete }
            [ Original_cleanup_report.Piece.Child report ]
        ]
      | first :: rest ->
        let first =
          if diagnostics_complete then first else { first with custody_joined = false }
        in
        E.with_original first [ Original_cleanup_report.Piece.Child report ]
        :: List.map rest ~f:(fun duty -> E.with_original duty [])
    ;;

    let rec scope_with_context ?startup_prerequisite (recovery : t) =
      Owner_affinity.check_domain_exn recovery.affinity;
      let evidence, complete =
        match recovery.phase with
        | Original_running captured ->
          ( E.make ~progress:Blocked []
            :: recipe ?startup_prerequisite captured ~kind:recovery.kind ~completion:None
          , false )
        | Complete { captured; completion; current; original_projection; _ }
        | Retrying
            { before = { captured; completion; current; original_projection; _ }; _ } ->
          let captured = { captured with observing = Some current } in
          ( recipe
              ?startup_prerequisite
              captured
              ~kind:recovery.kind
              ~completion:(Some completion)
            @ [ E.with_original
                  E.succeeded
                  (List.map original_projection.boundary_diagnostics ~f:(fun diagnostic ->
                     Original_cleanup_report.Piece.Own diagnostic))
              ]
          , recovery.original_initializers_sealed )
      in
      original_child evidence ~diagnostics_complete:complete

    and child ?startup_prerequisite selected =
      scope_with_context ?startup_prerequisite selected.recovery

    and selected_claims captured (phase : Retirement_cleanup_phase.t) =
      List.filter (List.rev captured.policy_claims_rev) ~f:(fun selected ->
        List.mem selected.selected_phases phase ~equal:Retirement_cleanup_phase.equal)

    and child_alias captured selected (phase : Retirement_cleanup_phase.t) =
      let lease_alias =
        List.exists captured.leases_rev ~f:(fun earlier ->
          phys_equal earlier.finished selected.wrapper_finished
          && phys_equal earlier.recovery selected.wrapper_recovery
          &&
          match earlier.role, phase with
          | Retirement_lease { phase = Ordinary; _ }, (Ordinary | Complete_resources)
          | Retirement_lease { phase = Complete_resources; _ }, Complete_resources -> true
          | Source_lexical, (Ordinary | Complete_resources)
          | Retirement_lease { phase = Complete_resources; _ }, Ordinary -> false)
      in
      lease_alias
      ||
      match phase with
      | Ordinary -> false
      | Complete_resources ->
        List.mem selected.selected_phases Ordinary ~equal:Retirement_cleanup_phase.equal

    and claim
          ?startup_prerequisite
          captured
          selected
          (phase : Retirement_cleanup_phase.t)
          ~prerequisite
      =
      let attempt =
        match phase with
        | Retirement_cleanup_phase.Ordinary -> selected.ordinary_attempt
        | Complete_resources -> selected.complete_attempt
      in
      let wrapper = scope_with_context ?startup_prerequisite selected.wrapper_recovery in
      let wrapper =
        if child_alias captured selected phase
        then
          [ E.make
              ~custody_joined:(E.custody_joined wrapper)
              ~progress:(Cleanup_evidence.snapshot wrapper).progress
              []
          ]
        else wrapper
      in
      let current =
        Option.bind captured.observing ~f:(fun current ->
          List.find current.claims ~f:(fun current -> phys_equal current.claim selected))
      in
      let pin =
        Policy_claim_pin.observe
          selected.natural_release
          ~current:(Option.bind current ~f:(fun current -> current.claim_pin))
          ~attempts:
            (List.map
               [ selected.ordinary_attempt; selected.complete_attempt ]
               ~f:(fun attempt ->
                 Option.bind attempt ~f:(fun attempt ->
                   attempt.Policy_claim_attempt.release)))
      in
      let released = pin.released in
      let disposition =
        Agent_session.Session_actor.Delegated_policy_claim.Capture.disposition
          selected.capture
      in
      let custody =
        match disposition with
        | Released -> E.succeeded
        | Waiting | Retained ->
          E.make
            ~progress:
              (if Option.is_none (Eio.Promise.peek selected.wrapper_finished)
               then Blocked
               else Terminal)
            []
      in
      let observed =
        match attempt with
        | None ->
          (match Policy_claim_pin.without_attempt pin ~custody with
           | Some observed -> observed
           | None ->
             let prerequisite =
               match prerequisite with
               | Prerequisite.Ready -> Prerequisite.of_evidence (wrapper @ [ custody ])
               | Live | Repairable | Unfulfillable -> prerequisite
             in
             [ deferred prerequisite ~owned:true ])
        | Some attempt ->
          let drain =
            match attempt.drain, disposition with
            | Some primary, _ ->
              original ?diagnostic:attempt.drain_diagnostic ~prerequisite (Some primary)
            | None, Released -> E.succeeded
            | None, (Waiting | Retained) ->
              deferred (Prerequisite.of_evidence [ custody ]) ~owned:true
          in
          let pin =
            if released
            then E.succeeded
            else (
              match
                match current with
                | Some { claim_pin = Some primary; _ } -> Some primary
                | None | Some { claim_pin = None; _ } -> attempt.release
              with
              | Some primary ->
                E.make_origin_failures
                  ~progress:Retryable
                  (let origin =
                     match current with
                     | Some { claim_pin = Some _; claim_pin_origin; _ } ->
                       claim_pin_origin
                     | None | Some { claim_pin = None; _ } -> attempt.release_origin
                   in
                   E.Origin_failures.create
                     origin
                     ~copy:false
                     ~fallback:
                       (List.map
                          (Option.to_list (E.primary_failure primary))
                          ~f:(fun failure -> E.original failure)))
              | None ->
                (match pin.selection with
                 | Policy_claim_pin.Selection.Natural natural -> natural
                 | Missing ->
                   deferred (Prerequisite.of_evidence [ custody; drain ]) ~owned:true
                 | Current ->
                   invalid_arg "claim lost its selected current pin observation"))
          in
          (* Checking-custody or clearing escapes are independent of the known
             drain/pin projection. Only those actual phases own the whole result. *)
          let control =
            match attempt.phase with
            | Checking_custody | Checking_release | Clearing_custody ->
              original
                ?diagnostic:attempt.control_diagnostic
                ~prerequisite
                attempt.original
            | Draining_capture | Releasing_loan | Complete -> E.succeeded
          in
          [ custody
          ; E.own drain attempt.drain_diagnostic
          ; E.own_origin pin attempt.release_origin
          ; E.own control attempt.control_diagnostic
          ]
      in
      let current_clear =
        Option.value_map current ~default:[] ~f:(fun current ->
          Option.to_list
            (Option.map current.claim_clear ~f:(fun primary ->
               current_original
                 ?diagnostic:current.claim_clear_diagnostic
                 ~prerequisite
                 (Some primary))))
      in
      let deferred_pin =
        match attempt, current with
        | None, Some { claim_pin = Some (Returned (Ok ())); _ } -> []
        | None, Some { claim_pin = Some primary; claim_pin_origin; _ } ->
          [ E.make_origin_failures
              ~progress:Retryable
              (E.Origin_failures.create
                 claim_pin_origin
                 ~copy:false
                 ~fallback:
                   (List.map
                      (Option.to_list (E.primary_failure primary))
                      ~f:(fun failure -> E.original failure)))
          ]
        | None, (None | Some { claim_pin = None; _ }) | Some _, _ -> []
      in
      let pin_diagnostics =
        Option.value_map current ~default:[] ~f:(fun current ->
          Option.value_map current.claim_pin_origin ~default:[] ~f:(fun origin ->
            origin.Physical_origin.diagnostics))
      in
      let observed =
        match observed with
        | [] -> invalid_arg "claim observation lost its custody duty"
        | first :: rest -> E.with_diagnostics first pin_diagnostics :: rest
      in
      let ownership =
        match phase with
        | Retirement_cleanup_phase.Ordinary -> Policy_claim_pin.Ownership.Original
        | Complete_resources ->
          if
            List.mem
              selected.selected_phases
              Ordinary
              ~equal:Retirement_cleanup_phase.equal
          then Alias
          else Original
      in
      wrapper
      @ Policy_claim_pin.attach_original pin ~ownership observed
      @ deferred_pin
      @ current_clear

    and lease_admission ?startup_prerequisite captured =
      List.concat_map
        (List.rev captured.preparation_sources_rev)
        ~f:(scope_with_context ?startup_prerequisite)
      @ batches
          captured
          (function
            | Recovery_batch.Role.Lease_admission -> true
            | Lease_detachment
            | Source_entry
            | Construction_exit
            | Construction_local _
            | Ordinary_warm
            | Source_exit
            | Administration_prefix
            | Administration_before_commit
            | Administration_exit
            | Retirement_ordinary
            | Retirement_complete -> false)
          ~prerequisite:Ready

    and direct_callbacks captured =
      List.concat_map (List.rev captured.callbacks_rev) ~f:Callback_evidence.direct

    and lease_body captured =
      direct_callbacks captured @ physical captured Lease_pin ~prerequisite:Ready

    and lease_exit captured completion =
      let published =
        match completion with
        | Some (Original_completion.Lease Borrow_published) -> true
        | Some (Lease No_borrow) | None -> false
        | Some (Stage _ | Administration_transition | Construction | Retirement _) ->
          invalid_arg "lexical cleanup has a non-lease completion"
      in
      let observe position =
        if published
        then required_step captured position ~prerequisite:Ready
        else observe_steps captured position ~prerequisite:Ready
      in
      observe Lease_retire
      @ observe Lease_detach
      @ batches
          captured
          (function
            | Recovery_batch.Role.Lease_detachment -> true
            | Lease_admission
            | Source_entry
            | Construction_exit
            | Construction_local _
            | Ordinary_warm
            | Source_exit
            | Administration_prefix
            | Administration_before_commit
            | Administration_exit
            | Retirement_ordinary
            | Retirement_complete -> false)
          ~prerequisite:Ready
      @ observe Lease_check
      @ observe_steps captured Lease_escape ~prerequisite:Ready

    and source_child_parts ?startup_prerequisite selected =
      let captured, completion, running =
        match selected.recovery.phase with
        | Original_running captured -> captured, None, true
        | Complete { captured; completion; current; _ }
        | Retrying { before = { captured; completion; current; _ }; _ } ->
          { captured with observing = Some current }, Some completion, false
      in
      let admission = lease_admission ?startup_prerequisite captured in
      let exit = lease_body captured @ lease_exit captured completion in
      let complete = (not running) && selected.recovery.original_initializers_sealed in
      ( original_child admission ~diagnostics_complete:complete
      , original_child
          (if running then E.make ~progress:Blocked [] :: exit else exit)
          ~diagnostics_complete:complete )

    and source_end captured duty ~prerequisite =
      let current =
        Option.bind captured.observing ~f:(fun current ->
          Option.bind
            (List.find current.source_ends ~f:(fun ending -> phys_equal ending.duty duty))
            ~f:(fun ending ->
              Option.map ending.end_latest ~f:(fun primary ->
                primary, ending.end_diagnostic)))
      in
      let outcome, diagnostic =
        match current with
        | Some (primary, diagnostic) -> Some primary, diagnostic
        | None -> duty.Source_cleanup.original, duty.diagnostic
      in
      match outcome with
      | None -> deferred prerequisite ~owned:true
      | Some primary ->
        E.own
          (E.with_diagnostics
             (match E.primary_failure primary with
              | None -> E.succeeded
              | Some failure ->
                E.make ~progress:Retryable [ E.original ?diagnostic failure ])
             (Option.to_list diagnostic))
          duty.diagnostic

    and source_tail captured ~prerequisite =
      let ordered =
        List.stable_sort
          (List.rev captured.source_tail_rev)
          ~compare:Source_cleanup.compare_order
      in
      let _, reversed =
        List.fold
          ordered
          ~init:(prerequisite, [])
          ~f:(fun (native_prefix, reversed) duty ->
            let selected_prerequisite =
              match duty.Source_cleanup.target with
              | Native_release _ -> native_prefix
              | Policy_end _ | Native_end _ | Restored_release _ | Delegated_end _ ->
                prerequisite
            in
            let reports =
              match duty.target with
              | Native_release loan ->
                List.filter_map
                  (List.rev captured.native_release_reports_rev)
                  ~f:(fun report ->
                    if
                      phys_equal report.Native_release_report.loan loan
                      && Physical_cleanup_position.same report.position (Source_tail duty)
                    then
                      Some
                        (native_report
                           captured
                           report
                           ~prerequisite:selected_prerequisite)
                    else None)
                |> List.concat
              | Restored_release anchor ->
                List.filter_map
                  (List.rev captured.restored_release_reports_rev)
                  ~f:(fun report ->
                    if
                      phys_equal report.Restored_release_report.anchor anchor
                      && Physical_cleanup_position.same report.position (Source_tail duty)
                    then
                      Some
                        (restored_report
                           captured
                           report
                           ~prerequisite:selected_prerequisite)
                    else None)
                |> List.concat
              | Policy_end _ | Native_end _ | Delegated_end _ -> []
            in
            let original_reports = reports in
            let reports =
              if not (List.is_empty reports)
              then reports
              else (
                match duty.target with
                | Native_release loan ->
                  Option.value_map
                    (Option.bind (current_native captured loan) ~f:(fun entry ->
                       entry.native_latest))
                    ~default:[]
                    ~f:(fun latest ->
                      native_report
                        captured
                        ~position:(Source_tail duty)
                        latest.report
                        ~prerequisite:selected_prerequisite)
                | Restored_release anchor ->
                  Option.value_map
                    (Option.bind (current_restored captured anchor) ~f:(fun entry ->
                       entry.restored_latest))
                    ~default:[]
                    ~f:(fun latest ->
                      restored_report
                        captured
                        ~position:(Source_tail duty)
                        latest.report
                        ~prerequisite:selected_prerequisite)
                | Policy_end _ | Native_end _ | Delegated_end _ -> [])
            in
            let evidence =
              match duty.target, reports with
              | (Native_release _ | Restored_release _), _ :: _ ->
                let boundary =
                  if List.is_empty original_reports
                  then
                    Option.to_list
                      (Option.map duty.original ~f:(fun primary ->
                         original
                           ?diagnostic:duty.Source_cleanup.diagnostic
                           ~prerequisite:selected_prerequisite
                           (Some primary)))
                  else []
                in
                boundary @ reports
              | Native_release loan, [] ->
                let boundary =
                  Option.to_list
                    (Option.map duty.original ~f:(fun primary ->
                       original
                         ?diagnostic:duty.Source_cleanup.diagnostic
                         ~prerequisite:selected_prerequisite
                         (Some primary)))
                in
                let prerequisite =
                  if List.is_empty boundary
                  then selected_prerequisite
                  else Prerequisite.of_evidence boundary
                in
                let physical =
                  match loan.Native_loan.phase with
                  | Released -> E.succeeded
                  | Closing producer
                    when Option.is_none (Eio.Promise.peek producer.finished) ->
                    deferred Prerequisite.Live ~owned:true
                  | Available | Retiring | Closing _ -> deferred prerequisite ~owned:true
                in
                boundary @ [ physical ]
              | Restored_release anchor, [] ->
                let boundary =
                  Option.to_list
                    (Option.map duty.original ~f:(fun primary ->
                       original
                         ?diagnostic:duty.Source_cleanup.diagnostic
                         ~prerequisite:selected_prerequisite
                         (Some primary)))
                in
                let prerequisite =
                  if List.is_empty boundary
                  then selected_prerequisite
                  else Prerequisite.of_evidence boundary
                in
                let physical =
                  if restored_released captured anchor
                  then E.succeeded
                  else deferred prerequisite ~owned:true
                in
                boundary @ [ physical ]
              | (Policy_end _ | Native_end _ | Delegated_end _), [] ->
                [ source_end captured duty ~prerequisite:selected_prerequisite ]
              | (Policy_end _ | Native_end _ | Delegated_end _), _ :: _ ->
                invalid_arg "Actor End has physical helper reports"
            in
            let native_prefix =
              match duty.target with
              | Native_release _ -> Prerequisite.of_evidence evidence
              | Policy_end _ | Native_end _ | Restored_release _ | Delegated_end _ ->
                native_prefix
            in
            native_prefix, List.rev_append evidence reversed)
      in
      List.rev reversed

    and source_stage ?startup_prerequisite captured completion =
      let lexical =
        List.filter (List.rev captured.leases_rev) ~f:(fun selected ->
          match selected.role with
          | Source_lexical -> true
          | Retirement_lease _ -> false)
      in
      let parts = List.map lexical ~f:(source_child_parts ?startup_prerequisite) in
      let head =
        observe_steps captured Administration_unload_admission ~prerequisite:Ready
        @ List.concat_map parts ~f:fst
        @ batches
            captured
            (function
              | Recovery_batch.Role.Source_entry -> true
              | Lease_admission
              | Lease_detachment
              | Construction_exit
              | Construction_local _
              | Ordinary_warm
              | Source_exit
              | Administration_prefix
              | Administration_before_commit
              | Administration_exit
              | Retirement_ordinary
              | Retirement_complete -> false)
            ~prerequisite:Ready
      in
      let precommit =
        physical
          captured
          Administration_precommit
          ~prerequisite:(Prerequisite.of_evidence head)
      in
      let body =
        List.concat_map (List.rev captured.constructions_rev) ~f:(fun receipt ->
          construction ?startup_prerequisite captured receipt ~prerequisite:Ready)
        @ observe_steps captured Warm_release ~prerequisite:Ready
        @ batches
            captured
            (function
              | Recovery_batch.Role.Ordinary_warm -> true
              | Lease_admission
              | Lease_detachment
              | Source_entry
              | Construction_exit
              | Construction_local _
              | Source_exit
              | Administration_prefix
              | Administration_before_commit
              | Administration_exit
              | Retirement_ordinary
              | Retirement_complete -> false)
            ~prerequisite:Ready
        @ List.concat_map parts ~f:snd
      in
      let outer =
        observe_steps captured Source_physical_selection ~prerequisite:Ready
        @ (match completion with
           | Some (Original_completion.Stage Reserved) ->
             required_step captured Source_retire ~prerequisite:Ready
           | Some (Stage Not_reserved) | None ->
             observe_steps captured Source_retire ~prerequisite:Ready
           | Some (Lease _ | Administration_transition | Construction | Retirement _) ->
             invalid_arg "source stage has a non-stage completion")
        @ batches
            captured
            (function
              | Recovery_batch.Role.Source_exit -> true
              | Lease_admission
              | Lease_detachment
              | Source_entry
              | Construction_exit
              | Construction_local _
              | Ordinary_warm
              | Administration_prefix
              | Administration_before_commit
              | Administration_exit
              | Retirement_ordinary
              | Retirement_complete -> false)
            ~prerequisite:Ready
        @ observe_steps captured Source_installed_selection ~prerequisite:Ready
        @ observe_steps captured Source_prefix_selection ~prerequisite:Ready
      in
      let prefix =
        Option.value_map
          captured.prefix
          ~default:[]
          ~f:(scope_with_context ?startup_prerequisite)
        @ observe_steps captured Administration_unload_prefix ~prerequisite:Ready
        @ batches
            captured
            (function
              | Recovery_batch.Role.Administration_prefix -> true
              | Lease_admission
              | Lease_detachment
              | Source_entry
              | Construction_exit
              | Construction_local _
              | Ordinary_warm
              | Source_exit
              | Administration_before_commit
              | Administration_exit
              | Retirement_ordinary
              | Retirement_complete -> false)
            ~prerequisite:Ready
      in
      let outer_control =
        observe_steps captured Source_physical_selection ~prerequisite:Ready
        @ List.concat_map (List.rev captured.batches_rev) ~f:(fun selected ->
          match selected.Recovery_batch.role, selected.batch.original with
          | Source_exit, Raised (exn, backtrace) ->
            [ E.make ~progress:Terminal [ E.original (Raised (exn, backtrace)) ] ]
          | Source_exit, Returned _ -> []
          | ( ( Lease_admission
              | Lease_detachment
              | Source_entry
              | Construction_exit
              | Construction_local _
              | Ordinary_warm
              | Administration_prefix
              | Administration_before_commit
              | Administration_exit
              | Retirement_ordinary
              | Retirement_complete )
            , _ ) -> [])
        @ observe_steps captured Source_installed_selection ~prerequisite:Ready
        @ observe_steps captured Source_prefix_selection ~prerequisite:Ready
      in
      (* Control-only prerequisite: caught retire and physical batch negatives
         did not stop the original independent source tail. Its failures still
         occur exactly once in the full evidence above. *)
      let prerequisites = Prerequisite.of_evidence (outer_control @ prefix) in
      head
      @ precommit
      @ body
      @ outer
      @ prefix
      @ source_tail captured ~prerequisite:prerequisites
      @ required_step captured Source_clear ~prerequisite:Ready
      @
      match completion with
      | Some (Original_completion.Stage Reserved) ->
        required_step captured Source_exclusion_complete ~prerequisite:Ready
      | Some (Stage Not_reserved) | None ->
        observe_steps captured Source_exclusion_complete ~prerequisite:Ready
      | Some (Lease _ | Administration_transition | Construction | Retirement _) ->
        invalid_arg "source stage has a non-stage completion"

    and administration captured =
      observe_steps captured Administration_unload_transition ~prerequisite:Ready
      @ batches
          captured
          (function
            | Recovery_batch.Role.Administration_before_commit -> true
            | Lease_admission
            | Lease_detachment
            | Source_entry
            | Construction_exit
            | Construction_local _
            | Ordinary_warm
            | Source_exit
            | Administration_prefix
            | Administration_exit
            | Retirement_ordinary
            | Retirement_complete -> false)
          ~prerequisite:Ready
      @ observe_steps captured Administration_retire ~prerequisite:Ready
      @ batches
          captured
          (function
            | Recovery_batch.Role.Administration_exit -> true
            | Lease_admission
            | Lease_detachment
            | Source_entry
            | Construction_exit
            | Construction_local _
            | Ordinary_warm
            | Source_exit
            | Administration_prefix
            | Administration_before_commit
            | Retirement_ordinary
            | Retirement_complete -> false)
          ~prerequisite:Ready
      @ required_step captured Administration_registration ~prerequisite:Ready
      @ required_step captured Administration_clear ~prerequisite:Ready

    and selected_leases captured (phase : Retirement_cleanup_phase.t) =
      List.filter (List.rev captured.leases_rev) ~f:(fun selected ->
        match selected.role with
        | Source_lexical -> false
        | Retirement_lease actual -> phase_matches actual.phase phase)

    and consumer_join_control selected =
      let aliases observation =
        E.own_origin E.succeeded (Some (Physical_origin.of_observation observation))
      in
      let original =
        Option.to_list
          (Option.map selected.join_original ~f:(fun outcome ->
             aliases (Relation_evidence.Join_outcome.observation outcome)))
      in
      let current =
        match selected.join_current_control with
        | None -> []
        | Some (Relation_evidence.Join_control.Settled observation) ->
          [ aliases observation ]
        | Some (Blocking observation) ->
          [ E.own_origin
              (E.make
                 ~progress:Terminal
                 (E.reported_failures
                    (Physical_origin.of_observation observation)
                    ~copy:false))
              (Some (Physical_origin.of_observation observation))
          ]
      in
      List.map current ~f:(fun duty -> E.with_original duty []) @ original

    and relation selected provider =
      match selected.relation with
      | None -> consumer_join_control selected @ provider
      | Some relation ->
        let incomplete progress =
          original_child [ E.make ~progress [] ] ~diagnostics_complete:false
        in
        let consumer, consumer_complete =
          match relation.Relation_evidence.operation with
          | None -> incomplete Blocked, false
          | Some { selection = Detached_handoff; _ } -> [], true
          | Some { selection = Active_revocation; _ } ->
            (match relation.consumer with
             | Unfinished -> incomplete Blocked, false
             | Observed { guard; evidence } ->
               (match Relation_guard.check guard with
                | Current -> evidence, true
                | Busy ->
                  ( incomplete Blocked
                    @ [ E.with_original E.succeeded (E.original_pieces evidence) ]
                  , false )
                | Stale ->
                  ( incomplete Terminal
                    @ [ E.with_original E.succeeded (E.original_pieces evidence) ]
                  , false )))
        in
        let provider_complete =
          Option.is_some (Eio.Promise.peek selected.finished)
          &&
          match selected.recovery.phase with
          | Complete _ -> true
          | Original_running _ | Retrying _ -> false
        in
        let provider =
          if provider_complete
          then provider
          else
            E.make ~progress:Blocked []
            :: List.map provider ~f:(fun duty -> E.with_custody_joined duty false)
        in
        let native =
          match Relation_evidence.selected_native_evidence relation with
          | None ->
            incomplete
              (if consumer_complete && provider_complete then Terminal else Blocked)
          | Some { duties; complete } ->
            if complete then duties else original_child duties ~diagnostics_complete:false
        in
        consumer_join_control selected @ consumer @ provider @ native

    and retirement_leases
          ?startup_prerequisite
          captured
          (phase : Retirement_cleanup_phase.t)
          ~prerequisite
      =
      let selected = selected_leases captured phase in
      let cancellation =
        List.map selected ~f:(fun selected ->
          match selected.role with
          | Source_lexical -> invalid_arg "retirement selected a source lexical lease"
          | Retirement_lease { cancel; _ } ->
            (match cancel.original, selected.retry_cancel with
             | None, Some actual ->
               E.with_original
                 (original
                    ~origin:(Physical_origin.of_observation actual)
                    ~prerequisite
                    (Some (Physical_origin.primary actual)))
                 []
             | (Some _ | None), _ ->
               original
                 ?diagnostic:cancel.Original_cleanup_step.diagnostic
                 ?origin:cancel.forwarded_origin
                 ~prerequisite
                 cancel.original))
      in
      let reports =
        List.concat_map selected ~f:(fun selected ->
          let evidence = relation selected (child ?startup_prerequisite selected) in
          let previously_owned =
            match phase with
            | Retirement_cleanup_phase.Ordinary -> false
            | Complete_resources ->
              List.exists (selected_leases captured Ordinary) ~f:(fun earlier ->
                phys_equal earlier.finished selected.finished
                && phys_equal earlier.recovery selected.recovery)
              || List.exists (selected_claims captured Ordinary) ~f:(fun earlier ->
                phys_equal earlier.wrapper_finished selected.finished
                && phys_equal earlier.wrapper_recovery selected.recovery)
          in
          if previously_owned
          then
            [ E.make
                ~custody_joined:(E.custody_joined evidence)
                ~progress:(E.snapshot evidence).progress
                []
            ]
          else evidence)
      in
      cancellation @ reports

    and retirement_ordinary ?startup_prerequisite captured =
      let pending =
        Option.value_map
          captured.pending_source
          ~default:[]
          ~f:(scope_with_context ?startup_prerequisite)
      in
      let selection =
        required_step
          captured
          Actor_selection
          ~prerequisite:(Prerequisite.of_evidence pending)
      in
      let dependency =
        observe_steps
          captured
          Dependency_stop
          ~prerequisite:(Prerequisite.of_evidence selection)
      in
      let borrowers =
        retirement_leases
          ?startup_prerequisite
          captured
          Ordinary
          ~prerequisite:(Prerequisite.of_evidence (selection @ dependency))
      in
      let claims =
        List.concat_map (selected_claims captured Ordinary) ~f:(fun selected ->
          claim
            ?startup_prerequisite
            captured
            selected
            Ordinary
            ~prerequisite:(Prerequisite.of_evidence (selection @ dependency @ borrowers)))
      in
      let prefix = selection @ dependency @ borrowers @ claims in
      let actor =
        match captured.actor_cleanup with
        | Some step -> actor captured step ~prerequisite:(Prerequisite.of_evidence prefix)
        | None -> [ deferred (Prerequisite.of_evidence prefix) ~owned:false ]
      in
      let before_physical = prefix @ actor in
      let physical =
        physical
          captured
          Ordinary_suffix
          ~prerequisite:(Prerequisite.of_evidence before_physical)
      in
      let policy =
        observe_steps
          captured
          Policy_ordinary
          ~prerequisite:(Prerequisite.of_evidence (before_physical @ physical))
      in
      let detach =
        required_step
          captured
          Retirement_detach
          ~prerequisite:(Prerequisite.of_evidence (before_physical @ physical @ policy))
      in
      let batches =
        batches
          captured
          (function
            | Recovery_batch.Role.Retirement_ordinary -> true
            | Lease_admission
            | Lease_detachment
            | Source_entry
            | Construction_exit
            | Construction_local _
            | Ordinary_warm
            | Source_exit
            | Administration_prefix
            | Administration_before_commit
            | Administration_exit
            | Retirement_complete -> false)
          ~prerequisite:(Prerequisite.of_evidence detach)
      in
      pending @ before_physical @ physical @ policy @ detach @ batches

    and retirement_complete ?startup_prerequisite captured ordinary ~entered =
      let prerequisite = Prerequisite.of_evidence ordinary in
      if not entered
      then [ deferred prerequisite ~owned:false ]
      else (
        let selection = required_step captured Complete_selection ~prerequisite in
        let borrowers =
          retirement_leases
            ?startup_prerequisite
            captured
            Complete_resources
            ~prerequisite:(Prerequisite.of_evidence selection)
        in
        let claims =
          List.concat_map
            (selected_claims captured Complete_resources)
            ~f:(fun selected ->
              claim
                ?startup_prerequisite
                captured
                selected
                Complete_resources
                ~prerequisite:(Prerequisite.of_evidence (selection @ borrowers)))
        in
        let before_drains = selection @ borrowers @ claims in
        let drains =
          List.fold
            [ Step_position.Complete_constructor
            ; Complete_callback
            ; Complete_descendant
            ; Complete_inference
            ]
            ~init:[]
            ~f:(fun evidence position ->
              evidence
              @ required_step
                  captured
                  position
                  ~prerequisite:(Prerequisite.of_evidence (before_drains @ evidence)))
        in
        (* The same foreground report was emitted in the ordinary pass. Complete
           reuses it as a prerequisite; it does not select another current worker. *)
        let physical =
          physical
            captured
            Complete_suffix
            ~prerequisite:(Prerequisite.of_evidence (before_drains @ drains))
        in
        let policy =
          observe_steps
            captured
            Policy_complete
            ~prerequisite:(Prerequisite.of_evidence (before_drains @ drains @ physical))
        in
        let batches =
          batches
            captured
            (function
              | Recovery_batch.Role.Retirement_complete -> true
              | Lease_admission
              | Lease_detachment
              | Source_entry
              | Construction_exit
              | Construction_local _
              | Ordinary_warm
              | Source_exit
              | Administration_prefix
              | Administration_before_commit
              | Administration_exit
              | Retirement_ordinary -> false)
            ~prerequisite:
              (Prerequisite.of_evidence (before_drains @ drains @ physical @ policy))
        in
        let checked =
          required_step
            captured
            Complete_check
            ~prerequisite:
              (Prerequisite.of_evidence
                 (before_drains @ drains @ physical @ policy @ batches))
        in
        let checked =
          match captured.disposal_check with
          | Some check
            when check.physical_ready
                 && (check.source_custody_joined
                     || Option.exists startup_prerequisite ~f:(fun candidate ->
                       phys_equal candidate.check check
                       && Option.exists captured.observing ~f:(fun current ->
                         phys_equal current.owner candidate.retirement)
                       && Option.exists check.source ~f:(phys_equal candidate.source))
                     || Option.exists captured.observing ~f:(fun current ->
                       Option.exists current.startup_settlement ~f:(fun settled ->
                         phys_equal current.owner settled.candidate.retirement
                         && phys_equal settled.candidate.check check
                         && Option.exists
                              check.source
                              ~f:(phys_equal settled.candidate.source)
                         && Agent_session.Session_actor.Failed_startup_disposal
                            .same_construction
                              settled.release
                              ~construction:settled.candidate.construction.construction))
                    ) ->
            let selected = steps captured Complete_check in
            if List.is_empty selected
            then checked
            else
              List.map selected ~f:(fun step ->
                let observed =
                  original
                    ~prerequisite:
                      (Prerequisite.of_evidence
                         (before_drains @ drains @ physical @ policy @ batches))
                    ?diagnostic:step.Original_cleanup_step.diagnostic
                    ?origin:step.forwarded_origin
                    step.Original_cleanup_step.original
                in
                match step.original with
                | Some (Returned (Error _)) when phys_equal step check.guard ->
                  E.with_custody_joined observed true
                | None | Some (Returned _ | Raised _) -> observed)
          | None | Some _ -> checked
        in
        before_drains @ drains @ physical @ policy @ batches @ checked)

    and retirement ?startup_prerequisite captured completion =
      let coverage = observe_steps captured Coverage_check ~prerequisite:Ready in
      coverage
      @
      match completion with
      | Some (Original_completion.Retirement { required; entered }) ->
        (match required, entered with
         | (Ordinary | All_resources), Released_before_physical ->
           let pending =
             Option.value_map
               captured.pending_source
               ~default:[]
               ~f:(scope_with_context ?startup_prerequisite)
           in
           let borrowers =
             retirement_leases
               ?startup_prerequisite
               captured
               Ordinary
               ~prerequisite:(Prerequisite.of_evidence pending)
           in
           pending
           @ borrowers
           @ required_step
               captured
               Released_check
               ~prerequisite:(Prerequisite.of_evidence (pending @ borrowers))
         | (Ordinary | All_resources), Not_entered ->
           Option.value_map
             captured.pending_source
             ~default:[]
             ~f:(scope_with_context ?startup_prerequisite)
           @ [ E.make ~progress:Terminal [] ]
         | Ordinary, Ordinary -> retirement_ordinary ?startup_prerequisite captured
         | All_resources, Ordinary ->
           let ordinary = retirement_ordinary ?startup_prerequisite captured in
           ordinary
           @ retirement_complete ?startup_prerequisite captured ordinary ~entered:false
         | All_resources, Complete_resources ->
           let ordinary = retirement_ordinary ?startup_prerequisite captured in
           ordinary
           @ retirement_complete ?startup_prerequisite captured ordinary ~entered:true
         | Ordinary, Complete_resources ->
           invalid_arg "complete cleanup entered without its required strength")
      | None -> retirement_ordinary ?startup_prerequisite captured
      | Some (Lease _ | Stage _ | Administration_transition | Construction) ->
        invalid_arg "retirement cleanup has a non-retirement completion"

    and recipe ?startup_prerequisite captured ~kind ~completion =
      initializations captured
      @
      match kind, completion with
      | Kind.Lease, (None | Some (Original_completion.Lease _)) ->
        lease_admission ?startup_prerequisite captured
        @ lease_body captured
        @ lease_exit captured completion
      | Source Stage, (None | Some (Stage _)) ->
        source_stage ?startup_prerequisite captured completion
      | Source Administration_transition, (None | Some Administration_transition) ->
        administration captured
      | Retirement, (None | Some (Retirement _)) ->
        retirement ?startup_prerequisite captured completion
      | Construction, (None | Some Construction) ->
        batches
          captured
          (function
            | Recovery_batch.Role.Construction_exit -> true
            | Lease_admission
            | Lease_detachment
            | Source_entry
            | Construction_local _
            | Ordinary_warm
            | Source_exit
            | Administration_prefix
            | Administration_before_commit
            | Administration_exit
            | Retirement_ordinary
            | Retirement_complete -> false)
          ~prerequisite:Prerequisite.Ready
      | ( (Lease | Construction | Source _ | Retirement)
        , Some
            (Lease _ | Stage _ | Administration_transition | Construction | Retirement _)
        ) -> invalid_arg "cleanup completion does not match its actual original scope"
    ;;

    (* Only this Boolean query can construct the transient exception. Neither
       canonical progress nor original diagnostics are altered or published. *)
    let startup_prerequisites (candidate : startup_candidate) =
      let positive recovery =
        let evidence = scope_with_context ~startup_prerequisite:candidate recovery in
        E.custody_joined evidence
        && Original_cleanup_report.diagnostics_complete
             (E.child_view evidence ~diagnostics_complete:true)
      in
      positive candidate.source && positive candidate.retirement
    ;;

    (* Normal consumers have a unary no-context entry. Only the Boolean
       prerequisite above supplies the private transient exception. *)
    let scope recovery = scope_with_context recovery

    let snapshot duties =
      let snapshot, selections = E.snapshot_with_selection duties in
      let callback_failure = function
        | Scope_outcome.Cleanup.Failure.Protocol error ->
          Agent_session.Callback_cleanup.Failure.Protocol error
        | Raised (exn, trace) -> Raised (exn, trace)
      in
      let diagnostic (selected : E.Selection.t) =
        match selected.diagnostic with
        | Some _ as diagnostic -> diagnostic
        | None ->
          (match selected.source with
           | Callback (boundary, completion, slot) ->
             Some
               (Initialization_capture_data.completion_diagnostic
                  boundary
                  ~completion
                  ~slot
                  ~failure:(callback_failure selected.failure))
           | Evidence (Initializer_end boundary) ->
             Initialization_capture_data.end_diagnostic boundary
           | Evidence (Reported_actual (diagnostic, _) | Reported_prior (diagnostic, _))
             -> Some diagnostic
           | Evidence (Original | Retired_actual _ | Retired_prior _ | Native_actual _) ->
             None)
      in
      let selected = List.map selections ~f:diagnostic in
      (* Resolve actual callback slots before reading the fresh original view:
         the bounded slot cache may have acquired a late write-once failure. *)
      let diagnostics =
        List.concat_map duties ~f:(fun duty -> duty.E.diagnostics)
        @ Original_cleanup_report.diagnostics
            (E.child_view duties ~diagnostics_complete:true)
        @ List.filter_opt selected
      in
      let observation =
        if List.exists selected ~f:Option.is_none
        then Cleanup_snapshot.unavailable_observation ()
        else (
          match selections, selected with
          | [], [] ->
            if Cleanup_snapshot.equal_progress snapshot.progress Joined
            then
              Ok
                (Agent_session.Close_observation.create
                   (Returned (Ok ()))
                   ~diagnostics
                   ())
            else Cleanup_snapshot.unavailable_observation ()
          | first :: _, Some primary :: _ ->
            let exit =
              match first.failure with
              | Scope_outcome.Cleanup.Failure.Protocol error ->
                Agent_session.Close_observation.Returned (Error error)
              | Raised (exn, trace) -> Raised (exn, trace)
            in
            Ok (Agent_session.Close_observation.create exit ~diagnostics ~primary ())
          | [], _ :: _ | _ :: _, ([] | None :: _) ->
            invalid_arg "cleanup failure selection metadata changed cardinality")
      in
      Cleanup_snapshot.with_observation snapshot observation
    ;;

    let%expect_test "snapshot metadata preserves selection, aliases and refusal" =
      let module D = Agent_session.Cleanup_diagnostic in
      let module CO = Agent_session.Close_observation in
      let recorded = Stdlib.Printexc.backtrace_status () in
      Stdlib.Printexc.record_backtrace true;
      Exn.protect
        ~finally:(fun () -> Stdlib.Printexc.record_backtrace recorded)
        ~f:(fun () ->
          (* These are diagnostic DATA only: no Owner, callback or custody receipt
             is created. Distinct actual catches retain distinct trace objects. *)
          let sentinel = Failure "snapshot selected producer" in
          let caught () =
            match raise sentinel with
            | _ -> assert false
            | exception actual ->
              assert (phys_equal actual sentinel);
              let trace = Stdlib.Printexc.get_raw_backtrace () in
              assert (
                not (String.is_empty (Stdlib.Printexc.raw_backtrace_to_string trace)));
              trace
          in
          let first_trace = caught () in
          let second_trace = caught () in
          assert (not (phys_equal first_trace second_trace));
          let historical = D.create [ Raised (sentinel, first_trace) ] in
          let first = D.create [ Raised (sentinel, first_trace) ] in
          let second = D.create [ Raised (sentinel, second_trace) ] in
          let current diagnostic trace =
            current_original
              ~diagnostic
              ~prerequisite:Prerequisite.Ready
              (Some (Scope_outcome.Raised (sentinel, trace)))
          in
          let selected =
            E.own_list
              [ current first first_trace; current second second_trace ]
              (Some historical)
          in
          let original = E.child_view selected ~diagnostics_complete:true in
          assert (
            List.equal
              D.equal
              (Original_cleanup_report.diagnostics original)
              [ historical ]);
          let preceding = E.with_diagnostics E.succeeded [ historical ] in
          let failed = snapshot (preceding :: selected) in
          assert (
            Cleanup_snapshot.equal_progress (Cleanup_snapshot.progress failed) Terminal);
          let observation_exn snapshot =
            match Cleanup_snapshot.observation snapshot with
            | Ok observation -> observation
            | Error _ -> failwith "complete metadata refused its observation"
          in
          let observation = observation_exn failed in
          (* Encounter order retains G first; the first canonical failure K,
             rather than the first diagnostic group G, selects the raw exit. *)
          assert (
            List.equal D.equal (CO.diagnostics observation) [ historical; first; second ]);
          assert (Option.equal D.equal (CO.primary observation) (Some first));
          (match CO.exit observation with
           | Raised (actual, trace) ->
             assert (phys_equal actual sentinel);
             assert (phys_equal trace first_trace)
           | Returned _ -> failwith "snapshot lost its first canonical raw failure");
          let positive =
            snapshot [ E.with_diagnostics E.succeeded [ historical; first; historical ] ]
          in
          let positive = observation_exn positive in
          (match CO.exit positive with
           | Returned (Ok ()) -> ()
           | Returned (Error _) | Raised _ -> failwith "positive metadata changed exit");
          assert (Option.is_none (CO.primary positive));
          assert (List.equal D.equal (CO.diagnostics positive) [ historical; first ]);
          let missing =
            E.make
              ~progress:Terminal
              [ E.original (Scope_outcome.Cleanup.Failure.Raised (sentinel, second_trace))
              ]
          in
          let refused = snapshot [ current first first_trace; missing ] in
          assert (
            Cleanup_snapshot.equal_progress (Cleanup_snapshot.progress refused) Terminal);
          List.iter2_exn
            (Cleanup_snapshot.failures refused)
            [ first_trace; second_trace ]
            ~f:(fun failure expected ->
              match failure with
              | Raised (actual, trace) ->
                assert (phys_equal actual sentinel);
                assert (phys_equal trace expected)
              | Protocol _ -> failwith "missing origin changed actual raw failures");
          (match Cleanup_snapshot.observation refused with
           | Error error ->
             assert (Agent_protocol.Error.equal_code error.code Conflict);
             assert (not error.retryable)
           | Ok _ -> failwith "missing origin fabricated a producer observation");
          (* Producer selection is distinct from encounter metadata. An actual
             H report may retain old G first without selecting G again. *)
          let origin primary : Physical_origin.t =
            { diagnostics = [ historical; second ]; primary }
          in
          let from_origin origin fallback =
            E.Origin_failures.create (Some origin) ~copy:false ~fallback
            |> E.make_origin_failures ~progress:Retryable
          in
          let fallback =
            E.original (Scope_outcome.Cleanup.Failure.Raised (sentinel, second_trace))
          in
          let current_evidence = from_origin (origin (Some second)) [ fallback ] in
          let current_evidence = E.own_list [ current_evidence ] (Some historical) in
          let selected = snapshot current_evidence in
          let selected_observation = observation_exn selected in
          assert (
            List.equal
              D.equal
              (CO.diagnostics selected_observation)
              [ historical; second ]);
          assert (Option.equal D.equal (CO.primary selected_observation) (Some second));
          (match CO.exit selected_observation with
           | Raised (actual, trace) ->
             assert (phys_equal actual sentinel);
             assert (phys_equal trace second_trace)
           | Returned _ -> failwith "origin primary lost the actual H raw failure");
          assert (
            List.equal
              D.equal
              (Original_cleanup_report.diagnostics
                 (E.child_view current_evidence ~diagnostics_complete:true))
              [ historical ]);
          let empty = D.create [] in
          List.iter [ None; Some empty; Some first ] ~f:(fun primary ->
            let actual_origin = origin primary in
            let unknown = snapshot [ from_origin actual_origin [ fallback ] ] in
            (match Cleanup_snapshot.failures unknown with
             | Raised (actual, trace) :: _ ->
               assert (phys_equal actual sentinel);
               assert (phys_equal trace second_trace)
             | [] | Protocol _ :: _ -> failwith "missing primary guessed inventory G");
            (match Cleanup_snapshot.observation unknown with
             | Error _ -> ()
             | Ok _ -> failwith "invalid primary invented actual failure metadata");
            (* A real fallback carrier may be known even if omitted from an
               invalid origin; retain it after the encounter inventory. *)
            let known_fallback =
              E.original
                ~diagnostic:first
                (Scope_outcome.Cleanup.Failure.Raised (sentinel, first_trace))
            in
            let known =
              from_origin actual_origin [ known_fallback ]
              |> fun duty -> snapshot [ duty ]
            in
            let known = observation_exn known in
            assert (Option.equal D.equal (CO.primary known) (Some first));
            assert (
              List.equal D.equal (CO.diagnostics known) [ historical; second; first ]));
          let positive =
            E.Origin_failures.create
              (Some (origin (Some second)))
              ~copy:false
              ~fallback:[]
            |> E.make_origin_failures ~progress:Joined
          in
          assert (List.is_empty positive.failures);
          let positive = snapshot [ positive ] |> observation_exn in
          assert (Option.is_none (CO.primary positive));
          assert (List.equal D.equal (CO.diagnostics positive) [ historical; second ]);
          let () =
            (* Actual boundary captures, then private DATA projection only. No
             Owner, claim, loan or physical completion receipt is fabricated. *)
            let module Release = Policy_claim_release in
            let module Pin = Policy_claim_pin in
            let natural_failure = Failure "natural claim pin boundary" in
            let natural =
              Physical_origin.opaque_result (fun () -> raise natural_failure)
            in
            let natural_trace =
              match CO.exit natural with
              | Raised (actual, trace) ->
                assert (phys_equal actual natural_failure);
                assert (
                  not (String.is_empty (Stdlib.Printexc.raw_backtrace_to_string trace)));
                trace
              | Returned _ -> failwith "actual natural boundary lost its exception"
            in
            let natural_group = Option.value_exn (CO.primary natural) in
            let current_error =
              Agent_protocol.Error.create
                Conflict
                ~message:"actual current claim pin boundary"
                ~retryable:true
                ()
            in
            let current_observation =
              Physical_origin.opaque_result (fun () -> Error current_error)
            in
            let current_group = Option.value_exn (CO.primary current_observation) in
            assert (not (D.equal natural_group current_group));
            let release = Release.create () in
            let missing () =
              assert (Option.is_none (Release.original release));
              let pin = Pin.observe release ~current:None ~attempts:[ None; None ] in
              assert (not pin.released);
              assert (Option.is_none (Pin.without_attempt pin ~custody:E.succeeded));
              let unavailable = snapshot [ deferred Ready ~owned:true ] in
              assert (not (E.custody_joined [ deferred Ready ~owned:true ]));
              match Cleanup_snapshot.observation unavailable with
              | Error _ -> ()
              | Ok _ -> failwith "absent natural pin report inferred successful release"
            in
            let refuses f =
              match f () with
              | () -> failwith "natural pin report accepted duplicate publication"
              | exception Invalid_argument _ -> ()
            in
            missing ();
            refuses (fun () -> Release.record release natural);
            missing ();
            Release.begin_release release;
            missing ();
            refuses (fun () -> Release.begin_release release);
            Release.record release natural;
            let natural_original = Option.value_exn (Release.original release) in
            assert (phys_equal natural_original.observation natural);
            assert (
              Option.equal D.equal natural_original.origin.primary (Some natural_group));
            refuses (fun () -> Release.record release current_observation);
            assert (
              phys_equal (Option.value_exn (Release.original release)) natural_original);
            let duties ~current ~ownership =
              let pin = Pin.observe release ~current ~attempts:[ None; None ] in
              let observed =
                match Pin.without_attempt pin ~custody:E.succeeded with
                | Some observed -> observed
                | None -> failwith "known natural pin report became unavailable"
              in
              Pin.attach_original pin ~ownership observed
            in
            let ordinary = duties ~current:None ~ownership:Original in
            let complete = duties ~current:None ~ownership:Alias in
            let natural_selected = snapshot (ordinary @ complete) |> observation_exn in
            (match CO.exit natural_selected with
             | Raised (actual, trace) ->
               assert (phys_equal actual natural_failure);
               assert (phys_equal trace natural_trace)
             | Returned _ -> failwith "known natural failure became missing evidence");
            assert (
              Option.equal D.equal (CO.primary natural_selected) (Some natural_group));
            [%test_eq: int]
              1
              (List.length (Cleanup_snapshot.failures (snapshot (ordinary @ complete))));
            let original_groups evidence =
              Original_cleanup_report.diagnostics
                (E.child_view evidence ~diagnostics_complete:true)
            in
            assert (List.equal D.equal (original_groups ordinary) [ natural_group ]);
            assert (List.is_empty (original_groups complete));
            assert (
              List.equal D.equal (original_groups (ordinary @ complete)) [ natural_group ]);
            (* The existing current-pin path owns K. Natural G supplies original
             aliases only: it must not win current selection or reappear twice. *)
            let current_primary = Physical_origin.primary current_observation in
            let current_ordinary =
              duties ~current:(Some current_primary) ~ownership:Original
            in
            assert (E.custody_joined current_ordinary);
            assert (List.is_empty (Cleanup_snapshot.failures (snapshot current_ordinary)));
            let current_duty =
              current_original
                ~diagnostic:current_group
                ~prerequisite:Ready
                (Some current_primary)
            in
            let current_selected =
              snapshot (current_ordinary @ [ current_duty ]) |> observation_exn
            in
            (match CO.exit current_selected with
             | Returned (Error actual) -> assert (phys_equal actual current_error)
             | Returned (Ok ()) | Raised _ -> failwith "natural G replaced current K");
            assert (
              Option.equal D.equal (CO.primary current_selected) (Some current_group));
            assert (
              List.equal
                D.equal
                (CO.diagnostics current_selected)
                [ natural_group; current_group ]);
            let repaired_ordinary =
              duties ~current:(Some (Returned (Ok ()))) ~ownership:Original
            in
            let repaired_complete =
              duties ~current:(Some (Returned (Ok ()))) ~ownership:Alias
            in
            let repaired = repaired_ordinary @ repaired_complete in
            assert (E.custody_joined repaired);
            let repaired_observation = snapshot repaired |> observation_exn in
            (match CO.exit repaired_observation with
             | Returned (Ok ()) -> ()
             | Returned (Error _) | Raised _ ->
               failwith "pin repair projection retained current G");
            assert (Option.is_none (CO.primary repaired_observation));
            assert (
              List.equal D.equal (CO.diagnostics repaired_observation) [ natural_group ]);
            assert (List.equal D.equal (original_groups repaired) [ natural_group ]);
            assert (
              phys_equal (Option.value_exn (Release.original release)) natural_original);
            assert (phys_equal natural_original.observation natural);
            match CO.exit natural_original.observation with
            | Raised (actual, trace) ->
              assert (phys_equal actual natural_failure);
              assert (phys_equal trace natural_trace)
            | Returned _ -> failwith "current repair rewrote original natural failure"
          in
          let native = Relation_evidence.native () in
          assert (Option.is_none (Relation_evidence.native_evidence native));
          assert (Option.is_none (Relation_evidence.native_handoff_evidence native));
          let original_native =
            CO.create
              (Raised (sentinel, first_trace))
              ~diagnostics:[ first; second ]
              ~primary:first
              ()
          in
          let raw_outer =
            CO.create
              (Raised (sentinel, second_trace))
              ~diagnostics:[ second ]
              ~primary:second
              ()
          in
          Relation_evidence.seal_native
            native
            ~observation:original_native
            ~outer_escape:(Some raw_outer);
          let native_duties =
            match Relation_evidence.native_evidence native with
            | Some { duties; complete = true } -> duties
            | Some { complete = false; _ } | None ->
              failwith "known native receipt became incomplete"
          in
          let after_pin_repair =
            snapshot (E.succeeded :: native_duties) |> observation_exn
          in
          assert (Option.equal D.equal (CO.primary after_pin_repair) (Some second));
          (match CO.exit after_pin_repair with
           | Raised (actual, trace) ->
             assert (phys_equal actual sentinel);
             assert (phys_equal trace second_trace)
           | Returned _ -> failwith "pin repair erased the actual outer escape");
          let original_view = E.child_view native_duties ~diagnostics_complete:true in
          assert (
            List.equal
              D.equal
              (Original_cleanup_report.diagnostics original_view)
              [ first; second ]);
          assert (Option.equal D.equal (CO.primary original_native) (Some first));
          let detached =
            match Relation_evidence.native_handoff_evidence native with
            | Some { duties; complete = true } -> duties
            | None | Some { complete = false; _ } ->
              failwith "sealed historical native receipt became unavailable"
          in
          let detached_current = snapshot (E.succeeded :: detached) |> observation_exn in
          (match CO.exit detached_current with
           | Returned (Ok ()) -> ()
           | Returned (Error _) | Raised _ ->
             failwith "historical handoff newly selected the child outer failure");
          assert (Option.is_none (CO.primary detached_current));
          let provider_failed =
            snapshot (current first first_trace :: detached) |> observation_exn
          in
          assert (Option.equal D.equal (CO.primary provider_failed) (Some first));
          (match CO.exit provider_failed with
           | Raised (actual, trace) ->
             assert (phys_equal actual sentinel);
             assert (phys_equal trace first_trace)
           | Returned _ ->
             failwith "historical handoff erased the actual provider failure");
          assert (List.equal D.equal (CO.diagnostics detached_current) [ first; second ]);
          let detached_original = E.child_view detached ~diagnostics_complete:true in
          assert (
            List.equal
              D.equal
              (Original_cleanup_report.diagnostics detached_original)
              [ first; second ]);
          assert (Option.equal D.equal (CO.primary original_native) (Some first));
          let pin_only = Relation_evidence.native () in
          Relation_evidence.seal_native
            pin_only
            ~observation:original_native
            ~outer_escape:None;
          let after_repair =
            match Relation_evidence.native_evidence pin_only with
            | Some { duties; complete = true } ->
              snapshot (E.succeeded :: duties) |> observation_exn
            | Some { complete = false; _ } | None ->
              failwith "known pin receipt became incomplete"
          in
          (match CO.exit after_repair with
           | Returned (Ok ()) -> ()
           | Returned (Error _) | Raised _ ->
             failwith "original pin failure was frozen after repair");
          assert (Option.is_none (CO.primary after_repair));
          assert (List.equal D.equal (CO.diagnostics after_repair) [ first; second ]);
          assert (phys_equal (observation_exn failed) observation);
          assert (
            List.equal
              D.equal
              (Original_cleanup_report.diagnostics original)
              [ historical ]));
      print_endline
        "canonical primary; ordered aliases; positive groups; missing-origin refusal";
      [%expect
        {| canonical primary; ordered aliases; positive groups; missing-origin refusal |}]
    ;;

    let completed captured ~kind ~completion =
      snapshot (recipe captured ~kind ~completion:(Some completion))
    ;;
  end

  let original_initialization_projection (t : t) =
    Owner_affinity.check_domain_exn t.affinity;
    let captured = captured_of_phase t.phase in
    let snapshot, selections =
      Cleanup_evidence.snapshot_with_selection (Observation.initializations captured)
    in
    let cleanup =
      match Cleanup_snapshot.failures snapshot with
      | first :: rest -> Scope_outcome.Cleanup.Failed (first, rest)
      | [] ->
        if
          List.exists captured.initializations_rev ~f:(fun boundary ->
            Option.is_some (Initialization_capture_data.original_end boundary))
        then Scope_outcome.Cleanup.Joined
        else Never_acquired
    in
    let original =
      List.concat_map (List.rev captured.initializations_rev) ~f:(fun boundary ->
        Original_cleanup_report.diagnostics
          (Initialization_capture_data.original_diagnostics boundary))
    in
    let callback_failure = function
      | Scope_outcome.Cleanup.Failure.Protocol error ->
        Agent_session.Callback_cleanup.Failure.Protocol error
      | Raised (exn, trace) -> Raised (exn, trace)
    in
    let diagnostics =
      List.map selections ~f:(fun selected ->
        match selected.Cleanup_evidence.Selection.source with
        | Callback (boundary, completion, slot) ->
          Initialization_capture_data.completion_diagnostic
            boundary
            ~completion
            ~slot
            ~failure:(callback_failure selected.failure)
        | Evidence (Initializer_end boundary) ->
          (match Initialization_capture_data.end_diagnostic boundary with
           | Some diagnostic -> diagnostic
           | None -> invalid_arg "original End failure has no original producer group")
        | Evidence
            ( Original
            | Retired_actual _
            | Retired_prior _
            | Native_actual _
            | Reported_actual _
            | Reported_prior _ ) ->
          (* This is the actual failed snapshot-read boundary, not an inferred
             callback origin. Its result is selected once by this lexical call. *)
          Original_cleanup_report.Segment.create [ callback_failure selected.failure ])
    in
    match diagnostics with
    | [] -> Cleanup_projection.create cleanup ~origin:None
    | primary :: _ ->
      (* Keep known original groups plus every freshly selected exact slot.
         New slots retain one carrier on the original Completion association;
         repeated selections forward it. The old frozen view stays unchanged. *)
      Cleanup_projection.create
        cleanup
        ~boundary_diagnostics:diagnostics
        ~origin:
          (Some
             { Physical_origin.diagnostics = diagnostics @ original
             ; primary = Some primary
             })
  ;;

  (* The installed claim bracket records producer exit before its protected
     finish call and waits for the actual reply. A positive finish handler has
     already recorded release; rejection/handler/await failures have an explicit
     slot. Therefore a supported nonescaping acquired body cannot return with an
     enrolled pending claim and no cleanup negative. This flat report describes
     actual calls only; the canonical child recipe still checks current custody. *)
  let original_callback_projection (t : t) =
    Owner_affinity.check_domain_exn t.affinity;
    let captured = captured_of_phase t.phase in
    let snapshot, selections =
      Cleanup_evidence.snapshot_with_selection (Observation.direct_callbacks captured)
    in
    let cleanup =
      match Cleanup_snapshot.failures snapshot with
      | first :: rest -> Scope_outcome.Cleanup.Failed (first, rest)
      | [] ->
        if List.is_empty captured.callbacks_rev
        then Scope_outcome.Cleanup.Never_acquired
        else Joined
    in
    let diagnostics =
      List.map selections ~f:(fun selected ->
        match selected.Cleanup_evidence.Selection.source with
        | Evidence (Reported_actual (diagnostic, _) | Reported_prior (diagnostic, _)) ->
          diagnostic
        | Callback _
        | Evidence
            ( Original
            | Retired_actual _
            | Retired_prior _
            | Native_actual _
            | Initializer_end _ ) ->
          invalid_arg "direct callback projection lost its actual reported origin")
    in
    let origin =
      match diagnostics with
      | [] -> None
      | primary :: _ -> Some { Physical_origin.diagnostics; primary = Some primary }
    in
    Cleanup_projection.create cleanup ~boundary_diagnostics:diagnostics ~origin
  ;;

  let original_initialization_cleanup t =
    Cleanup_projection.cleanup (original_initialization_projection t)
  ;;

  let original_projection (t : t) =
    Owner_affinity.check_domain_exn t.affinity;
    match t.phase with
    | Original_running _ -> Cleanup_projection.empty
    | Complete { original_projection; _ }
    | Retrying { before = { original_projection; _ }; _ } -> original_projection
  ;;

  let prepare_original (t : t) ?projection ~completion original =
    Owner_affinity.check_domain_exn t.affinity;
    match t.phase with
    | Complete _ | Retrying _ ->
      invalid_arg "cleanup holder original report is already complete"
    | Original_running captured ->
      let current = initialize_current t captured in
      let latest =
        Observation.completed
          { captured with observing = Some current }
          ~kind:t.kind
          ~completion
      in
      (* Only this holder's own Init diagnostic uncertainty is sticky. Ordinary
         running child holders remain exact edges and may finish normally later. *)
      t.original_initializers_sealed
      <- List.for_all
           captured.initializations_rev
           ~f:Initialization_capture_data.original_diagnostics_sealed;
      let original_projection =
        Option.value projection ~default:(Cleanup_projection.create original ~origin:None)
      in
      { original; original_projection; captured; completion; current; latest }
  ;;

  let finish_original (t : t) ?projection ~completion original =
    let completed = prepare_original t ?projection ~completion original in
    t.phase <- Complete completed
  ;;

  (* Invoked once by the actual retirement producer, never by an observer.
     An unavailable selected report is a real final-publication CONTROL refusal;
     it is not a physical receipt. The canonical latest stays unavailable, so
     custody/metadata guards cannot turn this refusal into successful disposal.
     Reads and old handles reuse the stored outcome and never issue it again. *)
  let validate_final_retirement_report ~actual ~current ~completion_ready =
    match Agent_session.Close_observation.exit actual with
    | Returned (Error _) | Raised _ -> actual
    | Returned (Ok ()) ->
      (match current with
       | Ok current ->
         (match Agent_session.Close_observation.exit current with
          | Returned (Error _) | Raised _ -> current
          | Returned (Ok ()) when completion_ready -> current
          | Returned (Ok ()) ->
            Physical_origin.opaque_result (fun () ->
              Error
                (Agent_protocol.Error.create
                   Conflict
                   ~message:"retirement report has not released its actual custody"
                   ~retryable:true
                   ())))
       | Error error -> Physical_origin.opaque_result (fun () -> Error error))
  ;;

  (* Physical work and diagnostic selection are distinct. This producer chooses
     its final raw exit only after the actual retirement stages have returned.
     No intermediate Complete record or child result is published/rewritten. *)
  let finish_retirement_original (t : t) ~completion ~actual =
    let module CO = Agent_session.Close_observation in
    (match t.kind, completion with
     | Kind.Retirement, Original_completion.Retirement _ -> ()
     | ( (Lease | Source _ | Construction | Retirement)
       , (Lease _ | Stage _ | Administration_transition | Construction | Retirement _) )
       -> invalid_arg "retirement finalization requires its actual retirement holder");
    let projection observation =
      Cleanup_projection.of_primary
        ~origin:(Physical_origin.of_observation observation)
        ~acquired:true
        (Physical_origin.primary observation)
    in
    let initial = projection actual in
    let completed =
      prepare_original
        t
        ~projection:initial
        ~completion
        (Cleanup_projection.cleanup initial)
    in
    let evidence =
      Observation.recipe
        { completed.captured with observing = Some completed.current }
        ~kind:t.kind
        ~completion:(Some completion)
    in
    let completion_ready =
      Cleanup_evidence.custody_joined evidence
      && Original_cleanup_report.diagnostics_complete
           (Cleanup_evidence.child_view evidence ~diagnostics_complete:true)
    in
    let selected =
      validate_final_retirement_report
        ~actual
        ~current:(Cleanup_snapshot.observation completed.latest)
        ~completion_ready
    in
    let original_projection = projection selected in
    let diagnostics =
      CO.diagnostics actual
      @ CO.diagnostics selected
      @ Option.value_map
          (Result.ok (Cleanup_snapshot.observation completed.latest))
          ~default:[]
          ~f:CO.diagnostics
    in
    let selected =
      CO.create (CO.exit selected) ~diagnostics ?primary:(CO.primary selected) ()
    in
    let original_projection =
      Cleanup_projection.create
        ~boundary_diagnostics:diagnostics
        (Cleanup_projection.cleanup original_projection)
        ~origin:(Some (Physical_origin.of_observation selected))
    in
    t.phase
    <- Complete
         { completed with
           original = Cleanup_projection.cleanup original_projection
         ; original_projection
         };
    selected
  ;;

  let original_diagnostics (t : t) =
    Cleanup_evidence.child_view (Observation.scope t) ~diagnostics_complete:true
  ;;

  let original_diagnostics_complete (t : t) =
    Original_cleanup_report.diagnostics_complete (original_diagnostics t)
  ;;

  let is_finished (t : t) =
    Owner_affinity.check_domain_exn t.affinity;
    match t.phase with
    | Original_running _ -> false
    | Complete _ | Retrying _ -> true
  ;;

  (* Called only by the external dependency join after authentic consumer
     selection. No effects or custody are inferred from the report slot. *)
  let relation_consumer_observation holder =
    let open Result.Let_syntax in
    let%bind guard = Relation_guard.capture holder in
    let evidence = Observation.scope holder in
    match Relation_guard.check guard with
    | Current -> Ok (guard, evidence)
    | Busy | Stale ->
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"consumer cleanup changed during its observation"
           ~retryable:true
           ())
  ;;

  module Reservation = struct
    type recovery = t

    type t =
      { root : reserved
      ; scopes : reserved list
      }

    let refused message =
      Error (Agent_protocol.Error.create Conflict ~message ~retryable:false ())
    ;;

    let children = captured_children

    let restore t =
      List.iter t.scopes ~f:(fun reserved ->
        match reserved.holder.phase with
        | Retrying current when phys_equal current reserved ->
          reserved.holder.phase <- Complete reserved.before
        | Original_running _ | Complete _ | Retrying _ -> ())
    ;;

    let reserve (holder : recovery) ~affinity ~after =
      let installed = ref [] in
      let rec select path (holder : recovery) =
        if not (Owner_affinity.same affinity holder.affinity)
        then refused "cleanup dependency belongs to another Owner"
        else if List.mem path holder ~equal:phys_equal
        then invalid_arg "original cleanup dependencies contain a cycle"
        else (
          match
            List.find !installed ~f:(fun reserved -> phys_equal reserved.holder holder)
          with
          | Some reserved -> Ok (Some reserved)
          | None ->
            (match holder.phase with
             | Original_running _ -> Ok None
             | Retrying _ -> refused "cleanup dependency is already reserved"
             | Complete before ->
               if Cleanup_snapshot.equal_progress before.latest.progress Joined
               then Ok None
               else (
                 let reserved = { holder; before; executed = false } in
                 holder.phase <- Retrying reserved;
                 installed := reserved :: !installed;
                 let open Result.Let_syntax in
                 let%map () =
                   List.fold_result
                     (children before.captured)
                     ~init:()
                     ~f:(fun () child ->
                       Result.map (select (holder :: path) child) ~f:(fun _ -> ()))
                 in
                 Some reserved)))
      in
      match holder.phase with
      | Original_running _ -> refused "cleanup original scope is still running"
      | Retrying _ -> refused "cleanup is already reserved"
      | Complete before ->
        if not (phys_equal before.latest after)
        then refused "cleanup snapshot is stale"
        else if Cleanup_snapshot.equal_progress before.latest.progress Joined
        then refused "cleanup is already joined"
        else (
          let outcome =
            try select [] holder with
            | exn ->
              let backtrace = Stdlib.Printexc.get_raw_backtrace () in
              List.iter !installed ~f:(fun reserved ->
                reserved.holder.phase <- Complete reserved.before);
              Exn.raise_with_original_backtrace exn backtrace
          in
          match outcome with
          | Ok (Some root) -> Ok { root; scopes = List.rev !installed }
          | Ok None -> invalid_arg "cleanup root reservation disappeared"
          | Error error ->
            List.iter !installed ~f:(fun reserved ->
              reserved.holder.phase <- Complete reserved.before);
            Error error)
    ;;

    let find t holder =
      List.find t.scopes ~f:(fun reserved -> phys_equal reserved.holder holder)
    ;;

    let after t = t.root.before.latest

    let publish t =
      (* Calculate every fresh immutable view before releasing any reservation.
         Observation recursively uses exact current objects, never child.latest. *)
      let publications =
        List.filter_map t.scopes ~f:(fun reserved ->
          if reserved.executed || phys_equal reserved t.root
          then (
            let latest = Observation.snapshot (Observation.scope reserved.holder) in
            Some (reserved, { reserved.before with latest }))
          else None)
      in
      List.iter publications ~f:(fun (reserved, completed) ->
        reserved.holder.phase <- Complete completed);
      restore t
    ;;
  end
end

module Background_lease = struct
  module Phase = struct
    type t =
      | Active
      | Detached
  end

  module Provider_basis = struct
    type t =
      | Lifetime_only
      | Binding of Runtime_binding.t
      | Restored of Restored_policy_anchor.t
      | Native of Native_loan.t
  end

  module Relation = struct
    type 'consumer t =
      { consumer : 'consumer
      ; basis : Provider_basis.t
      ; report : Recovery_data.relation_guard Relation_evidence.t
      ; mutable pin : Native_loan.t option
      }

    let create ~consumer ~basis ~mode =
      { consumer
      ; basis
      ; report =
          (match mode with
           | Relation_evidence.Mode.Native -> Relation_evidence.native ()
           | Ordinary -> Relation_evidence.ordinary ())
      ; pin = None
      }
    ;;

    let attach_pin t pin =
      match t.pin with
      | None -> t.pin <- Some pin
      | Some _ -> invalid_arg "native relation pin was already captured"
    ;;
  end

  type 'consumer t =
    { recovery : Recovery_data.t
    ; cancel : unit -> unit
    ; finished : unit Eio.Promise.t
    ; finish : unit Eio.Promise.u
    ; survives_stop : bool
    ; mutable purpose : Lease_purpose.t
    ; mutable resources : Agent_session.Runtime_builder.t option
    ; mutable phase : Phase.t
    ; mutable relation : 'consumer Relation.t option
    }

  let is_active t =
    match t.phase with
    | Active -> true
    | Detached -> false
  ;;
end

module Source_extent = struct
  module Origin = Source_origin

  type t =
    { finished : Cleanup_projection.t Eio.Promise.t
    ; finish : Cleanup_projection.t Eio.Promise.u
    ; mutable projection : Cleanup_projection.t
    ; recovery : Recovery_data.t
    ; association : Source_resource_association.t
    }

  let create ~affinity ~origin =
    let finished, finish = Eio.Promise.create () in
    let recovery = Recovery_data.create ~affinity ~kind:(Source origin) in
    { finished
    ; finish
    ; projection = Cleanup_projection.empty
    ; recovery
    ; association = Option.value_exn recovery.source_association
    }
  ;;

  let record_batch t ~role batch =
    if Option.is_some (Eio.Promise.peek t.finished)
    then invalid_arg "source cleanup batch is already complete";
    Recovery_data.record_batch t.recovery ~role batch
  ;;

  let record_projection t projection =
    if Option.is_some (Eio.Promise.peek t.finished)
    then invalid_arg "source cleanup report is already complete";
    t.projection <- Cleanup_projection.combine t.projection projection
  ;;

  let finish t ~completion ~projection =
    record_projection t projection;
    record_projection t (Recovery_data.original_initialization_projection t.recovery);
    (* These are aliases of the actual retained producer groups, not a new
       flattened diagnostic. Existing recipe ownership wins; a genuinely opaque
       original boundary is retained once at this final lexical position. *)
    let projection =
      { t.projection with
        boundary_diagnostics =
          t.projection.boundary_diagnostics
          @ Option.value_map t.projection.origin ~default:[] ~f:(fun origin ->
            origin.Physical_origin.diagnostics)
      }
    in
    let cleanup = Cleanup_projection.cleanup projection in
    Recovery_data.finish_original t.recovery ~projection ~completion cleanup;
    Eio.Promise.resolve t.finish projection;
    cleanup
  ;;
end

module Source_stage_binding = struct
  type t =
    | Delegated_load of Agent_session.Session_actor.Delegated_load.Source_binding.t
    | Initial of Agent_session.Session_actor.Source_binding.t
    | Existing_goal of Agent_session.Session_actor.Goal_continuation.Source_binding.t
    | Existing_goal_execution of
        Agent_session.Session_actor.Goal_execution_runtime.Source_binding.t

  let close = function
    | Delegated_load binding ->
      Agent_session.Session_actor.Delegated_load.Source_binding.close binding
    | Initial binding -> Agent_session.Session_actor.Source_binding.close binding
    | Existing_goal binding ->
      Agent_session.Session_actor.Goal_continuation.Source_binding.close binding
    | Existing_goal_execution binding ->
      Agent_session.Session_actor.Goal_execution_runtime.Source_binding.close binding
  ;;
end

(* Administration previously retained the Owner mutex through its committed
   initialization suffix. Its same source stage now carries that exclusion while
   source work runs outside the mutex. Completion is only a local join point,
   never a successful cleanup/Store acknowledgement or execution permission. *)
module Source_stage_retirement = struct
  type t =
    | Cancellable
    | Administration of
        { finished : unit Eio.Promise.t
        ; finish : unit Eio.Promise.u
        }

  let administration () =
    let finished, finish = Eio.Promise.create () in
    Administration { finished; finish }
  ;;

  let completion = function
    | Cancellable -> None
    | Administration { finished; _ } -> Some finished
  ;;

  let complete = function
    | Cancellable -> ()
    | Administration { finished; finish } ->
      if Option.is_none (Eio.Promise.peek finished) then Eio.Promise.resolve finish ()
  ;;
end

module Private_lease_callbacks = struct
  let check_open recovery = Recovery_data.callback_registration_status recovery
  let capture recovery completion = Recovery_data.register_callback recovery completion
end

type source_stage =
  { domain : int
  ; runtime : Runtime_binding.t option
  ; resource_handoff : Resource_handoff.t option
  ; foreground_retirement : Agent_session.Session_actor.Runtime_retirement.t option
  ; retirement : Source_stage_retirement.t
  ; extent : Source_extent.t
  ; mutable cancel_lease : (unit -> unit) option
  ; mutable restored_anchor : Restored_policy_anchor.t option
  ; mutable policy_construction : Agent_session.Session_actor.Policy_restoration.t option
  ; mutable native_construction : Agent_session.Session_actor.Native_setup.t option
  ; mutable native_anchor : Native_loan.t option
  ; mutable load_preparation :
      Agent_session.Session_actor.Delegated_load.Preparation.t option
  ; mutable binding : Source_stage_binding.t option
  ; mutable continuation : Agent_session.Session_actor.Runtime_construction.t option
  ; mutable closed : bool
  ; mutable retire_installed : Agent_session.Runtime_builder.t option
  ; mutable handoff_runtime : Agent_session.Runtime_builder.t option
  }

let close_source_stage stage =
  stage.closed <- true;
  Option.iter stage.native_construction ~f:Agent_session.Session_actor.Native_setup.close;
  Option.iter
    stage.policy_construction
    ~f:Agent_session.Session_actor.Policy_restoration.close;
  Option.iter
    stage.load_preparation
    ~f:Agent_session.Session_actor.Delegated_load.Preparation.close;
  Option.iter stage.binding ~f:Source_stage_binding.close;
  Option.iter stage.continuation ~f:(fun continuation ->
    Agent_session.Session_actor.Runtime_construction.close continuation)
;;

let check_prepared_stage stage ~basis =
  if not (Int.equal stage.domain (Stdlib.Domain.self () :> int))
  then
    Error
      (Agent_protocol.Error.invalid_request
         "prepared source belongs to another Eio domain")
  else if stage.closed || Option.is_some stage.continuation
  then
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"prepared source lifetime is closed or consumed"
         ~retryable:false
         ())
  else basis ()
;;

module Prepared = struct
  type t =
    { stage : source_stage
    ; binding : Agent_session.Session_actor.Source_binding.t
    ; preparation : Agent_session.Run_preparation.t
    ; basis : unit -> (unit, Agent_protocol.Error.t) result
    }

  let binding t = t.binding

  let check_basis t =
    let open Result.Let_syntax in
    let%bind () = check_prepared_stage t.stage ~basis:t.basis in
    Agent_session.Session_actor.Source_binding.check_basis t.binding
  ;;
end

module Prepared_goal_continuation = struct
  type t =
    { stage : source_stage
    ; binding : Agent_session.Session_actor.Goal_continuation.Source_binding.t
    ; basis : unit -> (unit, Agent_protocol.Error.t) result
    }

  let binding t = t.binding

  let check_basis t =
    let open Result.Let_syntax in
    let%bind () = check_prepared_stage t.stage ~basis:t.basis in
    Agent_session.Session_actor.Goal_continuation.Source_binding.check_basis t.binding
  ;;
end

module Prepared_goal_execution = struct
  type t =
    { stage : source_stage
    ; binding : Agent_session.Session_actor.Goal_execution_runtime.Source_binding.t
    ; basis : unit -> (unit, Agent_protocol.Error.t) result
    }

  let binding t = t.binding

  let check_basis t =
    let open Result.Let_syntax in
    let%bind () = check_prepared_stage t.stage ~basis:t.basis in
    Agent_session.Session_actor.Goal_execution_runtime.Source_binding.check_basis
      t.binding
  ;;
end

module Source_consumer = struct
  type t =
    { stage : source_stage
    ; cancel : unit -> unit
    }

  let check_domain t =
    if Int.equal t.stage.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "source consumer belongs to another Eio domain")
  ;;

  let check t =
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    if t.stage.closed
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"source consumer lifetime is closed"
           ~retryable:false
           ())
    else Ok ()
  ;;

  let create stage =
    let open Result.Let_syntax in
    match stage.cancel_lease with
    | Some cancel ->
      let t = { stage; cancel } in
      let%map () = check t in
      t
    | None ->
      Error
        (Agent_protocol.Error.invalid_request
           "source consumer requires its registered source lease")
  ;;

  let revoke t =
    let open Result.Let_syntax in
    let%map () = check_domain t in
    close_source_stage t.stage
  ;;

  let join t =
    let open Result.Let_syntax in
    let%map () = revoke t in
    t.cancel ();
    Eio.Promise.await t.stage.extent.finished |> Cleanup_projection.cleanup
  ;;

  let cleanup t = t.stage.extent.recovery
end

module Prepared_delegated_load = struct
  type t =
    { stage : source_stage
    ; binding : Agent_session.Session_actor.Delegated_load.Source_binding.t
    ; basis : unit -> (unit, Agent_protocol.Error.t) result
    }

  let binding t = t.binding

  let check_basis t =
    let open Result.Let_syntax in
    let%bind () = check_prepared_stage t.stage ~basis:t.basis in
    Agent_session.Session_actor.Delegated_load.Source_binding.check_basis t.binding
  ;;
end

module Delegated_load_source = struct
  type t =
    { consumer : Source_consumer.t
    ; prepare_source :
        source:Agent_session.Runtime_builder.Source.t
        -> (Prepared_delegated_load.t, Agent_protocol.Error.t) result
    }

  let consumer t = t.consumer
  let prepare t ~source = t.prepare_source ~source
end

module Delegated_load_retention = struct
  type t =
    | Keep_loaded
    | Release_if_stopped
end

type delegated_loader =
  source:Delegated_load_source.t
  -> retention:Delegated_load_retention.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

module Delegated_loader = struct
  type t =
    | Legacy of delegated_loader
    | Observed of
        (source:Delegated_load_source.t
         -> retention:Delegated_load_retention.t
         -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result
              Agent_session.Close_observation.t)

  let legacy f = Legacy f
  let observed f = Observed f
end

(* This is the SAME actual issuer received from Factory creation. Keeping it in
   its Factory-owned driver does not create or publish another issuer. *)
type delegated_driver =
  { source_owner : Agent_session.Runtime_source_owner.t
  ; run : Delegated_loader.t
  }

module Cleanup_request = struct
  type t =
    { mutable entries : Retired_resource.t list
    ; mutable initialization_recovery : Recovery_data.t option
    }

  let create () = { entries = []; initialization_recovery = None }

  let attach_initialization_recovery t recovery =
    match t.initialization_recovery with
    | None -> t.initialization_recovery <- Some recovery
    | Some original when phys_equal original recovery -> ()
    | Some _ -> invalid_arg "cleanup request initializer owner was reassigned"
  ;;

  let add t entry =
    if not (List.mem t.entries entry ~equal:phys_equal)
    then t.entries <- t.entries @ [ entry ]
  ;;
end

module Administration_transition = struct
  type phase =
    | Live
    | Retired

  type t =
    { mutable phase : phase
    ; extent : Source_extent.t
    }

  let create ~affinity =
    { phase = Live
    ; extent =
        Source_extent.create
          ~affinity
          ~origin:Source_extent.Origin.Administration_transition
    }
  ;;

  let retire t = t.phase <- Retired

  let is_live t =
    match t.phase with
    | Live -> true
    | Retired -> false
  ;;
end

type unload_outcome =
  ((unit, Agent_protocol.Error.t) result, exn * Stdlib.Printexc.raw_backtrace) result

exception Cleanup_failed of Agent_protocol.Error.t

module Construction_result = struct
  type 'a t =
    | Built of 'a
    | Callback_completed of Agent_session.Session_actor.Goal_callback_completion.t
end

let ordinary_construction_result = function
  | Construction_result.Built value -> Ok value
  | Callback_completed _ ->
    Error
      (Agent_protocol.Error.create
         Invalid_state
         ~message:"Goal callback completion reached an ordinary runtime adapter"
         ~retryable:false
         ())
;;

type deferred_builder =
  initialization_cleanup:Initialization_capture.t
  -> source_consumer:Source_consumer.t
  -> source:Agent_session.Runtime_builder.Source.t
  -> expected_product:Agent_session.Runtime_builder.Product_identity.t option
  -> construction:Agent_session.Session_actor.Runtime_construction.t
  -> ( Agent_session.Runtime_builder.Attachment.t Construction_result.t
       , Agent_protocol.Error.t )
       result

module Ordinary_warm = struct
  type selection =
    | No_retained_product
    | Attached of
        { attachment : Agent_session.Runtime_builder.Attachment.t
        ; expected : Agent_session.Session_actor.Ordinary_attachment_basis.t
        }

  type prepare =
    expected_product:Agent_session.Runtime_builder.Product_identity.t option
    -> original_actor:Operator_authorization.t option
    -> run_preparation:Agent_session.Run_preparation.t option
    -> (selection, Agent_protocol.Error.t) result
end

module Policy_restoration_source = struct
  module Publication = struct
    module Custody = struct
      type t =
        | Caller_retains
        | Owner_retains
    end

    type t =
      { custody : Custody.t
      ; primary : unit Scope_outcome.primary
      }

    let custody t = t.custody

    let resume t =
      match t.primary with
      | Scope_outcome.Returned result -> result
      | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
    ;;
  end

  type t =
    { consumer : Source_consumer.t
    ; check : unit -> (unit, Agent_protocol.Error.t) result
    ; prepare :
        source_owner:Agent_session.Runtime_source_owner.t
        -> source:Agent_session.Runtime_builder.Source.t
        -> basis:Agent_session.Policy_restore_basis.t
        -> ( Agent_session.Session_actor.Policy_restoration.t
             , Agent_protocol.Error.t )
             result
    ; publish :
        restoration:Agent_session.Session_actor.Policy_restoration.t
        -> loan:Session_policy_resource.Lease.t
        -> access:Agent_session.Runtime_builder.Restored_policy_access.t
        -> Publication.t
    }

  let consumer t = t.consumer
  let check t = t.check ()
  let prepare t ~source_owner ~source ~basis = t.prepare ~source_owner ~source ~basis
  let publish t ~restoration ~loan ~access = t.publish ~restoration ~loan ~access
end

module Native_resource_origin = struct
  module Route = struct
    type t =
      | Direct of Agent_store.Delegation_store.Reference.t
      | Ancestry of Agent_session.Delegation_authority.Resource_path.Prefix.t
  end

  module Validation = struct
    type t =
      | Direct
      | Ancestry of Agent_session.Delegation_authority.Resource_path.Observation.t
  end

  type t =
    { source_owner : Agent_session.Runtime_source_owner.t
    ; route : Route.t
    ; authorize :
        route:Route.t
        -> target:Agent_session.Session_state.t
        -> (Validation.t, Agent_protocol.Error.t) result
    }

  (* Construction is DATA-only. In particular, issuer/domain validation remains
     inside the actual Owner admission boundary, including for Direct adapters. *)
  let create ~source_owner ~route ~authorize = { source_owner; route; authorize }
  let route t = t.route

  let reference t =
    match t.route with
    | Direct reference -> reference
    | Ancestry prefix ->
      Agent_session.Delegation_authority.Resource_path.Prefix.reference prefix
  ;;

  let check_issuer t ~source_owner =
    let module S = Agent_session.Runtime_source_owner in
    let module Path = Agent_session.Delegation_authority.Resource_path in
    if
      S.matches source_owner (S.identity t.source_owner)
      &&
      match t.route with
      | Direct _ -> true
      | Ancestry prefix -> Path.issued_by (Path.Prefix.path prefix) ~source_owner
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "native resource origin belongs to another source owner")
  ;;

  let check_validation t validation ~(expected : Agent_session.Session_state.t) =
    let module Path = Agent_session.Delegation_authority.Resource_path in
    let module D = Agent_store.Delegation_store in
    let module P = Agent_protocol in
    let open Result.Let_syntax in
    let%bind () = check_issuer t ~source_owner:t.source_owner in
    match t.route, validation with
    | Direct _, Validation.Direct -> Ok ()
    | Ancestry prefix, Validation.Ancestry observation ->
      let%bind () =
        Path.Observation.check_prefix observation ~source_owner:t.source_owner ~prefix
      in
      let record = Path.Observation.record observation in
      let actual = Path.Observation.state observation in
      if
        D.Reference.equal (D.reference record) (Path.Prefix.reference prefix)
        && P.Id.Session.equal record.key.parent_session_id expected.identity.session_id
        && Int.equal record.key.parent_generation expected.identity.generation
        && P.Id.Session.equal actual.identity.session_id expected.identity.session_id
        && Int.equal actual.identity.generation expected.identity.generation
      then Ok ()
      else
        Error
          (P.Error.invalid_request
             "native resource observation differs from its actual target")
    | Direct _, Validation.Ancestry _ | Ancestry _, Validation.Direct ->
      Error (P.Error.invalid_request "native resource validation differs from its route")
  ;;

  let parent t =
    match t.route with
    | Direct _ -> Ok None
    | Ancestry prefix ->
      Result.map
        (Agent_session.Delegation_authority.Resource_path.Prefix.parent prefix)
        ~f:(Option.map ~f:(fun prefix -> { t with route = Ancestry prefix }))
  ;;
end

module Native_resource_admission = struct
  type t =
    { domain : int
    ; origin : Native_resource_origin.t
    ; session_id : Agent_protocol.Id.Session.t
    ; generation : int
    ; current : unit -> (unit, Agent_protocol.Error.t) result
    ; mutable closed : bool
    }

  let origin t = t.origin
  let reference t = Native_resource_origin.reference t.origin

  let observe t ~(expected : Agent_session.Session_state.t) =
    let open Result.Let_syntax in
    let%bind () =
      if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
      then
        Error
          (Agent_protocol.Error.invalid_request
             "native resource admission belongs to another Eio domain")
      else if
        t.closed
        || (not
              (Agent_protocol.Id.Session.equal t.session_id expected.identity.session_id))
        || not (Int.equal t.generation expected.identity.generation)
      then
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"native resource consumer admission is no longer current"
             ~retryable:false
             ())
      else t.current ()
    in
    (* The per-consumer original guard may perform IO. Never call it while the
       Owner mutex or Actor mailbox is held. Recheck our extent after the wait. *)
    let%bind validation = t.origin.authorize ~route:t.origin.route ~target:expected in
    let%bind () = Native_resource_origin.check_validation t.origin validation ~expected in
    let%map () =
      if t.closed
      then
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"native resource consumer admission ended during authorization"
             ~retryable:false
             ())
      else t.current ()
    in
    validation
  ;;

  let check t ~expected = Result.map (observe t ~expected) ~f:(fun _ -> ())
end

module Native_source_stage = struct
  module Publication = Policy_restoration_source.Publication

  type t =
    { consumer : Source_consumer.t
    ; check : unit -> (unit, Agent_protocol.Error.t) result
    ; admission : Native_resource_admission.t
    ; prepare :
        source_owner:Agent_session.Runtime_source_owner.t
        -> source:Agent_session.Runtime_builder.Source.t
        -> expected:Agent_session.Session_state.t
        -> (Agent_session.Session_actor.Native_setup.t, Agent_protocol.Error.t) result
    ; publish :
        setup:Agent_session.Session_actor.Native_setup.t
        -> loan:Session_policy_resource.Native_lease.t
        -> Publication.t
    }

  let consumer t = t.consumer
  let check t = t.check ()
  let admission t = t.admission

  let prepare t ~source_owner ~source ~expected =
    t.prepare ~source_owner ~source ~expected
  ;;

  let publish t ~setup ~loan = t.publish ~setup ~loan
end

module Native_resource_access = struct
  type custody_check =
    | Resources
    | Retained_source of Agent_session.Runtime_builder.Source.t

  type preparation =
    | Raw of
        (admission:Native_resource_admission.t
         -> stage:Native_source_stage.t
         -> (unit, Agent_protocol.Error.t) result)
    | Observed of
        (admission:Native_resource_admission.t
         -> stage:Native_source_stage.t
         -> (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t)

  type t =
    { prepare : preparation
    ; pin :
        admission:Native_resource_admission.t
        -> expected_native:Agent_session.Runtime_builder.Native_identity.t
        -> (Session_policy_resource.Native_lease.t, Agent_protocol.Error.t) result
    ; check :
        Session_policy_resource.Native_lease.t
        -> admission:Native_resource_admission.t
        -> expected:Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result
    ; check_custody :
        Session_policy_resource.Native_lease.t
        -> custody_check
        -> (unit, Agent_protocol.Error.t) result
    }

  let create ~prepare ~pin ~check ~check_custody =
    { prepare = Raw prepare; pin; check; check_custody }
  ;;

  let create_observed ~prepare ~pin ~check ~check_custody =
    { prepare = Observed prepare; pin; check; check_custody }
  ;;
end

module Policy_resource_access = struct
  type loan = Session_policy_resource.Lease.t

  type restoration =
    { restore :
        stage:Policy_restoration_source.t
        -> request:Agent_session.Script_tool_calls.preparation
        -> governance:Agent_session.Session_actor.Causal_governance.t
        -> authorize:(unit -> (unit, Agent_protocol.Error.t) result)
        -> (unit, Agent_protocol.Error.t) result
    ; check_restored :
        loan
        -> access:Agent_session.Runtime_builder.Restored_policy_access.t
        -> expected:Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result
    }

  type t =
    { restoration : restoration option
    ; pin :
        expected_product:Agent_session.Runtime_builder.Product_identity.t
        -> (loan, Agent_protocol.Error.t) result
    ; prepare_access :
        loan
        -> expected:Agent_session.Session_state.t
        -> (Agent_session.Runtime_builder.Policy_access.t, Agent_protocol.Error.t) result
    }

  let create ~pin ~prepare_access = { pin; prepare_access; restoration = None }

  let with_restoration t ~restore ~check_restored =
    { t with restoration = Some { restore; check_restored } }
  ;;
end

module Policy_claim_holder = struct
  type 'consumer t =
    { capture : Agent_session.Session_actor.Delegated_policy_claim.Capture.t
    ; loan : Session_policy_resource.Lease.t
    ; runtime : Agent_session.Runtime_builder.t
    ; wrapper : 'consumer Background_lease.t
    ; natural_release : Policy_claim_release.t
    }
end

type builder =
  | Ordinary of (unit -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  | For_runs of
      (Agent_session.Run_preparation.t option
       -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  | For_authenticated_runs of
      (original_actor:Operator_authorization.t option
       -> Agent_session.Run_preparation.t option
       -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  | For_factory of
      (initialization_cleanup:Initialization_capture.t
       -> original_actor:Operator_authorization.t option
       -> Agent_session.Run_preparation.t option
       -> (Agent_session.Runtime_builder.Attachment.t, Agent_protocol.Error.t) result)

module Originated_retirement = struct
  type t =
    { outcome : unload_outcome
    ; origin : Physical_origin.t option
    }

  let of_observation actual =
    { outcome = Physical_origin.native actual
    ; origin = Some (Physical_origin.of_observation actual)
    }
  ;;

  let opaque outcome =
    Physical_origin.capture_result (fun () ->
      Agent_session.Close_observation.create
        (match outcome with
         | Ok result -> Returned result
         | Error (exn, trace) -> Raised (exn, trace))
        ~diagnostics:[]
        ())
    |> of_observation
  ;;

  (* Pure projection of the actual result and its paired producer. These helpers
     neither execute a callback nor recover an origin from a nearby report. *)
  let select_primary ?origin = function
    | Scope_outcome.Returned (Ok value) -> Ok value
    | Returned (Error error) ->
      Error
        (match origin with
         | Some _ -> { outcome = Ok (Error error); origin }
         | None -> opaque (Ok (Error error)))
    | Raised (exn, trace) ->
      Error
        (match origin with
         | Some _ -> { outcome = Error (exn, trace); origin }
         | None -> opaque (Error (exn, trace)))
  ;;

  let select actual =
    select_primary
      ~origin:(Physical_origin.of_observation actual)
      (Physical_origin.primary actual)
  ;;

  let origin_of_diagnostic diagnostic =
    Option.map diagnostic ~f:(fun primary ->
      { Physical_origin.diagnostics = [ primary ]; primary = Some primary })
  ;;

  let select_step step primary =
    select_primary
      ?origin:
        (Cleanup_projection.origin (Original_cleanup_step.projection step ~acquired:true))
      primary
  ;;

  let project t =
    match t.outcome with
    | Ok result -> result
    | Error (exn, trace) -> Exn.raise_with_original_backtrace exn trace
  ;;
end

module Retirement_coverage = struct
  type cell =
    { target : Session_policy_resource.Cleanup_target.t
    ; source : Source_consumer.t option
    ; mutable facets : Session_policy_resource.Cleanup_target.Facets.t
    }

  type t =
    | Whole_owner
    | Cell of cell

  let is_whole_owner = function
    | Whole_owner -> true
    | Cell _ -> false
  ;;

  let equal first second =
    match first, second with
    | Whole_owner, Whole_owner -> true
    | Cell first, Cell second ->
      Session_policy_resource.Cleanup_target.equal first.target second.target
    | Whole_owner, Cell _ | Cell _, Whole_owner -> false
  ;;
end

module Resource_retirement_data = struct
  type strength = Original_completion.Retirement.Required.t =
    | Ordinary
    | All_resources

  type phase =
    | Queued
    | Running
    | Finished of unload_outcome

  type t =
    { mutex : Eio.Mutex.t
    ; domain : int
    ; recovery : Recovery_data.t
    ; coverage : Retirement_coverage.t
    ; retry_source : Recovery_data.t option
    ; latest_repair : t option ref
    ; mutable strength : strength
    ; mutable phase : phase
    ; mutable delivered : bool
    ; finished : Originated_retirement.t Eio.Promise.t
    ; finish : Originated_retirement.t Eio.Promise.u
    ; mutable perform : (t -> unit) option
    ; mutable predecessor : t option
    ; mutable source_extent : Source_extent.t option
    }

  let create
        ~mutex
        ~domain
        ~affinity
        ~coverage
        ~repair_of
        ~retry_source
        ~strength
        ~perform
    =
    let finished, finish = Eio.Promise.create () in
    { mutex
    ; domain
    ; recovery =
        Recovery_data.create_with_completion_requirement
          ~affinity
          ~kind:Retirement
          ~completion_requirement:
            (match coverage with
             | Retirement_coverage.Whole_owner -> Legacy
             | Cell _ -> Canonical_custody)
    ; coverage
    ; retry_source
    ; latest_repair =
        (match repair_of with
         | None -> ref None
         | Some original -> original.latest_repair)
    ; strength
    ; phase = Queued
    ; delivered = false
    ; finished
    ; finish
    ; perform = Some perform
    ; predecessor = None
    ; source_extent = None
    }
  ;;

  (* One shared link names the latest accepted repair of this exact attempt.
     No observer searches the current Owner slots or follows a chain of old failures. *)
  let publish_repair successor = successor.latest_repair := Some successor

  let latest_retry t =
    Option.filter !(t.latest_repair) ~f:(fun latest -> not (phys_equal t latest))
  ;;

  let is_success = function
    | Ok (Ok ()) -> true
    | Ok (Error _) | Error _ -> false
  ;;

  let check_domain t =
    if Int.equal t.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "resource retirement belongs to another Eio domain")
  ;;

  let await_observed t =
    match check_domain t with
    | Error error -> Originated_retirement.opaque (Ok (Error error))
    | Ok () ->
      (* Observer cancellation before this actual completion is not delivery. *)
      let actual = Eio.Promise.await t.finished in
      Eio.Cancel.protect (fun () ->
        Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.delivered <- true);
        actual)
  ;;

  let await t = Originated_retirement.project (await_observed t)
end

module Resource_retirement_claim = struct
  type t =
    | Run of Resource_retirement_data.t
    | Join of Resource_retirement_data.t
end

module Resource_consumer_request = struct
  type origin =
    | External
    | Retained_native_dependency

  type t =
    | Released
    | Retirement of Resource_retirement_claim.t
end

module Joined_close_handoff = struct
  type t = { mutable attempt : Resource_retirement_data.t option }

  let create () = { attempt = None }
end

(* Naming and reply-loss state for ONE actual elected completed retirement.
   There is no callback, new producer or public receipt constructor here. *)
module Startup_settlement_request = struct
  module A = Agent_session.Session_actor

  type reply =
    { outcome : A.Failed_startup_disposal.t Scope_outcome.primary
    ; diagnostic : Disposal_report_data.Diagnostic.t option
    }

  type t =
    { attempt : Resource_retirement_data.t
    ; completed : Recovery_data.completed
    ; candidate : Recovery_data.startup_candidate
    ; actor_step : Actor_cleanup_step.t
    ; request : A.Failed_startup_request.t
    ; mutable first_reply : reply option
    }

  let refuse t =
    match t.first_reply with
    | Some { outcome = Scope_outcome.Raised (exn, trace); _ } ->
      Exn.raise_with_original_backtrace exn trace
    | Some { outcome = Returned (Error error); _ } -> raise (Cleanup_failed error)
    | None | Some { outcome = Returned (Ok _); _ } ->
      raise
        (Cleanup_failed
           (Agent_protocol.Error.create
              Conflict
              ~message:"failed startup custody settlement is still retained"
              ~retryable:true
              ()))
  ;;
end

(* This stamp names cache freshness only. A new accepted cleanup producer
   supersedes the old completed report but preserves its diagnostic aliases.
   The completion boundary and later publication compare stamps under the SAME
   Owner mutex, so work elected between those locks cannot become a cache hit. *)
module Disposal_cache : sig
  type stamp
  type t

  val create : unit -> t
  val stamp : t -> stamp
  val current : t -> Disposal_report_data.t option
  val prior : t -> Disposal_report_data.t option
  val supersede : t -> t
  val publish : t -> completed:stamp option -> Disposal_report_data.t -> t
end = struct
  type stamp = unit ref

  type state =
    | Empty
    | Current of Disposal_report_data.t
    | Superseded of Disposal_report_data.t

  type t =
    { stamp : stamp
    ; state : state
    }

  let create () = { stamp = ref (); state = Empty }
  let stamp t = t.stamp

  let current t =
    match t.state with
    | Current report -> Some report
    | Empty | Superseded _ -> None
  ;;

  let prior t =
    match t.state with
    | Empty -> None
    | Current report | Superseded report -> Some report
  ;;

  let supersede t =
    { stamp = ref ()
    ; state =
        (match t.state with
         | Empty -> Empty
         | Current report | Superseded report -> Superseded report)
    }
  ;;

  let publish t ~completed report =
    { t with
      state =
        (if Option.exists completed ~f:(phys_equal t.stamp)
         then Current report
         else Superseded report)
    }
  ;;
end

type t =
  { affinity : Owner_affinity.t
  ; actor : Agent_session.Session_actor.t
  ; runtime_source_owner : Agent_session.Runtime_source_owner.Identity.t option
  ; build : builder
  ; build_deferred : deferred_builder option
  ; ordinary_warm : Ordinary_warm.prepare option
  ; policy_resource_access : Policy_resource_access.t option
  ; native_resource_access : Native_resource_access.t option
  ; mutable native_anchor : Native_loan.t option
  ; mutable native_loans : Native_loan.t list
  ; mutable policy_claim : native_consumer Policy_claim_holder.t option
  ; mutable restored_policy : Restored_policy_anchor.t option
  ; delegated_driver : delegated_driver option
  ; before_unload : (closing:bool -> (unit, Agent_protocol.Error.t) result) option
  ; policy_resource_cleanup : Policy_resource_cleanup.t option
  ; mutex : Eio.Mutex.t
  ; moderator_work_mutex : Eio.Mutex.t
  ; mutable binding : Runtime_binding.t option
  ; mutable prepared_source : source_stage option
  ; mutable activation_handoff : bool
  ; domain : int
  ; mutable closed : bool
  ; mutable close_finished : bool
  ; mutable disposal_cache : Disposal_cache.t
  ; mutable startup_settlement_request : Startup_settlement_request.t option
  ; mutable joined_close_handoff : Joined_close_handoff.t option
  ; disposal_mutex : Eio.Mutex.t
  ; mutable foreground_retirement :
      Agent_session.Session_actor.Runtime_retirement.t option
  ; mutable joined_foreground_retirement :
      Agent_session.Session_actor.Runtime_retirement.t option
  ; mutable unloading : Resource_retirement_data.t option
  ; mutable pending_resource_retirement : Resource_retirement_data.t option
  ; mutable background_leases : native_consumer Background_lease.t list
  ; mutable retired_resources : Retired_resource.t list
  ; mutable administration_transition : Administration_transition.t option
  ; mutable failed_source_cleanup : Recovery_data.t option
  }

and resource_consumer =
  { owner : t
  ; target : Session_policy_resource.Cleanup_target.t
  ; facets : Session_policy_resource.Cleanup_target.Facets.t
  ; mutable bound_source : Source_consumer.t option
  ; mutable retirement : Resource_retirement_data.t option
  }

and native_consumer =
  | Source of
      { source : Source_consumer.t
      ; owner : t
      }
  | Resource of resource_consumer

type background_lease = native_consumer Background_lease.t

module Resource_consumer_core = struct
  type owner = t
  type t = resource_consumer

  module Status = struct
    type t =
      | Held
      | Released
    [@@deriving equal, sexp_of]
  end

  let conflict message = Agent_protocol.Error.create Conflict ~message ~retryable:false ()

  let check_target owner target =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal owner.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request
             "resource consumer belongs to another Eio domain")
    in
    match Option.bind owner.policy_resource_cleanup ~f:Policy_resource_cleanup.cell with
    | None -> Error (conflict "resource consumer requires this Owner's fixed Cell")
    | Some cell -> Session_policy_resource.Cleanup_target.check target ~cell
  ;;

  let capture owner ~target =
    let open Result.Let_syntax in
    let%bind status = check_target owner target in
    match status with
    | Held facets -> Ok { owner; target; facets; bound_source = None; retirement = None }
    | Released -> Error (conflict "resource consumer target is already released")
  ;;

  let check t =
    Result.map (check_target t.owner t.target) ~f:(function
      | Session_policy_resource.Cleanup_target.Status.Held _ -> Status.Held
      | Released -> Status.Released)
  ;;

  let bind_source t ~(source : Source_consumer.t) =
    let open Result.Let_syntax in
    (* Check BOTH immutable domains before either mutable Source or Cell read. *)
    let%bind () =
      if
        Int.equal t.owner.domain (Stdlib.Domain.self () :> int)
        && Int.equal source.stage.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request
             "source/resource association belongs to another Eio domain")
    in
    let%bind () =
      if Owner_affinity.same t.owner.affinity source.stage.extent.recovery.affinity
      then Ok ()
      else Error (conflict "source/resource association belongs to another Owner")
    in
    let%bind () = Source_consumer.check source in
    let%bind status = check_target t.owner t.target in
    match status with
    | Released -> Error (conflict "source/resource association target is released")
    | Held _ ->
      let%bind () =
        match t.bound_source with
        | None -> Ok ()
        | Some actual when phys_equal actual.stage source.stage -> Ok ()
        | Some _ -> Error (conflict "resource consumer is bound to another Source")
      in
      let%bind () =
        match source.stage.extent.association.target with
        | None -> Ok ()
        | Some actual ->
          if Session_policy_resource.Cleanup_target.equal actual t.target
          then Ok ()
          else Error (conflict "source is already associated with another resource")
      in
      (* All checks precede both writes; neither side can be partially rebound. *)
      source.stage.extent.association.target <- Some t.target;
      t.bound_source <- Some source;
      Ok ()
  ;;
end

module Native_consumer = struct
  type owner = t
  type t = native_consumer

  let resource t = Resource t
  let conflict message = Agent_protocol.Error.create Conflict ~message ~retryable:false ()

  let check = function
    | Source { source; owner } ->
      let open Result.Let_syntax in
      let%bind () =
        if Int.equal owner.domain (Stdlib.Domain.self () :> int)
        then Ok ()
        else
          Error
            (Agent_protocol.Error.invalid_request
               "consumer Owner belongs to another Eio domain")
      in
      Source_consumer.check source
    | Resource resource ->
      Result.bind (Resource_consumer_core.check resource) ~f:(function
        | Held -> Ok ()
        | Released -> Error (conflict "dependency consumer resource is already released"))
  ;;

  let check_owner consumer ~(owner : owner) =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal owner.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request
             "consumer Owner belongs to another Eio domain")
    in
    let%bind () = check consumer in
    let%bind () =
      match consumer with
      | Source { source; owner = actual } ->
        if Owner_affinity.same actual.affinity source.stage.extent.recovery.affinity
        then Ok ()
        else Error (conflict "source consumer belongs to another Owner")
      | Resource _ -> Ok ()
    in
    let affinity =
      match consumer with
      | Source { owner = actual; _ } -> actual.affinity
      | Resource resource -> resource.owner.affinity
    in
    if Owner_affinity.same affinity owner.affinity
    then Ok ()
    else Error (conflict "dependency consumer belongs to another Owner")
  ;;

  (* A provider cannot wait for its own consumer scope. Authentic dependency
     edges cross Owners; unassociated lexical Source leases are unaffected. *)
  let check_provider consumer ~(provider : owner) =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal provider.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request "provider belongs to another Eio domain")
    in
    let%bind () = check consumer in
    let consumer_affinity =
      match consumer with
      | Source { owner; _ } -> owner.affinity
      | Resource resource -> resource.owner.affinity
    in
    if Owner_affinity.same consumer_affinity provider.affinity
    then Error (conflict "dependency consumer cannot be its own provider")
    else Ok ()
  ;;

  let source ~owner source =
    let consumer = Source { source; owner } in
    let%map.Result () = check_owner consumer ~owner in
    consumer
  ;;
end

(* The whole physical engine is reused only after ALL affected custody is proven
   to belong to this exact unit. Unknown/mixed custody is a refusal, not a subset
   selector. Factual target facets may refresh only from this SAME Held target. *)
module Retirement_coverage_guard = struct
  module Cell = Session_policy_resource

  let conflict () =
    Agent_protocol.Error.create
      Conflict
      ~message:"resource retirement has mixed or unassociated Owner custody"
      ~retryable:true
      ()
  ;;

  let target cell actual =
    Cell.Cleanup_target.equal cell.Retirement_coverage.target actual
  ;;

  let binding cell actual =
    match Runtime_binding.product actual, cell.Retirement_coverage.facets.product with
    | Some actual, Some expected ->
      Agent_session.Runtime_builder.Product_identity.equal actual expected
    | Some _, None | None, (Some _ | None) ->
      (* Native provenance is usable only when the SAME captured Held unit
         actually exposes a Native facet. Ordinary Product facets never do. *)
      (match Runtime_binding.native_identity actual, cell.facets.native with
       | Some actual, Some expected ->
         Agent_session.Runtime_builder.Native_identity.equal actual expected
       | (Some _ | None), (Some _ | None) -> false)
  ;;

  let restored cell (actual : Restored_policy_anchor.t) =
    target cell (Cell.Lease.cleanup_target actual.loan)
  ;;

  let native cell (actual : Native_loan.t) =
    target cell (Cell.Native_lease.cleanup_target actual.loan)
  ;;

  let source cell (actual : Recovery_data.t) =
    match actual.source_association with
    | None -> false
    | Some association ->
      let selected =
        Option.exists association.Source_resource_association.target ~f:(target cell)
      in
      let basis =
        Option.value_map association.basis ~default:(false, false) ~f:(fun basis ->
          let known =
            Option.is_some basis.binding
            || Option.is_some basis.handoff
            || Option.is_some basis.restored
            || Option.is_some basis.native
          in
          let compatible =
            Option.for_all basis.binding ~f:(binding cell)
            && Option.for_all basis.handoff ~f:(fun handoff ->
              binding cell handoff.Resource_handoff.binding)
            && Option.for_all basis.restored ~f:(restored cell)
            && Option.for_all basis.native ~f:(native cell)
          in
          known, compatible)
      in
      let known, compatible = basis in
      (selected || known)
      && compatible
      && Option.for_all association.target ~f:(target cell)
  ;;

  let captured (holder : Recovery_data.t) =
    match holder.phase with
    | Original_running captured -> captured
    | Complete completed | Retrying { before = completed; _ } -> completed.captured
  ;;

  let source_lease recovery (lease : background_lease) =
    List.exists (captured recovery).leases_rev ~f:(fun selected ->
      match selected.Recovery_data.role with
      | Source_lexical ->
        phys_equal selected.finished lease.finished
        && phys_equal selected.recovery lease.recovery
      | Retirement_lease _ -> false)
  ;;

  let background (owner : t) cell (lease : background_lease) =
    let lexical () =
      let sources =
        Option.to_list
          (Option.map owner.prepared_source ~f:(fun stage -> stage.extent.recovery))
        @ Option.to_list
            (Option.map owner.administration_transition ~f:(fun stage ->
               stage.extent.recovery))
        @ Option.to_list owner.failed_source_cleanup
        @ Option.to_list
            (Option.map cell.Retirement_coverage.source ~f:(fun source ->
               source.stage.extent.recovery))
      in
      List.exists sources ~f:(fun recovery ->
        source cell recovery && source_lease recovery lease)
    in
    match lease.relation with
    | Some relation ->
      let basis =
        match relation.Background_lease.Relation.basis with
        | Lifetime_only -> lexical ()
        | Binding actual -> binding cell actual
        | Restored actual -> restored cell actual
        | Native actual -> native cell actual
      in
      basis && Option.for_all relation.pin ~f:(native cell)
    | None -> lexical ()
  ;;

  let actor (owner : t) cell =
    match owner.foreground_retirement with
    | None -> true
    | Some retirement
      when Agent_session.Session_actor.Runtime_retirement.is_successful retirement ->
      (* A successfully completed actual terminal checkpoint has no remaining
         Actor duty to select. Other resource/source custody still must match A.
         Merely finished negative checkpoints retain the provenance checks. *)
      true
    | Some retirement ->
      let selected recovery =
        Option.exists (captured recovery).actor_cleanup ~f:(fun step ->
          phys_equal
            retirement
            (Agent_session.Session_actor.Retirement_cleanup.retirement step.cleanup))
      in
      let stages =
        Option.to_list owner.prepared_source
        @ Option.to_list
            (Option.map cell.Retirement_coverage.source ~f:(fun source -> source.stage))
      in
      let original_source =
        List.exists stages ~f:(fun stage ->
          source cell stage.extent.recovery
          && Option.exists stage.foreground_retirement ~f:(phys_equal retirement))
      in
      let source_prefix =
        List.map stages ~f:(fun stage -> stage.extent.recovery)
        @ Option.to_list owner.failed_source_cleanup
        @ Option.to_list
            (Option.map owner.administration_transition ~f:(fun stage ->
               stage.extent.recovery))
        |> List.exists ~f:(fun recovery ->
          source cell recovery && Option.exists (captured recovery).prefix ~f:selected)
      in
      original_source
      || source_prefix
      || List.exists
           (Option.to_list owner.pending_resource_retirement
            @ Option.to_list owner.unloading)
           ~f:(fun attempt ->
             match attempt.Resource_retirement_data.coverage with
             | Whole_owner -> false
             | Cell actual when target cell actual.target -> selected attempt.recovery
             | Cell _ -> false)
  ;;

  let affected (owner : t) cell =
    Option.for_all owner.binding ~f:(binding cell)
    && List.for_all owner.retired_resources ~f:(fun actual ->
      binding cell actual.Retired_resource.binding)
    && Option.for_all owner.restored_policy ~f:(restored cell)
    && Option.for_all owner.native_anchor ~f:(native cell)
    && List.for_all owner.native_loans ~f:(native cell)
    && Option.for_all owner.policy_claim ~f:(fun claim ->
      target cell (Cell.Lease.cleanup_target claim.loan))
    && List.for_all owner.background_leases ~f:(background owner cell)
    && Option.for_all owner.prepared_source ~f:(fun stage ->
      source cell stage.extent.recovery)
    && Option.for_all owner.administration_transition ~f:(fun stage ->
      source cell stage.extent.recovery)
    && Option.for_all owner.failed_source_cleanup ~f:(source cell)
    && actor owner cell
  ;;

  let check (owner : t) = function
    | Retirement_coverage.Whole_owner -> Ok None
    | Cell cell ->
      let open Result.Let_syntax in
      let%bind status = Resource_consumer_core.check_target owner cell.target in
      (match status with
       | Cell.Cleanup_target.Status.Released -> Ok (Some status)
       | Held facets ->
         let refreshed = { cell with Retirement_coverage.facets } in
         if affected owner refreshed
         then (
           cell.facets <- facets;
           Ok (Some status))
         else Error (conflict ()))
  ;;

  let before_effect owner coverage =
    let open Result.Let_syntax in
    let%bind status = check owner coverage in
    match coverage, status with
    | Whole_owner, None -> Ok ()
    | Cell cell, Some _ -> if affected owner cell then Ok () else Error (conflict ())
    | Whole_owner, Some _ | Cell _, None ->
      invalid_arg "retirement coverage check changed its kind"
  ;;
end

let loaded_runtime (t : t) = Option.map t.binding ~f:Runtime_binding.runtime

(* This is an admission fence, not another producer/election state. Factual
   release never requires this predicate to be false. *)
let resource_retirement_pending (t : t) = Option.is_some t.pending_resource_retirement

let resource_retirement_blocks_admission (t : t) =
  resource_retirement_pending t || Option.is_some t.unloading
;;

let current_source_extent (t : t) =
  match t.prepared_source, t.administration_transition with
  | None, None -> Ok None
  | Some stage, None -> Ok (Some stage.extent)
  | None, Some transition -> Ok (Some transition.extent)
  | Some _, Some _ ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"resource retirement found overlapping source and administration"
         ~retryable:false
         ())
;;

(* Actual extent association only. A reservation that already passed admission
   before signal must attach before publishing, even if its RPC yielded. *)
let attach_pending_source_extent (t : t) extent =
  match t.pending_resource_retirement with
  | None -> Ok ()
  | Some attempt ->
    let open Result.Let_syntax in
    let%bind () =
      match attempt.coverage with
      | Whole_owner -> Ok ()
      | Cell cell ->
        if Retirement_coverage_guard.source cell extent.Source_extent.recovery
        then Ok ()
        else Error (Retirement_coverage_guard.conflict ())
    in
    (match attempt.source_extent with
     | None ->
       Recovery_data.register_source
         attempt.recovery
         ~source:extent.Source_extent.recovery;
       attempt.source_extent <- Some extent;
       Ok ()
     | Some actual when phys_equal actual extent -> Ok ()
     | Some _ ->
       Error
         (Agent_protocol.Error.create
            Conflict
            ~message:"pending resource retirement already owns another source extent"
            ~retryable:false
            ()))
;;

let policy_cleanup_pending (t : t) =
  Option.exists t.policy_resource_cleanup ~f:Policy_resource_cleanup.pending
;;

let constructor_model_pending (t : t) =
  Agent_session.Session_actor.constructor_model_retirement_pending t.actor
;;

let policy_claim_pending (t : t) =
  Option.is_some t.policy_claim
  || Agent_session.Session_actor.Delegated_policy_claim.pending t.actor
;;

let factual_cleanup_pending (t : t) =
  constructor_model_pending t
  || policy_claim_pending t
  || Agent_session.Session_actor.inference_usage_cleanup_pending t.actor
;;

let close_pending_policy_resource_reported (t : t) =
  Option.value_map
    t.policy_resource_cleanup
    ~default:
      (Agent_session.Close_observation.create (Returned (Ok ())) ~diagnostics:[] ())
    ~f:Policy_resource_cleanup.close_pending_reported
;;

let close_pending_policy_resource t =
  close_pending_policy_resource_reported t |> Agent_session.Close_observation.resume
;;

let stage_has_activation_handoff stage =
  Option.exists stage.continuation ~f:(fun continuation ->
    Agent_session.Session_actor.Runtime_construction.handoff_acknowledged continuation)
;;

let has_activation_handoff t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "Runtime_owner.has_activation_handoff: foreign domain";
  t.activation_handoff || Option.exists t.prepared_source ~f:stage_has_activation_handoff
;;

module Execution_access = struct
  type t =
    { stage : source_stage
    ; runtime : Agent_session.Runtime_builder.t
    ; borrow : Agent_session.Session_actor.Runtime_construction_resource.t
    }
end

let execution_access_is_current t (access : Execution_access.t) =
  (not (resource_retirement_pending t))
  && Int.equal access.stage.domain (Stdlib.Domain.self () :> int)
  && (not access.stage.closed)
  && Option.exists t.prepared_source ~f:(phys_equal access.stage)
  && Option.exists (loaded_runtime t) ~f:(phys_equal access.runtime)
  && Option.exists access.stage.handoff_runtime ~f:(phys_equal access.runtime)
  && Result.is_ok
       (Agent_session.Session_actor.Runtime_construction_resource.check_basis
          access.borrow)
;;

(* A root worker can request its actual resources before its submitting caller
   resumes. Only the Actor's immutable same-activation ACK permits coexistence
   with that old lexical stage; installation alone grants no such admission. *)
let administration_blocks ?administration t =
  match t.administration_transition with
  | None -> false
  | Some current ->
    not
      (Administration_transition.is_live current
       && Option.exists administration ~f:(phys_equal current))
;;

let prepared_source_blocks_runtime ?execution_access ?administration t =
  resource_retirement_pending t
  || administration_blocks ?administration t
  ||
  match execution_access, t.prepared_source with
  | Some access, _ -> not (execution_access_is_current t access)
  | None, None -> false
  | None, Some stage ->
    not
      (match stage.continuation, stage.handoff_runtime, loaded_runtime t with
       | Some continuation, Some installed, Some current ->
         phys_equal installed current
         && Agent_session.Session_actor.Runtime_construction.handoff_acknowledged
              continuation
       | None, _, _ | _, None, _ | _, _, None -> false)
;;

let create_internal
      ~runtime_source_owner
      ~before_unload
      ~policy_resource_cleanup
      ~policy_resource_access
      ~native_resource_access
      ~actor
      ~initial
      ~build
      ~build_deferred
      ~ordinary_warm
      ~delegated_driver
  =
  { affinity = Owner_affinity.create ~actor ~source_owner:runtime_source_owner
  ; actor
  ; runtime_source_owner
  ; build
  ; build_deferred
  ; ordinary_warm
  ; delegated_driver
  ; before_unload
  ; policy_resource_cleanup
  ; policy_resource_access
  ; native_resource_access
  ; native_anchor = None
  ; native_loans = []
  ; policy_claim = None
  ; restored_policy = None
  ; mutex = Eio.Mutex.create ()
  ; moderator_work_mutex = Eio.Mutex.create ()
  ; binding = initial
  ; prepared_source = None
  ; activation_handoff = false
  ; domain = (Stdlib.Domain.self () :> int)
  ; closed = false
  ; close_finished = false
  ; disposal_cache = Disposal_cache.create ()
  ; startup_settlement_request = None
  ; joined_close_handoff = None
  ; disposal_mutex = Eio.Mutex.create ()
  ; foreground_retirement = None
  ; joined_foreground_retirement = None
  ; unloading = None
  ; pending_resource_retirement = None
  ; background_leases = []
  ; retired_resources = []
  ; failed_source_cleanup = None
  ; administration_transition = None
  }
;;

let create_with_unload ~before_unload ~actor ~initial ~build =
  create_internal
    ~runtime_source_owner:None
    ~before_unload:(Some before_unload)
    ~policy_resource_cleanup:None
    ~policy_resource_access:None
    ~native_resource_access:None
    ~actor
    ~initial:(Option.map initial ~f:(fun runtime -> Runtime_binding.Legacy runtime))
    ~build:(Ordinary build)
    ~build_deferred:None
    ~ordinary_warm:None
    ~delegated_driver:None
;;

let create ~actor ~initial ~build =
  create_internal
    ~runtime_source_owner:None
    ~before_unload:None
    ~policy_resource_cleanup:None
    ~policy_resource_access:None
    ~native_resource_access:None
    ~actor
    ~initial:(Option.map initial ~f:(fun runtime -> Runtime_binding.Legacy runtime))
    ~build:(Ordinary build)
    ~build_deferred:None
    ~ordinary_warm:None
    ~delegated_driver:None
;;

let create_with_run_unload ~before_unload ~actor ~initial ~build =
  create_internal
    ~runtime_source_owner:None
    ~before_unload:(Some before_unload)
    ~policy_resource_cleanup:None
    ~policy_resource_access:None
    ~native_resource_access:None
    ~actor
    ~initial:(Option.map initial ~f:(fun runtime -> Runtime_binding.Legacy runtime))
    ~build:(For_runs build)
    ~build_deferred:None
    ~ordinary_warm:None
    ~delegated_driver:None
;;

let create_with_authenticated_run_unload ~before_unload ~actor ~initial ~build =
  create_internal
    ~runtime_source_owner:None
    ~before_unload:(Some before_unload)
    ~policy_resource_cleanup:None
    ~policy_resource_access:None
    ~native_resource_access:None
    ~actor
    ~initial:(Option.map initial ~f:(fun runtime -> Runtime_binding.Legacy runtime))
    ~build:(For_authenticated_runs build)
    ~build_deferred:None
    ~ordinary_warm:None
    ~delegated_driver:None
;;

let create_with_deferred_run_unload
      ~source_owner
      ~before_unload
      ~policy_resource_cleanup
      ~policy_resource_access
      ~native_resource_access
      ~actor
      ~initial
      ~build
      ~build_deferred
      ~ordinary_warm
      ~delegated_load
  =
  create_internal
    ~runtime_source_owner:
      (Some (Agent_session.Runtime_source_owner.identity source_owner))
    ~before_unload:(Some before_unload)
    ~policy_resource_cleanup:(Some policy_resource_cleanup)
    ~policy_resource_access
    ~native_resource_access
    ~actor
    ~initial:
      (Option.map initial ~f:(fun attachment -> Runtime_binding.Attached attachment))
    ~build:(For_factory build)
    ~build_deferred:(Some build_deferred)
    ~ordinary_warm
    ~delegated_driver:(Option.map delegated_load ~f:(fun run -> { source_owner; run }))
;;

let is_loaded t = Eio.Mutex.use_ro t.mutex (fun () -> Option.is_some (loaded_runtime t))
let belongs_to_actor t actor = phys_equal t.actor actor

let background_busy () =
  Agent_protocol.Error.create
    Conflict
    ~message:"background execution still owns the loaded runtime"
    ~retryable:true
    ()
;;

let check_native_installation t binding =
  match t.native_anchor with
  | None -> Ok ()
  | Some anchor ->
    if
      Option.exists
        (Runtime_binding.native_identity binding)
        ~f:(Agent_session.Runtime_builder.Native_identity.equal anchor.identity)
    then Ok ()
    else Error (background_busy ())
;;

let check_restored_policy_installation t binding =
  match t.restored_policy with
  | None -> Ok ()
  | Some anchor ->
    (match Runtime_binding.product binding with
     | Some product
       when Agent_session.Runtime_builder.Product_identity.equal
              product
              (Restored_policy_anchor.product anchor) -> Ok ()
     | Some _ | None ->
       Error
         (Agent_protocol.Error.create
            Conflict
            ~message:"ordinary binding displaced its restored policy Product"
            ~retryable:false
            ()))
;;

let install_actor ?run_preparation ?original_actor ?expected_ordinary t binding =
  let runtime = Runtime_binding.runtime binding in
  let open Result.Let_syntax in
  let%bind () = check_native_installation t binding in
  let%bind () = check_restored_policy_installation t binding in
  let%bind () =
    match runtime.automatic_turn_policy with
    | None -> Ok ()
    | Some policy ->
      (match run_preparation with
       | None ->
         Agent_session.Session_actor.enable_automatic_turn_budget
           ?actor:original_actor
           t.actor
           policy
       | Some preparation ->
         Agent_session.Session_actor.enable_run_constructor_turn_budget
           t.actor
           ~preparation
           policy)
  in
  let%bind _ =
    match run_preparation with
    | None ->
      let%bind state = Agent_session.Session_actor.state t.actor in
      let%bind moderator =
        Agent_session.Moderator_checkpoint.reuse_unchanged_snapshot
          ~previous:state.moderator
          ~incoming:(runtime.moderator_snapshot ())
      in
      Agent_session.Session_actor.change_moderator ?actor:original_actor t.actor moderator
    | Some preparation ->
      Agent_session.Session_actor.checkpoint_run_moderator
        ?moderator_manager:runtime.moderator_manager
        t.actor
        ~preparation
        (runtime.moderator_snapshot ())
  in
  let%bind initial_startup =
    match run_preparation, original_actor, runtime.moderator_activation with
    | None, Some _, Some activation when activation.startup_pending () ->
      let%bind state = Agent_session.Session_actor.state t.actor in
      (match state.lifecycle.desired with
       | Stopped -> Ok None
       | Running ->
         let%map startup = Agent_session.Runtime_builder.authored_start runtime in
         Option.some_if
           (Agent_session.Initial_lifecycle_startup.eligible startup ~state)
           startup)
    | (Some _ | None), (Some _ | None), (Some _ | None) -> Ok None
  in
  Agent_session.Session_actor.set_runtime_worker
    ?actor:original_actor
    ?initial_startup
    ?unmoderated_source:runtime.unmoderated_source
    ?moderator_manager:runtime.moderator_manager
    ?expected_ordinary
    t.actor
    ~worker:(Some runtime.worker)
    ~inference:(Some (Agent_session.Runtime_builder.inference_execution runtime))
;;

let release_native_after_adoption t =
  let open Result.Let_syntax in
  match t.native_anchor with
  | None -> Ok ()
  | Some anchor ->
    (* The SAME-native finalized Product binding was adopted before this
       baseline release. It is not the last physical reference and cannot
       elect teardown under the Owner mutex. Failed release keeps custody. *)
    let%map () =
      match anchor.phase with
      | Closing _ ->
        (* Its closer must reacquire this mutex to publish the actual result.
           Adoption must not join that attempt or replace its custody here. *)
        Error (background_busy ())
      | Released -> Ok ()
      | Available | Retiring ->
        Native_loan.retire anchor;
        let%map () = Session_policy_resource.Native_lease.close anchor.loan in
        anchor.phase <- Released
    in
    t.native_anchor <- None;
    t.native_loans
    <- List.filter t.native_loans ~f:(fun loan -> not (phys_equal loan anchor))
;;

let release_restored_policy_after_install t =
  let open Result.Let_syntax in
  let%bind () = release_native_after_adoption t in
  match t.restored_policy with
  | None -> Ok ()
  | Some anchor ->
    (* The successfully installed attachment owns a distinct finalized Cell
       lease. Dropping this baseline cannot be the physical last release. *)
    Restored_policy_anchor.revoke anchor;
    let%map () = Session_policy_resource.Lease.close anchor.loan in
    t.restored_policy <- None
;;

let replace_foreground_retirement t retirement =
  t.foreground_retirement <- retirement;
  t.joined_foreground_retirement <- None
;;

let install ?initialization_recovery ?run_preparation ?original_actor t binding =
  let open Result.Let_syntax in
  let%bind () = install_actor ?run_preparation ?original_actor t binding in
  t.binding <- Some binding;
  Option.iter initialization_recovery ~f:(fun recovery ->
    Recovery_data.record_transfer
      recovery
      ~binding
      ~proof:Source_transfer.Ordinary_installed);
  replace_foreground_retirement t None;
  release_restored_policy_after_install t
;;

let closed_error () =
  Agent_protocol.Error.create
    Server_shutting_down
    ~message:"session runtime owner is closed"
    ~retryable:true
    ()
;;

(* Host admission callbacks can fail while consulting another actor or store.
   Propagate their exception after releasing the mutex, so cleanup remains usable. *)
let with_owner_lock t ~protect f =
  let outcome =
    Eio.Mutex.use_rw ~protect t.mutex (fun () ->
      match f () with
      | result -> Ok result
      | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

(* Revocation is nonyielding; native/factual teardown is outside the Owner
   mutex and remains owned on every returned Error or unexpected exception. *)
let failed_source_cleanup_pending (t : t) = Option.is_some t.failed_source_cleanup

let source_cleanup_owns_duties recovery ~acquired =
  let captured = Recovery_data.captured_of_phase recovery.Recovery_data.phase in
  acquired
  || (not (List.is_empty captured.initializations_rev))
  || Option.is_some captured.prefix
  || (not (List.is_empty captured.source_tail_rev))
  || (not (List.is_empty captured.native_releases_rev))
  || (not (List.is_empty captured.restored_releases_rev))
  || (not (List.is_empty captured.constructions_rev))
  || List.exists captured.batches_rev ~f:(fun selected ->
    (not (List.is_empty selected.Recovery_batch.batch.receipts))
    ||
    match selected.batch.original with
    | Returned _ -> false
    | Raised _ -> true)
  || List.exists captured.original_steps_rev ~f:(fun step ->
    Option.exists step.Original_cleanup_step.original ~f:(fun primary ->
      Option.is_some (Cleanup_evidence.primary_failure primary)))
;;

(* Retain the original association under the SAME lock that clears the stage.
   A rejected empty extent cannot displace a genuine previous holder. *)
let retain_source_cleanup_locked t recovery ~acquired =
  if source_cleanup_owns_duties recovery ~acquired
  then (
    match t.failed_source_cleanup with
    | None -> t.failed_source_cleanup <- Some recovery
    | Some current when phys_equal current recovery -> ()
    | Some _ -> invalid_arg "failed source cleanup holder was displaced")
;;

let finish_source_cleanup_fence t recovery =
  with_owner_lock t ~protect:true (fun () ->
    match t.failed_source_cleanup, recovery.Recovery_data.phase with
    | Some current, Complete completed
      when phys_equal current recovery
           && Cleanup_snapshot.equal_progress completed.latest.progress Joined
           && Recovery_data.original_diagnostics_complete recovery ->
      t.failed_source_cleanup <- None
    | (Some _ | None), (Original_running _ | Complete _ | Retrying _) -> ())
;;

(* A legacy factual close can settle the SAME source-owned physical target.
   Re-observe that holder only at real cleanup completion, never in a getter or
   admission predicate. Missing End/control duties remain nonpositive. *)
let refresh_settled_source_cleanup_locked t =
  match t.failed_source_cleanup with
  | None -> ()
  | Some source ->
    (match source.Recovery_data.phase with
     | Original_running _ | Retrying _ -> ()
     | Complete completed ->
       let latest =
         if Cleanup_snapshot.equal_progress completed.latest.progress Joined
         then completed.latest
         else Recovery_data.Observation.snapshot (Recovery_data.Observation.scope source)
       in
       if
         Cleanup_snapshot.equal_progress latest.progress Joined
         && Recovery_data.original_diagnostics_complete source
       then (
         source.phase <- Complete { completed with latest };
         t.failed_source_cleanup <- None))
;;

let failed_source_allows_runtime_locked t binding =
  match t.failed_source_cleanup with
  | None -> true
  | Some source ->
    let captured = Recovery_data.captured_of_phase source.Recovery_data.phase in
    List.exists captured.transfers_rev ~f:(fun transfer ->
      phys_equal transfer.Source_transfer.binding binding)
;;

let close_restored_anchor_report_observed t (anchor : Restored_policy_anchor.t) report =
  let diagnostics = ref [] in
  let primary_origin = ref None in
  let retain origin primary =
    diagnostics := !diagnostics @ origin.Physical_origin.diagnostics;
    match primary with
    | Scope_outcome.Returned (Ok _) -> ()
    | Returned (Error _) | Raised _ -> primary_origin := origin.primary
  in
  let control ?diagnostic primary =
    let diagnostic =
      match diagnostic with
      | Some _ as diagnostic -> diagnostic
      | None -> Original_diagnostic.primary primary
    in
    retain
      { Physical_origin.diagnostics = Option.to_list diagnostic; primary = diagnostic }
      primary
  in
  let outcome =
    Scope_outcome.capture (fun () ->
      let open Result.Let_syntax in
      let revoke =
        Scope_outcome.capture (fun () ->
          Restored_policy_anchor.revoke anchor;
          Ok ())
      in
      Option.iter report ~f:(fun report ->
        report.Recovery_data.Restored_release_report.helper_revoke <- Some revoke;
        report.revoke_diagnostic <- Original_diagnostic.primary revoke);
      control
        ?diagnostic:(Option.bind report ~f:(fun report -> report.revoke_diagnostic))
        revoke;
      let%bind () = Scope_outcome.resume_primary revoke in
      let actual =
        Physical_origin.capture_result (fun () ->
          Session_policy_resource.Lease.close_reported anchor.loan)
      in
      let operation = Physical_origin.primary actual in
      retain (Physical_origin.of_observation actual) operation;
      Option.iter report ~f:(fun report ->
        report.Recovery_data.Restored_release_report.operation <- Some operation;
        report.operation_origin <- Some (Physical_origin.of_observation actual));
      let%bind () = Scope_outcome.resume_primary operation in
      let clear =
        Scope_outcome.capture (fun () ->
          with_owner_lock t ~protect:true (fun () ->
            if Option.exists t.restored_policy ~f:(phys_equal anchor)
            then t.restored_policy <- None);
          Ok ())
      in
      Option.iter report ~f:(fun report ->
        report.Recovery_data.Restored_release_report.clear <- Some clear;
        report.clear_diagnostic <- Original_diagnostic.primary clear);
      control
        ?diagnostic:(Option.bind report ~f:(fun report -> report.clear_diagnostic))
        clear;
      Scope_outcome.resume_primary clear)
  in
  Option.iter report ~f:(fun report ->
    report.Recovery_data.Restored_release_report.original <- Some outcome);
  let observed =
    Agent_session.Close_observation.create
      (match outcome with
       | Scope_outcome.Returned result -> Returned result
       | Raised (exn, trace) -> Raised (exn, trace))
      ~diagnostics:!diagnostics
      ?primary:!primary_origin
      ()
  in
  Physical_origin.with_boundary_failure
    observed
    (match outcome with
     | Returned (Ok ()) -> None
     | Returned (Error error) ->
       Some (Agent_session.Callback_cleanup.Failure.Protocol error)
     | Raised (exn, trace) -> Some (Raised (exn, trace)))
;;

let close_restored_anchor_observed
      ?recovery
      ?current
      t
      (anchor : Restored_policy_anchor.t)
  =
  let report =
    match recovery, current with
    | Some (recovery, position), None ->
      Some (Recovery_data.begin_restored_release recovery ~position anchor)
    | None, Some (slot, after, owner, position) ->
      if not (phys_equal slot.Recovery_data.anchor anchor)
      then invalid_arg "restored cleanup target was rebound";
      Some (Recovery_data.begin_current_restored slot ~after ~owner ~position)
    | None, None -> None
    | Some _, Some _ -> invalid_arg "restored cleanup has two receipt owners"
  in
  close_restored_anchor_report_observed t anchor report
;;

let close_restored_anchor_outcome ?recovery ?current t anchor =
  close_restored_anchor_observed ?recovery ?current t anchor |> Physical_origin.primary
;;

let close_restored_anchor ?recovery ?current t anchor =
  close_restored_anchor_outcome ?recovery ?current t anchor
  |> Scope_outcome.resume_primary
;;

let close_restored_policy_observed ?recovery t =
  let selection_origin = ref None in
  let selected =
    Scope_outcome.capture (fun () ->
      Ok
        (with_owner_lock t ~protect:true (fun () ->
           Option.map t.restored_policy ~f:(fun anchor ->
             let report =
               Option.map recovery ~f:(fun (recovery, position) ->
                 Recovery_data.begin_restored_release recovery ~position anchor)
             in
             (* Admission closes here under the original lock. Capture this actual
           revoke escape too, before the outside-lock pin helper is entered. *)
             let revoke =
               Scope_outcome.capture (fun () ->
                 Restored_policy_anchor.revoke anchor;
                 Ok ())
             in
             Option.iter report ~f:(fun report ->
               report.Recovery_data.Restored_release_report.selection_revoke
               <- Some revoke;
               report.selection_diagnostic <- Original_diagnostic.primary revoke);
             let diagnostic =
               match
                 Option.bind report ~f:(fun report -> report.selection_diagnostic)
               with
               | Some _ as diagnostic -> diagnostic
               | None -> Original_diagnostic.primary revoke
             in
             Option.iter diagnostic ~f:(fun primary ->
               selection_origin
               := Some
                    { Physical_origin.diagnostics = [ primary ]; primary = Some primary });
             (match revoke with
              | Scope_outcome.Returned (Ok ()) -> ()
              | Raised (exn, backtrace) ->
                Option.iter report ~f:(fun report ->
                  report.Recovery_data.Restored_release_report.original <- Some revoke);
                Exn.raise_with_original_backtrace exn backtrace
              | Returned (Error _) ->
                invalid_arg "unit restored revoke returned a protocol error");
             anchor, report))))
  in
  match selected with
  | Scope_outcome.Returned (Ok None) ->
    Agent_session.Close_observation.create (Returned (Ok ())) ~diagnostics:[] ()
  | Returned (Ok (Some (anchor, report))) ->
    close_restored_anchor_report_observed t anchor report
  | Returned (Error error) ->
    let origin =
      Option.value_or_thunk !selection_origin ~default:(fun () ->
        let diagnostic = Agent_session.Cleanup_diagnostic.create [ Protocol error ] in
        { Physical_origin.diagnostics = [ diagnostic ]; primary = Some diagnostic })
    in
    Agent_session.Close_observation.create
      (Returned (Error error))
      ~diagnostics:origin.diagnostics
      ?primary:origin.primary
      ()
  | Raised (exn, trace) ->
    let origin =
      Option.value_or_thunk !selection_origin ~default:(fun () ->
        let diagnostic =
          Agent_session.Cleanup_diagnostic.create [ Raised (exn, trace) ]
        in
        { Physical_origin.diagnostics = [ diagnostic ]; primary = Some diagnostic })
    in
    Agent_session.Close_observation.create
      (Raised (exn, trace))
      ~diagnostics:origin.diagnostics
      ?primary:origin.primary
      ()
;;

let close_restored_policy ?recovery t =
  close_restored_policy_observed ?recovery t |> Agent_session.Close_observation.resume
;;

let check_original_actor = function
  | Some actor when not (Operator_authorization.is_current actor) ->
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:"runtime initialization authentication is no longer current"
         ~retryable:false
         ())
  | Some _ | None -> Ok ()
;;

let native_release_pending t =
  List.exists t.native_loans ~f:(fun loan ->
    match loan.Native_loan.phase with
    | Available | Released -> false
    | Retiring | Closing _ -> true)
;;

let close_native_loan_observed ?recovery ?current t (loan : Native_loan.t) =
  let report =
    match recovery, current with
    | Some (recovery, position), None ->
      Some (Recovery_data.begin_native_release recovery ~position loan)
    | None, Some (slot, after, owner, position) ->
      if not (phys_equal slot.Recovery_data.loan loan)
      then invalid_arg "native cleanup target was rebound";
      Some (Recovery_data.begin_current_native slot ~after ~owner ~position)
    | None, None -> None
    | Some _, Some _ -> invalid_arg "native cleanup has two receipt owners"
  in
  let diagnostics = ref [] in
  let primary_origin = ref None in
  let retain origin primary =
    diagnostics := !diagnostics @ origin.Physical_origin.diagnostics;
    match primary with
    | Scope_outcome.Returned (Ok _) -> ()
    | Returned (Error _) | Raised _ -> primary_origin := origin.primary
  in
  let control ?diagnostic primary =
    let diagnostic =
      match diagnostic with
      | Some _ as diagnostic -> diagnostic
      | None -> Original_diagnostic.primary primary
    in
    retain
      { Physical_origin.diagnostics = Option.to_list diagnostic; primary = diagnostic }
      primary
  in
  let outcome =
    try
      Ok
        (Eio.Cancel.protect (fun () ->
           let selection =
             Scope_outcome.capture (fun () ->
               Ok
                 (with_owner_lock t ~protect:true (fun () ->
                    match loan.phase with
                    | Released -> Recovery_data.Native_release_report.Selection.Released
                    | (Available | Retiring | Closing _)
                      when not (List.mem t.native_loans loan ~equal:phys_equal) ->
                      Recovery_data.Native_release_report.Selection.Missing
                        (Agent_protocol.Error.create
                           Conflict
                           ~message:"native release lost its actual loan custody"
                           ~retryable:false
                           ())
                    | Closing attempt ->
                      Recovery_data.Native_release_report.Selection.Join attempt
                    | Available | Retiring ->
                      let attempt = Native_loan.Attempt.create () in
                      loan.phase <- Closing attempt;
                      Recovery_data.Native_release_report.Selection.Close attempt)))
           in
           Option.iter report ~f:(fun report ->
             report.Recovery_data.Native_release_report.selection <- Some selection;
             report.selection_diagnostic
             <- (match selection with
                 | Scope_outcome.Returned (Ok (Missing error)) ->
                   Original_diagnostic.primary (Scope_outcome.Returned (Error error))
                 | Returned (Ok (Released | Join _ | Close _)) -> None
                 | Returned (Error error) ->
                   Original_diagnostic.primary (Scope_outcome.Returned (Error error))
                 | Raised (exn, trace) ->
                   Original_diagnostic.primary (Scope_outcome.Raised (exn, trace))));
           let selection_primary =
             match selection with
             | Scope_outcome.Returned (Ok (Missing error)) ->
               Scope_outcome.Returned (Error error)
             | Returned (Ok (Released | Join _ | Close _)) -> Returned (Ok ())
             | Returned (Error error) -> Returned (Error error)
             | Raised (exn, trace) -> Raised (exn, trace)
           in
           control
             ?diagnostic:
               (Option.bind report ~f:(fun report -> report.selection_diagnostic))
             selection_primary;
           let open Result.Let_syntax in
           let%bind selected = Scope_outcome.resume_primary selection in
           let resume = function
             | Ok result -> result
             | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
           in
           match selected with
           | Recovery_data.Native_release_report.Selection.Released -> Ok ()
           | Recovery_data.Native_release_report.Selection.Missing error -> Error error
           | Recovery_data.Native_release_report.Selection.Join attempt ->
             Option.iter report ~f:(fun report ->
               report.Recovery_data.Native_release_report.attempt <- Some attempt);
             let operation = Eio.Promise.await attempt.finished in
             Option.iter attempt.Native_loan.Attempt.origin ~f:(fun origin ->
               retain
                 origin
                 (match operation with
                  | Ok result -> Scope_outcome.Returned result
                  | Error (exn, trace) -> Raised (exn, trace)));
             Option.iter report ~f:(fun report ->
               report.Recovery_data.Native_release_report.operation <- Some operation);
             resume operation
           | Recovery_data.Native_release_report.Selection.Close attempt ->
             Option.iter report ~f:(fun report ->
               report.Recovery_data.Native_release_report.attempt <- Some attempt);
             let actual =
               Physical_origin.capture_result (fun () ->
                 Session_policy_resource.Native_lease.close_reported loan.loan)
             in
             let outcome = Physical_origin.native actual in
             let origin = Physical_origin.of_observation actual in
             attempt.origin <- Some origin;
             retain origin (Physical_origin.primary actual);
             (* Actual close outcome belongs to this exact invocation BEFORE
                the later commit can raise. A successful pin alone does not
                certify that bookkeeping or the whole holder joined. *)
             Option.iter report ~f:(fun report ->
               report.Recovery_data.Native_release_report.operation <- Some outcome);
             let commit =
               Scope_outcome.capture (fun () ->
                 with_owner_lock t ~protect:true (fun () ->
                   (match outcome with
                    | Ok (Ok ()) ->
                      loan.phase <- Released;
                      t.native_loans
                      <- List.filter t.native_loans ~f:(fun current ->
                           not (phys_equal current loan));
                      if Option.exists t.native_anchor ~f:(phys_equal loan)
                      then t.native_anchor <- None
                    | Ok (Error _) | Error _ -> loan.phase <- Retiring);
                   Eio.Promise.resolve attempt.finish outcome);
                 Ok ())
             in
             Option.iter report ~f:(fun report ->
               report.Recovery_data.Native_release_report.commit <- Some commit;
               report.commit_diagnostic <- Original_diagnostic.primary commit);
             control
               ?diagnostic:
                 (Option.bind report ~f:(fun report -> report.commit_diagnostic))
               commit;
             let%bind () = Scope_outcome.resume_primary commit in
             resume outcome))
    with
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  Option.iter report ~f:(fun report ->
    report.Recovery_data.Native_release_report.original <- Some outcome);
  let exit =
    match outcome with
    | Ok result -> Agent_session.Close_observation.Returned result
    | Error (exn, trace) -> Raised (exn, trace)
  in
  (* Only an escape outside the typed selection/operation/commit boundaries
     owns an opaque helper boundary. A known underlying producer is forwarded. *)
  let observed =
    Agent_session.Close_observation.create
      exit
      ~diagnostics:!diagnostics
      ?primary:!primary_origin
      ()
  in
  Physical_origin.with_boundary_failure
    observed
    (match outcome with
     | Ok (Ok ()) -> None
     | Ok (Error error) -> Some (Agent_session.Callback_cleanup.Failure.Protocol error)
     | Error (exn, trace) -> Some (Raised (exn, trace)))
;;

let close_native_loan_reported ?recovery ?current t loan =
  close_native_loan_observed ?recovery ?current t loan |> Physical_origin.native
;;

let close_native_loan ?recovery ?current t loan =
  match close_native_loan_reported ?recovery ?current t loan with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let check_relation_pin (t : t) relation (actual : Native_loan.t) =
  let open Result.Let_syntax in
  let conflict message =
    Agent_protocol.Error.create Conflict ~message ~retryable:false ()
  in
  let%bind () =
    if Int.equal t.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "native relation belongs to another Eio domain")
  in
  let target = Session_policy_resource.Native_lease.cleanup_target actual.loan in
  let%bind facets =
    match Option.bind t.policy_resource_cleanup ~f:Policy_resource_cleanup.cell with
    | None -> Error (conflict "native relation provider has no fixed Cell")
    | Some cell ->
      Result.bind (Session_policy_resource.Cleanup_target.check target ~cell) ~f:(function
        | Held facets -> Ok facets
        | Released -> Error (conflict "native relation pin is already released"))
  in
  let same_target expected =
    Session_policy_resource.Cleanup_target.equal target expected
  in
  let matches =
    match relation.Background_lease.Relation.basis with
    | Lifetime_only -> false
    | Binding binding ->
      Option.exists (Runtime_binding.product binding) ~f:(fun expected ->
        Option.exists
          facets.product
          ~f:(Agent_session.Runtime_builder.Product_identity.equal expected))
      && Option.exists
           (Runtime_binding.native_identity binding)
           ~f:(Agent_session.Runtime_builder.Native_identity.equal actual.identity)
    | Restored anchor ->
      same_target (Session_policy_resource.Lease.cleanup_target anchor.loan)
      && Agent_session.Runtime_builder.Native_identity.equal
           actual.identity
           (Agent_session.Runtime_builder.Product.native_identity
              (Session_policy_resource.Lease.product anchor.loan))
    | Native selected ->
      same_target (Session_policy_resource.Native_lease.cleanup_target selected.loan)
      && Agent_session.Runtime_builder.Native_identity.equal
           selected.identity
           actual.identity
  in
  if matches
  then Ok ()
  else Error (conflict "native relation pin differs from its captured provider basis")
;;

(* Native's returned Cleanup and SAME factual release retain one actual provider
   report. The lease already owns recovery, finished and sealed relation receipt;
   no parallel identity fields or completion executor are needed. *)
module Native_dependency_report = struct
  type t = { lease : background_lease }
end

let handoff_completed_dependency (t : t) (lease : background_lease) =
  (* A completed outgoing scope/native report must already own this exact lease
     recovery before this fixed metadata-only publication is called. *)
  if Int.equal t.domain (Stdlib.Domain.self () :> int)
  then
    with_owner_lock t ~protect:true (fun () ->
      if
        Owner_affinity.same t.affinity lease.recovery.affinity
        && (not (Background_lease.is_active lease))
        && Option.is_some (Eio.Promise.peek lease.finished)
        && List.mem t.background_leases lease ~equal:phys_equal
      then (
        match lease.relation, lease.recovery.phase with
        | None, _ | Some _, (Original_running _ | Retrying _) -> ()
        | Some relation, Complete _ ->
          let historical =
            match relation.report.operation with
            | None | Some { selection = Detached_handoff; _ } -> true
            | Some { selection = Active_revocation; _ } -> false
          in
          let receipt =
            match Relation_evidence.native_handoff_evidence relation.report with
            | None | Some { complete = false; _ } -> false
            | Some { complete = true; duties } -> Cleanup_evidence.custody_joined duties
          in
          let provider = Recovery_data.Observation.scope lease.recovery in
          if
            historical
            && receipt
            && Result.is_ok
                 (Cleanup_snapshot.observation
                    (Recovery_data.Observation.snapshot provider))
            && Cleanup_evidence.custody_joined provider
            && Original_cleanup_report.diagnostics_complete
                 (Cleanup_evidence.child_view provider ~diagnostics_complete:true)
          then
            t.background_leases
            <- List.filter t.background_leases ~f:(fun actual ->
                 not (phys_equal actual lease))))
;;

module Native_release = struct
  type owner = t

  type t =
    { owner : owner
    ; loan : Native_loan.t
    ; mutable provider_report : Native_dependency_report.t option
    }

  type status =
    | Held
    | Released
  [@@deriving equal, sexp_of]

  let create owner loan = { owner; loan; provider_report = None }

  let attach_report t (report : Native_dependency_report.t) =
    match t.provider_report with
    | Some _ -> invalid_arg "native release report was already attached"
    | None ->
      if
        Option.exists report.lease.relation ~f:(fun relation ->
          Option.exists relation.pin ~f:(phys_equal t.loan))
      then t.provider_report <- Some report
      else invalid_arg "native release report changed its actual provider pin"
  ;;

  let check_domain t =
    if Int.equal t.owner.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "native release belongs to another Eio domain")
  ;;

  let status t =
    let open Result.Let_syntax in
    let%map () = check_domain t in
    match t.loan.phase with
    | Released -> Released
    | Available | Retiring | Closing _ -> Held
  ;;

  let close_observed t =
    Physical_origin.capture_result (fun () ->
      match check_domain t with
      | Error error ->
        Agent_session.Close_observation.create (Returned (Error error)) ~diagnostics:[] ()
      | Ok () ->
        (match t.loan.phase with
         | Available ->
           let error =
             Agent_protocol.Error.create
               Conflict
               ~message:"native release callback has not exited"
               ~retryable:true
               ()
           in
           Agent_session.Close_observation.create
             (Returned (Error error))
             ~diagnostics:[]
             ()
         | Released | Retiring | Closing _ ->
           let observed = close_native_loan_observed t.owner t.loan in
           (* Reobserve custody only after the actual factual invocation. This
              never calls a consumer, repeats physical work or changes observed. *)
           Option.iter t.provider_report ~f:(fun report ->
             handoff_completed_dependency t.owner report.lease);
           observed))
  ;;

  let close t = close_observed t |> Agent_session.Close_observation.resume
end

module Native_resource_custody = struct
  type owner = t

  type t =
    { owner : owner
    ; domain : int
    ; loan : Native_loan.t
    ; check_current :
        Session_policy_resource.Native_lease.t
        -> Native_resource_access.custody_check
        -> (unit, Agent_protocol.Error.t) result
    }

  let create (owner : owner) (loan : Native_loan.t) ~check_current =
    { owner; domain = owner.domain; loan; check_current }
  ;;

  let check_loan t (query : Native_resource_access.custody_check) =
    with_owner_lock t.owner ~protect:true (fun () ->
      let retiring =
        match query with
        | Resources -> false
        | Retained_source _ -> resource_retirement_blocks_admission t.owner
      in
      let available =
        match t.loan.phase with
        | Available -> true
        | Retiring | Closing _ | Released -> false
      in
      if t.owner.closed
      then Error (closed_error ())
      else if retiring
      then Error (background_busy ())
      else if
        available
        && List.mem t.owner.native_loans t.loan ~equal:phys_equal
        && Agent_session.Runtime_builder.Native_identity.equal
             t.loan.identity
             (Session_policy_resource.Native_lease.identity t.loan.loan)
      then Ok ()
      else
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"native resource custody is no longer current"
             ~retryable:false
             ()))
  ;;

  let check_for t query =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal t.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request
             "native resource custody belongs to another Eio domain")
    in
    let%bind () = check_loan t query in
    (* Fixed factual Cell/source/profile check may yield. The actual provider's
       permanent close revokes use, independently of the first consumer's
       lexical closure. No original consumer, origin or admission is retained. *)
    let%bind () = t.check_current t.loan.loan query in
    check_loan t query
  ;;

  let check t = check_for t Resources
  let check_source t ~source = check_for t (Retained_source source)
end

module Native_borrow_outcome = struct
  module Preparation_failure = struct
    module CO = Agent_session.Close_observation
    module D = Agent_session.Cleanup_diagnostic

    type stage =
      | Admission
      | Source_callback
      | Source_admission
      | Selection
      | Lease_admission
      | Lease_callback
    [@@deriving equal, sexp_of]

    type t =
      { stage : stage
      ; observation : (unit, Agent_protocol.Error.t) result CO.t
      }

    let stage t = t.stage
    let observation t = t.observation

    let of_observation ~stage observed =
      let exit =
        match CO.exit observed with
        | Returned (Ok _) -> None
        | Returned (Error error) -> Some (CO.Returned (Error error))
        | Raised (exn, trace) -> Some (CO.Raised (exn, trace))
      in
      Option.map exit ~f:(fun exit ->
        { stage
        ; observation =
            CO.create
              exit
              ~diagnostics:(CO.diagnostics observed)
              ?primary:(CO.primary observed)
              ()
        })
    ;;

    (* These are the four finite actual invocation slots, not a diagnostic
       history. Only the explicit selected carrier establishes an alias. *)
    let select ~primary ~candidates =
      match CO.exit primary with
      | Returned (Ok _) -> None
      | Returned (Error _) | Raised _ ->
        Option.bind (CO.primary primary) ~f:(fun selected ->
          if List.is_empty (D.failures selected)
          then None
          else
            List.find_map candidates ~f:(fun candidate ->
              Option.bind candidate ~f:(fun candidate ->
                Option.bind (CO.primary candidate.observation) ~f:(fun original ->
                  Option.some_if (D.equal selected original) candidate))))
    ;;
  end

  module Pin = struct
    type t =
      | Not_acquired
      | Acquired of
          { release : Native_release.t
          ; cleanup : Scope_outcome.Cleanup.t
          }
  end

  module Cleanup = struct
    type t =
      { preparation_report : unit Scope_outcome.t option
      ; pin : Pin.t
      ; outer_lease : Scope_outcome.Cleanup.t
      ; preparation_observation :
          (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
      ; outer_lease_observation :
          (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
      ; observation :
          (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
      ; provider_report : Native_dependency_report.t option
      ; outer_recovery : Recovery_data.t option
      }

    let preparation t =
      Option.value_map
        t.preparation_report
        ~default:Scope_outcome.Cleanup.Never_acquired
        ~f:Scope_outcome.cleanup
    ;;

    let pin t = t.pin
    let outer_lease t = t.outer_lease
    let observation (t : t) = t.observation

    let pin_cleanup t =
      match t.pin with
      | Not_acquired -> Scope_outcome.Cleanup.Never_acquired
      | Acquired pin -> pin.cleanup
    ;;

    let combined t =
      Scope_outcome.Cleanup.combine
        (preparation t)
        (Scope_outcome.Cleanup.combine (pin_cleanup t) t.outer_lease)
    ;;

    let positive (cleanup : Scope_outcome.Cleanup.t) =
      match Scope_outcome.Cleanup.status cleanup with
      | Never_acquired | Joined -> true
      | Failed _ -> false
    ;;

    let observation_positive observation =
      match Agent_session.Close_observation.exit observation with
      | Returned (Ok ()) -> true
      | Returned (Error _) | Raised _ -> false
    ;;

    let outer_recovery t ~(owner : Native_release.owner) =
      let open Result.Let_syntax in
      let refusal message = Agent_protocol.Error.invalid_request message in
      let%bind () =
        if Int.equal owner.domain (Stdlib.Domain.self () :> int)
        then Ok ()
        else Error (refusal "native outer cleanup belongs to another Eio domain")
      in
      match t.outer_recovery with
      | None -> Ok None
      | Some recovery ->
        let%bind () =
          if Int.equal recovery.affinity.domain (Stdlib.Domain.self () :> int)
          then Ok ()
          else Error (refusal "native outer cleanup belongs to another Eio domain")
        in
        let%bind () =
          if Owner_affinity.same recovery.affinity owner.affinity
          then Ok ()
          else Error (refusal "native outer cleanup belongs to another Owner")
        in
        let eligible =
          positive (preparation t)
          && observation_positive t.preparation_observation
          && (not (observation_positive t.outer_lease_observation))
          &&
          match t.pin with
          | Acquired { cleanup; _ } ->
            (match Scope_outcome.Cleanup.status cleanup with
             | Joined -> true
             | Never_acquired | Failed _ -> false)
          | Not_acquired -> false
        in
        if not eligible
        then Ok None
        else (
          match recovery.phase with
          | Original_running _ | Retrying _ ->
            Error (refusal "native outer cleanup is not currently complete")
          | Complete completed ->
            if Recovery_data.original_diagnostics_complete recovery
            then Ok (Some (recovery, completed.latest))
            else
              Error (refusal "native outer cleanup original diagnostics are unfinished"))
    ;;

    let pin_only_failure t =
      match t.pin with
      | Acquired { release; cleanup }
        when positive (preparation t)
             && positive t.outer_lease
             && observation_positive t.preparation_observation
             && observation_positive t.outer_lease_observation ->
        (match Scope_outcome.Cleanup.status cleanup with
         | Failed _ -> Some release
         | Never_acquired | Joined -> None)
      | Not_acquired | Acquired _ -> None
    ;;
  end

  type 'a t =
    { primary : 'a Scope_outcome.primary
    ; preadmission : unit Scope_outcome.primary option
    ; preparation_failure : Preparation_failure.t option
    ; cleanup : Cleanup.t
    ; observation : ('a, Agent_protocol.Error.t) result Agent_session.Close_observation.t
    }

  let preadmission t = t.preadmission
  let preparation_failure t = t.preparation_failure
  let preparation t = t.cleanup.preparation_report
  let primary t = t.primary
  let cleanup t = t.cleanup
  let observation (t : 'a t) = t.observation

  let to_scope_outcome t =
    { Scope_outcome.primary = t.primary; cleanup = Cleanup.combined t.cleanup }
  ;;

  let resume t = Scope_outcome.resume (to_scope_outcome t)
end

let retry_native_releases_observed ?recovery t =
  Physical_origin.capture_result (fun () ->
    let pending =
      with_owner_lock t ~protect:true (fun () ->
        let pending =
          List.filter t.native_loans ~f:(fun loan ->
            match loan.Native_loan.phase with
            | Available | Released -> false
            | Retiring | Closing _ -> true)
        in
        Option.iter recovery ~f:(fun (recovery, position) ->
          List.iter pending ~f:(Recovery_data.register_native_release recovery ~position));
        pending)
    in
    let rec close diagnostics_rev = function
      | [] ->
        Agent_session.Close_observation.create
          (Returned (Ok ()))
          ~diagnostics:(List.rev diagnostics_rev)
          ()
      | loan :: rest ->
        let actual = close_native_loan_observed ?recovery t loan in
        let diagnostics = Agent_session.Close_observation.diagnostics actual in
        (match Agent_session.Close_observation.exit actual with
         | Returned (Ok ()) -> close (List.rev_append diagnostics diagnostics_rev) rest
         | Returned (Error _) | Raised _ ->
           if List.is_empty diagnostics_rev
           then actual
           else
             Agent_session.Close_observation.create
               (Agent_session.Close_observation.exit actual)
               ~diagnostics:(List.rev_append diagnostics_rev diagnostics)
               ?primary:(Agent_session.Close_observation.primary actual)
               ())
    in
    close [] pending)
;;

let retry_native_releases ?recovery t =
  retry_native_releases_observed ?recovery t
  |> Physical_origin.primary
  |> Scope_outcome.resume_primary
;;

let retry_native_resource_cleanup t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then
    Error
      (Agent_protocol.Error.invalid_request
         "native resource cleanup belongs to another Eio domain")
  else retry_native_releases t
;;

let close_native_anchor_observed ?recovery t =
  Physical_origin.capture_result (fun () ->
    let anchor =
      with_owner_lock t ~protect:true (fun () ->
        Option.iter t.native_anchor ~f:(fun anchor ->
          Option.iter recovery ~f:(fun (recovery, position) ->
            Recovery_data.register_native_release recovery ~position anchor));
        t.native_anchor)
    in
    match anchor with
    | None -> Agent_session.Close_observation.create (Returned (Ok ())) ~diagnostics:[] ()
    | Some anchor -> close_native_loan_observed ?recovery t anchor)
;;

let close_native_anchor ?recovery t =
  close_native_anchor_observed ?recovery t
  |> Physical_origin.primary
  |> Scope_outcome.resume_primary
;;

(* A native borrower retains one ACTUAL Product binding, never every retired
   view of that same unit. Prefer the current binding; while unloaded retain
   one healthy retired binding for the existing Resource_handoff. Explicit
   resource borrowers below still retain their own exact runtime independently. *)
let native_retention_binding t identity =
  let matches binding =
    Option.exists
      (Runtime_binding.native_identity binding)
      ~f:(Agent_session.Runtime_builder.Native_identity.equal identity)
  in
  match t.binding with
  | Some binding when matches binding -> Some binding
  | Some _ | None ->
    List.find_map t.retired_resources ~f:(fun entry ->
      match entry.Retired_resource.phase with
      | Retained None when matches entry.binding -> Some entry.binding
      | Retained (Some _) | Closing _ | Closed | Retained None -> None)
;;

module Resource_borrow_route = struct
  type t =
    | Direct_resource
    | Native_retention of
        Agent_session.Runtime_builder.Native_identity.t * Runtime_binding.t
end

let resource_borrow_route (t : t) runtime (lease : background_lease) =
  if not (Background_lease.is_active lease)
  then None
  else if Option.exists lease.resources ~f:(phys_equal runtime)
  then Some Resource_borrow_route.Direct_resource
  else (
    match lease.purpose with
    | Shared_native identity ->
      let binding =
        match t.binding with
        | Some binding when Runtime_binding.matches binding runtime -> Some binding
        | Some _ | None ->
          List.find_map t.retired_resources ~f:(fun entry ->
            Option.some_if
              (Runtime_binding.matches entry.Retired_resource.binding runtime)
              entry.binding)
      in
      let native = Option.bind binding ~f:Runtime_binding.native_identity in
      if
        Option.exists
          native
          ~f:(Agent_session.Runtime_builder.Native_identity.equal identity)
      then
        Option.bind (native_retention_binding t identity) ~f:(fun selected ->
          Option.some_if
            (Runtime_binding.matches selected runtime)
            (Resource_borrow_route.Native_retention (identity, selected)))
      else None
    | Exclusive | Shared_resource _ -> None)
;;

let resources_borrowed (t : t) runtime =
  List.exists t.background_leases ~f:(fun lease ->
    Option.is_some (resource_borrow_route t runtime lease))
;;

(* SAME predicate as resources_borrowed; retain its actual matching lease and
   immutable route/binding at the selection cut, not a later global boolean. *)
let retired_resource_borrowers (t : t) (entry : Retired_resource.t) =
  List.filter_map t.background_leases ~f:(fun lease ->
    Option.map
      (resource_borrow_route t (Retired_resource.runtime entry) lease)
      ~f:(function
        | Direct_resource ->
          { Retired_resource.Receipt.Borrower.finished = lease.finished
          ; binding = entry.Retired_resource.binding
          ; route = Direct_resource
          }
        | Native_retention (identity, binding) ->
          { Retired_resource.Receipt.Borrower.finished = lease.finished
          ; binding
          ; route = Native_retention identity
          }))
;;

let restored_anchor_cleanup_pending t =
  Option.exists t.restored_policy ~f:(fun anchor ->
    match anchor.Restored_policy_anchor.phase with
    | Preparing | Retiring -> true
    | Available _ -> false)
;;

let retired_cleanup_pending t =
  native_release_pending t
  || restored_anchor_cleanup_pending t
  || factual_cleanup_pending t
  || policy_cleanup_pending t
  || List.exists t.retired_resources ~f:(fun entry ->
    not (resources_borrowed t (Retired_resource.runtime entry)))
;;

let retired_cleanup_error () =
  Agent_protocol.Error.create
    Conflict
    ~message:"runtime cleanup must finish before fresh admission"
    ~retryable:true
    ()
;;

(* Retention and election occur under the mutex; actual close and join never do. *)
let request_retired_resource_locked t cleanup runtime =
  Option.iter
    (List.find t.retired_resources ~f:(fun entry ->
       phys_equal (Retired_resource.runtime entry) runtime))
    ~f:(Cleanup_request.add cleanup)
;;

let release_runtime_locked t cleanup binding =
  let runtime = Runtime_binding.runtime binding in
  if
    not
      (List.exists t.retired_resources ~f:(fun entry ->
         phys_equal (Retired_resource.runtime entry) runtime))
  then t.retired_resources <- Retired_resource.create binding :: t.retired_resources;
  request_retired_resource_locked t cleanup runtime
;;

let retry_retired_resources_locked t cleanup =
  List.iter t.retired_resources ~f:(Cleanup_request.add cleanup)
;;

let run_cleanup_request_reported
      ?(requirement = Retired_resource.Receipt.Requirement.Initial)
      t
      cleanup
  =
  Eio.Cancel.protect (fun () ->
    let first_failure = ref None in
    (* Every exact original entry is retained BEFORE the queue is cleared or
       selection/close can raise. No empty later queue can complete this batch. *)
    let receipts =
      List.map cleanup.Cleanup_request.entries ~f:(fun resource ->
        ref { Retired_resource.Receipt.resource; disposition = Unattempted })
    in
    cleanup.entries <- [];
    let original =
      try
        List.iter receipts ~f:(fun receipt ->
          let entry = !receipt.Retired_resource.Receipt.resource in
          let selected =
            with_owner_lock t ~protect:true (fun () ->
              (* Closed is sticky evidence even if unrelated Actor facts remain.
                 Otherwise preserve the original borrowed/factual no-close gate. *)
              match entry.phase with
              | Closed -> Retired_resource.Skip Retired_resource.Receipt.Already_closed
              | Retained _ | Closing _ ->
                let factual_pending = factual_cleanup_pending t in
                let borrowers = retired_resource_borrowers t entry in
                if factual_pending || not (List.is_empty borrowers)
                then (
                  let previous_failure, producer =
                    match entry.phase with
                    | Retained failure -> failure, None
                    | Closing attempt -> attempt.prior_failure, Some attempt
                    | Closed ->
                      invalid_arg "retired resource changed during locked selection"
                  in
                  let custody : Retired_resource.Receipt.Custody.t =
                    { borrowers; factual_pending }
                  in
                  Retired_resource.Skip
                    (Retired_resource.Receipt.deferred
                       ~requirement
                       ~custody
                       ~previous_failure
                       ~producer))
                else (
                  match entry.phase with
                  | Closed ->
                    Retired_resource.Skip Retired_resource.Receipt.Already_closed
                  | Closing attempt -> Retired_resource.Join attempt
                  | Retained previous_failure ->
                    let attempt =
                      Retired_resource.Attempt.create ~prior_failure:previous_failure
                    in
                    entry.phase <- Closing attempt;
                    Retired_resource.Close attempt))
          in
          let record disposition =
            receipt := { Retired_resource.Receipt.resource = entry; disposition }
          in
          let outcome =
            match selected with
            | Retired_resource.Skip disposition ->
              record disposition;
              (match disposition with
               | Already_closed ->
                 if
                   Option.for_all
                     entry.product_selection
                     ~f:Product_selection.custody_joined
                 then
                   with_owner_lock t ~protect:true (fun () ->
                     t.retired_resources
                     <- List.filter t.retired_resources ~f:(fun current ->
                          not (phys_equal current entry)))
               | Unattempted | Attempted _ | Transferred _ | Blocked _ -> ());
              Ok ()
            | Retired_resource.Join attempt ->
              let outcome = Eio.Promise.await attempt.finished in
              record (Attempted { attempt; outcome });
              outcome
            | Retired_resource.Close attempt ->
              Retired_resource.capture_product entry attempt;
              let actual = Runtime_binding.close_reported entry.binding in
              let outcome = Physical_origin.cleanup actual in
              (* Capture actual execution BEFORE fallible commit bookkeeping.
                 Naked close Ok is not a sticky Closed/finished receipt. *)
              record (Attempted { attempt; outcome });
              let physical = Physical_origin.of_observation actual in
              let product_diagnostics =
                Option.value_map
                  attempt.product_selection
                  ~default:[]
                  ~f:Product_selection.diagnostics
              in
              attempt.origin <- Some physical;
              (* Exact actual Close/Join transport includes Product aliases.
                 Prior/Transferred physical views keep only [origin], so an
                 earlier non-owning receipt cannot adopt this later selection. *)
              attempt.close_origin
              <- Some
                   { physical with
                     diagnostics = physical.diagnostics @ product_diagnostics
                   };
              with_owner_lock t ~protect:true (fun () ->
                (match entry.phase with
                 | Closing current when phys_equal current attempt ->
                   (match outcome with
                    | Ok () ->
                      entry.phase <- Closed;
                      if
                        Option.for_all
                          entry.product_selection
                          ~f:Product_selection.custody_joined
                      then
                        t.retired_resources
                        <- List.filter t.retired_resources ~f:(fun current ->
                             not (phys_equal current entry))
                    | Error failure ->
                      entry.phase
                      <- Retained
                           (Some
                              { Retired_failure_receipt.producer_finished =
                                  attempt.finished
                              ; failure
                              ; origin = Option.value_exn attempt.origin
                              }))
                 | Retained _ | Closed | Closing _ ->
                   failwith "runtime close attempt displaced during cleanup");
                Eio.Promise.resolve attempt.finish outcome);
              outcome
          in
          match outcome with
          | Ok () -> ()
          | Error failure ->
            if Option.is_none !first_failure then first_failure := Some failure);
        Cleanup_batch.Original.Returned
          (match !first_failure with
           | None -> Ok ()
           | Some failure -> Error failure)
      with
      | exn -> Cleanup_batch.Original.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    let diagnostic =
      match original with
      | Cleanup_batch.Original.Returned _ -> None
      | Raised (exn, trace) -> Original_diagnostic.cleanup (Error (exn, trace))
    in
    { Cleanup_batch.original
    ; receipts = List.map receipts ~f:(fun receipt -> !receipt)
    ; diagnostic
    })
;;

let run_cleanup_request t cleanup =
  run_cleanup_request_reported t cleanup |> Cleanup_batch.resume
;;

let with_cleanup_request_reported t cleanup f =
  let primary = Scope_outcome.capture f in
  let batch = run_cleanup_request_reported t cleanup in
  { Scope_outcome.primary; cleanup = Cleanup_batch.cleanup batch }
;;

let with_cleanup_request t cleanup f =
  with_cleanup_request_reported t cleanup f |> Scope_outcome.resume
;;

let retain_construction_request_locked t cleanup =
  match cleanup.Cleanup_request.initialization_recovery with
  | None -> ()
  | Some recovery ->
    (* A nonempty request is an actually selected physical duty. An empty
       request neither acquires admission nor invents an initializer duty. *)
    retain_source_cleanup_locked
      t
      recovery
      ~acquired:(not (List.is_empty cleanup.entries))
;;

let with_construction_cleanup_request ?capture_cleanup t cleanup f =
  Owner_affinity.check_domain_exn t.affinity;
  let recovery = Recovery_data.create ~affinity:t.affinity ~kind:Construction in
  Cleanup_request.attach_initialization_recovery cleanup recovery;
  let primary =
    Scope_outcome.capture (fun () ->
      Eio.Cancel.protect (fun () ->
        Option.iter capture_cleanup ~f:(fun capture -> capture recovery));
      f ())
  in
  Eio.Cancel.protect (fun () ->
    let batch = run_cleanup_request_reported t cleanup in
    Recovery_data.record_batch recovery ~role:Construction_exit batch;
    let projection =
      Cleanup_projection.combine
        (Recovery_data.original_initialization_projection recovery)
        (Cleanup_batch.projection batch)
    in
    let actual = Cleanup_projection.cleanup projection in
    Recovery_data.finish_original recovery ~projection ~completion:Construction actual;
    finish_source_cleanup_fence t recovery;
    Scope_outcome.resume { primary; cleanup = actual })
;;

(* Pure selected-result transport. Every callback runs once at its original
   boundary; maps/binds preserve known carriers rather than resuming/re-catching
   them. These observations confer no ownership or cleanup authority. *)
module Observed_result = struct
  module CO = Agent_session.Close_observation

  let capture f =
    let observation = Physical_origin.capture_raw f in
    let failure =
      match CO.exit observation with
      | Returned (Ok _) -> None
      | Returned (Error error) ->
        Some (Agent_session.Callback_cleanup.Failure.Protocol error)
      | Raised (exn, trace) -> Some (Raised (exn, trace))
    in
    Physical_origin.with_boundary_failure observation failure
  ;;

  let raw f = capture (fun () -> CO.create (Returned (f ())) ~diagnostics:[] ())
  let return value = CO.create (Returned (Ok value)) ~diagnostics:[] ()

  let negative t =
    match CO.exit t with
    | Returned (Ok _) -> None
    | Returned (Error error) -> Some (CO.Returned (Error error))
    | Raised (exn, trace) -> Some (CO.Raised (exn, trace))
  ;;

  let bind t ~f =
    match CO.exit t with
    | Returned (Ok value) ->
      let next = capture (fun () -> f value) in
      CO.create
        (CO.exit next)
        ~diagnostics:(CO.diagnostics t @ CO.diagnostics next)
        ?primary:(CO.primary next)
        ()
    | Returned (Error error) ->
      CO.create
        (Returned (Error error))
        ~diagnostics:(CO.diagnostics t)
        ?primary:(CO.primary t)
        ()
    | Raised (exn, trace) ->
      CO.create
        (Raised (exn, trace))
        ~diagnostics:(CO.diagnostics t)
        ?primary:(CO.primary t)
        ()
  ;;

  let map t ~f = bind t ~f:(fun value -> return (f value))

  let of_primary primary ~diagnostics ?primary:diagnostic () =
    CO.create
      (match primary with
       | Scope_outcome.Returned result -> Returned result
       | Raised (exn, trace) -> Raised (exn, trace))
      ~diagnostics
      ?primary:diagnostic
      ()
  ;;

  let with_cleanup observed projection =
    let cleanup_origin = Cleanup_projection.origin projection in
    let exit, selected =
      match CO.exit observed with
      | Raised (exn, trace) -> CO.Raised (exn, trace), CO.primary observed
      | Returned (Error error) -> Returned (Error error), CO.primary observed
      | Returned (Ok value) ->
        (match Cleanup_projection.cleanup projection with
         | Never_acquired | Joined -> Returned (Ok value), None
         | Failed (failure, _) ->
           ( (match failure with
              | Protocol error -> CO.Returned (Error error)
              | Raised (exn, trace) -> Raised (exn, trace))
           , Option.bind cleanup_origin ~f:(fun origin -> origin.Physical_origin.primary)
           ))
    in
    CO.create
      exit
      ~diagnostics:
        (CO.diagnostics observed
         @ Option.value_map cleanup_origin ~default:[] ~f:(fun origin ->
           origin.Physical_origin.diagnostics))
      ?primary:selected
      ()
  ;;

  module Let_syntax = struct
    let bind = bind
    let map = map
  end
end

let with_source_cleanup_request_observed t extent ~role cleanup observed =
  let batch = run_cleanup_request_reported t cleanup in
  Source_extent.record_batch extent ~role batch;
  (* This source helper historically resumes Scope_outcome.cleanup, whose
     physical-first projection differs from direct Cleanup_batch.resume. *)
  let projection = Cleanup_batch.projection batch in
  Source_extent.record_projection extent projection;
  Observed_result.with_cleanup observed projection
;;

let with_source_cleanup_request t extent ~role cleanup f =
  let observed = Observed_result.raw f in
  with_source_cleanup_request_observed t extent ~role cleanup observed
  |> Agent_session.Close_observation.resume
;;

(* Private observation of this original call only: no stored callback, added
   cleanup action, or copied dependency failure. Caller values/raw exits survive. *)
let capture_source_operation_observed ?operation extent f =
  let step =
    Option.map
      operation
      ~f:(Recovery_data.register_original_step extent.Source_extent.recovery)
  in
  let primary = Scope_outcome.capture f in
  Option.iter step ~f:(fun step -> Original_cleanup_step.record_projection step primary);
  let origin =
    Option.bind step ~f:(fun step ->
      Cleanup_projection.origin (Original_cleanup_step.projection step ~acquired:true))
  in
  Agent_session.Close_observation.create
    (match primary with
     | Scope_outcome.Returned result -> Returned result
     | Raised (exn, trace) -> Raised (exn, trace))
    ~diagnostics:
      (Option.value_map origin ~default:[] ~f:(fun origin -> origin.diagnostics))
    ?primary:(Option.bind origin ~f:(fun origin -> origin.primary))
    ()
;;

let record_source_unit_observed extent ~operation (f : unit -> unit) =
  let step =
    Recovery_data.register_original_step extent.Source_extent.recovery operation
  in
  let primary =
    Scope_outcome.capture (fun () ->
      f ();
      Ok ())
  in
  Original_cleanup_step.record_projection step primary;
  let origin =
    Cleanup_projection.origin (Original_cleanup_step.projection step ~acquired:true)
  in
  Agent_session.Close_observation.create
    (match primary with
     | Scope_outcome.Returned (Ok ()) -> Returned ()
     | Returned (Error _) -> invalid_arg "unit source action returned a protocol error"
     | Raised (exn, trace) -> Raised (exn, trace))
    ~diagnostics:
      (Option.value_map origin ~default:[] ~f:(fun origin -> origin.diagnostics))
    ?primary:(Option.bind origin ~f:(fun origin -> origin.primary))
    ()
;;

let record_source_observation_observed extent observed =
  let projection =
    Cleanup_projection.of_primary
      ~origin:(Physical_origin.of_observation observed)
      ~acquired:true
      (Physical_origin.primary observed)
  in
  Source_extent.record_projection extent projection;
  observed
;;

let record_source_observation extent observed =
  record_source_observation_observed extent observed
  |> Agent_session.Close_observation.resume
;;

let record_source_cleanup ?operation extent f =
  record_source_observation extent (capture_source_operation_observed ?operation extent f)
;;

let initialization_capture_locked t recovery =
  Owner_affinity.check_domain_exn t.affinity;
  Initialization_capture_data.create ~actor:t.actor ~register:(fun boundary ->
    Recovery_data.register_initialization recovery boundary;
    (* The caller holds the original construction's Owner mutex. Retain before
       releasing that lock, including failed admission before an outer Borrow. *)
    retain_source_cleanup_locked t recovery ~acquired:false)
;;

let initialization_capture_for_source t recovery =
  Owner_affinity.check_domain_exn t.affinity;
  (* The actual prepared source already owns exclusion until its under-lock
     final clear. Registration here is same-domain factual data, not a lock or
     an early replacement of another source/Owner association. *)
  Initialization_capture_data.create
    ~actor:t.actor
    ~register:(Recovery_data.register_initialization recovery)
;;

let same_initialization_owner cleanup source =
  Option.exists cleanup.Cleanup_request.initialization_recovery ~f:(phys_equal source)
;;

let ensure_loaded_locked
      ?execution_access
      ?run_preparation
      ?original_actor
      ?administration
      t
      ~cleanup
  =
  let open Result.Let_syntax in
  let original_actor =
    Option.first_some
      original_actor
      (Option.bind run_preparation ~f:Agent_session.Run_preparation.original_actor)
  in
  let%bind () = check_original_actor original_actor in
  let check (runtime : Agent_session.Runtime_builder.t) =
    match runtime.check_execution with
    | None -> Ok ()
    | Some check -> check ()
  in
  match t.closed, loaded_runtime t with
  | true, _ -> Error (closed_error ())
  | false, None
    when Option.exists t.failed_source_cleanup ~f:(fun source ->
           not (same_initialization_owner cleanup source)) -> Error (background_busy ())
  | false, Some _
    when Option.exists t.binding ~f:(fun binding ->
           not (failed_source_allows_runtime_locked t binding)) ->
    Error (background_busy ())
  | false, None when retired_cleanup_pending t -> Error (retired_cleanup_error ())
  | false, _ when prepared_source_blocks_runtime ?execution_access ?administration t ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"session runtime has an exclusive prepared source lifetime"
         ~retryable:true
         ())
  | false, _ when resource_retirement_blocks_admission t ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"session runtime is waiting for background cleanup"
         ~retryable:true
         ())
  | false, Some _ when Option.is_some t.foreground_retirement ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"session runtime retirement must finish before execution resumes"
         ~retryable:true
         ())
  | false, Some runtime -> check runtime
  | false, None ->
    let built =
      match t.build, run_preparation, original_actor with
      | Ordinary build, None, None ->
        Result.map (build ()) ~f:(fun runtime -> Runtime_binding.Legacy runtime)
      | For_runs build, preparation, None ->
        Result.map (build preparation) ~f:(fun runtime -> Runtime_binding.Legacy runtime)
      | For_authenticated_runs build, preparation, original_actor ->
        Result.map (build ~original_actor preparation) ~f:(fun runtime ->
          Runtime_binding.Legacy runtime)
      | For_factory build, preparation, original_actor ->
        let recovery =
          match cleanup.Cleanup_request.initialization_recovery with
          | Some recovery -> recovery
          | None -> invalid_arg "Factory construction lacks its original cleanup request"
        in
        Result.map
          (build
             ~initialization_cleanup:(initialization_capture_locked t recovery)
             ~original_actor
             preparation)
          ~f:(fun attachment -> Runtime_binding.Attached attachment)
      | Ordinary _, Some _, None | (Ordinary _ | For_runs _), _, Some _ ->
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"runtime constructor does not support scoped run admission"
             ~retryable:false
             ())
    in
    (match built with
     | Error _ as failure -> failure
     | Ok binding ->
       let runtime = Runtime_binding.runtime binding in
       (match
          Result.bind (check_original_actor original_actor) ~f:(fun () ->
            install
              ?initialization_recovery:cleanup.Cleanup_request.initialization_recovery
              ?run_preparation
              ?original_actor
              t
              binding)
        with
        | Ok () ->
          if resource_retirement_pending t
          then Error (background_busy ())
          else check runtime
        | Error _ as failure ->
          (* The admitted install failure remains primary; exact constructor
             cleanup and the original resource close still run. *)
          if not (Option.exists t.binding ~f:(phys_equal binding))
          then release_runtime_locked t cleanup binding;
          failure))
;;

(* Detach execution before cleanup. The same retired resource remains owned
   until close returns positively, so a failed factual finish can be retried
   without exposing that runtime as loaded again. *)
let retire_runtime_locked t cleanup =
  if factual_cleanup_pending t
  then Ok ()
  else (
    match
      (match
         Agent_session.Session_actor.set_runtime_worker
           t.actor
           ~worker:None
           ~inference:None
       with
       | Ok () -> ()
       | Error error -> raise (Cleanup_failed error));
      let previous = t.binding in
      t.binding <- None;
      Option.iter previous ~f:(release_runtime_locked t cleanup);
      retry_retired_resources_locked t cleanup
    with
    | () -> Ok ()
    | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
;;

let raise_cleanup = function
  | Ok () -> ()
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let foreground_retirement_finished t =
  Option.exists t.foreground_retirement ~f:(fun retirement ->
    Agent_session.Session_actor.Runtime_retirement.is_finished retirement
    &&
    match Agent_session.Session_actor.Runtime_retirement.await retirement with
    | Ok () -> true
    | Error error -> raise (Cleanup_failed error))
;;

(* A successful unloaded retirement is historical DATA. Join outside the mutex,
   then clear only that exact handle while the runtime is still absent. *)
let completed_unloaded_retirement t =
  let candidate =
    with_owner_lock t ~protect:true (fun () ->
      match t.closed, t.unloading, loaded_runtime t, t.foreground_retirement with
      | false, None, None, Some retirement
        when Agent_session.Session_actor.Runtime_retirement.is_finished retirement ->
        Some retirement
      | (true | false), (Some _ | None), (Some _ | None), (Some _ | None) -> None)
  in
  Option.bind candidate ~f:(fun retirement ->
    match Agent_session.Session_actor.Runtime_retirement.await retirement with
    | Ok () -> Some retirement
    | Error _ -> None)
;;

let forget_completed_retirement_locked t completed_retirement =
  match
    completed_retirement, t.foreground_retirement, t.closed, t.unloading, loaded_runtime t
  with
  | Some completed, Some current, false, None, None
    when phys_equal completed current
         && (not (resource_retirement_pending t))
         && not (retired_cleanup_pending t) -> replace_foreground_retirement t None
  | (Some _ | None), (Some _ | None), (true | false), (Some _ | None), (Some _ | None) ->
    ()
;;

let source_lane_idle t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "Runtime_owner.source_lane_idle: foreign domain";
  Eio.Mutex.use_ro t.mutex (fun () ->
    (not t.closed)
    && (not (failed_source_cleanup_pending t))
    && (not (retired_cleanup_pending t))
    && (not (resource_retirement_blocks_admission t))
    && Option.is_none t.prepared_source
    && Option.is_none t.administration_transition
    && List.is_empty t.background_leases
    && Option.value_map t.foreground_retirement ~default:true ~f:(fun retirement ->
      Agent_session.Session_actor.Runtime_retirement.is_finished retirement
      && Result.is_ok (Agent_session.Session_actor.Runtime_retirement.await retirement)))
;;

module Lease_admission = struct
  type ('value, 'result) t =
    | No_work of 'result
    | Borrow of 'value * Agent_session.Runtime_builder.t option * source_stage option
end

module Admission_protection = struct
  type t =
    | Protected
    | Cancellable

  let protect = function
    | Protected -> true
    | Cancellable -> false
  ;;
end

module Lease_invocation = struct
  module CO = Agent_session.Close_observation

  type ('input, 'output) t =
    | Raw of
        (recovery:Recovery_data.t -> 'input -> ('output, Agent_protocol.Error.t) result)
    | Observed of
        (recovery:Recovery_data.t
         -> 'input
         -> ('output, Agent_protocol.Error.t) result CO.t)

  let capture = Observed_result.capture
  let raw = Observed_result.raw

  let invoke t ~recovery input =
    match t with
    | Raw f -> raw (fun () -> f ~recovery input)
    | Observed f -> capture (fun () -> f ~recovery input)
  ;;

  (* Only failure DATA changes value type. No positive value or callback is
     manufactured to pass an admission result into the final result slot. *)
  let negative observation =
    let exit =
      match CO.exit observation with
      | Returned (Ok _) -> None
      | Returned (Error error) -> Some (CO.Returned (Error error))
      | Raised (exn, trace) -> Some (CO.Raised (exn, trace))
    in
    Option.map exit ~f:(fun exit ->
      CO.create
        exit
        ~diagnostics:(CO.diagnostics observation)
        ?primary:(CO.primary observation)
        ())
  ;;

  let before t ~check =
    match t with
    | Raw f ->
      Raw
        (fun ~recovery input ->
          let%bind.Result () = check input in
          f ~recovery input)
    | Observed f ->
      Observed
        (fun ~recovery input ->
          let checked = raw (fun () -> check input) in
          match negative checked with
          | Some negative -> negative
          | None ->
            let observation = capture (fun () -> f ~recovery input) in
            CO.create
              (CO.exit observation)
              ~diagnostics:(CO.diagnostics checked @ CO.diagnostics observation)
              ?primary:(CO.primary observation)
              ())
  ;;
end

module Lease_selection = struct
  module CO = Agent_session.Close_observation

  module Stage = struct
    type t =
      | Admission
      | Admission_batch
      | Before_callback
      | Callback
      | After_callback
      | Scope_exit_before_entry
      | Scope_exit
    [@@deriving equal, sexp_of]
  end

  type 'a primary_source =
    | Admission of ('a, Agent_protocol.Error.t) result CO.t
    | Admission_batch of ('a, Agent_protocol.Error.t) result CO.t
    | Before_callback of ('a, Agent_protocol.Error.t) result CO.t
    | Callback of ('a, Agent_protocol.Error.t) result CO.t
    | After_callback of ('a, Agent_protocol.Error.t) result CO.t
    | Scope_exit_before_entry of ('a, Agent_protocol.Error.t) result CO.t
    | Scope_exit of ('a, Agent_protocol.Error.t) result CO.t

  let observation = function
    | Admission observation
    | Admission_batch observation
    | Before_callback observation
    | Callback observation
    | After_callback observation
    | Scope_exit_before_entry observation
    | Scope_exit observation -> observation
  ;;

  let stage = function
    | Admission _ -> Stage.Admission
    | Admission_batch _ -> Stage.Admission_batch
    | Before_callback _ -> Stage.Before_callback
    | Callback _ -> Stage.Callback
    | After_callback _ -> Stage.After_callback
    | Scope_exit_before_entry _ -> Stage.Scope_exit_before_entry
    | Scope_exit _ -> Stage.Scope_exit
  ;;

  let resumed report ~selected ~diagnostics ~cleanup =
    let outcome = Scope_outcome.capture (fun () -> Scope_outcome.resume report) in
    let origin = Cleanup_projection.origin cleanup in
    let cleanup_diagnostics =
      Option.value_map origin ~default:[] ~f:(fun origin ->
        origin.Physical_origin.diagnostics)
    in
    let primary =
      match Scope_outcome.primary report with
      | Raised _ | Returned (Error _) ->
        Option.bind selected ~f:(fun source -> CO.primary (observation source))
      | Returned (Ok _) ->
        (match Scope_outcome.cleanup report with
         | Never_acquired | Joined -> None
         | Failed _ ->
           Option.bind origin ~f:(fun origin -> origin.Physical_origin.primary))
    in
    CO.create
      (match outcome with
       | Scope_outcome.Returned result -> CO.Returned result
       | Raised (exn, trace) -> CO.Raised (exn, trace))
      ~diagnostics:(diagnostics @ cleanup_diagnostics)
      ?primary
      ()
  ;;
end

module Lease_scope_outcome = struct
  type 'a t =
    { report : 'a Scope_outcome.t
    ; recovery : Recovery_data.t
    ; primary_stage : Lease_selection.Stage.t option
    ; observation : ('a, Agent_protocol.Error.t) result Agent_session.Close_observation.t
    }
end

module Lease_outcome = struct
  type 'a t = 'a Lease_scope_outcome.t

  module Primary_stage = Lease_selection.Stage

  let outcome t = t.Lease_scope_outcome.report
  let cleanup t = t.Lease_scope_outcome.recovery
  let primary_stage t = t.Lease_scope_outcome.primary_stage
  let observation t = t.Lease_scope_outcome.observation
end

let provider_basis_locked t purpose resources =
  let module Basis = Background_lease.Provider_basis in
  match purpose with
  | Lease_purpose.Selection.Native_resource identity ->
    let binding =
      Option.bind t.binding ~f:(fun binding ->
        Option.some_if
          (Option.exists
             (Runtime_binding.native_identity binding)
             ~f:(Agent_session.Runtime_builder.Native_identity.equal identity))
          (Basis.Binding binding))
    in
    let restored =
      Option.bind t.restored_policy ~f:(fun anchor ->
        Option.some_if
          (Agent_session.Runtime_builder.Native_identity.equal
             identity
             (Agent_session.Runtime_builder.Product.native_identity
                (Session_policy_resource.Lease.product anchor.loan)))
          (Basis.Restored anchor))
    in
    Result.of_option
      (Option.first_some
         binding
         (Option.first_some
            restored
            (List.find_map t.native_loans ~f:(fun loan ->
               match loan.Native_loan.phase with
               | Available
                 when Agent_session.Runtime_builder.Native_identity.equal
                        identity
                        loan.identity -> Some (Basis.Native loan)
               | Available | Retiring | Closing _ | Released -> None))))
      ~error:
        (Agent_protocol.Error.create
           Conflict
           ~message:"native dependency lost its actual provider basis"
           ~retryable:false
           ())
  | Exclusive | Delegation_resource | Policy_resource _ ->
    (match resources with
     | None -> Ok Basis.Lifetime_only
     | Some runtime ->
       (match t.binding with
        | Some binding when Runtime_binding.matches binding runtime ->
          Ok (Basis.Binding binding)
        | Some _ | None ->
          (match t.restored_policy with
           | Some anchor when phys_equal (Restored_policy_anchor.runtime anchor) runtime
             -> Ok (Basis.Restored anchor)
           | Some _ | None ->
             Error
               (Agent_protocol.Error.create
                  Conflict
                  ~message:"dependency lost its actual provider runtime"
                  ~retryable:false
                  ()))))
;;

let relation_report (lease : background_lease) =
  Option.map lease.relation ~f:(fun relation -> relation.Background_lease.Relation.report)
;;

let retirement_relation_selections (recovery : Recovery_data.t) (lease : background_lease)
  =
  let captured =
    match recovery.phase with
    | Original_running captured -> captured
    | Complete completed -> completed.captured
    | Retrying reserved -> reserved.before.captured
  in
  List.filter captured.leases_rev ~f:(fun (selected : Recovery_data.lease) ->
    phys_equal selected.finished lease.finished
    && phys_equal selected.recovery lease.recovery
    && Option.equal phys_equal selected.relation (relation_report lease)
    &&
    match selected.role with
    | Source_lexical -> false
    | Retirement_lease _ -> true)
;;

let retirement_relation_ready (recovery : Recovery_data.t) (lease : background_lease) =
  if Background_lease.is_active lease || Option.is_none (Eio.Promise.peek lease.finished)
  then false
  else (
    match lease.relation, lease.recovery.phase with
    | None, _ | Some _, (Original_running _ | Retrying _) -> false
    | Some relation, Complete _ ->
      let selections = retirement_relation_selections recovery lease in
      let cancellations =
        List.for_all selections ~f:(fun (selected : Recovery_data.lease) ->
          let control_ready =
            match selected.join_current_control with
            | None | Some (Relation_evidence.Join_control.Settled _) -> true
            | Some (Blocking _) -> false
          in
          control_ready
          &&
          match selected.role with
          | Source_lexical -> false
          | Retirement_lease { cancel; _ } ->
            (match cancel.original, selected.retry_cancel with
             | Some (Returned (Ok ())), _ -> true
             | None, Some actual ->
               (match Agent_session.Close_observation.exit actual with
                | Returned (Ok ()) -> true
                | Returned (Error _) | Raised _ -> false)
             | None, None | Some (Returned (Error _) | Raised _), _ -> false))
      in
      let receipt =
        match Relation_evidence.selected_native_evidence relation.report with
        | None | Some { complete = false; _ } -> false
        | Some { complete = true; duties } -> Cleanup_evidence.custody_joined duties
      in
      let consumer =
        match relation.report.operation with
        | None -> false
        | Some { selection = Detached_handoff; _ } -> true
        | Some { selection = Active_revocation; _ } ->
          (match relation.report.consumer with
           | Unfinished -> false
           | Observed { guard; evidence } ->
             (match Recovery_data.Relation_guard.check guard with
              | Busy | Stale -> false
              | Current ->
                Result.is_ok
                  (Cleanup_snapshot.observation
                     (Recovery_data.Observation.snapshot evidence))
                && Cleanup_evidence.custody_joined evidence
                && Original_cleanup_report.diagnostics_complete
                     (Cleanup_evidence.child_view evidence ~diagnostics_complete:true)))
      in
      let provider = Recovery_data.Observation.scope lease.recovery in
      (not (List.is_empty selections))
      && cancellations
      && receipt
      && consumer
      && Result.is_ok
           (Cleanup_snapshot.observation (Recovery_data.Observation.snapshot provider))
      && Cleanup_evidence.custody_joined provider
      && Original_cleanup_report.diagnostics_complete
           (Cleanup_evidence.child_view provider ~diagnostics_complete:true))
;;

(* Metadata may remain until completion publication. These exact enrolled and
   proof-complete detached relations are not physical borrowers; this query does
   not remove them or certify a not-yet-completed final report. *)
let retirement_background_ready_locked owner ~recovery =
  List.for_all owner.background_leases ~f:(retirement_relation_ready recovery)
;;

let reap_retirement_relations_locked owner ~recovery =
  match recovery.Recovery_data.phase with
  | Original_running _ | Retrying _ -> ()
  | Complete _ ->
    let evidence = Recovery_data.Observation.scope recovery in
    if
      Result.is_ok
        (Cleanup_snapshot.observation (Recovery_data.Observation.snapshot evidence))
      && Cleanup_evidence.custody_joined evidence
      && Original_cleanup_report.diagnostics_complete
           (Cleanup_evidence.child_view evidence ~diagnostics_complete:true)
    then
      owner.background_leases
      <- List.filter owner.background_leases ~f:(fun lease ->
           not (retirement_relation_ready recovery lease))
;;

let with_lease_scope_callback_internal
      ?consumer
      ?execution_access
      ?lease_wrapper
      ?(cleanup_request = Cleanup_request.create ())
      ?(retire = ignore)
      ?(capture_recovery = fun ~finished:_ _ -> ())
      ?(before_admission = fun _ -> Observed_result.return ())
      ?after_acquired
      ?(admission_protection = Admission_protection.Protected)
      ?(purpose = Lease_purpose.Selection.Exclusive)
      t
      ~survives_stop
      ~admit
      callback
  =
  let module CO = Agent_session.Close_observation in
  let recovery = Recovery_data.create ~affinity:t.affinity ~kind:Lease in
  Cleanup_request.attach_initialization_recovery cleanup_request recovery;
  let completed = ref None in
  let acquired_lease = ref None in
  let selected_primary = ref None in
  let original_entered = ref false in
  let diagnostics = ref [] in
  let resumed_cleanup = ref Cleanup_projection.empty in
  let record source observation =
    diagnostics := !diagnostics @ CO.diagnostics observation;
    selected_primary := Some (source observation)
  in
  let record_negative source observation =
    match Lease_invocation.negative observation with
    | None -> diagnostics := !diagnostics @ CO.diagnostics observation
    | Some observation -> record source observation
  in
  let check source f =
    let observation =
      Lease_invocation.raw (fun () ->
        f ();
        Ok ())
    in
    record_negative source observation;
    CO.resume observation
  in
  let acquired = ref false in
  let admission_projection = ref Cleanup_projection.empty in
  let actual_projection = ref Cleanup_projection.empty in
  let record_projection projection =
    actual_projection := Cleanup_projection.combine !actual_projection projection
  in
  let seal ~projection report =
    resumed_cleanup := projection;
    if not (Recovery_data.is_finished recovery)
    then
      Recovery_data.finish_original
        recovery
        ~projection
        ~completion:(Lease (if !acquired then Borrow_published else No_borrow))
        (Scope_outcome.cleanup report);
    completed := Some report;
    finish_source_cleanup_fence t recovery
  in
  let report =
    try
      Eio.Cancel.sub (fun context ->
        (* This actual entry fact separates parent-context refusal from an
           escaping entered scope. Missing capture/callback is not entry proof. *)
        original_entered := true;
        let active = Atomic.make true in
        let finished, finish = Eio.Promise.create () in
        let lease =
          { Background_lease.finished
          ; finish
          ; phase = Active
          ; relation = None
          ; recovery
          ; survives_stop
          ; purpose = Lease_purpose.Exclusive
          ; resources = None
          ; cancel =
              (fun () ->
                match Atomic.get active with
                | true ->
                  retire ();
                  Eio.Cancel.cancel context Exit
                | false -> ())
          }
        in
        (* Established BEFORE admission can publish this lease or cleanup can
         escape. Every acquired exit runs this exact finalizer. *)
        let cleanup () =
          Eio.Cancel.protect (fun () ->
            Atomic.set active false;
            actual_projection
            := Cleanup_projection.combine
                 (Recovery_data.original_preparation_projection recovery)
                 !admission_projection;
            if not !acquired
            then !actual_projection
            else (
              let retire_step =
                Recovery_data.register_original_step recovery Lease_retire
              in
              let _retired =
                Original_cleanup_step.capture retire_step (fun () ->
                  retire ();
                  Ok ())
              in
              record_projection
                (Original_cleanup_step.projection retire_step ~acquired:true);
              let cleanup = Cleanup_request.create () in
              let detach_step =
                Recovery_data.register_original_step recovery Lease_detach
              in
              let detached =
                try
                  with_owner_lock t ~protect:true (fun () ->
                    lease.phase <- Background_lease.Phase.Detached;
                    if Option.is_none lease.relation
                    then
                      t.background_leases
                      <- List.filter t.background_leases ~f:(fun current ->
                           not (phys_equal current lease));
                    Option.iter
                      lease.resources
                      ~f:(request_retired_resource_locked t cleanup);
                    (match lease.purpose with
                     | Shared_native identity ->
                       List.iter t.retired_resources ~f:(fun entry ->
                         if
                           Option.exists
                             (Runtime_binding.native_identity entry.binding)
                             ~f:
                               (Agent_session.Runtime_builder.Native_identity.equal
                                  identity)
                         then Cleanup_request.add cleanup entry)
                     | Exclusive | Shared_resource _ -> ());
                    match
                      ( t.closed
                      , List.filter t.background_leases ~f:Background_lease.is_active
                      , t.before_unload
                      , t.unloading )
                    with
                    | true, [], None, None
                      when (not (resource_retirement_pending t))
                           && foreground_retirement_finished t ->
                      retire_runtime_locked t cleanup
                    | _ -> Ok ())
                with
                | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
              in
              Original_cleanup_step.record_projection
                detach_step
                (match detached with
                 | Ok () -> Scope_outcome.Returned (Ok ())
                 | Error (exn, backtrace) -> Raised (exn, backtrace));
              record_projection
                (Original_cleanup_step.projection detach_step ~acquired:true);
              let batch = run_cleanup_request_reported t cleanup in
              record_projection (Cleanup_batch.projection batch);
              Recovery_data.record_batch recovery ~role:Lease_detachment batch;
              let cleaned =
                match batch.Cleanup_batch.original with
                | Returned outcome -> Result.is_ok outcome
                | Raised _ -> false
              in
              let check_step =
                Recovery_data.register_original_step recovery Lease_completion_check
              in
              let _checked =
                Original_cleanup_step.capture check_step (fun () ->
                  with_owner_lock t ~protect:true (fun () ->
                    let report_positive =
                      match Cleanup_projection.cleanup !actual_projection with
                      | Scope_outcome.Cleanup.Never_acquired | Joined -> true
                      | Failed _ -> false
                    in
                    if
                      report_positive
                      && Result.is_ok detached
                      && cleaned
                      && t.closed
                      && List.is_empty t.background_leases
                      && List.is_empty t.retired_resources
                      && Option.is_none t.restored_policy
                      && List.is_empty t.native_loans
                      && (not (policy_cleanup_pending t))
                      && (not (factual_cleanup_pending t))
                      && Option.is_none t.unloading
                      && (not (resource_retirement_pending t))
                      && Option.is_none t.prepared_source
                      && Option.is_none t.administration_transition
                      && Option.is_none t.before_unload
                      && foreground_retirement_finished t
                      && not (failed_source_cleanup_pending t)
                    then t.close_finished <- true);
                  Ok ())
              in
              record_projection
                (Original_cleanup_step.projection check_step ~acquired:true);
              !actual_projection))
        in
        let primary =
          Scope_outcome.capture (fun () ->
            let admission_observation =
              let open Observed_result in
              let%bind () =
                Observed_result.raw (fun () ->
                  capture_recovery ~finished recovery;
                  Ok ())
              in
              let%bind () =
                Observed_result.capture (fun () -> before_admission recovery)
              in
              Observed_result.raw (fun () ->
                let open Result.Let_syntax in
                let%bind decision =
                  with_owner_lock
                    t
                    ~protect:(Admission_protection.protect admission_protection)
                    (fun () ->
                       let open Result.Let_syntax in
                       let%bind () =
                         if prepared_source_blocks_runtime ?execution_access t
                         then Error (background_busy ())
                         else Ok ()
                       in
                       let%bind decision = admit () in
                       match decision with
                       | Lease_admission.No_work _ -> Ok decision
                       | Borrow (_, resources, source) ->
                         let%bind relation =
                           match consumer with
                           | None -> Ok None
                           | Some consumer ->
                             let%bind () =
                               Native_consumer.check_provider consumer ~provider:t
                             in
                             let%map basis = provider_basis_locked t purpose resources in
                             Some
                               (Background_lease.Relation.create
                                  ~consumer
                                  ~basis
                                  ~mode:
                                    (match purpose with
                                     | Native_resource _ -> Relation_evidence.Mode.Native
                                     | Exclusive | Delegation_resource | Policy_resource _
                                       -> Ordinary))
                         in
                         let%map purpose =
                           match purpose, resources, t.binding with
                           | Delegation_resource, Some runtime, Some binding
                             when Runtime_binding.matches binding runtime ->
                             Ok
                               (Option.value_map
                                  (Runtime_binding.product binding)
                                  ~default:Lease_purpose.Exclusive
                                  ~f:(fun product ->
                                    Lease_purpose.Shared_resource product))
                           | Policy_resource product, Some _, _ ->
                             Ok (Shared_resource product)
                           | Policy_resource _, None, _ ->
                             Error
                               (Agent_protocol.Error.invalid_request
                                  "policy resource lease requires its actual runtime")
                           | Native_resource identity, _, _ -> Ok (Shared_native identity)
                           | Exclusive, _, _
                           | Delegation_resource, (Some _ | None), (Some _ | None) ->
                             Ok Lease_purpose.Exclusive
                         in
                         lease.relation <- relation;
                         lease.resources <- resources;
                         lease.purpose <- purpose;
                         Option.iter source ~f:(fun stage ->
                           stage.cancel_lease <- Some lease.cancel);
                         t.background_leases <- lease :: t.background_leases;
                         acquired_lease := Some lease;
                         Option.iter lease_wrapper ~f:(fun selected ->
                           selected := Some lease);
                         acquired := true;
                         decision)
                in
                (* The actual Borrow is registered and its finalizer is already
                   installed. Capture outside the mutex, before admission cleanup
                   or cancellation/body gates can skip the acquired caller value.
                   Keep a raised hook inside admission_primary so the selected
                   admission batch still runs below. *)
                (match decision with
                 | Lease_admission.No_work _ -> ()
                 | Borrow (value, _, _) ->
                   Eio.Cancel.protect (fun () ->
                     Option.iter after_acquired ~f:(fun capture -> capture value)));
                Ok decision)
            in
            record_negative
              (fun observation -> Lease_selection.Admission observation)
              admission_observation;
            let admission_primary = Physical_origin.primary admission_observation in
            let batch = run_cleanup_request_reported t cleanup_request in
            admission_projection := Cleanup_batch.projection batch;
            Recovery_data.record_batch recovery ~role:Lease_admission batch;
            (* Returned admission-close failure remains secondary and does not newly
           prevent admitted work. An unforeseen cleanup escape prevents work,
           but never masks an earlier original admission primary. *)
            let admitted =
              match admission_primary with
              | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
              | Returned (Error _ as failure) -> failure
              | Returned (Ok decision) ->
                (match batch.original with
                 | Returned _ -> Ok decision
                 | Raised (exn, backtrace) ->
                   let origin = Cleanup_batch.resume_origin batch in
                   let observation =
                     CO.create
                       (CO.Raised (exn, backtrace))
                       ~diagnostics:
                         (Option.value_map origin ~default:[] ~f:(fun origin ->
                            origin.Physical_origin.diagnostics))
                       ?primary:
                         (Option.bind origin ~f:(fun origin ->
                            origin.Physical_origin.primary))
                       ()
                   in
                   record
                     (fun observation -> Lease_selection.Admission_batch observation)
                     observation;
                   Exn.raise_with_original_backtrace exn backtrace)
            in
            match admitted with
            | Error _ as failure -> failure
            | Ok (Lease_admission.No_work result) ->
              let%map.Result () =
                check
                  (fun observation -> Lease_selection.Before_callback observation)
                  Eio.Fiber.check
              in
              result
            | Ok (Borrow (runtime, _, _)) ->
              let before =
                Lease_invocation.raw (fun () ->
                  Eio.Fiber.check ();
                  if resource_retirement_pending t
                  then Error (background_busy ())
                  else Ok ())
              in
              record_negative
                (fun observation -> Lease_selection.Before_callback observation)
                before;
              let%bind.Result () = CO.resume before in
              let observation = Lease_invocation.invoke callback ~recovery runtime in
              record (fun observation -> Lease_selection.Callback observation) observation;
              let result = CO.resume observation in
              (match result with
               | Error _ -> ()
               | Ok _ ->
                 ignore
                   (check
                      (fun observation -> Lease_selection.After_callback observation)
                      Eio.Fiber.check
                    : (unit, Agent_protocol.Error.t) result));
              result)
        in
        let cleaned =
          match cleanup () with
          | report -> report
          | exception exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            (* Retire/detach/check and batch failures were already captured at
             their actual owners. This is the remaining finalizer/registration
             escape, not a resumed dependency or callback primary. *)
            let step =
              Recovery_data.register_original_step recovery Lease_cleanup_escape
            in
            Original_cleanup_step.record_projection
              step
              (Scope_outcome.Raised (exn, backtrace));
            Cleanup_projection.combine
              !actual_projection
              (Original_cleanup_step.projection step ~acquired:true)
        in
        let cleaned =
          Cleanup_projection.combine
            (Cleanup_projection.combine
               cleaned
               (Recovery_data.original_initialization_projection recovery))
            (Recovery_data.original_callback_projection recovery)
        in
        let report =
          { Scope_outcome.primary; cleanup = Cleanup_projection.cleanup cleaned }
        in
        (* Actual cleanup DATA is complete BEFORE this actual lexical unit join.
         The unit signal is never an independent successful-cleanup receipt. *)
        Eio.Cancel.protect (fun () ->
          seal ~projection:cleaned report;
          Eio.Promise.resolve lease.finish ());
        report)
    with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      let observation =
        Physical_origin.with_boundary_failure
          (CO.create (CO.Raised (exn, backtrace)) ~diagnostics:[] ())
          (Some (Agent_session.Callback_cleanup.Failure.Raised (exn, backtrace)))
      in
      record
        (fun observation ->
           if !original_entered
           then Lease_selection.Scope_exit observation
           else Lease_selection.Scope_exit_before_entry observation)
        observation;
      let cleanup =
        match !completed with
        | Some _ -> Recovery_data.original_projection recovery
        | None ->
          Cleanup_projection.combine
            (Recovery_data.original_preparation_projection recovery)
            !admission_projection
      in
      let cleanup =
        Cleanup_projection.combine
          (Cleanup_projection.combine
             cleanup
             (Recovery_data.original_initialization_projection recovery))
          (Recovery_data.original_callback_projection recovery)
      in
      let report =
        { Scope_outcome.primary = Raised (exn, backtrace)
        ; cleanup = Cleanup_projection.cleanup cleanup
        }
      in
      seal ~projection:cleanup report;
      report
  in
  let observation =
    Lease_selection.resumed
      report
      ~selected:!selected_primary
      ~diagnostics:!diagnostics
      ~cleanup:!resumed_cleanup
  in
  let outcome =
    { Lease_scope_outcome.report
    ; recovery
    ; primary_stage = Option.map !selected_primary ~f:Lease_selection.stage
    ; observation
    }
  in
  (* The complete real outcome owns SAME R/aliases before metadata transfer.
     A native bracket is still unsealed here and will publish its own report. *)
  Option.iter !acquired_lease ~f:(fun lease ->
    if not (phys_equal lease.recovery outcome.recovery)
    then invalid_arg "returned lease report changed its actual recovery";
    handoff_completed_dependency t lease);
  outcome
;;

let with_lease_scope_reported_internal
      ?consumer
      ?execution_access
      ?lease_wrapper
      ?cleanup_request
      ?retire
      ?capture_recovery
      ?before_admission
      ?after_acquired
      ?admission_protection
      ?purpose
      t
      ~survives_stop
      ~admit
      f
  =
  with_lease_scope_callback_internal
    ?consumer
    ?execution_access
    ?lease_wrapper
    ?cleanup_request
    ?retire
    ?capture_recovery
    ?before_admission:
      (Option.map before_admission ~f:(fun f recovery ->
         Observed_result.raw (fun () -> f recovery)))
    ?after_acquired
    ?admission_protection
    ?purpose
    t
    ~survives_stop
    ~admit
    (Lease_invocation.Raw f)
;;

let with_lease_reported_internal
      ?consumer
      ?execution_access
      ?lease_wrapper
      ?cleanup_request
      ?retire
      ?capture_recovery
      ?before_admission
      ?after_acquired
      ?admission_protection
      ?purpose
      t
      ~survives_stop
      ~admit
      f
  =
  let scope =
    with_lease_scope_reported_internal
      ?consumer
      ?execution_access
      ?lease_wrapper
      ?cleanup_request
      ?retire
      ?capture_recovery
      ?before_admission
      ?after_acquired
      ?admission_protection
      ?purpose
      t
      ~survives_stop
      ~admit
      (fun ~recovery:_ value -> f value)
  in
  scope.Lease_scope_outcome.report
;;

let with_lease_internal
      ?execution_access
      ?before_admission
      ?cleanup_request
      ?retire
      ?purpose
      t
      ~survives_stop
      ~admit
      f
  =
  with_lease_reported_internal
    ?execution_access
    ?before_admission
    ?cleanup_request
    ?retire
    ?purpose
    t
    ~survives_stop
    ~admit
    f
  |> Scope_outcome.resume
;;

let with_lease_reported
      ?consumer
      ?execution_access
      ?retire
      ?admission_protection
      t
      ~survives_stop
      ~admit
      f
  =
  with_lease_reported_internal
    ?consumer
    ?execution_access
    ?retire
    ?admission_protection
    t
    ~survives_stop
    ~admit:(fun () ->
      Result.map (admit ()) ~f:(fun (value, resources) ->
        Lease_admission.Borrow (value, resources, None)))
    f
;;

let with_source_lease_callback_internal
      ?after_acquired
      t
      ~recovery
      ~admission_protection
      ~retire
      ~admit
      f
  =
  with_lease_scope_callback_internal
    ?after_acquired
    ~capture_recovery:(fun ~finished lease_recovery ->
      ignore
        (Recovery_data.register_lease
           recovery
           ~position:Source_lexical
           ~finished
           ~recovery:lease_recovery
         : Recovery_data.lease))
    t
    ~admission_protection
    ~survives_stop:false
    ~retire
    ~admit:(fun () ->
      Result.map (admit ()) ~f:(fun (stage, resources) ->
        Lease_admission.Borrow (stage, resources, Some stage)))
    f
;;

let with_lease
      ?execution_access
      ?cleanup_request
      ?retire
      ?purpose
      t
      ~survives_stop
      ~admit
      f
  =
  with_lease_internal
    ~before_admission:(fun _ ->
      Agent_session.Session_actor.exclude_effect_deferred_execution t.actor)
    ?execution_access
    ?cleanup_request
    ?retire
    ?purpose
    t
    ~survives_stop
    ~admit:(fun () ->
      Result.map (admit ()) ~f:(fun (value, resources) ->
        Lease_admission.Borrow (value, resources, None)))
    f
;;

let unload_locked t cleanup =
  if factual_cleanup_pending t
  then Error (retired_cleanup_error ())
  else (
    match t.background_leases, t.binding with
    | _ :: _, _ -> Error (background_busy ())
    | [], None ->
      retry_retired_resources_locked t cleanup;
      Ok ()
    | [], Some runtime ->
      let open Result.Let_syntax in
      let%map () =
        Agent_session.Session_actor.set_runtime_worker
          t.actor
          ~worker:None
          ~inference:None
      in
      t.binding <- None;
      release_runtime_locked t cleanup runtime;
      retry_retired_resources_locked t cleanup)
;;

let unload t =
  let open Result.Let_syntax in
  let cleanup = Cleanup_request.create () in
  let%bind () =
    with_cleanup_request t cleanup (fun () ->
      with_owner_lock t ~protect:true (fun () ->
        match t.unloading with
        | Some _ -> Error (background_busy ())
        | None when resource_retirement_pending t -> Error (background_busy ())
        | None when factual_cleanup_pending t -> Error (retired_cleanup_error ())
        | None ->
          let result = unload_locked t cleanup in
          (match result with
           | Ok () ->
             Option.iter t.administration_transition ~f:Administration_transition.retire
           | Error _ -> ());
          result))
  in
  let%bind () = close_restored_policy t in
  close_pending_policy_resource t
;;

let drain_policy_holder ?attempt t (holder : native_consumer Policy_claim_holder.t) =
  let module A = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let current () =
    with_owner_lock t ~protect:true (fun () ->
      match t.policy_claim with
      | Some actual when phys_equal actual holder -> Ok true
      | None -> Ok false
      | Some _ -> Error (retired_cleanup_error ()))
  in
  let%bind owned = current () in
  if not owned
  then Ok ()
  else (
    let%bind () =
      if Option.exists holder.wrapper.resources ~f:(phys_equal holder.runtime)
      then Ok ()
      else Error (retired_cleanup_error ())
    in
    let%bind () =
      match A.Delegated_policy_claim.Capture.disposition holder.capture with
      | Released -> Ok ()
      | Waiting | Retained ->
        (match attempt with
         | None -> A.Delegated_policy_claim.drain t.actor
         | Some attempt ->
           attempt.Policy_claim_attempt.phase <- Draining_capture;
           let outcome =
             Scope_outcome.capture (fun () -> A.Delegated_policy_claim.drain t.actor)
           in
           attempt.drain <- Some outcome;
           attempt.drain_diagnostic <- Original_diagnostic.primary outcome;
           Scope_outcome.resume_primary outcome)
    in
    Option.iter attempt ~f:(fun attempt ->
      attempt.Policy_claim_attempt.phase <- Checking_release);
    let%bind () =
      match A.Delegated_policy_claim.Capture.disposition holder.capture with
      | Released -> Ok ()
      | Waiting | Retained -> Error (retired_cleanup_error ())
    in
    let%bind owned = current () in
    let%bind () =
      if not owned
      then Ok ()
      else (
        match attempt with
        | None ->
          (* Reserve before the actual port may yield. A repeated natural call
             cannot run physical work before detecting duplicate publication. *)
          Policy_claim_release.begin_release holder.natural_release;
          let actual =
            Physical_origin.capture_result (fun () ->
              Session_policy_resource.Lease.close_reported holder.loan)
          in
          Policy_claim_release.record holder.natural_release actual;
          Scope_outcome.resume_primary (Physical_origin.primary actual)
        | Some attempt ->
          attempt.Policy_claim_attempt.phase <- Releasing_loan;
          let actual =
            Physical_origin.capture_result (fun () ->
              Session_policy_resource.Lease.close_reported holder.loan)
          in
          let outcome = Physical_origin.primary actual in
          attempt.release <- Some outcome;
          attempt.release_origin <- Some (Physical_origin.of_observation actual);
          Scope_outcome.resume_primary outcome)
    in
    Option.iter attempt ~f:(fun attempt ->
      attempt.Policy_claim_attempt.phase <- Clearing_custody);
    with_owner_lock t ~protect:true (fun () ->
      if Option.exists t.policy_claim ~f:(phys_equal holder) then t.policy_claim <- None);
    Option.iter attempt ~f:(fun attempt -> attempt.Policy_claim_attempt.phase <- Complete);
    Ok ())
;;

let capture_policy_claim recovery ~phase (holder : native_consumer Policy_claim_holder.t) =
  Recovery_data.register_policy_claim
    recovery
    ~phase
    ~capture:holder.capture
    ~loan:holder.loan
    ~runtime:holder.runtime
    ~wrapper_finished:holder.wrapper.finished
    ~wrapper_recovery:holder.wrapper.recovery
    ~natural_release:holder.natural_release
;;

let drain_policy_claim_observed ?recovery ?(slot = Policy_claim_attempt.Slot.Ordinary) t =
  Physical_origin.capture_result (fun () ->
    let holder = with_owner_lock t ~protect:true (fun () -> t.policy_claim) in
    match holder with
    | None -> Agent_session.Close_observation.create (Returned (Ok ())) ~diagnostics:[] ()
    | Some holder ->
      let attempt = Policy_claim_attempt.create () in
      Option.iter recovery ~f:(fun recovery ->
        let receipt = capture_policy_claim recovery ~phase:slot holder in
        match slot with
        | Policy_claim_attempt.Slot.Ordinary ->
          if Option.is_some receipt.ordinary_attempt
          then invalid_arg "ordinary policy claim cleanup already ran";
          receipt.ordinary_attempt <- Some attempt
        | Complete_resources ->
          if Option.is_some receipt.complete_attempt
          then invalid_arg "complete-resource policy claim cleanup already ran";
          receipt.complete_attempt <- Some attempt);
      let outcome =
        Scope_outcome.capture (fun () -> drain_policy_holder ~attempt t holder)
      in
      attempt.original <- Some outcome;
      (match attempt.phase with
       | Checking_custody | Checking_release | Clearing_custody ->
         attempt.control_diagnostic <- Original_diagnostic.primary outcome
       | Draining_capture | Releasing_loan | Complete -> ());
      let release = attempt.release_origin in
      let control =
        Originated_retirement.origin_of_diagnostic attempt.control_diagnostic
      in
      let drain = Originated_retirement.origin_of_diagnostic attempt.drain_diagnostic in
      let selected =
        match attempt.phase with
        | Checking_custody | Checking_release | Clearing_custody -> control
        | Draining_capture -> drain
        | Releasing_loan -> release
        | Complete -> None
      in
      let diagnostics =
        List.concat_map [ drain; release; control ] ~f:(fun origin ->
          Option.value_map origin ~default:[] ~f:(fun origin ->
            origin.Physical_origin.diagnostics))
      in
      Agent_session.Close_observation.create
        (match outcome with
         | Scope_outcome.Returned result -> Returned result
         | Raised (exn, trace) -> Raised (exn, trace))
        ~diagnostics
        ?primary:(Option.bind selected ~f:(fun origin -> origin.Physical_origin.primary))
        ())
;;

let drain_policy_claim ?recovery ?slot t =
  drain_policy_claim_observed ?recovery ?slot t
  |> Physical_origin.primary
  |> Scope_outcome.resume_primary
;;

let prepare_dependency_stop t =
  match t.before_unload with
  | None -> Ok ()
  | Some prepare -> prepare ~closing:false
;;

let prepare_dependency_close t =
  match t.before_unload with
  | None -> Ok ()
  | Some prepare -> prepare ~closing:true
;;

let capture_retirement_releases_locked t ~recovery ~phase ~position ~closing =
  Option.iter t.policy_claim ~f:(fun holder ->
    ignore (capture_policy_claim recovery ~phase holder : Recovery_data.policy_claim));
  Option.iter
    t.restored_policy
    ~f:(Recovery_data.register_restored_release recovery ~position);
  if closing
  then
    Option.iter
      t.native_anchor
      ~f:(Recovery_data.register_native_release recovery ~position);
  List.iter t.native_loans ~f:(fun loan ->
    match loan.Native_loan.phase with
    | Retiring | Closing _ ->
      Recovery_data.register_native_release recovery ~position loan
    | Available | Released -> ())
;;

let retirement_actor_step recovery =
  Owner_affinity.check_domain_exn recovery.Recovery_data.affinity;
  let captured = Recovery_data.captured_of_phase recovery.phase in
  match captured.actor_cleanup with
  | Some step -> step
  | None -> invalid_arg "retirement lost its original Actor cleanup selection"
;;

let observe_actor_cleanup_snapshot snapshot =
  let module C = Agent_session.Session_actor.Retirement_cleanup in
  let failures =
    List.map (C.Snapshot.failures snapshot) ~f:(function
      | C.Failure.Protocol error -> Scope_outcome.Cleanup.Failure.Protocol error
      | C.Failure.Raised (exn, backtrace) ->
        Scope_outcome.Cleanup.Failure.Raised (exn, backtrace))
  in
  let duty =
    match C.Snapshot.progress snapshot with
    | Joined -> Cleanup_duty.Succeeded
    | Pending -> Blocked failures
    | Retryable -> Retryable failures
    | Terminal -> Terminal failures
  in
  Cleanup_snapshot.of_duties [ duty ]
;;

let actor_cleanup_observation ?diagnostic outcome =
  Physical_origin.capture_result (fun () ->
    Agent_session.Close_observation.create
      (match outcome with
       | Scope_outcome.Returned result -> Returned result
       | Raised (exn, trace) -> Raised (exn, trace))
      ~diagnostics:(Option.to_list diagnostic)
      ?primary:diagnostic
      ())
;;

let resume_actor_cleanup_pass_observed ~final ?diagnostic outcome =
  match outcome with
  | Scope_outcome.Returned (Error error) ->
    actor_cleanup_observation ?diagnostic (Scope_outcome.Returned (Error error))
  | Raised (exn, trace) -> actor_cleanup_observation ?diagnostic (Raised (exn, trace))
  | Returned (Ok actual) ->
    let snapshot = observe_actor_cleanup_snapshot actual in
    (match Cleanup_snapshot.cleanup snapshot with
     | Some (Scope_outcome.Cleanup.Failed (Protocol error, _)) ->
       actor_cleanup_observation ?diagnostic (Returned (Error error))
     | Some (Failed (Raised (exn, trace), _)) ->
       actor_cleanup_observation ?diagnostic (Raised (exn, trace))
     | Some (Never_acquired | Joined) | None ->
       (match Cleanup_snapshot.progress snapshot with
        | Joined -> actor_cleanup_observation (Returned (Ok ()))
        | Blocked when (not final) && List.is_empty (Cleanup_snapshot.failures snapshot)
          -> actor_cleanup_observation (Returned (Ok ()))
        | Blocked | Retryable | Terminal ->
          (* This projection refusal is a new actual boundary, not a failed
             advance RPC or an inferred snapshot failure. *)
          actor_cleanup_observation (Returned (Error (retired_cleanup_error ())))))
;;

let advance_original_actor_cleanup_observed (t : t) step ~final =
  (match Actor_cleanup_step.phase step, final with
   | Initial_drain, false | Final_refresh, true -> ()
   | (Initial_drain | Same_foreground | Final_refresh | Complete), _ ->
     invalid_arg "original Actor cleanup pass is out of order");
  let previous = if final then step.Actor_cleanup_step.final else step.initial in
  match previous with
  | Some _ -> invalid_arg "original Actor cleanup pass already ran"
  | None ->
    let outcome =
      Scope_outcome.capture (fun () ->
        let open Result.Let_syntax in
        let module C = Agent_session.Session_actor.Retirement_cleanup in
        let%bind after = C.snapshot step.cleanup in
        C.advance step.cleanup ~actor:t.actor ~after)
    in
    if final
    then (
      step.final <- Some outcome;
      step.final_diagnostic <- Actor_cleanup_step.diagnostic outcome)
    else (
      step.initial <- Some outcome;
      step.initial_diagnostic <- Actor_cleanup_step.diagnostic outcome);
    resume_actor_cleanup_pass_observed
      ~final
      ?diagnostic:(if final then step.final_diagnostic else step.initial_diagnostic)
      outcome
;;

let await_original_foreground_observed step =
  match step.Actor_cleanup_step.foreground with
  | Some outcome ->
    actor_cleanup_observation ?diagnostic:step.foreground_diagnostic outcome
  | None ->
    let outcome =
      Scope_outcome.capture (fun () ->
        Agent_session.Session_actor.Runtime_retirement.await
          (Agent_session.Session_actor.Retirement_cleanup.retirement step.cleanup))
    in
    step.foreground <- Some outcome;
    step.foreground_diagnostic <- Original_diagnostic.primary outcome;
    actor_cleanup_observation ?diagnostic:step.foreground_diagnostic outcome
;;

let check_retirement_coverage t ~recovery ~coverage =
  match coverage with
  | Retirement_coverage.Whole_owner -> Ok ()
  | Cell _ ->
    let step = Recovery_data.register_original_step recovery Retirement_coverage_check in
    Original_cleanup_step.capture step (fun () ->
      with_owner_lock t ~protect:true (fun () ->
        Retirement_coverage_guard.before_effect t coverage))
    |> Originated_retirement.select_step step
;;

module Retry_cleanup = struct
  module D = Recovery_data
  module O = Recovery_data.Observation
  module E = Cleanup_evidence
  module R = Recovery_data.Reservation
  module A = Agent_session.Session_actor

  type owner = t

  let view (reserved : D.reserved) =
    { reserved.before.captured with observing = Some reserved.before.current }
  ;;

  let joined evidence =
    Cleanup_snapshot.equal_progress (E.snapshot evidence).progress Joined
  ;;

  let positive = function
    | Scope_outcome.Returned (Ok _) -> true
    | Returned (Error _) | Raised _ -> false
  ;;

  let terminal = function
    | None -> false
    | Some primary -> not (positive primary)
  ;;

  let after reservation = R.after reservation

  let check_gate (owner : owner) (reservation : R.t) =
    let root = reservation.root.holder in
    if not (Owner_affinity.same owner.affinity root.affinity)
    then Error (Agent_protocol.Error.invalid_request "cleanup belongs to another Owner")
    else if
      Option.is_some owner.prepared_source
      || Option.is_some owner.administration_transition
      || Option.is_some owner.startup_settlement_request
    then Error (background_busy ())
    else (
      match owner.failed_source_cleanup with
      | Some source when Option.is_none (R.find reservation source) ->
        Error (background_busy ())
      | Some _ | None -> Ok ())
  ;;

  let current_batch_alias reservation resource =
    List.find_map reservation.R.scopes ~f:(fun reserved ->
      List.find_map reserved.D.before.current.retired ~f:(fun current ->
        if phys_equal current.resource resource
        then
          Option.filter current.retired_latest ~f:(fun latest ->
            phys_equal latest.after (after reservation))
        else None))
  ;;

  let run_batch owner reservation reserved (batch : Cleanup_batch.t) =
    List.iter batch.receipts ~f:(fun original ->
      match original.Retired_resource.Receipt.disposition with
      | Already_closed | Transferred _ -> ()
      | Unattempted | Blocked _ | Attempted _ ->
        let current =
          List.find_exn reserved.D.before.current.retired ~f:(fun current ->
            phys_equal current.resource original.resource)
        in
        (match current.resource.phase with
         | Closed -> ()
         | Closing producer when Option.is_none (Eio.Promise.peek producer.finished) -> ()
         | Retained _ | Closing _ ->
           (match current_batch_alias reservation current.resource with
            | Some actual -> current.retired_latest <- Some actual
            | None ->
              let fixed_control =
                Option.exists current.retired_latest ~f:(fun latest ->
                  match latest.batch.original with
                  | Raised _ -> true
                  | Returned _ -> false)
              in
              if not fixed_control
              then (
                let cleanup = Cleanup_request.create () in
                Cleanup_request.add cleanup current.resource;
                let batch =
                  run_cleanup_request_reported ~requirement:Required owner cleanup
                in
                current.retired_latest
                <- Some { D.after = after reservation; owner = reserved.holder; batch }))))
  ;;

  let batches owner reservation reserved matches =
    List.iter (List.rev reserved.D.before.captured.batches_rev) ~f:(fun selected ->
      if matches selected.Recovery_batch.role
      then run_batch owner reservation reserved selected.batch)
  ;;

  let native_alias reservation loan =
    List.find_map reservation.R.scopes ~f:(fun reserved ->
      List.find_map reserved.D.before.current.native ~f:(fun current ->
        if phys_equal current.loan loan
        then
          Option.filter current.native_latest ~f:(fun latest ->
            phys_equal latest.after (after reservation))
        else None))
  ;;

  let run_native owner reservation reserved position loan =
    let current =
      List.find_exn reserved.D.before.current.native ~f:(fun current ->
        phys_equal current.loan loan)
    in
    match loan.Native_loan.phase with
    | Released -> ()
    | Closing producer when Option.is_none (Eio.Promise.peek producer.finished) -> ()
    | Available | Retiring | Closing _ ->
      (match native_alias reservation loan with
       | Some actual -> current.native_latest <- Some actual
       | None ->
         let fixed_control =
           Option.exists current.native_latest ~f:(fun latest ->
             terminal latest.report.commit
             || terminal latest.report.selection
             ||
             match latest.report.selection with
             | Some (Returned (Ok (Missing _))) -> true
             | None
             | Some
                 ( Returned (Ok (Released | Join _ | Close _))
                 | Returned (Error _)
                 | Raised _ ) -> false)
         in
         let original_control =
           List.exists
             reserved.D.before.captured.native_release_reports_rev
             ~f:(fun report ->
               phys_equal report.D.Native_release_report.loan loan
               && Physical_cleanup_position.same report.position position
               && (terminal report.selection
                   || terminal report.commit
                   ||
                   match report.selection with
                   | Some (Returned (Ok (Missing _))) -> true
                   | None
                   | Some
                       ( Returned (Ok (Released | Join _ | Close _))
                       | Returned (Error _)
                       | Raised _ ) -> false))
         in
         if (not fixed_control) && not original_control
         then
           ignore
             (close_native_loan_reported
                ~current:(current, after reservation, reserved.holder, position)
                owner
                loan
              : Native_loan.Attempt.outcome))
  ;;

  let restored_alias reservation anchor =
    List.find_map reservation.R.scopes ~f:(fun reserved ->
      List.find_map reserved.D.before.current.restored ~f:(fun current ->
        if phys_equal current.anchor anchor
        then
          Option.filter current.restored_latest ~f:(fun latest ->
            phys_equal latest.after (after reservation))
        else None))
  ;;

  let run_restored owner reservation reserved position anchor =
    let current =
      List.find_exn reserved.D.before.current.restored ~f:(fun current ->
        phys_equal current.anchor anchor)
    in
    if not (O.restored_released (view reserved) anchor)
    then (
      match restored_alias reservation anchor with
      | Some actual -> current.restored_latest <- Some actual
      | None ->
        let fixed_control =
          Option.exists current.restored_latest ~f:(fun latest ->
            terminal latest.report.selection_revoke
            || terminal latest.report.helper_revoke
            || terminal latest.report.clear)
        in
        let original_control =
          List.exists
            reserved.D.before.captured.restored_release_reports_rev
            ~f:(fun report ->
              phys_equal report.D.Restored_release_report.anchor anchor
              && Physical_cleanup_position.same report.position position
              && (terminal report.selection_revoke
                  || terminal report.helper_revoke
                  || terminal report.clear))
        in
        if (not fixed_control) && not original_control
        then
          ignore
            (close_restored_anchor_outcome
               ~current:(current, after reservation, reserved.holder, position)
               owner
               anchor
             : unit Scope_outcome.primary))
  ;;

  let native_at reserved position loan =
    let captured = view reserved in
    let reports =
      List.filter (List.rev captured.native_release_reports_rev) ~f:(fun report ->
        phys_equal report.D.Native_release_report.loan loan
        && Physical_cleanup_position.same report.position position)
    in
    let boundary =
      match position, reports with
      | Physical_cleanup_position.Source_tail duty, [] ->
        Option.to_list
          (Option.map duty.Source_cleanup.original ~f:(fun primary ->
             O.original ~prerequisite:Ready (Some primary)))
      | (Lease_pin | Administration_precommit | Ordinary_suffix | Complete_suffix), _
      | Source_tail _, _ :: _ -> []
    in
    boundary
    @
    match reports with
    | first :: rest ->
      List.concat_map (first :: rest) ~f:(fun report ->
        O.native_report captured report ~prerequisite:Ready)
    | [] ->
      let current =
        List.find_exn reserved.D.before.current.native ~f:(fun current ->
          phys_equal current.loan loan)
      in
      (match current.native_latest with
       | Some latest ->
         O.native_report captured ~position latest.report ~prerequisite:Ready
       | None ->
         [ (match loan.Native_loan.phase with
            | Released -> E.succeeded
            | Available | Retiring | Closing _ -> E.make ~progress:Blocked [])
         ])
  ;;

  let physical owner reservation reserved position ~ready =
    if ready
    then (
      List.iter
        (List.rev reserved.D.before.captured.restored_releases_rev)
        ~f:(fun selected ->
          if
            Physical_cleanup_position.same
              selected.Selected_restored_release.position
              position
          then run_restored owner reservation reserved position selected.anchor);
      let prefix = ref true in
      List.iter
        (List.rev reserved.before.captured.native_releases_rev)
        ~f:(fun selected ->
          if
            !prefix
            && Physical_cleanup_position.same
                 selected.Selected_native_release.position
                 position
          then (
            run_native owner reservation reserved position selected.loan;
            prefix := joined (native_at reserved position selected.loan))))
  ;;

  let actor owner reservation reserved =
    match reserved.D.before.current.actor with
    | None -> ()
    | Some current ->
      let step = current.step in
      let original_ready =
        (not (terminal step.initial))
        && (not (terminal step.foreground))
        && not (terminal step.final)
      in
      let current_ready =
        (not
           (Option.exists current.initial ~f:(fun actual -> not (positive actual.result))))
        && (not (terminal current.foreground))
        && not
             (Option.exists current.final ~f:(fun actual -> not (positive actual.result)))
      in
      if original_ready && current_ready
      then (
        let advance () =
          Scope_outcome.capture (fun () ->
            let open Result.Let_syntax in
            let%bind snapshot = A.Retirement_cleanup.snapshot step.cleanup in
            A.Retirement_cleanup.advance step.cleanup ~actor:owner.actor ~after:snapshot)
        in
        let original_complete =
          Option.is_some step.initial
          && Option.is_some step.foreground
          && Option.is_some step.final
          && joined (O.actor (view reserved) step ~prerequisite:Ready)
        in
        if not original_complete
        then (
          let initial =
            match current.initial with
            | Some actual when phys_equal actual.after (after reservation) ->
              actual.result
            | None | Some _ ->
              let result = advance () in
              current.initial
              <- Some
                   { D.after = after reservation
                   ; owner = reserved.holder
                   ; result
                   ; diagnostic = Actor_cleanup_step.diagnostic result
                   };
              result
          in
          let initial_allows_join =
            match initial with
            | Returned (Ok snapshot) ->
              List.is_empty (A.Retirement_cleanup.Snapshot.failures snapshot)
            | Returned (Error _) | Raised _ -> false
          in
          let retirement = A.Retirement_cleanup.retirement step.cleanup in
          if initial_allows_join && A.Runtime_retirement.is_finished retirement
          then (
            let foreground =
              match current.foreground, step.foreground with
              | Some actual, _ -> actual
              | None, Some actual -> actual
              | None, None ->
                let actual =
                  Scope_outcome.capture (fun () -> A.Runtime_retirement.await retirement)
                in
                current.foreground_diagnostic <- Original_diagnostic.primary actual;
                current.foreground <- Some actual;
                actual
            in
            if positive foreground
            then (
              match current.final with
              | Some actual when phys_equal actual.after (after reservation) -> ()
              | None | Some _ ->
                let result = advance () in
                current.final
                <- Some
                     { D.after = after reservation
                     ; owner = reserved.holder
                     ; result
                     ; diagnostic = Actor_cleanup_step.diagnostic result
                     }))))
  ;;

  let source_prerequisite reserved =
    let captured = view reserved in
    let control =
      O.observe_steps captured Source_physical_selection ~prerequisite:Ready
      @ O.observe_steps captured Source_installed_selection ~prerequisite:Ready
      @ O.observe_steps captured Source_prefix_selection ~prerequisite:Ready
      @ List.concat_map (List.rev captured.batches_rev) ~f:(fun selected ->
        match selected.Recovery_batch.role, selected.batch.original with
        | Source_exit, Raised (exn, backtrace) ->
          [ E.make ~progress:Terminal [ E.original (Raised (exn, backtrace)) ] ]
        | ( ( Lease_admission
            | Lease_detachment
            | Source_entry
            | Construction_exit
            | Construction_local _
            | Ordinary_warm
            | Source_exit
            | Administration_prefix
            | Administration_before_commit
            | Administration_exit
            | Retirement_ordinary
            | Retirement_complete )
          , _ ) -> [])
    in
    let prefix =
      Option.value_map captured.prefix ~default:[] ~f:O.scope
      @ O.observe_steps captured Administration_unload_prefix ~prerequisite:Ready
      @ O.batches
          captured
          (function
            | Administration_prefix -> true
            | Lease_admission
            | Lease_detachment
            | Source_entry
            | Construction_exit
            | Construction_local _
            | Ordinary_warm
            | Source_exit
            | Administration_before_commit
            | Administration_exit
            | Retirement_ordinary
            | Retirement_complete -> false)
          ~prerequisite:Ready
    in
    joined (control @ prefix)
  ;;

  let source_tail owner reservation reserved =
    if source_prerequisite reserved
    then (
      let native_prefix = ref true in
      List.iter
        (List.stable_sort
           (List.rev reserved.D.before.captured.source_tail_rev)
           ~compare:Source_cleanup.compare_order)
        ~f:(fun duty ->
          match duty.Source_cleanup.target with
          | Native_release loan when !native_prefix ->
            run_native owner reservation reserved (Source_tail duty) loan;
            native_prefix := joined (native_at reserved (Source_tail duty) loan)
          | Native_release _ -> ()
          | Restored_release anchor ->
            run_restored owner reservation reserved (Source_tail duty) anchor
          | Policy_end _ | Native_end _ | Delegated_end _ ->
            let current =
              List.find_exn reserved.before.current.source_ends ~f:(fun current ->
                phys_equal current.duty duty)
            in
            let previous =
              match current.end_latest with
              | Some _ as actual -> actual
              | None -> duty.original
            in
            if not (Option.exists previous ~f:positive)
            then (
              let actual =
                Scope_outcome.capture (fun () ->
                  match duty.target with
                  | Policy_end restoration ->
                    A.end_policy_restoration owner.actor ~restoration
                  | Native_end setup -> A.end_native_setup owner.actor ~setup
                  | Delegated_end preparation ->
                    A.end_delegated_load_preparation owner.actor ~preparation
                  | Native_release _ | Restored_release _ ->
                    invalid_arg "source End target changed")
              in
              current.end_diagnostic <- Original_diagnostic.primary actual;
              current.end_latest <- Some actual)))
  ;;

  let construction owner reserved receipt =
    match receipt.Construction_cleanup.phase with
    | Awaiting_close | Close_failed _ | Closed (Admission_retained | Attempted _) -> ()
    | Closed Unattempted ->
      let captured = view reserved in
      let retention =
        O.construction_steps captured receipt Construction_retention ~prerequisite:Ready
      in
      if (not (List.is_empty retention)) && joined retention
      then (
        let current =
          List.find_exn reserved.D.before.current.constructions ~f:(fun current ->
            phys_equal current.receipt receipt)
        in
        if Option.is_none current.construction_end
        then (
          let actual =
            Scope_outcome.capture (fun () ->
              A.end_runtime_construction
                owner.actor
                ~construction:(Construction_cleanup.construction receipt))
          in
          current.construction_end_diagnostic <- Original_diagnostic.primary actual;
          current.construction_end <- Some actual))
  ;;

  let rec scope owner reservation holder =
    match R.find reservation holder with
    | None -> () (* A running child which completed later was not reserved. *)
    | Some reserved when reserved.executed -> () (* Same declared holder alias. *)
    | Some reserved ->
      reserved.executed <- true;
      let captured = reserved.before.captured in
      (match holder.D.kind with
       | Lease ->
         List.iter
           (List.rev captured.preparation_sources_rev)
           ~f:(scope owner reservation);
         batches owner reservation reserved (function
           | Lease_admission -> true
           | Lease_detachment
           | Source_entry
           | Construction_exit
           | Construction_local _
           | Ordinary_warm
           | Source_exit
           | Administration_prefix
           | Administration_before_commit
           | Administration_exit
           | Retirement_ordinary
           | Retirement_complete -> false);
         physical owner reservation reserved Lease_pin ~ready:true;
         batches owner reservation reserved (function
           | Lease_detachment -> true
           | Lease_admission
           | Source_entry
           | Construction_exit
           | Construction_local _
           | Ordinary_warm
           | Source_exit
           | Administration_prefix
           | Administration_before_commit
           | Administration_exit
           | Retirement_ordinary
           | Retirement_complete -> false)
       | Source Stage ->
         batches owner reservation reserved (function
           | Source_entry -> true
           | Lease_admission
           | Lease_detachment
           | Construction_exit
           | Construction_local _
           | Ordinary_warm
           | Source_exit
           | Administration_prefix
           | Administration_before_commit
           | Administration_exit
           | Retirement_ordinary
           | Retirement_complete -> false);
         physical owner reservation reserved Administration_precommit ~ready:true;
         List.iter (List.rev captured.constructions_rev) ~f:(fun receipt ->
           construction owner reserved receipt;
           batches owner reservation reserved (function
             | Construction_local actual -> phys_equal actual receipt
             | Construction_exit -> false
             | Lease_admission
             | Lease_detachment
             | Source_entry
             | Ordinary_warm
             | Source_exit
             | Administration_prefix
             | Administration_before_commit
             | Administration_exit
             | Retirement_ordinary
             | Retirement_complete -> false));
         batches owner reservation reserved (function
           | Ordinary_warm -> true
           | Lease_admission
           | Lease_detachment
           | Source_entry
           | Construction_exit
           | Construction_local _
           | Source_exit
           | Administration_prefix
           | Administration_before_commit
           | Administration_exit
           | Retirement_ordinary
           | Retirement_complete -> false);
         List.iter (List.rev captured.leases_rev) ~f:(fun lease ->
           scope owner reservation lease.recovery);
         batches owner reservation reserved (function
           | Source_exit -> true
           | Lease_admission
           | Lease_detachment
           | Source_entry
           | Construction_exit
           | Construction_local _
           | Ordinary_warm
           | Administration_prefix
           | Administration_before_commit
           | Administration_exit
           | Retirement_ordinary
           | Retirement_complete -> false);
         Option.iter captured.prefix ~f:(scope owner reservation);
         batches owner reservation reserved (function
           | Administration_prefix -> true
           | Lease_admission
           | Lease_detachment
           | Source_entry
           | Construction_exit
           | Construction_local _
           | Ordinary_warm
           | Source_exit
           | Administration_before_commit
           | Administration_exit
           | Retirement_ordinary
           | Retirement_complete -> false);
         source_tail owner reservation reserved
       | Source Administration_transition ->
         batches owner reservation reserved (function
           | Administration_before_commit | Administration_exit -> true
           | Lease_admission
           | Lease_detachment
           | Source_entry
           | Construction_exit
           | Construction_local _
           | Ordinary_warm
           | Source_exit
           | Administration_prefix
           | Retirement_ordinary
           | Retirement_complete -> false)
       | Construction ->
         batches owner reservation reserved (function
           | Construction_exit -> true
           | Lease_admission
           | Lease_detachment
           | Source_entry
           | Construction_local _
           | Ordinary_warm
           | Source_exit
           | Administration_prefix
           | Administration_before_commit
           | Administration_exit
           | Retirement_ordinary
           | Retirement_complete -> false)
       | Retirement -> retirement owner reservation reserved)

  and claim owner reservation reserved selected =
    scope owner reservation selected.D.wrapper_recovery;
    let current =
      List.find_exn reserved.D.before.current.claims ~f:(fun current ->
        phys_equal current.claim selected)
    in
    let released =
      Policy_claim_release.released
        selected.natural_release
        ~current:current.claim_pin
        ~attempts:
          (List.map
             [ selected.ordinary_attempt; selected.complete_attempt ]
             ~f:(fun attempt ->
               Option.bind attempt ~f:(fun attempt ->
                 attempt.Policy_claim_attempt.release)))
    in
    if
      joined (O.scope selected.wrapper_recovery)
      &&
      match A.Delegated_policy_claim.Capture.disposition selected.capture with
      | Released -> true
      | Waiting | Retained -> false
    then (
      if not released
      then (
        let actual =
          Physical_origin.capture_result (fun () ->
            Session_policy_resource.Lease.close_reported selected.loan)
        in
        current.claim_pin_origin <- Some (Physical_origin.of_observation actual);
        current.claim_pin <- Some (Physical_origin.primary actual));
      let original_clear_terminal =
        List.exists
          [ selected.ordinary_attempt; selected.complete_attempt ]
          ~f:(fun attempt ->
            Option.exists attempt ~f:(fun attempt ->
              match attempt.Policy_claim_attempt.phase with
              | Clearing_custody -> terminal attempt.original
              | Checking_custody
              | Draining_capture
              | Checking_release
              | Releasing_loan
              | Complete -> false))
      in
      if
        (released || Option.exists current.claim_pin ~f:positive)
        && not original_clear_terminal
      then
        if Option.is_none current.claim_clear
        then (
          let actual =
            Scope_outcome.capture (fun () ->
              with_owner_lock owner ~protect:true (fun () ->
                match owner.policy_claim with
                | None -> Ok ()
                | Some actual
                  when phys_equal actual.capture selected.capture
                       && phys_equal actual.loan selected.loan
                       && phys_equal actual.runtime selected.runtime
                       && phys_equal actual.natural_release selected.natural_release ->
                  owner.policy_claim <- None;
                  Ok ()
                | Some _ -> Error (retired_cleanup_error ())))
          in
          current.claim_clear_diagnostic <- Original_diagnostic.primary actual;
          current.claim_clear <- Some actual))

  and join_retirement_consumer (selected : D.lease) mode =
    let outcome =
      match selected.operation with
      | Some operation ->
        let cancellation =
          match mode, selected.role, selected.retry_cancel with
          | ( Relation_evidence.Join_mode.Recovery_retry
            , Retirement_lease { cancel; _ }
            , None )
            when Option.is_none cancel.original -> Relation_evidence.Cancellation.Attempt
          | (Original | Recovery_retry | Reobserve), _, _ -> Skip
        in
        operation.run mode ~cancellation
      | None ->
        { Relation_evidence.Run_result.join =
            Join_control
              (Physical_origin.opaque_result (fun () ->
                 Error
                   (Agent_protocol.Error.create
                      Conflict
                      ~message:"retirement lease has no actual enrolled join operation"
                      ~retryable:true
                      ())))
        ; cancellation = None
        }
    in
    D.record_consumer_join selected ~mode outcome;
    positive (Physical_origin.primary (Relation_evidence.Run_result.observation outcome))

  and retirement owner reservation reserved =
    let captured = reserved.D.before.captured in
    Option.iter captured.pending_source ~f:(scope owner reservation);
    let run_phase (phase : Retirement_cleanup_phase.t) =
      List.iter (O.selected_leases captured phase) ~f:(fun lease ->
        if join_retirement_consumer lease Relation_evidence.Join_mode.Recovery_retry
        then (
          scope owner reservation lease.recovery;
          (* Provider repair can change a dependency completion identity. Refresh
             only through the SAME actual consumer join, never a cached E read. *)
          ignore (join_retirement_consumer lease Reobserve : bool)));
      List.iter (O.selected_claims captured phase) ~f:(claim owner reservation reserved)
    in
    run_phase Ordinary;
    let canonical_custody =
      match reserved.D.holder.completion_requirement with
      | D.Completion_requirement.Canonical_custody -> true
      | Legacy ->
        List.exists captured.leases_rev ~f:(fun (selected : D.lease) ->
          Option.is_some selected.relation)
    in
    let ready evidence =
      if canonical_custody
      then
        E.custody_joined evidence
        && Result.is_ok (Cleanup_snapshot.observation (O.snapshot evidence))
        && Original_cleanup_report.diagnostics_complete
             (E.child_view evidence ~diagnostics_complete:true)
      else joined evidence
    in
    let before_actor =
      O.required_step (view reserved) Actor_selection ~prerequisite:Ready
      @ O.observe_steps (view reserved) Dependency_stop ~prerequisite:Ready
      @ O.retirement_leases (view reserved) Ordinary ~prerequisite:Ready
      @ List.concat_map (O.selected_claims captured Ordinary) ~f:(fun selected ->
        O.claim (view reserved) selected Ordinary ~prerequisite:Ready)
      @ Option.value_map captured.pending_source ~default:[] ~f:O.scope
    in
    if ready before_actor then actor owner reservation reserved;
    let actor_ready =
      ready before_actor
      && Option.exists captured.actor_cleanup ~f:(fun step ->
        joined (O.actor (view reserved) step ~prerequisite:Ready))
    in
    physical owner reservation reserved Ordinary_suffix ~ready:actor_ready;
    (* The retained ordinary batch is independent factual work. Its own original
       selection/control failures remain visible; no runtime is selected anew. *)
    batches owner reservation reserved (function
      | Retirement_ordinary -> true
      | Lease_admission
      | Lease_detachment
      | Source_entry
      | Construction_exit
      | Construction_local _
      | Ordinary_warm
      | Source_exit
      | Administration_prefix
      | Administration_before_commit
      | Administration_exit
      | Retirement_complete -> false);
    let entered_complete =
      match reserved.before.completion with
      | Retirement { entered = Complete_resources; _ } -> true
      | Retirement { entered = Not_entered | Ordinary | Released_before_physical; _ }
      | Lease _ | Stage _ | Administration_transition | Construction -> false
    in
    if entered_complete && ready (O.retirement_ordinary (view reserved))
    then (
      run_phase Complete_resources;
      let before_physical =
        O.required_step (view reserved) Complete_selection ~prerequisite:Ready
        @ O.retirement_leases (view reserved) Complete_resources ~prerequisite:Ready
        @ List.concat_map
            (O.selected_claims captured Complete_resources)
            ~f:(fun selected ->
              O.claim (view reserved) selected Complete_resources ~prerequisite:Ready)
        @ List.concat_map
            [ O.Step_position.Complete_constructor
            ; Complete_callback
            ; Complete_descendant
            ; Complete_inference
            ]
            ~f:(fun position ->
              O.required_step (view reserved) position ~prerequisite:Ready)
      in
      physical owner reservation reserved Complete_suffix ~ready:(ready before_physical);
      batches owner reservation reserved (function
        | Retirement_complete -> true
        | Lease_admission
        | Lease_detachment
        | Source_entry
        | Construction_exit
        | Construction_local _
        | Ordinary_warm
        | Source_exit
        | Administration_prefix
        | Administration_before_commit
        | Administration_exit
        | Retirement_ordinary -> false))
  ;;

  let run owner reservation = scope owner reservation reservation.R.root.holder
end

let check_cleanup_recovery_domain (holder : Recovery_data.t) =
  if Int.equal holder.affinity.domain (Stdlib.Domain.self () :> int)
  then Ok ()
  else
    Error
      (Agent_protocol.Error.invalid_request
         "cleanup holder belongs to another Eio domain")
;;

let retry_cleanup_recovery holder ~(owner : t) ~after =
  let open Result.Let_syntax in
  let%bind () = check_cleanup_recovery_domain holder in
  let%bind () =
    if Owner_affinity.same holder.affinity owner.affinity
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request "cleanup holder belongs to another Owner")
  in
  let%bind reservation =
    Recovery_data.Reservation.reserve holder ~affinity:owner.affinity ~after
  in
  (* Installed before either protection or mutex acquisition can yield. Readers
     throughout the reservation retain the physically SAME prior snapshot. *)
  Exn.protect
    ~finally:(fun () -> Recovery_data.Reservation.restore reservation)
    ~f:(fun () ->
      Eio.Cancel.protect (fun () ->
        let%bind () =
          with_owner_lock owner ~protect:true (fun () ->
            Retry_cleanup.check_gate owner reservation)
        in
        Retry_cleanup.run owner reservation;
        with_owner_lock owner ~protect:true (fun () ->
          Recovery_data.Reservation.publish reservation;
          (* Completed reports retain exact relation operations/carriers before
             their detached Owner-list metadata is removed. Never rewrite an
             already-issued original raw result or its finished promise. *)
          let release_settled slot =
            match slot with
            | Some (attempt : Resource_retirement_data.t)
              when Option.is_some
                     (Recovery_data.Reservation.find reservation attempt.recovery) ->
              (match attempt.recovery.phase with
               | Original_running _ | Retrying _ -> slot
               | Complete completed ->
                 let evidence = Recovery_data.Observation.scope attempt.recovery in
                 if
                   Result.is_ok (Cleanup_snapshot.observation completed.latest)
                   && Cleanup_evidence.custody_joined evidence
                   && Recovery_data.original_diagnostics_complete attempt.recovery
                 then (
                   reap_retirement_relations_locked owner ~recovery:attempt.recovery;
                   None)
                 else slot)
            | None | Some _ -> slot
          in
          owner.pending_resource_retirement
          <- release_settled owner.pending_resource_retirement;
          owner.unloading <- release_settled owner.unloading;
          (match owner.failed_source_cleanup with
           | Some source
             when Option.is_some (Recovery_data.Reservation.find reservation source) ->
             (match source.Recovery_data.phase with
              | Complete completed
                when Cleanup_snapshot.equal_progress completed.latest.progress Joined ->
                owner.failed_source_cleanup <- None
              | Original_running _ | Complete _ | Retrying _ -> ())
           | None | Some _ -> ());
          match holder.Recovery_data.phase with
          | Complete completed -> Ok completed.latest
          | Original_running _ | Retrying _ ->
            invalid_arg "cleanup publication did not complete")))
;;

(* Private helpers preceding the single existing retirement recursion group. *)
let consumer_join_control message =
  Relation_evidence.Join_outcome.Join_control
    (Physical_origin.opaque_result (fun () ->
       Error (Agent_protocol.Error.create Conflict ~message ~retryable:true ())))
;;

let consumer_join_error error =
  Relation_evidence.Join_outcome.Join_control
    (Physical_origin.opaque_result (fun () -> Error error))
;;

let capture_consumer_join f =
  match f () with
  | outcome -> outcome
  | exception exn ->
    let trace = Stdlib.Printexc.get_raw_backtrace () in
    Relation_evidence.Join_outcome.Join_control
      (Physical_origin.capture_result (fun () ->
         Agent_session.Close_observation.create (Raised (exn, trace)) ~diagnostics:[] ()))
;;

let complete_consumer_evidence evidence =
  Cleanup_evidence.custody_joined evidence
  && Original_cleanup_report.diagnostics_complete
       (Cleanup_evidence.child_view evidence ~diagnostics_complete:true)
;;

let consumer_custody_outcome evidence =
  let module CO = Agent_session.Close_observation in
  match Cleanup_snapshot.observation (Recovery_data.Observation.snapshot evidence) with
  | Error error -> consumer_join_error error
  | Ok actual ->
    if complete_consumer_evidence evidence
    then
      Relation_evidence.Join_outcome.Canonical_consumer
        (CO.create (Returned (Ok ())) ~diagnostics:(CO.diagnostics actual) ())
    else (
      match CO.exit actual with
      | Returned (Error _) | Raised _ -> Canonical_consumer actual
      | Returned (Ok ()) ->
        consumer_join_control "consumer cleanup has not released its actual custody")
;;

let publish_consumer_join relation recovery =
  match Recovery_data.relation_consumer_observation recovery with
  | Error error -> consumer_join_error error
  | Ok (guard, evidence) ->
    Relation_evidence.observe_consumer relation ~guard ~evidence;
    let outcome = consumer_custody_outcome evidence in
    (* A failed consumer can still hold this provider's native loan open. Forward
       that actual failure without claiming the provider receipt is sealed. *)
    (match
       ( outcome
       , Agent_session.Close_observation.exit
           (Relation_evidence.Join_outcome.observation outcome) )
     with
     | Canonical_consumer _, (Returned (Error _) | Raised _) -> outcome
     | Canonical_consumer _, Returned (Ok ()) | (Join_control _ | Continue_provider), _ ->
       (match Relation_evidence.native_evidence relation with
        | None -> consumer_join_control "native dependency receipt is not sealed"
        | Some { complete = false; _ } ->
          consumer_join_control "native dependency receipt has unavailable provenance"
        | Some { complete = true; _ } -> outcome))
;;

let consumer_needs_repair recovery =
  match Recovery_data.relation_consumer_observation recovery with
  | Error error -> Error error
  | Ok (_, evidence) -> Ok (not (complete_consumer_evidence evidence))
;;

module First_consumer_join = struct
  type t =
    | Observe_original
    | Repair_retained

  let select (attempt : Resource_retirement_data.t) =
    match attempt.retry_source with
    | None -> Observe_original
    | Some _ -> Repair_retained
  ;;

  let execution = function
    | Observe_original -> Relation_evidence.Join_mode.Original
    | Repair_retained -> Recovery_retry
  ;;
end

let rec perform_ordinary_retirement
          t
          ~(attempt : Resource_retirement_data.t)
          ~(leases : background_lease list)
          ~closing
  =
  let recovery = attempt.recovery in
  let coverage = attempt.coverage in
  let open Result.Let_syntax in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  (* Foreground cancellation precedes dependency cleanup: that
     cleanup may itself be waiting for a foreground-owned child. *)
  (* Native pins survive ordinary Stop while actually borrowed. Detached
     dependency metadata has no surviving physical borrower: transfer its exact
     cleanup/report obligations through this actual retirement before reuse. *)
  let leases =
    List.filter leases ~f:(fun lease ->
      closing || (not lease.survives_stop) || not (Background_lease.is_active lease))
  in
  let selected_leases =
    List.map leases ~f:(fun lease ->
      let enrollment = enroll_retirement_consumer t ~recovery ~phase:Ordinary lease in
      lease, enrollment)
  in
  let selection =
    Recovery_data.register_original_step recovery Actor_retirement_selection
  in
  let%bind selected =
    Original_cleanup_step.capture selection (fun () ->
      Agent_session.Session_actor.retire_runtime_worker_with_cleanup t.actor ~closing)
    |> Originated_retirement.select_step selection
  in
  Recovery_data.register_actor_cleanup recovery selected;
  let retirement = Agent_session.Session_actor.Retirement_cleanup.retirement selected in
  with_owner_lock t ~protect:true (fun () ->
    replace_foreground_retirement t (Some retirement);
    capture_retirement_releases_locked
      t
      ~recovery
      ~phase:Ordinary
      ~position:Ordinary_suffix
      ~closing);
  let dependency =
    Option.map
      (match coverage with
       | Whole_owner -> t.before_unload
       | Cell _ -> None)
      ~f:(fun prepare ->
        let step =
          Recovery_data.register_original_step recovery (Dependency_stop { closing })
        in
        step, prepare)
  in
  let policy_resource =
    Option.map t.policy_resource_cleanup ~f:(fun _ ->
      Recovery_data.register_original_step recovery (Policy_resource Ordinary))
  in
  let%bind () =
    Option.value_map dependency ~default:(Ok ()) ~f:(fun (step, prepare) ->
      Original_cleanup_step.capture step (fun () -> prepare ~closing)
      |> Originated_retirement.select_step step)
  in
  let%bind () = join_selected_original_retirement_consumers attempt selected_leases in
  let%bind () =
    List.fold_result selected_leases ~init:() ~f:(fun () (lease, enrollment) ->
      let%bind () = check_retirement_coverage t ~recovery ~coverage in
      match enrollment.Recovery_data.role with
      | Source_lexical -> invalid_arg "retirement lost its cancellation receipt"
      | Retirement_lease { cancel; _ } ->
        Scope_outcome.capture (fun () ->
          Original_cleanup_step.capture_unit cancel (fun () ->
            if Background_lease.is_active lease then lease.cancel ());
          Ok ())
        |> Originated_retirement.select_step cancel)
  in
  List.iter leases ~f:(fun lease -> Eio.Promise.await lease.finished);
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () = drain_policy_claim_observed ~recovery t |> Originated_retirement.select in
  let actor_step = retirement_actor_step recovery in
  let%bind () =
    advance_original_actor_cleanup_observed t actor_step ~final:false
    |> Originated_retirement.select
  in
  let%bind () =
    await_original_foreground_observed actor_step |> Originated_retirement.select
  in
  (* Required even when the initial pass was Joined: this is the original final
     selected-parent/cell observation, not a deduplicated second target. *)
  let%bind () =
    advance_original_actor_cleanup_observed t actor_step ~final:true
    |> Originated_retirement.select
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    close_restored_policy_observed ~recovery:(recovery, Ordinary_suffix) t
    |> Originated_retirement.select
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    if closing
    then
      close_native_anchor_observed ~recovery:(recovery, Ordinary_suffix) t
      |> Originated_retirement.select
    else Ok ()
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    retry_native_releases_observed ~recovery:(recovery, Ordinary_suffix) t
    |> Originated_retirement.select
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    Option.value_map policy_resource ~default:(Ok ()) ~f:(fun step ->
      Original_cleanup_step.capture_reported step (fun () ->
        close_pending_policy_resource_reported t)
      |> Originated_retirement.select_step step)
  in
  let cleanup = Cleanup_request.create () in
  let detach =
    Recovery_data.register_original_step recovery Retirement_detach_selection
  in
  let detached_primary =
    Scope_outcome.capture (fun () ->
      with_owner_lock t ~protect:true (fun () ->
        let%bind () = Retirement_coverage_guard.before_effect t coverage in
        if factual_cleanup_pending t
        then Error (retired_cleanup_error ())
        else Ok (retire_runtime_locked t cleanup)))
  in
  (* The successful outer selection still carries the actual detach result.
     Retain that negative now, but resume only the OUTER result so the original
     partial queue runs before detach-first cleanup projection below. *)
  Original_cleanup_step.record_projection
    detach
    (match detached_primary with
     | Scope_outcome.Returned (Ok (Ok ())) -> Scope_outcome.Returned (Ok ())
     | Returned (Ok (Error (exn, backtrace))) -> Raised (exn, backtrace)
     | Returned (Error error) -> Returned (Error error)
     | Raised (exn, backtrace) -> Raised (exn, backtrace));
  let%bind detached = Originated_retirement.select_step detach detached_primary in
  let batch = run_cleanup_request_reported t cleanup in
  Recovery_data.record_batch recovery ~role:Retirement_ordinary batch;
  let detached_origin =
    Cleanup_projection.origin (Original_cleanup_step.projection detach ~acquired:true)
  in
  let batch_origin = Cleanup_projection.origin (Cleanup_batch.projection batch) in
  let origin ~selected =
    Option.map selected ~f:(fun selected ->
      { selected with
        Physical_origin.diagnostics =
          Option.value_map detached_origin ~default:[] ~f:(fun origin ->
            origin.Physical_origin.diagnostics)
          @ Option.value_map batch_origin ~default:[] ~f:(fun origin ->
            origin.Physical_origin.diagnostics)
      })
  in
  (* An actual batch escape precedes the old detach-first normal-result
     projection. A normally returned batch still preserves detach-first. *)
  match batch.Cleanup_batch.original with
  | Raised (exn, trace) ->
    Originated_retirement.select_primary
      ?origin:
        (origin ~selected:(Originated_retirement.origin_of_diagnostic batch.diagnostic))
      (Raised (exn, trace))
  | Returned cleaned ->
    (match detached, cleaned with
     | Error (exn, trace), _ ->
       Originated_retirement.select_primary
         ?origin:(origin ~selected:detached_origin)
         (Raised (exn, trace))
     | Ok (), Error (exn, trace) ->
       Originated_retirement.select_primary
         ?origin:(origin ~selected:batch_origin)
         (Raised (exn, trace))
     | Ok (), Ok () ->
       join_selected_retirement_consumers
         Relation_evidence.Join_mode.Reobserve
         selected_leases)

and perform_complete_resource_retirement t ~(attempt : Resource_retirement_data.t) =
  let recovery = attempt.recovery in
  let coverage = attempt.coverage in
  let open Result.Let_syntax in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let actor_step = retirement_actor_step recovery in
  let%bind () =
    match actor_step.Actor_cleanup_step.final with
    | Some outcome ->
      resume_actor_cleanup_pass_observed
        ~final:true
        ?diagnostic:actor_step.final_diagnostic
        outcome
      |> Originated_retirement.select
    | None ->
      Originated_retirement.select_primary
        (Scope_outcome.Returned (Error (retired_cleanup_error ())))
  in
  let selection =
    Recovery_data.register_original_step recovery Complete_resource_selection
  in
  let%bind leases =
    Original_cleanup_step.capture selection (fun () ->
      with_owner_lock t ~protect:true (fun () ->
        let%bind () = Retirement_coverage_guard.before_effect t coverage in
        if
          (not t.closed)
          && List.exists t.background_leases ~f:(fun lease ->
            Background_lease.is_active lease
            && lease.survives_stop
            && Option.is_none lease.relation)
        then
          Error
            (Agent_protocol.Error.create
               Conflict
               ~message:"native resource borrowers require tracked dependency retirement"
               ~retryable:true
               ())
        else (
          capture_retirement_releases_locked
            t
            ~recovery
            ~phase:Complete_resources
            ~position:Complete_suffix
            ~closing:true;
          Ok t.background_leases)))
    |> Originated_retirement.select_step selection
  in
  let selected_leases =
    List.map leases ~f:(fun lease ->
      let enrollment =
        enroll_retirement_consumer t ~recovery ~phase:Complete_resources lease
      in
      lease, enrollment)
  in
  let policy_resource =
    Option.map t.policy_resource_cleanup ~f:(fun _ ->
      Recovery_data.register_original_step recovery (Policy_resource Complete_resources))
  in
  (* Reusable resource cleanup cannot cancel an unidentified surviving consumer.
     Permanent close keeps its existing stronger cancellation contract. *)
  let%bind () = join_selected_original_retirement_consumers attempt selected_leases in
  let%bind () =
    List.fold_result selected_leases ~init:() ~f:(fun () (lease, enrollment) ->
      let%bind () = check_retirement_coverage t ~recovery ~coverage in
      match enrollment.Recovery_data.role with
      | Source_lexical -> invalid_arg "retirement lost its cancellation receipt"
      | Retirement_lease { cancel; _ } ->
        Scope_outcome.capture (fun () ->
          Original_cleanup_step.capture_unit cancel (fun () ->
            if Background_lease.is_active lease then lease.cancel ());
          Ok ())
        |> Originated_retirement.select_step cancel)
  in
  List.iter leases ~f:(fun lease -> Eio.Promise.await lease.finished);
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  (* Joined borrower finalizers may have produced factual retirement work after
     the ordinary phase drained it. Use full-close ordering without permanently
     closing admission or treating a queued finalizer as joined. *)
  let constructor =
    Recovery_data.register_original_step recovery Complete_constructor_drain
  in
  let callback = Recovery_data.register_original_step recovery Complete_callback_drain in
  let descendant =
    Recovery_data.register_original_step recovery Complete_descendant_drain
  in
  let inference =
    Recovery_data.register_original_step recovery Complete_inference_drain
  in
  let%bind () =
    drain_policy_claim_observed ~recovery ~slot:Complete_resources t
    |> Originated_retirement.select
  in
  (* These are real additional original calls, selected after borrower finalizers.
     They do not enlarge the original Retirement_cleanup selection. Their otherwise
     unowned negatives stay immutable until an actual lexical owner retains them. *)
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    Original_cleanup_step.capture constructor (fun () ->
      Agent_session.Session_actor.drain_constructor_model_retirement t.actor)
    |> Originated_retirement.select_step constructor
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    Original_cleanup_step.capture callback (fun () ->
      Agent_session.Session_actor.drain_callback_retirement t.actor)
    |> Originated_retirement.select_step callback
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    Original_cleanup_step.capture descendant (fun () ->
      Agent_session.Session_actor.drain_descendant_cleanup t.actor)
    |> Originated_retirement.select_step descendant
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    Original_cleanup_step.capture inference (fun () ->
      Agent_session.Session_actor.drain_inference_usage t.actor)
    |> Originated_retirement.select_step inference
  in
  let%bind () =
    await_original_foreground_observed (retirement_actor_step recovery)
    |> Originated_retirement.select
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    close_restored_policy_observed ~recovery:(recovery, Complete_suffix) t
    |> Originated_retirement.select
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    close_native_anchor_observed ~recovery:(recovery, Complete_suffix) t
    |> Originated_retirement.select
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    retry_native_releases_observed ~recovery:(recovery, Complete_suffix) t
    |> Originated_retirement.select
  in
  let%bind () = check_retirement_coverage t ~recovery ~coverage in
  let%bind () =
    Option.value_map policy_resource ~default:(Ok ()) ~f:(fun step ->
      Original_cleanup_step.capture_reported step (fun () ->
        close_pending_policy_resource_reported t)
      |> Originated_retirement.select_step step)
  in
  let cleanup = Cleanup_request.create () in
  with_owner_lock t ~protect:true (fun () ->
    match Retirement_coverage_guard.before_effect t coverage with
    | Error error -> raise (Cleanup_failed error)
    | Ok () -> retry_retired_resources_locked t cleanup);
  let batch = run_cleanup_request_reported t cleanup in
  Recovery_data.record_batch recovery ~role:Retirement_complete batch;
  let%bind () =
    match batch.Cleanup_batch.original with
    | Raised (exn, trace) ->
      let original = Cleanup_batch.projection batch in
      let origin =
        Option.map batch.diagnostic ~f:(fun primary ->
          { Physical_origin.diagnostics =
              Option.value_map
                (Cleanup_projection.origin original)
                ~default:[]
                ~f:(fun origin -> origin.Physical_origin.diagnostics)
          ; primary = Some primary
          })
      in
      Originated_retirement.select_primary ?origin (Raised (exn, trace))
    | Returned (Error (exn, trace)) ->
      Originated_retirement.select_primary
        ?origin:(Cleanup_projection.origin (Cleanup_batch.projection batch))
        (Raised (exn, trace))
    | Returned (Ok ()) -> Ok ()
  in
  let%bind () =
    join_selected_retirement_consumers
      Relation_evidence.Join_mode.Reobserve
      selected_leases
  in
  let check = Recovery_data.register_original_step recovery Complete_resource_check in
  Original_cleanup_step.capture check (fun () ->
    with_owner_lock t ~protect:true (fun () ->
      refresh_settled_source_cleanup_locked t;
      let physical_ready =
        retirement_background_ready_locked t ~recovery
        && List.is_empty t.retired_resources
        && Option.is_none t.binding
        && Option.is_none t.restored_policy
        && Option.is_none t.native_anchor
        && List.is_empty t.native_loans
        && (not (policy_cleanup_pending t))
        && (not (factual_cleanup_pending t))
        && foreground_retirement_finished t
      in
      let source = t.failed_source_cleanup in
      let source_custody_joined =
        Option.for_all source ~f:(fun source ->
          match source.Recovery_data.phase with
          | Original_running _ | Retrying _ -> false
          | Complete _ ->
            Cleanup_evidence.custody_joined (Recovery_data.Observation.scope source))
      in
      Recovery_data.record_disposal_check
        recovery
        { source; guard = check; physical_ready; source_custody_joined };
      if physical_ready && Option.is_none source
      then Ok ()
      else Error (background_busy ())))
  |> Originated_retirement.select_step check

and observe_retirement_consumer mode (lease : background_lease) =
  let module CO = Agent_session.Close_observation in
  capture_consumer_join (fun () ->
    match lease.relation with
    | None -> Canonical_consumer (CO.create (Returned (Ok ())) ~diagnostics:[] ())
    | Some relation ->
      let consumer_affinity =
        match relation.consumer with
        | Source { owner; _ } -> owner.affinity
        | Resource resource -> resource.owner.affinity
      in
      if Owner_affinity.same consumer_affinity lease.recovery.affinity
      then consumer_join_control "dependency consumer cannot join its own provider scope"
      else (
        let publish recovery = publish_consumer_join relation.report recovery in
        let reobserve_source source =
          match Source_consumer.join source with
          | Error error -> consumer_join_error error
          | Ok _ -> publish (Source_consumer.cleanup source)
        in
        let reobserve_resource resource =
          match resource.retirement with
          | Some attempt ->
            ignore
              (Resource_retirement_data.await_observed attempt : Originated_retirement.t);
            publish attempt.recovery
          | None ->
            (match resource.bound_source with
             | None ->
               consumer_join_control "released consumer has no actual cleanup report"
             | Some source ->
               (match Source_consumer.join source with
                | Error error -> consumer_join_error error
                | Ok _ ->
                  (match Resource_consumer_core.check resource with
                   | Error error -> consumer_join_error error
                   | Ok Held -> consumer_join_control "consumer target is not released"
                   | Ok Released -> publish (Source_consumer.cleanup source))))
        in
        match relation.consumer with
        | Source { source; owner } ->
          (match Source_consumer.join source with
           | Error error -> consumer_join_error error
           | Ok _ ->
             let recovery = Source_consumer.cleanup source in
             (match mode with
              | Relation_evidence.Join_mode.Original | Reobserve -> publish recovery
              | Recovery_retry ->
                (match consumer_needs_repair recovery with
                 | Error error -> consumer_join_error error
                 | Ok false -> reobserve_source source
                 | Ok true ->
                   (match recovery.Recovery_data.phase with
                    | Original_running _ | Retrying _ ->
                      consumer_join_control "consumer cleanup is not available for retry"
                    | Complete completed ->
                      (match
                         retry_cleanup_recovery recovery ~owner ~after:completed.latest
                       with
                       | Error error -> consumer_join_error error
                       | Ok _ -> reobserve_source source)))))
        | Resource resource ->
          (match mode with
           | Relation_evidence.Join_mode.Reobserve -> reobserve_resource resource
           | Original | Recovery_retry ->
             let request =
               match mode, resource.retirement with
               | Recovery_retry, Some failed ->
                 let open Result.Let_syntax in
                 let%bind _ =
                   Resource_consumer_core.check_target resource.owner resource.target
                 in
                 (match Resource_retirement_data.latest_retry failed with
                  | Some successor ->
                    (* Only the actual Owner's authorized same-coverage repair.
                       Its completion stays observable after the Owner slot clears. *)
                    remember_cell_claim
                      resource
                      (Resource_retirement_claim.Join successor)
                  | None ->
                    (match consumer_needs_repair failed.recovery with
                     | Error error -> Error error
                     | Ok false -> Ok (Resource_consumer_request.Retirement (Join failed))
                     | Ok true ->
                       retry_cell_retirement resource ~failed_recovery:failed.recovery))
               | Original, (None | Some _) | Recovery_retry, None ->
                 let origin =
                   match relation.report.transport with
                   | Relation_evidence.Ordinary_scope ->
                     Resource_consumer_request.External
                   | Native_scope _ -> Retained_native_dependency
                 in
                 signal_cell_retirement ~origin resource
               | Reobserve, (None | Some _) ->
                 invalid_arg "reobserve entered a consumer election"
             in
             (match request with
              | Error error -> consumer_join_error error
              | Ok Resource_consumer_request.Released -> reobserve_resource resource
              | Ok (Retirement claim) ->
                let attempt =
                  match claim with
                  | Resource_retirement_claim.Run attempt | Join attempt -> attempt
                in
                run_retirement_attempt attempt;
                ignore
                  (Resource_retirement_data.await_observed attempt
                   : Originated_retirement.t);
                publish attempt.recovery))))

and run_retirement_consumer selection mode ~cancellation (lease : background_lease) =
  let join =
    match selection with
    | Relation_evidence.Selection.Active_revocation ->
      observe_retirement_consumer mode lease
    | Detached_handoff ->
      (* Operational no-op only: canonical provider/receipt/finished checks stay
         mandatory. Do not await here before cancellation of all selected leases. *)
      Relation_evidence.Join_outcome.Continue_provider
  in
  let cancellation =
    match
      ( cancellation
      , Agent_session.Close_observation.exit
          (Relation_evidence.Join_outcome.observation join) )
    with
    | Relation_evidence.Cancellation.Attempt, Returned (Ok ()) ->
      Some
        (Physical_origin.opaque_result (fun () ->
           (* This is the retained authentic action's first selected invocation,
              including its honest no-op branch after lexical detachment. *)
           if
             Background_lease.is_active lease
             && Option.is_none (Eio.Promise.peek lease.finished)
           then lease.cancel ();
           Ok ()))
    | Skip, _ | Attempt, (Returned (Error _) | Raised _) -> None
  in
  { Relation_evidence.Run_result.join; cancellation }

and enroll_retirement_consumer
      owner
      ~recovery
      ~(phase : Retirement_cleanup_phase.t)
      (lease : background_lease)
  =
  (* Enrollment and first selection share the SAME lock acquisition: waiting for
     this lock cannot classify an already-enrolled active revocation as history. *)
  with_owner_lock owner ~protect:true (fun () ->
    let enrollment =
      Recovery_data.register_lease
        ?relation:(relation_report lease)
        recovery
        ~position:(Retirement phase)
        ~finished:lease.finished
        ~recovery:lease.recovery
    in
    install_retirement_consumer_locked ~enrollment lease;
    enrollment)

and install_retirement_consumer_locked ~enrollment (lease : background_lease) =
  (match enrollment.Recovery_data.role with
   | Source_lexical ->
     invalid_arg "source lexical lease cannot install retirement operation"
   | Retirement_lease _ -> ());
  if
    (not (phys_equal enrollment.finished lease.finished))
    || (not (phys_equal enrollment.recovery lease.recovery))
    || not (Option.equal phys_equal enrollment.relation (relation_report lease))
  then invalid_arg "retirement operation must retain its actual selected lease";
  match enrollment.operation with
  | Some _ -> ()
  | None ->
    let selection =
      if Background_lease.is_active lease
      then Relation_evidence.Selection.Active_revocation
      else Detached_handoff
    in
    let operation =
      { Relation_evidence.Operation.selection
      ; run =
          (fun mode ~cancellation ->
            run_retirement_consumer selection mode ~cancellation lease)
      }
    in
    let operation =
      match lease.relation with
      | None -> operation
      | Some relation -> Relation_evidence.install_operation relation.report operation
    in
    enrollment.operation <- Some operation

and join_retirement_consumer_observed ~enrollment ~execution ~recording _lease =
  match enrollment.Recovery_data.operation with
  | None -> invalid_arg "retirement consumer operation was not installed"
  | Some operation ->
    let outcome =
      operation.run execution ~cancellation:Relation_evidence.Cancellation.Skip
    in
    Recovery_data.record_consumer_join enrollment ~mode:recording outcome;
    Relation_evidence.Run_result.observation outcome

and join_selected_original_retirement_consumers attempt selected_leases =
  let execution = First_consumer_join.execution (First_consumer_join.select attempt) in
  List.fold_result selected_leases ~init:() ~f:(fun () (lease, enrollment) ->
    (* Explicit successor repair still belongs to this NEW holder's original
       invocation. Its result must not be recorded as a retry of this holder. *)
    join_retirement_consumer_observed ~enrollment ~execution ~recording:Original lease
    |> Originated_retirement.select)

and join_selected_retirement_consumers mode selected_leases =
  List.fold_result selected_leases ~init:() ~f:(fun () (lease, enrollment) ->
    join_retirement_consumer_observed ~enrollment ~execution:mode ~recording:mode lease
    |> Originated_retirement.select)

and capture_retirement f =
  match f () with
  | Ok () -> { Originated_retirement.outcome = Ok (Ok ()); origin = None }
  | Error actual -> actual
  | exception exn ->
    Originated_retirement.opaque (Error (exn, Stdlib.Printexc.get_raw_backtrace ()))

and start_retirement t (attempt : Resource_retirement_data.t) =
  with_owner_lock t ~protect:true (fun () ->
    match attempt.phase with
    | Queued ->
      attempt.phase <- Running;
      true
    | Running | Finished _ -> false)

and finish_retirement t (attempt : Resource_retirement_data.t) ~entered completed outcome =
  let module R = Resource_retirement_data in
  let rec finish ~entered completed (actual : Originated_retirement.t) =
    let outcome = actual.outcome in
    let disposition =
      with_owner_lock t ~protect:true (fun () ->
        match completed, attempt.strength, outcome with
        | R.Ordinary, All_resources, Ok (Ok ()) -> `Complete_resources
        | (Ordinary | All_resources), (Ordinary | All_resources), _ ->
          let associated =
            let captured =
              match attempt.recovery.phase with
              | Original_running captured -> captured
              | Complete _ | Retrying _ ->
                invalid_arg "retirement original completion was already published"
            in
            (not (Retirement_coverage.is_whole_owner attempt.coverage))
            || List.exists captured.leases_rev ~f:(fun (lease : Recovery_data.lease) ->
              Option.is_some lease.relation)
          in
          let actual =
            if associated
            then (
              let observed =
                Agent_session.Close_observation.create
                  (match actual.outcome with
                   | Ok result -> Returned result
                   | Error (exn, trace) -> Raised (exn, trace))
                  ~diagnostics:
                    (Option.value_map actual.origin ~default:[] ~f:(fun origin ->
                       origin.Physical_origin.diagnostics))
                  ?primary:(Option.bind actual.origin ~f:(fun origin -> origin.primary))
                  ()
              in
              Recovery_data.finish_retirement_original
                attempt.recovery
                ~completion:(Retirement { required = attempt.strength; entered })
                ~actual:observed
              |> Originated_retirement.of_observation)
            else (
              Recovery_data.finish_original
                attempt.recovery
                ~projection:
                  (Cleanup_projection.create
                     ~boundary_diagnostics:
                       (Option.value_map actual.origin ~default:[] ~f:(fun origin ->
                          origin.Physical_origin.diagnostics))
                     (Scope_outcome.Cleanup.of_result ~acquired:true outcome)
                     ~origin:actual.origin)
                ~completion:(Retirement { required = attempt.strength; entered })
                (Scope_outcome.Cleanup.of_result ~acquired:true outcome);
              actual)
          in
          let outcome = actual.outcome in
          let custody_joined =
            match attempt.recovery.phase with
            | Original_running _ | Retrying _ -> false
            | Complete completed ->
              Result.is_ok (Cleanup_snapshot.observation completed.latest)
              && Cleanup_evidence.custody_joined
                   (Recovery_data.Observation.scope attempt.recovery)
              && Recovery_data.original_diagnostics_complete attempt.recovery
          in
          (* The actual completed R and returned report already own the SAME
             selected report slots. Only now may physical list metadata go. *)
          reap_retirement_relations_locked t ~recovery:attempt.recovery;
          attempt.phase <- Finished outcome;
          attempt.perform <- None;
          attempt.predecessor <- None;
          attempt.source_extent <- None;
          let settled =
            if associated
            then custody_joined
            else
              R.is_success outcome
              ||
              match attempt.strength with
              | Ordinary -> true
              | All_resources -> false
          in
          let unloading = Option.exists t.unloading ~f:(phys_equal attempt) in
          let pending =
            Option.exists t.pending_resource_retirement ~f:(phys_equal attempt)
          in
          (* Foreground completion alone cannot certify Owner cleanup. Record
             the exact receipt only when the elected whole retirement succeeds. *)
          if (unloading || pending) && Retirement_coverage.is_whole_owner attempt.coverage
          then
            t.joined_foreground_retirement
            <- (if settled && R.is_success outcome then t.foreground_retirement else None);
          if settled
          then (
            if unloading then t.unloading <- None;
            if pending then t.pending_resource_retirement <- None);
          Eio.Promise.resolve attempt.finish actual;
          `Finished)
    in
    match disposition with
    | `Finished -> ()
    | `Complete_resources ->
      finish
        ~entered:Original_completion.Retirement.Entered.Complete_resources
        All_resources
        (capture_retirement (fun () -> perform_complete_resource_retirement t ~attempt))
  in
  finish ~entered completed outcome

and perform_retirement_body t ~leases ~closing (attempt : Resource_retirement_data.t) =
  finish_retirement
    t
    attempt
    ~entered:Original_completion.Retirement.Entered.Ordinary
    Resource_retirement_data.Ordinary
    (capture_retirement (fun () ->
       perform_ordinary_retirement t ~attempt ~leases ~closing))

and perform_retirement t ~leases ~closing (attempt : Resource_retirement_data.t) =
  Eio.Cancel.protect (fun () ->
    if start_retirement t attempt then perform_retirement_body t ~leases ~closing attempt)

and run_retirement_attempt (attempt : Resource_retirement_data.t) =
  Option.iter attempt.perform ~f:(fun perform -> perform attempt)

and perform_pending_retirement t (attempt : Resource_retirement_data.t) =
  Eio.Cancel.protect (fun () ->
    if start_retirement t attempt
    then (
      let prepared =
        try
          let open Result.Let_syntax in
          let%bind extent, cancel_source, ordinary =
            Scope_outcome.capture (fun () ->
              with_owner_lock t ~protect:true (fun () ->
                if
                  not
                    (Option.exists t.pending_resource_retirement ~f:(phys_equal attempt))
                then
                  Error
                    (Agent_protocol.Error.invalid_request
                       "pending retirement was displaced")
                else (
                  let%bind status = Retirement_coverage_guard.check t attempt.coverage in
                  let%bind actual = current_source_extent t in
                  let%bind () =
                    Option.value_map
                      actual
                      ~default:(Ok ())
                      ~f:(attach_pending_source_extent t)
                  in
                  Option.iter
                    t.administration_transition
                    ~f:Administration_transition.retire;
                  Option.iter t.prepared_source ~f:close_source_stage;
                  Ok
                    ( attempt.source_extent
                    , Option.bind t.prepared_source ~f:(fun stage -> stage.cancel_lease)
                    , match attempt.coverage, status with
                      | ( Cell _
                        , Some Session_policy_resource.Cleanup_target.Status.Released ) ->
                        None
                      | Whole_owner, None | (Whole_owner | Cell _), Some (Held _) ->
                        t.unloading
                      | Whole_owner, Some Released | Cell _, None ->
                        invalid_arg "retirement coverage changed its kind" ))))
            |> Originated_retirement.select_primary
          in
          (* Never cancel or join while holding either Owner or Registry locks.
             A source finalizer may itself require the ordinary producer. *)
          Option.iter cancel_source ~f:(fun cancel -> cancel ());
          Option.iter ordinary ~f:run_retirement_attempt;
          let%bind () =
            Option.value_map extent ~default:(Ok ()) ~f:(fun extent ->
              let actual = Eio.Promise.await extent.Source_extent.finished in
              let primary =
                match Cleanup_projection.cleanup actual with
                | Scope_outcome.Cleanup.Never_acquired | Joined ->
                  Scope_outcome.Returned (Ok ())
                | Failed (Protocol error, _) -> Returned (Error error)
                | Failed (Raised (exn, trace), _) -> Raised (exn, trace)
              in
              Originated_retirement.select_primary
                ?origin:(Cleanup_projection.origin actual)
                primary)
          in
          let rec promote () =
            let%bind disposition =
              Scope_outcome.capture (fun () ->
                with_owner_lock t ~protect:true (fun () ->
                  if
                    not
                      (Option.exists
                         t.pending_resource_retirement
                         ~f:(phys_equal attempt))
                  then
                    Error
                      (Agent_protocol.Error.invalid_request
                         "pending retirement was displaced")
                  else if
                    Option.is_some t.prepared_source
                    || Option.is_some t.administration_transition
                  then
                    Error
                      (Agent_protocol.Error.create
                         Conflict
                         ~message:"source extent has not finished"
                         ~retryable:true
                         ())
                  else (
                    let%bind status =
                      Retirement_coverage_guard.check t attempt.coverage
                    in
                    let replace_failed_predecessor current =
                      Option.exists attempt.predecessor ~f:(phys_equal current)
                      && Retirement_coverage.equal
                           current.Resource_retirement_data.coverage
                           attempt.coverage
                      &&
                      match current.phase with
                      | Finished outcome ->
                        not (Resource_retirement_data.is_success outcome)
                      | Queued | Running -> false
                    in
                    match attempt.coverage, status with
                    | ( Cell cell
                      , Some Session_policy_resource.Cleanup_target.Status.Released ) ->
                      (* Source may have released A while unrelated B became
                         current. Never run/join or replace B's unloading here.
                         Only this exact failed predecessor can be superseded. *)
                      Option.iter t.unloading ~f:(fun current ->
                        if replace_failed_predecessor current
                        then t.unloading <- Some attempt);
                      Ok (`Released cell)
                    | Whole_owner, None | (Whole_owner | Cell _), Some (Held _) ->
                      let transfer () =
                        (* Post-ACK physical custody is selected only now. *)
                        t.pending_resource_retirement <- None;
                        t.unloading <- Some attempt;
                        Option.iter t.restored_policy ~f:Restored_policy_anchor.revoke;
                        Ok (`Promoted (t.background_leases, t.closed))
                      in
                      (match t.unloading with
                       | None -> transfer ()
                       | Some current when replace_failed_predecessor current ->
                         transfer ()
                       | Some current -> Ok (`Join current))
                    | Whole_owner, Some Released | Cell _, None ->
                      invalid_arg "retirement coverage changed its kind")))
              |> Originated_retirement.select_primary
            in
            match disposition with
            | `Promoted snapshot -> Ok (`Physical snapshot)
            | `Released cell -> Ok (`Released cell)
            | `Join current ->
              run_retirement_attempt current;
              let actual = Resource_retirement_data.await_observed current in
              let%bind () =
                match actual.Originated_retirement.outcome with
                | Ok (Ok ()) -> Ok ()
                | Ok (Error _) | Error _ -> Error actual
              in
              promote ()
          in
          let%bind () =
            Option.value_map ordinary ~default:(Ok ()) ~f:(fun ordinary ->
              if Option.exists attempt.predecessor ~f:(phys_equal ordinary)
              then Ok ()
              else (
                let actual = Resource_retirement_data.await_observed ordinary in
                match actual.Originated_retirement.outcome with
                | Ok (Ok ()) -> Ok ()
                | Ok (Error _) | Error _ -> Error actual))
          in
          promote ()
        with
        | exn ->
          Error
            (Originated_retirement.opaque
               (Error (exn, Stdlib.Printexc.get_raw_backtrace ())))
      in
      match prepared with
      | Ok (`Physical (leases, closing)) ->
        perform_retirement_body t ~leases ~closing attempt
      | Ok (`Released cell) -> finish_released_retirement t attempt cell
      | Error actual ->
        finish_retirement t attempt ~entered:Not_entered All_resources actual))

and finish_released_retirement t (attempt : Resource_retirement_data.t) cell =
  let recovery = attempt.recovery in
  let actual =
    capture_retirement (fun () ->
      let open Result.Let_syntax in
      (* This exact unit was already selected as Released. Its status is sticky:
         capture that actual fact before a child join can fail, so later factual
         retry need not invent an unexecuted target check or select current B.
         Source and selected-lease custody remain independent required duties. *)
      let step =
        Recovery_data.register_original_step
          recovery
          (Released_resource_check cell.target)
      in
      let%bind () =
        Original_cleanup_step.capture step (fun () ->
          with_owner_lock t ~protect:true (fun () ->
            let%bind status = Resource_consumer_core.check_target t cell.target in
            match status with
            | Session_policy_resource.Cleanup_target.Status.Released -> Ok ()
            | Held _ -> Error (Retirement_coverage_guard.conflict ())))
        |> Originated_retirement.select_step step
      in
      let leases =
        with_owner_lock t ~protect:true (fun () ->
          List.filter t.background_leases ~f:(Retirement_coverage_guard.background t cell))
      in
      let selected =
        List.map leases ~f:(fun lease ->
          let enrollment = enroll_retirement_consumer t ~recovery ~phase:Ordinary lease in
          lease, enrollment)
      in
      let%bind () = join_selected_original_retirement_consumers attempt selected in
      let%bind () =
        List.fold_result selected ~init:() ~f:(fun () (lease, enrollment) ->
          match enrollment.Recovery_data.role with
          | Source_lexical ->
            invalid_arg "released retirement lost its cancellation receipt"
          | Retirement_lease { cancel; _ } ->
            Scope_outcome.capture (fun () ->
              Original_cleanup_step.capture_unit cancel (fun () ->
                if Background_lease.is_active lease then lease.cancel ());
              Ok ())
            |> Originated_retirement.select_step cancel)
      in
      List.iter leases ~f:(fun lease -> Eio.Promise.await lease.finished);
      let%bind () =
        join_selected_retirement_consumers Relation_evidence.Join_mode.Reobserve selected
      in
      Ok ())
  in
  finish_retirement t attempt ~entered:Released_before_physical All_resources actual

and new_retirement_locked ?repair_of t strength =
  let closing = t.closed && not (resource_retirement_pending t) in
  let attempt =
    Resource_retirement_data.create
      ~mutex:t.mutex
      ~domain:t.domain
      ~affinity:t.affinity
      ~coverage:Whole_owner
      ~repair_of
      ~retry_source:None
      ~strength
      ~perform:(perform_retirement t ~leases:t.background_leases ~closing)
  in
  Option.iter t.administration_transition ~f:Administration_transition.retire;
  Option.iter t.prepared_source ~f:close_source_stage;
  Option.iter t.restored_policy ~f:Restored_policy_anchor.revoke;
  t.close_finished <- false;
  t.disposal_cache <- Disposal_cache.supersede t.disposal_cache;
  t.unloading <- Some attempt;
  Option.iter repair_of ~f:(fun _ -> Resource_retirement_data.publish_repair attempt);
  attempt

(* Same-domain/non-yielding: association to an actual extent is not a resource
   snapshot, and stays observable even if that stage clears before run starts. *)
and new_pending_retirement
      t
      ?predecessor
      ?(coverage = Retirement_coverage.Whole_owner)
      ?retry_of
      ()
  =
  let open Result.Let_syntax in
  let%bind current_extent = current_source_extent t in
  let%bind extent =
    match coverage with
    | Whole_owner -> Ok current_extent
    | Cell cell ->
      (match current_extent, cell.source with
       | Some actual, Some source when not (phys_equal actual source.stage.extent) ->
         Error (Retirement_coverage_guard.conflict ())
       | Some actual, (Some _ | None) -> Ok (Some actual)
       | None, Some source -> Ok (Some source.stage.extent)
       | None, None -> Ok None)
  in
  let%bind () =
    match coverage with
    | Whole_owner -> Ok ()
    | Cell cell ->
      let%bind _ = Retirement_coverage_guard.check t coverage in
      if
        Option.for_all extent ~f:(fun extent ->
          Retirement_coverage_guard.source cell extent.Source_extent.recovery)
      then Ok ()
      else Error (Retirement_coverage_guard.conflict ())
  in
  let attempt =
    Resource_retirement_data.create
      ~mutex:t.mutex
      ~domain:t.domain
      ~affinity:t.affinity
      ~coverage
      ~repair_of:retry_of
      ~retry_source:
        (Option.map retry_of ~f:(fun failed -> failed.Resource_retirement_data.recovery))
      ~strength:All_resources
      ~perform:(perform_pending_retirement t)
  in
  attempt.predecessor <- predecessor;
  Option.iter extent ~f:(fun extent ->
    Recovery_data.register_source attempt.recovery ~source:extent.Source_extent.recovery);
  attempt.source_extent <- extent;
  t.close_finished <- false;
  t.disposal_cache <- Disposal_cache.supersede t.disposal_cache;
  t.pending_resource_retirement <- Some attempt;
  Option.iter retry_of ~f:(fun _ -> Resource_retirement_data.publish_repair attempt);
  Ok attempt

and cell_retirement_coverage (consumer : resource_consumer) =
  Retirement_coverage.Cell
    { target = consumer.target; source = consumer.bound_source; facets = consumer.facets }

and same_cell_retirement
      (consumer : resource_consumer)
      (attempt : Resource_retirement_data.t)
  =
  match attempt.coverage with
  | Retirement_coverage.Cell actual ->
    Session_policy_resource.Cleanup_target.equal actual.target consumer.target
  | Whole_owner -> false

and remember_cell_claim (consumer : resource_consumer) claim =
  let attempt =
    match claim with
    | Resource_retirement_claim.Run attempt | Join attempt -> attempt
  in
  consumer.retirement <- Some attempt;
  Ok (Resource_consumer_request.Retirement claim)

and signal_cell_retirement ~origin (consumer : resource_consumer) =
  let open Result.Let_syntax in
  (* Target/domain validation precedes every mutable election read. A retained
     original handle is observed before the sticky Released shortcut. *)
  let%bind status = Resource_consumer_core.check_target consumer.owner consumer.target in
  let owner = consumer.owner in
  match consumer.retirement with
  | Some attempt -> remember_cell_claim consumer (Join attempt)
  | None ->
    let coverage = cell_retirement_coverage consumer in
    let current = Option.first_some owner.pending_resource_retirement owner.unloading in
    (match current with
     | Some attempt when same_cell_retirement consumer attempt ->
       remember_cell_claim consumer (Join attempt)
     | Some _ | None ->
       (match status with
        | Session_policy_resource.Cleanup_target.Status.Released ->
          (* Sticky A is independent of any unrelated current B election. No
             joining, strengthening, cancellation or physical work on B. *)
          Ok Resource_consumer_request.Released
        | Held _ ->
          (match current with
           | Some attempt ->
             (match attempt.coverage, attempt.strength, attempt.phase with
              | Whole_owner, All_resources, (Queued | Running) ->
                (* Join already-authorized broader work under its continuous
                   fence; this request never elects or widens Whole work. *)
                remember_cell_claim consumer (Join attempt)
              | Whole_owner, (Ordinary | All_resources), Finished _
              | Whole_owner, Ordinary, (Queued | Running)
              | Cell _, (Ordinary | All_resources), (Queued | Running | Finished _) ->
                Error (Retirement_coverage_guard.conflict ()))
           | None ->
             let%bind () = Retirement_coverage_guard.before_effect owner coverage in
             if not owner.closed
             then (
               let%bind attempt = new_pending_retirement owner ~coverage () in
               remember_cell_claim consumer (Run attempt))
             else (
               match origin, owner.joined_close_handoff with
               | Retained_native_dependency, Some ({ attempt = None } as handoff) ->
                 (* This enrolled native dependency reached a Held Cell while
                    its Owner's joined close was in the permanent prelude.
                    Dispose that exact Cell first. The joined close observes
                    this same completion before selecting its own Whole work. *)
                 let%bind attempt = new_pending_retirement owner ~coverage () in
                 handoff.attempt <- Some attempt;
                 remember_cell_claim consumer (Run attempt)
               | (External | Retained_native_dependency), _ ->
                 Error
                   (Resource_consumer_core.conflict
                      "resource consumer cannot elect after permanent close")))))

and retry_cell_retirement (consumer : resource_consumer) ~failed_recovery =
  let open Result.Let_syntax in
  let%bind _ = Resource_consumer_core.check_target consumer.owner consumer.target in
  let owner = consumer.owner in
  let%bind failed =
    match consumer.retirement with
    | Some actual when phys_equal actual.recovery failed_recovery -> Ok actual
    | Some successor
      when Option.exists successor.retry_source ~f:(phys_equal failed_recovery) ->
      Ok successor
    | Some _ | None -> Error (Retirement_coverage_guard.conflict ())
  in
  if Option.exists failed.retry_source ~f:(phys_equal failed_recovery)
  then remember_cell_claim consumer (Join failed)
  else (
    let%bind () =
      match failed.coverage with
      | Retirement_coverage.Cell _ -> Ok ()
      | Whole_owner -> Error (Retirement_coverage_guard.conflict ())
    in
    let current = Option.first_some owner.pending_resource_retirement owner.unloading in
    match current with
    | Some successor
      when same_cell_retirement consumer successor
           && Option.exists successor.retry_source ~f:(phys_equal failed_recovery) ->
      remember_cell_claim consumer (Join successor)
    | Some current when phys_equal current failed ->
      (match failed.phase with
       | Finished outcome when not (Resource_retirement_data.is_success outcome) ->
         let%bind () =
           match owner.unloading with
           | Some physical when not (phys_equal physical failed) ->
             if Retirement_coverage.equal physical.coverage failed.coverage
             then Ok ()
             else Error (Retirement_coverage_guard.conflict ())
           | Some _ | None -> Ok ()
         in
         let predecessor = Option.value owner.unloading ~default:failed in
         let%bind attempt =
           new_pending_retirement
             owner
             ~coverage:failed.coverage
             ~retry_of:failed
             ~predecessor
             ()
         in
         remember_cell_claim consumer (Run attempt)
       | Queued | Running | Finished _ -> Error (Retirement_coverage_guard.conflict ()))
    | Some _ | None -> Error (Retirement_coverage_guard.conflict ()))
;;

module Resource_retirement = struct
  type t = Resource_retirement_data.t
  type work = Resource_retirement_data.t

  type claim = Resource_retirement_claim.t =
    | Run of work
    | Join of t

  let completion work = work

  let peek t =
    match Resource_retirement_data.check_domain t with
    | Error error -> raise (Cleanup_failed error)
    | Ok () ->
      Option.map (Eio.Promise.peek t.finished) ~f:(fun actual ->
        match actual.Originated_retirement.outcome with
        | Ok result -> Scope_outcome.Returned result
        | Error (exn, backtrace) -> Scope_outcome.Raised (exn, backtrace))
  ;;

  let run work =
    match Resource_retirement_data.check_domain work with
    | Ok () -> run_retirement_attempt work
    | Error error -> raise (Cleanup_failed error)
  ;;

  let fail_to_start work failure =
    match Resource_retirement_data.check_domain work with
    | Error error -> raise (Cleanup_failed error)
    | Ok () ->
      Eio.Mutex.use_rw ~protect:true work.mutex (fun () ->
        match work.phase with
        | Queued ->
          let outcome =
            match failure with
            | Scope_outcome.Cleanup.Failure.Protocol error -> Ok (Error error)
            | Raised (exn, backtrace) -> Error (exn, backtrace)
          in
          let actual = Originated_retirement.opaque outcome in
          Recovery_data.finish_original
            work.recovery
            ~projection:
              (Cleanup_projection.create
                 ~boundary_diagnostics:
                   (Option.value_map actual.origin ~default:[] ~f:(fun origin ->
                      origin.Physical_origin.diagnostics))
                 (Scope_outcome.Cleanup.of_result ~acquired:true outcome)
                 ~origin:actual.origin)
            ~completion:(Retirement { required = work.strength; entered = Not_entered })
            (Scope_outcome.Cleanup.of_result ~acquired:true outcome);
          work.phase <- Finished outcome;
          work.perform <- None;
          work.predecessor <- None;
          work.source_extent <- None;
          Eio.Promise.resolve work.finish actual
        | Running | Finished _ -> ())
  ;;

  let await = Resource_retirement_data.await
end

module Resource_consumer = struct
  include Resource_consumer_core

  module Request = struct
    type t = Resource_consumer_request.t =
      | Released
      | Retirement of Resource_retirement.claim
  end

  let signal t = signal_cell_retirement ~origin:External t

  let retry t ~failed =
    retry_cell_retirement t ~failed_recovery:failed.Resource_retirement_data.recovery
  ;;
end

let active_source_retirement_error () =
  Agent_protocol.Error.create
    Conflict
    ~message:"active source preparation must finish before resource retirement"
    ~retryable:true
    ()
;;

let claim_resource_retirement t =
  let open Result.Let_syntax in
  let%bind () =
    if Int.equal t.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "resource retirement belongs to another Eio domain")
  in
  with_owner_lock t ~protect:true (fun () ->
    (* Admin and source finalizers may themselves join ordinary unloading.
       Do not publish an election which would then wait on that same finalizer. *)
    if Option.is_some t.prepared_source || Option.is_some t.administration_transition
    then Error (active_source_retirement_error ())
    else (
      match t.pending_resource_retirement, t.unloading with
      | Some attempt, _ ->
        if Retirement_coverage.is_whole_owner attempt.coverage
        then Ok (Resource_retirement.Join attempt)
        else Error (Retirement_coverage_guard.conflict ())
      | None, None -> Ok (Resource_retirement.Run (new_retirement_locked t All_resources))
      | None, Some attempt ->
        if not (Retirement_coverage.is_whole_owner attempt.coverage)
        then Error (Retirement_coverage_guard.conflict ())
        else (
          (match attempt.phase with
           | Queued | Running -> attempt.strength <- All_resources
           | Finished _ -> ());
          Ok (Resource_retirement.Join attempt))))
;;

let signal_resource_retirement t =
  let open Result.Let_syntax in
  let%bind () =
    if Int.equal t.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "resource retirement belongs to another Eio domain")
  in
  match t.pending_resource_retirement with
  | Some attempt ->
    if Retirement_coverage.is_whole_owner attempt.coverage
    then Ok (Resource_retirement.Join attempt)
    else Error (Retirement_coverage_guard.conflict ())
  | None ->
    (match t.prepared_source, t.administration_transition, t.unloading with
     | None, None, Some attempt ->
       if not (Retirement_coverage.is_whole_owner attempt.coverage)
       then Error (Retirement_coverage_guard.conflict ())
       else (
         (match attempt.phase with
          | Queued | Running -> attempt.strength <- All_resources
          | Finished _ -> ());
         Ok (Resource_retirement.Join attempt))
     | (Some _ | None), (Some _ | None), (Some _ | None) ->
       let%map attempt = new_pending_retirement t () in
       Resource_retirement.Run attempt)
;;

let retry_signalled_resource_retirement t ~(failed : Resource_retirement.t) =
  let module R = Resource_retirement_data in
  let open Result.Let_syntax in
  let%bind () =
    if Int.equal t.domain (Stdlib.Domain.self () :> int)
    then R.check_domain failed
    else
      Error
        (Agent_protocol.Error.invalid_request
           "resource retirement belongs to another Eio domain")
  in
  let current = Option.first_some t.pending_resource_retirement t.unloading in
  if not (phys_equal failed.mutex t.mutex)
  then Error (Agent_protocol.Error.invalid_request "resource retirement owner differs")
  else (
    match current, failed.phase with
    | Some current, Finished outcome
      when phys_equal current failed && not (R.is_success outcome) ->
      (* The public failed handle may be a queued/prelude failure while an
         older failed physical producer still owns [unloading]. Carry that
         actual producer, not an unbounded chain of intervening attempts. *)
      let predecessor =
        match t.unloading with
        | Some actual ->
          (match actual.Resource_retirement_data.phase with
           | Finished outcome when not (R.is_success outcome) -> actual
           | Queued | Running | Finished _ -> failed)
        | None -> failed
      in
      let%bind () =
        if Retirement_coverage.equal predecessor.coverage failed.coverage
        then Ok ()
        else Error (Retirement_coverage_guard.conflict ())
      in
      let%map attempt =
        new_pending_retirement
          t
          ~predecessor
          ~coverage:failed.coverage
          ~retry_of:failed
          ()
      in
      Resource_retirement.Run attempt
    | (Some _ | None), (Queued | Running | Finished _) ->
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"resource retirement is not this owner's current failed attempt"
           ~retryable:false
           ()))
;;

let retry_resource_retirement t ~(failed : Resource_retirement.t) =
  let open Result.Let_syntax in
  let%bind () =
    if Int.equal t.domain (Stdlib.Domain.self () :> int)
    then Resource_retirement_data.check_domain failed
    else
      Error
        (Agent_protocol.Error.invalid_request
           "resource retirement belongs to another Eio domain")
  in
  with_owner_lock t ~protect:true (fun () ->
    if Option.is_some t.prepared_source || Option.is_some t.administration_transition
    then Error (active_source_retirement_error ())
    else retry_signalled_resource_retirement t ~failed)
;;

(* Called only while the Owner lock is held. The source finalizer uses this
   ordinary prefix without joining pending work that awaits its own extent. *)
let claim_ordinary_unload_locked t =
  match t.unloading with
  | Some attempt ->
    (match attempt.Resource_retirement_data.phase with
     | Finished outcome
       when Retirement_coverage.is_whole_owner attempt.coverage
            && attempt.delivered
            && not (Resource_retirement_data.is_success outcome) ->
       (* A pending resource retirement may already be waiting on this ordinary
          source prefix. Only an independently elected replacement supersedes
          the previously observed whole retirement. *)
       let repair_of = if resource_retirement_pending t then None else Some attempt in
       `Attempt (new_retirement_locked ?repair_of t attempt.strength)
     | Queued | Running | Finished _ -> `Attempt attempt)
  | None ->
    (match
       Option.bind t.prepared_source ~f:(fun stage ->
         Source_stage_retirement.completion stage.retirement)
     with
     | Some finished -> `Join_administration finished
     | None ->
       let strength =
         if t.closed && not (resource_retirement_pending t)
         then Resource_retirement_data.All_resources
         else Ordinary
       in
       `Attempt (new_retirement_locked t strength))
;;

let run_and_await_retirement attempt =
  Resource_retirement.run attempt;
  Resource_retirement.await attempt
;;

let run_and_await_retirement_observed attempt =
  Resource_retirement.run attempt;
  let observed = Resource_retirement_data.await_observed attempt in
  Agent_session.Close_observation.create
    (match observed.Originated_retirement.outcome with
     | Ok result -> Returned result
     | Error (exn, trace) -> Raised (exn, trace))
    ~diagnostics:
      (Option.value_map observed.origin ~default:[] ~f:(fun origin -> origin.diagnostics))
    ?primary:(Option.bind observed.origin ~f:(fun origin -> origin.primary))
    ()
;;

(* One joined-close invocation names only its actual selections and actual
   stage origins. It is never stored on Owner or used as cleanup authority. *)
module Close_invocation = struct
  module CO = Agent_session.Close_observation
  module Diagnostic = Agent_session.Cleanup_diagnostic

  type t =
    { mutable roots_rev : Recovery_data.t list
    ; mutable origins_rev : Physical_origin.t list
    ; mutable selected : Diagnostic.t option
    ; prior_diagnostics : Diagnostic.t list
    ; mutable completed_stamp : Disposal_cache.stamp option
    }

  let create ~prior =
    { roots_rev = []
    ; origins_rev = []
    ; selected = None
    ; prior_diagnostics =
        Option.value_map prior ~default:[] ~f:Disposal_report_data.diagnostics
    ; completed_stamp = None
    }
  ;;

  let root t recovery =
    if not (List.mem t.roots_rev recovery ~equal:phys_equal)
    then t.roots_rev <- recovery :: t.roots_rev
  ;;

  let attempt t (attempt : Resource_retirement_data.t) =
    root t attempt.recovery;
    Option.iter attempt.predecessor ~f:(fun predecessor -> root t predecessor.recovery);
    Option.iter attempt.source_extent ~f:(fun source -> root t source.recovery)
  ;;

  let origin t origin =
    Option.iter origin ~f:(fun origin -> t.origins_rev <- origin :: t.origins_rev)
  ;;

  let failure = function
    | Scope_outcome.Returned (Ok _) -> None
    | Returned (Error error) -> Some (Diagnostic.Failure.Protocol error)
    | Raised (exn, trace) -> Some (Diagnostic.Failure.Raised (exn, trace))
  ;;

  let actual_origin primary origin =
    match failure primary with
    | None -> origin
    | Some failure ->
      let known =
        Option.exists origin ~f:(fun origin ->
          Option.exists origin.Physical_origin.primary ~f:(fun diagnostic ->
            not (List.is_empty (Diagnostic.failures diagnostic))))
      in
      if known
      then origin
      else (
        let diagnostic = Diagnostic.create [ failure ] in
        Some
          { Physical_origin.diagnostics =
              Option.value_map origin ~default:[] ~f:(fun origin -> origin.diagnostics)
              @ [ diagnostic ]
          ; primary = Some diagnostic
          })
  ;;

  let observe t primary ~origin:incoming =
    let actual = actual_origin primary incoming in
    origin t actual;
    actual
  ;;

  let select t origin =
    t.selected <- Option.bind origin ~f:(fun origin -> origin.Physical_origin.primary)
  ;;

  let resume t primary ~origin:incoming =
    let actual = observe t primary ~origin:incoming in
    if Option.is_some (failure primary) then select t actual else t.selected <- None;
    Scope_outcome.resume_primary primary
  ;;

  let control t f = resume t (Scope_outcome.capture f) ~origin:None

  let unit t f =
    ignore
      (control t (fun () ->
         f ();
         Ok ())
       : (unit, _) result)
  ;;

  let result_exn t observed =
    match
      resume
        t
        (Physical_origin.primary observed)
        ~origin:(Some (Physical_origin.of_observation observed))
    with
    | Ok () -> ()
    | Error error -> raise (Cleanup_failed error)
  ;;

  let control_exn t f =
    match control t f with
    | Ok result -> result
    | Error error -> raise (Cleanup_failed error)
  ;;

  let cleanup_exn t outcome ~origin =
    let primary =
      match outcome with
      | Ok () -> Scope_outcome.Returned (Ok ())
      | Error (exn, trace) -> Scope_outcome.Raised (exn, trace)
    in
    ignore (resume t primary ~origin : (unit, _) result)
  ;;

  let await t attempt =
    let observed = Resource_retirement_data.await_observed attempt in
    origin t observed.Originated_retirement.origin;
    observed
  ;;

  let project_retirement t observed =
    (* A Protocol negative may be handled by Reported custody continuation.
       Only an actual raw raise selects its escaping carrier at this seam. *)
    (match observed.Originated_retirement.outcome with
     | Error _ -> select t observed.origin
     | Ok _ -> t.selected <- None);
    Originated_retirement.project observed
  ;;

  let report_locked t ~outcome =
    let origin_groups =
      List.rev t.origins_rev
      |> List.concat_map ~f:(fun origin -> origin.Physical_origin.diagnostics)
    in
    let original_groups =
      List.rev t.roots_rev
      |> List.concat_map ~f:(fun recovery ->
        Recovery_data.original_diagnostics recovery |> Original_cleanup_report.diagnostics)
    in
    let disposition, primary, diagnostics =
      match outcome with
      | CO.Returned () ->
        ( Disposal_report_data.Disposed
        , None
        , origin_groups @ original_groups @ t.prior_diagnostics )
      | Raised (exn, trace) ->
        let primary =
          Option.value_or_thunk t.selected ~default:(fun () ->
            Diagnostic.create [ Raised (exn, trace) ])
        in
        t.selected <- Some primary;
        ( Retained
        , Some primary
        , origin_groups @ original_groups @ t.prior_diagnostics @ [ primary ] )
    in
    Disposal_report_data.create ?primary ~disposition diagnostics
  ;;

  let resume_strict t outcome =
    match outcome with
    | CO.Returned () -> ()
    | Raised (exn, trace) ->
      (* Only THIS delivered producer, never historical/root secondaries. *)
      Option.iter t.selected ~f:(fun diagnostic ->
        ignore (Diagnostic.take_legacy_diagnostic_exn ~primary:diagnostic [ diagnostic ]));
      Exn.raise_with_original_backtrace exn trace
  ;;
end

let%expect_test "uncached Strict acknowledges only its actual selected diagnostic" =
  let module I = Close_invocation in
  let module D = Agent_session.Cleanup_diagnostic in
  let module CO = Agent_session.Close_observation in
  let recorded = Stdlib.Printexc.backtrace_status () in
  Stdlib.Printexc.record_backtrace true;
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace recorded)
    ~f:(fun () ->
      let sentinel = Failure "Strict raw exit" in
      let trace =
        match raise sentinel with
        | _ -> assert false
        | exception actual ->
          assert (phys_equal actual sentinel);
          Stdlib.Printexc.get_raw_backtrace ()
      in
      let failure = D.Failure.Raised (sentinel, trace) in
      let original = D.create [ failure ] in
      let selected = D.create [ failure ] in
      let invocation = I.create ~prior:None in
      let origin =
        Some
          { Physical_origin.diagnostics = [ original; selected ]
          ; primary = Some selected
          }
      in
      I.origin invocation origin;
      I.select invocation origin;
      let resume () =
        match I.resume_strict invocation (CO.Raised (sentinel, trace)) with
        | () -> failwith "Strict suppressed its actual raw exit"
        | exception actual ->
          let actual_trace = Stdlib.Printexc.get_raw_backtrace () in
          assert (phys_equal actual sentinel);
          let prefix = Stdlib.Printexc.raw_backtrace_to_string trace in
          assert (not (String.is_empty prefix));
          assert (
            String.is_prefix
              (Stdlib.Printexc.raw_backtrace_to_string actual_trace)
              ~prefix)
      in
      resume ();
      assert (Option.is_none (D.take_legacy_diagnostic_exn [ selected ]));
      (* Delivery of K cannot suppress the SAME invocation's raw exit, nor
         redirect acknowledgement to its independent original G. *)
      resume ();
      let take expected =
        match D.take_legacy_diagnostic_exn [ expected ] with
        | Some (actual, Raised (exn, raw)) ->
          assert (D.equal actual expected);
          assert (phys_equal exn sentinel);
          assert (phys_equal raw trace)
        | None | Some (_, Protocol _) ->
          failwith "Strict consumed an independent diagnostic"
      in
      take original;
      let historical = D.create [ failure ] in
      let current = D.create [ failure ] in
      let success =
        I.create
          ~prior:(Some (Disposal_report_data.create ~disposition:Disposed [ historical ]))
      in
      I.origin
        success
        (Some { Physical_origin.diagnostics = [ current ]; primary = None });
      I.resume_strict success (CO.Returned ());
      take historical;
      take current);
  print_endline "negative selects one carrier; raw exit persists; success consumes none";
  [%expect {| negative selects one carrier; raw exit persists; success consumes none |}]
;;

module Retirement_join = struct
  type t =
    { attempt : Resource_retirement_data.t
    ; outcome : (unit, Agent_protocol.Error.t) result
    ; origin : Physical_origin.t option
    }
end

(* Keep the SAME actual elected producer alongside its original projection.
   Reported permanent disposal must not reconstruct this from a current slot. *)
let unload_and_wait_observed ?invocation t =
  Eio.Cancel.protect (fun () ->
    let open Result.Let_syntax in
    let rec claim () =
      let%bind disposition =
        with_owner_lock t ~protect:true (fun () ->
          Option.iter invocation ~f:(fun invocation ->
            Option.iter
              (Option.first_some t.pending_resource_retirement t.unloading)
              ~f:(Close_invocation.attempt invocation));
          match t.pending_resource_retirement with
          | Some attempt ->
            (match attempt.Resource_retirement_data.phase with
             | Finished outcome
               when Retirement_coverage.is_whole_owner attempt.coverage
                    && attempt.delivered
                    && not (Resource_retirement_data.is_success outcome) ->
               let%map claim = retry_signalled_resource_retirement t ~failed:attempt in
               (match claim with
                | Resource_retirement.Run work -> `Attempt work
                | Join current -> `Attempt current)
             | Queued | Running | Finished _ -> Ok (`Attempt attempt))
          | None -> Ok (claim_ordinary_unload_locked t))
      in
      match disposition with
      | `Join_administration finished ->
        Eio.Promise.await finished;
        claim ()
      | `Attempt attempt ->
        Option.iter invocation ~f:(fun invocation ->
          with_owner_lock t ~protect:true (fun () ->
            Close_invocation.attempt invocation attempt));
        let outcome, origin =
          match invocation with
          | None -> run_and_await_retirement attempt, None
          | Some invocation ->
            Resource_retirement.run attempt;
            let observed = Close_invocation.await invocation attempt in
            Close_invocation.project_retirement invocation observed, observed.origin
        in
        (* A signal may have arrived while ordinary cleanup completed. Public
           Stop observes that work before returning success. *)
        if
          Result.is_ok outcome
          && (resource_retirement_pending t
              || not (Retirement_coverage.is_whole_owner attempt.coverage))
        then claim ()
        else Ok { Retirement_join.attempt; outcome; origin }
    in
    claim ())
;;

let unload_and_wait t =
  Result.bind (unload_and_wait_observed t) ~f:(fun joined ->
    joined.Retirement_join.outcome)
;;

let check_source_finalizer_extent_locked t extent =
  match t.prepared_source with
  | Some stage
    when phys_equal stage.extent extent
         && Option.is_none (Eio.Promise.peek extent.Source_extent.finished) -> Ok ()
  | Some _ | None ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"source finalizer no longer owns this actual extent"
         ~retryable:false
         ())
;;

let unload_source_runtime_and_wait_observed t ~extent =
  Eio.Cancel.protect (fun () ->
    let selection =
      capture_source_operation_observed
        ~operation:Source_prefix_selection
        extent
        (fun () ->
           with_owner_lock t ~protect:true (fun () ->
             let open Result.Let_syntax in
             let%bind () = check_source_finalizer_extent_locked t extent in
             match claim_ordinary_unload_locked t with
             | `Attempt attempt ->
               Recovery_data.register_prefix
                 extent.Source_extent.recovery
                 ~prefix:attempt.recovery;
               Ok attempt
             | `Join_administration _ ->
               Error
                 (Agent_protocol.Error.invalid_request
                    "runtime source finalizer cannot join administration")))
    in
    match Agent_session.Close_observation.exit selection with
    | Returned (Ok attempt) -> run_and_await_retirement_observed attempt
    | Returned (Error error) ->
      Agent_session.Close_observation.create
        (Returned (Error error))
        ~diagnostics:(Agent_session.Close_observation.diagnostics selection)
        ?primary:(Agent_session.Close_observation.primary selection)
        ()
    | Raised (exn, trace) ->
      Agent_session.Close_observation.create
        (Raised (exn, trace))
        ~diagnostics:(Agent_session.Close_observation.diagnostics selection)
        ?primary:(Agent_session.Close_observation.primary selection)
        ())
;;

(* Administration historically retired only its newly initialized runtime while
   holding the Owner mutex. Its exact finalizer may join ordinary unloading, but
   never the pending attempt which is waiting for this same source extent. *)
let retire_administration_runtime_observed t ~extent =
  let open Result.Let_syntax in
  let cleanup = Cleanup_request.create () in
  let selection =
    capture_source_operation_observed ~operation:Source_prefix_selection extent (fun () ->
      with_owner_lock t ~protect:true (fun () ->
        let%bind () = check_source_finalizer_extent_locked t extent in
        match t.unloading with
        | Some attempt ->
          Recovery_data.register_prefix
            extent.Source_extent.recovery
            ~prefix:attempt.recovery;
          Ok (`Join attempt)
        | None ->
          Ok
            (`Retired
                (capture_source_operation_observed
                   ~operation:(Administration_unload_selection Source_prefix)
                   extent
                   (fun () -> unload_locked t cleanup)))))
  in
  match Agent_session.Close_observation.exit selection with
  | Returned (Ok (`Retired primary)) ->
    with_source_cleanup_request_observed
      t
      extent
      ~role:Administration_prefix
      cleanup
      primary
  | Returned (Ok (`Join attempt)) -> run_and_await_retirement_observed attempt
  | Returned (Error error) ->
    Agent_session.Close_observation.create
      (Returned (Error error))
      ~diagnostics:(Agent_session.Close_observation.diagnostics selection)
      ?primary:(Agent_session.Close_observation.primary selection)
      ()
  | Raised (exn, trace) ->
    Agent_session.Close_observation.create
      (Raised (exn, trace))
      ~diagnostics:(Agent_session.Close_observation.diagnostics selection)
      ?primary:(Agent_session.Close_observation.primary selection)
      ()
;;

let check_source_owner t source_owner =
  match
    ( t.runtime_source_owner
    , Agent_session.Session_actor.runtime_source_owner_identity t.actor )
  with
  | Some owner, Some actor
    when Agent_session.Runtime_source_owner.matches source_owner owner
         && Agent_session.Runtime_source_owner.matches source_owner actor -> Ok ()
  | (Some _ | None), (Some _ | None) ->
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:"source lifetime belongs to another Factory"
         ~retryable:false
         ())
;;

module Source_stage_admission = struct
  type t =
    | Runtime_source
    | Administration of
        { validate : unit -> (unit, Agent_protocol.Error.t) result
        ; commit : unit -> (unit, Agent_protocol.Error.t) result
        ; before_initialize : unit -> unit
        ; capture_cleanup : (Recovery_data.t -> unit) option
        }
end

let native_identity_locked t =
  Option.first_some
    (Option.bind t.binding ~f:Runtime_binding.native_identity)
    (Option.first_some
       (Option.map t.restored_policy ~f:(fun anchor ->
          Agent_session.Runtime_builder.Product.native_identity
            (Session_policy_resource.Lease.product anchor.loan)))
       (List.find_map t.native_loans ~f:(fun loan ->
          match loan.Native_loan.phase with
          | Available -> Some loan.identity
          | Retiring | Closing _ | Released -> None)))
;;

module Native_source = struct
  type owner = t

  type t =
    { owner : owner
    ; cell : Session_policy_resource.t
    ; observed : Session_policy_resource.Native_source.t
    }

  let source t = Session_policy_resource.Native_source.source t.observed

  (* Same-domain mutation is cooperative. This saved comparison does not lock,
     yield or invoke a port; it is the final current-use check before Cell pin.
     Factual close/retry deliberately does not require it. *)
  let check_owner t =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal t.owner.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request
             "installed native Source belongs to another Eio domain")
    in
    if t.owner.closed
    then Error (closed_error ())
    else if
      resource_retirement_blocks_admission t.owner
      || administration_blocks t.owner
      || prepared_source_blocks_runtime t.owner
      || Option.is_some t.owner.foreground_retirement
    then Error (background_busy ())
    else if
      Option.exists
        (Option.bind t.owner.policy_resource_cleanup ~f:Policy_resource_cleanup.cell)
        ~f:(phys_equal t.cell)
      && Option.exists (native_identity_locked t.owner) ~f:(fun current ->
        Agent_session.Runtime_builder.Native_identity.equal
          current
          (Session_policy_resource.Native_source.identity t.observed))
    then Ok ()
    else
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"installed native Source no longer belongs to its current Owner"
           ~retryable:false
           ())
  ;;

  let state t =
    let open Result.Let_syntax in
    let%bind () = check_owner t in
    let%bind state = Agent_session.Session_actor.state t.owner.actor in
    let%map () = check_owner t in
    state
  ;;

  let check t ~profile ~expected =
    let open Result.Let_syntax in
    let%bind () = check_owner t in
    Session_policy_resource.Native_source.check t.observed ~profile ~expected
  ;;
end

let observe_native_source t ~handle ~profile ~expected =
  let open Result.Let_syntax in
  let%bind () =
    if Int.equal t.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "native Source Owner belongs to another Eio domain")
  in
  (* Capture may wait for the existing mutex. Only fixed pure Cell operations
     occur under it; no Actor, Store or arbitrary host callback is invoked. *)
  with_owner_lock t ~protect:true (fun () ->
    let%bind native_identity =
      if t.closed
      then Error (closed_error ())
      else if
        resource_retirement_blocks_admission t
        || administration_blocks t
        || prepared_source_blocks_runtime t
      then Error (background_busy ())
      else (
        let identity = native_identity_locked t in
        match t.foreground_retirement with
        | None -> Ok identity
        | Some retirement
          when Option.exists t.joined_foreground_retirement ~f:(phys_equal retirement)
               && Agent_session.Session_actor.Runtime_retirement.is_successful retirement
               && Option.is_none t.binding
               && Option.is_none identity
               && (not (retired_cleanup_pending t))
               && not (failed_source_cleanup_pending t) -> Ok None
        | Some _ -> Error (background_busy ()))
    in
    let%bind cell =
      Result.of_option
        (Option.bind t.policy_resource_cleanup ~f:Policy_resource_cleanup.cell)
        ~error:
          (Agent_protocol.Error.invalid_request
             "native Source observation requires its actual Factory Cell")
    in
    let%map observed =
      Session_policy_resource.Native_source.capture
        cell
        ~expected_native:native_identity
        ~actor:t.actor
        ~handle
        ~profile
        ~expected
    in
    Option.map observed ~f:(fun observed -> Native_source.{ owner = t; cell; observed }))
;;

let select_resource_handoff_locked t =
  match t.background_leases with
  | [] -> Ok None
  | leases ->
    let selected =
      match t.binding with
      | Some binding -> Resource_handoff.of_binding binding
      | None ->
        List.find_map t.retired_resources ~f:(fun entry ->
          match entry.Retired_resource.phase with
          | Retained None when resources_borrowed t (Retired_resource.runtime entry) ->
            Resource_handoff.of_binding entry.binding
          | Retained (Some _) | Closing _ | Closed | Retained None -> None)
    in
    (match selected with
     | Some handoff
       when List.for_all leases ~f:(fun lease ->
              match lease.purpose with
              | Exclusive -> false
              | Shared_native identity ->
                Option.exists
                  (Runtime_binding.native_identity handoff.binding)
                  ~f:(Agent_session.Runtime_builder.Native_identity.equal identity)
              | Shared_resource product ->
                Agent_session.Runtime_builder.Product_identity.equal
                  product
                  handoff.product) -> Ok (Some handoff)
     | None
       when List.for_all leases ~f:(fun lease ->
              match lease.purpose, native_identity_locked t with
              | Shared_native identity, Some selected ->
                Agent_session.Runtime_builder.Native_identity.equal identity selected
              | (Exclusive | Shared_resource _ | Shared_native _), (Some _ | None) ->
                false) -> Ok None
     | Some _ | None -> Error (background_busy ()))
;;

(* A terminal Turn ACK can dirty retained facts after the last provider release.
   Snapshot whether eligible facts need settlement before admission or idle
   polling. Actual settlement runs outside the Owner mutex. Other blocked
   ownership/lifecycle states retain their original path; this Boolean grants
   neither a runtime pin nor callback authority. *)
let inference_usage_settlement_pending t =
  with_owner_lock t ~protect:true (fun () ->
    if
      prepared_source_blocks_runtime t
      || t.closed
      || resource_retirement_blocks_admission t
      || administration_blocks t
      || constructor_model_pending t
      || policy_claim_pending t
      || policy_cleanup_pending t
      || List.exists t.retired_resources ~f:(fun entry ->
        not (resources_borrowed t (Retired_resource.runtime entry)))
    then false
    else Agent_session.Session_actor.inference_usage_cleanup_pending t.actor)
;;

let settle_admission_inference_usage t =
  if inference_usage_settlement_pending t
  then Agent_session.Session_actor.drain_inference_usage t.actor
  else Ok ()
;;

(* Idle polling can overlap an initial confirmation's live driver. Deferred
   facts stay owned while the original idle path progresses its existing work. *)
let progress_idle_inference_usage t =
  let open Result.Let_syntax in
  if inference_usage_settlement_pending t
  then (
    let%map outcome = Agent_session.Session_actor.poll_inference_usage t.actor in
    match outcome with
    | Settled | Deferred -> ())
  else Ok ()
;;

module Source_stage_outcome = struct
  module Selected_lease = struct
    type 'a t =
      { stage : Lease_outcome.Primary_stage.t option
      ; primary : ('a, Agent_protocol.Error.t) result Agent_session.Close_observation.t
      }
  end

  module Cleanup = struct
    type t =
      | Not_entered
      | Reported of
          (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t
      | Finalizer_escaped of
          (unit, Agent_protocol.Error.t) result Agent_session.Close_observation.t

    let observation = function
      | Not_entered -> Observed_result.return ()
      | Reported observation | Finalizer_escaped observation -> observation
    ;;
  end

  type 'a t =
    { reported : 'a Scope_outcome.t Agent_session.Close_observation.t
    ; observation : ('a, Agent_protocol.Error.t) result Agent_session.Close_observation.t
    ; cleanup : Cleanup.t
    ; selected_lease : 'a Selected_lease.t option
    }

  let report t = Agent_session.Close_observation.resume t.reported
  let observation t = t.observation
  let cleanup t = t.cleanup
  let selected_lease t = t.selected_lease
end

let with_source_stage_callback_internal
      ?preparation_recovery
      ?(admission = Source_stage_admission.Runtime_source)
      t
      callback
  =
  let module CO = Agent_session.Close_observation in
  let extent =
    Source_extent.create ~affinity:t.affinity ~origin:Source_extent.Origin.Stage
  in
  let retirement_cleanup = Cleanup_request.create () in
  let reserved = ref None in
  let selected_lease = ref None in
  let retire () = Option.iter !reserved ~f:close_source_stage in
  let primary_observation =
    Observed_result.capture (fun () ->
      Option.iter preparation_recovery ~f:(fun recovery ->
        Recovery_data.register_preparation_source recovery ~source:extent.recovery);
      let elect () =
        let completed_retirement =
          match admission with
          | Source_stage_admission.Runtime_source -> completed_unloaded_retirement t
          | Administration _ -> None
        in
        let report =
          with_source_lease_callback_internal
            ~after_acquired:(fun (stage : source_stage) ->
              match admission with
              | Runtime_source -> ()
              | Administration { capture_cleanup; _ } ->
                Option.iter capture_cleanup ~f:(fun capture ->
                  capture stage.extent.recovery))
            t
            ~recovery:extent.recovery
            ~retire
            ~admission_protection:
              (match admission with
               | Runtime_source -> Protected
               | Administration _ -> Cancellable)
            ~admit:(fun () ->
              let open Result.Let_syntax in
              forget_completed_retirement_locked t completed_retirement;
              let%bind resource_handoff =
                match admission with
                | Source_stage_admission.Runtime_source ->
                  if resource_retirement_pending t || failed_source_cleanup_pending t
                  then Error (background_busy ())
                  else if (not t.closed) && retired_cleanup_pending t
                  then Error (retired_cleanup_error ())
                  else (
                    match
                      t.closed, t.unloading, t.foreground_retirement, t.prepared_source
                    with
                    | true, _, _, _ -> Error (closed_error ())
                    | false, Some _, _, _
                    | false, None, Some _, _
                    | false, None, None, Some _ -> Error (background_busy ())
                    | false, None, None, None -> select_resource_handoff_locked t)
                | Administration { validate; _ } ->
                  let%bind () =
                    if t.closed
                    then Error (closed_error ())
                    else if
                      resource_retirement_blocks_admission t
                      || failed_source_cleanup_pending t
                      || not (List.is_empty t.background_leases)
                    then Error (background_busy ())
                    else validate ()
                  in
                  (* No unload action has started yet. A known factual refusal
                     belongs to admission, not to an attempted source cleanup
                     duty that would permanently fence this unacquired stage. *)
                  let%bind () =
                    if factual_cleanup_pending t
                    then Error (retired_cleanup_error ())
                    else Ok ()
                  in
                  (* Preserve the actual pre-unload basis even if admission
                     cleanup fails before a Source stage can be published. *)
                  Source_resource_association.capture
                    extent.association
                    ~binding:t.binding
                    ~handoff:None
                    ~restored:t.restored_policy
                    ~native:t.native_anchor;
                  Option.iter t.restored_policy ~f:(fun anchor ->
                    Recovery_data.register_restored_release
                      extent.recovery
                      ~position:Administration_precommit
                      anchor;
                    Recovery_data.register_source_target
                      extent.recovery
                      (Restored_release anchor);
                    Restored_policy_anchor.revoke anchor);
                  let%map () =
                    record_source_cleanup
                      ~operation:(Administration_unload_selection Source_admission)
                      extent
                      (fun () -> unload_locked t retirement_cleanup)
                  in
                  None
              in
              let stage =
                { domain = (Stdlib.Domain.self () :> int)
                ; runtime = t.binding
                ; resource_handoff
                ; foreground_retirement = t.foreground_retirement
                ; retirement =
                    (match admission with
                     | Source_stage_admission.Runtime_source -> Cancellable
                     | Administration _ -> Source_stage_retirement.administration ())
                ; extent
                ; cancel_lease = None
                ; restored_anchor = t.restored_policy
                ; policy_construction = None
                ; native_construction = None
                ; native_anchor = t.native_anchor
                ; load_preparation = None
                ; binding = None
                ; continuation = None
                ; closed = false
                ; retire_installed = None
                ; handoff_runtime = None
                }
              in
              if Option.is_none extent.association.basis
              then
                Source_resource_association.capture
                  extent.association
                  ~binding:stage.runtime
                  ~handoff:stage.resource_handoff
                  ~restored:stage.restored_anchor
                  ~native:stage.native_anchor;
              (* Capture before any cancellable writer/allocator operation. Even if
                 admission raises before lease registration, this exact stage is
                 closed and joined by the outer cleanup. *)
              reserved := Some stage;
              let%bind () = attach_pending_source_extent t stage.extent in
              t.prepared_source <- Some stage;
              Ok
                ( stage
                , Option.first_some
                    (Option.map resource_handoff ~f:Resource_handoff.runtime)
                    (Option.first_some
                       (loaded_runtime t)
                       (Option.map t.restored_policy ~f:Restored_policy_anchor.runtime))
                ))
            (Lease_invocation.Observed
               (fun ~recovery:_ stage ->
                 let open Observed_result in
                 let%bind () =
                   Observed_result.raw (fun () ->
                     match admission with
                     | Runtime_source -> Ok ()
                     | Administration _ ->
                       (* Capture already ran at actual Borrow publication, before
                      admission cleanup and body gating. Resume only this stage. *)
                       with_owner_lock t ~protect:false (fun () ->
                         if t.closed
                         then Error (closed_error ())
                         else if
                           stage.closed
                           || resource_retirement_blocks_admission t
                           || (not
                                 (Option.exists t.prepared_source ~f:(phys_equal stage)))
                           || (not (Option.equal phys_equal t.binding stage.runtime))
                           || not
                                (Option.equal
                                   phys_equal
                                   t.foreground_retirement
                                   stage.foreground_retirement)
                         then Error (background_busy ())
                         else Ok ()))
                 in
                 let%bind () =
                   with_source_cleanup_request_observed
                     t
                     extent
                     ~role:Source_entry
                     retirement_cleanup
                     (Observed_result.return ())
                 in
                 let%bind () =
                   match admission with
                   | Runtime_source -> Observed_result.return ()
                   | Administration { commit; before_initialize; _ } ->
                     let%bind () =
                       record_source_observation_observed
                         extent
                         (close_restored_policy_observed
                            ~recovery:(extent.recovery, Administration_precommit)
                            t)
                     in
                     let%bind () =
                       record_source_observation_observed
                         extent
                         (close_native_anchor_observed
                            ~recovery:(extent.recovery, Administration_precommit)
                            t)
                     in
                     let%bind () =
                       record_source_observation_observed
                         extent
                         (retry_native_releases_observed
                            ~recovery:(extent.recovery, Administration_precommit)
                            t)
                     in
                     stage.restored_anchor <- None;
                     stage.native_anchor <- None;
                     Observed_result.raw (fun () ->
                       let open Result.Let_syntax in
                       with_owner_lock t ~protect:false (fun () ->
                         let%bind () =
                           if t.closed
                           then Error (closed_error ())
                           else if
                             stage.closed
                             || resource_retirement_blocks_admission t
                             || (not
                                   (Option.exists t.prepared_source ~f:(phys_equal stage)))
                             || (not (Option.equal phys_equal t.binding stage.runtime))
                             || not
                                  (Option.equal
                                     phys_equal
                                     t.foreground_retirement
                                     stage.foreground_retirement)
                           then Error (background_busy ())
                           else Ok ()
                         in
                         let%map () = commit () in
                         before_initialize ()))
                 in
                 Lease_invocation.invoke callback ~recovery:extent.recovery stage))
        in
        let recorded =
          Observed_result.raw (fun () ->
            Source_extent.record_projection
              extent
              (Recovery_data.original_projection report.Lease_scope_outcome.recovery);
            Ok ())
        in
        let primary = Scope_outcome.primary report.report in
        let source_primary =
          Observed_result.of_primary
            primary
            ~diagnostics:(CO.diagnostics report.observation)
            ?primary:
              (match primary with
               | Returned (Ok _) -> None
               | Returned (Error _) | Raised _ -> CO.primary report.observation)
            ()
        in
        (* Keep the exact inner selection before the outer recording/finalizer
           projection. Native may classify only this actual stage and carrier. *)
        selected_lease
        := Some
             { Source_stage_outcome.Selected_lease.stage =
                 Lease_outcome.primary_stage report
             ; primary = source_primary
             };
        let selected = Observed_result.bind recorded ~f:(fun () -> source_primary) in
        CO.create
          (CO.exit selected)
          ~diagnostics:(CO.diagnostics report.observation @ CO.diagnostics selected)
          ?primary:(CO.primary selected)
          ()
      in
      match admission with
      | Source_stage_admission.Administration _ -> elect ()
      | Runtime_source ->
        Observed_result.bind
          (Observed_result.raw (fun () -> settle_admission_inference_usage t))
          ~f:(fun () -> elect ()))
  in
  let primary = Physical_origin.primary primary_observation in
  let cleanup () =
    Eio.Cancel.protect (fun () ->
      let clear_owned_stage_observed () =
        let clear =
          Physical_origin.capture (fun () ->
            record_source_unit_observed ~operation:Source_clear extent (fun () ->
              with_owner_lock t ~protect:true (fun () ->
                match t.prepared_source, !reserved with
                | Some current, Some owned when phys_equal current owned ->
                  t.activation_handoff
                  <- t.activation_handoff || stage_has_activation_handoff owned;
                  retain_source_cleanup_locked t extent.recovery ~acquired:true;
                  t.prepared_source <- None
                | Some _, (Some _ | None) | None, (Some _ | None) ->
                  retain_source_cleanup_locked t extent.recovery ~acquired:false)))
        in
        let exclusion =
          match !reserved with
          | None ->
            Agent_session.Close_observation.create (Returned ()) ~diagnostics:[] ()
          | Some stage ->
            Physical_origin.capture (fun () ->
              record_source_unit_observed
                ~operation:Source_exclusion_complete
                extent
                (fun () -> Source_stage_retirement.complete stage.retirement))
        in
        Cleanup_projection.protect_unit_observed clear exclusion
      in
      let body_origin = ref None in
      let resume_observed observed =
        (match Agent_session.Close_observation.exit observed with
         | Returned (Ok _) -> ()
         | Returned (Error _) | Raised _ ->
           body_origin := Some (Physical_origin.of_observation observed));
        Agent_session.Close_observation.resume observed
      in
      let resume_unit observed =
        (match Agent_session.Close_observation.exit observed with
         | Returned () -> ()
         | Raised _ -> body_origin := Some (Physical_origin.of_observation observed));
        Agent_session.Close_observation.resume observed
      in
      let body =
        Scope_outcome.capture (fun () ->
          (* Capture originally owed physical legs BEFORE retire, queue cleanup,
           or the installed-runtime prefix can fail. Available healthy custody
           remains transferred to the Owner; it is not a required source leg. *)
          Option.iter !reserved ~f:(fun _owned ->
            resume_unit
              (record_source_unit_observed
                 ~operation:Source_physical_selection
                 extent
                 (fun () ->
                    with_owner_lock t ~protect:true (fun () ->
                      List.iter t.native_loans ~f:(fun loan ->
                        match loan.Native_loan.phase with
                        | Retiring | Closing _ ->
                          Recovery_data.register_source_target
                            extent.recovery
                            (Native_release loan)
                        | Available | Released -> ());
                      Option.iter t.restored_policy ~f:(fun anchor ->
                        match anchor.Restored_policy_anchor.phase with
                        | Preparing | Retiring ->
                          Recovery_data.register_source_target
                            extent.recovery
                            (Restored_release anchor)
                        | Available _ -> ())))));
          (* A failing close signal does not erase independent acquired cleanup. *)
          let retire_step =
            Option.map !reserved ~f:(fun _ ->
              Recovery_data.register_original_step extent.recovery Source_retire)
          in
          let retired =
            Scope_outcome.capture (fun () ->
              retire ();
              Ok ())
          in
          Option.iter retire_step ~f:(fun step ->
            Original_cleanup_step.record_projection step retired;
            Source_extent.record_projection
              extent
              (Original_cleanup_step.projection step ~acquired:true));
          let retirement_batch = run_cleanup_request_reported t retirement_cleanup in
          Source_extent.record_batch extent ~role:Source_exit retirement_batch;
          Source_extent.record_projection
            extent
            (Cleanup_batch.projection retirement_batch);
          (* Normal caught close errors historically allowed the remaining source
           cleanup to run. An unforeseen selection/commit raise still escapes,
           after retaining the complete actual batch and under the clear bracket. *)
          (match retirement_batch.original with
           | Returned _ -> ()
           | Raised _ ->
             body_origin
             := Cleanup_projection.origin
                  (Cleanup_projection.of_diagnostic
                     (Cleanup_batch.cleanup retirement_batch)
                     retirement_batch.diagnostic));
          ignore (Cleanup_batch.resume retirement_batch : cleanup_outcome);
          let open Result.Let_syntax in
          let%bind retire_runtime =
            resume_observed
              (capture_source_operation_observed
                 ~operation:Source_installed_selection
                 extent
                 (fun () ->
                    with_owner_lock t ~protect:true (fun () ->
                      match !reserved with
                      | None -> Ok false
                      | Some owned ->
                        (match owned.retire_installed with
                         | None -> Ok false
                         | Some installed ->
                           (match t.prepared_source, loaded_runtime t with
                            | Some current, Some runtime
                              when phys_equal current owned
                                   && phys_equal installed runtime -> Ok true
                            | Some current, None when phys_equal current owned -> Ok false
                            | Some _, (Some _ | None) | None, (Some _ | None) ->
                              Error
                                (Agent_protocol.Error.create
                                   Conflict
                                   ~message:
                                     "installed runtime retirement owner was displaced"
                                   ~retryable:false
                                   ()))))))
          in
          (* The source lease has detached before this outer finalizer. Its one
           ordinary prefix cannot include that source lease or wait on pending
           work which is itself joining this extent. *)
          let%bind () =
            if not retire_runtime
            then Ok ()
            else
              resume_observed
                (match admission with
                 | Source_stage_admission.Runtime_source ->
                   unload_source_runtime_and_wait_observed t ~extent
                 | Administration _ -> retire_administration_runtime_observed t ~extent)
          in
          let native_failed = ref false in
          List.iter (Recovery_data.ordered_source_tail extent.recovery) ~f:(fun duty ->
            let run =
              match duty.Source_cleanup.target with
              | Native_release _ -> not !native_failed
              | Policy_end _ | Native_end _ | Restored_release _ | Delegated_end _ -> true
            in
            if run
            then (
              let observed =
                match duty.target with
                | Native_release loan ->
                  let observed =
                    close_native_loan_observed
                      ~recovery:(extent.recovery, Source_tail duty)
                      t
                      loan
                  in
                  ignore
                    (Source_cleanup.capture duty (fun () ->
                       Agent_session.Close_observation.resume observed)
                     : unit Scope_outcome.primary);
                  observed
                | Restored_release anchor ->
                  let observed =
                    close_restored_anchor_observed
                      ~recovery:(extent.recovery, Source_tail duty)
                      t
                      anchor
                  in
                  ignore
                    (Source_cleanup.capture duty (fun () ->
                       Agent_session.Close_observation.resume observed)
                     : unit Scope_outcome.primary);
                  observed
                | Policy_end _ | Native_end _ | Delegated_end _ ->
                  let original =
                    Source_cleanup.capture duty (fun () ->
                      match duty.target with
                      | Policy_end restoration ->
                        Agent_session.Session_actor.end_policy_restoration
                          t.actor
                          ~restoration
                      | Native_end setup ->
                        Agent_session.Session_actor.end_native_setup t.actor ~setup
                      | Delegated_end preparation ->
                        Agent_session.Session_actor.end_delegated_load_preparation
                          t.actor
                          ~preparation
                      | Native_release _ | Restored_release _ ->
                        invalid_arg "physical source target reached End helper")
                  in
                  Agent_session.Close_observation.create
                    (match original with
                     | Returned result -> Returned result
                     | Raised (exn, trace) -> Raised (exn, trace))
                    ~diagnostics:(Option.to_list duty.diagnostic)
                    ?primary:duty.diagnostic
                    ()
              in
              let original = Physical_origin.primary observed in
              (match duty.target, original with
               | Native_release _, (Returned (Error _) | Raised _) ->
                 native_failed := true
               | Native_release _, Returned (Ok ())
               | ( (Policy_end _ | Native_end _ | Restored_release _ | Delegated_end _)
                 , (Returned _ | Raised _) ) -> ());
              Source_extent.record_projection
                extent
                (Cleanup_projection.of_primary
                   ~origin:(Physical_origin.of_observation observed)
                   ~acquired:true
                   original)));
          Ok ())
      in
      let body_origin =
        match !body_origin with
        | Some _ as origin -> origin
        | None ->
          Option.map (Original_diagnostic.primary body) ~f:(fun primary ->
            { Physical_origin.diagnostics = [ primary ]; primary = Some primary })
      in
      let body =
        Agent_session.Close_observation.create
          (match body with
           | Returned result -> Returned result
           | Raised (exn, trace) -> Raised (exn, trace))
          ~diagnostics:
            (Option.value_map body_origin ~default:[] ~f:(fun origin ->
               origin.diagnostics))
          ?primary:(Option.bind body_origin ~f:(fun origin -> origin.primary))
          ()
      in
      let clear = clear_owned_stage_observed () in
      Cleanup_projection.protect_observed body clear)
  in
  (* Contain the original whole-finalizer boundary; preserve known cleanup
     groups even when later original publication/fence completion itself raises. *)
  let cleaned = Physical_origin.capture_result cleanup in
  let projection =
    Cleanup_projection.of_primary
      ~origin:(Physical_origin.of_observation cleaned)
      ~acquired:(Option.is_some !reserved)
      (Physical_origin.primary cleaned)
  in
  let finalized =
    Observed_result.raw (fun () ->
      let cleanup =
        Source_extent.finish
          extent
          ~completion:
            (Stage (if Option.is_some !reserved then Reserved else Not_reserved))
          ~projection
      in
      finish_source_cleanup_fence t extent.recovery;
      Ok cleanup)
  in
  let cleanup_diagnostics =
    Option.value_map
      (Cleanup_projection.origin extent.projection)
      ~default:[]
      ~f:(fun origin -> origin.Physical_origin.diagnostics)
    @ CO.diagnostics cleaned
  in
  let reported, observation, cleanup =
    match CO.exit finalized with
    | Returned (Ok cleanup) ->
      let report = { Scope_outcome.primary; cleanup } in
      let projection = Recovery_data.original_projection extent.recovery in
      let observation = Observed_result.with_cleanup primary_observation projection in
      let cleanup = Observed_result.with_cleanup (Observed_result.return ()) projection in
      ( CO.create (Returned report) ~diagnostics:(CO.diagnostics observation) ()
      , observation
      , Source_stage_outcome.Cleanup.Reported cleanup )
    | Returned (Error _) -> invalid_arg "source publication returned a protocol error"
    | Raised (exn, trace) ->
      let cleanup_diagnostics = cleanup_diagnostics @ CO.diagnostics finalized in
      let cleanup =
        CO.create
          (Raised (exn, trace))
          ~diagnostics:cleanup_diagnostics
          ?primary:(CO.primary finalized)
          ()
      in
      let diagnostics = CO.diagnostics primary_observation @ cleanup_diagnostics in
      let primary = CO.primary finalized in
      ( CO.create (Raised (exn, trace)) ~diagnostics ?primary ()
      , CO.create (Raised (exn, trace)) ~diagnostics ?primary ()
      , Source_stage_outcome.Cleanup.Finalizer_escaped cleanup )
  in
  { Source_stage_outcome.reported
  ; observation
  ; cleanup
  ; selected_lease = !selected_lease
  }
;;

let with_source_stage_observed_internal ?preparation_recovery ?admission t f =
  with_source_stage_callback_internal
    ?preparation_recovery
    ?admission
    t
    (Lease_invocation.Observed (fun ~recovery:_ stage -> f stage))
;;

let with_source_stage_reported ?preparation_recovery ?admission t f =
  with_source_stage_callback_internal
    ?preparation_recovery
    ?admission
    t
    (Lease_invocation.Raw (fun ~recovery:_ stage -> f stage))
  |> Source_stage_outcome.report
;;

let source_stage_basis t (stage : source_stage) () =
  if
    t.closed
    || resource_retirement_blocks_admission t
    || (not (Option.equal phys_equal t.foreground_retirement stage.foreground_retirement))
    || (not (Option.exists t.prepared_source ~f:(phys_equal stage)))
    || (not (Option.equal phys_equal t.binding stage.runtime))
    || (not (Option.equal phys_equal t.restored_policy stage.restored_anchor))
    || not (Option.equal phys_equal t.native_anchor stage.native_anchor)
  then
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"prepared runtime owner was displaced"
         ~retryable:false
         ())
  else Ok ()
;;

let consume_source_selection t (stage : source_stage) attempted ~label =
  let open Result.Let_syntax in
  let%bind () =
    if Int.equal stage.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request (label ^ " belongs to another Eio domain"))
  in
  let current =
    with_owner_lock t ~protect:true (fun () ->
      if
        (not !attempted)
        && (not stage.closed)
        && (not t.closed)
        && (not (resource_retirement_blocks_admission t))
        && Option.equal phys_equal t.foreground_retirement stage.foreground_retirement
        && Option.exists t.prepared_source ~f:(phys_equal stage)
        && Option.equal phys_equal t.binding stage.runtime
        && Option.is_none stage.binding
        && Option.is_none stage.continuation
      then (
        (* Consume selection before the Actor call can yield or fail. *)
        attempted := true;
        true)
      else false)
  in
  if current
  then Ok ()
  else
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:(label ^ " was closed, displaced or already selected")
         ~retryable:false
         ())
;;

let prepare_source t (stage : source_stage) ~preparation ~source =
  let open Result.Let_syntax in
  let%bind binding =
    Eio.Cancel.protect (fun () ->
      let result =
        Agent_session.Session_actor.bind_run_source t.actor ~preparation ~source
      in
      Result.iter result ~f:(fun binding ->
        stage.binding <- Some (Initial binding);
        if stage.closed then Agent_session.Session_actor.Source_binding.close binding);
      result)
  in
  let prepared : Prepared.t =
    { stage; binding; preparation; basis = source_stage_basis t stage }
  in
  let%bind () = Prepared.check_basis prepared in
  Eio.Fiber.check ();
  Ok prepared
;;

let policy_restoration_source
      t
      (stage : source_stage)
      ~consumer
      ~request
      ~governance
      ~authorize
  =
  let module A = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let attempted = ref false in
  let check_domain () =
    if Int.equal stage.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "policy restoration source belongs to another Eio domain")
  in
  let check () =
    let%bind () = check_domain () in
    let%bind () = source_stage_basis t stage () in
    if stage.closed then Error (background_busy ()) else Ok ()
  in
  let prepare ~source_owner ~source ~basis =
    let%bind () = check_domain () in
    let%bind () = check_source_owner t source_owner in
    let%bind () = check () in
    let%bind () =
      with_owner_lock t ~protect:true (fun () ->
        if (not !attempted) && Option.is_none stage.policy_construction
        then (
          attempted := true;
          Ok ())
        else Error (background_busy ()))
    in
    let%bind restoration =
      Eio.Cancel.protect (fun () ->
        let result =
          A.begin_policy_restoration
            t.actor
            ~source_owner
            ~source
            ~basis
            ~request
            ~governance
            ~authorize
        in
        Result.iter result ~f:(fun restoration ->
          stage.policy_construction <- Some restoration;
          Recovery_data.register_source_target
            stage.extent.recovery
            (Policy_end restoration);
          if stage.closed then A.Policy_restoration.close restoration);
        result)
    in
    let%bind () = authorize () in
    let%bind () = check () in
    let%map () = A.Policy_restoration.check restoration in
    restoration
  in
  let publish ~restoration ~loan ~access =
    let transferred = ref false in
    let anchor : Restored_policy_anchor.t = { loan; access; phase = Preparing } in
    let primary =
      Scope_outcome.capture (fun () ->
        Eio.Cancel.protect (fun () ->
          (* Record actual custody under the same lock before the first promotion
           wait. Even cancellation or an unexpected exception is reported with
           this ownership, so Factory records it BEFORE resuming the primary. *)
          let%bind () = check_domain () in
          let%bind () =
            if
              Agent_session.Runtime_builder.Product_identity.equal
                (Agent_session.Runtime_builder.Product.identity
                   (Session_policy_resource.Lease.product loan))
                (Agent_session.Runtime_builder.Restored_policy_access.product_identity
                   access)
            then Ok ()
            else Error (background_busy ())
          in
          let%bind () =
            with_owner_lock t ~protect:true (fun () ->
              let%bind () =
                match t.native_anchor with
                | None -> Ok ()
                | Some native ->
                  if
                    Agent_session.Runtime_builder.Native_identity.equal
                      native.identity
                      (Agent_session.Runtime_builder.Product.native_identity
                         (Session_policy_resource.Lease.product loan))
                  then Ok ()
                  else Error (background_busy ())
              in
              if
                Option.exists t.prepared_source ~f:(phys_equal stage)
                && Option.is_none t.restored_policy
                && Option.exists stage.policy_construction ~f:(phys_equal restoration)
              then (
                t.restored_policy <- Some anchor;
                stage.restored_anchor <- Some anchor;
                transferred := true;
                let%map () = release_native_after_adoption t in
                stage.native_anchor <- t.native_anchor)
              else Error (background_busy ()))
          in
          let%bind () = check () in
          let%bind published =
            A.publish_policy_restoration t.actor ~restoration ~access
          in
          with_owner_lock t ~protect:true (fun () ->
            if
              stage.closed
              || t.closed
              || resource_retirement_blocks_admission t
              || not (Option.exists t.restored_policy ~f:(phys_equal anchor))
            then (
              A.Policy_restoration.Published.close published;
              Error (background_busy ()))
            else (
              anchor.phase <- Available published;
              Ok ()))))
    in
    (match primary with
     | Returned (Ok ()) -> ()
     | Returned (Error _) | Raised _ ->
       if !transferred then Restored_policy_anchor.revoke anchor);
    { Policy_restoration_source.Publication.custody =
        (if !transferred then Owner_retains else Caller_retains)
    ; primary
    }
  in
  { Policy_restoration_source.consumer; check; prepare; publish }
;;

let native_source_stage
      t
      (stage : source_stage)
      admission
      (access : Native_resource_access.t)
  =
  let module A = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let%bind consumer = Source_consumer.create stage in
  let attempted = ref false in
  let check () =
    if not (Int.equal stage.domain (Stdlib.Domain.self () :> int))
    then
      Error
        (Agent_protocol.Error.invalid_request
           "native source stage belongs to another Eio domain")
    else if stage.closed
    then Error (background_busy ())
    else source_stage_basis t stage ()
  in
  let prepare ~source_owner ~source ~expected =
    let%bind () = check () in
    let%bind () = check_source_owner t source_owner in
    let%bind () = Native_resource_admission.check admission ~expected in
    let%bind () =
      with_owner_lock t ~protect:true (fun () ->
        if
          (not !attempted)
          && Option.is_none stage.native_construction
          && Option.is_none stage.policy_construction
          && Option.is_none stage.continuation
        then (
          attempted := true;
          Ok ())
        else Error (background_busy ()))
    in
    let%bind setup =
      Eio.Cancel.protect (fun () ->
        let result =
          A.begin_native_setup
            t.actor
            ~source_owner
            ~reference:(Native_resource_admission.reference admission)
            ~source
            ~expected
            ~authorize:(fun () -> Native_resource_admission.check admission ~expected)
        in
        Result.iter result ~f:(fun setup ->
          stage.native_construction <- Some setup;
          Recovery_data.register_source_target stage.extent.recovery (Native_end setup);
          if stage.closed then A.Native_setup.close setup);
        result)
    in
    let%bind () = Native_resource_admission.check admission ~expected in
    let%bind () = check () in
    let%map () = A.Native_setup.check setup in
    setup
  in
  let publish ~setup ~loan =
    let transferred = ref false in
    let anchor = Native_loan.create loan in
    let primary =
      Scope_outcome.capture (fun () ->
        Eio.Cancel.protect (fun () ->
          (* The Factory still owns this first loan until the transfer below.
             Its fixed port binds actual Cell/Actor/Handle/profile/source affinity
             and this consumer's private grant outside the Owner mutex. *)
          let%bind () = check () in
          let%bind expected = A.state t.actor in
          let%bind () = Native_resource_admission.check admission ~expected in
          let%bind () = access.check loan ~admission ~expected in
          let%bind () = check () in
          let%bind () =
            with_owner_lock t ~protect:true (fun () ->
              if
                (not t.closed)
                && (not (resource_retirement_blocks_admission t))
                && Option.exists t.prepared_source ~f:(phys_equal stage)
                && Option.is_none t.native_anchor
                && Option.exists stage.native_construction ~f:(phys_equal setup)
              then (
                t.native_anchor <- Some anchor;
                t.native_loans <- anchor :: t.native_loans;
                stage.native_anchor <- Some anchor;
                transferred := true;
                Ok ())
              else Error (background_busy ()))
          in
          let%bind () = A.Native_setup.check setup in
          check ()))
    in
    (match primary with
     | Scope_outcome.Returned (Ok ()) -> ()
     | Returned (Error _) | Raised _ -> if !transferred then Native_loan.retire anchor);
    { Native_source_stage.Publication.custody =
        (if !transferred then Owner_retains else Caller_retains)
    ; primary
    }
  in
  Ok { Native_source_stage.consumer; check; admission; prepare; publish }
;;

let with_prepared_source_reported t ~preparation ~source f =
  with_source_stage_reported t (fun stage ->
    let open Result.Let_syntax in
    let%bind prepared = prepare_source t stage ~preparation ~source in
    f prepared)
;;

let with_prepared_source t ~preparation ~source f =
  with_prepared_source_reported t ~preparation ~source f |> Scope_outcome.resume
;;

module Deferred_source = struct
  type owner = t

  type t =
    { owner : owner
    ; stage : source_stage
    ; consumer : Source_consumer.t
    ; deferred : Agent_session.Run_preparation.Deferred.t
    ; retry : Agent_session.Session_actor.Recurrence_retry.t option
    ; source_attempted : bool ref
    }

  let consumer t = t.consumer
  let preparation t = t.deferred
  let retry t = t.retry

  let prepare t ~source =
    let open Result.Let_syntax in
    let%bind () =
      consume_source_selection t.owner t.stage t.source_attempted ~label:"deferred source"
    in
    prepare_source
      t.owner
      t.stage
      ~preparation:(Agent_session.Run_preparation.Deferred.preparation t.deferred)
      ~source
  ;;

  module Decision = struct
    type 'a t =
      | Retained of Agent_protocol.Run_receipt.t
      | Fresh of 'a
  end

  module Report = struct
    type 'a t =
      { deferred : Agent_session.Run_preparation.Deferred.t option
      ; retry : Agent_session.Session_actor.Recurrence_retry.t option
      ; outcome : 'a Decision.t Scope_outcome.t
      }

    let deferred t = t.deferred
    let retry t = t.retry
    let outcome t = t.outcome
  end
end

module Deferred_operator_request = struct
  type t =
    | Initial of Agent_session.Run_admission_request.Headless.t
    | Retry of
        { observation : Agent_session.Session_actor.No_effects_observation.t
        ; intent : Agent_session.Recurrence_execution_intent.t
        ; previous : Agent_session.Recurrence_execution_attempt.t
        ; request : Agent_session.Run_admission_request.Headless.t
        }
    | Resume of Agent_session.Session_actor.Recurrence_retry.t
end

module Deferred_issuance = struct
  type t =
    | Operator of
        { actor : Operator_authorization.t
        ; authorize :
            Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result
        ; request : Deferred_operator_request.t
        }
    | Native_start of Agent_session.Session_actor.Goal_native_start.Selection.t

  type decision =
    | Retained of Agent_protocol.Run_receipt.t
    | Issued of
        { deferred : Agent_session.Run_preparation.Deferred.t
        ; retry : Agent_session.Session_actor.Recurrence_retry.t option
        }

  let issue_operator (request : Deferred_operator_request.t) target ~actor ~authorize =
    let module Actor = Agent_session.Session_actor in
    match request with
    | Initial request ->
      Actor.begin_effect_deferred_headless_preparation target ~actor ~authorize ~request
      |> Result.map ~f:(function
        | Agent_session.Run_preparation.Deferred.Decision.Retained receipt ->
          Retained receipt
        | Prepare deferred -> Issued { deferred; retry = None })
    | Retry { observation; intent; previous; request } ->
      Actor.begin_recurrence_retry_preparation
        target
        ~observation
        ~actor
        ~authorize
        ~intent
        ~previous
        ~request
      |> Result.map ~f:(fun (retry, deferred) -> Issued { deferred; retry = Some retry })
    | Resume retry ->
      Actor.resume_recurrence_retry_preparation target ~retry ~actor ~authorize
      |> Result.map ~f:(fun (retry, deferred) -> Issued { deferred; retry = Some retry })
  ;;

  let issue t target =
    match t with
    | Operator { actor; authorize; request } ->
      issue_operator request target ~actor ~authorize
    | Native_start selection ->
      Agent_session.Session_actor.begin_native_goal_start_preparation target ~selection
      |> Result.map ~f:(function
        | Agent_session.Run_preparation.Deferred.Decision.Retained receipt ->
          Retained receipt
        | Prepare deferred -> Issued { deferred; retry = None })
  ;;
end

let with_deferred_source_reported t ~source_owner ~issuance f =
  let deferred = ref None in
  let retry = ref None in
  let outcome =
    match Scope_outcome.capture (fun () -> check_source_owner t source_owner) with
    | Raised (exn, backtrace) ->
      { Scope_outcome.primary = Raised (exn, backtrace); cleanup = Never_acquired }
    | Returned (Error failure) ->
      { Scope_outcome.primary = Returned (Error failure); cleanup = Never_acquired }
    | Returned (Ok ()) ->
      with_source_stage_reported t (fun stage ->
        let open Result.Let_syntax in
        let%bind decision =
          Eio.Cancel.protect (fun () ->
            let result = Deferred_issuance.issue issuance t.actor in
            Result.iter result ~f:(function
              | Deferred_issuance.Issued issued ->
                (* Capture the actual pair before pending cancellation can escape,
                   including when source selection never invokes its callback. *)
                deferred := Some issued.deferred;
                retry := issued.retry
              | Retained _ -> ());
            result)
        in
        match decision with
        | Retained receipt -> Ok (Deferred_source.Decision.Retained receipt)
        | Issued issued ->
          Eio.Fiber.check ();
          let%bind consumer = Source_consumer.create stage in
          let source =
            { Deferred_source.owner = t
            ; stage
            ; consumer
            ; deferred = issued.deferred
            ; retry = issued.retry
            ; source_attempted = ref false
            }
          in
          Result.map (f source) ~f:(fun result -> Deferred_source.Decision.Fresh result))
  in
  { Deferred_source.Report.deferred = !deferred; retry = !retry; outcome }
;;

let with_effect_deferred_source_reported t ~source_owner ~actor ~authorize ~request f =
  with_deferred_source_reported
    t
    ~source_owner
    ~issuance:(Deferred_issuance.Operator { actor; authorize; request = Initial request })
    f
;;

let with_recurrence_retry_source_reported
      t
      ~source_owner
      ~observation
      ~actor
      ~authorize
      ~intent
      ~previous
      ~request
      f
  =
  with_deferred_source_reported
    t
    ~source_owner
    ~issuance:
      (Deferred_issuance.Operator
         { actor; authorize; request = Retry { observation; intent; previous; request } })
    f
;;

let resume_recurrence_retry_source_reported t ~source_owner ~retry ~actor ~authorize f =
  with_deferred_source_reported
    t
    ~source_owner
    ~issuance:(Deferred_issuance.Operator { actor; authorize; request = Resume retry })
    f
;;

let with_native_goal_start_source_reported t ~source_owner ~selection f =
  with_deferred_source_reported
    t
    ~source_owner
    ~issuance:(Deferred_issuance.Native_start selection)
    f
;;

module Held_runtime_residency = struct
  type t =
    | Local of
        { binding : Runtime_binding.t
        ; construction : Construction_cleanup.t
        }
    | Reused of Runtime_binding.t
    | Installed of
        { binding : Runtime_binding.t
        ; construction : Construction_cleanup.t
        }
    | Handed_off of Runtime_binding.t
    | Ordinary_installed of Runtime_binding.t

  let binding = function
    | Local { binding; _ }
    | Reused binding
    | Installed { binding; _ }
    | Handed_off binding
    | Ordinary_installed binding -> binding
  ;;

  let handed_off residency =
    Option.iter !residency ~f:(fun current ->
      residency := Some (Handed_off (binding current)))
  ;;

  let ordinary_installed residency ~recovery =
    Option.iter !residency ~f:(fun current ->
      let binding = binding current in
      residency := Some (Ordinary_installed binding);
      Recovery_data.record_transfer
        recovery
        ~binding
        ~proof:Source_transfer.Ordinary_installed)
  ;;
end

let bind_existing_goal_source owner stage attempted ~label ~source ~bind ~wrap =
  let open Result.Let_syntax in
  let%bind () = consume_source_selection owner stage attempted ~label in
  Eio.Cancel.protect (fun () ->
    let result = bind source in
    Result.iter result ~f:(fun binding ->
      let owned = wrap binding in
      stage.binding <- Some owned;
      if stage.closed then Source_stage_binding.close owned);
    result)
;;

let with_delegated_load_source_callback_internal
      ?preparation_recovery
      ?admission
      t
      ~source_owner
      ~custody
      callback
  =
  let module CO = Agent_session.Close_observation in
  let checked = Observed_result.raw (fun () -> check_source_owner t source_owner) in
  match Observed_result.negative checked with
  | Some exit ->
    let observation =
      CO.create
        exit
        ~diagnostics:(CO.diagnostics checked)
        ?primary:(CO.primary checked)
        ()
    in
    let report =
      { Scope_outcome.primary = Physical_origin.primary observation
      ; cleanup = Never_acquired
      }
    in
    { Source_stage_outcome.reported =
        CO.create (Returned report) ~diagnostics:(CO.diagnostics observation) ()
    ; observation
    ; cleanup = Source_stage_outcome.Cleanup.Not_entered
    ; selected_lease = None
    }
  | None ->
    with_source_stage_observed_internal ?preparation_recovery ?admission t (fun stage ->
      let source =
        Observed_result.raw (fun () ->
          let module Actor = Agent_session.Session_actor in
          let open Result.Let_syntax in
          let%bind preparation =
            Eio.Cancel.protect (fun () ->
              let result =
                Actor.begin_delegated_load_preparation t.actor ~source_owner ~custody
              in
              Result.iter result ~f:(fun preparation ->
                stage.load_preparation <- Some preparation;
                Recovery_data.register_source_target
                  stage.extent.recovery
                  (Delegated_end preparation);
                if stage.closed then Actor.Delegated_load.Preparation.close preparation);
              result)
          in
          Eio.Fiber.check ();
          let attempted = ref false in
          let prepare_source ~source =
            let%bind binding =
              bind_existing_goal_source
                t
                stage
                attempted
                ~label:"delegated load source"
                ~source
                ~bind:(fun source ->
                  Actor.bind_delegated_load_source t.actor ~preparation ~source)
                ~wrap:(fun binding -> Source_stage_binding.Delegated_load binding)
            in
            let prepared : Prepared_delegated_load.t =
              { stage; binding; basis = source_stage_basis t stage }
            in
            let%bind () = Prepared_delegated_load.check_basis prepared in
            Eio.Fiber.check ();
            Ok prepared
          in
          let%bind consumer = Source_consumer.create stage in
          Ok { Delegated_load_source.consumer; prepare_source })
      in
      Observed_result.bind source ~f:(fun source ->
        Lease_invocation.invoke callback ~recovery:stage.extent.recovery source))
;;

let with_delegated_load_source_reported_internal
      ?preparation_recovery
      ?admission
      t
      ~source_owner
      ~custody
      f
  =
  with_delegated_load_source_callback_internal
    ?preparation_recovery
    ?admission
    t
    ~source_owner
    ~custody
    (Lease_invocation.Raw (fun ~recovery:_ source -> f source))
  |> Source_stage_outcome.report
;;

let with_delegated_load_source_reported t ~source_owner ~custody f =
  with_delegated_load_source_reported_internal t ~source_owner ~custody f
;;

let with_delegated_load_source_observed t ~source_owner ~custody f =
  with_delegated_load_source_callback_internal
    t
    ~source_owner
    ~custody
    (Lease_invocation.Observed (fun ~recovery:_ source -> f source))
  |> Source_stage_outcome.observation
;;

module Goal_resume_source = struct
  type owner = t

  type t =
    { owner : owner
    ; stage : source_stage
    ; preparation : Agent_session.Session_actor.Goal_continuation.Preparation.t
    ; source_attempted : bool ref
    }

  let preparation t = t.preparation

  let prepare t ~source =
    let module Actor = Agent_session.Session_actor in
    let open Result.Let_syntax in
    let%bind binding =
      bind_existing_goal_source
        t.owner
        t.stage
        t.source_attempted
        ~label:"Goal resume source"
        ~source
        ~bind:(fun source ->
          Actor.bind_goal_continuation_source
            t.owner.actor
            ~preparation:t.preparation
            ~source)
        ~wrap:(fun binding -> Source_stage_binding.Existing_goal binding)
    in
    let prepared : Prepared_goal_continuation.t =
      { stage = t.stage; binding; basis = source_stage_basis t.owner t.stage }
    in
    let%bind () = Prepared_goal_continuation.check_basis prepared in
    Eio.Fiber.check ();
    Ok prepared
  ;;

  module Decision = struct
    type 'a t =
      | Retained of Agent_session.Goal_command_receipt.t
      | Fresh of 'a
  end

  module Report = struct
    type 'a t =
      { preparation : Agent_session.Session_actor.Goal_continuation.Preparation.t option
      ; outcome : 'a Decision.t Scope_outcome.t
      }

    let preparation t = t.preparation
    let outcome t = t.outcome
  end
end

module Goal_resume_issuer = struct
  type t =
    | Operator of
        { actor : Operator_authorization.t
        ; authorize :
            Agent_session.Session_state.t -> (unit, Agent_protocol.Error.t) result
        ; session : Agent_protocol.Session_ref.t
        ; request : Agent_session.Goal_resume_request.t
        }
    | Native of Agent_session.Session_actor.Goal_native_resume.Selection.t

  let issue t actor =
    match t with
    | Operator { actor = original; authorize; session; request } ->
      Agent_session.Session_actor.begin_goal_resume_preparation
        actor
        ~actor:original
        ~authorize
        ~session
        ~request
    | Native selection ->
      Agent_session.Session_actor.begin_native_goal_resume_preparation actor ~selection
  ;;
end

let with_goal_resume_issued_source_reported t ~source_owner ~issuer f =
  let module Actor = Agent_session.Session_actor in
  let preparation = ref None in
  let outcome =
    match Scope_outcome.capture (fun () -> check_source_owner t source_owner) with
    | Raised (exn, backtrace) ->
      { Scope_outcome.primary = Raised (exn, backtrace); cleanup = Never_acquired }
    | Returned (Error failure) ->
      { Scope_outcome.primary = Returned (Error failure); cleanup = Never_acquired }
    | Returned (Ok ()) ->
      with_source_stage_reported t (fun stage ->
        let open Result.Let_syntax in
        let%bind decision =
          Eio.Cancel.protect (fun () ->
            let result = Goal_resume_issuer.issue issuer t.actor in
            Result.iter result ~f:(function
              | Actor.Goal_continuation.Resume_issuance.Prepare issued ->
                preparation := Some issued
              | Retained _ -> ());
            result)
        in
        match decision with
        | Retained receipt -> Ok (Goal_resume_source.Decision.Retained receipt)
        | Prepare issued ->
          let source =
            { Goal_resume_source.owner = t
            ; stage
            ; preparation = issued
            ; source_attempted = ref false
            }
          in
          Eio.Fiber.check ();
          Result.map (f source) ~f:(fun value -> Goal_resume_source.Decision.Fresh value))
  in
  { Goal_resume_source.Report.preparation = !preparation; outcome }
;;

let with_goal_resume_source_reported t ~source_owner ~actor ~authorize ~session ~request f
  =
  with_goal_resume_issued_source_reported
    t
    ~source_owner
    ~issuer:(Goal_resume_issuer.Operator { actor; authorize; session; request })
    f
;;

let with_native_goal_resume_source_reported t ~source_owner ~selection f =
  with_goal_resume_issued_source_reported
    t
    ~source_owner
    ~issuer:(Goal_resume_issuer.Native selection)
    f
;;

module Goal_execution_source = struct
  type owner = t

  type t =
    { owner : owner
    ; stage : source_stage
    ; preparation : Agent_session.Session_actor.Goal_execution_runtime.Preparation.t
    ; source_attempted : bool ref
    }

  let prepare t ~source =
    let module Actor = Agent_session.Session_actor in
    let open Result.Let_syntax in
    let%bind binding =
      bind_existing_goal_source
        t.owner
        t.stage
        t.source_attempted
        ~label:"Goal execution source"
        ~source
        ~bind:(fun source ->
          Actor.bind_goal_execution_runtime_source
            t.owner.actor
            ~preparation:t.preparation
            ~source)
        ~wrap:(fun binding -> Source_stage_binding.Existing_goal_execution binding)
    in
    let prepared : Prepared_goal_execution.t =
      { stage = t.stage; binding; basis = source_stage_basis t.owner t.stage }
    in
    let%bind () = Prepared_goal_execution.check_basis prepared in
    Eio.Fiber.check ();
    Ok prepared
  ;;

  module Decision = struct
    type 'a t =
      | No_work
      | Suspended_goal of Agent_session.Goal_execution_selection.Suspension.t
      | Fresh of 'a
  end

  module Report = struct
    type 'a t =
      { preparation :
          Agent_session.Session_actor.Goal_execution_runtime.Preparation.t option
      ; outcome : 'a Decision.t Scope_outcome.t
      }

    let preparation t = t.preparation
    let outcome t = t.outcome
  end
end

module Goal_execution_issuance = struct
  type t =
    | Wake of Agent_protocol.Run_wake.t
    | Claimed_job of Agent_session.Session_actor.Job_runtime_owner.t

  let issue t actor =
    let module Actor = Agent_session.Session_actor in
    match t with
    | Wake wake -> Actor.begin_goal_wake_preparation actor ~wake
    | Claimed_job owner ->
      Actor.begin_goal_job_runtime_preparation actor ~owner
      |> Result.map ~f:(fun preparation ->
        Actor.Goal_execution_runtime.Wake_issuance.Prepare preparation)
  ;;
end

let with_goal_execution_source_reported t ~source_owner ~issuance f =
  let module Actor = Agent_session.Session_actor in
  let preparation = ref None in
  let outcome =
    match Scope_outcome.capture (fun () -> check_source_owner t source_owner) with
    | Raised (exn, backtrace) ->
      { Scope_outcome.primary = Raised (exn, backtrace); cleanup = Never_acquired }
    | Returned (Error failure) ->
      { Scope_outcome.primary = Returned (Error failure); cleanup = Never_acquired }
    | Returned (Ok ()) ->
      with_source_stage_reported t (fun stage ->
        let open Result.Let_syntax in
        let%bind decision =
          Eio.Cancel.protect (fun () ->
            let result = Goal_execution_issuance.issue issuance t.actor in
            Result.iter result ~f:(function
              | Actor.Goal_execution_runtime.Wake_issuance.Prepare issued ->
                preparation := Some issued
              | No_work | Suspended_goal _ -> ());
            result)
        in
        match decision with
        | No_work -> Ok Goal_execution_source.Decision.No_work
        | Suspended_goal suspension -> Ok (Suspended_goal suspension)
        | Prepare issued ->
          let source =
            { Goal_execution_source.owner = t
            ; stage
            ; preparation = issued
            ; source_attempted = ref false
            }
          in
          Eio.Fiber.check ();
          Result.map (f source) ~f:(fun result ->
            Goal_execution_source.Decision.Fresh result))
  in
  { Goal_execution_source.Report.preparation = !preparation; outcome }
;;

let with_goal_wake_source_reported t ~source_owner ~wake f =
  with_goal_execution_source_reported
    t
    ~source_owner
    ~issuance:(Goal_execution_issuance.Wake wake)
    f
;;

let with_goal_job_source_reported t ~source_owner ~owner f =
  with_goal_execution_source_reported
    t
    ~source_owner
    ~issuance:(Goal_execution_issuance.Claimed_job owner)
    f
;;

module Runtime_request = struct
  module Actor = Agent_session.Session_actor

  type _ t =
    | Delegated_load : Prepared_delegated_load.t -> Actor.Delegated_load.t t
    | Initial :
        { prepared : Prepared.t
        ; held : Actor.Held_run.t
        ; checked : Actor.Admitted_activation.Checked.t
        }
        -> Actor.Admitted_activation.Continuation.t t
    | Existing_goal :
        { prepared : Prepared_goal_continuation.t
        ; continuation : Actor.Goal_continuation.t
        }
        -> Actor.Goal_continuation.t t
    | Existing_goal_execution :
        { prepared : Prepared_goal_execution.t
        ; execution : Actor.Goal_execution_runtime.t
        }
        -> Actor.Goal_execution_runtime.t t

  let stage : type a. a t -> source_stage = function
    | Delegated_load prepared -> prepared.stage
    | Initial { prepared; _ } -> prepared.stage
    | Existing_goal { prepared; _ } -> prepared.stage
    | Existing_goal_execution { prepared; _ } -> prepared.stage
  ;;

  let basis : type a. a t -> unit -> (unit, Agent_protocol.Error.t) result = function
    | Delegated_load prepared -> prepared.basis
    | Initial { prepared; _ } -> prepared.basis
    | Existing_goal { prepared; _ } -> prepared.basis
    | Existing_goal_execution { prepared; _ } -> prepared.basis
  ;;

  let check_prepared : type a. a t -> (unit, Agent_protocol.Error.t) result = function
    | Delegated_load prepared -> Prepared_delegated_load.check_basis prepared
    | Initial { prepared; _ } -> Prepared.check_basis prepared
    | Existing_goal { prepared; continuation } ->
      let open Result.Let_syntax in
      let%bind () =
        Actor.Goal_continuation.check_source_binding
          continuation
          ~binding:prepared.binding
      in
      Prepared_goal_continuation.check_basis prepared
    | Existing_goal_execution { prepared; execution } ->
      let open Result.Let_syntax in
      let%bind () =
        Actor.Goal_execution_runtime.check_source_binding
          execution
          ~binding:prepared.binding
      in
      Prepared_goal_execution.check_basis prepared
  ;;

  let source : type a. a t -> Agent_session.Runtime_builder.Source.t = function
    | Delegated_load prepared ->
      Actor.Delegated_load.Source_binding.source prepared.binding
    | Initial { prepared; _ } -> Actor.Source_binding.source prepared.binding
    | Existing_goal { prepared; _ } ->
      Actor.Goal_continuation.Source_binding.source prepared.binding
    | Existing_goal_execution { prepared; _ } ->
      Actor.Goal_execution_runtime.Source_binding.source prepared.binding
  ;;

  let acquire : type a. Actor.t -> a t -> (a, Agent_protocol.Error.t) result =
    fun actor -> function
    | Delegated_load prepared ->
      Actor.prepare_delegated_load actor ~binding:prepared.binding
    | Initial { prepared; held; checked } ->
      Actor.begin_held_activation actor ~binding:prepared.binding ~held ~checked
    | Existing_goal { prepared; continuation } ->
      Actor.Goal_continuation.check_source_binding continuation ~binding:prepared.binding
      |> Result.map ~f:(fun () -> continuation)
    | Existing_goal_execution { prepared; execution } ->
      Actor.Goal_execution_runtime.check_source_binding
        execution
        ~binding:prepared.binding
      |> Result.map ~f:(fun () -> execution)
  ;;

  let construction : type a. a t -> a -> Actor.Runtime_construction.t =
    fun request active ->
    match request with
    | Delegated_load _ -> Actor.Runtime_construction.of_delegated_load active
    | Initial _ -> Actor.Runtime_construction.of_admitted active
    | Existing_goal _ -> Actor.Runtime_construction.of_goal active
    | Existing_goal_execution _ ->
      Actor.Runtime_construction.of_existing_goal_execution active
  ;;

  let begin_cold
    : type a.
      Actor.t
      -> a t
      -> Actor.Runtime_construction.t
      -> (unit, Agent_protocol.Error.t) result
    =
    fun actor request construction ->
    match request with
    | Initial _ | Delegated_load _ -> Ok ()
    | Existing_goal _ | Existing_goal_execution _ ->
      Actor.begin_goal_runtime_construction actor ~construction
  ;;
end

let with_runtime_construction_callback_internal
      (type a)
      t
      (request : a Runtime_request.t)
      callback
  =
  let module Actor = Agent_session.Session_actor in
  let module Construction = Actor.Runtime_construction in
  let open Observed_result in
  let stage = Runtime_request.stage request in
  let owner_basis () =
    if
      t.closed
      || resource_retirement_blocks_admission t
      || (not
            (Option.equal phys_equal t.foreground_retirement stage.foreground_retirement))
      || (not (Option.exists t.prepared_source ~f:(phys_equal stage)))
      || (not (Option.equal phys_equal t.binding stage.runtime))
      || (not (Option.equal phys_equal t.restored_policy stage.restored_anchor))
      || not (Option.equal phys_equal t.native_anchor stage.native_anchor)
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"prepared source belongs to another or displaced runtime owner"
           ~retryable:false
           ())
    else Runtime_request.basis request ()
  in
  let%bind () = Observed_result.raw (fun () -> Runtime_request.check_prepared request) in
  let%bind () = Observed_result.raw owner_basis in
  let%bind () =
    Observed_result.raw (fun () ->
      match stage.runtime, t.build_deferred with
      | None, None ->
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"runtime owner has no verified-source deferred constructor"
             ~retryable:false
             ())
      | Some _, (Some _ | None) | None, Some _ -> Ok ())
  in
  let construction = ref None in
  let residency : Held_runtime_residency.t option ref = ref None in
  let consumed_basis context =
    if
      (not (Int.equal stage.domain (Stdlib.Domain.self () :> int)))
      || stage.closed
      || not (Option.exists stage.continuation ~f:(phys_equal context))
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"consumed runtime source owner is closed or displaced"
           ~retryable:false
           ())
    else owner_basis ()
  in
  let check_effect context =
    let open Result.Let_syntax in
    let%bind () = consumed_basis context in
    let%bind () = Construction.refresh context in
    let%bind () = Construction.check_policy context in
    let%bind () = Construction.check_timing context in
    let%bind () = Construction.check_authority context in
    let%bind () = consumed_basis context in
    Construction.check_basis context
  in
  let cleanup_diagnostics = ref [] in
  let cleanup () =
    Eio.Cancel.protect (fun () ->
      (* Observe only the actual ACK on this original construction. Its SAME
         receipt and residency survive Reused and a lost post-ACK reply. Capture
         before close/End can fail or source association can be cleared. *)
      Option.iter !construction ~f:(fun receipt ->
        let context = Construction_cleanup.construction receipt in
        if Construction.handoff_acknowledged context
        then
          Option.iter !residency ~f:(fun current ->
            Recovery_data.record_transfer
              stage.extent.recovery
              ~binding:(Held_runtime_residency.binding current)
              ~proof:(Source_transfer.Handed_off receipt)));
      let observed_primary primary diagnostic =
        Agent_session.Close_observation.create
          (match primary with
           | Scope_outcome.Returned result -> Returned result
           | Raised (exn, trace) -> Raised (exn, trace))
          ~diagnostics:(Option.to_list diagnostic)
          ?primary:diagnostic
          ()
      in
      let ended =
        Physical_origin.capture_result (fun () ->
          match !construction with
          | None -> observed_primary (Scope_outcome.Returned (Ok ())) None
          | Some receipt ->
            let context = Construction_cleanup.construction receipt in
            let closed = Construction_cleanup.close_original receipt in
            (match closed with
             | Returned (Error _) | Raised _ ->
               observed_primary closed receipt.close_diagnostic
             | Returned (Ok ()) ->
               let retained =
                 capture_source_operation_observed
                   ~operation:(Construction_retention_check receipt)
                   stage.extent
                   (fun () -> Ok (Construction.admission_retained context))
               in
               (match Agent_session.Close_observation.exit retained with
                | Returned (Ok true) ->
                  Construction_cleanup.retain_admission receipt;
                  observed_primary (Scope_outcome.Returned (Ok ())) None
                | Returned (Ok false) ->
                  let original =
                    Scope_outcome.capture (fun () ->
                      Actor.end_runtime_construction t.actor ~construction:context)
                  in
                  (* Keep the actual End carrier before projecting its exit. *)
                  Construction_cleanup.record_end receipt original;
                  observed_primary original receipt.end_diagnostic
                | Returned (Error error) ->
                  Agent_session.Close_observation.create
                    (Returned (Error error))
                    ~diagnostics:(Agent_session.Close_observation.diagnostics retained)
                    ?primary:(Agent_session.Close_observation.primary retained)
                    ()
                | Raised (exn, trace) ->
                  Agent_session.Close_observation.create
                    (Raised (exn, trace))
                    ~diagnostics:(Agent_session.Close_observation.diagnostics retained)
                    ?primary:(Agent_session.Close_observation.primary retained)
                    ())))
      in
      cleanup_diagnostics := Agent_session.Close_observation.diagnostics ended;
      let ended_primary = Physical_origin.primary ended in
      Source_extent.record_projection
        stage.extent
        (Cleanup_projection.of_primary
           ~origin:(Physical_origin.of_observation ended)
           ~acquired:(Option.is_some !construction)
           ended_primary);
      let closed =
        match !residency with
        | Some (Local { binding; construction = local_construction }) ->
          residency := None;
          let cleanup = Cleanup_request.create () in
          Observed_result.capture (fun () ->
            let selected =
              Observed_result.capture (fun () ->
                capture_source_operation_observed
                  ~operation:(Local_release_selection local_construction)
                  stage.extent
                  (fun () ->
                     with_owner_lock t ~protect:true (fun () ->
                       release_runtime_locked t cleanup binding);
                     Ok ())
                |> record_source_observation_observed stage.extent)
            in
            with_source_cleanup_request_observed
              t
              stage.extent
              ~role:(Construction_local local_construction)
              cleanup
              selected)
        | None | Some (Reused _ | Handed_off _ | Ordinary_installed _) ->
          Observed_result.return ()
        | Some (Installed { binding; construction = installed_construction }) ->
          let step =
            Recovery_data.register_original_step
              stage.extent.recovery
              (Construction_residency_check installed_construction)
          in
          let original =
            Original_cleanup_step.capture step (fun () ->
              let runtime = Runtime_binding.runtime binding in
              if
                Option.exists !construction ~f:(fun receipt ->
                  Construction.handoff_acknowledged
                    (Construction_cleanup.construction receipt))
              then (
                (* Actual adoption survives a lost publication/reply for all
                 final outcomes. Never locally close an ACKed worker. *)
                residency := Some (Handed_off binding);
                Ok ())
              else if
                Option.exists !construction ~f:(fun receipt ->
                  Construction.admission_retained
                    (Construction_cleanup.construction receipt))
              then Ok ()
              else (
                stage.retire_installed <- Some runtime;
                Ok ()))
          in
          let projection = Original_cleanup_step.projection step ~acquired:false in
          Source_extent.record_projection stage.extent projection;
          let origin = Cleanup_projection.origin projection in
          Observed_result.of_primary
            original
            ~diagnostics:
              (Option.value_map origin ~default:[] ~f:(fun origin ->
                 origin.Physical_origin.diagnostics))
            ?primary:
              (Option.bind origin ~f:(fun origin -> origin.Physical_origin.primary))
            ()
      in
      cleanup_diagnostics
      := !cleanup_diagnostics @ Agent_session.Close_observation.diagnostics closed;
      let closed_projection =
        Cleanup_projection.of_primary
          ~origin:(Physical_origin.of_observation closed)
          ~acquired:false
          (Physical_origin.primary closed)
      in
      Observed_result.with_cleanup ended closed_projection)
  in
  let result =
    Observed_result.capture (fun () ->
      let open Observed_result in
      let%bind built, active, context =
        Observed_result.raw (fun () ->
          let open Result.Let_syntax in
          let%bind active, context, receipt =
            Eio.Cancel.protect (fun () ->
              let%map active = Runtime_request.acquire t.actor request in
              let context = Runtime_request.construction request active in
              let receipt =
                Recovery_data.register_construction stage.extent.recovery context
              in
              construction := Some receipt;
              stage.continuation <- Some context;
              if stage.closed then Construction.close context;
              active, context, receipt)
          in
          Eio.Fiber.check ();
          let%bind built =
            match stage.runtime with
            | Some binding ->
              residency := Some (Held_runtime_residency.Reused binding);
              Ok (Construction_result.Built (Runtime_binding.runtime binding))
            | None ->
              let%bind () = check_effect context in
              let%bind () =
                match request with
                | Runtime_request.Initial _ | Delegated_load _ -> Ok ()
                | Existing_goal _ | Existing_goal_execution _ ->
                  let%bind () = Runtime_request.begin_cold t.actor request context in
                  check_effect context
              in
              (match t.build_deferred with
               | None ->
                 Error
                   (Agent_protocol.Error.create
                      Invalid_state
                      ~message:"runtime owner has no verified-source deferred constructor"
                      ~retryable:false
                      ())
               | Some build ->
                 let%bind source_consumer = Source_consumer.create stage in
                 let result =
                   build
                     ~initialization_cleanup:
                       (initialization_capture_for_source t stage.extent.recovery)
                     ~source_consumer
                     ~source:(Runtime_request.source request)
                     ~expected_product:
                       (Option.first_some
                          (Option.map stage.resource_handoff ~f:Resource_handoff.product)
                          (Option.map
                             stage.restored_anchor
                             ~f:Restored_policy_anchor.product))
                     ~construction:context
                 in
                 Result.map result ~f:(function
                   | Construction_result.Built attachment ->
                     let binding = Runtime_binding.Attached attachment in
                     residency
                     := Some
                          (Held_runtime_residency.Local
                             { binding; construction = receipt });
                     Construction_result.Built (Runtime_binding.runtime binding)
                   | Callback_completed completion -> Callback_completed completion))
          in
          Ok (built, active, context))
      in
      let selected =
        match built with
        | Construction_result.Callback_completed completion ->
          Observed_result.raw (fun () ->
            let%map.Result () = consumed_basis context in
            Construction_result.Callback_completed completion)
        | Built runtime ->
          let%bind () =
            Observed_result.raw (fun () ->
              let open Result.Let_syntax in
              let%bind () =
                match runtime.Agent_session.Runtime_builder.check_execution with
                | None -> Ok ()
                | Some check -> check ()
              in
              check_effect context)
          in
          Lease_invocation.invoke
            callback
            ~recovery:stage.extent.recovery
            (runtime, active, context, residency)
      in
      Observed_result.bind selected ~f:(fun value ->
        Observed_result.raw (fun () ->
          Eio.Fiber.check ();
          Ok value)))
  in
  (* This invocation was outside the old body catch. A genuinely escaping
     cleanup failure overrides even a negative body; a returned End/local failure
     follows the old primary-first result selector. Do not conflate those cases. *)
  match cleanup () with
  | cleaned ->
    Observed_result.with_cleanup
      result
      (Cleanup_projection.of_primary
         ~origin:(Physical_origin.of_observation cleaned)
         ~acquired:false
         (Physical_origin.primary cleaned))
  | exception exn ->
    let trace = Stdlib.Printexc.get_raw_backtrace () in
    let escaped =
      Physical_origin.with_boundary_failure
        (Agent_session.Close_observation.create (Raised (exn, trace)) ~diagnostics:[] ())
        (Some (Agent_session.Callback_cleanup.Failure.Raised (exn, trace)))
    in
    Agent_session.Close_observation.create
      (Raised (exn, trace))
      ~diagnostics:
        (Agent_session.Close_observation.diagnostics result
         @ !cleanup_diagnostics
         @ Agent_session.Close_observation.diagnostics escaped)
      ?primary:(Agent_session.Close_observation.primary escaped)
      ()
;;

let with_runtime_construction_internal t request f =
  with_runtime_construction_callback_internal
    t
    request
    (Lease_invocation.Raw
       (fun ~recovery:_ (runtime, active, context, residency) ->
         f runtime active context residency))
  |> Agent_session.Close_observation.resume
;;

let with_held_runtime_internal t ~prepared ~held ~checked f =
  with_runtime_construction_internal
    t
    (Runtime_request.Initial { prepared; held; checked })
    f
;;

let with_held_runtime t ~prepared ~held ~checked f =
  with_held_runtime_internal
    t
    ~prepared
    ~held
    ~checked
    (fun runtime continuation _construction _residency ->
       Result.map (f runtime continuation) ~f:(fun value ->
         Construction_result.Built value))
  |> Result.bind ~f:ordinary_construction_result
;;

let with_held_recurrence_runtime t ~prepared ~held ~checked f =
  with_held_runtime
    t
    ~prepared
    ~held
    ~checked:(Agent_session.Session_actor.Recurrence_activation.Checked.admitted checked)
    f
;;

let install_runtime_for_request
      (type a)
      ?retained_lifecycle
      t
      (request : a Runtime_request.t)
      ~runtime
      ~construction
      ~residency
  =
  let module Actor = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let stage = Runtime_request.stage request in
  let unavailable () =
    Agent_protocol.Error.create
      Conflict
      ~message:"held runtime installation owner was displaced"
      ~retryable:false
      ()
  in
  let%bind () =
    with_owner_lock t ~protect:true (fun () ->
      if
        t.closed
        || resource_retirement_blocks_admission t
        || (not
              (Option.equal
                 phys_equal
                 t.foreground_retirement
                 stage.foreground_retirement))
        || stage.closed
        || (not (Option.exists t.prepared_source ~f:(phys_equal stage)))
        || (not (Option.equal phys_equal t.binding stage.runtime))
        || not (Option.exists stage.continuation ~f:(phys_equal construction))
      then Error (unavailable ())
      else Runtime_request.basis request ())
  in
  let install () =
    Actor.install_runtime_construction
      ?unmoderated_source:runtime.Agent_session.Runtime_builder.unmoderated_source
      ?retained_lifecycle
      ?automatic_turn_policy:runtime.Agent_session.Runtime_builder.automatic_turn_policy
      ?moderator_manager:runtime.moderator_manager
      t.actor
      ~construction
      ~worker:runtime.worker
      ~inference:(Agent_session.Runtime_builder.inference_execution runtime)
  in
  match !residency with
  | Some (Held_runtime_residency.Reused existing)
    when Runtime_binding.matches existing runtime ->
    (match request with
     | Initial _ -> Ok ()
     | Delegated_load _ -> Error (unavailable ())
     | Existing_goal _ | Existing_goal_execution _ ->
       let%bind () = install () in
       (* The reused binding was already Owner-owned. An ACK cannot authorize
          continuation after a signal received during the Actor RPC. *)
       if stage.closed || resource_retirement_pending t
       then Error (background_busy ())
       else Ok ())
  | Some (Local { binding; construction }) when Runtime_binding.matches binding runtime ->
    let%bind () = check_native_installation t binding in
    let%bind () = check_restored_policy_installation t binding in
    Eio.Cancel.protect (fun () ->
      let%bind () = install () in
      with_owner_lock t ~protect:true (fun () ->
        (* Preserve a concurrent newer retirement; the Actor owns this worker
           after its ACK even when Stop closed fresh admission meanwhile. *)
        t.binding <- Some binding;
        (match request with
         | Runtime_request.Delegated_load _ ->
           if Option.equal phys_equal t.foreground_retirement stage.foreground_retirement
           then replace_foreground_retirement t None
         | Initial _ | Existing_goal _ | Existing_goal_execution _ -> ());
        stage.handoff_runtime <- Some runtime;
        residency := Some (Held_runtime_residency.Installed { binding; construction }));
      let%bind () = release_restored_policy_after_install t in
      stage.restored_anchor <- t.restored_policy;
      stage.native_anchor <- t.native_anchor;
      if stage.closed || resource_retirement_pending t
      then Error (background_busy ())
      else Ok ())
  | None | Some (Local _ | Reused _ | Installed _ | Handed_off _ | Ordinary_installed _)
    -> Error (unavailable ())
;;

let load_delegated_runtime_observed t ~prepared ~retention =
  let module Actor = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let request = Runtime_request.Delegated_load prepared in
  with_runtime_construction_callback_internal
    t
    request
    (Lease_invocation.Raw
       (fun ~recovery:_ (runtime, _load, construction, residency) ->
         let%bind () =
           install_runtime_for_request t request ~runtime ~construction ~residency
         in
         let%bind state = Actor.state t.actor in
         let%map () =
           if (Runtime_request.stage request).closed || resource_retirement_pending t
           then Error (background_busy ())
           else Ok ()
         in
         let summary = Agent_session.Session_state.summary state in
         (match retention, state.lifecycle.desired with
          | Delegated_load_retention.Keep_loaded, (Running | Stopped)
          | Release_if_stopped, Running ->
            Held_runtime_residency.ordinary_installed
              residency
              ~recovery:(Runtime_request.stage request).extent.recovery
          | Release_if_stopped, Stopped -> ());
         Construction_result.Built summary))
  |> fun observed ->
  Observed_result.bind observed ~f:(fun result ->
    Observed_result.raw (fun () -> ordinary_construction_result result))
;;

let load_delegated_runtime t ~prepared ~retention =
  load_delegated_runtime_observed t ~prepared ~retention
  |> Agent_session.Close_observation.resume
;;

let run_delegated_driver_observed ?preparation_recovery ?admission t ~custody ~retention =
  match t.delegated_driver with
  | None ->
    Observed_result.raw (fun () ->
      Error (Agent_protocol.Error.invalid_request "owner has no delegated load driver"))
  | Some driver ->
    let callback =
      match driver.run with
      | Delegated_loader.Legacy f ->
        Lease_invocation.Raw (fun ~recovery:_ source -> f ~source ~retention)
      | Observed f ->
        Lease_invocation.Observed (fun ~recovery:_ source -> f ~source ~retention)
    in
    with_delegated_load_source_callback_internal
      ?preparation_recovery
      ?admission
      t
      ~source_owner:driver.source_owner
      ~custody
      callback
    |> Source_stage_outcome.observation
;;

let run_delegated_driver ?preparation_recovery ?admission t ~custody ~retention =
  run_delegated_driver_observed ?preparation_recovery ?admission t ~custody ~retention
  |> Agent_session.Close_observation.resume
;;

module Ordinary_preflight = struct
  type t =
    | Cold_allowed
    | Installed of Runtime_binding.t
    | No_work
end

module Ordinary_preparation = struct
  type t =
    | Prepared of Ordinary_preflight.t
    | Deferred of Agent_protocol.Error.t

  module Selection = struct
    type t =
      | Loaded of Runtime_binding.t option
      | Deferred of Agent_protocol.Error.t
  end
end

let select_ordinary_load_observed ?preparation_recovery ?run_preparation ?original_actor t
  =
  let open Observed_result in
  match t.delegated_driver, t.ordinary_warm with
  | Some _, _ | None, None ->
    Observed_result.return (Ordinary_preparation.Prepared Cold_allowed)
  | None, Some prepare ->
    (* Only the initial pure Owner-field guard below certifies preparation
       deferral. Later source/prepare/cleanup errors keep their original outcome. *)
    let%bind loaded =
      Observed_result.raw (fun () ->
        with_owner_lock t ~protect:true (fun () ->
          if t.closed
          then Error (closed_error ())
          else if
            resource_retirement_blocks_admission t
            || administration_blocks t
            || prepared_source_blocks_runtime t
          then Ok (Ordinary_preparation.Selection.Deferred (background_busy ()))
          else Ok (Ordinary_preparation.Selection.Loaded t.binding)))
    in
    (match loaded with
     | Deferred error -> Observed_result.return (Ordinary_preparation.Deferred error)
     | Loaded (Some binding) ->
       Observed_result.return (Ordinary_preparation.Prepared (Installed binding))
     | Loaded None ->
       with_source_stage_observed_internal ?preparation_recovery t (fun stage ->
         let cleanup = Cleanup_request.create () in
         let observed =
           let%bind selected =
             Observed_result.raw (fun () ->
               prepare
                 ~expected_product:
                   (Option.first_some
                      (Option.map stage.resource_handoff ~f:Resource_handoff.product)
                      (Option.map stage.restored_anchor ~f:Restored_policy_anchor.product))
                 ~original_actor
                 ~run_preparation)
           in
           match selected with
           | No_retained_product -> Observed_result.return Ordinary_preflight.Cold_allowed
           | Attached { attachment; expected } ->
             let binding = Runtime_binding.Attached attachment in
             let installed = ref false in
             let original_actor =
               Option.first_some
                 original_actor
                 (Option.bind
                    run_preparation
                    ~f:Agent_session.Run_preparation.original_actor)
             in
             let primary =
               Observed_result.raw (fun () ->
                 let open Result.Let_syntax in
                 let%bind () = check_original_actor original_actor in
                 let%bind () =
                   with_owner_lock t ~protect:true (fun () ->
                     if stage.closed
                     then Error (background_busy ())
                     else source_stage_basis t stage ())
                 in
                 (* Match existing finite install cancellation protection, without
                  running Factory, source or Manager preparation under mutex. *)
                 Eio.Cancel.protect (fun () ->
                   let%bind () =
                     install_actor
                       ?run_preparation
                       ?original_actor
                       ~expected_ordinary:expected
                       t
                       binding
                   in
                   let%bind () =
                     with_owner_lock t ~protect:true (fun () ->
                       (* The Actor ACK already owns this worker. Like the
                          genuine deferred install, publish its exact residency
                          even if retirement closed the stage while RPC waited.
                          The source lease keeps that retirement joined until
                          this assignment; never locally close an ACKed worker. *)
                       t.binding <- Some binding;
                       if
                         Option.equal
                           phys_equal
                           t.foreground_retirement
                           stage.foreground_retirement
                       then replace_foreground_retirement t None;
                       stage.handoff_runtime <- Some (Runtime_binding.runtime binding);
                       installed := true;
                       Recovery_data.record_transfer
                         stage.extent.recovery
                         ~binding
                         ~proof:Source_transfer.Warm_installed;
                       Ok ())
                   in
                   let%bind () = release_restored_policy_after_install t in
                   stage.restored_anchor <- t.restored_policy;
                   stage.native_anchor <- t.native_anchor;
                   if resource_retirement_pending t
                   then Error (background_busy ())
                   else Ok (Ordinary_preflight.Installed binding)))
             in
             let release_step =
               Recovery_data.register_original_step
                 stage.extent.recovery
                 Warm_release_selection
             in
             let released =
               try
                 if not !installed
                 then
                   with_owner_lock t ~protect:true (fun () ->
                     release_runtime_locked t cleanup binding);
                 Ok (Ok ())
               with
               | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
             in
             Original_cleanup_step.record_projection
               release_step
               (match released with
                | Ok result -> Scope_outcome.Returned result
                | Error (exn, backtrace) -> Raised (exn, backtrace));
             let projection =
               Original_cleanup_step.projection release_step ~acquired:false
             in
             Source_extent.record_projection stage.extent projection;
             Observed_result.with_cleanup primary projection
         in
         with_source_cleanup_request_observed
           t
           stage.extent
           ~role:Ordinary_warm
           cleanup
           observed)
       |> Source_stage_outcome.observation
       |> fun observed ->
       Observed_result.map observed ~f:(fun ordinary ->
         Ordinary_preparation.Prepared ordinary))
;;

let select_ordinary_load ?preparation_recovery ?run_preparation ?original_actor t =
  select_ordinary_load_observed ?preparation_recovery ?run_preparation ?original_actor t
  |> Agent_session.Close_observation.resume
;;

let prepare_ordinary_load ?run_preparation ?original_actor t =
  let open Result.Let_syntax in
  let%bind selected = select_ordinary_load ?run_preparation ?original_actor t in
  match selected with
  | Ordinary_preparation.Prepared ordinary -> Ok ordinary
  | Deferred error -> Error error
;;

let admit_ordinary_preflight_locked
      ?execution_access
      ?run_preparation
      ?original_actor
      t
      ~cleanup
      preflight
  =
  match preflight with
  | Ordinary_preflight.Cold_allowed ->
    ensure_loaded_locked ?execution_access ?run_preparation ?original_actor t ~cleanup
  | No_work -> Error (background_busy ())
  | Installed selected ->
    (match t.binding with
     | Some current when phys_equal current selected ->
       (* The mutex makes this exact loaded branch incapable of cold building. *)
       ensure_loaded_locked ?execution_access ?run_preparation ?original_actor t ~cleanup
     | Some _ | None -> Error (background_busy ()))
;;

let prepare_delegated_load_observed
      ?preparation_recovery
      ?execution_access
      ?run_preparation
      ?original_actor
      t
  =
  let open Observed_result in
  match t.delegated_driver with
  | None -> Observed_result.return ()
  | Some _ ->
    let%bind () =
      Observed_result.raw (fun () ->
        check_original_actor
          (Option.first_some
             original_actor
             (Option.bind run_preparation ~f:Agent_session.Run_preparation.original_actor)))
    in
    let%bind needed =
      Observed_result.raw (fun () ->
        with_owner_lock t ~protect:true (fun () ->
          if t.closed
          then Error (closed_error ())
          else if
            prepared_source_blocks_runtime ?execution_access t
            || resource_retirement_blocks_admission t
          then Error (background_busy ())
          else Ok (Option.is_some t.delegated_driver && Option.is_none (loaded_runtime t))))
    in
    if not needed
    then Observed_result.return ()
    else (
      let custody =
        match run_preparation with
        | Some preparation ->
          Agent_session.Session_actor.Delegated_load.Custody.Prepared_run preparation
        | None -> Host original_actor
      in
      run_delegated_driver_observed
        ?preparation_recovery
        t
        ~custody
        ~retention:Keep_loaded
      |> fun observed -> Observed_result.map observed ~f:ignore)
;;

let prepare_delegated_load
      ?preparation_recovery
      ?execution_access
      ?run_preparation
      ?original_actor
      t
  =
  prepare_delegated_load_observed
    ?preparation_recovery
    ?execution_access
    ?run_preparation
    ?original_actor
    t
  |> Agent_session.Close_observation.resume
;;

module Runtime_lease_attempt = struct
  type 'a t =
    | Preparation_deferred of Agent_protocol.Error.t
    | Lease_report of 'a Scope_outcome.t
end

module Runtime_lease_preflight = struct
  type t =
    | Unstarted
    | Refused of Agent_protocol.Error.t
    | Raised of exn * Stdlib.Printexc.raw_backtrace
    | Deferred of Agent_protocol.Error.t
    | Prepared of Ordinary_preflight.t
end

let with_runtime_lease_callback_internal
      ?execution_access
      ?capture_recovery
      ?run_preparation
      t
      ~retain_resources
      callback
  =
  let open Result.Let_syntax in
  let preflight = ref Runtime_lease_preflight.Unstarted in
  let cleanup_request = Cleanup_request.create () in
  let before_admission recovery =
    let module CO = Agent_session.Close_observation in
    let selected =
      let open Observed_result in
      let%bind () =
        prepare_delegated_load_observed
          ~preparation_recovery:recovery
          ?execution_access
          ?run_preparation
          t
      in
      select_ordinary_load_observed ~preparation_recovery:recovery ?run_preparation t
    in
    match CO.exit selected with
    | Raised (exn, backtrace) ->
      preflight := Raised (exn, backtrace);
      CO.create
        (Raised (exn, backtrace))
        ~diagnostics:(CO.diagnostics selected)
        ?primary:(CO.primary selected)
        ()
    | Returned (Error error) ->
      preflight := Refused error;
      CO.create
        (Returned (Error error))
        ~diagnostics:(CO.diagnostics selected)
        ?primary:(CO.primary selected)
        ()
    | Returned (Ok (Ordinary_preparation.Deferred error)) ->
      preflight := Deferred error;
      (* A real pure busy-control decision, never an acquired physical receipt. *)
      let deferred = Observed_result.raw (fun () -> Error error) in
      CO.create
        (CO.exit deferred)
        ~diagnostics:(CO.diagnostics selected @ CO.diagnostics deferred)
        ?primary:(CO.primary deferred)
        ()
    | Returned (Ok (Prepared ordinary)) ->
      let excluded =
        Observed_result.raw (fun () ->
          Agent_session.Session_actor.exclude_effect_deferred_execution t.actor)
      in
      (match CO.exit excluded with
       | Returned (Ok ()) -> preflight := Prepared ordinary
       | Returned (Error error) -> preflight := Refused error
       | Raised (exn, backtrace) -> preflight := Raised (exn, backtrace));
      CO.create
        (CO.exit excluded)
        ~diagnostics:(CO.diagnostics selected @ CO.diagnostics excluded)
        ?primary:(CO.primary excluded)
        ()
  in
  let scope =
    with_lease_scope_callback_internal
      ?execution_access
      ?capture_recovery
      ~cleanup_request
      ~before_admission
      ~purpose:
        (if retain_resources
         then Lease_purpose.Selection.Delegation_resource
         else Exclusive)
      t
      ~survives_stop:retain_resources
      ~admit:(fun () ->
        let open Result.Let_syntax in
        let%bind () =
          match t.binding with
          | Some binding when not (failed_source_allows_runtime_locked t binding) ->
            Error (background_busy ())
          | Some _ | None -> Ok ()
        in
        let ordinary =
          match !preflight with
          | Runtime_lease_preflight.Prepared ordinary -> ordinary
          | Unstarted | Refused _ | Raised _ | Deferred _ ->
            invalid_arg "runtime lease admitted without original preflight"
        in
        let%bind () =
          match t.delegated_driver with
          | None ->
            admit_ordinary_preflight_locked
              ?execution_access
              ?run_preparation
              t
              ~cleanup:cleanup_request
              ordinary
          | Some _ ->
            if
              Option.is_some (loaded_runtime t)
              && (not (resource_retirement_blocks_admission t))
              && Option.is_none t.foreground_retirement
            then Ok ()
            else Error (background_busy ())
        in
        let%map () =
          match execution_access with
          | None -> Ok ()
          | Some access ->
            (* Runtime validation may invoke its own execution checks. Recheck the
             exact borrow after those checks, before publishing this lease. *)
            if execution_access_is_current t access
            then Ok ()
            else Error (background_busy ())
        in
        let runtime = Option.value_exn (loaded_runtime t) in
        Lease_admission.Borrow
          ( runtime
          , (if retain_resources || Option.is_some t.delegated_driver
             then Some runtime
             else None)
          , None ))
      (Lease_invocation.before callback ~check:(fun runtime ->
         match t.delegated_driver with
         | None -> Ok ()
         | Some _ ->
           let%bind () =
             match
               t.delegated_driver, runtime.Agent_session.Runtime_builder.check_execution
             with
             | Some _, Some check -> check ()
             | (Some _ | None), None | None, Some _ -> Ok ()
           in
           Eio.Fiber.check ();
           let%bind () =
             check_original_actor
               (Option.bind
                  run_preparation
                  ~f:Agent_session.Run_preparation.original_actor)
           in
           let%bind () =
             with_owner_lock t ~protect:true (fun () ->
               if
                 (not (Option.exists (loaded_runtime t) ~f:(phys_equal runtime)))
                 || t.closed
                 || resource_retirement_blocks_admission t
                 || Option.is_some t.foreground_retirement
                 || prepared_source_blocks_runtime ?execution_access t
               then Error (background_busy ())
               else Ok ())
           in
           Eio.Fiber.check ();
           Ok ()))
  in
  !preflight, scope
;;

let with_runtime_lease_scope_internal
      ?execution_access
      ?capture_recovery
      ?run_preparation
      t
      ~retain_resources
      f
  =
  with_runtime_lease_callback_internal
    ?execution_access
    ?capture_recovery
    ?run_preparation
    t
    ~retain_resources
    (Lease_invocation.Raw f)
;;

let with_runtime_lease_attempt_internal
      ?execution_access
      ?run_preparation
      t
      ~retain_resources
      f
  =
  let preflight, scope =
    with_runtime_lease_scope_internal
      ?execution_access
      ?run_preparation
      t
      ~retain_resources
      f
  in
  match preflight with
  | Runtime_lease_preflight.Refused error -> Error error
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  | Deferred error -> Ok (Runtime_lease_attempt.Preparation_deferred error)
  | Prepared _ -> Ok (Runtime_lease_attempt.Lease_report scope.Lease_scope_outcome.report)
  | Unstarted ->
    (* The context/capture failed before preflight could start. Do not manufacture
       a Prepared/Deferred branch or hide its original cancellation/rawtrace. *)
    (match Scope_outcome.primary scope.report with
     | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
     | Returned (Error error) -> Error error
     | Returned (Ok _) -> invalid_arg "runtime lease returned without original preflight")
;;

let with_runtime_lease_attempt ?execution_access ?run_preparation t ~retain_resources f =
  with_runtime_lease_attempt_internal
    ?execution_access
    ?run_preparation
    t
    ~retain_resources
    (fun ~recovery:_ runtime -> f runtime)
;;

let with_runtime_lease_internal ?execution_access ?run_preparation t ~retain_resources f =
  let open Result.Let_syntax in
  let%bind attempt =
    with_runtime_lease_attempt_internal
      ?execution_access
      ?run_preparation
      t
      ~retain_resources
      f
  in
  match attempt with
  | Runtime_lease_attempt.Preparation_deferred error -> Error error
  | Lease_report report -> Scope_outcome.resume report
;;

let with_runtime_lease ?execution_access ?run_preparation t ~retain_resources f =
  with_runtime_lease_internal
    ?execution_access
    ?run_preparation
    t
    ~retain_resources
    (fun ~recovery:_ runtime -> f runtime)
;;

let loaded_runtime_basis_locked t runtime =
  if t.closed
  then Error (closed_error ())
  else if
    resource_retirement_blocks_admission t
    || Option.is_some t.foreground_retirement
    || prepared_source_blocks_runtime t
    || not (Option.exists (loaded_runtime t) ~f:(phys_equal runtime))
  then Error (background_busy ())
  else Ok ()
;;

let check_runtime_execution (runtime : Agent_session.Runtime_builder.t) =
  match runtime.check_execution with
  | None -> Ok ()
  | Some check -> check ()
;;

let with_loaded_runtime_reported_internal ?consumer t f =
  with_lease_reported
    ?consumer
    t
    ~survives_stop:false
    ~admit:(fun () ->
      let open Result.Let_syntax in
      let%bind runtime =
        if resource_retirement_pending t
        then Error (background_busy ())
        else (
          match t.closed, t.unloading, t.foreground_retirement, loaded_runtime t with
          | true, _, _, _ -> Error (closed_error ())
          | false, Some _, _, _ | false, None, Some _, _ -> Error (background_busy ())
          | false, None, None, None ->
            Error
              (Agent_protocol.Error.create
                 Invalid_state
                 ~message:"session runtime is not already loaded"
                 ~retryable:true
                 ())
          | false, None, None, Some runtime -> Ok runtime)
      in
      Ok (runtime, Some runtime))
    (fun runtime ->
       let open Result.Let_syntax in
       (* Delegation checks may read Store/Actor state. The same actual lease
          retains this physical runtime while that IO runs outside our mutex. *)
       let%bind () = check_runtime_execution runtime in
       Eio.Fiber.check ();
       let%bind () =
         with_owner_lock t ~protect:true (fun () -> loaded_runtime_basis_locked t runtime)
       in
       Eio.Fiber.check ();
       f runtime)
;;

let with_loaded_runtime t f =
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_actor.exclude_effect_deferred_execution t.actor in
  with_loaded_runtime_reported_internal t f |> Scope_outcome.resume
;;

let with_loaded_source_capabilities_reported ?consumer t ~source_owner f =
  match Scope_outcome.capture (fun () -> check_source_owner t source_owner) with
  | Raised (exn, backtrace) ->
    { Scope_outcome.primary = Raised (exn, backtrace); cleanup = Never_acquired }
  | Returned (Error failure) ->
    { Scope_outcome.primary = Returned (Error failure); cleanup = Never_acquired }
  | Returned (Ok ()) ->
    with_loaded_runtime_reported_internal ?consumer t (fun runtime ->
      let open Result.Let_syntax in
      let unavailable message =
        Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
      in
      let%bind native =
        Result.of_option
          runtime.Agent_session.Runtime_builder.native_runtime
          ~error:(unavailable "delegation parent has no loaded native bindings")
      in
      let%bind () =
        if Lazy.is_val native.capabilities
        then Ok ()
        else
          Error
            (unavailable
               "delegation parent native bindings have not already been realized")
      in
      let%bind capabilities =
        Lazy.force native.capabilities
        |> Result.map_error ~f:(fun error ->
          Agent_protocol.Error.create
            Permission_denied
            ~message:error.Chat_response.Tool_capability.message
            ~retryable:false
            ())
      in
      f capabilities)
;;

let with_native_resources_projected_callback_outcome
      ?consumer:dependency_consumer
      t
      ~source_owner
      ~origin
      ~capture_release
      ~project
      callback
  =
  let module A = Agent_session.Session_actor in
  let module CO = Agent_session.Close_observation in
  let module Preparation = Native_borrow_outcome.Preparation_failure in
  let lease_wrapper = ref None in
  let outer_recovery = ref None in
  let outer_escape = ref None in
  let admission_preparation = ref None in
  let source_preparation = ref None in
  let selection_preparation = ref None in
  let lease_preparation = ref None in
  let pre_callback_stage = function
    | Some
        (Lease_outcome.Primary_stage.Admission | Before_callback | Scope_exit_before_entry)
      -> true
    | Some (Admission_batch | Callback | After_callback | Scope_exit) | None -> false
  in
  let admission = ref None in
  let preadmission = ref None in
  let preparation = ref None in
  let pin = ref Native_borrow_outcome.Pin.Not_acquired in
  let outer_lease = ref Scope_outcome.Cleanup.Never_acquired in
  let preparation_observation = ref (Observed_result.return ()) in
  let pin_observation = ref (Observed_result.return ()) in
  let outer_lease_observation = ref (Observed_result.return ()) in
  let projection observation ~acquired =
    Cleanup_projection.of_primary
      ~origin:(Physical_origin.of_observation observation)
      ~acquired
      (Physical_origin.primary observation)
  in
  let primary_observation =
    Observed_result.capture (fun () ->
      let open Observed_result in
      let admitted =
        Observed_result.raw (fun () ->
          let open Result.Let_syntax in
          let%bind () =
            if Int.equal t.domain (Stdlib.Domain.self () :> int)
            then check_source_owner t source_owner
            else
              Error
                (Agent_protocol.Error.invalid_request
                   "native resource owner belongs to another Eio domain")
          in
          let%bind () = Native_resource_origin.check_issuer origin ~source_owner in
          let%bind () =
            with_owner_lock t ~protect:true (fun () ->
              if failed_source_cleanup_pending t
              then Error (background_busy ())
              else Ok ())
          in
          let%bind access =
            Result.of_option
              t.native_resource_access
              ~error:
                (Agent_protocol.Error.invalid_request "owner has no native resource port")
          in
          let%bind expected = A.state t.actor in
          let current () =
            with_owner_lock t ~protect:true (fun () ->
              if t.closed
              then Error (closed_error ())
              else if resource_retirement_blocks_admission t || administration_blocks t
              then Error (background_busy ())
              else Ok ())
          in
          let consumer : Native_resource_admission.t =
            { domain = t.domain
            ; origin
            ; session_id = expected.identity.session_id
            ; generation = expected.identity.generation
            ; current
            ; closed = false
            }
          in
          admission := Some consumer;
          let%bind () = Native_resource_admission.check consumer ~expected in
          Ok (consumer, access))
      in
      admission_preparation := Preparation.of_observation ~stage:Admission admitted;
      let%bind consumer, access = admitted in
      let previous = retry_native_releases_observed t in
      preadmission := Some (Physical_origin.primary previous);
      let%bind () = previous in
      let%bind () =
        let selected =
          with_owner_lock t ~protect:true (fun () -> native_identity_locked t)
        in
        match selected with
        | Some _ -> Observed_result.return ()
        | None ->
          let callback_preparation = ref None in
          let prepared =
            with_source_stage_observed_internal t (fun stage ->
              let callback =
                let open Observed_result in
                let%bind source =
                  Observed_result.raw (fun () ->
                    native_source_stage t stage consumer access)
                in
                let%bind () =
                  match access.Native_resource_access.prepare with
                  | Raw prepare ->
                    Observed_result.raw (fun () ->
                      prepare ~admission:consumer ~stage:source)
                  | Observed prepare ->
                    Observed_result.capture (fun () ->
                      prepare ~admission:consumer ~stage:source)
                in
                Observed_result.raw (fun () ->
                  with_owner_lock t ~protect:true (fun () ->
                    match t.native_anchor with
                    | Some anchor ->
                      (match anchor.phase with
                       | Available -> Ok ()
                       | Retiring | Closing _ | Released ->
                         Error (retired_cleanup_error ()))
                    | None -> Error (background_busy ())))
              in
              callback_preparation
              := Preparation.of_observation ~stage:Source_callback callback;
              callback)
          in
          preparation_observation
          := Source_stage_outcome.Cleanup.observation
               (Source_stage_outcome.cleanup prepared);
          (match CO.exit prepared.Source_stage_outcome.reported with
           | Returned report -> preparation := Some report
           | Raised _ -> ());
          let observed = Source_stage_outcome.observation prepared in
          (source_preparation
           := match Source_stage_outcome.cleanup prepared with
              | Finalizer_escaped _ -> None
              | Not_entered | Reported _ ->
                (match Source_stage_outcome.selected_lease prepared with
                 | None -> None
                 | Some selected ->
                   let candidate =
                     match selected.stage with
                     | Some Lease_outcome.Primary_stage.Callback ->
                       Preparation.select
                         ~primary:selected.primary
                         ~candidates:[ !callback_preparation ]
                     | stage ->
                       if pre_callback_stage stage
                       then
                         Preparation.of_observation
                           ~stage:Source_admission
                           selected.primary
                       else None
                   in
                   Preparation.select ~primary:observed ~candidates:[ candidate ]));
          observed
      in
      let selected =
        Observed_result.raw (fun () ->
          let open Result.Let_syntax in
          with_owner_lock t ~protect:true (fun () ->
            let%bind () =
              if t.closed
              then Error (closed_error ())
              else if
                resource_retirement_blocks_admission t || prepared_source_blocks_runtime t
              then Error (background_busy ())
              else Ok ()
            in
            Result.of_option (native_identity_locked t) ~error:(background_busy ())))
      in
      selection_preparation := Preparation.of_observation ~stage:Selection selected;
      let%bind selected_native = selected in
      let lease_recovery = ref None in
      let callback_preparation = ref None in
      let returned = ref false in
      let outer =
        Observed_result.capture (fun () ->
          let report =
            with_lease_scope_callback_internal
              ?consumer:dependency_consumer
              ~lease_wrapper
              ~capture_recovery:(fun ~finished:_ recovery ->
                lease_recovery := Some recovery)
              t
              ~survives_stop:true
              ~purpose:(Lease_purpose.Selection.Native_resource selected_native)
              ~retire:(fun () -> consumer.closed <- true)
              ~admit:(fun () ->
                if
                  Option.exists
                    (native_identity_locked t)
                    ~f:
                      (Agent_session.Runtime_builder.Native_identity.equal
                         selected_native)
                  && (not t.closed)
                  && (not (resource_retirement_blocks_admission t))
                  && not (failed_source_cleanup_pending t)
                then Ok (Lease_admission.Borrow (selected_native, None, None))
                else Error (background_busy ()))
              (Lease_invocation.Observed
                 (fun ~recovery selected_native ->
                   let owned = ref None in
                   let primary =
                     Observed_result.capture (fun () ->
                       let open Observed_result in
                       let%bind loan, check, custody =
                         Observed_result.raw (fun () ->
                           let open Result.Let_syntax in
                           let%bind loan, actual =
                             Eio.Cancel.protect (fun () ->
                               let%bind loan =
                                 access.pin
                                   ~admission:consumer
                                   ~expected_native:selected_native
                               in
                               let actual = Native_loan.create loan in
                               let release = Native_release.create t actual in
                               with_owner_lock t ~protect:true (fun () ->
                                 t.native_loans <- actual :: t.native_loans;
                                 owned := Some (actual, release);
                                 Option.iter !lease_wrapper ~f:(fun lease ->
                                   Option.iter
                                     lease.Background_lease.relation
                                     ~f:(fun relation ->
                                       Background_lease.Relation.attach_pin
                                         relation
                                         actual));
                                 match !lease_recovery with
                                 | None ->
                                   invalid_arg
                                     "native pin has no actual lexical cleanup holder"
                                 | Some recovery ->
                                   Recovery_data.register_native_release
                                     recovery
                                     ~position:Lease_pin
                                     actual);
                               let%bind () =
                                 Option.value_map
                                   !lease_wrapper
                                   ~default:(Ok ())
                                   ~f:(fun lease ->
                                     Option.value_map
                                       lease.Background_lease.relation
                                       ~default:(Ok ())
                                       ~f:(fun relation ->
                                         check_relation_pin t relation actual))
                               in
                               capture_release release;
                               Ok (loan, actual))
                           in
                           let custody =
                             Native_resource_custody.create
                               t
                               actual
                               ~check_current:access.check_custody
                           in
                           let check () =
                             let%bind expected = A.state t.actor in
                             let%bind () =
                               Native_resource_admission.check consumer ~expected
                             in
                             let%bind () =
                               access.check loan ~admission:consumer ~expected
                             in
                             with_owner_lock t ~protect:true (fun () ->
                               if
                                 (not t.closed)
                                 && (not consumer.closed)
                                 && Option.exists !owned ~f:(fun (actual, _) ->
                                   List.mem t.native_loans actual ~equal:phys_equal)
                               then Ok ()
                               else Error (background_busy ()))
                           in
                           let%bind () = check () in
                           Eio.Fiber.check ();
                           Ok (loan, check, custody))
                       in
                       (* The existing generic Result CPS validates before f and
                       tail-returns it. Only DATA crosses its Ok wrapper. *)
                       let projected =
                         Observed_result.raw (fun () ->
                           project loan ~f:(fun resources ->
                             Ok
                               (Observed_result.bind
                                  (Observed_result.raw check)
                                  ~f:(fun () ->
                                    Lease_invocation.invoke
                                      callback
                                      ~recovery
                                      (check, custody, resources)))))
                       in
                       Observed_result.bind projected ~f:Fn.id)
                   in
                   callback_preparation
                   := Preparation.of_observation ~stage:Lease_callback primary;
                   (match !owned with
                    | None -> ()
                    | Some (actual, release) ->
                      let actual_close =
                        Observed_result.capture (fun () ->
                          close_native_loan_observed
                            ~recovery:(recovery, Lease_pin)
                            t
                            actual)
                      in
                      pin_observation := actual_close;
                      pin
                      := Native_borrow_outcome.Pin.Acquired
                           { release
                           ; cleanup =
                               Cleanup_projection.cleanup
                                 (projection actual_close ~acquired:true)
                           });
                   primary))
          in
          returned := true;
          outer_recovery := Some report.recovery;
          outer_lease := Scope_outcome.cleanup report.report;
          outer_lease_observation
          := Observed_result.with_cleanup
               (Observed_result.return ())
               (Recovery_data.original_projection report.recovery);
          (* Native's legacy primary excludes normally reported wrapper cleanup.
             The cleanup-only partition below owns that distinct actual leg. *)
          let selected =
            Observed_result.of_primary
              (Scope_outcome.primary report.report)
              ~diagnostics:(CO.diagnostics report.observation)
              ?primary:
                (match Scope_outcome.primary report.report with
                 | Returned (Ok _) -> None
                 | Returned (Error _) | Raised _ -> CO.primary report.observation)
              ()
          in
          (lease_preparation
           := match Lease_outcome.primary_stage report with
              | Some Lease_outcome.Primary_stage.Callback ->
                Preparation.select ~primary:selected ~candidates:[ !callback_preparation ]
              | stage ->
                if pre_callback_stage stage
                then Preparation.of_observation ~stage:Lease_admission selected
                else None);
          selected)
      in
      if not !returned
      then (
        outer_escape
        := Some
             (CO.create
                (match CO.exit outer with
                 | Raised (exn, trace) -> Raised (exn, trace)
                 | Returned (Error error) -> Returned (Error error)
                 | Returned (Ok _) ->
                   invalid_arg "native wrapper escaped without a negative outcome")
                ~diagnostics:(CO.diagnostics outer)
                ?primary:(CO.primary outer)
                ());
        (* An escape from the lease engine itself is an actual bracket exit,
           not absence of a legacy report or an invented successful cleanup. *)
        let prior =
          Option.value_map
            !lease_recovery
            ~default:Cleanup_projection.empty
            ~f:Recovery_data.original_projection
        in
        let diagnostics =
          Option.value_map (Cleanup_projection.origin prior) ~default:[] ~f:(fun origin ->
            origin.Physical_origin.diagnostics)
          @ CO.diagnostics outer
        in
        let exit =
          match CO.exit outer with
          | Raised (exn, trace) -> CO.Raised (exn, trace)
          | Returned (Error error) -> CO.Returned (Error error)
          | Returned (Ok _) ->
            invalid_arg "native wrapper escaped without a negative outcome"
        in
        outer_lease_observation
        := CO.create exit ~diagnostics ?primary:(CO.primary outer) ());
      outer)
  in
  Option.iter !admission ~f:(fun admission -> admission.closed <- true);
  let cleanup_projection =
    Cleanup_projection.combine
      (projection !preparation_observation ~acquired:(Option.is_some !preparation))
      (Cleanup_projection.combine
         (projection
            !pin_observation
            ~acquired:
              (match !pin with
               | Not_acquired -> false
               | Acquired _ -> true))
         (projection !outer_lease_observation ~acquired:false))
  in
  let cleanup_observation =
    Observed_result.with_cleanup (Observed_result.return ()) cleanup_projection
  in
  Option.iter !lease_wrapper ~f:(fun lease ->
    Option.iter lease.Background_lease.relation ~f:(fun relation ->
      Relation_evidence.seal_native
        relation.Background_lease.Relation.report
        ~observation:cleanup_observation
        ~outer_escape:!outer_escape));
  let provider_report =
    Option.bind !lease_wrapper ~f:(fun lease ->
      Option.map lease.Background_lease.relation ~f:(fun _ ->
        { Native_dependency_report.lease }))
  in
  let outcome =
    { Native_borrow_outcome.primary = Physical_origin.primary primary_observation
    ; preadmission = !preadmission
    ; preparation_failure =
        Preparation.select
          ~primary:primary_observation
          ~candidates:
            [ !admission_preparation
            ; !source_preparation
            ; !selection_preparation
            ; !lease_preparation
            ]
    ; cleanup =
        { Native_borrow_outcome.Cleanup.preparation_report = !preparation
        ; pin = !pin
        ; outer_lease = !outer_lease
        ; preparation_observation = !preparation_observation
        ; outer_lease_observation = !outer_lease_observation
        ; observation = cleanup_observation
        ; provider_report
        ; outer_recovery = !outer_recovery
        }
    ; observation = Observed_result.with_cleanup primary_observation cleanup_projection
    }
  in
  (* Actual Cleanup now owns the sealed native report before provider metadata
     can be removed; the captured release shares this SAME report for repair. *)
  Option.iter outcome.cleanup.provider_report ~f:(fun report ->
    (match outcome.cleanup.pin with
     | Not_acquired -> ()
     | Acquired { release; _ } -> Native_release.attach_report release report);
    handoff_completed_dependency t report.lease);
  outcome
;;

let with_native_resources_projected_outcome
      ?consumer
      t
      ~source_owner
      ~reference
      ~authorize
      ~capture_release
      ~project
      ~f
  =
  with_native_resources_projected_callback_outcome
    ?consumer
    t
    ~source_owner
    ~origin:
      (Native_resource_origin.create
         ~source_owner
         ~route:(Direct reference)
         ~authorize:(fun ~route:_ ~target ->
           Result.map (authorize ~reference ~target) ~f:(fun () ->
             Native_resource_origin.Validation.Direct)))
    ~capture_release
    ~project
    (Lease_invocation.Raw
       (fun ~recovery:_ (check, _custody, resources) -> f ~check resources))
;;

let with_native_resources_observed
      ?consumer
      t
      ~source_owner
      ~reference
      ~authorize
      ~capture_release
      ~f
  =
  with_native_resources_projected_callback_outcome
    ?consumer
    t
    ~source_owner
    ~origin:
      (Native_resource_origin.create
         ~source_owner
         ~route:(Direct reference)
         ~authorize:(fun ~route:_ ~target ->
           Result.map (authorize ~reference ~target) ~f:(fun () ->
             Native_resource_origin.Validation.Direct)))
    ~capture_release
    ~project:Session_policy_resource.Native_lease.with_resources
    (Lease_invocation.Observed
       (fun ~recovery:_ (check, _custody, resources) -> f ~check resources))
;;

let with_native_source_resources_observed
      ?consumer
      t
      ~source_owner
      ~reference
      ~authorize
      ~capture_release
      ~f
  =
  with_native_resources_projected_callback_outcome
    ?consumer
    t
    ~source_owner
    ~origin:
      (Native_resource_origin.create
         ~source_owner
         ~route:(Direct reference)
         ~authorize:(fun ~route:_ ~target ->
           Result.map (authorize ~reference ~target) ~f:(fun () ->
             Native_resource_origin.Validation.Direct)))
    ~capture_release
    ~project:(fun loan ~f ->
      Session_policy_resource.Native_lease.with_source_resources
        loan
        ~f:(fun ~source resources -> f (source, resources)))
    (Lease_invocation.Observed
       (fun ~recovery:_ (check, _custody, (source, resources)) ->
         f ~check ~source resources))
;;

let with_native_source_resources_for_origin_observed
      ?consumer
      t
      ~source_owner
      ~origin
      ~capture_release
      ~f
  =
  with_native_resources_projected_callback_outcome
    ?consumer
    t
    ~source_owner
    ~origin
    ~capture_release
    ~project:(fun loan ~f ->
      Session_policy_resource.Native_lease.with_source_resources
        loan
        ~f:(fun ~source resources -> f (source, resources)))
    (Lease_invocation.Observed
       (fun ~recovery:_ (check, custody, (source, resources)) ->
         f ~check ~custody ~source resources))
;;

let with_native_resources_outcome
      ?consumer
      t
      ~source_owner
      ~reference
      ~authorize
      ~capture_release
      ~f
  =
  with_native_resources_projected_outcome
    ?consumer
    t
    ~source_owner
    ~reference
    ~authorize
    ~capture_release
    ~project:Session_policy_resource.Native_lease.with_resources
    ~f
;;

let with_native_source_resources_outcome
      ?consumer
      t
      ~source_owner
      ~reference
      ~authorize
      ~capture_release
      ~f
  =
  with_native_resources_projected_outcome
    ?consumer
    t
    ~source_owner
    ~reference
    ~authorize
    ~capture_release
    ~project:(fun loan ~f ->
      Session_policy_resource.Native_lease.with_source_resources
        loan
        ~f:(fun ~source resources -> f (source, resources)))
    ~f:(fun ~check (source, resources) -> f ~check ~source resources)
;;

let with_native_resources_reported t ~source_owner ~reference ~authorize ~f =
  with_native_resources_outcome
    t
    ~source_owner
    ~reference
    ~authorize
    ~capture_release:ignore
    ~f
  |> Native_borrow_outcome.to_scope_outcome
;;

let with_native_source_resources_reported t ~source_owner ~reference ~authorize ~f =
  with_native_source_resources_outcome
    t
    ~source_owner
    ~reference
    ~authorize
    ~capture_release:ignore
    ~f
  |> Native_borrow_outcome.to_scope_outcome
;;

let with_native_resources t ~source_owner ~reference ~authorize ~f =
  with_native_resources_reported t ~source_owner ~reference ~authorize ~f
  |> Scope_outcome.resume
;;

let with_background_runtime t f = with_runtime_lease t ~retain_resources:false f

let with_background_runtime_activation t f =
  with_runtime_lease_internal t ~retain_resources:false (fun ~recovery runtime ->
    let activate =
      Option.map
        runtime.Agent_session.Runtime_builder.moderator_activation
        ~f:(fun activation () ->
          let%bind.Result () = Private_lease_callbacks.check_open recovery in
          activation.run ~capture_cleanup:(Private_lease_callbacks.capture recovery) ())
    in
    f runtime ~activate)
;;

let with_execution_runtime t ~owner f =
  let open Result.Let_syntax in
  let%bind selected =
    with_owner_lock t ~protect:true (fun () ->
      if not (prepared_source_blocks_runtime t)
      then Ok None
      else (
        match t.prepared_source, loaded_runtime t with
        | Some stage, Some runtime when not stage.closed ->
          (match stage.continuation, stage.handoff_runtime with
           | Some continuation, Some installed when phys_equal installed runtime ->
             Ok (Some (stage, runtime, continuation))
           | _ -> Error (background_busy ()))
        | _ -> Error (background_busy ())))
  in
  match selected with
  | None -> with_background_runtime t f
  | Some (stage, runtime, continuation) ->
    (* Selection can refresh the Actor's Store and must stay outside our mutex.
       The resulting token is a pure, exact execution-borrow check; identifiers
       supplied by a caller do not themselves authorize access. *)
    let%bind borrow =
      Agent_session.Session_actor.runtime_construction_resource
        t.actor
        ~construction:continuation
        ~owner
    in
    let execution_access : Execution_access.t = { stage; runtime; borrow } in
    with_runtime_lease ~execution_access t ~retain_resources:false f
;;

module Loaded_job_runtime = struct
  type 'a t =
    | No_runtime
    | Reused of 'a
end

let with_loaded_goal_job_runtime t ~owner f =
  let module Actor = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let completed_retirement = completed_unloaded_retirement t in
  let%bind selected =
    with_owner_lock t ~protect:true (fun () ->
      forget_completed_retirement_locked t completed_retirement;
      if resource_retirement_pending t
      then Error (background_busy ())
      else (
        match t.closed, t.unloading, t.foreground_retirement, loaded_runtime t with
        | true, _, _, _ -> Error (closed_error ())
        | false, Some _, _, _ | false, None, Some _, _ -> Error (background_busy ())
        | false, None, None, runtime ->
          if prepared_source_blocks_runtime t
          then Error (background_busy ())
          else Ok runtime))
  in
  match selected with
  | None -> Ok Loaded_job_runtime.No_runtime
  | Some runtime ->
    let%bind borrow =
      Actor.job_runtime_resource
        t.actor
        ~owner
        ~worker:runtime.worker
        ~inference:runtime.inference_execution
        ~moderator_manager:runtime.moderator_manager
    in
    let%bind result =
      with_lease
        t
        ~survives_stop:false
        ~admit:(fun () ->
          let%bind () = loaded_runtime_basis_locked t runtime in
          let%map () = Actor.Runtime_construction_resource.check_basis borrow in
          runtime, Some runtime)
        (fun runtime ->
           (* The physical cancellation lease protects resources while delegated
             authority may perform IO. No callback or Actor RPC runs under the
             Owner mutex, and no runtime escapes before the final exact proof. *)
           let%bind () = check_runtime_execution runtime in
           Eio.Fiber.check ();
           let%bind current =
             Actor.job_runtime_resource
               t.actor
               ~owner
               ~worker:runtime.worker
               ~inference:runtime.inference_execution
               ~moderator_manager:runtime.moderator_manager
           in
           let%bind () =
             with_owner_lock t ~protect:true (fun () ->
               let%bind () = loaded_runtime_basis_locked t runtime in
               Actor.Runtime_construction_resource.check_basis current)
           in
           Eio.Fiber.check ();
           f runtime)
    in
    Ok (Loaded_job_runtime.Reused result)
;;

let with_prepared_run_runtime t ~preparation f =
  let open Result.Let_syntax in
  let%bind () =
    Agent_session.Session_actor.check_prepared_run_execution t.actor ~preparation
  in
  with_runtime_lease ~run_preparation:preparation t ~retain_resources:false f
;;

let with_delegation_resources_outcome t ~capture f =
  let _, scope =
    with_runtime_lease_scope_internal
      ~capture_recovery:(fun ~finished:_ recovery -> capture recovery)
      t
      ~retain_resources:true
      (fun ~recovery:_ runtime -> f runtime)
  in
  scope
;;

let with_delegation_resources_observed t ~capture f =
  let _, scope =
    with_runtime_lease_callback_internal
      ~capture_recovery:(fun ~finished:_ recovery -> capture recovery)
      t
      ~retain_resources:true
      (Lease_invocation.Observed (fun ~recovery:_ runtime -> f runtime))
  in
  scope
;;

let with_background_runtime_observed t ~capture f =
  let _, scope =
    with_runtime_lease_callback_internal
      ~capture_recovery:(fun ~finished:_ recovery -> capture recovery)
      t
      ~retain_resources:false
      (Lease_invocation.Observed (fun ~recovery:_ runtime -> f runtime))
  in
  scope
;;

let with_delegation_resources t f =
  with_delegation_resources_outcome t ~capture:ignore f
  |> Lease_outcome.outcome
  |> Scope_outcome.resume
;;

let with_auxiliary_execution_lifetime t f =
  with_lease
    t
    ~survives_stop:false
    ~admit:(fun () ->
      match t.closed, t.unloading with
      | true, _ -> Error (closed_error ())
      | false, Some _ -> Error (background_busy ())
      | false, None ->
        Ok
          ( Option.map
              (loaded_runtime t)
              ~f:Agent_session.Runtime_builder.inference_execution
          , loaded_runtime t ))
    f
;;

let with_resource_lifetime_outcome ?consumer t ~capture f =
  with_lease_scope_reported_internal
    ?consumer
    ~capture_recovery:(fun ~finished:_ recovery -> capture recovery)
    ~before_admission:(fun _ ->
      Agent_session.Session_actor.exclude_effect_deferred_execution t.actor)
    t
    ~survives_stop:true
    ~admit:(fun () ->
      match t.closed, t.unloading with
      | true, _ -> Error (closed_error ())
      | false, Some _ -> Error (background_busy ())
      | false, None -> Ok (Lease_admission.Borrow ((), None, None)))
    (fun ~recovery:_ value -> f value)
;;

let with_resource_lifetime_observed ?consumer t ~capture f =
  with_lease_scope_callback_internal
    ?consumer
    ~capture_recovery:(fun ~finished:_ recovery -> capture recovery)
    ~before_admission:(fun _ ->
      Observed_result.raw (fun () ->
        Agent_session.Session_actor.exclude_effect_deferred_execution t.actor))
    t
    ~survives_stop:true
    ~admit:(fun () ->
      match t.closed, t.unloading with
      | true, _ -> Error (closed_error ())
      | false, Some _ -> Error (background_busy ())
      | false, None -> Ok (Lease_admission.Borrow ((), None, None)))
    (Lease_invocation.Observed (fun ~recovery:_ value -> f value))
;;

let with_resource_lifetime t f =
  with_resource_lifetime_outcome t ~capture:ignore f
  |> Lease_outcome.outcome
  |> Scope_outcome.resume
;;

module Policy_resource_selection = struct
  type t =
    | Retained of Resource_handoff.t
    | Restored of Restored_policy_anchor.t

  let runtime = function
    | Retained handoff -> Resource_handoff.runtime handoff
    | Restored anchor -> Restored_policy_anchor.runtime anchor
  ;;

  let product = function
    | Retained handoff -> Resource_handoff.product handoff
    | Restored anchor -> Restored_policy_anchor.product anchor
  ;;

  let running t (expected : Agent_session.Session_state.t) =
    match t with
    | Retained _ ->
      Agent_protocol.Session.equal_desired_state expected.lifecycle.desired Running
    | Restored _ -> false
  ;;
end

module Policy_runtime_selection = struct
  type t =
    { resource : Policy_resource_selection.t
    ; wrapper : background_lease
    ; capture : Agent_session.Session_actor.Delegated_policy_claim.Capture.t option ref
    }

  let runtime t = Policy_resource_selection.runtime t.resource
  let product t = Policy_resource_selection.product t.resource
  let running t expected = Policy_resource_selection.running t.resource expected

  module Access = struct
    type t =
      | Existing of Agent_session.Runtime_builder.Policy_access.t
      | Restored of Agent_session.Session_actor.Policy_restoration.Published.t
  end
end

let policy_resource_current_locked t resource ~running =
  let binding_current =
    match resource with
    | Policy_resource_selection.Restored anchor ->
      Option.is_none t.binding
      && Option.exists t.restored_policy ~f:(phys_equal anchor)
      && Result.is_ok (Restored_policy_anchor.available anchor)
    | Retained handoff ->
      (match t.binding with
       | Some binding -> phys_equal binding handoff.binding
       | None ->
         (not running)
         && List.exists t.retired_resources ~f:(fun entry ->
           phys_equal entry.binding handoff.binding
           &&
           match entry.phase with
           | Retained None -> true
           | Retained (Some _) | Closing _ | Closed -> false))
  in
  if
    t.closed
    || resource_retirement_blocks_admission t
    || administration_blocks t
    || prepared_source_blocks_runtime t
    || not binding_current
  then Error (background_busy ())
  else Ok ()
;;

let policy_selection_current_locked t (selected : Policy_runtime_selection.t) ~running =
  let open Result.Let_syntax in
  let%bind () = policy_resource_current_locked t selected.resource ~running in
  let same_product product =
    Agent_session.Runtime_builder.Product_identity.equal
      product
      (Policy_runtime_selection.product selected)
  in
  let other_leases_valid =
    running
    || List.for_all t.background_leases ~f:(fun lease ->
      (not (Background_lease.is_active lease))
      || phys_equal lease selected.wrapper
      ||
      match lease.purpose with
      | Shared_resource product -> same_product product
      | Shared_native identity ->
        (match selected.resource with
         | Retained handoff ->
           Option.exists
             (Runtime_binding.native_identity handoff.binding)
             ~f:(Agent_session.Runtime_builder.Native_identity.equal identity)
         | Restored anchor ->
           Agent_session.Runtime_builder.Native_identity.equal
             identity
             (Agent_session.Runtime_builder.Product.native_identity
                (Session_policy_resource.Lease.product anchor.loan)))
      | Exclusive -> false)
  in
  if
    (not other_leases_valid)
    || (not (Background_lease.is_active selected.wrapper))
    || not (List.mem t.background_leases selected.wrapper ~equal:phys_equal)
  then Error (background_busy ())
  else Ok ()
;;

(* Record ACTUAL finalizers before projecting only their original primary.
   A cleanup-only failure remains cleanup when the enclosing callback joins. *)
let policy_report_result cleanup report =
  cleanup := Scope_outcome.Cleanup.combine !cleanup (Scope_outcome.cleanup report);
  match Scope_outcome.primary report with
  | Returned result -> result
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let policy_scope_report f =
  let cleanup = ref Scope_outcome.Cleanup.Never_acquired in
  let primary = Scope_outcome.capture (fun () -> f cleanup) in
  { Scope_outcome.primary; cleanup = !cleanup }
;;

let policy_resource_admission_locked t =
  if failed_source_cleanup_pending t
  then Error (background_busy ())
  else if t.closed
  then Error (closed_error ())
  else if
    resource_retirement_blocks_admission t
    || administration_blocks t
    || factual_cleanup_pending t
    || retired_cleanup_pending t
  then Error (retired_cleanup_error ())
  else Ok ()
;;

let select_retained_policy_resource_locked t =
  let open Result.Let_syntax in
  let%bind handoff =
    match t.binding with
    | Some binding -> Ok (Resource_handoff.of_binding binding)
    | None -> select_resource_handoff_locked t
  in
  let%map handoff =
    Result.of_option
      handoff
      ~error:
        (Agent_protocol.Error.invalid_request
           "expected retained parent policy resource is unavailable")
  in
  Policy_resource_selection.Retained handoff
;;

let prepare_policy_access t (selected : Policy_runtime_selection.t) loan =
  let module A = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let%bind port =
    Result.of_option
      t.policy_resource_access
      ~error:
        (Agent_protocol.Error.invalid_request "parent has no actual policy resource port")
  in
  let%bind expected = A.state t.actor in
  let running = Policy_runtime_selection.running selected expected in
  let current () =
    with_owner_lock t ~protect:true (fun () ->
      policy_selection_current_locked t selected ~running)
  in
  let%bind () = current () in
  match selected.resource with
  | Retained _ ->
    let%bind access = port.Policy_resource_access.prepare_access loan ~expected in
    let%bind () = current () in
    let%map () = Agent_session.Runtime_builder.Policy_access.check access ~expected in
    expected, Policy_runtime_selection.Access.Existing access
  | Restored anchor ->
    let%bind restored =
      Result.of_option
        port.restoration
        ~error:
          (Agent_protocol.Error.invalid_request "parent has no restored policy checks")
    in
    let%bind () = restored.check_restored loan ~access:anchor.access ~expected in
    let%bind published = Restored_policy_anchor.available anchor in
    let%map () = current () in
    expected, Policy_runtime_selection.Access.Restored published
;;

(* One pin/cleanup primitive for BOTH actual resource kinds. The specialized
   resource purpose is derived only from a selected actual Product below;
   execution/legacy observation continue to use the default Exclusive wrapper. *)
let with_policy_resource_pin_reported
      ?(purpose = Lease_purpose.Selection.Exclusive)
      t
      ~settle
      ~select
      f
  =
  let module A = Agent_session.Session_actor in
  let open Result.Let_syntax in
  policy_scope_report (fun cleanup ->
    let%bind port =
      Result.of_option
        t.policy_resource_access
        ~error:
          (Agent_protocol.Error.invalid_request
             "parent has no actual policy resource port")
    in
    let%bind () = if settle then settle_admission_inference_usage t else Ok () in
    let wrapper = ref None in
    let capture = ref None in
    let report =
      with_lease_reported_internal
        ~lease_wrapper:wrapper
        ~purpose
        ~retire:(fun () ->
          Option.iter !capture ~f:A.Delegated_policy_claim.Capture.cancel_waiting)
        t
        ~survives_stop:false
        ~admit:(fun () ->
          let%bind () = policy_resource_admission_locked t in
          let%map resource = select () in
          Lease_admission.Borrow
            (resource, Some (Policy_resource_selection.runtime resource), None))
        (fun resource ->
           (* Registered Owner custody precedes this nonyielding pin and every
              Actor/profile/source await. No selected failure can reselect. *)
           let%bind loan =
             port.pin ~expected_product:(Policy_resource_selection.product resource)
           in
           let published_holder = ref None in
           let primary =
             Scope_outcome.capture (fun () ->
               let%bind wrapper =
                 Result.of_option
                   !wrapper
                   ~error:
                     (Agent_protocol.Error.invalid_request
                        "policy wrapper lease was not admitted")
               in
               let selected : Policy_runtime_selection.t =
                 { resource; wrapper; capture }
               in
               f selected loan published_holder)
           in
           let released =
             match !published_holder with
             | Some _ -> Scope_outcome.Cleanup.Joined
             | None ->
               let closed =
                 try Ok (Session_policy_resource.Lease.close loan) with
                 | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
               in
               Scope_outcome.Cleanup.of_result ~acquired:true closed
           in
           policy_report_result cleanup { Scope_outcome.primary; cleanup = released })
    in
    policy_report_result cleanup report)
;;

let with_policy_runtime t f =
  with_policy_resource_pin_reported
    t
    ~settle:true
    ~select:(fun () -> select_retained_policy_resource_locked t)
    (fun selected loan published ->
       let open Result.Let_syntax in
       let%bind expected, access = prepare_policy_access t selected loan in
       f selected loan published expected access)
  |> Scope_outcome.resume
;;

let ensure_policy_restored_reported t ~request ~governance ~authorize =
  let open Result.Let_syntax in
  policy_scope_report (fun cleanup ->
    let%bind needed =
      with_owner_lock t ~protect:true (fun () ->
        if t.closed
        then Error (closed_error ())
        else if
          Option.is_some t.binding
          || (not (List.is_empty t.retired_resources))
          || Option.is_some t.restored_policy
        then Ok false
        else if
          resource_retirement_blocks_admission t
          || prepared_source_blocks_runtime t
          || factual_cleanup_pending t
          || policy_cleanup_pending t
        then Error (retired_cleanup_error ())
        else Ok true)
    in
    if not needed
    then Ok ()
    else (
      let%bind restoration =
        Result.of_option
          (Option.bind t.policy_resource_access ~f:(fun port -> port.restoration))
          ~error:
            (Agent_protocol.Error.invalid_request "parent has no policy restoration port")
      in
      let report =
        with_source_stage_reported t (fun stage ->
          let%bind () = authorize () in
          let%bind () =
            Agent_session.Session_actor.Causal_governance.check_current governance
          in
          let%bind consumer = Source_consumer.create stage in
          let source =
            policy_restoration_source t stage ~consumer ~request ~governance ~authorize
          in
          let%bind () =
            restoration.restore ~stage:source ~request ~governance ~authorize
          in
          let%bind () = authorize () in
          let%bind () = Policy_restoration_source.check source in
          match t.restored_policy with
          | Some anchor -> Result.map (Restored_policy_anchor.available anchor) ~f:ignore
          | None ->
            Error
              (Agent_protocol.Error.invalid_request
                 "policy restoration did not publish its owned resource"))
      in
      policy_report_result cleanup report))
;;

module Delegated_policy_resource_data = struct
  type owner = t

  type t =
    { owner : owner
    ; resource : Policy_resource_selection.t
    ; source : (Agent_session.Runtime_builder.Source.t, Agent_protocol.Error.t) result
    ; wrapper : background_lease
    ; request : Agent_session.Script_tool_calls.preparation
    ; governance : Agent_session.Session_actor.Causal_governance.t
    ; authorize : unit -> (unit, Agent_protocol.Error.t) result
    ; mutable active : bool
    }
end

let delegated_policy_resource_current_locked (resource : Delegated_policy_resource_data.t)
  =
  let open Result.Let_syntax in
  let%bind () = if resource.active then Ok () else Error (background_busy ()) in
  let%bind () =
    policy_resource_current_locked resource.owner resource.resource ~running:false
  in
  if
    Background_lease.is_active resource.wrapper
    && List.mem resource.owner.background_leases resource.wrapper ~equal:phys_equal
  then Ok ()
  else Error (background_busy ())
;;

let check_delegated_policy_resource (resource : Delegated_policy_resource_data.t) =
  if not (Int.equal resource.owner.domain (Stdlib.Domain.self () :> int))
  then
    Error
      (Agent_protocol.Error.invalid_request
         "delegated policy resource belongs to another Eio domain")
  else
    with_owner_lock resource.owner ~protect:true (fun () ->
      delegated_policy_resource_current_locked resource)
;;

let retained_moderation_source t =
  with_policy_runtime t (fun selected _loan _published expected _access ->
    let open Result.Let_syntax in
    let%bind () =
      match expected.Agent_session.Session_state.lifecycle.desired with
      | Stopped -> Ok ()
      | Running -> Error (background_busy ())
    in
    Agent_session.Runtime_builder.moderation_source
      (Policy_runtime_selection.runtime selected))
;;

let moderation_source t =
  with_background_runtime t Agent_session.Runtime_builder.moderation_source
;;

let run_delegated_policy_reported
      t
      selected
      loan
      published
      expected
      access
      ~request
      ~governance
      ~delegation
      ~event
      ~authorize
  =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let open Result.Let_syntax in
  policy_scope_report (fun cleanup ->
    let%bind () = A.Causal_governance.check_current governance in
    let runtime = Policy_runtime_selection.runtime selected in
    let running = Policy_runtime_selection.running selected expected in
    let unavailable () =
      Error
        (Agent_protocol.Error.create
           Invalid_state
           ~message:
             "delegation.owner_mediation_unavailable: parent policy runtime is \
              unavailable"
           ~retryable:false
           ())
    in
    let%bind () =
      match event with
      | Chat_response.Moderation.Event.Pre_tool_call _ -> Ok ()
      | _ ->
        Error
          (Agent_protocol.Error.invalid_request
             "delegated policy requires exact Pre_tool_call")
    in
    let%bind () = A.exclude_effect_deferred_execution t.actor in
    let%bind capture =
      A.Delegated_policy_claim.Capture.create
        t.actor
        ~access:
          (match access with
           | Policy_runtime_selection.Access.Existing access ->
             if running then Running else Policy_only access
           | Restored published -> Restored_unloaded published)
    in
    selected.capture := Some capture;
    let primary =
      Scope_outcome.capture (fun () ->
        A.Delegated_policy_claim.Capture.with_extent capture (fun () ->
          (* Bracket starts before this registration can await and before the
           Manager's pre-claim identity_snapshot can wait on its execution lock. *)
          let%bind () =
            with_owner_lock t ~protect:true (fun () ->
              let%bind () = policy_selection_current_locked t selected ~running in
              match t.policy_claim with
              | Some _ -> Error (retired_cleanup_error ())
              | None ->
                let holder : native_consumer Policy_claim_holder.t =
                  { capture
                  ; loan
                  ; runtime
                  ; wrapper = selected.wrapper
                  ; natural_release = Policy_claim_release.create ()
                  }
                in
                t.policy_claim <- Some holder;
                published := Some holder;
                Ok ())
          in
          match runtime.Agent_session.Runtime_builder.moderator_manager with
          | None -> unavailable ()
          | Some manager when Option.is_none (M.extension_definition manager) ->
            unavailable ()
          | Some manager ->
            let script_tools =
              if running
              then runtime.moderator_script_tools
              else
                Option.map runtime.moderator_script_tools ~f:(fun tools ->
                  Agent_session.Script_tool_calls.with_lifecycle
                    tools
                    ~is_halted:(fun () ->
                      Result.is_error
                        (A.Delegated_policy_claim.Capture.check_policy_effect_current
                           t.actor
                           capture)))
            in
            let history = ref [] in
            let claim ~notifications ~snapshot handle =
              A.with_delegated_moderator_event
                ~notifications
                t.actor
                ~capture
                ~request
                ~governance
                ~delegation
                ~event
                ~authorize
                ~snapshot
                (fun
                    ~executing
                     ~policy_caller
                     ~model_jobs
                     ~allocate_inference
                     ~event
                     ~execute
                     ~commit
                   ->
                   let%bind state = A.state t.actor in
                   let%bind entries =
                     Agent_session.History_codec.all_of_protocol
                       state.conversation.canonical_history
                   in
                   history := entries;
                   handle
                     ~executing
                     ~policy_caller
                     ~model_jobs
                     ~allocate_inference
                     ~event
                     ~execute
                     ~commit)
            in
            Agent_session.Moderator_event.run_delegated
              ~model_recipes:runtime.model_recipes
              ~event
              ~claim
              ~run_actions:(A.run_actions t.actor)
              ?script_tools
              ~manager
              ~history:(fun () -> !history)
              ~available_tools:runtime.moderator_tools
              ~session_meta:`Null
              ~now:runtime.now
              ()))
    in
    (* No selected-runtime callback/closure is used after the actual bracket
       joins. Exact claim cleanup precedes any separate Actor-owned follow-up. *)
    let cleaned =
      try
        Ok
          (Eio.Cancel.protect (fun () ->
             Option.value_map !published ~default:(Ok ()) ~f:(drain_policy_holder t)))
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    let follow_up =
      try
        Ok
          (match cleaned, primary with
           | Ok (Ok ()), Returned _ when running ->
             A.apply_moderator_follow_up t.actor |> Result.map ~f:ignore
           | (Ok (Ok ()) | Ok (Error _) | Error _), (Returned _ | Raised _) -> Ok ())
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    cleanup
    := Scope_outcome.Cleanup.combine
         (Scope_outcome.Cleanup.of_result ~acquired:true cleaned)
         (Scope_outcome.Cleanup.of_result ~acquired:true follow_up);
    let%bind result =
      match primary with
      | Returned result -> result
      | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
    in
    match result with
    | Some { receipt = { decision = Some _; _ } as receipt; _ } -> Ok receipt
    | None | Some _ ->
      (* No actual receipt value exists. Preserve the original no-value boundary:
         holder/follow-up cleanup must succeed before unavailable is projected.
         Valid receipts above keep primary-only reported composition. *)
      let%bind () =
        Scope_outcome.resume { primary = Returned (Ok ()); cleanup = !cleanup }
      in
      unavailable ())
;;

module Delegated_policy_resource = struct
  type t = Delegated_policy_resource_data.t

  let check_current = check_delegated_policy_resource

  let check_source resource ~source =
    let open Result.Let_syntax in
    let%bind () = check_current resource in
    let%bind selected_source = resource.Delegated_policy_resource_data.source in
    if phys_equal selected_source source
    then Ok ()
    else
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"delegated policy resource selected Source changed"
           ~retryable:false
           ())
  ;;

  let evaluate_reported resource ~delegation ~event ~authorize =
    let open Result.Let_syntax in
    policy_scope_report (fun cleanup ->
      let%bind () = check_current resource in
      let%bind () =
        Agent_session.Session_actor.Causal_governance.check_current resource.governance
      in
      let select () =
        let%map () = delegated_policy_resource_current_locked resource in
        resource.resource
      in
      let report =
        with_policy_resource_pin_reported
          resource.owner
          ~settle:
            (match resource.resource with
             | Retained _ -> true
             | Restored _ -> false)
          ~select
          (fun selected loan published ->
             let%bind expected, access =
               prepare_policy_access resource.owner selected loan
             in
             let report =
               run_delegated_policy_reported
                 resource.owner
                 selected
                 loan
                 published
                 expected
                 access
                 ~request:resource.request
                 ~governance:resource.governance
                 ~delegation
                 ~event
                 ~authorize:(fun () ->
                   let%bind () = resource.authorize () in
                   authorize ())
             in
             policy_report_result cleanup report)
      in
      policy_report_result cleanup report)
  ;;
end

let with_delegated_policy_resource_reported
      ?authorize_restoration
      t
      ~request
      ~governance
      ~authorize
      ~f
  =
  let open Result.Let_syntax in
  policy_scope_report (fun cleanup ->
    let%bind () =
      if Int.equal t.domain (Stdlib.Domain.self () :> int)
      then Agent_session.Session_actor.Causal_governance.check_current governance
      else
        Error
          (Agent_protocol.Error.invalid_request
             "delegated policy owner belongs to another Eio domain")
    in
    let%bind () = settle_admission_inference_usage t in
    let authorize_restoration = Option.value authorize_restoration ~default:authorize in
    let restored =
      ensure_policy_restored_reported
        t
        ~request
        ~governance
        ~authorize:authorize_restoration
    in
    cleanup := Scope_outcome.Cleanup.combine !cleanup (Scope_outcome.cleanup restored);
    (* This unit preflight must finish cleanup before a generic callback can
       produce its value. A failed preparation never exposes a resource. *)
    let%bind () = Scope_outcome.resume restored in
    let%bind selected =
      with_owner_lock t ~protect:true (fun () ->
        let%bind () = policy_resource_admission_locked t in
        if Option.is_none t.binding && Option.is_some t.restored_policy
        then (
          let%bind anchor =
            Result.of_option t.restored_policy ~error:(background_busy ())
          in
          let%map _ = Restored_policy_anchor.available anchor in
          Policy_resource_selection.Restored anchor)
        else select_retained_policy_resource_locked t)
    in
    let report =
      with_policy_resource_pin_reported
        ~purpose:
          (Lease_purpose.Selection.Policy_resource
             (Policy_resource_selection.product selected))
        t
        ~settle:
          (match selected with
           | Retained _ -> true
           | Restored _ -> false)
        ~select:(fun () ->
          let%map () = policy_resource_current_locked t selected ~running:false in
          selected)
        (fun selected loan _published ->
           (* Capture the actual optional-selection result without strengthening
              existing resource admission. Only check_source requires it. *)
           let source =
             Agent_session.Runtime_builder.Product.selected_source
               (Session_policy_resource.Lease.product loan)
           in
           let resource : Delegated_policy_resource_data.t =
             { owner = t
             ; resource = selected.resource
             ; source
             ; wrapper = selected.wrapper
             ; request
             ; governance
             ; authorize
             ; active = true
             }
           in
           Exn.protect
             ~finally:(fun () -> resource.active <- false)
             ~f:(fun () -> f resource))
    in
    policy_report_result cleanup report)
;;

let prepare_delegated_tool t ~request ~governance ~delegation ~event ~authorize =
  let cleanup = ref Scope_outcome.Cleanup.Never_acquired in
  let report =
    with_delegated_policy_resource_reported
      t
      ~request
      ~governance
      ~authorize
      ~f:(fun resource ->
        let report =
          Delegated_policy_resource.evaluate_reported
            resource
            ~delegation
            ~event
            ~authorize
        in
        let open Result.Let_syntax in
        let%bind receipt = policy_report_result cleanup report in
        match receipt.Agent_protocol.Moderator_execution.decision with
        | Some decision -> Ok decision
        | None ->
          Error
            (Agent_protocol.Error.create
               Invalid_state
               ~message:
                 "delegation.owner_mediation_unavailable: parent policy runtime is \
                  unavailable"
               ~retryable:false
               ()))
  in
  Scope_outcome.prepend_cleanup report ~cleanup:!cleanup |> Scope_outcome.resume
;;

let ensure_loaded_with ?capture_cleanup ?original_actor t =
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_actor.exclude_effect_deferred_execution t.actor in
  match t.delegated_driver with
  | None ->
    let%bind ordinary = prepare_ordinary_load ?original_actor t in
    let cleanup = Cleanup_request.create () in
    with_construction_cleanup_request ?capture_cleanup t cleanup (fun () ->
      with_owner_lock t ~protect:true (fun () ->
        Exn.protect
          ~finally:(fun () -> retain_construction_request_locked t cleanup)
          ~f:(fun () ->
            admit_ordinary_preflight_locked ?original_actor t ~cleanup ordinary)))
  | Some _ ->
    let%bind () = check_original_actor original_actor in
    let%bind () = prepare_delegated_load ?original_actor t in
    with_loaded_runtime t (fun _ -> check_original_actor original_actor)
;;

let ensure_loaded ?capture_cleanup t = ensure_loaded_with ?capture_cleanup t

let ensure_loaded_authenticated ?capture_cleanup t ~actor =
  ensure_loaded_with ?capture_cleanup ~original_actor:actor t
;;

module Held_input_runtime = struct
  module Phase = struct
    type t =
      | Available
      | Submitted
      | Closed
  end

  type owner = t

  type t =
    { owner : owner
    ; prepared : Prepared.t
    ; runtime : Agent_session.Runtime_builder.t
    ; continuation : Agent_session.Session_actor.Admitted_activation.Continuation.t
    ; construction : Agent_session.Session_actor.Runtime_construction.t
    ; request :
        Agent_session.Session_actor.Admitted_activation.Continuation.t Runtime_request.t
    ; residency : Held_runtime_residency.t option ref
    ; admission : Agent_protocol.Run_receipt.t
    ; mutable phase : Phase.t
    }

  let unavailable () =
    Agent_protocol.Error.create
      Conflict
      ~message:"held input runtime is closed or already submitted"
      ~retryable:false
      ()
  ;;

  let submit t ~prepare =
    let module A = Agent_session.Session_actor in
    let open Result.Let_syntax in
    let%bind () =
      match t.phase with
      | Available
        when Int.equal t.prepared.stage.domain (Stdlib.Domain.self () :> int)
             && (not t.prepared.stage.closed)
             && (not (resource_retirement_pending t.owner))
             && Option.exists t.owner.prepared_source ~f:(phys_equal t.prepared.stage)
             && Option.exists t.prepared.stage.continuation ~f:(phys_equal t.construction)
        ->
        t.phase <- Submitted;
        Ok ()
      | Available | Submitted | Closed -> Error (unavailable ())
    in
    A.with_held_input
      t.owner.actor
      ~continuation:t.continuation
      ~prepare:(fun ~scope _execution ->
        Eio.Fiber.check ();
        if resource_retirement_pending t.owner
        then Error (unavailable ())
        else prepare ~runtime:t.runtime ~scope)
      (fun ~scope ~input ->
         let%bind () =
           install_runtime_for_request
             t.owner
             t.request
             ~runtime:t.runtime
             ~construction:t.construction
             ~residency:t.residency
         in
         Eio.Fiber.check ();
         (* The root ACK precedes this bookkeeping. Keep the exact original
            Admission, rather than manufacturing a receipt from the submission. *)
         Exn.protect
           ~finally:(fun () ->
             if A.Runtime_construction.handoff_acknowledged t.construction
             then Held_runtime_residency.handed_off t.residency)
           ~f:(fun () ->
             let%map _submission = A.submit_held_run_input t.owner.actor ~scope ~input in
             t.admission))
  ;;
end

(* Factual source-stage/resource association after an installed lifecycle. The
   pre-install basis intentionally expects stage.runtime and cannot be reused
   after this exact install. This check performs no fresh execution refresh. *)
let installed_control_source_basis t request ~runtime ~construction =
  let stage = Runtime_request.stage request in
  if
    Int.equal stage.domain (Stdlib.Domain.self () :> int)
    && (not (t.closed || stage.closed))
    && (not (resource_retirement_blocks_admission t))
    && Option.equal phys_equal t.foreground_retirement stage.foreground_retirement
    && Option.exists t.prepared_source ~f:(phys_equal stage)
    && Option.exists (loaded_runtime t) ~f:(phys_equal runtime)
    && Option.exists stage.continuation ~f:(phys_equal construction)
  then Ok ()
  else
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"controlled installed runtime source owner was displaced"
         ~retryable:false
         ())
;;

let with_held_input_runtime_result t ~prepared ~held ~checked f =
  with_held_runtime_internal
    t
    ~prepared
    ~held
    ~checked
    (fun runtime continuation construction residency ->
       prepared.stage.handoff_runtime <- Some runtime;
       let input_runtime : Held_input_runtime.t =
         { owner = t
         ; prepared
         ; runtime
         ; continuation
         ; construction
         ; request = Runtime_request.Initial { prepared; held; checked }
         ; residency
         ; admission = Agent_session.Session_actor.Held_run.admission held
         ; phase = Available
         }
       in
       Exn.protect
         ~finally:(fun () -> input_runtime.phase <- Closed)
         ~f:(fun () ->
           Result.map (f input_runtime) ~f:(fun value -> Construction_result.Built value)))
;;

let with_held_input_runtime t ~prepared ~held ~checked f =
  with_held_input_runtime_result t ~prepared ~held ~checked f
  |> Result.bind ~f:ordinary_construction_result
;;

let start_held_authored_runtime_result t ~prepared ~held ~checked =
  with_held_runtime_internal
    t
    ~prepared
    ~held
    ~checked
    (fun runtime continuation construction residency ->
       let open Result.Let_syntax in
       let%bind startup = Agent_session.Runtime_builder.authored_start runtime in
       prepared.stage.handoff_runtime <- Some runtime;
       let%bind () =
         install_runtime_for_request
           t
           (Runtime_request.Initial { prepared; held; checked })
           ~runtime
           ~construction
           ~residency
       in
       Eio.Fiber.check ();
       (* The Actor joins the actual lifecycle callback and releases the aggregate.
       Do not shield that work from cancellation. Its immutable ACK also lets
       ordinary scoped cleanup reconcile a lost post-commit reply. *)
       let result =
         Agent_session.Session_actor.start_held_authored_run
           t.actor
           ~continuation
           ~startup
       in
       let completed completion =
         let%map () =
           installed_control_source_basis
             t
             (Runtime_request.Initial { prepared; held; checked })
             ~runtime
             ~construction
         in
         Construction_result.Callback_completed completion
       in
       Result.bind result ~f:(function
         | Agent_session.Session_actor.Held_authored_result.Started admission ->
           Held_runtime_residency.handed_off residency;
           Ok (Construction_result.Built admission)
         | Controlled control ->
           completed
             (Agent_session.Session_actor.Goal_callback_completion.Controlled control)
         | Settled settlement ->
           completed
             (Agent_session.Session_actor.Goal_callback_completion.Settled settlement)))
;;

let start_held_authored_runtime t ~prepared ~held ~checked =
  start_held_authored_runtime_result t ~prepared ~held ~checked
  |> Result.bind ~f:ordinary_construction_result
;;

let with_joined_existing_goal_runtime
      (type a)
      t
      (request : a Runtime_request.t)
      ~run_resume
      f
  =
  let module Actor = Agent_session.Session_actor in
  let module Construction = Actor.Runtime_construction in
  with_runtime_construction_internal
    t
    request
    (fun runtime active construction residency ->
       let open Result.Let_syntax in
       let%bind resume, retained_lifecycle =
         match !residency with
         | Some (Held_runtime_residency.Reused _) -> Ok (None, None)
         | Some (Local { binding = Runtime_binding.Legacy _; _ }) ->
           Agent_session.Runtime_builder.runtime_resume runtime
           |> Result.map ~f:(fun resume -> Some resume, None)
         | Some (Local { binding = Attached attachment; _ }) ->
           let%bind selected =
             Agent_session.Runtime_builder.Attachment.runtime_resume attachment
           in
           (match selected with
            | Continue_live ->
              let%bind witness =
                Agent_session.Runtime_builder.Attachment.retained_lifecycle attachment
              in
              (match witness with
               | Some witness -> Ok (None, Some witness)
               | None ->
                 Error
                   (Agent_protocol.Error.create
                      Conflict
                      ~message:
                        "retained runtime has no actual completed lifecycle witness"
                      ~retryable:false
                      ()))
            | Pending resume -> Ok (Some resume, None)
            | Pending_legacy _ ->
              Error
                (Agent_protocol.Error.create
                   Conflict
                   ~message:"scoped legacy runtime resume has no constructor ACK"
                   ~retryable:false
                   ()))
         | None | Some (Installed _ | Handed_off _ | Ordinary_installed _) ->
           Error
             (Agent_protocol.Error.create
                Conflict
                ~message:"Goal runtime residency changed before installation"
                ~retryable:false
                ())
       in
       (Runtime_request.stage request).handoff_runtime <- Some runtime;
       let%bind () =
         install_runtime_for_request
           ?retained_lifecycle
           t
           request
           ~runtime
           ~construction
           ~residency
       in
       Eio.Fiber.check ();
       let%bind () =
         if (Runtime_request.stage request).closed || resource_retirement_pending t
         then Error (background_busy ())
         else Ok ()
       in
       let%bind control =
         match resume with
         | None -> Ok None
         | Some resume -> run_resume active resume
       in
       match control with
       | Some control ->
         let%map () = installed_control_source_basis t request ~runtime ~construction in
         Construction_result.Callback_completed
           (Agent_session.Session_actor.Goal_callback_completion.Controlled control)
       | None ->
         let result =
           if (Runtime_request.stage request).closed || resource_retirement_pending t
           then Error (background_busy ())
           else f runtime active construction
         in
         if Construction.handoff_acknowledged construction
         then Held_runtime_residency.handed_off residency;
         Result.map result ~f:(fun value -> Construction_result.Built value))
;;

let resume_goal_episode t ~prepared ~continuation =
  let module Actor = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let%bind result =
    with_joined_existing_goal_runtime
      t
      (Runtime_request.Existing_goal { prepared; continuation })
      ~run_resume:(fun continuation resume ->
        Actor.run_goal_runtime_resume t.actor ~continuation ~resume)
      (fun _runtime continuation _construction ->
         (* A joined lifecycle may really Wait or settle. A command receipt alone
         does not permit another Operation; preserve its actual final decision. *)
         Result.bind (Actor.continue_goal_episode t.actor ~continuation) ~f:(function
           | (Actor.Goal_resume_result.Resumed _ | Retained _) as result -> Ok result
           | Resume_failed { receipt = _; error } | Rejected error | Uncertain error ->
             Error error))
  in
  match result with
  | Construction_result.Built result -> Ok result
  | Callback_completed (Actor.Goal_callback_completion.Controlled _) ->
    Actor.continue_goal_episode t.actor ~continuation
  | Callback_completed (Settled settlement) ->
    Ok
      (Actor.Goal_resume_result.Resumed
         { receipt = Actor.Goal_continuation.receipt continuation
         ; continuation = Settled settlement
         })
;;

let with_goal_job_runtime t ~prepared ~execution f =
  let module Actor = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let%bind () =
    match Actor.Goal_execution_runtime.kind execution with
    | Claimed_job -> Ok ()
    | Queued_wake | Subscription_wake ->
      Error
        (Agent_protocol.Error.invalid_request
           "Goal Job runtime requires its actual Job owner")
  in
  with_joined_existing_goal_runtime
    t
    (Runtime_request.Existing_goal_execution { prepared; execution })
    ~run_resume:(fun execution resume ->
      Actor.run_goal_execution_runtime_resume t.actor ~execution ~resume)
    (fun runtime execution _construction ->
       let%bind () = Actor.acknowledge_goal_job_runtime t.actor ~execution in
       Eio.Fiber.check ();
       if resource_retirement_pending t then Error (background_busy ()) else f runtime)
;;

let with_unloaded t f =
  let result =
    (* An actor request continues after its caller cancels. Keep the owner until
       its bounded checkpoint actually returns, rather than permitting reload
       while that actor is still inspecting/deleting cache-dependent artifacts. *)
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      try
        Ok
          (match t.closed, t.unloading, loaded_runtime t, t.background_leases with
           | false, None, None, []
             when (not (resource_retirement_pending t))
                  && (not (retired_cleanup_pending t))
                  && Option.is_none t.administration_transition
                  && Option.is_none t.prepared_source -> Result.map (f ()) ~f:Option.some
           | _ -> Ok None)
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let reserve_inactive_close t =
  with_owner_lock t ~protect:true (fun () ->
    match t.closed, t.unloading, loaded_runtime t with
    | false, None, None
      when (not (List.exists t.background_leases ~f:Background_lease.is_active))
           && (not (resource_retirement_pending t))
           && (not (retired_cleanup_pending t))
           && (not (failed_source_cleanup_pending t))
           && Option.is_none t.administration_transition
           && Option.is_none t.prepared_source ->
      t.closed <- true;
      true
    | _ -> false)
;;

let retire_after_administration t cleanup =
  Eio.Cancel.protect (fun () ->
    (match
       Agent_session.Session_actor.set_runtime_worker t.actor ~worker:None ~inference:None
     with
     | Ok () -> ()
     | Error error -> raise (Cleanup_failed error));
    let previous = t.binding in
    t.binding <- None;
    Option.iter previous ~f:(release_runtime_locked t cleanup))
;;

let begin_administration_transition_locked t ~validate =
  let open Result.Let_syntax in
  let%bind () =
    if t.closed
    then Error (closed_error ())
    else if
      resource_retirement_blocks_admission t
      || failed_source_cleanup_pending t
      || (not (List.is_empty t.background_leases))
      || administration_blocks t
      || Option.is_some t.prepared_source
    then Error (background_busy ())
    else if retired_cleanup_pending t
    then Error (retired_cleanup_error ())
    else validate ()
  in
  let transition = Administration_transition.create ~affinity:t.affinity in
  Source_resource_association.capture
    transition.extent.association
    ~binding:t.binding
    ~handoff:None
    ~restored:t.restored_policy
    ~native:t.native_anchor;
  let%bind () = attach_pending_source_extent t transition.extent in
  t.administration_transition <- Some transition;
  Ok transition
;;

let check_administration_transition_locked t transition =
  if t.closed
  then Error (closed_error ())
  else if
    resource_retirement_blocks_admission t
    || (not (Administration_transition.is_live transition))
    || not (Option.exists t.administration_transition ~f:(phys_equal transition))
  then Error (background_busy ())
  else Ok ()
;;

(* An acquired transition can be the exact extent a pending retirement is
   waiting to join. Its already-owed disposal must run before that join, even
   when the pending producer has retired fresh work. This does not permit a
   commit, initialization, or a displaced/unrelated transition to resume. *)
let check_administration_cleanup_locked t (transition : Administration_transition.t) =
  let owned_by_pending =
    match t.pending_resource_retirement, t.unloading with
    | Some pending, None ->
      Option.exists pending.source_extent ~f:(phys_equal transition.extent)
    | (Some _ | None), (Some _ | None) -> false
  in
  if t.closed
  then Error (closed_error ())
  else if
    owned_by_pending
    && Option.exists t.administration_transition ~f:(phys_equal transition)
  then Ok ()
  else check_administration_transition_locked t transition
;;

let finish_administration_transition t owned =
  with_owner_lock t ~protect:true (fun () ->
    match t.administration_transition, owned with
    | Some current, Some owned when phys_equal current owned ->
      retain_source_cleanup_locked t owned.extent.recovery ~acquired:true;
      t.administration_transition <- None
    | (Some _ | None), (Some _ | None) -> ())
;;

(* Actual final cleanup is retained before its projection or transition clear.
   The accepted commit's primary remains separate from this immutable report. *)
let finish_administration_cleanup t owned cleanup =
  Eio.Cancel.protect (fun () ->
    let actual_projection = ref Cleanup_projection.empty in
    let registration_step =
      Option.map owned ~f:(fun transition ->
        Recovery_data.register_original_step
          transition.Administration_transition.extent.recovery
          Administration_batch_registration)
    in
    let finalized =
      Scope_outcome.capture (fun () ->
        let batch = run_cleanup_request_reported t cleanup in
        actual_projection := Cleanup_batch.projection batch;
        Option.iter owned ~f:(fun transition ->
          Source_extent.record_batch
            transition.Administration_transition.extent
            ~role:Administration_exit
            batch);
        Ok ())
    in
    let boundary_projection step primary =
      match step with
      | Some step ->
        Original_cleanup_step.record_projection step primary;
        Original_cleanup_step.projection step ~acquired:false
      | None ->
        (* No transition was acquired. This is an actual local finalizer exit,
           not a projection of a retained source or physical producer. *)
        Cleanup_projection.of_diagnostic
          (match primary with
           | Scope_outcome.Returned result ->
             Scope_outcome.Cleanup.of_result ~acquired:false (Ok result)
           | Raised (exn, trace) ->
             Scope_outcome.Cleanup.of_result ~acquired:false (Error (exn, trace)))
          (Original_diagnostic.primary primary)
    in
    let projection =
      Cleanup_projection.combine
        !actual_projection
        (boundary_projection registration_step finalized)
    in
    let clear_step =
      Option.map owned ~f:(fun transition ->
        Recovery_data.register_original_step
          transition.Administration_transition.extent.recovery
          Administration_clear)
    in
    let cleared =
      Scope_outcome.capture (fun () ->
        finish_administration_transition t owned;
        Ok ())
    in
    let projection =
      Cleanup_projection.combine projection (boundary_projection clear_step cleared)
    in
    Option.value_map
      owned
      ~default:(Cleanup_projection.cleanup projection)
      ~f:(fun transition ->
        let cleanup =
          Source_extent.finish
            transition.extent
            ~completion:Administration_transition
            ~projection
        in
        finish_source_cleanup_fence t transition.extent.recovery;
        cleanup))
;;

let with_administration ?capture_cleanup t f =
  let cleanup = Cleanup_request.create () in
  let owned = ref None in
  let primary =
    Scope_outcome.capture (fun () ->
      let open Result.Let_syntax in
      let%bind transition =
        with_owner_lock t ~protect:false (fun () ->
          let%map transition =
            begin_administration_transition_locked t ~validate:(fun () -> Ok ())
          in
          owned := Some transition;
          transition)
      in
      Eio.Cancel.protect (fun () ->
        Option.iter capture_cleanup ~f:(fun capture -> capture transition.extent.recovery));
      with_owner_lock t ~protect:false (fun () ->
        let%bind () = check_administration_transition_locked t transition in
        match f () with
        | Error _ as failure -> failure
        | Ok _ as result ->
          let%bind () =
            record_source_cleanup
              ~operation:Administration_retire_selection
              transition.extent
              (fun () ->
                 retire_after_administration t cleanup;
                 Ok ())
          in
          result))
  in
  ignore (finish_administration_cleanup t !owned cleanup : Scope_outcome.Cleanup.t);
  (* Pending retirement separately observes actual cleanup through SAME extent.
     A cleanup escape cannot overwrite the already captured commit primary. *)
  match primary with
  | Returned result -> result
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let retire_administration ?capture_cleanup t ~validate ~commit =
  let open Result.Let_syntax in
  let cleanup = Cleanup_request.create () in
  let owned = ref None in
  let primary =
    Scope_outcome.capture (fun () ->
      let%bind transition =
        with_owner_lock t ~protect:false (fun () ->
          let%map transition = begin_administration_transition_locked t ~validate in
          owned := Some transition;
          transition)
      in
      Eio.Cancel.protect (fun () ->
        Option.iter capture_cleanup ~f:(fun capture -> capture transition.extent.recovery));
      let%bind () =
        with_owner_lock t ~protect:false (fun () ->
          let%bind () = check_administration_cleanup_locked t transition in
          record_source_cleanup
            ~operation:(Administration_unload_selection Transition)
            transition.extent
            (fun () -> unload_locked t cleanup))
      in
      let%bind () =
        with_source_cleanup_request
          t
          transition.extent
          ~role:Administration_before_commit
          cleanup
          (fun () -> Ok ())
      in
      with_owner_lock t ~protect:false (fun () ->
        let%bind () = check_administration_transition_locked t transition in
        commit ()))
  in
  ignore (finish_administration_cleanup t !owned cleanup : Scope_outcome.Cleanup.t);
  match primary with
  | Returned result -> result
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let reinitialize_ordinary_administration
      ?capture_cleanup
      t
      ~validate
      ~commit
      ~before_initialize
  =
  let open Result.Let_syntax in
  let cleanup = Cleanup_request.create () in
  let owned = ref None in
  let primary =
    Scope_outcome.capture (fun () ->
      let%bind transition =
        with_owner_lock t ~protect:false (fun () ->
          let%map transition = begin_administration_transition_locked t ~validate in
          owned := Some transition;
          transition)
      in
      Eio.Cancel.protect (fun () ->
        Option.iter capture_cleanup ~f:(fun capture -> capture transition.extent.recovery));
      let%bind () =
        with_owner_lock t ~protect:false (fun () ->
          let%bind () = check_administration_cleanup_locked t transition in
          record_source_cleanup
            ~operation:(Administration_unload_selection Transition)
            transition.extent
            (fun () -> unload_locked t cleanup))
      in
      let%bind () =
        with_source_cleanup_request
          t
          transition.extent
          ~role:Administration_before_commit
          cleanup
          (fun () -> Ok ())
      in
      with_owner_lock t ~protect:false (fun () ->
        let%bind () = check_administration_transition_locked t transition in
        let%bind () = commit () in
        before_initialize ();
        (* Only this exact lexical transition can pass its own exclusion. *)
        Cleanup_request.attach_initialization_recovery cleanup transition.extent.recovery;
        let%bind () = ensure_loaded_locked ~administration:transition t ~cleanup in
        let%bind state = Agent_session.Session_actor.state t.actor in
        let%map () =
          match state.lifecycle.desired with
          | Stopped -> unload_locked t cleanup
          | Running -> Ok ()
        in
        Agent_session.Session_state.summary state))
  in
  let cleanup = finish_administration_cleanup t !owned cleanup in
  Scope_outcome.resume { primary; cleanup }
;;

let reinitialize_administration ?capture_cleanup t ~validate ~commit ~before_initialize =
  match t.delegated_driver with
  | None ->
    reinitialize_ordinary_administration
      ?capture_cleanup
      t
      ~validate
      ~commit
      ~before_initialize
  | Some _ ->
    run_delegated_driver
      ~admission:
        (Source_stage_admission.Administration
           { validate; commit; before_initialize; capture_cleanup })
      t
      ~custody:(Agent_session.Session_actor.Delegated_load.Custody.Host None)
      ~retention:Release_if_stopped
;;

let parse_user_content t ~id content =
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_actor.exclude_effect_deferred_execution t.actor in
  Eio.Mutex.use_ro t.mutex (fun () ->
    match t.closed, loaded_runtime t with
    | true, _ -> Error (closed_error ())
    | false, _ when prepared_source_blocks_runtime t -> Error (background_busy ())
    | false, Some runtime -> runtime.parse_user_content ~id content
    | false, None ->
      Error
        (Agent_protocol.Error.create
           Invalid_state
           ~message:"session runtime is not loaded"
           ~retryable:true
           ()))
;;

let with_activity (runtime : Agent_session.Runtime_builder.t) f =
  match runtime.activity with
  | None -> f ()
  | Some activity ->
    (try Agent_session.Runtime_activity.run activity f with
     | Agent_session.Runtime_activity.Closed ->
       Error
         (Agent_protocol.Error.create
            Interrupted
            ~message:"generated runtime execution scope is closed"
            ~retryable:false
            ())
     | Eio.Cancel.Cancelled _ as exn ->
       (match Eio.Fiber.is_cancelled () with
        | true -> raise exn
        | false ->
          Error
            (Agent_protocol.Error.create
               Interrupted
               ~message:"generated runtime execution scope was cancelled"
               ~retryable:false
               ())))
;;

let with_delivery_cancellation f =
  match f () with
  | result -> result
  | exception (Eio.Cancel.Cancelled _ as exn) ->
    (* Retirement cancels its delivery lease, not the shared scheduler. Let
       caller/shutdown cancellation propagate, but keep a local interruption
       from cancelling the daemon switch and its still-needed actor loops. *)
    (match Eio.Fiber.is_cancelled () with
     | true -> raise exn
     | false ->
       Error
         (Agent_protocol.Error.create
            Interrupted
            ~message:"runtime delivery was cancelled"
            ~retryable:false
            ()))
;;

let with_cancellable_access t f =
  (* Moderator checkpoints may reauthorize delegated calls through this owner.
     Retain the actual cancellable lease while waiting outside the owner mutex. *)
  with_delivery_cancellation (fun () ->
    with_background_runtime t (fun runtime ->
      Eio.Mutex.use_ro t.moderator_work_mutex (fun () ->
        with_activity runtime (fun () -> f runtime))))
;;

let submit_ingress t ~producer ~registration_id ~namespace ~key ~payload =
  with_cancellable_access t (fun runtime ->
    let module A = Agent_session.Session_actor in
    let open Result.Let_syntax in
    let%bind runtime, source =
      match runtime.moderator_script_tools with
      | Some _ ->
        Option.bind
          runtime.moderator_manager
          ~f:Chat_response.Moderator_manager.invocation_observer
        |> Option.map ~f:(fun source -> runtime, source)
        |> Result.of_option
             ~error:
               (Agent_protocol.Error.invalid_request
                  "ingress requires a qualified moderator")
      | _ -> Error (Agent_protocol.Error.invalid_request "ingress runtime is unavailable")
    in
    A.with_moderator_checkpoint t.actor (fun () ->
      let%bind decision =
        A.prepare_ingress_submission
          t.actor
          ~source
          ~producer
          ~registration_id
          ~namespace
          ~key
          ~payload
      in
      match decision with
      | Duplicate receipt -> Ok receipt
      | Enqueue proposal ->
        let%bind frame = Agent_session.Ingress_submission.frame proposal in
        let%bind payload =
          Chat_response.Ingress_delivery.capture frame
          |> Chatml.Chatml_value_codec.Snapshot.of_value
          |> Result.map ~f:Chatml.Chatml_value_codec.Snapshot.to_jsonaf
          |> Result.map_error ~f:Agent_protocol.Error.invalid_request
        in
        let committed = ref None in
        let%bind _ =
          runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
            let%map receipt =
              A.commit_ingress_submission t.actor proposal ~before ~snapshot
            in
            committed := Some receipt)
        in
        Result.of_option
          !committed
          ~error:
            (Agent_protocol.Error.invalid_request
               "ingress queue did not commit a receipt")))
;;

module Schedule_delivery = struct
  type t =
    | Delivered
    | Deferred of Agent_protocol.Error.t
end

let deliver_schedule_in_runtime
      t
      (schedule : Agent_protocol.Schedule.t)
      (runtime : Agent_session.Runtime_builder.t)
  =
  let open Result.Let_syntax in
  let%bind () =
    match schedule.ownership with
    | None -> Ok ()
    | Some ownership ->
      (match
         Option.bind
           runtime.moderator_manager
           ~f:Chat_response.Moderator_manager.invocation_observer
       with
       | Some source when Agent_protocol.Invocation.equal_observer source ownership.source
         -> Ok ()
       | _ ->
         Error
           (Agent_protocol.Error.create
              Permission_denied
              ~message:"timer belongs to a different moderator source"
              ~retryable:false
              ()))
  in
  let%bind payload =
    match schedule.ownership with
    | None -> Ok schedule.payload
    | Some _ ->
      let module V = Chatml.Chatml_value_codec in
      Chat_response.Schedule_delivery.capture schedule
      |> Result.bind ~f:V.Snapshot.of_value
      |> Result.map ~f:V.Snapshot.to_jsonaf
      |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  in
  Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
    runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
      Agent_session.Session_actor.complete_schedule
        ~expected:before
        ~expected_schedule:schedule
        ?moderator_manager:runtime.moderator_manager
        t.actor
        ~schedule_id:schedule.id
        ~generation:schedule.generation
        ~moderator_snapshot:
          (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
      |> Result.map ~f:ignore)
    |> Result.map ~f:ignore)
;;

let deliver_schedule_attempt t schedule =
  with_delivery_cancellation (fun () ->
    let open Result.Let_syntax in
    let%bind attempt =
      with_runtime_lease_attempt t ~retain_resources:false (fun runtime ->
        Eio.Mutex.use_ro t.moderator_work_mutex (fun () ->
          with_activity runtime (fun () -> deliver_schedule_in_runtime t schedule runtime)))
    in
    match attempt with
    | Runtime_lease_attempt.Preparation_deferred error ->
      Ok (Schedule_delivery.Deferred error)
    | Lease_report report ->
      (match
         ( Scope_outcome.primary report
         , Scope_outcome.Cleanup.status (Scope_outcome.cleanup report) )
       with
       | ( Returned (Error ({ code = Conflict; retryable = true; _ } as error))
         , Never_acquired ) -> Ok (Schedule_delivery.Deferred error)
       | _ ->
         Scope_outcome.resume report
         |> Result.map ~f:(fun () -> Schedule_delivery.Delivered)))
;;

let deliver_schedule t schedule =
  let open Result.Let_syntax in
  let%bind outcome = deliver_schedule_attempt t schedule in
  match outcome with
  | Schedule_delivery.Delivered -> Ok ()
  | Deferred error -> Error error
;;

let idle_callback_limit t =
  let open Result.Let_syntax in
  let%map state = Agent_session.Session_actor.state t.actor in
  match state.automatic_turn_budget with
  | None -> 32
  | Some budget ->
    (match
       Chat_response.Automatic_turn_policy.has_pause_condition
         budget.policy
         Pause_internal_event_drains
     with
     | true -> 0
     | false -> Int.min 32 budget.policy.budget.max_internal_event_drain)
;;

let drain_loaded_legacy_events t runtime =
  let module Completion = Agent_session.Session_actor.Idle_moderator_completion in
  let completion = Completion.create t.actor in
  let finished = ref false in
  Exn.protect
    ~f:(fun () ->
      let result =
        Eio.Cancel.protect (fun () ->
          Exn.protect
            ~f:(fun () ->
              let open Result.Let_syntax in
              let%bind history = Completion.claim completion in
              match history with
              | None -> Ok None
              | Some history ->
                (match
                   runtime.Agent_session.Runtime_builder.drain_internal_events history
                 with
                 | Ok drain ->
                   let%map () = Completion.capture completion drain in
                   Some drain.remaining_events
                 | Error failure ->
                   (* The callback error remains primary. Its failure-metadata
                      writer diagnostic stays with the same Actor-owned claim. *)
                   ignore
                     (Completion.fail_callback completion ~failure
                      : (unit, Agent_protocol.Error.t) result);
                   Error failure))
            ~finally:(fun () -> Completion.leave_capture completion))
      in
      match result with
      | Ok None ->
        finished := true;
        Ok false
      | Ok (Some remaining_events) ->
        let%map.Result () = Completion.finish completion in
        finished := true;
        remaining_events
      | Error failure ->
        Completion.cancel completion ~failure;
        (* Outside the original finite callback mask and Owner state lock.
           Failure retains the exact owner; source close joins it again rather
           than invoking global idle failure or replaying a writer. *)
        ignore
          (Completion.join_retirement completion : (unit, Agent_protocol.Error.t) result);
        Error failure)
    ~finally:(fun () ->
      if not !finished
      then
        Completion.cancel
          completion
          ~failure:
            (Agent_protocol.Error.create
               Interrupted
               ~message:"legacy idle callback left before its handoff"
               ~retryable:true
               ()))
;;

let drain_loaded_queued_events
      ~max_events
      t
      (runtime : Agent_session.Runtime_builder.t)
      manager
  =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let open Result.Let_syntax in
  let history = ref [] in
  let claim ~snapshot handle =
    A.with_current_idle_queued_moderator_event_tools
      t.actor
      ~snapshot
      (fun
          ~executing
           ~policy_caller
           ~model_jobs
           ~allocate_inference
           ~retirement_reason
           ~event
           ~execute
           ~commit
         ->
         let%bind state = A.state t.actor in
         let%bind entries =
           Agent_session.History_codec.all_of_protocol
             state.conversation.canonical_history
         in
         history := entries;
         handle
           ~executing
           ~policy_caller
           ~model_jobs
           ~allocate_inference
           ~retirement_reason
           ~event
           ~execute
           ~commit)
  in
  let rec loop remaining handled =
    match remaining with
    | 0 -> Ok true
    | _ ->
      let%bind state = A.state t.actor in
      let blocked =
        Option.exists (M.invocation_observer manager) ~f:(fun observer ->
          Agent_session.Queued_moderator_event.has_unsettled_claim ~state ~observer)
      in
      if blocked
      then Ok handled
      else (
        let%bind outcome =
          Agent_session.Moderator_event.run_queued_idle
            ~model_recipes:runtime.model_recipes
            ~claim
            ~run_actions:(A.run_actions t.actor)
            ?script_tools:runtime.Agent_session.Runtime_builder.moderator_script_tools
            ~manager
            ~history:(fun () -> !history)
            ~available_tools:runtime.moderator_tools
            ~session_meta:`Null
            ~now:runtime.now
            ()
        in
        match outcome with
        | None -> Ok handled
        | Some outcome ->
          (match
             Chat_response.Runtime_semantics.should_end_session outcome.runtime_requests
           with
           | Some _ -> Ok true
           | None -> loop (remaining - 1) true))
  in
  loop max_events false
;;

let drain_loaded_owned_legacy_events ~capture_cleanup ~max_events t drain =
  let module A = Agent_session.Session_actor in
  let module B = Agent_session.Runtime_builder in
  let open Result.Let_syntax in
  let rec loop remaining handled =
    match remaining with
    | 0 -> Ok true
    | _ ->
      let%bind state = A.state t.actor in
      (* Legacy source identity is real checkpoint DATA, not a v1 invocation
         capability. Use the same retained failed/interrupted-head guard as
         admission before EACH selection, including callbacks after a yield. *)
      let%bind observer = B.moderator_snapshot_observer state.moderator in
      let%bind halted = B.moderator_snapshot_is_halted state.moderator in
      let%bind observer =
        observer
        |> Result.of_option
             ~error:
               (Agent_protocol.Error.create
                  Conflict
                  ~message:"installed legacy drain requires its retained moderator source"
                  ~retryable:false
                  ())
      in
      if
        halted
        || Agent_session.Queued_moderator_event.has_unsettled_claim ~state ~observer
      then Ok handled
      else (
        let%bind history =
          Agent_session.History_codec.all_of_protocol state.conversation.canonical_history
        in
        let%bind outcomes = drain ~capture_cleanup ~max_events:1 ~history in
        match outcomes with
        | [] -> Ok handled
        | [ (outcome : Chat_response.Moderation.Outcome.t) ] ->
          (match
             Chat_response.Runtime_semantics.should_end_session outcome.runtime_requests
           with
           | Some _ -> Ok true
           | None -> loop (remaining - 1) true)
        | _ ->
          Error
            (Agent_protocol.Error.create
               Conflict
               ~message:"installed legacy drain exceeded its selected head budget"
               ~retryable:false
               ()))
  in
  loop max_events false
;;

let drain_loaded_idle_moderator ~capture_cleanup t runtime =
  let open Result.Let_syntax in
  let%bind max_events = idle_callback_limit t in
  match max_events with
  | 0 -> Ok false
  | _ ->
    (match runtime.Agent_session.Runtime_builder.moderator_manager with
     | Some manager
       when Option.is_some (Chat_response.Moderator_manager.extension_definition manager)
       ->
       let open Result.Let_syntax in
       let%bind more = drain_loaded_queued_events ~max_events t runtime manager in
       let%map applied = Agent_session.Session_actor.apply_moderator_follow_up t.actor in
       more || applied
     | Some _ ->
       (match runtime.legacy_events with
        | Some drain ->
          let%bind more =
            drain_loaded_owned_legacy_events ~capture_cleanup ~max_events t drain
          in
          let%map applied =
            Agent_session.Session_actor.apply_moderator_follow_up t.actor
          in
          more || applied
        | None -> drain_loaded_legacy_events t runtime)
     | None -> Ok false)
;;

let pending_observation state observer =
  List.exists state.Agent_session.Session_state.invocations ~f:(fun invocation ->
    invocation.context.generation = state.identity.generation
    && Agent_protocol.Id.Session.equal
         invocation.context.session_id
         state.identity.session_id
    &&
    match invocation.observation, invocation.status with
    | Some { observer = owner; status = Awaiting; _ }, (Resolved _ | Published _) ->
      Agent_protocol.Invocation.equal_observer owner observer
    | _ -> false)
;;

let drain_loaded_observations t runtime =
  let open Result.Let_syntax in
  let%bind max_observations = idle_callback_limit t in
  match max_observations with
  | 0 -> Ok false
  | _ ->
    (match
       Option.bind
         runtime.Agent_session.Runtime_builder.moderator_manager
         ~f:(fun manager ->
           Option.map
             (Chat_response.Moderator_manager.invocation_observer manager)
             ~f:(fun observer -> manager, observer))
     with
     | None -> Ok false
     | Some (manager, observer) ->
       let history = ref [] in
       let with_history handle =
         let%bind state = Agent_session.Session_actor.state t.actor in
         let%bind entries =
           Agent_session.History_codec.all_of_protocol
             state.conversation.canonical_history
         in
         history := entries;
         handle ()
       in
       let drain =
         match
           ( runtime.moderator_script_tools
           , Chat_response.Moderator_manager.extension_definition manager )
         with
         | Some script_tools, Some definition ->
           Agent_session.Moderator_observation.drain_idle_with_tools
             ~model_recipes:runtime.model_recipes
             ~max_observations
             ~script_tools
             ~definition
             ~claim:(fun handle ->
               Agent_session.Session_actor.with_idle_moderator_observation_tools
                 t.actor
                 ~observer
                 (fun ~observing ~capabilities ~execute ~commit ->
                    with_history (fun () ->
                      handle ~observing ~capabilities ~execute ~commit)))
         | _ ->
           Agent_session.Moderator_observation.drain_idle
             ~model_recipes:runtime.model_recipes
             ~max_observations
             ~on_tool_call:(fun ~name:_ ~args:_ ->
               Ok
                 (Chat_response.Moderation.Capabilities.Tool_error
                    "invocation.unavailable"))
             ~claim:(fun handle ->
               Agent_session.Session_actor.with_idle_moderator_observation
                 t.actor
                 ~observer
                 (fun ~observing ~capabilities ~commit ->
                    with_history (fun () -> handle ~observing ~capabilities ~commit)))
       in
       let%map drain =
         drain
           ~manager
           ~history:(fun () -> !history)
           ~available_tools:runtime.moderator_tools
           ~session_meta:`Null
           ~now:runtime.now
           ()
       in
       drain.budget_exhausted)
;;

let drain_loaded_notifications runtime =
  match runtime.Agent_session.Runtime_builder.moderator_activation with
  | Some activation when activation.pending () -> Ok false
  | _ ->
    (match runtime.idle_notifications with
     | None -> Ok false
     | Some deliver -> deliver ())
;;

let snapshot_has_pending_events t =
  let open Result.Let_syntax in
  let%bind state = Agent_session.Session_actor.state t.actor in
  if
    Agent_protocol.Session.equal_desired_state state.lifecycle.desired Running
    && (match state.lifecycle.observed with
        | Agent_protocol.Session.Idle -> true
        | Stopped
        | Queued_for_slot
        | Starting
        | Recovering
        | Running_turn _
        | Compacting _
        | Waiting_for_permission _
        | Stopping
        | Failed _ -> false)
    && Option.is_none state.active_operation
    && (not state.halted)
    && Option.is_none state.failure
  then (
    let%bind queued =
      Agent_session.Runtime_builder.moderator_snapshot_has_queued_events state.moderator
    in
    let%bind halted =
      Agent_session.Runtime_builder.moderator_snapshot_is_halted state.moderator
    in
    let%bind eligibility =
      Agent_session.Pending_eligibility.create
        state
        ~boundary:Idle_start
        ~runtime_admission_open:true
    in
    let%bind pending =
      Agent_session.Pending_eligibility.eligible_prefix
        eligibility
        state.conversation.deferred_user_entries
    in
    let%map observer =
      Agent_session.Runtime_builder.moderator_snapshot_observer state.moderator
    in
    Option.exists (loaded_runtime t) ~f:(fun runtime ->
      Option.exists runtime.moderator_activation ~f:(fun activation ->
        activation.pending ()))
    || (queued
        && (not halted)
        && not
             (Option.exists observer ~f:(fun observer ->
                Agent_session.Queued_moderator_event.has_unsettled_claim ~state ~observer))
       )
    || ((not halted) && not (List.is_empty pending))
    || List.exists state.invocations ~f:Agent_session.Observation_follow_up.pending
    || List.exists
         state.moderator_executions
         ~f:Agent_session.Observation_follow_up.pending_event
    || ((not halted) && Option.exists observer ~f:(pending_observation state))
    || ((not halted) && Agent_session.Notification_delivery.has_idle_work state))
  else Ok false
;;

let drain_idle_moderator_leased ?capture_cleanup t =
  let open Result.Let_syntax in
  let%bind () = progress_idle_inference_usage t in
  let%bind ordinary =
    match t.delegated_driver, t.ordinary_warm with
    | None, None -> Ok Ordinary_preflight.Cold_allowed
    | None, Some _ ->
      let%bind pending = snapshot_has_pending_events t in
      if pending then prepare_ordinary_load t else Ok Ordinary_preflight.No_work
    | Some _, (Some _ | None) ->
      let%bind pending = snapshot_has_pending_events t in
      let%map () = if pending then prepare_delegated_load t else Ok () in
      Ordinary_preflight.No_work
  in
  let cleanup_request = Cleanup_request.create () in
  let scope =
    with_lease_scope_reported_internal
      ~capture_recovery:(fun ~finished:_ recovery ->
        Option.iter capture_cleanup ~f:(fun capture -> capture recovery))
      ~cleanup_request
      t
      ~survives_stop:false
      ~admit:(fun () ->
        let open Result.Let_syntax in
        let%bind pending = snapshot_has_pending_events t in
        match pending with
        | false -> Ok (Lease_admission.No_work false)
        | true ->
          let%map () =
            match t.delegated_driver with
            | None -> admit_ordinary_preflight_locked t ~cleanup:cleanup_request ordinary
            | Some _ ->
              if
                Option.is_some (loaded_runtime t)
                && (not (resource_retirement_blocks_admission t))
                && Option.is_none t.foreground_retirement
              then Ok ()
              else Error (background_busy ())
          in
          Lease_admission.Borrow
            ( loaded_runtime t
            , (match t.delegated_driver with
               | None -> None
               | Some _ -> loaded_runtime t)
            , None ))
      (fun ~recovery -> function
        | None -> Ok false
        | Some runtime ->
          let capture_cleanup = Some (Private_lease_callbacks.capture recovery) in
          let open Result.Let_syntax in
          let%bind () =
            match t.delegated_driver with
            | None -> Ok ()
            | Some _ -> check_runtime_execution runtime
          in
          Eio.Fiber.check ();
          let%bind () =
            match t.delegated_driver with
            | None -> Ok ()
            | Some _ ->
              with_owner_lock t ~protect:true (fun () ->
                loaded_runtime_basis_locked t runtime)
          in
          Eio.Fiber.check ();
          let%bind () =
            Agent_session.Session_actor.exclude_effect_deferred_execution t.actor
          in
          Eio.Mutex.use_ro t.moderator_work_mutex (fun () ->
            with_activity runtime (fun () ->
              let%bind notifications = drain_loaded_notifications runtime in
              let%bind applied =
                match notifications with
                | true -> Ok true
                | false -> Agent_session.Session_actor.apply_moderator_follow_up t.actor
              in
              if applied
              then Ok true
              else (
                let%bind activated =
                  match runtime.moderator_activation with
                  | None -> Ok false
                  | Some activation -> activation.run ?capture_cleanup ()
                in
                if activated
                then Ok true
                else (
                  let%bind more_observations = drain_loaded_observations t runtime in
                  let%bind applied =
                    Agent_session.Session_actor.apply_moderator_follow_up t.actor
                  in
                  if applied
                  then Ok true
                  else (
                    let%map more_events =
                      drain_loaded_idle_moderator ~capture_cleanup t runtime
                    in
                    more_observations || more_events))))))
  in
  Scope_outcome.resume scope.Lease_scope_outcome.report
;;

let drain_idle_moderator ?capture_cleanup t =
  let outcome =
    match drain_idle_moderator_leased ?capture_cleanup t with
    | result -> Ok result
    | exception (Eio.Cancel.Cancelled _ as exn) ->
      Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  match outcome with
  | Ok result -> result
  | Error (exn, backtrace) ->
    (* Stopping one idle session cancels its owned event sub-context. Once that
       scope has unwound, the scheduler's caller may still be live. Propagating
       that local cancellation out of its worker would fail the shared daemon
       switch and strand other requests. Caller/shutdown cancellation must still
       propagate. Any event borrow has unwound; retained factual work remains
       owned even when cancellation occurs before a borrow is acquired. *)
    (match Eio.Fiber.is_cancelled () with
     | true -> Exn.raise_with_original_backtrace exn backtrace
     | false ->
       Error
         (Agent_protocol.Error.create
            Interrupted
            ~message:"idle moderator execution was cancelled"
            ~retryable:false
            ()))
;;

let with_locked_runtime t f =
  let cleanup_request = Cleanup_request.create () in
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_actor.exclude_effect_deferred_execution t.actor in
  let%bind () = prepare_delegated_load t in
  let%bind ordinary = prepare_ordinary_load t in
  let%bind () =
    match t.delegated_driver with
    | None -> Ok ()
    | Some _ -> with_loaded_runtime t (fun _ -> Ok ())
  in
  with_construction_cleanup_request t cleanup_request (fun () ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      Exn.protect
        ~finally:(fun () -> retain_construction_request_locked t cleanup_request)
        ~f:(fun () ->
          let open Result.Let_syntax in
          let%bind () =
            match t.delegated_driver with
            | None -> admit_ordinary_preflight_locked t ~cleanup:cleanup_request ordinary
            | Some _ ->
              if Option.is_some (loaded_runtime t)
              then Ok ()
              else Error (background_busy ())
          in
          match loaded_runtime t with
          | Some runtime -> f runtime
          | None ->
            Error
              (Agent_protocol.Error.create
                 Internal_error
                 ~message:"runtime load completed without an installed runtime"
                 ~retryable:false
                 ()))))
;;

module For_testing = struct
  let joined_close_cell t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then invalid_arg "joined close observation belongs to another Eio domain";
    Eio.Mutex.use_ro t.mutex (fun () ->
      let attempt =
        Option.bind t.joined_close_handoff ~f:(fun handoff ->
          handoff.Joined_close_handoff.attempt)
      in
      Option.iter attempt ~f:(fun attempt ->
        if Retirement_coverage.is_whole_owner attempt.coverage
        then invalid_arg "joined close handed off a Whole retirement");
      attempt)
  ;;

  let with_loaded_runtime t f =
    Eio.Mutex.use_ro t.moderator_work_mutex (fun () ->
      with_locked_runtime t (fun _ -> f ()))
  ;;
end

let execute_model_job
      t
      ~allocate_inference
      ~inference_context
      ~capture_recipe_target
      ~recipe
      ~payload
  =
  with_background_runtime t (fun runtime ->
    runtime.Agent_session.Runtime_builder.execute_model_job
      ~allocate_inference
      ~inference_context
      ~capture_recipe_target
      ~recipe
      ~payload)
;;

type background_result =
  | Completed of Agent_protocol.Completion.t
  | Pending of Agent_protocol.Job.dependency

module Background_job = struct
  type t =
    { job : Agent_protocol.Job.t
    ; ceiling : Chat_response.One_off_request.policy
    ; request : Chat_response.Background_request.t
    ; deadline : Agent_protocol.Timestamp.t
    }

  let equal_policy
        (left : Chat_response.One_off_request.policy)
        (right : Chat_response.One_off_request.policy)
    =
    let equal_compilation
          ({ wall_seconds; max_source_bytes } : Chatml_compilation.limits)
          (right : Chatml_compilation.limits)
      =
      Float.equal wall_seconds right.wall_seconds
      && Int.equal max_source_bytes right.max_source_bytes
    in
    let equal_execution
          ({ fuel
           ; max_tasks
           ; wall_seconds
           ; max_value_bytes
           ; max_array_items
           ; max_depth
           ; allocation_bytes
           ; max_calls
           ; max_invocation_depth
           } :
            Chatml_execution.limits)
          (right : Chatml_execution.limits)
      =
      Int.equal fuel right.fuel
      && Int.equal max_tasks right.max_tasks
      && Float.equal wall_seconds right.wall_seconds
      && Int.equal max_value_bytes right.max_value_bytes
      && Int.equal max_array_items right.max_array_items
      && Int.equal max_depth right.max_depth
      && Int.equal allocation_bytes right.allocation_bytes
      && Int.equal max_calls right.max_calls
      && Int.equal max_invocation_depth right.max_invocation_depth
    in
    equal_compilation left.compilation right.compilation
    && equal_execution left.execution right.execution
    && Int.equal left.max_output_bytes right.max_output_bytes
  ;;

  let prepare ~policy job =
    let open Result.Let_syntax in
    let%bind request =
      Chat_response.Background_request.of_json ~policy job.Agent_protocol.Job.payload
    in
    let%map deadline =
      Result.try_with (fun () ->
        Time_ns.add
          (Agent_protocol.Timestamp.to_time_ns job.created_at)
          (Time_ns.Span.of_sec
             (Chat_response.Background_request.policy request).execution.wall_seconds)
        |> Agent_protocol.Timestamp.of_time_ns)
      |> Result.map_error ~f:(fun _ ->
        Agent_protocol.Error.invalid_request
          "background deadline is outside the supported timestamp range")
    in
    { job; ceiling = policy; request; deadline }
  ;;

  let deadline t = t.deadline
end

let execute_background_job_runtime t ~runtime ~execution ~request =
  let open Result.Let_syntax in
  let%bind () =
    Agent_session.Session_actor.check_job_runtime_owner
      t.actor
      ~owner:execution.Agent_session.Session_actor.runtime_owner
  in
  let%bind () =
    if
      Option.exists
        (Agent_session.Session_actor.Job_runtime_owner.deadline execution.runtime_owner)
        ~f:(Agent_protocol.Timestamp.equal request.Background_job.deadline)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request
           "background scope deadline differs from its request")
  in
  let%bind () = check_runtime_execution runtime in
  Eio.Fiber.check ();
  let%bind () =
    Eio.Mutex.use_ro t.mutex (fun () ->
      if Option.exists (loaded_runtime t) ~f:(phys_equal runtime)
      then Ok ()
      else
        Error
          (Agent_protocol.Error.create
             Conflict
             ~message:"background runtime installation changed"
             ~retryable:false
             ()))
  in
  let%bind executor =
    Result.of_option
      runtime.background_executor
      ~error:
        (Agent_protocol.Error.create
           Invalid_state
           ~message:"generic background execution is not configured"
           ~retryable:false
           ())
  in
  let%bind () =
    if
      Background_job.equal_policy request.Background_job.ceiling executor.policy
      && Jsonaf.exactly_equal
           (Agent_protocol.Job.to_json execution.job)
           (Agent_protocol.Job.to_json request.job)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"background Job occurrence or host policy changed"
           ~retryable:false
           ())
  in
  let%bind checked = Background_job.prepare ~policy:executor.policy execution.job in
  let%bind () =
    if Agent_protocol.Timestamp.equal checked.deadline request.deadline
    then Ok ()
    else Error (Agent_protocol.Error.invalid_request "background deadline changed")
  in
  let deadline = request.deadline in
  let request = checked.request in
  if Agent_protocol.Timestamp.compare deadline (executor.now ()) <= 0
  then Ok (Completed Agent_protocol.Completion.Expired)
  else (
    let%bind result =
      let job = execution.job in
      let is_halted () =
        match Agent_session.Session_actor.state t.actor with
        | Error _ -> true
        | Ok state ->
          state.halted
          || Option.is_some state.failure
          || Agent_protocol.Session.equal_desired_state state.lifecycle.desired Stopped
          || not
               (List.exists state.jobs ~f:(fun current ->
                  Agent_protocol.Id.Job.equal current.id job.id
                  && Int.equal current.generation job.generation
                  && Int.equal current.attempt job.attempt
                  &&
                  match current.status with
                  | Running | Waiting_permission _ -> true
                  | _ -> false))
      in
      executor.run
        ~job
        ~deadline
        ~execute:execution.execute
        ~moderator_execute:execution.moderator_execute
        ~claim_event:execution.claim_event
        ~is_halted
        ~request
    in
    let%bind () =
      match result.runtime_requests with
      | [] -> Ok ()
      | _ ->
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"background runtime request consumption is not installed"
             ~retryable:false
             ())
    in
    match result.pending, result.resolved.status with
    | Some target, Resolved (Complete _) ->
      (match target.invocation.status, target.invocation.context.deadline with
       | Resolved (Pending (work, _)), Some deadline ->
         let policy = Chat_response.Background_request.policy request in
         Ok
           (Pending
              { invocation_id = target.invocation.context.id
              ; work
              ; deadline
              ; completion_schema = target.completion_schema
              ; max_output_bytes = policy.max_output_bytes
              ; max_output_depth = policy.execution.max_depth
              })
       | _ ->
         Error
           (Agent_protocol.Error.invalid_request
              "background Pending target has no owned work or deadline"))
    | None, Resolved (Complete value) ->
      Ok (Completed (Agent_protocol.Completion.Succeeded value))
    | _, Resolved (Fail error) -> Ok (Completed (Agent_protocol.Completion.Failed error))
    | _, Resolved (Cancelled reason) ->
      Ok (Completed (Agent_protocol.Completion.Cancelled reason))
    | _ ->
      Error
        (Agent_protocol.Error.create
           Invalid_state
           ~message:"background execution did not produce a terminal outcome"
           ~retryable:false
           ()))
;;

let execute_background_job_in_scope t ~execution ~request =
  let module Actor = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let%bind () =
    Actor.check_job_runtime_owner t.actor ~owner:execution.Actor.runtime_owner
  in
  let%bind () =
    match Actor.Job_runtime_owner.attribution execution.runtime_owner with
    | Ordinary -> Ok ()
    | Goal ->
      Error
        (Agent_protocol.Error.invalid_request
           "Goal background Job requires its Factory source owner")
  in
  with_background_runtime t (fun runtime ->
    execute_background_job_runtime t ~runtime ~execution ~request)
;;

let execute_background_job t (job : Agent_protocol.Job.t) =
  with_background_runtime t (fun runtime ->
    let open Result.Let_syntax in
    let%bind executor =
      Result.of_option
        runtime.background_executor
        ~error:
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"generic background execution is not configured"
             ~retryable:false
             ())
    in
    let%bind request = Background_job.prepare ~policy:executor.policy job in
    if
      Agent_protocol.Timestamp.compare (Background_job.deadline request) (executor.now ())
      <= 0
    then Ok (Completed Agent_protocol.Completion.Expired)
    else
      Agent_session.Session_actor.with_job_execution
        t.actor
        ~job_id:job.id
        ~generation:job.generation
        ~attempt:job.attempt
        ~deadline:(Some (Background_job.deadline request))
        (fun execution -> execute_background_job_runtime t ~runtime ~execution ~request))
;;

let deliver_model_job_completion t (job : Agent_protocol.Job.t) =
  with_cancellable_access t (fun runtime ->
    Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
      runtime.enqueue_model_job_completion job ~prepare:(fun ~before ~snapshot ->
        Agent_session.Session_actor.deliver_job
          ~expected:before
          ~expected_job:job
          ?moderator_manager:runtime.moderator_manager
          t.actor
          ~job_id:job.id
          ~generation:job.generation
          ~moderator_snapshot:
            (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
        |> Result.map ~f:ignore)
      |> Result.map ~f:ignore))
;;

let deliver_moderated_background_job_completion
      t
      (runtime : Agent_session.Runtime_builder.t)
      (job : Agent_protocol.Job.t)
  =
  let open Result.Let_syntax in
  Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
    let%bind state = Agent_session.Session_actor.state t.actor in
    let%bind retired =
      Agent_session.Session_actor.retire_obsolete_moderator_delivery
        t.actor
        ~revision:state.counters.revision
        ~job
    in
    match retired with
    | true -> Ok ()
    | false ->
      let%bind observer =
        match runtime.moderator_manager with
        | None ->
          Error
            (Agent_protocol.Error.invalid_request
               "background completion requires its actual moderator")
        | Some manager ->
          (match Chat_response.Moderator_manager.invocation_observer manager with
           | Some observer -> Ok observer
           | None ->
             Chat_response.Moderator_manager.identity_snapshot manager
             |> Result.map_error ~f:Agent_protocol.Error.invalid_request
             |> Result.map ~f:(fun snapshot ->
               { Agent_protocol.Invocation.script_id = snapshot.script_id
               ; source_sha256 = snapshot.script_source_hash
               }))
      in
      let%bind frame = Agent_session.Background_job_event.frame ~state ~observer job in
      let%bind payload =
        Chat_response.Background_delivery.capture frame
        |> Chatml.Chatml_value_codec.Snapshot.of_value
        |> Result.map ~f:Chatml.Chatml_value_codec.Snapshot.to_jsonaf
        |> Result.map_error ~f:Agent_protocol.Error.invalid_request
      in
      runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
        Agent_session.Session_actor.deliver_job
          ~expected:before
          ~expected_job:job
          ?moderator_manager:runtime.moderator_manager
          t.actor
          ~job_id:job.id
          ~generation:job.generation
          ~moderator_snapshot:
            (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
        |> Result.map ~f:ignore)
      |> Result.map ~f:ignore)
;;

let deliver_run_job_completion t delivery =
  with_cancellable_access t (fun runtime ->
    let open Result.Let_syntax in
    Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
      let%bind payload =
        Agent_session.Run_job_delivery.frame delivery
        |> Chat_response.Background_delivery.capture
        |> Chatml.Chatml_value_codec.Snapshot.of_value
        |> Result.map ~f:Chatml.Chatml_value_codec.Snapshot.to_jsonaf
        |> Result.map_error ~f:Agent_protocol.Error.invalid_request
      in
      runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
        Agent_session.Session_actor.enqueue_run_job_delivery
          t.actor
          ~delivery
          ~before
          ~after:(Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
      |> Result.map ~f:ignore))
;;

let deliver_background_job_completion t (job : Agent_protocol.Job.t) =
  with_cancellable_access t (fun runtime ->
    let open Result.Let_syntax in
    let%bind state = Agent_session.Session_actor.state t.actor in
    let standalone =
      match
        Option.bind job.launch ~f:(fun launch ->
          Agent_protocol.Job.Birth_parent.callback_owner launch.owner)
      with
      | Some (Invocation id) ->
        List.exists state.invocations ~f:(fun invocation ->
          Agent_protocol.Id.Invocation.equal invocation.context.id id
          && Option.is_some invocation.completion_contract)
      | _ -> false
    in
    match standalone, runtime.standalone_completion with
    | true, Some deliver -> deliver job
    | true, None ->
      Error
        (Agent_protocol.Error.invalid_request "standalone completion adapter unavailable")
    | false, _ -> deliver_moderated_background_job_completion t runtime job)
;;

module Goal_wake_consumer = struct
  module Actor = Agent_session.Session_actor
  module Builder = Agent_session.Runtime_builder

  let conflict message = Agent_protocol.Error.create Conflict ~message ~retryable:false ()

  let manager (runtime : Builder.t) =
    match runtime.moderator_manager with
    | Some manager
      when Option.is_some (Chat_response.Moderator_manager.extension_definition manager)
      -> Ok manager
    | Some _ | None ->
      Error
        (Agent_protocol.Error.create
           Permission_denied
           ~message:"Goal wake requires its installed extension Manager"
           ~retryable:false
           ())
  ;;

  let encode_frame frame =
    Chat_response.Background_delivery.capture frame
    |> Chatml.Chatml_value_codec.Snapshot.of_value
    |> Result.map ~f:Chatml.Chatml_value_codec.Snapshot.to_jsonaf
    |> Result.map_error ~f:Agent_protocol.Error.invalid_request
  ;;

  let enqueue_retained_job t runtime execution delivery =
    let open Result.Let_syntax in
    match Agent_session.Run_job_delivery.disposition delivery with
    | Pending ->
      Actor.with_moderator_checkpoint t.actor (fun () ->
        let%bind payload = encode_frame (Agent_session.Run_job_delivery.frame delivery) in
        runtime.Builder.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
          Actor.enqueue_goal_wake_job_delivery
            t.actor
            ~execution
            ~delivery
            ~before
            ~after:(Builder.encode_moderator_snapshot snapshot))
        |> Result.map ~f:ignore)
    | Enqueued _ -> Ok ()
    | Claimed _ | Retired _ ->
      Error (conflict "Goal Job wake has already been claimed or retired")
  ;;

  let enqueue_latest_job t runtime execution state job_id attempt =
    let open Result.Let_syntax in
    let%bind job =
      List.find state.Agent_session.Session_state.jobs ~f:(fun job ->
        Agent_protocol.Id.Job.equal job.Agent_protocol.Job.id job_id
        && Int.equal job.attempt attempt)
      |> Result.of_option ~error:(conflict "Goal Job wake occurrence is absent")
    in
    match job.delivery with
    | Not_required -> Error (conflict "Goal Job has no completion delivery")
    | Delivered _ -> Ok ()
    | Discarded _ -> Error (conflict "Goal Job completion was discarded")
    | Pending ->
      Actor.with_moderator_checkpoint t.actor (fun () ->
        let prepare ~before ~snapshot =
          Actor.deliver_goal_wake_job
            t.actor
            ~execution
            ~job
            ~before
            ~after:(Builder.encode_moderator_snapshot snapshot)
        in
        match job.kind with
        | Model_call ->
          runtime.Builder.enqueue_model_job_completion job ~prepare
          |> Result.map ~f:ignore
        | Async_tool ->
          let%bind manager = manager runtime in
          let%bind observer =
            Chat_response.Moderator_manager.invocation_observer manager
            |> Result.of_option ~error:(conflict "Goal Job moderator source is absent")
          in
          let%bind frame =
            Agent_session.Background_job_event.frame ~state ~observer job
          in
          let%bind payload = encode_frame frame in
          runtime.enqueue_internal_event payload ~prepare |> Result.map ~f:ignore
        | Nested_agent | Scheduled_event | Shell_process | Compaction ->
          Error (conflict "Goal Job kind has no supported completion adapter"))
  ;;

  let enqueue_job t runtime execution state wake =
    let open Result.Let_syntax in
    let%bind key =
      Agent_session.Run_job_delivery.Key.of_wake wake
      |> Result.of_option ~error:(conflict "Goal Job wake key is absent")
    in
    match
      Option.bind state.Agent_session.Session_state.run_state ~f:(fun index ->
        Agent_session.Run_state.find_job_delivery index key)
    with
    | Some delivery -> enqueue_retained_job t runtime execution delivery
    | None ->
      (match wake.Agent_protocol.Run_wake.occurrence with
       | Job_completion { job_id; attempt } ->
         enqueue_latest_job t runtime execution state job_id attempt
       | Delivered_timer _ | Subscription_delivery _ ->
         Error (conflict "Goal Job wake changed occurrence"))
  ;;

  let enqueue_timer t runtime execution state schedule_id =
    let open Result.Let_syntax in
    let%bind schedule =
      List.find state.Agent_session.Session_state.schedules ~f:(fun schedule ->
        Agent_protocol.Id.Schedule.equal schedule.Agent_protocol.Schedule.id schedule_id)
      |> Result.of_option ~error:(conflict "Goal timer wake occurrence is absent")
    in
    let%bind schedule =
      match schedule.status with
      | Scheduled ->
        Actor.claim_goal_wake_schedule t.actor ~execution ~schedule
        |> Result.bind
             ~f:(Result.of_option ~error:(conflict "Goal timer was not claimed"))
      | Delivering | Delivered -> Ok schedule
      | Cancelled | Failed _ -> Error (conflict "Goal timer is no longer deliverable")
    in
    match schedule.status with
    | Delivered -> Ok ()
    | Delivering ->
      let%bind manager = manager runtime in
      let%bind payload =
        Chat_response.Schedule_delivery.capture schedule
        |> Result.bind ~f:Chatml.Chatml_value_codec.Snapshot.of_value
        |> Result.map ~f:Chatml.Chatml_value_codec.Snapshot.to_jsonaf
        |> Result.map_error ~f:Agent_protocol.Error.invalid_request
      in
      Actor.with_moderator_checkpoint t.actor (fun () ->
        runtime.Builder.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
          Actor.complete_goal_wake_schedule
            t.actor
            ~execution
            ~expected:before
            ~expected_schedule:schedule
            ~moderator_manager:manager
            ~moderator_snapshot:(Builder.encode_moderator_snapshot snapshot)
          |> Result.map ~f:ignore)
        |> Result.map ~f:ignore)
    | Scheduled | Cancelled | Failed _ ->
      Error (conflict "Goal timer lost its actual delivering claim")
  ;;

  let run_queue_head t runtime execution =
    let open Result.Let_syntax in
    let%bind manager = manager runtime in
    let history = ref [] in
    let claim ~snapshot handle =
      Actor.with_current_goal_wake_queued_event_tools
        t.actor
        ~execution
        ~snapshot
        (fun
            ~executing
             ~policy_caller
             ~model_jobs
             ~allocate_inference
             ~retirement_reason
             ~event
             ~execute
             ~commit
           ->
           let%bind state = Actor.state t.actor in
           let%bind entries =
             Agent_session.History_codec.all_of_protocol
               state.conversation.canonical_history
           in
           history := entries;
           handle
             ~executing
             ~policy_caller
             ~model_jobs
             ~allocate_inference
             ~retirement_reason
             ~event
             ~execute
             ~commit)
    in
    Agent_session.Moderator_event.run_queued_idle
      ~model_recipes:runtime.Builder.model_recipes
      ~claim
      ~run_actions:(Actor.run_actions t.actor)
      ?script_tools:runtime.moderator_script_tools
      ~manager
      ~history:(fun () -> !history)
      ~available_tools:runtime.moderator_tools
      ~session_meta:`Null
      ~now:runtime.now
      ()
    |> Result.map ~f:ignore
  ;;

  let subscription t runtime execution ~notification_policy ~max_notifications =
    let open Result.Let_syntax in
    let%bind tools =
      runtime.Builder.moderator_script_tools
      |> Result.of_option ~error:(conflict "Goal subscription tools are absent")
    in
    let%bind manager = manager runtime in
    let%bind state = Actor.state t.actor in
    let%bind proposal =
      Agent_session.Notification_delivery.prepare_idle_for_runtime
        ~state
        ~source:(Chat_response.Moderator_manager.invocation_observer manager)
        ~current_capabilities:(Agent_session.Script_tool_calls.current_capabilities tools)
        ~policy:notification_policy
        ~max_count:max_notifications
    in
    Actor.deliver_goal_wake_notifications t.actor ~execution proposal
    |> Result.map ~f:ignore
  ;;

  let run t runtime execution ~notification_policy ~max_notifications =
    let open Result.Let_syntax in
    let%bind wake = Actor.Goal_execution_runtime.wake execution in
    let%bind state = Actor.state t.actor in
    let%bind run =
      Option.bind state.run_state ~f:(fun index ->
        Agent_session.Run_state.find index wake.run_id)
      |> Result.of_option ~error:(conflict "Goal wake Run is absent after lifecycle")
    in
    let%bind () =
      match run.lifecycle with
      | Waiting current when Agent_protocol.Run_wake.equal current wake ->
        (match wake.occurrence with
         | Subscription_delivery _ ->
           subscription t runtime execution ~notification_policy ~max_notifications
         | Delivered_timer { schedule_id; _ } ->
           let%bind () = enqueue_timer t runtime execution state schedule_id in
           run_queue_head t runtime execution
         | Job_completion _ ->
           let%bind () = enqueue_job t runtime execution state wake in
           run_queue_head t runtime execution)
      | Waiting _ | Terminal _ | Admitted | Active ->
        (* The actual finisher validates a new Wait or writes the same genuine
           Finish aggregate. No second producer is enqueued after lifecycle use. *)
        Ok ()
    in
    Actor.finish_goal_wake_runtime t.actor ~execution
  ;;
end

let run_goal_wake_runtime t ~prepared ~execution ~notification_policy ~max_notifications =
  let module Actor = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let%bind () =
    match Actor.Goal_execution_runtime.kind execution with
    | Queued_wake | Subscription_wake -> Ok ()
    | Claimed_job ->
      Error (Agent_protocol.Error.invalid_request "claimed Job is not a wake")
  in
  let%bind result =
    with_joined_existing_goal_runtime
      t
      (Runtime_request.Existing_goal_execution { prepared; execution })
      ~run_resume:(fun execution resume ->
        Actor.run_goal_execution_runtime_resume t.actor ~execution ~resume)
      (fun runtime execution _construction ->
         Eio.Mutex.use_ro t.moderator_work_mutex (fun () ->
           with_activity runtime (fun () ->
             Goal_wake_consumer.run
               t
               runtime
               execution
               ~notification_policy
               ~max_notifications)))
  in
  match result with
  | Construction_result.Built result -> Ok result
  | Callback_completed _ -> Actor.finish_goal_wake_runtime t.actor ~execution
;;

module Disposal_report = struct
  type t = Session_disposal_report.t

  type disposition =
    | Retained
    | Joined
  [@@deriving equal, sexp_of]

  let disposition t =
    match Disposal_report_data.disposition t with
    | Retained -> Retained
    | Disposed -> Joined
  ;;

  let failures t =
    List.map (Disposal_report_data.failures t) ~f:(function
      | Agent_session.Callback_cleanup.Failure.Protocol error ->
        Scope_outcome.Cleanup.Failure.Protocol error
      | Raised (exn, trace) -> Raised (exn, trace))
  ;;
end

let resume_disposal_report report =
  match Disposal_report_data.take_legacy_failure_exn report with
  | Some (Agent_session.Callback_cleanup.Failure.Raised (exn, trace)) ->
    Exn.raise_with_original_backtrace exn trace
  | Some (Protocol error) -> raise (Cleanup_failed error)
  | None ->
    (match Disposal_report.disposition report with
     | Joined -> ()
     | Retained -> raise (Cleanup_failed (background_busy ())))
;;

let cached_disposal_report t =
  Eio.Mutex.use_ro t.mutex (fun () -> Disposal_cache.current t.disposal_cache)
;;

module Close_prelude = struct
  type t =
    | Cell_barrier of Resource_retirement.t
    | Finished
    | Prepared of
        { leases : background_lease list
        ; cancel_administration : (unit -> unit) option
        }
end

let close_uncached ?invocation ?handoff t =
  let control f =
    match invocation with
    | None -> f ()
    | Some invocation -> Close_invocation.control invocation f
  in
  let unit f =
    match invocation with
    | None -> f ()
    | Some invocation -> Close_invocation.unit invocation f
  in
  let handed_off () =
    Option.exists handoff ~f:(fun handoff ->
      Option.is_some handoff.Joined_close_handoff.attempt)
  in
  let retirement_owns_prelude () = resource_retirement_pending t || handed_off () in
  let rec prepare () =
    let disposition =
      with_owner_lock t ~protect:true (fun () ->
        match
          Option.filter
            (Option.first_some t.pending_resource_retirement t.unloading)
            ~f:(fun attempt -> not (Retirement_coverage.is_whole_owner attempt.coverage))
        with
        | Some attempt -> Close_prelude.Cell_barrier attempt
        | None ->
          (* The barrier decision and permanent exclusion share this exact lock.
             A newly elected Cell cannot slip between observation and mutation. *)
          Option.iter t.startup_settlement_request ~f:Startup_settlement_request.refuse;
          if t.close_finished
          then Close_prelude.Finished
          else (
            Option.iter invocation ~f:(fun invocation ->
              (* Retain the actual producers promoted/joined by this permanent
                 prelude before cancellation or reply can clear their association. *)
              Option.iter
                t.pending_resource_retirement
                ~f:(Close_invocation.attempt invocation);
              Option.iter t.unloading ~f:(Close_invocation.attempt invocation);
              Option.iter t.failed_source_cleanup ~f:(Close_invocation.root invocation);
              Option.iter t.prepared_source ~f:(fun stage ->
                Close_invocation.root invocation stage.extent.recovery));
            let cancel_administration =
              Option.bind t.prepared_source ~f:(fun stage ->
                match stage.retirement with
                | Source_stage_retirement.Cancellable -> None
                | Administration _ -> stage.cancel_lease)
            in
            if not t.closed then replace_foreground_retirement t None;
            t.closed <- true;
            Option.iter handoff ~f:(fun handoff -> t.joined_close_handoff <- Some handoff);
            if not (resource_retirement_pending t)
            then
              Option.iter t.unloading ~f:(fun attempt ->
                match attempt.Resource_retirement_data.phase, attempt.coverage with
                | (Queued | Running), Whole_owner -> attempt.strength <- All_resources
                | Finished _, (Whole_owner | Cell _) | (Queued | Running), Cell _ -> ());
            Option.iter t.administration_transition ~f:Administration_transition.retire;
            Option.iter t.prepared_source ~f:close_source_stage;
            Option.iter t.restored_policy ~f:Restored_policy_anchor.revoke;
            let leases =
              match t.before_unload, t.unloading, t.background_leases with
              | Some _, _, _ -> []
              | None, _, leases -> leases
            in
            Option.iter invocation ~f:(fun invocation ->
              List.iter leases ~f:(fun lease ->
                Close_invocation.root invocation lease.recovery));
            Close_prelude.Prepared { leases; cancel_administration }))
    in
    match disposition with
    | Close_prelude.Finished -> None
    | Prepared { leases; cancel_administration } -> Some (leases, cancel_administration)
    | Cell_barrier attempt ->
      Resource_retirement.run attempt;
      let outcome =
        match invocation with
        | None -> Resource_retirement.await attempt
        | Some invocation ->
          Close_invocation.attempt invocation attempt;
          let actual = Close_invocation.await invocation attempt in
          Close_invocation.project_retirement invocation actual
      in
      (match outcome with
       | Ok () -> prepare ()
       | Error error -> raise (Cleanup_failed error))
  in
  match prepare () with
  | None -> ()
  | Some (leases, cancel_administration) ->
    (match
       control (fun () ->
         Agent_session.Session_actor.retire_runtime_worker t.actor ~closing:true)
     with
     | Error error -> raise (Cleanup_failed error)
     | Ok retirement ->
       with_owner_lock t ~protect:true (fun () ->
         (* An exact Cell accepted during the Actor RPC must record the
            receipt through its own selected Actor cleanup step. *)
         if not (handed_off ()) then replace_foreground_retirement t (Some retirement)));
    (* Explicit close must cancel this exact pending admin source even when
       other Factory leases await dependency retirement. Never invoke cancellation
       callbacks under the Owner mutex. Admission before registration still holds
       that mutex; an exceptional admission has no later source callback and its
       captured-stage finalizer releases the administrative completion. *)
    Option.iter cancel_administration ~f:(fun cancel -> unit cancel);
    if not (retirement_owns_prelude ())
    then List.iter leases ~f:(fun lease -> unit lease.cancel);
    (* Never wait on remote root/factual writers under the Owner mutex. In
       particular a native ACK may own an unlaunched Starting operation whose
       retirement join is completed only by this retained cleanup. *)
    (* Cancellation is not a producer join. Finite close leaves constructor
       factual work to the joined path, even when that producer already exited.
       The callback's own terminal writer cannot precede its retained Jobs. *)
    let defer_prelude = retirement_owns_prelude () in
    if (not defer_prelude) && not (factual_cleanup_pending t)
    then (
      match
        control (fun () -> Agent_session.Session_actor.drain_callback_retirement t.actor)
      with
      | Ok () -> ()
      | Error failure -> raise (Cleanup_failed failure));
    if not (defer_prelude || handed_off ())
    then (
      (match
         control (fun () -> Agent_session.Session_actor.drain_descendant_cleanup t.actor)
       with
       | Ok () -> ()
       | Error failure -> raise (Cleanup_failed failure));
      let cleanup = Cleanup_request.create () in
      let detached =
        Scope_outcome.capture (fun () ->
          Ok
            (with_owner_lock t ~protect:true (fun () ->
               match t.before_unload, t.unloading, t.background_leases with
               | None, None, []
                 when (not (resource_retirement_pending t))
                      && (not (handed_off ()))
                      && foreground_retirement_finished t ->
                 retire_runtime_locked t cleanup
               | _ -> Ok ())))
      in
      let detach_primary =
        match detached with
        | Scope_outcome.Returned (Ok (Ok ())) -> Scope_outcome.Returned (Ok ())
        | Returned (Ok (Error (exn, trace))) | Raised (exn, trace) -> Raised (exn, trace)
        | Returned (Error error) -> Returned (Error error)
      in
      let detach_origin =
        Option.bind invocation ~f:(fun invocation ->
          Close_invocation.observe invocation detach_primary ~origin:None)
      in
      (* An outer selection escape still stops before the queue, as before.
         A returned detach failure remains DATA until the independent batch ran. *)
      let retired =
        match detached with
        | Scope_outcome.Raised _ | Returned (Error _) ->
          Option.iter invocation ~f:(fun invocation ->
            Close_invocation.select invocation detach_origin);
          (match Scope_outcome.resume_primary detached with
           | Ok retired -> retired
           | Error error -> raise (Cleanup_failed error))
        | Returned (Ok retired) -> retired
      in
      let batch = run_cleanup_request_reported t cleanup in
      let batch_projection = Cleanup_batch.projection batch in
      Option.iter invocation ~f:(fun invocation ->
        Close_invocation.origin invocation batch_projection.origin);
      let cleaned =
        match batch.Cleanup_batch.original with
        | Raised (exn, trace) ->
          let origin = Cleanup_batch.resume_origin batch in
          Option.iter invocation ~f:(fun invocation ->
            Close_invocation.origin invocation origin;
            Close_invocation.select invocation origin);
          Exn.raise_with_original_backtrace exn trace
        | Returned outcome -> outcome
      in
      with_owner_lock t ~protect:true (fun () ->
        refresh_settled_source_cleanup_locked t;
        if
          Result.is_ok retired
          && Result.is_ok cleaned
          && (not (handed_off ()))
          && Option.is_none t.before_unload
          && Option.is_none t.unloading
          && (not (resource_retirement_pending t))
          && Option.is_none t.prepared_source
          && Option.is_none t.administration_transition
          && List.is_empty t.background_leases
          && List.is_empty t.retired_resources
          && Option.is_none t.restored_policy
          && List.is_empty t.native_loans
          && (not (policy_cleanup_pending t))
          && (not (factual_cleanup_pending t))
          && foreground_retirement_finished t
          && not (failed_source_cleanup_pending t)
        then t.close_finished <- true);
      match invocation with
      | None ->
        raise_cleanup
          (match retired with
           | Error _ -> retired
           | Ok () -> cleaned)
      | Some invocation ->
        let outcome, origin =
          match retired with
          | Error _ -> retired, detach_origin
          | Ok () -> cleaned, batch_projection.origin
        in
        Close_invocation.cleanup_exn invocation outcome ~origin)
;;

let close t =
  match cached_disposal_report t with
  | Some report -> resume_disposal_report report
  | None -> close_uncached t
;;

let join_selected_close_retirement_observed t ~invocation attempt =
  with_owner_lock t ~protect:true (fun () -> Close_invocation.attempt invocation attempt);
  Resource_retirement.run attempt;
  let observed = Close_invocation.await invocation attempt in
  let outcome = Close_invocation.project_retirement invocation observed in
  { Retirement_join.attempt; outcome; origin = observed.origin }
;;

let rec join_close_retirement_observed ?handoff ~invocation t =
  let open Result.Let_syntax in
  let%bind selected =
    match handoff with
    | Some attempt -> Ok (Some (Resource_retirement.Join attempt))
    | None ->
      with_owner_lock t ~protect:true (fun () ->
        let current = Option.first_some t.pending_resource_retirement t.unloading in
        Option.iter current ~f:(Close_invocation.attempt invocation);
        match current with
        | Some attempt ->
          (match attempt.Resource_retirement_data.phase with
           | Finished outcome
             when Retirement_coverage.is_whole_owner attempt.coverage
                  && attempt.delivered
                  && not (Resource_retirement_data.is_success outcome) ->
             let%map claim = retry_signalled_resource_retirement t ~failed:attempt in
             Some claim
           | Queued | Running | Finished _ -> Ok (Some (Resource_retirement.Join attempt)))
        | None -> Ok None)
  in
  let%bind joined =
    match selected with
    | None -> unload_and_wait_observed ~invocation t
    | Some claim ->
      let attempt =
        match claim with
        | Resource_retirement.Run work -> Resource_retirement.completion work
        | Join attempt -> attempt
      in
      Ok (join_selected_close_retirement_observed t ~invocation attempt)
  in
  if
    Result.is_ok joined.outcome
    && (resource_retirement_pending t
        || not (Retirement_coverage.is_whole_owner joined.attempt.coverage))
  then join_close_retirement_observed ~invocation t
  else Ok joined
;;

let source_custody_joined source =
  match source.Recovery_data.phase with
  | Original_running _ | Retrying _ -> false
  | Complete _ ->
    let evidence = Recovery_data.Observation.scope source in
    Cleanup_evidence.custody_joined evidence
    && Original_cleanup_report.diagnostics_complete
         (Cleanup_evidence.child_view evidence ~diagnostics_complete:true)
;;

(* This only admits the existing remaining close suffix. It is not the final
   disposal receipt: every subsequent actual stage still has to complete. *)
let diagnostic_join_permits_suffix_locked t (joined : Retirement_join.t) =
  if
    (not t.closed)
    || (not (phys_equal joined.attempt.mutex t.mutex))
    || not (Owner_affinity.same t.affinity joined.attempt.recovery.affinity)
  then false
  else (
    match joined.attempt.recovery.phase with
    | Original_running _ | Retrying _ -> false
    | Complete completed ->
      (match completed.completion, completed.captured.disposal_check with
       | Retirement { required = All_resources; entered = Complete_resources }, Some check
         when check.physical_ready && check.source_custody_joined ->
         let same_source =
           match check.source, t.failed_source_cleanup with
           | None, None -> true
           | Some source, None ->
             Owner_affinity.same t.affinity source.affinity
             && source_custody_joined source
           | Some source, Some actual when phys_equal source actual ->
             source_custody_joined source
           | None, Some _ | Some _, Some _ -> false
         in
         same_source
         && Cleanup_evidence.custody_joined
              (Recovery_data.Observation.scope joined.attempt.recovery)
       | ( (Lease _ | Stage _ | Administration_transition | Construction | Retirement _)
         , (None | Some _) ) -> false))
;;

let diagnostic_join_permits_suffix t joined =
  with_owner_lock t ~protect:true (fun () ->
    diagnostic_join_permits_suffix_locked t joined)
;;

type close_reporting =
  | Strict
  | Reported

module Startup_settlement = struct
  module A = Agent_session.Session_actor
  module D = Recovery_data
  module O = Recovery_data.Observation

  let positive = function
    | Some (Scope_outcome.Returned (Ok _)) -> true
    | None | Some (Returned (Error _) | Raised _) -> false
  ;;

  let joined_snapshot = function
    | Scope_outcome.Returned (Ok snapshot) ->
      A.Retirement_cleanup.Snapshot.equal_progress
        (A.Retirement_cleanup.Snapshot.progress snapshot)
        Joined
    | Returned (Error _) | Raised _ -> false
  ;;

  (* Every item is an actual original stage, not the historical physical-ready
     summary. Optional physical duties are checked by the SAME canonical recipe. *)
  let stages_completed (completed : D.completed) (step : Actor_cleanup_step.t) =
    let required position =
      match O.steps completed.captured position with
      | [] -> false
      | selected ->
        List.for_all selected ~f:(fun (step : Original_cleanup_step.t) ->
          positive step.original)
    in
    let actor =
      positive step.initial
      && positive step.foreground
      && Option.exists step.final ~f:joined_snapshot
      &&
      match A.Retirement_cleanup.snapshot step.cleanup with
      | Error _ -> false
      | Ok snapshot ->
        A.Retirement_cleanup.Snapshot.equal_progress
          (A.Retirement_cleanup.Snapshot.progress snapshot)
          Joined
    in
    let leases =
      List.for_all completed.captured.leases_rev ~f:(fun (selected : D.lease) ->
        Option.is_some (Eio.Promise.peek selected.finished)
        &&
        match selected.role with
        | Source_lexical -> false
        | Retirement_lease { cancel; _ } -> positive cancel.original)
    in
    actor
    && leases
    && List.for_all
         [ O.Step_position.Complete_selection
         ; Complete_constructor
         ; Complete_callback
         ; Complete_descendant
         ; Complete_inference
         ]
         ~f:required
    && List.exists completed.captured.batches_rev ~f:(fun batch ->
      match batch.Recovery_batch.role with
      | Retirement_complete -> true
      | Lease_admission
      | Lease_detachment
      | Source_entry
      | Construction_exit
      | Construction_local _
      | Ordinary_warm
      | Source_exit
      | Administration_prefix
      | Administration_before_commit
      | Administration_exit
      | Retirement_ordinary -> false)
  ;;

  let source_complete (source : D.t) =
    match source.phase with
    | D.Complete completed -> Some completed
    | Original_running _ | Retrying _ -> None
  ;;

  let same_source t (candidate : D.startup_candidate) =
    Option.exists t.failed_source_cleanup ~f:(phys_equal candidate.source)
  ;;

  (* Fresh read-only checks never select new cleanup targets. The Actor also
     verifies its exact original scopes and permanent election independently. *)
  let owner_quiescent_locked t attempt candidate =
    t.closed
    && phys_equal t.mutex attempt.Resource_retirement_data.mutex
    && Owner_affinity.same t.affinity attempt.recovery.affinity
    && same_source t candidate
    && Option.for_all t.pending_resource_retirement ~f:(phys_equal attempt)
    && Option.for_all t.unloading ~f:(phys_equal attempt)
    && Option.is_none t.prepared_source
    && Option.is_none t.administration_transition
    && List.is_empty t.background_leases
    && List.is_empty t.retired_resources
    && Option.is_none t.binding
    && Option.is_none t.restored_policy
    && Option.is_none t.native_anchor
    && List.is_empty t.native_loans
    && (not (policy_cleanup_pending t))
    && (not (factual_cleanup_pending t))
    && foreground_retirement_finished t
  ;;

  let still_completed (pending : Startup_settlement_request.t) =
    match pending.attempt.recovery.phase, pending.attempt.phase with
    | D.Complete completed, Resource_retirement_data.Finished _ ->
      phys_equal completed pending.completed
      && stages_completed completed pending.actor_step
    | (Original_running _ | Retrying _), _ | Complete _, (Queued | Running) -> false
  ;;

  let prerequisites_locked t (pending : Startup_settlement_request.t) =
    still_completed pending
    && owner_quiescent_locked t pending.attempt pending.candidate
    && O.startup_prerequisites pending.candidate
  ;;

  let create_locked t (joined : Retirement_join.t) =
    match joined.outcome, joined.attempt.phase, joined.attempt.recovery.phase with
    | Error _, Resource_retirement_data.Finished _, D.Complete completed ->
      (match
         ( completed.completion
         , completed.captured.disposal_check
         , completed.captured.actor_cleanup )
       with
       | ( Retirement { required = All_resources; entered = Complete_resources }
         , Some check
         , Some actor_step )
         when check.physical_ready ->
         Option.bind check.source ~f:(fun source ->
           Option.bind (source_complete source) ~f:(fun source_completed ->
             let candidates =
               List.filter source_completed.captured.constructions_rev ~f:(fun receipt ->
                 match receipt.Construction_cleanup.phase with
                 | Closed (Attempted (Returned (Error _) | Raised _)) -> true
                 | Awaiting_close | Close_failed _
                 | Closed (Unattempted | Admission_retained | Attempted (Returned (Ok ())))
                   -> false)
             in
             match candidates with
             | [ construction ] ->
               let candidate =
                 { D.retirement = joined.attempt.recovery; check; source; construction }
               in
               if
                 (not (stages_completed completed actor_step))
                 || (not (owner_quiescent_locked t joined.attempt candidate))
                 || not (O.startup_prerequisites candidate)
               then None
               else (
                 match
                   A.Failed_startup_request.create
                     t.actor
                     ~construction:construction.construction
                     ~retirement:actor_step.cleanup
                 with
                 | Error _ -> None
                 | Ok request ->
                   let pending =
                     { Startup_settlement_request.attempt = joined.attempt
                     ; completed
                     ; candidate
                     ; actor_step
                     ; request
                     ; first_reply = None
                     }
                   in
                   t.startup_settlement_request <- Some pending;
                   Some pending)
             | [] | _ :: _ :: _ -> None))
       | ( (Lease _ | Stage _ | Administration_transition | Construction | Retirement _)
         , (None | Some _)
         , (None | Some _) ) -> None)
    | Ok (), _, _
    | Error _, (Queued | Running), _
    | Error _, Finished _, (Original_running _ | Retrying _) -> None
  ;;

  let publish_locked t (pending : Startup_settlement_request.t) release =
    if
      (not (Option.exists t.startup_settlement_request ~f:(phys_equal pending)))
      || (not (prerequisites_locked t pending))
      || not
           (A.Failed_startup_disposal.same_construction
              release
              ~construction:pending.candidate.construction.construction)
    then raise (Cleanup_failed (background_busy ()));
    (* Only the actual Actor receipt changes normal custody. Original phases,
       check flags, snapshots and diagnostics remain untouched. *)
    pending.candidate.construction.startup_release <- Some release;
    pending.completed.current.startup_settlement
    <- Some { D.candidate = pending.candidate; release }
  ;;

  let resume ?startup_failure t (pending : Startup_settlement_request.t) =
    let capture_failure diagnostic =
      Option.iter startup_failure ~f:(fun captured -> captured := diagnostic)
    in
    let release =
      match A.Failed_startup_request.released pending.request with
      | Error error -> raise (Cleanup_failed error)
      | Ok (Some release) -> release
      | Ok None ->
        if
          not (with_owner_lock t ~protect:true (fun () -> prerequisites_locked t pending))
        then (
          capture_failure
            (Option.bind pending.first_reply ~f:(fun reply -> reply.diagnostic));
          Startup_settlement_request.refuse pending);
        (* Outside Owner mutex. The exact request was retained before this RPC.
           A lost reply is still a real failure for THIS invocation. *)
        let reply =
          Scope_outcome.capture (fun () ->
            A.release_failed_startup_custody t.actor ~request:pending.request)
        in
        let diagnostic =
          match reply with
          | Scope_outcome.Returned (Ok _) -> None
          | Returned (Error error) ->
            Some (Disposal_report_data.Diagnostic.create [ Protocol error ])
          | Raised (exn, trace) ->
            Some (Disposal_report_data.Diagnostic.create [ Raised (exn, trace) ])
        in
        with_owner_lock t ~protect:true (fun () ->
          if Option.is_none pending.first_reply
          then pending.first_reply <- Some { outcome = reply; diagnostic });
        capture_failure diagnostic;
        (match Scope_outcome.resume_primary reply with
         | Ok release -> release
         | Error error -> raise (Cleanup_failed error))
    in
    with_owner_lock t ~protect:true (fun () -> publish_locked t pending release);
    { Retirement_join.attempt = pending.attempt
    ; outcome = Error (background_busy ())
    ; origin = None
    }
  ;;
end

let close_joined_suffix ~invocation t =
  let leases =
    with_owner_lock t ~protect:true (fun () ->
      let leases = t.background_leases in
      List.iter leases ~f:(fun lease -> Close_invocation.root invocation lease.recovery);
      leases)
  in
  List.iter leases ~f:(fun lease -> Close_invocation.unit invocation lease.cancel);
  List.iter leases ~f:(fun lease ->
    Close_invocation.unit invocation (fun () -> Eio.Promise.await lease.finished));
  Close_invocation.result_exn invocation (drain_policy_claim_observed t);
  Close_invocation.control_exn invocation (fun () ->
    Agent_session.Session_actor.drain_constructor_model_retirement t.actor);
  Close_invocation.control_exn invocation (fun () ->
    Agent_session.Session_actor.drain_callback_retirement t.actor);
  Close_invocation.control_exn invocation (fun () ->
    Agent_session.Session_actor.drain_descendant_cleanup t.actor);
  Close_invocation.control_exn invocation (fun () ->
    Agent_session.Session_actor.drain_inference_usage t.actor);
  Option.iter
    (with_owner_lock t ~protect:true (fun () -> t.foreground_retirement))
    ~f:(fun retirement ->
      Close_invocation.control_exn invocation (fun () ->
        Agent_session.Session_actor.Runtime_retirement.await retirement));
  Close_invocation.result_exn invocation (close_restored_policy_observed t);
  Close_invocation.result_exn invocation (close_native_anchor_observed t);
  Close_invocation.result_exn invocation (retry_native_releases_observed t);
  Close_invocation.result_exn invocation (close_pending_policy_resource_reported t);
  let cleanup = Cleanup_request.create () in
  Close_invocation.unit invocation (fun () ->
    with_owner_lock t ~protect:true (fun () -> retry_retired_resources_locked t cleanup));
  let batch = run_cleanup_request_reported t cleanup in
  let origin = Cleanup_batch.resume_origin batch in
  match batch.Cleanup_batch.original with
  | Raised (exn, trace) ->
    Close_invocation.origin invocation origin;
    Close_invocation.select invocation origin;
    Exn.raise_with_original_backtrace exn trace
  | Returned outcome -> Close_invocation.cleanup_exn invocation outcome ~origin
;;

(* Only actual handles consumed by this close tail, not merely captured report
   roots. Kept locally: no Owner registry or retry authority is introduced. *)
module Close_retirement_tail = struct
  type t = Resource_retirement.t list

  let create (joined : Retirement_join.t) = [ joined.attempt ]
  let contains t attempt = List.mem t attempt ~equal:phys_equal
  let add t attempt = attempt :: t
end

module Close_final_disposition = struct
  type t =
    | Complete
    | Join_current of
        { attempt : Resource_retirement.t
        ; tail : Close_retirement_tail.t
        }
    | Refused of Agent_protocol.Error.t
end

let finish_joined_close t ~invocation ~reporting ~tail (joined : Retirement_join.t) =
  with_owner_lock t ~protect:true (fun () ->
    refresh_settled_source_cleanup_locked t;
    let retirement_fences_clear =
      (not (resource_retirement_pending t)) && Option.is_none t.unloading
    in
    let retirement_fences_settled =
      retirement_fences_clear
      ||
      match reporting with
      | Strict -> false
      | Reported ->
        (* A negative original All_resources report retains its admission
                  fences. Only THIS elected attempt may remain after its actual
                  custody proof succeeds; no other pending work is exempted. *)
        Option.for_all t.pending_resource_retirement ~f:(phys_equal joined.attempt)
        && Option.for_all t.unloading ~f:(phys_equal joined.attempt)
        && (diagnostic_join_permits_suffix_locked t joined
            || Option.exists t.startup_settlement_request ~f:(fun pending ->
              phys_equal pending.Startup_settlement_request.attempt joined.attempt
              && Option.is_some pending.completed.current.startup_settlement
              && Startup_settlement.prerequisites_locked t pending
              && source_custody_joined pending.candidate.source
              && Cleanup_evidence.custody_joined
                   (Recovery_data.Observation.scope joined.attempt.recovery)))
    in
    if
      retirement_fences_settled
      && Option.is_none t.prepared_source
      && Option.is_none t.administration_transition
      && List.is_empty t.background_leases
      && List.is_empty t.retired_resources
      && Option.is_none t.restored_policy
      && List.is_empty t.native_loans
      && (not (policy_cleanup_pending t))
      && not (factual_cleanup_pending t)
    then (
      (match t.failed_source_cleanup with
       | None -> if retirement_fences_clear then t.close_finished <- true
       | Some source ->
         (match reporting with
          | Reported
            when Option.is_none t.binding
                 && Option.is_none t.native_anchor
                 && foreground_retirement_finished t
                 && source_custody_joined source -> ()
          | Strict | Reported -> raise (Cleanup_failed (background_busy ()))));
      invocation.Close_invocation.completed_stamp
      <- Some (Disposal_cache.stamp t.disposal_cache);
      Close_final_disposition.Complete)
    else (
      match tail, t.pending_resource_retirement, t.unloading with
      | Some tail, None, Some attempt
        when (not retirement_fences_settled)
             && t.closed
             && phys_equal attempt.mutex t.mutex
             && Int.equal attempt.domain t.domain
             && Owner_affinity.same attempt.recovery.affinity t.affinity
             && (not (phys_equal attempt joined.attempt))
             && not (Close_retirement_tail.contains tail attempt) ->
        (match attempt.coverage, attempt.strength with
         | Whole_owner, All_resources ->
           (* This exact producer was accepted while the completed suffix
              yielded. Capture before unlocking; only its actual run/await is
              permitted, never a new election or implicit retry. *)
           Close_invocation.attempt invocation attempt;
           Join_current { attempt; tail = Close_retirement_tail.add tail attempt }
         | Whole_owner, Ordinary | Cell _, (Ordinary | All_resources) ->
           Refused (background_busy ()))
      | (None | Some _), (None | Some _), (None | Some _) -> Refused (background_busy ())))
;;

let close_and_wait_body ~invocation ~reporting t =
  let startup_resume pending =
    let startup_failure = ref None in
    let primary =
      Scope_outcome.capture (fun () ->
        Ok (Startup_settlement.resume ~startup_failure t pending))
    in
    let origin =
      Option.map !startup_failure ~f:(fun diagnostic ->
        { Physical_origin.diagnostics = [ diagnostic ]; primary = Some diagnostic })
    in
    match Close_invocation.resume invocation primary ~origin with
    | Ok joined -> joined
    | Error error -> raise (Cleanup_failed error)
  in
  let rec finish_with_tail tail joined =
    let selected =
      Close_invocation.control_exn invocation (fun () ->
        match finish_joined_close t ~invocation ~reporting ~tail joined with
        | Close_final_disposition.Refused error -> raise (Cleanup_failed error)
        | Complete -> Ok None
        | Join_current { attempt; tail } -> Ok (Some (attempt, tail)))
    in
    match selected with
    | None -> ()
    | Some (attempt, tail) ->
      let joined = join_selected_close_retirement_observed t ~invocation attempt in
      if
        Result.is_ok joined.outcome
        ||
        match reporting with
        | Strict -> false
        | Reported -> diagnostic_join_permits_suffix t joined
      then
        (* The original suffix already ran. This actual All_resources producer
           supplies only its own completion; the unchanged locked final guard
           must still prove every remaining slot before stamping disposal. *)
        finish_with_tail (Some tail) joined
      else (
        match
          Close_invocation.resume
            invocation
            (Scope_outcome.Returned joined.outcome)
            ~origin:joined.origin
        with
        | Ok () -> invalid_arg "positive late retirement refused its final guard"
        | Error error -> raise (Cleanup_failed error))
  in
  (* Startup settlement keeps its existing guard-only finalization. Only the
     normal path below issues a tail after its actual close suffix returns. *)
  let finish joined = finish_with_tail None joined in
  Eio.Cancel.protect (fun () ->
    match
      Eio.Mutex.use_ro t.mutex (fun () ->
        if t.close_finished
        then
          invocation.Close_invocation.completed_stamp
          <- Some (Disposal_cache.stamp t.disposal_cache);
        t.close_finished)
    with
    | true -> ()
    | false ->
      let pending =
        with_owner_lock t ~protect:true (fun () -> t.startup_settlement_request)
      in
      (match pending, reporting with
       | Some pending, Strict ->
         (match Option.bind pending.first_reply ~f:(fun reply -> reply.diagnostic) with
          | None ->
            Close_invocation.unit invocation (fun () ->
              Startup_settlement_request.refuse pending)
          | Some diagnostic ->
            let origin =
              Some
                { Physical_origin.diagnostics = [ diagnostic ]
                ; primary = Some diagnostic
                }
            in
            Close_invocation.origin invocation origin;
            Close_invocation.select invocation origin;
            Startup_settlement_request.refuse pending)
       | Some pending, Reported -> finish (startup_resume pending)
       | None, (Strict | Reported) ->
         let handoff = Joined_close_handoff.create () in
         Exn.protect
           ~f:(fun () -> close_uncached ~invocation ~handoff t)
           ~finally:(fun () ->
             with_owner_lock t ~protect:true (fun () ->
               Option.iter handoff.attempt ~f:(Close_invocation.attempt invocation);
               if Option.exists t.joined_close_handoff ~f:(phys_equal handoff)
               then t.joined_close_handoff <- None));
         (match join_close_retirement_observed ?handoff:handoff.attempt ~invocation t with
          | Ok joined
            when Result.is_ok joined.outcome
                 ||
                 match reporting with
                 | Strict -> false
                 | Reported -> diagnostic_join_permits_suffix t joined ->
            close_joined_suffix ~invocation t;
            finish_with_tail (Some (Close_retirement_tail.create joined)) joined
          | Ok joined ->
            let pending =
              match reporting with
              | Strict -> None
              | Reported ->
                with_owner_lock t ~protect:true (fun () ->
                  Startup_settlement.create_locked t joined)
            in
            (match pending with
             | Some pending -> finish (startup_resume pending)
             | None ->
               (match
                  Close_invocation.resume
                    invocation
                    (Scope_outcome.Returned joined.outcome)
                    ~origin:joined.origin
                with
                | Ok () -> invalid_arg "positive permanent join did not enter its suffix"
                | Error error -> raise (Cleanup_failed error)))
          | Error error -> Close_invocation.control_exn invocation (fun () -> Error error))))
;;

let capture_close_invocation ~reporting t =
  let invocation =
    with_owner_lock t ~protect:true (fun () ->
      let invocation =
        Close_invocation.create ~prior:(Disposal_cache.prior t.disposal_cache)
      in
      Option.iter t.failed_source_cleanup ~f:(Close_invocation.root invocation);
      Option.iter t.startup_settlement_request ~f:(fun pending ->
        Close_invocation.attempt invocation pending.attempt;
        Close_invocation.root invocation pending.candidate.source;
        Option.iter
          (Option.bind pending.first_reply ~f:(fun reply -> reply.diagnostic))
          ~f:(fun diagnostic ->
            Close_invocation.origin
              invocation
              (Some { Physical_origin.diagnostics = [ diagnostic ]; primary = None })));
      invocation)
  in
  let outcome =
    match close_and_wait_body ~invocation ~reporting t with
    | () -> Agent_session.Close_observation.Returned ()
    | exception exn ->
      Agent_session.Close_observation.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  let report =
    with_owner_lock t ~protect:true (fun () ->
      let report = Close_invocation.report_locked invocation ~outcome in
      (match Disposal_report_data.disposition report with
       | Disposed ->
         t.disposal_cache
         <- Disposal_cache.publish
              t.disposal_cache
              ~completed:invocation.completed_stamp
              report;
         t.startup_settlement_request <- None
       | Retained -> ());
      report)
  in
  invocation, outcome, report
;;

let check_disposal_domain_exn t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "runtime disposal belongs to another creating domain"
;;

(* Joined close is external teardown. Finite close never holds this coordinator.
   Trusted capture is local assignment, not retained cleanup or a reentrant call. *)
let close_and_wait ?capture_report t =
  check_disposal_domain_exn t;
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_ro t.disposal_mutex (fun () ->
      match cached_disposal_report t with
      | Some report ->
        Option.iter capture_report ~f:(fun capture -> capture report);
        resume_disposal_report report
      | None ->
        let invocation, outcome, report = capture_close_invocation ~reporting:Strict t in
        Option.iter capture_report ~f:(fun capture -> capture report);
        Close_invocation.resume_strict invocation outcome))
;;

let close_and_wait_reported t =
  check_disposal_domain_exn t;
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_ro t.disposal_mutex (fun () ->
      match cached_disposal_report t with
      | Some report -> report
      | None ->
        let _, _, report = capture_close_invocation ~reporting:Reported t in
        report))
;;

module Cleanup_recovery = struct
  type owner = t
  type t = Recovery_data.t

  module Snapshot = Cleanup_snapshot

  let check_domain = check_cleanup_recovery_domain

  let original holder =
    let open Result.Let_syntax in
    let%map () = check_domain holder in
    match holder.Recovery_data.phase with
    | Original_running _ -> None
    | Complete completed | Retrying { before = completed; _ } -> Some completed.original
  ;;

  let original_view holder =
    let open Result.Let_syntax in
    let%map () = check_domain holder in
    match holder.Recovery_data.phase with
    | Original_running _ -> None
    | Complete _ | Retrying _ -> Some (Recovery_data.original_diagnostics holder)
  ;;

  let snapshot holder =
    let open Result.Let_syntax in
    let%map () = check_domain holder in
    match holder.Recovery_data.phase with
    | Original_running _ -> None
    | Complete completed | Retrying { before = completed; _ } -> Some completed.latest
  ;;

  let retry holder ~(owner : owner) ~after = retry_cleanup_recovery holder ~owner ~after
end
