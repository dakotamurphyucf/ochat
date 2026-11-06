open! Core

module Phase = struct
  type t =
    | Open
    | Sealed
    | Finished
end

module Registration = struct
  type phase =
    | Active
    | Joined

  type t =
    { handle : Inference_ledger.Handle.t
    ; configuration : Inference.Observation.Configuration.t
    ; mutable attempt : Inference_runtime.Attempt.t option
    ; goal_id : Agent_protocol.Id.Goal.t option
    ; policy_live : bool Atomic.t
    ; initial_preparation : Run_preparation.t option
    ; mutable phase : phase
    }

  let initial_preparation t = t.initial_preparation

  let is_active t =
    match t.phase with
    | Active -> true
    | Joined -> false
  ;;

  let join t =
    t.phase <- Joined;
    t.attempt <- None;
    Atomic.set t.policy_live false
  ;;

  let handle t = t.handle
  let configuration t = t.configuration
  let attempt t = t.attempt
  let acknowledge t attempt = t.attempt <- Some attempt
  let goal_id t = t.goal_id
  let policy_is_current t = Atomic.get t.policy_live
  let invalidate_policy t = Atomic.set t.policy_live false
end

type t =
  { issuing_actor : unit ref
  ; source : Transcript.Source_id.t
  ; generation : int
  ; mutable qualified_source : Transcript.Source_id.t option
  ; mutable phase : Phase.t
  ; mutable registrations : Registration.t Int64.Map.t
  }

module Host = struct
  let create ~issuing_actor ~source ~generation ~qualified_source =
    { issuing_actor
    ; source
    ; generation
    ; qualified_source = Some qualified_source
    ; phase = Open
    ; registrations = Int64.Map.empty
    }
  ;;

  let issuing_actor t = t.issuing_actor
  let source t = t.source
  let generation t = t.generation
  let phase t = t.phase
  let qualified_source t = t.qualified_source

  let registration t ~ordinal =
    Map.find t.registrations ordinal |> Option.filter ~f:Registration.is_active
  ;;

  let retained_registration t handle =
    Map.find t.registrations (Inference_ledger.Handle.ordinal handle)
    |> Option.filter ~f:(fun registration ->
      Inference_ledger.Handle.equal (Registration.handle registration) handle)
  ;;

  let retained_row ledger registration =
    let handle = Registration.handle registration in
    Option.exists
      (Inference_ledger.find ledger ~ordinal:(Inference_ledger.Handle.ordinal handle))
      ~f:(fun row ->
        Inference_ledger.Handle.equal (Inference_ledger.Row.handle row) handle)
  ;;

  let retain_joined t ledger =
    t.registrations
    <- Map.filter t.registrations ~f:(fun registration ->
         Registration.is_active registration || retained_row ledger registration)
  ;;

  let register t ~initial_preparation ~handle ~configuration ~goal_id =
    t.qualified_source
    <- Some (Transcript.Scope.key (Inference_ledger.Handle.scope handle)).source;
    t.registrations
    <- Map.set
         t.registrations
         ~key:(Inference_ledger.Handle.ordinal handle)
         ~data:
           { handle
           ; configuration
           ; attempt = None
           ; goal_id
           ; policy_live = Atomic.make true
           ; initial_preparation
           ; phase = Active
           }
  ;;

  let iter_registrations t ~f =
    Map.iter t.registrations ~f:(fun registration ->
      if Registration.is_active registration then f registration)
  ;;

  let iter_retained_registrations t ~f = Map.iter t.registrations ~f

  let release t ~ledger ~ordinal =
    match Map.find t.registrations ordinal with
    | None -> ()
    | Some registration ->
      if retained_row ledger registration
      then Registration.join registration
      else t.registrations <- Map.remove t.registrations ordinal
  ;;

  let seal t = t.phase <- Sealed

  let finish t =
    t.phase <- Finished;
    t.registrations <- Int64.Map.empty
  ;;
end
