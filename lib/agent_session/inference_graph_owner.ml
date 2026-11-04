open! Core

module Phase = struct
  type t =
    | Open
    | Sealed
    | Finished
end

module Registration = struct
  type t =
    { handle : Inference_ledger.Handle.t
    ; configuration : Inference.Observation.Configuration.t
    ; mutable attempt : Inference_runtime.Attempt.t option
    ; goal_id : Agent_protocol.Id.Goal.t option
    ; policy_live : bool Atomic.t
    }

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
  let registration t ~ordinal = Map.find t.registrations ordinal

  let register t ~handle ~configuration ~goal_id =
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
           }
  ;;

  let iter_registrations t ~f = Map.iter t.registrations ~f
  let release t ~ordinal = t.registrations <- Map.remove t.registrations ordinal
  let seal t = t.phase <- Sealed

  let finish t =
    t.phase <- Finished;
    t.registrations <- Int64.Map.empty
  ;;
end
