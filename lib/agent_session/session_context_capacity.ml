open! Core
module Capacity = Inference_runtime.Context_capacity

module Mode = struct
  type t =
    | Manual
    | Automatic
  [@@deriving equal, sexp_of]
end

module Host = struct
  type t =
    { stamp : unit ref
    ; mode_for_session : Agent_protocol.Session_ref.t -> Mode.t option
    ; targets : (Capacity.Policy.t * [ `Manual | `Automatic ]) list
    }

  type error = Duplicate_target [@@deriving equal, sexp_of]

  let off = { stamp = ref (); mode_for_session = (fun _ -> None); targets = [] }

  let create ~mode_for_session ~targets =
    let rec distinct seen = function
      | [] -> Ok { stamp = ref (); mode_for_session; targets }
      | (policy, _) :: other ->
        let target = Capacity.Policy.target policy in
        if List.mem seen target ~equal:Inference.Request.Target.equal
        then Error Duplicate_target
        else distinct (target :: seen) other
    in
    distinct [] targets
  ;;
end

type availability =
  { requested : Mode.t
  ; effective : Mode.t
  }

module Selection = struct
  type t =
    { host : unit ref
    ; target : Inference.Request.Target.t
    ; availability : availability
    ; policy : Capacity.Policy.t option
    }

  let select (host : Host.t) ~owner ~target =
    Option.map (host.mode_for_session owner) ~f:(fun requested ->
      let target_policy =
        List.find host.targets ~f:(fun (policy, _) ->
          Inference.Request.Target.equal (Capacity.Policy.target policy) target)
      in
      let effective =
        match requested, target_policy with
        | Automatic, Some (_, `Automatic) -> Mode.Automatic
        | Manual, _ | Automatic, (None | Some (_, `Manual)) -> Mode.Manual
      in
      { host = host.stamp
      ; target
      ; availability = { requested; effective }
      ; policy = Option.map target_policy ~f:fst
      })
  ;;

  let policy t = t.policy

  let equal a b =
    phys_equal a.host b.host
    && Inference.Request.Target.equal a.target b.target
    && Mode.equal a.availability.requested b.availability.requested
    && Mode.equal a.availability.effective b.availability.effective
  ;;
end

let availability (selection : Selection.t) = selection.availability

type status =
  | Disabled
  | Awaiting_preparation of availability
  | Stale_preparation of availability
  | Current of
      { availability : availability
      ; phase : Agent_protocol.Session_configuration.phase
      ; configuration : Inference.Observation.Configuration.t
      ; assessment : Capacity.Assessment.t
      }

module Port = struct
  type t = { read : unit -> (status, Agent_protocol.Error.t) Result.t }

  let create ~read = { read }
  let read t = t.read ()
end
