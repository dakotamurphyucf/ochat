open! Core
module P = Agent_protocol
module Cause = Policy_cause_data

module Work = struct
  type t =
    | Root_operation of P.Id.Operation.t
    | Member of Cause.Allocation_work.t
  [@@deriving equal]
end

module Registered = struct
  type t =
    { owner : P.Session_ref.t
    ; source : Cause.Caller_source.t
    ; source_owner : Runtime_source_owner.Identity.t
    ; work : Work.t
    ; origin : P.Inference_origin.t
    ; local : Cause.Goal_target.t option
    ; inherited : Agent_store.Delegation_goal_origin.t option
    ; incoming : Cause.Forwarding.t option
    }

  let equal left right =
    P.Session_ref.equal left.owner right.owner
    && Cause.Caller_source.equal left.source right.source
    && Runtime_source_owner.Identity.equal left.source_owner right.source_owner
    && Work.equal left.work right.work
    && P.Inference_origin.equal left.origin right.origin
    && Option.equal Cause.Goal_target.equal left.local right.local
    && Option.equal
         Agent_store.Delegation_goal_origin.equal
         left.inherited
         right.inherited
    && Option.equal
         (fun left right ->
            Cause.Forwarding.equal left right
            && Document_schema.Json.equal
                 (Cause.Forwarding.to_json left)
                 (Cause.Forwarding.to_json right))
         left.incoming
         right.incoming
  ;;
end

type t =
  | Standalone_unrelated of Policy_standalone_source.t
  | Registered of Registered.t

let equal left right =
  match left, right with
  | Standalone_unrelated left, Standalone_unrelated right ->
    Policy_standalone_source.equal left right
  | Registered left, Registered right -> Registered.equal left right
  | Standalone_unrelated _, Registered _ | Registered _, Standalone_unrelated _ -> false
;;
