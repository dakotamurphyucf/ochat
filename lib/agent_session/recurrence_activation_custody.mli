open! Core

(** Private controller-owned revocable custody. Only the actor retains its owner;
    no decoder, IDs, copied principal or durable facts construct external authority.
    All operations are synchronous on the owning actor's Eio domain. *)
module Owner : sig
  type t

  val create : unit -> t
end

type t

(** Structural/controller policy preparation only. The actor additionally requires
    actual same-Store target observation, reciprocal ACK, genuine Current, runtime
    admission and current nominal grace before publishing the cell. *)
val create
  :  owner:Owner.t
  -> state:Session_state.t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> origin:Recurrence_run_origin.t
  -> (t, Agent_protocol.Error.t) result

val origin : t -> Recurrence_run_origin.t
val definition : t -> Recurrence_definition.t
val entry : t -> Session_state.t -> (Recurrence_entry.t, Agent_protocol.Error.t) result
val nominal : t -> Recurrence_nominal.t
val is_live : t -> bool
val belongs_to : t -> owner:Owner.t -> bool
val same_occurrence : t -> t -> bool

(** Trusted nonyielding current original host policy, not a pure predicate. *)
val check_policy : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result

(** Repeat original authentication currentness and Send_messages scope, without
    the execution-selection policy. Currentness executes trusted nonyielding host
    code; the actor must check the pure basis after all such callbacks. *)
val check_authority : t -> (unit, Agent_protocol.Error.t) result

(** Callback-free owner/live cell and exact semantic controller basis. Checks no
    original authentication, policy or clock callback and grants no authority by
    itself. Store Current and target basis remain actor-owned. *)
val check_basis
  :  t
  -> owner:Owner.t
  -> Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** Before the actual persistence call, using its normalized candidate. No policy,
    yield, cancellation/join or revival. Unrelated catalog/state revisions do not
    close a cell; changed exact control/source/attempt or terminal evidence does.
    Common source/runtime/observed-failure facts are computed once for the complete
    inventory. Empty inventories inspect no candidate. Returns only live cells. *)
val retain_candidates : t list -> Session_state.t -> t list

val close : t -> unit
