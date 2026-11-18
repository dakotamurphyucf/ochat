open! Core

(** Ordinary actual delegated-child load custody, not Goal/Run authority. The
    Actor authenticates Factory allocation/source/selection and ordinary Init
    eligibility independently. No decoder, principal inference or host callbacks. *)
module Custody : sig
  type t =
    | Host of Operator_authorization.t option
    | Prepared_run of Run_preparation.t
end

type t

(** The actual Actor keeps Owner private and captures only its own child state.
    This records ordinary custody; it does not make Host None valid Ready Init.
    Prepared_run uses its SAME live tracking cell, not a second revision tracker. *)
val capture
  :  owner:Run_preparation.Owner.t
  -> state:Session_state.t
  -> custody:Custody.t
  -> (t, Agent_protocol.Error.t) result

val tracking : t -> Preparation_tracking.t
val initial_preparation : t -> Run_preparation.t option
val original_actor : t -> Operator_authorization.t option

val check_basis
  :  t
  -> owner:Run_preparation.Owner.t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** Only this load's current actual original. Host Some preserves ordinary
    initialization's is_current check; Prepared_run preserves its actual existing
    check/current policy. No foreign Actor, parent callback or guessed operator. *)
val check_current
  :  t
  -> owner:Run_preparation.Owner.t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

val check_authority
  :  t
  -> owner:Run_preparation.Owner.t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** Closes this view. Host owns/closes its tracking; borrowed Prepared_run retains
    the SAME caller-owned RP and its original finalizer, never closes it here. *)
val close : t -> unit

module Source_binding : sig
  type preparation := t
  type t

  val bind
    :  preparation
    -> owner:Run_preparation.Owner.t
    -> state:Session_state.t
    -> source:Runtime_source_preparation.t
    -> (t, Agent_protocol.Error.t) result

  val preparation : t -> preparation
  val source : t -> Runtime_source_preparation.t

  val check_basis
    :  t
    -> owner:Run_preparation.Owner.t
    -> state:Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** One actual construction consumes this binding; a second consume rejects. *)
  val consume
    :  t
    -> owner:Run_preparation.Owner.t
    -> state:Session_state.t
    -> (unit, Agent_protocol.Error.t) result

  (** Source view only, monotonic; does not replace or resurrect Init/Run custody. *)
  val close : t -> unit
end
