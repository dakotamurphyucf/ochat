open! Core

(** Immutable admission basis, owned by one actor. Validation may acquire host
    metadata locks and read disk, and must execute outside the actor mailbox.
    Tokens own no durable receipt, paid attempt, or pending-input authority. *)
type t

module Owner : sig
  type t

  val create : unit -> t
end

module Validated : sig
  type t

  val request : t -> Agent_protocol.Session_configuration.Update_request.t
  val proposed : t -> Inference.Request.Target.t
end

(** The actor must check the live writer capability before capturing this basis. *)
val create
  :  owner:Owner.t
  -> policy:Configuration_policy.t
  -> request:Agent_protocol.Session_configuration.Update_request.t
  -> state:Session_state.t
  -> (t, Agent_protocol.Error.t) Result.t

val validate : t -> (Validated.t, Agent_protocol.Error.t) Result.t

(** Pure final basis/current-binding check. The actor must independently repeat
    its live writer/admission check before the atomic durable transition. Exact
    target/history comparison permits unrelated state changes while rejecting
    content, generation, selection and host-policy changes. *)
val recheck
  :  Validated.t
  -> owner:Owner.t
  -> policy:Configuration_policy.t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) Result.t
