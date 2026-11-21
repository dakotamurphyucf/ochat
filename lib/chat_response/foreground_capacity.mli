(** Local worker observations for a single ordinary root request. An actor must
    separately validate the exact unsent preparation or completed attempt. *)
type t

val create : Capacity_rollover.t -> on_retired:(unit -> unit) -> t
val on_retired : t -> unit

module Request : sig
  type control := t
  type t

  val create : control -> can_roll_over:bool -> t
  val observe : t -> Inference.Event.t -> unit
  val before_dispatch : t -> Inference_runtime.Prepared.t -> unit

  val after_receipt
    :  t
    -> attempt:Inference_runtime.Attempt.t
    -> receipt:Inference_runtime.Receipt.t
    -> finish_source:(unit -> unit)
    -> unit

  (** Raised only after an actor-confirmed retirement. The caller releases the
      old root capture before selecting the continuation. *)
  exception Retired
end
