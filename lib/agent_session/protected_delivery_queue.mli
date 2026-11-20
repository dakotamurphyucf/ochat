(** One operation's committed, transient output plans. Taking transfers the
    plans once; closing retires all unclaimed plans and prevents late appends. *)
type t

val create : unit -> t

val add
  :  t
  -> Inference_runtime.Protected_delivery.t
  -> (unit, Agent_protocol.Error.t) result

val take : t -> Inference_runtime.Protected_delivery.t list
val close : t -> unit
