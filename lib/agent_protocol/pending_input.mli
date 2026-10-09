(** Pending input extends the existing durable deferred history queue. It grants
    no writer, scheduler or provider authority. All constructors and decoders
    validate generation, occurrence identity and temporal binding. *)
module Revision : sig
  type t [@@deriving compare, equal, sexp]

  val zero : t
  val of_int64 : int64 -> (t, Error.t) result
  val to_int64 : t -> int64
  val succ : t -> (t, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Timing : sig
  type t =
    | Safe_boundary
    | After_current_operation
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Terminal_proof : sig
  (** Bounded proof of the actual matching terminal root operation. Retains its
      typed identity/generation/outcome, never raw failure or interruption prose. *)
  type t [@@deriving equal, sexp]

  val of_operation : Operation.t -> (t, Error.t) result
  val operation_id : t -> Id.Operation.t
  val generation : t -> int
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Binding : sig
  type t = private
    | Safe_boundary
    | Await_idle
    | After_root of
        { operation_id : Id.Operation.t
        ; generation : int
        ; terminal : Terminal_proof.t option
        }
  [@@deriving equal, sexp]

  (** Validated payload-free safe binding for authored/legacy entries. Admission
      still validates generation and complete wrapper ownership independently. *)
  val safe_boundary : t

  (** Capture only an actual active Turn root. Compaction or no root waits for
      safe idle admission; the client cannot supply an inferred operation ID. *)
  val create
    :  Timing.t
    -> generation:int
    -> operation:Operation.t option
    -> (t, Error.t) result

  (** Only an exact matching terminal proof releases an after-root barrier.
      Unrelated terminal operations leave the binding unchanged. *)
  val release : t -> Terminal_proof.t -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t [@@deriving equal, sexp]

val create
  :  entry:History.entry
  -> generation:int
  -> binding:Binding.t
  -> (t, Error.t) result

val entry : t -> History.entry
val history_id : t -> History.Id.t
val generation : t -> int
val binding : t -> Binding.t
val with_binding : t -> Binding.t -> (t, Error.t) result
val with_entry : t -> History.entry -> (t, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
