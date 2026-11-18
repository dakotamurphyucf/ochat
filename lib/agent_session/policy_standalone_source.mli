open! Core

(** Immutable process-local source association, not an admission or a loan.
    Dune-private and codec-free. The actual Caller/custody owner separately
    pins its physical issuer, domain, original custody and lexical lifetime. *)
module Source : sig
  type t = private
    { observer : Agent_protocol.Invocation.observer option
    ; installation : Agent_protocol.Run_source.t option
    }

  (** Capture both fields from the SAME actual decoded checkpoint. A full
      installation must project the same observer. An opaque historical
      observer can be retained without an installation for factual comparison;
      it cannot select a positive native Event/Job boundary. *)
  val create
    :  observer:Agent_protocol.Invocation.observer option
    -> installation:Agent_protocol.Run_source.t option
    -> (t, Agent_protocol.Error.t) result

  val equal : t -> t -> bool
end

module Subject : sig
  type t =
    | Root_operation of Agent_protocol.Id.Operation.t
    | Member of Policy_cause_data.Member.t
  [@@deriving equal]
end

type t = private
  { session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; subject : Subject.t
  ; origin : Agent_protocol.Inference_origin.t
  ; source : Source.t
  }

(** Validate generation, exact member generation, actual Run/Unbound value and
    represented source generation. Construction proves no currentness or
    ownership. Only the actual trusted owner may supply the captured facts.
    Missing historical origin must remain Error before calling this function. *)
val create
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> subject:Subject.t
  -> origin:Agent_protocol.Inference_origin.t
  -> source:Source.t
  -> (t, Agent_protocol.Error.t) result

val equal : t -> t -> bool

module Classification : sig
  type descriptor = t

  type t =
    | Unknown
    | Standalone_unrelated of descriptor
end
