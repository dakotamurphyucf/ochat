open! Core

(** Actor-private continuation of one actual completed native result. Its exact
    invocation and original admission are captured before result ACK. Missing
    custody, genuine admitted None and known expiry remain distinct. No codec,
    authority lookup or Run obligation exists. *)
type t

module Parent : sig
  (** Raw settlement facts. Reading these does not authorize another effect. *)
  type t =
    | Available of Execution_custody.Admission.t
    | Expired
    | Unavailable
end

(** No guard invocation: truthful outcome writers preserve negative facts. *)
val parent : t -> Parent.t

(** Stage only an actual resolved/published Awaiting observation. No observation
    returns None. A missing parent creates an unavailable component, never fresh
    legacy authority. Completed-fact transfer preserves known expiry; unrelated
    errors propagate. The caller retires speculative values on failed ACK. *)
val stage_completed
  :  Agent_protocol.Invocation.t
  -> source:Agent_protocol.Run_source.t option
  -> parent:Parent.t
  -> (t option, Agent_protocol.Error.t) result

(** Exact immutable result/context/source, allowing only protocol-valid publication
    and observation/follow-up progress. This is factual retention, not permission. *)
val matches
  :  t
  -> source:Agent_protocol.Run_source.t option
  -> Agent_protocol.Invocation.t
  -> bool

(** Stage an independent loan for the actual Awaiting→Observing claim. Installation
    consumes the prior component only after claim ACK; failure preserves it. *)
val stage_claim
  :  t
  -> source:Agent_protocol.Run_source.t option
  -> previous:Agent_protocol.Invocation.t
  -> observing:Agent_protocol.Invocation.t
  -> (t, Agent_protocol.Error.t) result

(** Stage an independent component for an actual Observed pending follow-up. The
    active callback loan may retire after ACK without ending this continuation.
    Unavailable and expired remain negative facts. *)
val stage_follow_up
  :  t
  -> resolved:Agent_protocol.Invocation.t
  -> (t, Agent_protocol.Error.t) result

(** Strict fresh-effect access. Both unavailable and known-expired reject. Never
    use this accessor to gate truthful result/observation settlement. *)
val admission : t -> (Execution_custody.Admission.t, Agent_protocol.Error.t) result

(** Idempotently closes this loan only; independently transferred children survive. *)
val retire : t -> unit

module Selection : sig
  type observation := t
  type t

  (** Select one compatible original across actual observation contributors and
      other required custodians. Missing/mixed contributors reject. A genuine
      admitted None observation remains a positive process-local admission. *)
  val create
    :  observations:observation list
    -> other_custodies:Execution_custody.t option list
    -> (t, Agent_protocol.Error.t) result

  val custody : t -> Execution_custody.t option
  val bindings : t -> (Agent_protocol.Id.Invocation.t * observation) list

  (** Nonyielding original/admission guards, repeated at the actual Turn/Compact
      prewrite boundary. The Actor must then check exact retained bindings/source
      and generation without invoking host policy or a clock. *)
  val check_current : t -> (unit, Agent_protocol.Error.t) result
end
