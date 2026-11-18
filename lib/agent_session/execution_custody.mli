open! Core

(** Actor-private epoch around an actual original authorization. Creation does
    not authenticate its argument; only genuine actor admissions call it. There
    is no codec, principal lookup or restoration from durable facts. *)
type t

val create : Operator_authorization.t -> t
val actor : t -> (Operator_authorization.t, Agent_protocol.Error.t) result
val transfer : t -> (t, Agent_protocol.Error.t) result
val retire : t -> unit
val same_origin : t -> t -> bool

module Admission : sig
  type custody = t
  type t

  (** A real admitted legacy None is distinct from absence of an admission. *)
  val create : custody option -> t

  (** Unchecked access preserves Some expired custody; it never downgrades to
      None when an epoch or original proof expires. *)
  val custody : t -> custody option

  val check : t -> (unit, Agent_protocol.Error.t) result
  val transfer : t -> (t, Agent_protocol.Error.t) result

  module Status : sig
    type t =
      | Current
      | Closed
      | Authority_expired
  end

  (** Exact nonyielding negative facts, not classification of arbitrary errors. *)
  val status : t -> Status.t

  (** End only the admission cell. Existing exact lifecycle handoffs may still
      transfer its contained custody before retiring the prior token. *)
  val close : t -> unit

  (** End both the admission and contained custody, idempotently. *)
  val retire : t -> unit
end

module Continuation : sig
  (** Only a genuine admitted completed producer may transfer Current custody.
      Expired is a process-local negative fact, never a restored credential.
      Missing inventory is separately unavailable, not Current legacy None. *)
  type t =
    | Current of Admission.t
    | Expired

  (** For an actual completed-fact ACK path only. Known admission/original expiry
      yields Expired so truthful facts can persist; unrelated errors propagate.
      The caller owns Current until ACK installation or retirement. *)
  val transfer_completed : Admission.t -> (t, Agent_protocol.Error.t) result

  (** Strict borrow: Expired rejects. Ordinary new-work selection and emit must
      never use completed-fact attenuation to manufacture legacy None. *)
  val transfer : t -> (Admission.t, Agent_protocol.Error.t) result

  val check : t -> (unit, Agent_protocol.Error.t) result
  val retire : t -> unit
end
