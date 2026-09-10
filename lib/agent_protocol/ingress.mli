(** Scoped external DATA admission. A registration ID is not a credential.
    The server authenticates the producer separately; callers cannot supply it. *)
module Submit_request : sig
  type t =
    { session_id : Id.Session.t
    ; registration_id : Id.Capability.t
    ; namespace : string
    ; idempotency_key : Idempotency_key.t
    ; payload : Jsonaf.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** Durable acceptance acknowledgement, not handler execution or model completion.
    Matching retries return the same event identity, digest and acceptance time. *)
module Acknowledgement : sig
  type t =
    { session_id : Id.Session.t
    ; registration_id : Id.Capability.t
    ; event_id : Id.Ingress_event.t
    ; idempotency_key : Idempotency_key.t
    ; payload_sha256 : string
    ; accepted_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
