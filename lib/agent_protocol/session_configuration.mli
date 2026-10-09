open! Core
module Error = Protocol_error

(** Nonsecret configuration intent. Patches preserve unknown private target data;
    constructing a patch grants no profile/account or tool authority. *)
module Patch : sig
  type t [@@deriving sexp]

  (** [settings] are explicit overrides, including absent/null/value. Duplicate
      names, empty patches and oversized data reject at construction/decoding. *)
  val create
    :  ?model:string
    -> ?profile:string
    -> settings:Inference.Request.Setting.t list
    -> unit
    -> (t, Error.t) Result.t

  val model : t -> string option
  val profile : t -> string option
  val settings : t -> Inference.Request.Setting.t list
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

(** Effective is a host capture, never a second mutable settings store. Preparing
    is scoped to a live root request and is cleared on failure/cancellation.
    All Configuration values use the existing explicit safe whitelist. *)
type phase =
  | Preparing
  | Effective
  | Retained
[@@deriving equal, sexp]

type capture =
  { operation_id : Id.Operation.t
  ; revision : int64
  ; phase : phase
  ; configuration : (Inference.Observation.Configuration.t[@sexp.opaque])
  }
[@@deriving sexp]

type t =
  { revision : int64
  ; selected : (Inference.Observation.Configuration.t option[@sexp.opaque])
  ; capture : capture option
  ; pending : bool
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) Result.t

module Get_request : sig
  type t = { session_id : Id.Session.t } [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Update_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_revision : int64
    ; patch : Patch.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end
