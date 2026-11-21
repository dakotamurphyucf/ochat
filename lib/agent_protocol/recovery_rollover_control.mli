(** Host-local status and exact-cut publication through the owning writer. *)
open! Core

val feature : string
val max_envelope_bytes : int

module Basis : sig
  type t = private
    { scope : Recovery_window.Scope.t
    ; revision : int64
    }
  [@@deriving equal, sexp_of]

  val create : Recovery_window.Scope.t -> revision:int64 -> (t, Protocol_error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

module Inspect : sig
  val method_name : string

  type t =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t option
    ; key : Idempotency_key.t option
    }

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

module Request : sig
  val method_name : string

  type t =
    { expected : Basis.t
    ; attachment_id : Id.Attachment.t
    ; key : Idempotency_key.t
    ; force : bool
    }

  val session : t -> Session_ref.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

module Receipt : sig
  type failure =
    | Basis_changed
    | Cancelled
    | Checkpoint_required
    | Hook_vetoed
    | Hook_failed
  [@@deriving equal, sexp_of]

  type quality =
    | Checkpointed
    | Archive_only_missing_checkpoint
    | Archive_only_unavailable_checkpoint
  [@@deriving equal, sexp_of]

  type outcome =
    | Pending
    | Committed of
        { successor : Recovery_window.Scope.t
        ; cut_revision : int64
        ; quality : quality
        }
    | Failed of failure
  [@@deriving equal, sexp_of]

  type t = private
    { expected : Basis.t
    ; outcome : outcome
    }
  [@@deriving equal, sexp_of]

  val create : Basis.t -> outcome -> (t, Protocol_error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
  val validate : t -> request:Request.t -> (unit, Protocol_error.t) result
end

module Capacity : sig
  type target = private
    { profile : string
    ; model : string
    ; configuration_revision : int64
    }
  [@@deriving equal, sexp_of]

  val target
    :  profile:string
    -> model:string
    -> configuration_revision:int64
    -> (target, Protocol_error.t) result

  type unavailable =
    | Disabled
    | Awaiting_preparation
    | Stale_preparation
    | Evidence_unavailable
  [@@deriving equal, sexp_of]

  type freshness = Session_configuration.phase =
    | Preparing
    | Effective
    | Retained
  [@@deriving equal, sexp_of]

  type evidence =
    | Measured
    | Estimated
  [@@deriving equal, sexp_of]

  type remaining =
    | Available of int64
    | Exceeded_by of int64
    | Excess_out_of_range
  [@@deriving equal, sexp_of]

  type t =
    | Unknown of
        { reason : unavailable
        ; target : target option
        }
    | Observed of
        { target : target
        ; freshness : freshness
        ; remaining : (evidence * remaining) option
        }
  [@@deriving equal, sexp_of]
end

module Status : sig
  type actions =
    { rollover : bool
    ; force : bool
    }
  [@@deriving equal, sexp_of]

  type view =
    | Unavailable
    | Available of
        { current : Basis.t
        ; previous : Recovery_window.Scope.t option
        ; capacity : Capacity.t
        ; receipt : Receipt.t option
        ; actions : actions
        }
  [@@deriving equal, sexp_of]

  type t = private
    { session : Session_ref.t
    ; view : view
    }
  [@@deriving equal, sexp_of]

  val create : Session_ref.t -> view -> (t, Protocol_error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
  val validate : t -> request:Inspect.t -> (unit, Protocol_error.t) result
end
