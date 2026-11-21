(** Exact, metadata-only management of source-owned recovery note shares. *)
open! Core

val feature : string
val max_envelope_bytes : int

module Path : sig
  type t [@@deriving compare, equal, sexp_of]

  val of_string : string -> (t, Protocol_error.t) result
  val to_string : t -> string
end

module Selection : sig
  type t = private
    { id : int64
    ; revision : int64
    }

  val create : id:int64 -> revision:int64 -> (t, Protocol_error.t) result
end

(** The opaque route accepted by the selected recipient tool. The route
    carries no grant authority. Disclose it only to the intended recipient. *)
module Reference : sig
  type t [@@deriving equal]

  val create
    :  Session_ref.t
    -> nonce:Id.Session_incarnation.t
    -> id:int64
    -> (t, Protocol_error.t) result

  val source : t -> Session_ref.t
  val id : t -> int64
  val matches : t -> source:Session_ref.t -> nonce:Id.Session_incarnation.t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Protocol_error.t) result
end

module Source : sig
  type t = private
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    }

  val create
    :  Session_ref.t
    -> attachment_id:Id.Attachment.t
    -> expected_generation:int
    -> (t, Protocol_error.t) result
end

module Basis : sig
  type t = private
    { share_revision : int64
    ; catalog_revision : int64
    }

  val create
    :  share_revision:int64
    -> catalog_revision:int64
    -> (t, Protocol_error.t) result
end

module Request : sig
  type t =
    | Inspect of
        { source : Source.t
        ; path : Path.t option
        }
    | Share of
        { source : Source.t
        ; expected : Basis.t
        ; selection : Selection.t
        ; recipient : Session_ref.t
        ; expires_at : Timestamp.t
        ; key : Idempotency_key.t
        }
    | Revoke of
        { source : Source.t
        ; expected_share_revision : int64
        ; reference : Reference.t
        ; key : Idempotency_key.t
        }

  val source : t -> Source.t
  val methods : string list
  val method_name : t -> string
  val to_json : t -> Jsonaf.t
  val of_method_and_params : method_:string -> Jsonaf.t -> (t, Protocol_error.t) result
end

module Metadata : sig
  type t =
    { basis : Basis.t
    ; selection : Selection.t option
    }
end

module Receipt : sig
  type t = private
    { reference : Reference.t
    ; share_revision : int64
    }

  val create : Reference.t -> share_revision:int64 -> (t, Protocol_error.t) result
end

module Response : sig
  type t =
    | Inspected of Metadata.t
    | Shared of Receipt.t
    | Revoked of Receipt.t

  val method_name : t -> string
  val to_json : t -> Jsonaf.t
  val of_method_and_json : method_:string -> Jsonaf.t -> (t, Protocol_error.t) result

  (** Bind the result to the original request. Retried receipts can describe
      a historical revision even after the current ledger has advanced. *)
  val validate : t -> request:Request.t -> (unit, Protocol_error.t) result
end
