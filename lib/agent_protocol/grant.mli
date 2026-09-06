(** Durable invocation grants and revocation requests. *)

type scope =
  | Exact_session
  | Prefix_session
  | Durable_exact
[@@deriving compare, equal, sexp]

type state =
  | Active
  | Revoked
  | Expired
[@@deriving compare, equal, sexp]

type t =
  { id : Id.Grant.t
  ; session_id : Id.Session.t
  ; principal_id : Id.Principal.t
  ; tool_name : string
  ; identity_digest : string
  ; scope : scope
  ; state : state
  ; created_at : Timestamp.t
  ; expires_at : Timestamp.t option
  ; revoked_at : Timestamp.t option
  ; revocation_reason : string option
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module List_request : sig
  type t =
    { page : Page.Request.t
    ; session_id : Id.Session.t option
    ; principal_id : Id.Principal.t option
    ; state : state option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Revoke_request : sig
  type t =
    { grant_id : Id.Grant.t
    ; session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; reason : string
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Revoke_result : sig
  type nonrec t =
    { grant : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
