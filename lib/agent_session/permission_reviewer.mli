open! Core

(** Fail-closed reviewer interface for unattended permission decisions. *)

module Request : sig
  type t =
    { tool_name : string
    ; identity_digest : string
    ; invocation_display : string
    ; effects : string list
    }
  [@@deriving sexp]
end

module Decision : sig
  type t =
    | Allow
    | Deny of string
  [@@deriving compare, equal, sexp]
end

module Error : sig
  type t =
    { code : string
    ; message : string
    }
  [@@deriving compare, equal, sexp]
end

type kind =
  | Model
  | External
[@@deriving compare, equal, sexp]

module type Reviewer = sig
  val review : Request.t -> (Decision.t, Error.t) result
end

type t

(** [create ~id ~kind ~revision ~review] creates a named reviewer. [revision]
    identifies security-relevant reviewer configuration and participates in
    the containing permission-profile digest. *)
val create
  :  id:string
  -> kind:kind
  -> revision:string
  -> review:(Request.t -> (Decision.t, Error.t) result)
  -> (t, Agent_protocol.Error.t) result

(** [unavailable] creates a reviewer that always fails closed with [message]. *)
val unavailable : id:string -> kind:kind -> message:string -> t

(** [review] converts reviewer exceptions and malformed empty denials into
    typed failures. Callers must treat every failure as denial. *)
val review : t -> Request.t -> (Decision.t, Error.t) result

(** [id t] returns the configured reviewer ID. *)
val id : t -> string

(** [kind t] returns the reviewer strategy kind. *)
val kind : t -> kind

(** [revision t] returns the security-relevant implementation revision. *)
val revision : t -> string
