open Core

(** Explicit administrator-selected upload authority. No root/path/member or
    manifest ID in this record supplies a native source, compiler or Store
    grant. Every operation is separately authorized by actual authenticated
    principal ID and authentication kind at use. A server configuration change
    requires the existing daemon restart; no mutable permission registry is
    constructed from this value. *)

module Grant : sig
  type operation =
    | Upload_write
    | Publish
    | Bundle_read
    | Rebind
  [@@deriving compare, equal, sexp]

  type t =
    { principal_id : Agent_protocol.Id.Principal.t
    ; authentication_kind : string
    ; operation : operation
    }
  [@@deriving compare, equal, sexp]
end

module Selection : sig
  type t =
    { principal_id : Agent_protocol.Id.Principal.t
    ; authentication_kind : string
    ; request : Authoring_project_access_configuration.Selection.request
    ; selected_environment : string
    ; target : Chatml.Chatml_compiler_contract.Target.t
    }
  [@@deriving sexp]

  val compare : t -> t -> int
  val equal : t -> t -> bool
end

type mode =
  | Upload_only
  | Augment_core
[@@deriving compare, equal, sexp]

type t =
  { mode : mode
  ; grants : Grant.t list
  ; selections : Selection.t list
  }
[@@deriving compare, equal, sexp]

(** Revalidate directly constructed records before Standard conversion or any
    owner/store opening. Bound grant/selection counts and aggregate text
    bytes; require distinct (principal,kind,operation) grants and distinct
    (principal,kind,request) selections, a valid nonempty authentication kind
    via the existing Principal constructor, a valid request/selected environment and
    a known Standard target. A selection is usable only with the corresponding
    actor's current Publish or Rebind grant at the actual operation. Empty
    selections permit source-only ChatMD closure without a selected Project;
    they never synthesize a compiler contract. *)
val validate : t -> unit Or_error.t

(** Distinct Standard targets in first-seen order, for a single shared finite
    conversion cache across this configuration and configured native access.
    This is comparison data, not target authority. *)
val targets : t -> Chatml.Chatml_compiler_contract.Target.t list
