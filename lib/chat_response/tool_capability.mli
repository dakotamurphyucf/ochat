open Core

(** Host-owned bindings to actual registered tool implementations. Public
    reference data never grants authority; selecting/narrowing requires an
    existing registry. No function is executed by these operations. *)
type reference = private
  { version : int
  ; id : Agent_protocol.Id.Capability.t
  ; name : string
  ; owner : string
  ; implementation_revision : string
  ; fingerprint : string
  ; input_schema : Jsonaf.t
  }

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp]

type binding
type t

(** Register implementations already constructed under the host configuration. Each pair carries
    the host's digest of its declaration/implementation revision. The resource
    digest identifies the effective host configuration used to construct them.
    This trusted host API cannot be invoked from script reference data.
    Random capability IDs distinguish live registrations, including identical
    names/configuration across restarts. Restoration requires explicit host
    re-admission; this function never rebinds an old reference by name. *)
val create
  :  owner:string
  -> resource_fingerprint:string
  -> (string * Ochat_function.t) list
  -> (t, error) result

val references : t -> reference list
val reference : binding -> reference

(** Recover the original implementation for the owning invocation service.
    The service must still perform per-call authorization, moderation, schema
    checks and output disclosure. This accessor is not an invocation endpoint. *)
val implementation : binding -> Ochat_function.t

(** Select exact registered names. Empty selects none; duplicates/missing names
    reject. The resulting registry retains the same bindings and implementations. *)
val select : t -> names:string list -> (t, error) result

(** Resolve an opaque reference only within the supplied selected registry.
    A stale/foreign/mismatched ID or fingerprint never falls back to a name. *)
val resolve
  :  t
  -> id:Agent_protocol.Id.Capability.t
  -> fingerprint:string
  -> (binding, error) result

val find : t -> name:string -> (binding, error) result

(** Stable ordering of the selected live reference fingerprints. Changes to the
    selection or a re-registration invalidate the resulting aggregate digest. *)
val fingerprint : t -> string
