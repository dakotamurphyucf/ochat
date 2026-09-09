open Core

(** Host-owned bindings to native implementations and managed extension targets. Public
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

(** Native text is opaque unless its actual host registration explicitly opts
    into the versioned invocation outcome envelope. Submitted code, tool names
    and output text cannot choose or change this contract. *)
type result_contract =
  | Native_output
  | Invocation_v1
[@@deriving sexp, equal]

type binding
type t

type implementation =
  | Native of Ochat_function.t
  | Managed of Chatmd_shell_spec.Extension_spec.implementation

type managed_registration =
  { descriptor : Openai.Completions.tool
  ; target : Chatmd_shell_spec.Extension_spec.implementation
  ; implementation_revision : string
  ; metadata : Chatmd_shell_spec.Authoring_metadata.t
  }

(** Register implementations already constructed under the host configuration. Each pair carries
    the host's digest of its declaration/implementation revision. The resource
    digest identifies the effective host configuration used to construct them.
    This trusted host API cannot be invoked from script reference data.
    Random capability IDs distinguish live registrations, including identical
    names/configuration across restarts. Restoration requires explicit host
    re-admission; this function never rebinds an old reference by name. *)
val create
  :  ?metadata:(string * Chatmd_shell_spec.Authoring_metadata.t) list
  -> ?result_contracts:(string * result_contract) list
  -> owner:string
  -> resource_fingerprint:string
  -> (string * Ochat_function.t) list
  -> (t, error) result

val references : t -> reference list
val reference : binding -> reference

(** Inspect the native runner or managed target for the owning invocation service.
    A managed target must resolve to its matching prepared definition and dispatcher.
    The service must still perform per-call authorization, moderation, schema
    checks and output disclosure. This accessor is not an invocation endpoint. *)
val implementation : binding -> implementation

(** Descriptor shared by native and owned managed dispatch. No executable is
    synthesized for a managed target. *)
val descriptor : binding -> Openai.Completions.tool

val native_implementation : binding -> Ochat_function.t option

(** Trusted admission of captured managed definitions alongside existing native
    bindings. The host must validate sources/schemas/dependencies first, bind
    revision/resource digests to that full authority, then install the matching
    compiled owned dispatcher before exposure. Target kind participates in identity;
    managed tools always return structured invocation outcomes and cannot claim
    native authoring-helper roles. Existing bindings retain their exact IDs and
    implementations; collisions cannot overwrite or rebind them. *)
val extend_managed
  :  t
  -> owner:string
  -> resource_fingerprint:string
  -> managed_registration list
  -> (t, error) result

(** Explicit authoring/helper metadata retained with this actual implementation.
    Selection cannot edit it, and a same-name registration does not inherit it. *)
val metadata : binding -> Chatmd_shell_spec.Authoring_metadata.t

(** Included in live and permission fingerprints, preserved by selection.
    [Native_output] retains existing registration identities. The invocation
    host decodes [Invocation_v1] only after normal output disclosure, validates
    the outcome and enforces owned-work rules before accepting it. *)
val result_contract : binding -> result_contract

(** Configuration identity for host permission grants. Stable across equivalent
    registrations, but changes with owner, resources, implementation, interface
    or metadata. Unlike a live reference fingerprint, it excludes the random
    capability ID. It never resolves or authorizes a live binding by itself. *)
val permission_fingerprint : binding -> string

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
