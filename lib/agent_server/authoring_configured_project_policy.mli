open Core
module Configuration = Authoring_project_access_configuration
module Sources = Authoring_source_host

(** One immutable startup binding of explicit administrative read grants and
    Standard Project contracts to the actual configured source host. It owns
    no source anchor, actor, parser, Build, cache, callback registry or filesystem
    operation. The host and installation still own all resource lifetimes. *)
type t

module Error : sig
  type t =
    | Invalid_configuration
    | Source_host_closed
    | Foreign_domain
    | Root_binding_mismatch
    | Contract_limit
  [@@deriving sexp_of]
end

(** Validate before retention and bind exact configured root names to the
    Project_source_host's already opened ordered (name, Anchor) inventory.
    The resulting index map is private comparison data, never a configured
    or wire grant; reordering the configuration requires a new host/binding.
    The binding retains only the opened host's opaque physical identity token;
    matching root names, indices or native paths cannot rebind it.
    Every distinct selected Standard target is constructed once with the
    existing contract adapter, its conversion allocation charged under the
    positive finite aggregate allocation/work limits before retention. Each
    distinct target also has a fixed 16 MiB conversion-allocation and
    encoded-byte cap,
    so a larger aggregate allowance cannot admit an unbounded single target.
    A supplied shared cache must have been admitted under ceilings no wider
    than these binding ceilings and must contain every selected target; a
    wrong-ceiling or underprovisioned cache is a typed bind error, not a reason to
    convert an extra target. No Build is prepared here. Checkpoint and
    cancellation exceptions keep identity. *)
val bind
  :  Project_source_host.t
  -> configuration:Configuration.t
  -> project_sources:Project_source_configuration.t
  -> contracts:Authoring_standard_contracts.t option
  -> max_contract_identity_bytes:int
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** Physical affinity check for installation. The installer must call this
    against its actual [Project_source_host.t] before opening any source or
    upload owner; a policy bound to another host is rejected even if its
    configured paths and root indices happen to match. This comparison does
    not grant source access. *)
val issued_by : t -> Project_source_host.t -> bool

(** Nonyielding current operator decision by the *bound* internal root index,
    actual authenticated Principal.id and authentication_kind, and
    Operator_authorization.is_current. An unlisted principal/root denies.
    Source_host separately enforces physical Root issuer and both original and
    request actor checks on every borrow/read/capture. No grant is inferred
    from equal paths, current protocol feature names or catalog labels. *)
val read_policy : t -> Sources.Read_policy.t

(** Bounded, nonyielding comparison against this binding's installed grants
    for an actual Source_host Root supplied by the caller. This repeats the
    same current-actor decision as [read_policy]; Root.index alone does not
    prove physical affinity, which the enclosing Source/Context owner must
    check. It performs no I/O and lends no Root or read capability. Callers
    check original and current actors separately around yields. *)
val read_admitted : t -> Operator_authorization.t -> root:Sources.Root.t -> bool

(** Current comparison for one genuine, separately acquired document project
    receipt. The receipt must be from this bound physical host and the exact
    original Root actor; both original and request actors must still have this
    root's explicit configured read grant. This does no I/O and grants neither
    a native read nor a selected Build. The enclosing Source/Core owner still
    checks source generation and declaration membership. *)
val project_capture_authorized
  :  t
  -> original_actor:Operator_authorization.t
  -> actor:Operator_authorization.t
  -> Sources.Document_context.Project_capture.t
  -> bool

(** Comparison-only lookup of a preadmitted actual Standard contract for the
    exact authorized root/project/Entry-or-Library environment request. Returns
    None on missing/revoked actor or unselected mapping. Source_host's context
    owner separately proves physical root and current source authority; this
    callback does not confer a source read or compose a project. Bounded and
    nonyielding, including at the final authoring owner check. *)
val project_contract
  :  t
  -> Operator_authorization.t
  -> root:Sources.Root.t
  -> project:Source.Portable_path.t
  -> selection:Agent_protocol.Authoring.Context.Selection.t
  -> Chatml.Chatml_compiler_contract.t option
