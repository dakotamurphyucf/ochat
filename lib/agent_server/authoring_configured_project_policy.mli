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
    existing contract adapter, its canonical identity charged under these
    positive finite byte/work limits before retention. No Build is prepared
    here. Checkpoint and cancellation exceptions keep identity. *)
val bind
  :  Project_source_host.t
  -> configuration:Configuration.t
  -> project_sources:Project_source_configuration.t
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
