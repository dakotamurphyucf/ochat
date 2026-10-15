open! Core
module P = Agent_protocol

(** Startup-only composition of explicitly configured native authoring
    authority. This module opens no source owner and retains no mutable
    selection registry. The daemon supplies its already-open physical
    Project_source_host after catalog construction and before installing the
    authoring child owners. *)
type t

module Limits : sig
  type t

  (** A finite, host-owned Standard profile. The contract identity byte limit
      caps aggregate conversion allocation across distinct configured targets;
      the binding also enforces its fixed 16 MiB per-target conversion and
      encoded-byte cap. The aggregate work
      budget covers preadmission of those targets, including Moderator; the
      joined compiler still uses [analysis]'s separate source
      and semantic limits. [document_dir] below is parser display provenance,
      never a filesystem read grant. All constructors validate their own
      ranges before this profile is retained. *)
  val create
    :  max_contract_identity_bytes:int
    -> max_contract_work_steps:int
    -> source:Authoring_source_host.Limits.t
    -> analysis:Chatml_authoring_analysis.Policy.t
    -> project_build:Chatml_project_build.Limits.t
    -> semantic:Authoring_context_service.Semantic.Limits.t
    -> prefix:Authoring_context_service.Semantic.Prefix_limits.t
    -> query:Authoring_context_service.Semantic.Query_limits.t
    -> (t, P.Error.t) result

  (** Actual finite daemon/CLI profile, independent of wire-selected feature
      strings. It is built from existing lower validated constructors and
      admits all five Standard targets within a 40 MiB aggregate identity cap. *)
  val standard : (t, P.Error.t) result

  val max_contract_identity_bytes : t -> int
  val max_contract_work_steps : t -> int
end

(** [None] config preserves [base] exactly, including an intentional disabled
    authoring service. A configured grant with [base=None] is a typed startup
    conflict. Otherwise bind the exact resolved root inventory and physical
    Project_source_host identity with
    Authoring_configured_project_policy's immutable binding; when [contracts]
    is [Some] it must cover each configured native target within this
    profile's ceilings and is reused without another conversion. Preserve
    supplied-document/core/lexical/assistance families and install the real
    source, semantic, prefix/query and checked-template policies. The actual
    Initialize inbound ceiling is revalidated before any authoring child
    owner opens. No native read occurs during composition. A revoked actor,
    changed contract or foreign root is denied by the existing Source owner
    and final Core gate. A changed Server config requires a daemon restart;
    there is no hot-reload permission registry. *)
val create
  :  base:Authoring_installation.Host_policy.t option
  -> configured:Authoring_project_access_configuration.t option
  -> contracts:Authoring_standard_contracts.t option
  -> source_configuration:Project_source_configuration.t
  -> source_host:Project_source_host.t
  -> initialize_limits:P.Initialize.Limits.t
  -> document_dir:Eio.Fs.dir_ty Eio.Path.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, P.Error.t) result

val host_policy : t -> Authoring_installation.Host_policy.t option
