module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Captured = Chatml_source
module Project = Chatml_authoring_bundle_project
module Binding = Chatml_prompt_binding
module Contract = Chatml.Chatml_compiler_contract

(** Captured-only validation of one uploaded ChatMD definition and all its
    genuine parser-owned ChatML script declarations. No host filesystem/network
    read, preprocessing, compiler initialization or runtime is performed here.
    The caller independently owns the live sealed upload and selects authentic
    contracts and any prior Builds; this value grants no authority. *)

module Selection : sig
  type t

  (** Host-supplied actual target contract and separately admitted previous
      Build. A manifest selector or selected Project cannot create either. *)
  val create : contract:Contract.t -> previous:Chatml_project_build.t option -> t

  val contract : t -> Contract.t
  val previous : t -> Chatml_project_build.t option
end

module Published_selection : sig
  type t

  (** Exact published final lock/graph captures and a separately admitted
      current compiler contract. Constructing this value is not source custody
      or host authority; the Host obtains bytes only inside Store's protected
      published-capture borrow. *)
  val create
    :  contract:Contract.t
    -> lock_source:Captured.t
    -> sources:Captured.t list
    -> t
end

module Error : sig
  type problem =
    | Wrong_document_basis
    | Snapshot_mismatch
    | Document_bundle_limit
    | Parser of Prompt.Chat_markdown.Generated.error
    | Missing_or_surplus_document_member
    | Missing_or_surplus_script
    | Stale_or_foreign_selector
    | Script_body_mismatch
    | External_source_unavailable
    | Project_route_unavailable
    | Project_root_mismatch
    | Wrong_authored_environment
    | Dynamic_project_unavailable
    | Selected_contract_unavailable
    | Unexpected_selected_contract
    | Project of Project.Error.t

  type t

  val problem : t -> problem
end

module Validated : sig
  module Script : sig
    type t

    val selector : t -> Bundle.Manifest.Script.Selector.t
    val binding : t -> Binding.t
    val project : t -> Project.Validated.t option
  end

  type t

  val manifest_fingerprint : t -> Captured.Digest.t
  val root_source : t -> Captured.Id.t
  val document_bundle_fingerprint : t -> Captured.Digest.t
  val scripts : t -> Script.t list
end

(** [sources] is the exact in-memory uploaded snapshot; its store claim is bound
    only by the later publisher. [dir] anchors parser display provenance and is
    never used to fetch missing bytes. Admit every source exactly once into the
    Project.Attempt. Map explicit Document_member IDs to exact captured bytes,
    construct Chatmd_source_bundle under its physical hard caps, and run the
    existing generated captured parser with no fallback. Match the manifest's
    scripts one-to-one with ALL actual Parsed.declarations; that registry contains
    only the parser's supported ChatML script families. Bind each declaration via
    Chatml_prompt_binding, including its registration Root, actual
    imported declaring input/range, body digest and external-source provenance.

    Source_only is accepted only for an actual script with no authored project;
    it retains no fake graph. Selected_project requires its actual static authored
    project, resolved relative to that declaring portable input through
    Source_loader.resolve_member. Its root must have an explicit Document_member
    mapping; no Source.Id suffix is used as a path. Actual environment selection
    must match Project intent before the host-supplied independent contract/prior
    Build is passed into Project.Attempt.validate_selected_script.

    Every mapped Document_member must have been read by the parser or be an
    actual resolved selected Project root. The union of Document_member IDs and
    each selected Project subset must equal the uploaded manifest inventory;
    no extra capture is silently admitted. The SAME Attempt ledger charges
    snapshot, parser reads/conversion/Binding/selection and all Project Builds,
    including repeated retained bytes; errors and callback exceptions leave no
    partial Validated value. Original checkpoint/cancellation exceptions retain
    identity and backtrace. Results prove in-memory closure coherence only. *)
val validate
  :  Bundle.Manifest.t
  -> sources:Captured.t list
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> selected_projects:(Bundle.Manifest.Script.Selector.t * Selection.t) list
  -> limits:Project.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Validated.t, Error.t) result

(** Reparse the exact captured definition with the same generated parser,
    imported-member and Binding checks as [validate]. Actual parsed declarations
    must match all manifest selectors, authored body/external source identities,
    per-script project roots and environments. Source_only retains no graph.
    Every Selected_project must have exactly one supplied final published graph;
    a missing, surplus or repeated selector rejects. Its final lock is restored
    with the selected current contract through Project.Attempt's SAME work/input
    ledger, without requiring the original Compose previous Build. No host file
    or network fallback, preprocessing, compiler initializer or runtime work. *)
val rebind
  :  Bundle.Manifest.t
  -> sources:Captured.t list
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> selected_projects:(Bundle.Manifest.Script.Selector.t * Published_selection.t) list
  -> limits:Project.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Validated.t, Error.t) result
