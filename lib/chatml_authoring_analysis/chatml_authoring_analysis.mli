open Core

(** Project analysis core. The host owns
    actor/source authorization, authentic target choice and one serialized gate
    for context mutation and result publication. No parser, fetch or runtime. *)

module Policy : sig
  type t

  (** Positive host ceilings. Compilation and semantic limits govern the WHOLE
      prepared graph. Retained count and semantic source/fact/edge/label limits
      bound current results. Projection diagnostic/site/source/index-entry and
      source-byte caps apply across one page, not separately per related site;
      these are not estimated OCaml heap byte counts.
      The host separately bounds active joined workers and old response values. *)
  val create
    :  compilation:Chatml_compilation.limits
    -> semantic:Chatml_semantics.Limits.t
    -> max_dependency_depth:int
    -> max_results:int
    -> max_projected_diagnostics:int
    -> max_projected_sites:int
    -> max_projection_sources:int
    -> max_position_index_entries:int
    -> max_projection_source_bytes:int
    -> t Or_error.t

  val max_results : t -> int

  (** Reuse this exact validated policy for provisional prefix checking; the
      service cannot silently substitute broader compiler or semantic limits. *)
  val compilation : t -> Chatml_compilation.limits

  val semantic : t -> Chatml_semantics.Limits.t
  val max_dependency_depth : t -> int
end

module Error : sig
  type t =
    | Context of Chatml_authoring_context.Error.t
    | Compiler of Chatml_compilation.Analysis_error.t
    | Receipt_mismatch
    | Result_identity_mismatch
    | Stale_generation
    | Policy_mismatch
    | Invalid_occupancy
    | Retention_limit

  (** Finite, source/path-free error code. Native compiler details remain in the
      typed error for the trusted host and never reach a wire response by an
      automatic derived serializer. *)
  val sexp_of_t : t -> Sexp.t
end

module Checked : sig
  (** Compiler-owned immutable result, independent of an editor context. A
      Failed result retains same-run syntax/diagnostics only. Expected admission
      and resource failures are typed Error; cancellation and unexpected errors
      retain exception identity after worker join. No initializer executes. *)
  type status =
    | Complete of Chatml_semantics.Snapshot.t
    | Failed of Chatml_semantics.Partial.t

  type t

  val status : t -> status
  val diagnostics : t -> Chatml_diagnostic.t list
  val input_fingerprint : t -> Chatml_source.Digest.t
  val required_contract : t -> Chatml_source.Digest.t
  val selected_contract : t -> Chatml_source.Digest.t
  val environment : t -> Chatml_prepared_program.Environment.t

  (** Analyze only an already completed Build. Compare the actual semantic
      identity with its exact Prepared fingerprint, required/selected authentic
      contract and selected environment before returning. The Build can come
      from a validated project overlay or independently admitted script
      selection; this call itself grants neither source nor project authority. *)
  val analyze_build
    :  Chatml_project_build.t
    -> policy:Policy.t
    -> env:Eio_unix.Stdenv.base
    -> target:Chatml_compilation.target
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  (** Same joined checker and exact result-identity validation with the actual
      host-selected contract already associated with this Build. *)
  val analyze_build_with_contract
    :  Chatml_project_build.t
    -> contract:Chatml.Chatml_compiler_contract.t
    -> policy:Policy.t
    -> env:Eio_unix.Stdenv.base
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t
end

module Project_stamp : sig
  (** Validated project Input fingerprint, not a claim that a Failed compiler
      result has a certified semantic graph. Prepared identity contains every
      exact source ID/digest/revision; context generation owns editor revisions. *)
  type t [@@deriving equal, sexp_of]

  val identity : t -> Chatml_authoring_context.Identity.t
  val generation : t -> Chatml_authoring_context.Generation.t
  val input_fingerprint : t -> Chatml_source.Digest.t
  val required_contract : t -> Chatml_source.Digest.t
  val selected_contract : t -> Chatml_source.Digest.t
  val environment : t -> Chatml_prepared_program.Environment.t
end

module Project_payload : sig
  type t

  val stamp : t -> Project_stamp.t
  val checked : t -> Checked.t
end

module Project_candidate : sig
  type t

  val payload : t -> Project_payload.t

  (** Compare OCH-294 receipt to these SAME OCH-278 Inputs: identity,
      generation, each effective captured source and exact unchanged old lock.
      The generated lock comes only from the completed Build. A mismatch never
      reaches the compiler. The host supplied authentic contract to the receipt
      and selects [target] here. *)
  val analyze_validated
    :  Chatml_authoring_context.Inputs.t
    -> receipt:Chatml_authoring_effective.Overlay.t
    -> policy:Policy.t
    -> env:Eio_unix.Stdenv.base
    -> target:Chatml_compilation.target
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  (** Preserve the exact receipt/context comparison and stamp while checking
      with the authentic selected contract. *)
  val analyze_validated_with_contract
    :  Chatml_authoring_context.Inputs.t
    -> receipt:Chatml_authoring_effective.Overlay.t
    -> contract:Chatml.Chatml_compiler_contract.t
    -> policy:Policy.t
    -> env:Eio_unix.Stdenv.base
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t
end

module Project_cache : sig
  (** One host-owned immutable context ledger plus at most one latest payload
      per current project context. The host replaces this value under its shared
      context gate and cancels/joins active analysis before releasing resources. *)
  type t

  val create : contexts:Chatml_authoring_context.t -> policy:Policy.t -> t
  val contexts : t -> Chatml_authoring_context.t
  val retained_count : t -> int

  (** Apply an already authorized OCH-278 transition. Evict payloads whose
      context identity/generation/analysis key vanished or changed. Edit, close,
      refresh and document transitions invalidate immediately. *)
  val transition : t -> contexts:Chatml_authoring_context.t -> t

  (** After the joined worker returns, compare Candidate to CURRENT Inputs,
      including source revisions, input fingerprint, environment and contract;
      then call Context.note_analysis and retain one result. Candidate must use
      this cache's exact Policy instance. An intervening edit/close publishes
      nothing. [occupied_elsewhere] is the current Document_cache retained
      count from the SAME serialized host gate; the sum cannot exceed the
      shared policy's max_results. A negative count rejects. This and
      [transition] use the same host mutation gate. *)
  val publish
    :  t
    -> Project_candidate.t
    -> occupied_elsewhere:int
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  (** Recheck current context key before exposure. The host separately checks
      actor authority and client affinity to the echoed stamp/revisions. *)
  val current
    :  t
    -> identity:Chatml_authoring_context.Identity.t
    -> Project_payload.t option
end

module Site_projection : sig
  type t

  module Error : sig
    type t =
      | Position of Chatml_editor_position.Error.t
      | Projection_limit
    [@@deriving sexp_of]
  end

  val native : t -> Chatml_diagnostic.Site.t

  (** Exact native byte location remains even if editor conversion rejects
      invalid UTF-8, scalar/CRLF interior or exhausted index allowance. *)
  val byte_range : t -> (Chatml_editor_position.Range.t, Error.t) Result.t

  val utf16_range : t -> (Chatml_editor_position.Range.t, Error.t) Result.t
end

module Diagnostic_projection : sig
  type t

  type page =
    { items : t list
    ; truncated : bool
    }

  val native : t -> Chatml_diagnostic.t
  val primary : t -> Site_projection.t option
  val sites_truncated : t -> bool

  (** Admitted prefix of native related sites in original order, including
      bounded messages. [sites_truncated] reports omitted tail sites; the
      native diagnostic still retains them. No related owner is replaced by the
      primary source. *)
  val related : t -> (Chatml_diagnostic.Related.t * Site_projection.t) list

  (** The same source-owner conversion for a diagnostic supplied by a trusted
      compiler/parser adapter. This does not certify arbitrary caller metadata. *)
  val project_native
    :  Chatml_diagnostic.t
    -> policy:Policy.t
    -> checkpoint:(unit -> unit)
    -> t

  (** Project bounded diagnostics using each exact captured Site owner. An
      unavailable position stays a typed per-site result; the native diagnostic
      remains present. A single request charges each indexed source's exact
      bytes once against [max_projection_source_bytes]. Aggregate site, source,
      and exact retained index-entry limits bound multi-file projection;
      omitted sites and diagnostics have separate explicit truncation flags. *)
  val project : Checked.t -> policy:Policy.t -> checkpoint:(unit -> unit) -> page
end
