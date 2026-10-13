open Core

(** Selected-script semantic analysis for a current ChatMD Document context.
    Raw or malformed documents have lexical/syntax assistance but no invented
    project graph. This adapter uses the real Parsed declaration and extraction
    map plus the shared ChatML checked compiler; it does not parse, fetch,
    preprocess, execute initializers or grant source/host authority. OCH-262
    owns the sole mutable context ledger, actor/currentness checks, serialized
    publication and bounded joined request lifetime. *)

module Unavailable : sig
  type t =
    | No_selected_script
    | Incomplete_or_malformed_document
    | No_validated_project
    | Dynamic_only_source
  [@@deriving sexp_of]
end

module Error : sig
  type t =
    | Invalid_selection
    | Membership_limit
    | Foreign_declaration
    | Ambiguous_declaration
    | Stale_document
    | Script_body_mismatch
    | Selection_mismatch
    | Compiler of Chatml_authoring_analysis.Error.t
    | Projection of Prompt.Chat_markdown.Declaration.Extraction.error
    | Policy_mismatch
    | Invalid_occupancy
    | Retention_limit

  (** Finite source/path-free code; never serialize native parser/compiler text
      from this printer into a client error. *)
  val sexp_of_t : t -> Sexp.t
end

module Selection : sig
  (** Trusted host classification after separate project/root/catalog admission.
      A Build is captured coherence data, not its own authorization proof. The
      client never supplies this variant directly. *)
  type t =
    | Validated_build of Chatml_project_build.t
    | No_validated_build
    | Dynamic_only
end

module Document_stamp : sig
  (** Host-produced comparison evidence, not client-supplied authority. The
      declaration range belongs to its actual parser input's effective bytes,
      which may differ from the root document for an import. Failed checked output still has an
      input fingerprint but no certified semantic graph. *)
  type t

  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
  val identity : t -> Chatml_authoring_context.Identity.t
  val generation : t -> Chatml_authoring_context.Generation.t
  val document_source : t -> Chatml_source.Id.t
  val document_digest : t -> Chatml_source.Digest.t
  val parser_input_fingerprint : t -> string
  val script_id : t -> string
  val declaration_input_fingerprint : t -> string
  val declaration_input_byte_length : t -> int
  val declaration_start_byte : t -> int
  val declaration_end_byte : t -> int
  val selected_script_source : t -> Chatml_source.Id.t
  val selected_script_digest : t -> Chatml_source.Digest.t
  val input_fingerprint : t -> Chatml_source.Digest.t
  val required_contract : t -> Chatml_source.Digest.t
  val selected_contract : t -> Chatml_source.Digest.t
  val environment : t -> Chatml_prepared_program.Environment.t
end

module Script : sig
  type t

  type admission =
    | Ready of t
    | Unavailable of Unavailable.t

  (** Select only an actual retained declaration in [Parsed.declarations] by
      bounded script ID, rejecting absent or ambiguous membership. Also check
      its range input belongs to [Parsed.inputs]. The parser root input's exact
      authored bytes must match current Document source bytes; effective bytes
      remain parser provenance and are not assumed equal. No display path or
      extension establishes custody. The selected Build's exact entry source
      must equal the emitted script body; authored project/environment selection
      still needs the host's real per-script authorization before this call.
      Missing selector, incomplete parse, missing validated Build or dynamic-only
      source return typed Unavailable. Invalid/stale inputs return Error.
      [max_membership_work_steps] bounds all declaration/input association scans;
      checkpoint exceptions propagate without conversion. *)
  val assess
    :  Chatml_authoring_context.Document.Inputs.t
    -> parsed:Prompt.Chat_markdown.Parsed.t option
    -> script_id:string option
    -> selection:Selection.t
    -> max_membership_work_steps:int
    -> checkpoint:(unit -> unit)
    -> (admission, Error.t) Result.t

  val document_stamp : t -> Document_stamp.t
end

module Candidate : sig
  type t

  val stamp : t -> Document_stamp.t
  val checked : t -> Chatml_authoring_analysis.Checked.t

  (** Reuse the context-neutral joined checker on the selected captured Build.
      Bind the actual declaration's public Extraction map against its exact
      compiled entry source. No new parser, compiler, source read or worker. *)
  val analyze
    :  Script.t
    -> policy:Chatml_authoring_analysis.Policy.t
    -> env:Eio_unix.Stdenv.base
    -> target:Chatml_compilation.target
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t
end

module Document_cache : sig
  (** Pure current-result map only; it owns no second context ledger. OCH-262
      atomically applies Project_cache.transition and this transition to its
      one host-owned ledger under the same gate. At most one selected-script
      result is retained per current Document context. *)
  type t

  val create : policy:Chatml_authoring_analysis.Policy.t -> t
  val retained_count : t -> int
  val transition : t -> contexts:Chatml_authoring_context.t -> t

  (** Compare Candidate to current Document Inputs after its joined worker,
      including identity/generation/source ID/digest/length and exact Policy
      object. [occupied_elsewhere] is the current
      Project_cache retained count from the SAME serialized host gate; the sum
      cannot exceed the shared Policy.max_results. A negative count rejects.
      The host independently reauthorizes parser, script selection, actor and
      source custody. *)
  val publish
    :  t
    -> current_inputs:Chatml_authoring_context.Document.Inputs.t
    -> occupied_elsewhere:int
    -> Candidate.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  (** Recheck current Document revision against the one host-owned ledger;
      host separately checks actor/current parser/selection before exposure. *)
  val current
    :  t
    -> contexts:Chatml_authoring_context.t
    -> identity:Chatml_authoring_context.Identity.t
    -> Candidate.t option
end

module Projection : sig
  type site =
    { native : Chatml_authoring_analysis.Site_projection.t
    ; authored : Prompt.Chat_markdown.Declaration.Extraction.projection option
    }

  type t

  val native : t -> Chatml_diagnostic.t
  val primary : t -> site option
  val related : t -> (Chatml_diagnostic.Related.t * site) list
  val sites_truncated : t -> bool

  (** Decorate only OCH-301's already admitted bounded primary/related sites;
      preserve its exact byte/UTF-16 outcomes and truncation flag. The public
      parser-owned extraction map may yield exact inline provenance, unresolved
      external member/digest, or approximate declaration context. It never
      turns an imported/display path into a captured editor source without a
      separate host custody receipt. Other checked sources stay native only.
      This call builds no position index or second related-site budget. *)
  val of_diagnostic
    :  Candidate.t
    -> Chatml_authoring_analysis.Diagnostic_projection.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t
end
