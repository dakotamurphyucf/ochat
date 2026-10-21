open! Core
module P = Agent_protocol
module W = P.Authoring.Wire
module Owner = Authoring_context_service_core
module Highlight = Language_highlighting.Highlight_snapshot
module Chatmd = Chatmd_authoring_assistance
module Chatml = Chatml_authoring_fallback
module Mapped = Chatmd_authoring_fallback
module Tool_registry = Chat_response.Tool_capability

(** One lexical/early-assistance sibling over the existing physical context
    owner. It owns no context ledger, source resolver, parser, uploaded bundle,
    compiler worker, scheduler or transport method registration. *)
type t

module Limits : sig
  type t

  (** Choose finite host work/metadata ceilings against the actual initialized
      protocol limits. The implementation constructs the lower Highlight,
      Chatmd, Chatml and Mapped limit values itself; callers cannot inject an
      abstract lower limit larger than wire span/issue/suggestion/label/result
      maxima. In particular spans <= Lexical.Limits.max_spans, issues <=
      max_issues, items <= Assistance.Limits.max_suggestions, labels <=
      max_label_bytes, source/response/path bytes <= the supplied protocol
      limits. [max_contract_identity_bytes] separately caps the bounded
      selected-contract encoding/conversion allocation at 8 MiB; this is
      temporary construction work, not wire response bytes. Every phase has a
      fixed allowance and the total per request is
      their bounded sum; no allowance resets per item/import/map callback.
      Result creation and [Owner.checked_response] check the actual encoded
      envelope before publication. Parser/import source bytes also remain
      bounded by the admitted Document context and Parsed_capture budgets. *)
  val create
    :  protocol_limits:P.Authoring.Limits.t
    -> max_scan_work_steps:int
    -> max_assistance_work_steps:int
    -> max_contract_identity_bytes:int
    -> max_contract_work_steps:int
    -> max_projection_work_steps:int
    -> max_parser_input_bytes:int
    -> max_imported_inputs:int
    -> max_selected_tools:int
    -> max_tool_metadata_bytes:int
    -> t Or_error.t
end

module Selected_tool_policy : sig
  module Registry : sig
    type t

    (** Admit only the host's ACTUAL selected registry, never a caller-supplied
        reference or global union. Preflight count before [references] allocates,
        then charge all retained IDs/names/fingerprints and cache the existing
        registry fingerprint under [Limits]. This may checkpoint and happens
        before any final nonyielding currentness comparison. It retains a
        borrowed registry plus bounded immutable reference data, not runnable
        implementations in wire output. An independently authorized registry
        owner must outlive this value. *)
    val admit
      :  Tool_registry.t
      -> limits:Limits.t
      -> checkpoint:(unit -> unit)
      -> (t, Owner.Error.t) result

    val selected : t -> Tool_registry.t
    val equal_identity : t -> t -> bool
  end

  type t

  (** The trusted host policy checks BOTH original and request actors against
      its current selected tool registry and returns a previously admitted
      Registry.t. [None] means no selected tool registry. The callback is
      bounded/non-yielding and never uses the authoring context ID as a grant.
      [context] is the current Core-provided public comparison view, not an
      authority token; Core still checks the physical owner and source grant.
      Re-read it after any yield; losing/changing a previously used registry
      rejects publication rather than silently removing its suggestions.
      Source/root/Build authorization is independent. *)
  val create
    :  current:
         (original_actor:Operator_authorization.t
          -> actor:Operator_authorization.t
          -> context:P.Authoring.Context.t
          -> Registry.t option)
    -> t
end

(** Borrow the SAME Core owner and optional SAME selected-script and decoded
    project-metadata policies as the
    semantic sibling. [None] selection is truthful local-only help; it cannot
    be used to swallow failed Selected.admit or a revoked catalog. The service
    reads an optional generation/source-bound immutable Parsed_capture through
    Core.Current only after an explicit Analyze/refresh/rebuild admitted it.
    It never calls Document_context.capture_parser_bundle or reparses imported
    files. A missing capture leaves authored lexical/structural assistance
    available but no parsed/import/selected-script claim. The source host may
    perform separately authorized bounded directory NAME enumeration for a
    relevant path cursor; it does not read dependency contents, widen a grant,
    or make a fresh project capture. No construction I/O. *)
val create
  :  owner:Owner.t
  -> script_selection:Authoring_script_selection.Policy.t option
  -> project_metadata:Authoring_project_metadata.Policy.t option
  -> selected_tools:Selected_tool_policy.t option
  -> project_build_limits:Chatml_project_build.Limits.t
  -> limits:Limits.t
  -> (t, Owner.Error.t) result

(** Exact project ChatML uses current effective captured bytes. ChatMD Authored
    uses the current root or an exactly retained imported input/source pair;
    malformed/supplied root needs no successful parse. ChatMD Effective requires
    the ACTUAL retained parser input; absent preparation is a typed unavailable
    error, never synthesized effective bytes. Convert emitted ordered spans,
    issues, owner digests and truthful prefix status through portable Lexical
    constructors. This selects only the lexical feature, not core or semantic. *)
val handle_lexical
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring_assistance.Lexical.Request.t
  -> (W.Response.t, Owner.Error.t) result

(** Use exact AUTHORED source/cursor ownership. ChatMD local tag/attribute/value
    and structural template help works on malformed supplied documents through
    a lexical Root input made from the admitted capture; the value does not
    claim it is a successful parser result. Parsed/imported references require
    the actual retained Parsed_capture and its input->captured source pairs.
    Decoded project/environment names may use the one genuine current
    declaration Project_capture plus its shared current host policy, even
    when checking the selected Build failed. Per-script certified catalog
    facts require the one current Authoring_script_selection.Policy and
    genuine Script.assess, and are attempted only at a lexical Script_body
    cursor. ChatML in a
    project uses the current pure effective closure; invalid overlays remain
    lexical-only. Embedded body fallback needs the actual bound extraction map
    and exact authored<->compiled point/replacement proofs. Host-root path
    candidates use independently authorized bounded name-only listing. The
    current service supplies host-root path candidates; decoded-project and
    selected-closure path candidates remain lower producer variants and are
    not emitted by this handler. Selected tool names come only from the current admitted
    Selected_tool_policy registry. Absent parser/catalog/map yields the
    producer's honest local-only or typed Unavailable status, not invented
    semantic names, checked templates or arbitrary path guesses. Recheck
    original/current actor, source/context generation, selected script, tool
    registry and listed root after yields. No callback or wait occurs after
    final admission before successful reply. This selects only the assistance
    feature, not core or semantic. *)
val handle_assistance
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring_assistance.Assistance.Request.t
  -> (W.Response.t, Owner.Error.t) result
