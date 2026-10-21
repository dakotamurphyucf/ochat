open Core

module Projection_receipt : sig
  type t

  val binding : t -> Chatml_prompt_programs.Binding.t
  val compiler_contract : t -> Chatml_source.Digest.t
  val declaration_range : t -> Chatmd_parser_input.Range.t

  type projection =
    | Exact_inline of Prompt.Chat_markdown.Declaration.Extraction.input_span
    | Exact_external of Chatml_source.Location.t
    | Approximate_declaration of Chatmd_parser_input.Range.t

  type error = Prompt.Chat_markdown.Declaration.Extraction.error =
    | Stale_script_source
    | Invalid_compiler_location
    | Invalid_authored_point
  [@@deriving sexp_of]

  (** An immutable receipt from the SAME admitted parser/capture program-source
      construction. It holds no native source capability or callback. Only a
      sealed location in that source/revision is accepted. External exact sites
      require the producer's successful read member/digest to match the same
      captured route/member/bytes used to build the source ID. A mismatch yields
      the declaration as approximate. Query work is finite and checkpointed;
      cancellation exceptions propagate unchanged. *)
  val project
    :  t
    -> Chatml_source.Location.t
    -> checkpoint:(unit -> unit)
    -> (projection, error) Result.t
end

module Captured : sig
  type t

  val programs : t -> Chatml_prompt_programs.t
  val projections : t -> Projection_receipt.t list
end

(** One pass through the existing source-custody path. Projection receipts are
    created only for selected/implicit host-constructed sources, after actual
    Parsed/capture membership, route/member/raw-byte, binding, contract and
    namespace checks. Arbitrary Provided graphs receive no external receipt.
    Receipt count and retained bytes share existing program/source ceilings. *)
val capture_with_projections
  :  Prompt_source_owner.Scope.t
  -> capture:Prompt_source_owner.Capture.t
  -> root_plan:Chat_response.Script_contract_plan.t
  -> role_plans:Chat_response.Script_contract_plan.t list
  -> operation:
       (Chat_response.Script_contract_plan.Program.t -> Chatml_project_build.Operation.t)
  -> provided:(Chatml_prompt_programs.Binding.t * Chatml_prepared_program.t) list
  -> additional_anchors:Chatml_local_roots.Anchor.t list
  -> git_store:Chatml_git_host.Store.t option
  -> git_grants:Chatml_git_host.Grant.t list
  -> root_limits:Chatml_local_roots.Limits.t
  -> max_grants:int
  -> max_grant_bytes:int
  -> max_request_bytes:int
  -> max_path_components:int
  -> max_input_bytes:int
  -> max_work_steps:int
  -> policy:Chatml_project_prepare.Policy.t
  -> limits:Chatml_project_prepare.Limits.t
  -> custody_limits:Chatml_prompt_programs.Limits.t
  -> clock:_ Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> (Captured.t, Prompt_revision_builder.Diagnostic.t list) Result.t

(** Source-only script custody over SAME original admission/scope. No live tool
    capability, compiler or initializer is constructed. Callback is never retained. *)
val capture
  :  Prompt_source_owner.Scope.t
  -> capture:Prompt_source_owner.Capture.t
  -> root_plan:Chat_response.Script_contract_plan.t
  -> role_plans:Chat_response.Script_contract_plan.t list
  -> operation:
       (Chat_response.Script_contract_plan.Program.t -> Chatml_project_build.Operation.t)
  -> provided:(Chatml_prompt_programs.Binding.t * Chatml_prepared_program.t) list
  -> additional_anchors:Chatml_local_roots.Anchor.t list
  -> git_store:Chatml_git_host.Store.t option
  -> git_grants:Chatml_git_host.Grant.t list
  -> root_limits:Chatml_local_roots.Limits.t
  -> max_grants:int
  -> max_grant_bytes:int
  -> max_request_bytes:int
  -> max_path_components:int
  -> max_input_bytes:int
  -> max_work_steps:int
  -> policy:Chatml_project_prepare.Policy.t
  -> limits:Chatml_project_prepare.Limits.t
  -> custody_limits:Chatml_prompt_programs.Limits.t
  -> clock:_ Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_prompt_programs.t, Prompt_revision_builder.Diagnostic.t list) Result.t

(** [root_plan] must belong to the actual original Root Parsed value in Capture.
    [role_plans] must belong to that same Root or actual queued-root Parsed values.
    Only [root_plan] binding+contract pairs become active. Explicitly requested
    inactive roles may share the original Root; union binding+contract variants,
    deduplicate common roles before source preparation. No unrequested variants.
    [operation] is invoked exactly once per unique configured program, after whole
    helper polling and before its request is retained. Its actual returned
    operation is forwarded to the existing project request; it is not invoked for
    unconfigured/Provided rows. Original callback exceptions propagate unchanged.
    The callback is borrowed only for this invocation and never retained.
    Bound pre-dedup work and preflight retained unique programs before acquisition. Supplied Provided
    graphs must match one unconfigured binding PLUS authentic contract/body;
    duplicate/surplus/selected-project substitution rejects. In a wholly
    unconfigured unsupplied context, retain parser-only custody/legacy light path.
    Mixed configured contexts give unconfigured executable siblings genuine
    implicit Provided graphs. External entries keep actual original-root member
    IDs; inline bodies use honest fragment IDs plus Binding declaration context.

    Selected requests execute in ONE existing source batch/Attempt. No native
    reads outside that batch, request-level budget reset or live-tool admission.
    The immutable custody inventory accounts its joint authored/private payloads and
    cumulative canonical graph inputs before publication. [max_input_bytes]
    additionally bounds helper-retained entry bytes and Provided graph admission;
    [max_work_steps] is ONE separate whole-helper cooperative callback allowance,
    including route/anchor normalization and final custody. It is not the native
    Attempt receipt or a per-script reset; native and custody limits remain
    unchanged. Metadata/cardinality stay under actual parser/plan/custody ceilings.
    Original checkpoint/preparation exceptions propagate unchanged; typed failures
    return no partial custody. Source mode and future child-role choices are the
    caller's explicit selection, not inferred defaults. *)
