open Core

(** Source-only script custody over SAME original admission/scope. No live tool
    capability, compiler or initializer is constructed. Callback is never retained. *)
val capture
  :  Prompt_source_owner.Scope.t
  -> capture:Prompt_source_owner.Capture.t
  -> plans:Chat_response.Script_contract_plan.t list
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

(** All plans must belong to actual root/queued-root Parsed values in Capture,
    exactly ONE current Root role plan. Queued Parsed contexts may have several
    explicitly requested authentic role plans: union binding+contract variants,
    deduplicate common roles before source preparation. No unrequested variants.
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
