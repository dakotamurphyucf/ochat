open Core

(** Existing host-selected invocation family, independent of authored source
    selection. Persistent child references still request delegated UI. *)
module Root_role = Chat_response.Script_contract_plan.Program.Role.Moderator_invocation

(** Source-capture policy is distinct from runtime restoration policy; there is no
    tool registry, inference worker, semantic restoration policy or initialized
    runtime in this owner. *)
module Source_policy : sig
  type t

  (** Exact existing Prompt_script_sources ceilings; no defaults or remapping.
      Component constructors have already validated roots/native/composition and
      custody policies. Positive helper/input/path/request and nonnegative grant
      ceilings are validated before retention. *)
  val create
    :  root_limits:Chatml_local_roots.Limits.t
    -> max_grants:int
    -> max_grant_bytes:int
    -> max_request_bytes:int
    -> max_path_components:int
    -> max_input_bytes:int
    -> max_work_steps:int
    -> policy:Chatml_project_prepare.Policy.t
    -> limits:Chatml_project_prepare.Limits.t
    -> custody_limits:Chatml_prompt_programs.Limits.t
    -> t Or_error.t
end

module Policy : sig
  type t

  val compilation : t -> Chatml_compilation.limits

  (** One source-only callback's policy. Compilation supplies the original startup
      deadline; it does not authorize compilation or activation. Role/reference
      counts are bounded before queue/list retention. Work covers ALL role planning,
      actual-reference joins and delegated source-helper checkpoints, rather than
      resetting for each Parsed or role. Units are cooperative phase polls, not
      CPU operations; the existing pure shell-manifest stage additionally retains
      its admitted parser-input bounds. Source_helper's independent whole-operation
      and native Source_batch ceilings remain enforced without widening. *)
  val create
    :  compilation:Chatml_compilation.limits
    -> planning:Chat_response.Script_contract_plan.Limits.t
    -> sources:Source_policy.t
    -> max_context_roles:int
    -> max_reference_visits:int
    -> max_work_steps:int
    -> t Or_error.t
end

module Host : sig
  (** Borrow actual existing grants/store only. The original primary root is held
      privately by Scope; no native path reversal or artifact lookup is exposed.
      Store/anchors must remain open until capture returns and joined cleanup ends. *)
  type t =
    { additional_anchors : Chatml_local_roots.Anchor.t list
    ; git_store : Chatml_git_host.Store.t option
    ; git_grants : Chatml_git_host.Grant.t list
    }
end

(** Result of one complete source preparation. Retains the exact plans used for
    that acquisition, including inactive queued roles, so the host can associate
    successful program selections with their original roles without replanning.
    Immutable parser/custody values only; no Scope, Startup, native grant or
    callback is retained. *)
module Prepared : sig
  type t

  val programs : t -> Chatml_prompt_programs.t

  (** The first plan is the active Root; remaining plans are the authentic
      requested roles in acquisition order. Each keeps its original Parsed
      identity from the supplied Capture. No roles are inferred from digests. *)
  val plans : t -> Chat_response.Script_contract_plan.t list

  (** Original plans for contexts whose programs were actually retained. An
      unconfigured context with no supplied graphs keeps the lightweight implicit
      execution path and has no captured program rows. Configured contexts always
      remain in this subset, so missing configured custody cannot be disguised
      as an implicit context. Use these plans for Baseline.Prepared.create. *)
  val captured_plans : t -> Chat_response.Script_contract_plan.t list
end

(** Suitable for Catalog.prepare_owned's existing prepare_programs callback.
    Seed the actual Capture.root with the explicitly supplied host [root_role]. Join each authentic emitted
    Parsed.Agent_reference.queued_key to the exact captured root/agents key,
    rejecting missing/duplicate queued keys before native preparation. Tool and
    Content request headless; Persistent_tool requests delegated UI. Walk only
    reachable contexts, once per actual physical Parsed for reference discovery;
    deduplicate requested (physical Parsed,role) pairs before Plan.create. Cycles
    to original Root add explicit inactive roles, never another capture.

    A None queued_key is genuinely external/nonlocal and supplies no retained
    preparation role; this API neither reopens it nor decides future source mode.
    Plan each role using authentic existing surface/target and pure shell material
    under SAME startup. Only the selected root-role Plan is active; all and only emitted requested
    roles become role_plans. One Prompt_script_sources.capture performs the complete
    unique program acquisition batch, with supplied explicit host operations.

    [operation] is borrowed, passed once per unique configured Plan.Program and
    never guessed from path/body. It must supply any previous lock association from
    the host's actual definition/record selection; previous lock is immutable input
    to the existing gate and grants no native authority. The returned custody contains immutable selected lock bytes; this helper
    never publishes a mutable source or lock outside that custody. Return source-owned diagnostics on expected failures;
    original borrowed callback/cancellation/backtrace identity propagates.
    Project [Prepared.programs] for Catalog's program callback. Retain the same
    result's [Prepared.captured_plans] when establishing a successful dependency baseline;
    do not rebuild plans against a later parser or compiler contract.
    No callback, Scope, Startup or grants are retained in the result. *)
val capture
  :  Policy.t
  -> ?startup:Chat_response.Startup_compilation.t
  -> root_role:Root_role.t
  -> host:Host.t
  -> scope:Prompt_source_owner.Scope.t
  -> capture:Prompt_source_owner.Capture.t
  -> operation:
       (Chat_response.Script_contract_plan.Program.t -> Chatml_project_build.Operation.t)
  -> provided:(Chatml_prompt_programs.Binding.t * Chatml_prepared_program.t) list
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> unit
  -> (Prepared.t, Prompt_revision_builder.Diagnostic.t list) Result.t
