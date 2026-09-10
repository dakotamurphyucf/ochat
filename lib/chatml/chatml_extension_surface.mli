(** Explicit version-1 computation surfaces for authored tool scripts.
    Surface availability is a compiler contract, not a capability grant or a
    host feature advertisement. Every Tool.call must still pass execution
    admission against the invocation's bound, selected capabilities. *)

val work_ref_ty : Chatml_builtin_spec.ty
val tool_error_ty : Chatml_builtin_spec.ty
val tool_outcome_ty : Chatml_builtin_spec.ty
val tool_context_ty : Chatml_builtin_spec.ty
val limits_ty : Chatml_builtin_spec.ty
val capability_ty : Chatml_builtin_spec.ty
val origin_ty : Chatml_builtin_spec.ty

(** Pure core computation, Task composition, diagnostic Log operations, Tool.call
    and the Job interface. Tool.spawn aliases Job.start_tool. Job operations require
    explicit host installation; starts reserve transactional intent, never execute
    before the owning commit. No stdout print, ambient model/process access,
    conversation mutation, session administration, timers or UI operations. *)
val one_off_v1 : Chatml_builtin_surface.surface

(** Adds typed tool_context, tool_outcome, tool_error, work_ref, tool_limits
    and tool_capability aliases. No moderator state is required. *)
val tool_v1 : Chatml_builtin_surface.surface

(** Host types for non-executing compilation with required_bindings.
    Main takes exactly one argument; run takes exactly two. *)
val one_off_entrypoints : (string * Chatml_builtin_spec.ty) list

val tool_entrypoints : (string * Chatml_builtin_spec.ty) list
val invocation_event_ty : Chatml_builtin_spec.ty
val completion_ty : Chatml_builtin_spec.ty
val work_completion_ty : Chatml_builtin_spec.ty
val moderator_event_ty : Chatml_builtin_spec.ty

(** Opt-in extensibility-v1 moderator contract. Adds Invocation.resolve,
    moderator-owned Subscription create/get/complete/fail/cancel operations and
    typed Tool_invoked/Job_completed/Subscription_expired events. Runtime.emit
    and Schedule.after_ms accept JSON data only, using distinct host operations
    Runtime.emit_json and Schedule.after_ms_json. Their host adapters must wrap
    payloads as Internal_event; they must never decode caller JSON as a native
    event constructor. Legacy surfaces and their event behavior are unchanged.
    Subscription.create takes kind, optional lifetime_ms and wake_policy
    (Request_turn/Next_turn/No_wake), returning a task of string identity. Get
    returns JSON status; complete/fail/cancel take identity, expected epoch and
    respectively JSON success, tool_error or cancellation reason, returning the
    retained JSON status. Internal transaction receipts are hidden from scripts.
    Operation availability still requires a qualified host implementation. *)
val moderator_v1 : Chatml_builtin_surface.surface

val moderator_entrypoints : (string * Chatml_builtin_spec.ty) list

(** Moderator contract for generated child definitions. Tool calls use the
    inherited registry; direct recipe-model and process modules are excluded.
    Owning-session mutations still require the delegated host's admission. *)
val delegated_moderator_v1 : Chatml_builtin_surface.surface
