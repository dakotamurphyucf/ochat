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

(** Pure core computation, Task composition, diagnostic Log operations and
    Tool.call. No stdout print, model/process access, conversation mutation,
    session administration, timers, spawning or UI operations. *)
val one_off_v1 : Chatml_builtin_surface.surface

(** Adds typed tool_context, tool_outcome, tool_error, work_ref, tool_limits
    and tool_capability aliases. Approved background operations will be added
    with the separately qualified job service. No moderator state is required. *)
val tool_v1 : Chatml_builtin_surface.surface

(** Host types for non-executing compilation with required_bindings.
    Main takes exactly one argument; run takes exactly two. *)
val one_off_entrypoints : (string * Chatml_builtin_spec.ty) list

val tool_entrypoints : (string * Chatml_builtin_spec.ty) list
