(** The optional extensibility-v1 moderator hook contract. Its events have a
    separate closed type from ordinary [on_event] events. This module's helpers
    compose proposals and grant no authorization. *)

val entrypoint : string
val scheme : Chatml_builtin_spec.ty
val aliases : Chatml_builtin_surface.builtin_type_alias list
val builtin : Chatml_builtin_spec.builtin_module
