open Core

(** Shared local CLI options. Parsing is pure; host creation still uses the
    daemon's validated authoring budget contract. *)
type t

val param : t Command.Param.t
val is_configured : t -> bool

(** No flags yields [None], preserving a supplied host's existing settings.
    Explicit flags use normal host defaults for omitted fields and validate
    the complete combination before any host startup or file access. *)
val resolve : t -> Chat_response.Authoring_validation.context_budget option Or_error.t
