(** Immutable description of a validated configuration replacement. *)

type ids =
  { added : string list
  ; removed : string list
  ; changed : string list
  }
[@@deriving compare, equal, sexp]

type t =
  { server_changed : bool
  ; workspaces : ids
  ; prompts : ids
  ; permission_profiles : ids
  ; manifest_grants : ids
  }
[@@deriving compare, equal, sexp]

val between : previous:Config.t -> current:Config.t -> t
val is_empty : t -> bool
