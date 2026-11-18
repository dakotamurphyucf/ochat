(** Immutable moderator source identity. This is selector DATA, not installation
    custody or source access. Invocation keeps its public manifest alias. *)
type t =
  { script_id : string
  ; source_sha256 : string
  }
[@@deriving equal, sexp]
