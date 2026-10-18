open! Core

(** Bounded local-only S-expression syntax and closed principal-coordinate
    binding. This private grammar neither constructs Config grants nor resolves
    source authority; Config_validator runs the ordinary section parsers next. *)

type error =
  { path : string
  ; message : string
  }

val finite_syntax : string -> bool
val bind_project : Agent_protocol.Id.Principal.t -> Sexp.t -> (Sexp.t, error) result
val bind_upload : Agent_protocol.Id.Principal.t -> Sexp.t -> (Sexp.t, error) result
val bind_developer : Agent_protocol.Id.Principal.t -> Sexp.t -> (Sexp.t, error) result
val bind_manifest : Agent_protocol.Id.Principal.t -> Sexp.t -> (Sexp.t, error) result
