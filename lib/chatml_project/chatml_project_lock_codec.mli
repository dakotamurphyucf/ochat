open Core

(** Private lock wire owner: actual receipt primitives, no graph/capture admission.
    One decode context shares a whole-source origin. Encoding retains no JSON tree
    or callback; exact JSON measurement precedes bounded compact rendering. *)
val decode
  :  Chatml_source.t
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_project_lock_receipt.t, Chatml_project_declaration.Error.t) Result.t

val to_string
  :  Chatml_project_lock_receipt.t
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (string, Chatml_project_declaration.Error.t) Result.t
