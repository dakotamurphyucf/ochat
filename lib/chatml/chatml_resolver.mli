(** ChatML resolver — lexical-address resolution and lowering to the
    resolved evaluator AST. *)

open Chatml.Chatml_lang

(** Resolve a program using an existing successful typechecking snapshot. *)
val resolve_checked_program
  :  ?checkpoint:(unit -> unit)
  -> Chatml_typechecker.checked_program
  -> program
  -> resolved_program

(** Resolve an owned parser unit only with annotations from the same captured
    source identity and digest. Different identities/revisions fail before span
    lookup; successful resolution preserves all original byte spans. *)
val resolve_captured_program
  :  Chatml_typechecker.checked_program
  -> Chatml.Chatml_parse.Unit.t
  -> (resolved_program, Chatml_diagnostic.t) result

(** Resolve a program using the default typechecker entrypoint. *)
val resolve_program : program -> resolved_program

(** Failure mode for the end-to-end [run_program] helper. *)
type eval_error =
  | Type_diagnostic of Chatml_typechecker.diagnostic
  | Runtime_diagnostic of runtime_error

(** Typecheck, resolve, and evaluate a program into an existing
    environment, returning either a type or runtime diagnostic. *)
val run_program : env -> program -> (unit, eval_error) result

(** Typecheck, resolve, and evaluate a program, reporting only
    typechecking failures. *)
val typecheck_resolve_and_eval
  :  env
  -> program
  -> (unit, Chatml_typechecker.diagnostic) result

(** Typecheck and evaluate a program using the default pipeline expected by
    most tests and embedders. *)
val eval_program : env -> program -> (unit, Chatml_typechecker.diagnostic) result

open Core

(** Resolve an admitted successful graph unit through the existing resolver.
    Verify source identity/digest before reading annotations. At EVar, owned
    Value_read annotation wins over lexical-address lowering and becomes a
    canonical REConfigured_read. Unannotated variables use the unchanged lexical
    resolver. Ordinary inline/first-class SOpen remains RSOpen; ONLY an owned
    Inert_open annotation erases its configured statement. No second lexical
    resolver, uppercase scan, catalog seeding or retrospective edge discovery.
    Keep all original source spans. Poll every input-sized traversal, including
    patterns/slot layout and the final result, without retaining checkpoint.
    Checked schemes are read only to choose immutable slot tags; no type cells
    occur in the resulting resolved AST. *)
val resolve_configured_program
  :  Chatml_typechecker.checked_program
  -> Chatml.Chatml_parse.Unit.t
  -> checkpoint:(unit -> unit)
  -> (Chatml.Chatml_lang.resolved_program, Chatml_diagnostic.t) Result.t

(** Existing resolve_checked_program/resolve_captured_program remain adapters
    over the same traversal. Add optional checkpoint with no-op default if needed
    for compatibility; graph compilation supplies its invocation callback. *)
