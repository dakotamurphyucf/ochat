open Core

(** Exact retained managed authority before recompilation. Script bodies/project
    selections and diagnostic source locations may change; script identity/kind/
    limits, tool target/schema/uses and authoring help must remain unchanged.
    Each input has at most 16,384 elements. Traversal and input-sized comparisons
    invoke the borrowed whole-operation checkpoint before retention/comparison;
    it must enforce the caller's existing work/deadline allowance. No callback,
    source, compiler context or native capability is retained by this validation.
    Original callback failures and cancellation propagate without mapping. *)
val validate
  :  original:Prompt.Chat_markdown.top_level_elements list
  -> selected:Prompt.Chat_markdown.top_level_elements list
  -> checkpoint:(unit -> unit)
  -> (unit, Chatmd_shell_spec.Diagnostic.t list) Result.t
