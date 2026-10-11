open Core

(** Prepare one actual captured definition against its durable dependency history.
    This owner never publishes a catalog, index or mutable project lock. *)
module Mode : sig
  type t =
    | Build
    | Update
  [@@deriving equal, sexp_of]
end

module Prepared : sig
  type t

  val programs : t -> Chatml_prompt_programs.t
  val association : t -> Prompt_project_baseline.Prepared.t
end

(** Read the previous selection for [capture]'s exact original operator receipt.
    Reconcile established history with the exact saved artifact without compiling
    its old graph. Only authoritative first admission permits Initial; a known
    new slot or Provided input uses Local-only Build_without_previous. An Update
    is an explicitly authorized host operation, never inferred from edited code.
    Missing/corrupt history rejects configured project requests before acquisition;
    ordinary scripts without selected projects need no prior lock.

    The same startup and cooperative work allowance cover history lookup,
    reconciliation, operation selection, the single existing source batch and
    authentic next role associations. Limits and source grants are supplied by
    the existing owners; no callback, startup or live authority survives return.
    Expected failures are source-owned diagnostics. Unexpected callback failures
    and cancellation retain original identity/backtrace.

    The host must retain [Prepared.association] until the actual successfully
    installed artifact can finalize Baseline.Success. It must snapshot/seal its
    actual catalog generation and this index generation together. This function
    supplies no publication reservation or independent source authority. *)
val capture
  :  Prompt_script_preparation.Policy.t
  -> ?startup:Chat_response.Startup_compilation.t
  -> mode:Mode.t
  -> snapshot:Prompt_project_baseline_store.Snapshot.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> baseline_limits:Prompt_project_baseline.Limits.t
  -> custody_limits:Chatml_prompt_programs.Limits.t
  -> root_role:Prompt_script_preparation.Root_role.t
  -> host:Prompt_script_preparation.Host.t
  -> scope:Prompt_source_owner.Scope.t
  -> capture:Prompt_source_owner.Capture.t
  -> provided:(Chatml_prompt_programs.Binding.t * Chatml_prepared_program.t) list
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> unit
  -> (Prepared.t, Prompt_revision_builder.Diagnostic.t list) Result.t
