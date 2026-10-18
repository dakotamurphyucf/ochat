open! Core
module P = Agent_protocol

(** Immutable comparison data for one explicit retained-Task action. Its
    reference was issued with the exact Task origin by the REPL actor, but
    decoding this value cannot borrow a Task, select a source or start code.
    A historical same-key read compares these bytes before borrowing a
    process-local handle; after restart that comparison can report the
    persisted result but cannot recreate the expired evaluator. *)
type t

(** Validate finite requested execution/output limits and use a versioned
    Task-specific canonical envelope. This is distinct from the command
    entrypoint/input/fixtures fingerprint: no synthetic command or dummy
    capture is constructed. The reference binds the issuing instance, epoch,
    Task ordinal and retained source identity. *)
val create
  :  reference:Developer_repl_epoch.Task_reference.t
  -> requested_execution:Chatml_execution.limits option
  -> requested_max_output_bytes:int option
  -> checkpoint:(unit -> unit)
  -> (t, P.Error.t) result

val reference : t -> Developer_repl_epoch.Task_reference.t
val requested_execution : t -> Chatml_execution.limits option
val requested_max_output_bytes : t -> int option
val fingerprint : t -> Chatml_source.Digest.t
