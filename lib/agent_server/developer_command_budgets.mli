open! Core
module P = Agent_protocol

(** Immutable daemon-selected Check/Fixture/Live budget bundle. Requested
    values from Developer_command_original are comparison inputs, not a way to
    widen these ceilings. Root, compiler, runtime and Store each repeat their
    own lower admission. *)
type t

module Configuration : sig
  type t

  (** Validate explicit daemon-selected compilation/dependency policy,
      ChatML execution limits, aggregate output bytes and receipt JSON
      limits before the host opens its Store. This inert value is not bound to
      persistence and cannot be used directly for command execution. *)
  val create
    :  compilation:Chatml_compilation.policy
    -> max_dependency_depth:int
    -> execution:Chatml_execution.limits
    -> max_output_bytes:int
    -> receipt:Document_schema.Limits.t
    -> (t, P.Error.t) result
end

(** After Developer_host opens its physical Store, bind the validated
    Configuration to that Store's configured command/document caps. A
    missing/mismatched physical limit is an error; there is no default
    fallback. [Store.limits] supplies the exact immutable configured
    [Limits.document] and [Limits.commands]; neither a handle nor a caller
    default is needed. The effective whole-receipt JSON byte ceiling is the
    minimum of the configured receipt limit and Store's exact terminal cap,
    so an overlarge projected terminal is rejected or converted to a bounded
    limit receipt before Store Finish selects its output.
    This causes no source/provider effect. *)
val bind
  :  Configuration.t
  -> store:Agent_store.Developer_run_store.t
  -> (t, P.Error.t) result

val compilation : t -> Chatml_compilation.policy
val max_dependency_depth : t -> int
val execution : t -> Chatml_execution.limits
val max_output_bytes : t -> int
val receipt : t -> Document_schema.Limits.t

(** Apply this exact immutable Original's requested execution/output ceilings
    to the installed host ceilings. Every numeric dimension is narrowed with
    its matching host dimension; absent request means the host value. The
    returned budget cannot be wider and is the one used for execution and
    terminal receipt admission. Compile/dependency policy remains selected by
    the authenticated host, not by a request digest. This is pure comparison
    and does not grant any effect. *)
val for_original : t -> Developer_command_original.t -> (t, P.Error.t) result

(** Narrow the same installed host/Store bundle for a retained REPL Task's
    explicit requested limits. The effective receipt byte ceiling also
    respects the Task output cap. Reject a request that cannot represent the
    conservative Task limit fallback before Reserve or any effect. No Command
    Original, entrypoint or source authority is constructed. *)
val for_repl_task
  :  t
  -> requested_execution:Chatml_execution.limits option
  -> requested_max_output_bytes:int option
  -> (t, P.Error.t) result
