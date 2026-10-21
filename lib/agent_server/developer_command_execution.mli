open! Core
module P = Agent_protocol
module Receipt = Agent_store.Developer_command_receipt

(** Private projection through the EXISTING portable Check/Fixture engine and
    captured One_off_v1 interpreter. No Store writer, scheduler, provider
    resolver or result ledger lives here. Receipt creation applies the same
    bounded diagnostics/output limits used by command persistence. *)

module Check : sig
  type t

  (** Compile exact captured code under the admitted compilation/dependency
      budget. Persisted Row.id supplies the portable Check.Id: no retry mints
      a second identity. A compiler rejection becomes a truthful bounded
      Check receipt, not a failed source pre-capture request. No Root or
      language effect is entered. Cancellation retains identity/backtrace. *)
  val run
    :  capture:Chatml_developer_capture.t
    -> id:Receipt.Id.t
    -> budgets:Developer_command_budgets.t
    -> env:Eio_unix.Stdenv.base
    -> (t * Receipt.t, P.Error.t) result

  (** Exact successful checked value for a subsequent Fixture. A copied
      Check receipt or equal-looking capture cannot supply the linked artifact.
      The same captured run retains this opaque value only within its host
      owner lifetime. Restart Recovery terminalizes any abandoned Reserved
      row as Interrupted, so its same key cannot replay; a new admitted key
      may perform a fresh pure Check. *)
  val checked : t -> Chatml_developer_test.Check.t
end

module Fixture : sig
  (** Deterministic fixture-only execution of the exact checked capture.
      The persisted Row.id maps to portable Run_id. All forbidden Job/Model/
      native effects remain forbidden by the portable engine; no fake Root is
      constructed. A stale capture/check mismatch yields a bounded failure.
      Unexpected exception/cancellation propagates after joined cleanup. *)
  val run
    :  capture:Chatml_developer_capture.t
    -> check:Chatml_developer_test.Check.t
    -> tool_selection:Chatml_developer_test.Tool_v1.Selection.t option
    -> admit:(unit -> bool)
    -> id:Receipt.Id.t
    -> budgets:Developer_command_budgets.t
    -> env:Eio_unix.Stdenv.base
    -> (Receipt.t, P.Error.t) result
end

module Live : sig
  type t

  (** Exact captured Prepared under its already active Root. [jobs] and
      [native] must be physically issued by [root]; the host repeats this
      check. One shared Root.Job_result owner is used by nested Tool/Script
      and Model work, and the sole scheduler owns their candidates. This
      constructor creates no second Root or native kernel. *)
  val create
    :  root:Developer_root_owner.run
    -> jobs:Developer_script_job_service.t
    -> native:Developer_native_executor.t
    -> authorize:
         (P.Host_invocation.t
          -> Chat_response.Tool_capability.binding
          -> (unit, P.Error.t) result)
    -> prepare_output:
         (Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, P.Error.t) result)
    -> capture:Chatml_developer_capture.t
    -> budgets:Developer_command_budgets.t
    -> env:Eio_unix.Stdenv.base
    -> (t, P.Error.t) result

  (** Called only inside the matching daemon-owned Active callback. [started]
      is its one-shot proof that the unique durable Execution_start was
      consumed; this function consumes it for the exact physical [root]
      before user code. A Reserved/reconciled/historical row cannot supply it.
      Run the existing prepared
      interpreter inside one physical Root action and lexical Job scope; use
      the selected native executor for Tool.call. Source/current actor/native
      permission and parent checks are repeated after waits and before each
      effect. Bare One_off_v1 has no top-level Model.spawn. An admitted managed
      Developer_tool_v1 callback may use its separately authorized Model path.
      Task.catch/discard and completed external effects preserve existing
      semantics; a later failure does not roll them back. Return bounded
      terminal truth for the one Store.Command.finish. No callback replay after
      uncertain phase/result save. *)
  val run
    :  t
    -> started:Developer_command_lifecycle.Active.Started.t
    -> (Receipt.t, P.Error.t) result
end
