open! Core
module P = Agent_protocol
module Repl = Developer_repl_epoch
module Evaluator = Chatml_repl_execution

(** One effectful Task action over OCH-413's retained interpreter. This module
    owns no evaluator, scheduler, Store writer or Root. It constructs the
    selected operation table from the exact Host-issued Root/native/Job
    package and installs it inside the existing Root action and Job scope.
    No synthetic function, command entrypoint or client Lang.Task is used.
    [run] consumes [started] once for [root] before interpreter entry. It
    repeats full REPL origin/source/actor and Root currentness after yielding
    preparation and native/Job/provider calls, with a nonyielding physical
    suffix at each effect. Source/handle checks never come from latest snippet.
    The evaluator's original runner and control remain active throughout;
    action policy narrows that control. Callback exceptions and cancellation
    preserve identity after joined Root/Job cleanup. The Host alone maps this
    bounded result to the durable Task receipt and owns uncertain Finish.
    The actual Root action seals only surviving Job/Model selections from
    [drive_retained]'s borrowed [prepare_result]; caught/discarded provisional
    work is rolled back, and a whole-action error releases any uncommitted
    reservations. Once an external effect or durable selection committed,
    later failure or unknown Finish retains custody and never repeats it. *)
val run
  :  root:Developer_root_owner.run
  -> jobs:Developer_script_job_service.t
  -> native:Developer_native_executor.t
  -> authorize:
       (P.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, P.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t -> (Jsonaf.t, P.Error.t) result)
  -> borrowed:Repl.borrowed
  -> origin:Repl.Task_origin.t
  -> handle:Evaluator.Task_handle.t
  -> started:Developer_command_lifecycle.Active.Started.t
  -> action_policy:Chatml_execution.policy
  -> check_current:(unit -> (unit, P.Error.t) result)
  -> (Evaluator.Task_result.t, P.Error.t) result
