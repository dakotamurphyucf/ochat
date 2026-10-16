open! Core
module P = Agent_protocol
module Receipt = Agent_store.Developer_command_receipt

(** Pure Check and bounded fixture projections over the existing captured
    One_off_v1 or Tool_v1 engine. This module has no Root, Store writer or
    scheduler. The Tool_v1 selection comes from the same source Candidate and
    authenticated host policy; an ordered Tool.call step never selects it. *)

(** Admit the whole completed receipt under the Store-bound effective terminal
    cap before handing it to Lifecycle.Finish. *)
val project_result
  :  Developer_command_budgets.t
  -> Receipt.value
  -> (Receipt.t, Document_schema.Error.t) result

val project
  :  Developer_command_budgets.t
  -> Receipt.value
  -> (Receipt.t, P.Error.t) result

module Check : sig
  type t

  val run
    :  capture:Chatml_developer_capture.t
    -> id:Receipt.Id.t
    -> budgets:Developer_command_budgets.t
    -> env:Eio_unix.Stdenv.base
    -> (t * Receipt.t, P.Error.t) result

  val checked : t -> Chatml_developer_test.Check.t
end

module Fixture : sig
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

(** One portable callback body for a freshly reserved command. The persisted
    receipt ID is reused for Check.Id or Run_id; Check and Fixture share the
    same physical captured Prepared and admitted budget. A rejected Check is
    passed to the existing fixture engine so its compile failure is reported
    under Fixture_run rather than installing an effectful runtime. Live_run
    belongs to the physical Root execution path and rejects here. No Store
    mutation or callback launch happens in this function. *)
val run_kind
  :  kind:Receipt.Kind.t
  -> capture:Chatml_developer_capture.t
  -> tool_selection:Chatml_developer_test.Tool_v1.Selection.t option
  -> admit:(unit -> bool)
  -> id:Receipt.Id.t
  -> budgets:Developer_command_budgets.t
  -> env:Eio_unix.Stdenv.base
  -> (Receipt.t, P.Error.t) result
