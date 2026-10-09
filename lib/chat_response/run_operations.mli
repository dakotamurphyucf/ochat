open! Core

(** Run decisions exist only inside the actual actor-issued callback scope.
    Staging changes no durable state and executes no work. Catch rollback drops
    its receipt. Preparing selects surviving receipts and rejects incompatible
    decisions before the host checkpoint is persisted. *)
type handlers =
  { stage : Agent_protocol.Run_action.t -> (int, string) result
  ; rollback : int -> unit
  }

type transaction =
  { handlers : handlers
  ; prepare : int list -> (Agent_protocol.Run_action.t option, string) result
  }

val dynamic_handlers : (unit -> transaction option) -> handlers

val install
  :  ?control:Chatml.Chatml_lang.execution_control
  -> handlers:handlers
  -> Chatml_host_runtime.runtime_config
  -> Chatml_host_runtime.runtime_config

(** Remove only checked run decision receipts; preserve every unrelated effect's
    order. Receipts are not authority. The actor rechecks callback ownership,
    principal, exact source installation, generation and revision at commit. *)
val split_actions
  :  Chatml.Chatml_lang.eff list
  -> (int list * Chatml.Chatml_lang.eff list, string) result
