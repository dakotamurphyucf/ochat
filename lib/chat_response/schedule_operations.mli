open Core

(** Moderator-only adapter. Callbacks enforce live source/owner, generation,
    quotas and timer binding. Mutations return private rollback receipts. *)
type handlers =
  { create :
      delay_ms:int
      -> payload:Jsonaf.t
      -> misfire:Agent_protocol.Schedule.misfire
      -> (int * Agent_protocol.Schedule.t, string) result
  ; get : Agent_protocol.Id.Schedule.t -> (Agent_protocol.Schedule.t, string) result
  ; cancel : Agent_protocol.Id.Schedule.t -> (int, string) result
  ; rollback : int -> unit
  }

type transaction =
  { handlers : handlers
  ; prepare : int list -> (unit -> unit, string) result
  }

val dynamic_handlers : (unit -> transaction option) -> handlers

val install
  :  ?control:Chatml.Chatml_lang.execution_control
  -> handlers:handlers
  -> Chatml_host_runtime.runtime_config
  -> Chatml_host_runtime.runtime_config

val split_mutations
  :  Chatml.Chatml_lang.eff list
  -> (int list * Chatml.Chatml_lang.eff list, string) result
