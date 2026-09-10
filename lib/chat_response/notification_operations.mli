open Core

type correlation =
  { key : string
  ; invocation_id : Agent_protocol.Id.Invocation.t option
  ; work : Agent_protocol.Invocation.work option
  }

(** Host callbacks enforce live moderator/source ownership, correlation, result
    disclosure and admission limits. Publication returns a staged intent, not a
    claim that a provider has received it. Reads observe the scoped provisional
    state. Registration supplies no authority by itself. *)
type handlers =
  { publish :
      correlation:correlation
      -> completion:Agent_protocol.Completion.t
      -> wake:Agent_protocol.Completion.wake
      -> (int * Agent_protocol.Delivery.t, string) result
  ; get : Agent_protocol.Id.Delivery.t -> (Agent_protocol.Delivery.t, string) result
  ; rollback : int -> unit
  }

type transaction =
  { handlers : handlers
  ; prepare : int list -> (unit -> unit, string) result
  }

val dynamic_handlers : (unit -> transaction option) -> handlers

(** Uses private ordered mutation receipts. Catch rolls back staged intent, and
    prepare acknowledgement runs only after the owning durable save. *)
val install
  :  ?control:Chatml.Chatml_lang.execution_control
  -> handlers:handlers
  -> Chatml_host_runtime.runtime_config
  -> Chatml_host_runtime.runtime_config

val split_mutations
  :  Chatml.Chatml_lang.eff list
  -> (int list * Chatml.Chatml_lang.eff list, string) result
