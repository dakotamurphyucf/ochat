(** Moderator-only registration operations. The host captures producer, source,
    policy and lifetime; script arguments never establish that authority. *)
type handlers =
  { register :
      subscription_id:Agent_protocol.Id.Subscription.t
      -> expected_epoch:int
      -> namespace:string
      -> schema:Jsonaf.t
      -> (int * Agent_protocol.Id.Capability.t, string) result
  ; get : Agent_protocol.Id.Capability.t -> (Jsonaf.t, string) result
  ; revoke :
      Agent_protocol.Id.Capability.t -> reason:string -> (int * Jsonaf.t, string) result
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
