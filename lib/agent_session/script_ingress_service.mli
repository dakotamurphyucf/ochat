type host =
  { register :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> subscription_id:Agent_protocol.Id.Subscription.t
      -> expected_epoch:int
      -> namespace:string
      -> schema:Jsonaf.t
      -> (int * External_ingress.t, Agent_protocol.Error.t) result
  ; get :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> Agent_protocol.Id.Capability.t
      -> (External_ingress.t, Agent_protocol.Error.t) result
  ; revoke :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> Agent_protocol.Id.Capability.t
      -> reason:string
      -> (int * External_ingress.t, Agent_protocol.Error.t) result
  ; select :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> int list
      -> (unit, Agent_protocol.Error.t) result
  ; abort : Agent_protocol.Job.launch_owner -> int -> unit
  }

type t
type scope

val create : host:host -> t

(** Lexical moderator ownership with private receipt tracking. Aborted scopes
    release reservations; successful scopes acknowledge only after durable save. *)
val with_scope
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> error:(string -> 'error)
  -> (scope -> ('a, 'error) result)
  -> ('a, 'error) result

val moderator_transaction : scope -> Chat_response.Ingress_operations.transaction
