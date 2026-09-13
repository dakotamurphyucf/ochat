type host =
  { create :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> correlation:Chat_response.Notification_operations.correlation
      -> completion:Agent_protocol.Completion.t
      -> wake:Agent_protocol.Completion.wake
      -> disclosure_pins:(string * string) list
      -> (int * Agent_protocol.Delivery.t, Agent_protocol.Error.t) result
  ; get :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> Agent_protocol.Id.Delivery.t
      -> (Agent_protocol.Delivery.t, Agent_protocol.Error.t) result
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

(** The actor owns source/correlation/result validation and persistence. This
    scope adds job capability checks, private receipt cleanup and cancellation-
    protected admission. It expires on return and acknowledges only after save. *)
val with_scope
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> selected:Chat_response.Tool_capability.t
  -> jobs:Script_job_service.scope option
  -> error:(string -> 'error)
  -> (scope -> ('a, 'error) result)
  -> ('a, 'error) result

val moderator_transaction : scope -> Chat_response.Notification_operations.transaction

(** Re-admit the exact persisted disclosure ceiling against actual current
    registrations. Does not execute, grant source/session authority or disclose
    a result. Historical absent metadata fails closed. *)
val validate_disclosure
  :  current_capabilities:Chat_response.Tool_capability.t
  -> Agent_protocol.Delivery.t
  -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result
