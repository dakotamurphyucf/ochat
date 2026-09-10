open Core

type host =
  { create :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> delay_ms:int
      -> payload:Jsonaf.t
      -> misfire:Agent_protocol.Schedule.misfire
      -> (int * Agent_protocol.Schedule.t, Agent_protocol.Error.t) result
  ; stage :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> previous:Agent_protocol.Schedule.t option
      -> next:Agent_protocol.Schedule.t
      -> (int, Agent_protocol.Error.t) result
  ; get :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> Agent_protocol.Id.Schedule.t
      -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result
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

val with_scope
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> error:(string -> 'error)
  -> (scope -> ('a, 'error) result)
  -> ('a, 'error) result

val moderator_transaction : scope -> Chat_response.Schedule_operations.transaction

(** Coordinator operations for subscription arming/terminal cleanup. The caller
    retains each returned receipt and rolls it back on failure. No authority is
    gained beyond this same lexical moderator scope. *)
val get
  :  scope
  -> Agent_protocol.Id.Schedule.t
  -> (Agent_protocol.Schedule.t, string) result

val stage
  :  scope
  -> previous:Agent_protocol.Schedule.t option
  -> next:Agent_protocol.Schedule.t
  -> (int, string) result

val cancel : scope -> Agent_protocol.Id.Schedule.t -> (int, string) result
val rollback : scope -> int -> unit

(** Set exactly the schedule receipts retained by surviving subscription effects.
    Called during subscription preparation before schedule preparation. Explicit
    schedule effects and these dependencies must be distinct, owned receipts. *)
val retain_dependencies : scope -> int list -> (unit, string) result
