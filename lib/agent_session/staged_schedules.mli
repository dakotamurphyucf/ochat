(** Ordered actor-local schedule mutations. The actor validates moderator
    ownership, source, generation, quotas and subscription linkage. *)
type limits =
  { max_active : int
  ; max_per_source : int
  ; max_retained : int
  ; max_delay_ms : int
  ; max_payload_bytes : int
  ; max_payload_depth : int
  }

val default_limits : limits
val validate_limits : limits -> (unit, Agent_protocol.Error.t) result

type t

val create : unit -> t
val is_empty : t -> bool
val reservations : t -> int

val find
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Schedule.t
  -> Agent_protocol.Schedule.t option

val stage
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> previous:Agent_protocol.Schedule.t option
  -> next:Agent_protocol.Schedule.t
  -> (int, Agent_protocol.Error.t) result

val select
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipts:int list
  -> lookup:(Agent_protocol.Id.Schedule.t -> Agent_protocol.Schedule.t option)
  -> (unit, Agent_protocol.Error.t) result

val selected
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> lookup:(Agent_protocol.Id.Schedule.t -> Agent_protocol.Schedule.t option)
  -> (Agent_protocol.Schedule.t list, Agent_protocol.Error.t) result

val abort
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipt:int
  -> (unit, Agent_protocol.Error.t) result

val release_owner : t -> owner:Agent_protocol.Job.launch_owner -> unit
val values : t -> Agent_protocol.Schedule.t list
val abort_all : t -> unit
