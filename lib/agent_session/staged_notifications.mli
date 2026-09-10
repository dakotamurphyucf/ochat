(** Ordered pending notification intents. Actor admission owns authority,
    correlation, result validation and shared durable/staged capacity. *)
type t

type limits =
  { max_pending : int
  ; max_per_source : int
  ; max_retained : int
  ; max_payload_bytes : int
  ; max_payload_depth : int
  }

val default_limits : limits
val validate_limits : limits -> (unit, Agent_protocol.Error.t) result
val create : unit -> t
val is_empty : t -> bool
val values : t -> Agent_protocol.Delivery.t list

val find
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> id:Agent_protocol.Id.Delivery.t
  -> Agent_protocol.Delivery.t option

val stage
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> previous:Agent_protocol.Delivery.t option
  -> next:Agent_protocol.Delivery.t
  -> (int, Agent_protocol.Error.t) result

val select
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipts:int list
  -> lookup:(Agent_protocol.Id.Delivery.t -> Agent_protocol.Delivery.t option)
  -> (unit, Agent_protocol.Error.t) result

val selected
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> lookup:(Agent_protocol.Id.Delivery.t -> Agent_protocol.Delivery.t option)
  -> (Agent_protocol.Delivery.t list, Agent_protocol.Error.t) result

val abort
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> receipt:int
  -> (unit, Agent_protocol.Error.t) result

val release_owner : t -> owner:Agent_protocol.Job.launch_owner -> unit
val abort_all : t -> unit
