(** Actor-local elapsed-time anchors. These values are never persisted: recovery
    derives a new remaining duration from absolute wall deadlines, then active
    work follows the host's monotonic clock. Retention includes staged records so
    saving or rolling back a transaction cannot reset another timer's duration. *)
module Key : sig
  type t =
    | Schedule of Agent_protocol.Id.Schedule.t
    | Subscription of Agent_protocol.Id.Subscription.t
  [@@deriving compare, equal, hash, sexp]
end

type t

val create : unit -> t

(** Capture an already validated relative delay at actual creation, before a
    handler can suspend or wait for persistence. *)
val capture : t -> Key.t -> now:Mtime.t -> delay_ms:int -> unit

(** Keep existing anchors, establish absent anchors from the current wall/elapsed
    clock pair, and discard work absent from durable and staged ownership. *)
val reconcile
  :  t
  -> retained:(Key.t * Agent_protocol.Timestamp.t) list
  -> wall_now:Agent_protocol.Timestamp.t
  -> monotonic_now:Mtime.t
  -> unit

val is_due : t -> Key.t -> now:Mtime.t -> (bool, Agent_protocol.Error.t) result
