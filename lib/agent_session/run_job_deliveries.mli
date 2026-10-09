(** Bounded private terminal-occurrence retention. Historical absence, explicit
    null and present empty arrays are distinct and survive unrelated writes.
    Keys are exact run/job/generation/attempt identities; entries never vanish. *)
type t

val empty : t
val entries : t -> Run_job_delivery.t list
val find : t -> Run_job_delivery.Key.t -> Run_job_delivery.t option
val add : t -> Run_job_delivery.t -> (t, Agent_protocol.Error.t) result
val replace : t -> Run_job_delivery.t -> (t, Agent_protocol.Error.t) result

val retire_run
  :  t
  -> run_id:Agent_protocol.Id.Run.t
  -> reason:Run_job_delivery.Retirement.t
  -> t

(** Match only an exact retained Enqueued private frame. Multiple owners conflict;
    Pending, Claimed and Retired cannot acquire a fresh callback. *)
val enqueued_frame
  :  t
  -> frame:Chat_response.Background_delivery.t
  -> (Run_job_delivery.t option, Agent_protocol.Error.t) result

(** Bytes reserved for monotone enqueue/claim/retirement metadata growth;
    whole-index admission subtracts this from its document budget. *)
val reserved_bytes : t -> int

val fields : t -> (string * Jsonaf.t) list
val of_field : Jsonaf.t option -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
val validate_transition : previous:t -> t -> (unit, Agent_protocol.Error.t) result
