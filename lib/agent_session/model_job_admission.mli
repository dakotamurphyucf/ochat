(** Actor-issued admission capability for a fresh model Job. It retains the
    actual physical parent and rechecks source, original authority and policy
    before atomic Job/Run persistence. Stored IDs cannot reconstruct it. *)
type t

val admit
  :  t
  -> Agent_protocol.Job.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

module Host : sig
  (** Host composition only. The callback must route through its captured actual
      owner; constructing this wrapper does not validate or grant authority. *)
  val create
    :  admit:
         (Agent_protocol.Job.t -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result)
    -> t
end

(** Pure fresh-DTO validation and classification chosen by the real actor owner.
    No persistence or execution authority is granted. The actual admission must
    reject existing IDs and validate the complete Job/Run/source-bound carrier
    under its configured limits before committing. Caller-supplied origin or
    launch data rejects rather than impersonating an owner. Never and Safe_retry
    are supported at initial admission; Safe_retry requires max_attempts >= 1
    and backoff_ms >= 0, both at most Int.max_value. Idempotent rejects. Retry
    policy is immutable after this birth. Existing ChatML Model.call/Model.spawn
    continue to choose Never; this host port adds no new syntax or defaults. *)
val prepare
  :  Agent_protocol.Job.t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> origin:Agent_protocol.Job.Inference_origin.t
  -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result
