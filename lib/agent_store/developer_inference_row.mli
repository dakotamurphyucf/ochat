(** Pure version-one inference row codec for the single developer-run document.
    Neither a row nor its transcript scope grants Store, worker or provider
    authority. The enclosing document validates its job/epoch/parent graph. *)
open! Core

type t

val ordinal : t -> int64
val owner : t -> Agent_protocol.Execution_owner.developer
val capture_fingerprint : t -> Chatml_source.Digest.t
val job_id : t -> Agent_protocol.Id.Job.t
val job_attempt : t -> int
val context_id : t -> Inference.Observation.Observation_id.t
val record : t -> Inference.Observation.Attempt_record.t
val encoded_bytes : t -> int

(** Stable non-Session source from validated host/run/epoch/job coordinates;
    attempt ID is the persisted ordinal. The relation is retained verbatim for
    enclosing-document graph validation, not accepted as physical proof. *)
val scope
  :  owner:Agent_protocol.Execution_owner.developer
  -> job_id:Agent_protocol.Id.Job.t
  -> ordinal:int64
  -> relation:Transcript.Scope.relation
  -> (Transcript.Scope.t, Store_error.t) result

val create
  :  ordinal:int64
  -> owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> job_id:Agent_protocol.Id.Job.t
  -> job_attempt:int
  -> context_id:Inference.Observation.Observation_id.t
  -> record:Inference.Observation.Attempt_record.t
  -> limits:Document_schema.Limits.t
  -> (t, Store_error.t) result

val with_record
  :  t
  -> Inference.Observation.Attempt_record.t
  -> limits:Document_schema.Limits.t
  -> (t, Store_error.t) result

val same_identity : t -> t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> limits:Document_schema.Limits.t -> (t, Store_error.t) result
