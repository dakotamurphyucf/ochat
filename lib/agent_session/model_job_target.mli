open! Core

(** Private immutable routing evidence for one actual durable model job. This
    value is not part of the public Job payload and grants no execution authority.
    The owning state validates the actual job kind, ID and generation. *)
type t [@@deriving sexp]

(** New admission requires a Model_call job and a captured target. The complete
    binding is bounded before the job and binding are committed atomically. *)
val create
  :  Agent_protocol.Job.t
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (t, Agent_protocol.Error.t) Result.t

val job_id : t -> Agent_protocol.Id.Job.t
val generation : t -> int
val source : t -> Inference.Selection.t
val execution : t -> Inference.Selection.t

(** Legacy unresolved binding may capture once, preserving unknown selection members. The state carrier owns
    unknown binding members. Existing complete target repetition is idempotent. *)
val capture_source
  :  t
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (t, Agent_protocol.Error.t) Result.t

(** Capture the root recipe's effective selection after its prompt config is
    known and before any moderator/model effect. A retry can repeat only the
    complete same target. Nested child overrides are independent captures. *)
val capture_recipe
  :  t
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> (t, Agent_protocol.Error.t) Result.t

(** Whole original JSON admission precedes projection. The enclosing state or
    delta carrier owns unknown binding members through [shape]; Selection owns
    its complete subtree. This is not a standalone durable document codec. *)
val of_json
  :  Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> (t, Agent_protocol.Error.t) Result.t

val to_json : t -> Jsonaf.t
val shape : Document_schema.Shape.t
