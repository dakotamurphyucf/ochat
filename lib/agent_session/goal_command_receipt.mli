(** Immutable metadata acceptance evidence, never execution authority. A safe
    control has no source claim. Historical receipts survive source/reset changes.
    No full mutable goal snapshot is duplicated in the receipt. *)
type t = private
  { principal_id : Agent_protocol.Id.Principal.t
  ; key : Agent_protocol.Idempotency_key.t
  ; request_sha256 : string
  ; goal_id : Agent_protocol.Id.Goal.t
  ; goal_revision : int64
  ; catalog_revision : int64
  ; source : Agent_protocol.Run_source.t option
  ; accepted_at : Agent_protocol.Timestamp.t
  }
[@@deriving equal, sexp_of]

val create
  :  principal_id:Agent_protocol.Id.Principal.t
  -> key:Agent_protocol.Idempotency_key.t
  -> request_sha256:string
  -> goal_id:Agent_protocol.Id.Goal.t
  -> goal_revision:int64
  -> catalog_revision:int64
  -> source:Agent_protocol.Run_source.t option
  -> accepted_at:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
