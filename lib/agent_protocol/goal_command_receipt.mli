(** Immutable metadata acceptance evidence, never execution authority. A safe
    control has no source claim. Historical receipts survive source/reset changes.
    No full mutable goal snapshot is duplicated in the receipt. *)
type t = private
  { principal_id : Id.Principal.t
  ; key : Idempotency_key.t
  ; request_sha256 : string
  ; goal_id : Id.Goal.t
  ; goal_revision : int64
  ; catalog_revision : int64
  ; source : Run_source.t option
  ; accepted_at : Timestamp.t
  }
[@@deriving equal, sexp_of]

val create
  :  principal_id:Id.Principal.t
  -> key:Idempotency_key.t
  -> request_sha256:string
  -> goal_id:Id.Goal.t
  -> goal_revision:int64
  -> catalog_revision:int64
  -> source:Run_source.t option
  -> accepted_at:Timestamp.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
