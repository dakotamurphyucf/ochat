(** Immutable metadata acceptance evidence. No receipt, including an enable
    receipt, grants current lifecycle, source, target or execution authority. *)
type kind =
  | Create
  | Edit
  | Enable
  | Pause
  | Retire
[@@deriving equal, sexp_of]

type t = private
  { principal_id : Agent_protocol.Id.Principal.t
  ; key : Agent_protocol.Idempotency_key.t
  ; request_sha256 : string
  ; recurrence_id : Agent_protocol.Id.Recurrence.t
  ; definition_revision : int64
  ; control_revision : int64
  ; catalog_revision : int64
  ; kind : kind
  ; source : Agent_protocol.Run_source.t option
  ; birth_lifecycle_revision : Agent_protocol.Session_lifecycle.Revision.t option
  ; accepted_at : Agent_protocol.Timestamp.t
  }
[@@deriving equal, sexp_of]

val create
  :  principal_id:Agent_protocol.Id.Principal.t
  -> key:Agent_protocol.Idempotency_key.t
  -> request_sha256:string
  -> recurrence_id:Agent_protocol.Id.Recurrence.t
  -> definition_revision:int64
  -> control_revision:int64
  -> catalog_revision:int64
  -> kind:kind
  -> source:Agent_protocol.Run_source.t option
  -> birth_lifecycle_revision:Agent_protocol.Session_lifecycle.Revision.t option
  -> accepted_at:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
