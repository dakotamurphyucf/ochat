open! Core

module Change : sig
  type t =
    | Replace_project of Instruction_capture.t
    | Replace_skills of Instruction_capture.t
    | Remove_project
    | Remove_skills of Instruction_content.Id.t list
    | Clear
end

(** Pure request identity, never authenticated authority. The real host supplies
    principal, source admission and session mutation permission separately. *)
type t

val create
  :  session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> expected_revision:int64
  -> key:Agent_protocol.Idempotency_key.t
  -> change:Change.t
  -> (t, Agent_protocol.Error.t) result

val session_id : t -> Agent_protocol.Id.Session.t
val generation : t -> int
val expected_revision : t -> int64
val key : t -> Agent_protocol.Idempotency_key.t
val change : t -> Change.t
val digest : t -> string

type request = t

module Receipt : sig
  type t = private
    { principal_id : Agent_protocol.Id.Principal.t
    ; key : Agent_protocol.Idempotency_key.t
    ; request_sha256 : string
    ; session_id : Agent_protocol.Id.Session.t
    ; generation : int
    ; revision : int64
    ; accepted_at : Agent_protocol.Timestamp.t
    }
  [@@deriving equal, sexp_of]

  val create
    :  principal_id:Agent_protocol.Id.Principal.t
    -> request:request
    -> revision:int64
    -> accepted_at:Agent_protocol.Timestamp.t
    -> (t, Agent_protocol.Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val identity : t -> string
  val shape : Document_schema.Shape.t
end
