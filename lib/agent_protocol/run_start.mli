(** Explicit host-authorized run start; no caller-supplied source/run identity.
    The actor captures current compiled source and current authorization. *)
module Input : sig
  type t =
    | User_submission of Session.Message_content.t
    | Authored_start
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { session_id : Id.Session.t
  ; attachment_id : Id.Attachment.t
  ; generation : int
  ; expected_revision : int64
  ; mode : Run.Mode.t
  ; input : Input.t
  ; key : Idempotency_key.t
  }
[@@deriving sexp]

(** Both modes accept [Authored_start]; the actor still requires a genuine
    unconsumed source-bound startup. Single_turn does not recreate a consumed
    startup or authorize caller-supplied source identity. *)
val create
  :  session_id:Id.Session.t
  -> attachment_id:Id.Attachment.t
  -> generation:int
  -> expected_revision:int64
  -> mode:Run.Mode.t
  -> input:Input.t
  -> key:Idempotency_key.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
