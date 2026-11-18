open! Core

(** Internal admission data. Neither these values nor their digest grant authority.
    The public writer request and its canonical digest remain unchanged. *)
module Caller : sig
  type t =
    | Writer of Agent_protocol.Id.Attachment.t
    | Headless_command
    | Native_goal_start of Agent_protocol.Goal_native_start_request.t
  [@@deriving equal, sexp_of]
end

module Headless : sig
  type t = private
    { session_id : Agent_protocol.Id.Session.t
    ; generation : int
    ; expected_revision : int64
    ; mode : Agent_protocol.Run.Mode.t
    ; input : Agent_protocol.Run_start.Input.t
    ; key : Agent_protocol.Idempotency_key.t
    }

  (** Validated command intent, with no attachment or caller-selected source/Run.
      Both inputs still require genuine current actor/runtime admission. *)
  val create
    :  session_id:Agent_protocol.Id.Session.t
    -> generation:int
    -> expected_revision:int64
    -> mode:Agent_protocol.Run.Mode.t
    -> input:Agent_protocol.Run_start.Input.t
    -> key:Agent_protocol.Idempotency_key.t
    -> (t, Agent_protocol.Error.t) result
end

type t = private
  { session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; expected_revision : int64
  ; mode : Agent_protocol.Run.Mode.t
  ; input : Agent_protocol.Run_start.Input.t
  ; key : Agent_protocol.Idempotency_key.t
  ; caller : Caller.t
  }

val of_writer : Agent_protocol.Run_start.t -> t
val of_headless : Headless.t -> t

(** Internal native DATA normalization. The actual Actor captures this revision
    once after source reservation and durable claim. Native command digest never
    includes this private preparation CAS. This function issues no custody. *)
val of_native_goal_start
  :  session_id:Agent_protocol.Id.Session.t
  -> expected_revision:int64
  -> Agent_protocol.Goal_native_start_request.t
  -> (t, Agent_protocol.Error.t) result

(** Writer requests use their exact original wire encoding. Headless commands
    use a disjoint canonical envelope; neither path includes executable authority. *)
val request_sha256 : t -> (string, Agent_protocol.Error.t) result
