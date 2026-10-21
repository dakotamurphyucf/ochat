(** Durable REPL Task receipt codec, sharing source locations with submission presentation. This projection grants no execution authority. *)

module Limits = Developer_repl_wire_common.Limits
module Submission = Developer_repl_wire_result.Submission

module Task_view : sig
  module Diagnostic : sig
    type t

    val create
      :  limits:Limits.t
      -> code:string
      -> message:string
      -> location:Submission.Location.t option
      -> (t, Error.t) result

    val code : t -> string
    val message : t -> string
    val location : t -> Submission.Location.t option
  end

  type outcome =
    | Succeeded of string option
    | Failed of
        { error : Error.t
        ; diagnostic : Diagnostic.t option
        }
    | Limited of
        { error : Error.t
        ; diagnostic : Diagnostic.t option
        }
    | Interrupted
    | Cancelled

  type phase =
    | Reserved
    | Waiting_permission
    | Execution_started
    | Terminal of
        { execution_started : bool
        ; outcome : outcome
        }

  (** This is only an authenticated Repl_task_run projection. The ordinary
      Developer_common.View decoder deliberately excludes Task rows. *)
  type t

  val create
    :  limits:Limits.t
    -> command_id:Developer_command_id.t
    -> owner:Developer_common.Owner.t
    -> key:Idempotency_key.t
    -> phase:phase
    -> request_fingerprint:Chatml_source.Digest.t
    -> capture_fingerprint:Chatml_source.Digest.t
    -> (t, Error.t) result

  val command_id : t -> Developer_command_id.t
  val owner : t -> Developer_common.Owner.t
  val key : t -> Idempotency_key.t
  val phase : t -> phase
  val request_fingerprint : t -> Chatml_source.Digest.t
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val to_json : t -> Jsonaf.t
  val of_json : limits:Limits.t -> Jsonaf.t -> (t, Error.t) result
end
