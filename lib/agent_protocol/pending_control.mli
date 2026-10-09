(** Controls for one still-pending occurrence. The authenticated submitting
    principal is captured by the host, never supplied in request parameters.
    Current writer/transcript authority and persisted submitting ownership must
    both permit the mutation. These requests grant neither provider interruption
    nor canonical-history deletion authority. *)
module Cancel_request : sig
  type t = private
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    ; expected_pending_revision : Pending_input.Revision.t
    ; history_id : History.Id.t
    ; expected_content_revision : History.Content_revision.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val create
    :  session_id:Id.Session.t
    -> attachment_id:Id.Attachment.t
    -> expected_generation:int
    -> expected_pending_revision:Pending_input.Revision.t
    -> history_id:History.Id.t
    -> expected_content_revision:History.Content_revision.t
    -> idempotency_key:Idempotency_key.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Replace_request : sig
  type t = private
    { target : Cancel_request.t
    ; text : string
    }
  [@@deriving sexp]

  (** Validates bounded UTF-8 text using the shared canonical edit contract.
      Empty text is valid. Host admission separately rejects non-plain targets. *)
  val create : target:Cancel_request.t -> text:string -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Result : sig
  type t =
    { pending_revision : Pending_input.Revision.t
    ; outcome : Pending_query.Outcome.t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  (** An adopted winner returns its actual public canonical occurrence; it is
      never cancelled or edited by this result. Unknown/expired outcomes remain
      unavailable and confer no retry permission. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) result
end
