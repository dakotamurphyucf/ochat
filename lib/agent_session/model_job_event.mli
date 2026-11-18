(** Pure terminal Model completion projection. Payloads grant neither delivery
    authority nor a continuation; the Actor separately validates the actual Job,
    source, attempt, original custody and atomic queue checkpoint. *)
module Format : sig
  type t =
    | Legacy
    | Extensibility_v1
  [@@deriving equal, sexp_of]

  (** Select formatting DATA from the actual compiled Manager. Persisted
      checkpoint identity and a Job's source do not identify the script API. *)
  val of_manager : Chat_response.Moderator_manager.t -> t
end

(** Existing Model_job_succeeded/Model_job_failed variants, byte-compatible with
    the original runtime projection. This is the compatibility default. *)
val legacy
  :  Agent_protocol.Job.t
  -> (Chatml.Chatml_lang.value, Agent_protocol.Error.t) result

(** [Extensibility_v1] requires an actual terminal Model_call value with its
    recipe and successful result present. It renders an existing bounded
    Internal_event JSON payload with kind=model_job_completed, job_id, decimal
    attempt, recipe and checked Stored_completion result. The existing 1MiB
    event bound is unchanged: oversized/invalid content returns a typed error
    without truncation. [format] selects representation only, never authority. *)
val render
  :  ?format:Format.t
  -> Agent_protocol.Job.t
  -> (Chatml.Chatml_lang.value, Agent_protocol.Error.t) result
