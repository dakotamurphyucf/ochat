open! Core

module Prepared : sig
  type t = private
    { guidance : Inference.Request.Guidance.t option
    ; guard : Inference_runtime.Submission_guard.t
    ; reserved_input_tokens : int
    }

  (** Positive local text-token estimate for the framed text and delimiters. Empty
      guidance requires zero reservation; present text requires positive tokens. *)
  val create
    :  guidance:Inference.Request.Guidance.t option
    -> guard:Inference_runtime.Submission_guard.t
    -> reserved_input_tokens:int
    -> (t, Agent_protocol.Error.t) result
end

(** Explicit agent-conversation owner composition. The owner keeps its genuine
    source Use and physical execution borrow OPEN for the complete callback,
    including provider preparation/admission/run. No ambient proof or history
    mutation; independent authored Model recipes must clear this port. *)
type t = { with_guidance : 'a. (Prepared.t -> 'a) -> ('a, Agent_protocol.Error.t) result }

module Scope : sig
  type nonrec t =
    | Eligible of t option
    | Excluded
end

(** Project once into the request-only slot, revalidating the complete request
    and composing the inherited guard. Existing request guidance rejects rather
    than silently replacing or duplicating it. None preserves ordinary behavior. *)
val with_request
  :  t option
  -> request:Inference.Request.t
  -> submission_guard:Inference_runtime.Submission_guard.t
  -> limits:Document_schema.Limits.t
  -> (Inference.Request.t
      -> guard:Inference_runtime.Submission_guard.t
      -> reserved_input_tokens:int
      -> 'a)
  -> ('a, Agent_protocol.Error.t) result
