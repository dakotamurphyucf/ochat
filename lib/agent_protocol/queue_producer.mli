open! Core

(** Bounded data correlations, never execution or continuation authority.
    Job completion attempt zero records a truthful terminal disposition before
    the first claim, such as queued cancellation. Final carrier validation still
    requires the exact retained terminal Delivered Job and same attempt; this
    data creates no claim, execution or Run obligation.

    [Invocation_at_source] names an actual resolved invocation without deferred
    observation intent and the full installation at its emitted checkpoint.
    The carrier must retain that exact receipt and source; this correlation
    neither manufactures observation work nor lends its original admission.
    Observer-backed invocations keep the existing [Invocation] representation.

    [Ingress_delivery] correlates one exact retained registration receipt and
    frame. Source and original inference attribution come from that registration;
    this DATA correlation supplies no submission or callback permission. *)
type t =
  | Event of Id.Moderator_execution.t
  | Invocation of Id.Invocation.t
  | Invocation_at_source of
      { id : Id.Invocation.t
      ; source : Run_source.t
      }
  | Job_completion of
      { id : Id.Job.t
      ; generation : int
      ; attempt : int
      }
  | Schedule_delivery of
      { id : Id.Schedule.t
      ; generation : int
      ; delivery_count : int
      }
  | Ingress_delivery of
      { registration_id : Id.Capability.t
      ; event_id : Id.Ingress_event.t
      }
  | Source_setup of { basis_revision : int64 }
[@@deriving equal, sexp]

val validate : t -> (unit, Protocol_error.t) result
val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, Protocol_error.t) result
val shape : Document_schema.Shape.t
