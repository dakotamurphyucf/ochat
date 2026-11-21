(** Scoped root-request configuration. This port is absent from nested/admitted
    executions. The owner pins one Context for the whole callback, including
    preparation/dispatch, and must release preparing ownership on every exit.
    [on_prepared] is called on the final preparation before allocating an
    inference attempt. *)
type t =
  { with_context :
      'a.
      previous:Inference_runtime.Context.t
      -> history:History_entry.t list
      -> (Inference_runtime.Context.t
          -> select_prepared:Inference_client.Prepared_selection.select option
          -> on_prepared:(Inference_runtime.Prepared.t -> unit)
          -> on_dispatch:(Inference.Observation.Configuration.t -> unit)
          -> 'a)
      -> 'a
  }

(** Exact first root admission of a fresh window. Preview entries remain
    transient until the selected attempt crosses the guarded submission cut. *)
module Successor : sig
  type t =
    { pending : History_entry.t list
    ; optional_hint : Agent_guidance.t option
    ; check_prepared : optional_hint:bool -> Inference_runtime.Prepared.t -> unit
    ; guard : Inference_runtime.Attempt.t -> unit -> unit
    }

  exception Omit_optional_hint
end
