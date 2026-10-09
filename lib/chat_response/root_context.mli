(** Scoped root-request configuration. This port is absent from nested/admitted
    executions. The owner pins one Context for the whole callback, including
    preparation/dispatch, and must release preparing ownership on every exit. *)
type t =
  { with_context :
      'a.
      previous:Inference_runtime.Context.t
      -> history:History_entry.t list
      -> (Inference_runtime.Context.t
          -> on_dispatch:(Inference.Observation.Configuration.t -> unit)
          -> 'a)
      -> 'a
  }
