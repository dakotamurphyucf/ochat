(** Canonical neutral usage projection shared by retained and cumulative views.
    Explicit Usage wins over absent-state classification. Context, configuration
    and diagnostics never become consumption. No observations are retained. *)
val count
  :  Inference.Observation.Attempt_record.t
  -> Inference.Observation.Usage.Component.t
  -> Inference.Observation.Count.view
