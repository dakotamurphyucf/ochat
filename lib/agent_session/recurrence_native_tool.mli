(** Explicit selected native registrations. Registration grants no controller,
    target access, source loan, execution permission or automatic host grant. *)
val registration
  :  Recurrence_native_admission.Effect.t
  -> Chat_response.Agent_runtime.native_registration
