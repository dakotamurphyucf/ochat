(** Explicit standard native registrations. Registration alone grants no source,
    selection, current actor, or provider-context authority. *)
val registration
  :  Instruction_native_admission.Effect.t
  -> Chat_response.Agent_runtime.native_registration
