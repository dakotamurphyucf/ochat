(** Opt-in bounded wait outside the actor, using the same scoped management
    service as read/send. Timeout never cancels or resumes the child. *)
val name : string

val registration : unit -> Chat_response.Agent_runtime.native_registration
