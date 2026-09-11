(** Explicit opt-in status registration. Inherited implementations use the actual
    invoking session's service and expiring scope, not their original owner. *)
val status_name : string

val status_registration : unit -> Chat_response.Agent_runtime.native_registration
