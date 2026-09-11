(** Opt-in plaintext submission through the actual caller's scoped management
    service. Native dispatch persists one initial provider response. *)
val name : string

val registration : unit -> Chat_response.Agent_runtime.native_registration
