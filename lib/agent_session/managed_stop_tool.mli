(** Opt-in managed stop using an atomic durable retry receipt. Does not grant
    approval authority or delete persisted child data. *)
val name : string

val registration : unit -> Chat_response.Agent_runtime.native_registration
