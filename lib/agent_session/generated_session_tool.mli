(** Internal opt-in native creator. Uses the invoking session's expiring services
    and capability borrow; a registration closure cannot select the parent.
    Invocation_v1 dispatch owns result persistence/provider publication. General
    authoring exposure requires A01, independently from this registration. *)
val name : string

val registration : unit -> Chat_response.Agent_runtime.native_registration
