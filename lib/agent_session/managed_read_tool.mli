(** Opt-in, actual-caller output read through the shared management service.
    Cursors are scoped positions and do not grant session access. *)
val name : string

val registration : unit -> Chat_response.Agent_runtime.native_registration
