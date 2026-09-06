(** Builds immutable management-page data from the live registry and durable
    session state without performing I/O. *)

val create
  :  agent_runtime:Chat_response.Agent_runtime.t
  -> session:Session.t option
  -> Model.Shell_security_page_state.snapshot
