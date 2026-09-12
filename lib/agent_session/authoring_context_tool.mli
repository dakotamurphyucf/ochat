(** Explicit readonly documentation tool using the actual invocation's host and
    selected capabilities. No automatic context insertion or helper grants.
    Inherited bindings retain their corpus but query using the child's scope. *)
val name : string

val registration
  :  host:Chat_response.Authoring_validation.host
  -> (Chat_response.Agent_runtime.native_registration, string) result
