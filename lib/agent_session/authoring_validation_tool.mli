(** Explicit, readonly native helper registration. The supplied host identifies
    the registered implementation. Execution uses the actual caller's host from
    its expiring Script_tool_calls services, including a child's delegated surface.
    Invocation borrows supply the actual capability ceiling; the model cannot provide either identity. This
    installs no corpus, context guidance or general feature flag by itself. *)
val name : string

val registration
  :  env:Eio_unix.Stdenv.base
  -> host:Chat_response.Authoring_validation.host
  -> Chat_response.Agent_runtime.native_registration
