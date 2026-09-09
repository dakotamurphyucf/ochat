(** Explicit, readonly native helper registration. The supplied host fixes the
    target runtime/surfaces and compiler ceilings. Invocation borrows supply the
    actual capability ceiling; the model cannot provide either identity. This
    installs no corpus, context guidance or general feature flag by itself. *)
val name : string

val registration
  :  env:Eio_unix.Stdenv.base
  -> host:Chat_response.Authoring_validation.host
  -> Chat_response.Agent_runtime.native_registration
