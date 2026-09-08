(** Internal bridge from streamed model calls to prepared moderator tools. Public
    feature qualification and standalone execution remain separate services.
    This adapter never executes a raw runner for a known
    extension declaration. *)
exception Dispatch_error of Agent_protocol.Error.t

(** [admit] must verify the live selected capability/revision and execution
    authority; it runs inside both actor and manager ownership, immediately
    before the handler, followed by the stream's final-target authorizer.
    [prepare_outcome] enforces host disclosure and output limits before recording
    an outcome; rejecting it rolls back moderator state and produces a bounded
    [invocation.disclosure_rejected] failure. Host failure codes identify the
    failing stage independently of script diagnostic text. Successful outcomes
    pass through unchanged.
    Pending work must have a qualified, owned completion path. Error details from
    host exceptions are not copied into model-visible output. Root actor ownership
    cannot be reused for transient fork calls. Unknown native names return [None].
    Rejected moderator calls record/publish a bounded terminal failure with the
    existing snapshot; no invocation handler or execution authorizer is run.
    The service's [validate_original] checks known prepared input schemas and the
    implementing script's array/depth/byte projection limits without running
    scripts or policy callbacks. Stream callers run it before pre-tool
    moderation. Unknown targets pass through for other services to validate;
    final-target validation still runs after any redirect or rewrite.
    [script_tools] supplies the scoped native Tool.call bridge during Tool_invoked.
    Omitting it retains existing host callbacks. The caller must provide the
    bridge's current policy/disclosure and deferred-observation services. *)
val create
  :  ?script_tools:Script_tool_calls.t
  -> definition:Chat_response.Extension_compiler.definition
  -> manager:Chat_response.Moderator_manager.t
  -> input:Operation_worker.Input.t
  -> capabilities:Operation_worker.Capabilities.t
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> validate_work:(Agent_protocol.Invocation.work -> (unit, string) result)
  -> admit:(Chat_response.In_memory_stream.Tool_dispatch.request -> (unit, string) result)
  -> prepare_outcome:(Agent_protocol.Invocation.outcome -> (unit, string) result)
  -> unit
  -> Chat_response.In_memory_stream.Tool_dispatch.t
