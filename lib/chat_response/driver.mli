(** Selected inference and canonical transcript entry points. Every model call,
    including generated/nested agents and initializer helpers, delegates the same
    In_memory_stream tool owner with explicit Context and tracking ports.
    Captured data is never decoded into legacy DTO history. Strict observation
    failures/cancellation propagate; provider terminals do not commit host turns. *)

open! Core

type agent_observer = Agent_response_loop.observer =
  { on_event : Transcript.Stream.t -> unit
  ; on_tool_execution : Tool_execution_event.t -> unit
  }

(** Child authored Config overrides are merged once into the selected target.
    Omitted fields inherit account/profile/endpoint/model/settings and actual
    parent relation; no backend or credential resolution occurs here. *)
val run_agent
  :  ?history_compaction:bool
  -> ?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?session_id:string
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?observer:agent_observer
  -> ?source:string
  -> ?parent_call_id:string
  -> ?shell_manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> ?shell_approval_provider:Shell_runtime.Approval_broker.provider
  -> ctx:Eio_unix.Stdenv.base Ctx.t
  -> string
  -> Prompt.Chat_markdown.content_item list
  -> string

(** Execute a host-prepared parser result through the same nested-agent loop.
    Does not parse, preprocess or acquire the script's sources again. The host
    restores [programs] against this exact [parsed] and this invocation's actual
    headless contracts before calling; normal runtime capability checks still
    precede initialization.
    A supplied literal document projection must belong to this exact [parsed].
    [agent_source_runner] owns the child prompt's tool/content declarations;
    [input_agent_source_runner] owns only caller-authored invocation content items.
    The two conversion batches remain separate. Omitted input runner preserves
    existing dynamic-item behavior; it never inherits the child's runner.
    [startup] borrows the host's current preparation context and requires its
    original [compilation_limits]. Forwarded [checkpoint] failures/cancellation
    propagate through the existing startup owner. No context is retained after
    preparation. Child invocations never inherit this invocation's program inventory.
    [prompt_dir] is the host-selected execution directory, not source authority.
    Expected startup failures use the same exception behavior as [run_agent]. *)
val run_prepared_agent
  :  Prompt.Chat_markdown.Parsed.t
  -> ?agent_source_runner:Tool.Agent_source_runner.t
  -> ?input_agent_source_runner:Tool.Agent_source_runner.t
  -> ?startup:Startup_compilation.t
  -> ?compilation_limits:Chatml_compilation.limits
  -> ?checkpoint:(unit -> unit)
  -> ?programs:Chatml_prompt_programs.Restored.t
  -> ?document_projection:Prompt.Chat_markdown.Document_projection.t
  -> ?history_compaction:bool
  -> ?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?session_id:string
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?observer:agent_observer
  -> ?source:string
  -> ?parent_call_id:string
  -> ?shell_manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> ?shell_approval_provider:Shell_runtime.Approval_broker.provider
  -> ctx:Eio_unix.Stdenv.base Ctx.t
  -> Prompt.Chat_markdown.content_item list
  -> string

(** Generation options are explicit overrides; omitted model/settings inherit.
    Supplied legacy provider post/post_stream callbacks reject before effects. *)
val run_entries
  :  ctx:Eio_unix.Stdenv.base Ctx.t
  -> allocator:History_entry.Allocator.t
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?tools:Openai.Responses.Request.Tool.t list
  -> ?reasoning:Openai.Responses.Request.Reasoning.t
  -> ?history_compaction:bool
  -> ?response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?observer:agent_observer
  -> ?source:string
  -> ?parent_call_id:string
  -> ?post:Response_loop.post
  -> ?post_stream:Agent_response_loop.post_stream
  -> ?model:Openai.Responses.Request.model
  -> tool_tbl:(string, Ochat_function.runner) Base.Hashtbl.t
  -> History_entry.t list
  -> History_entry.t list

val run_completion
  :  env:Eio_unix.Stdenv.base
  -> inference_context:Inference_runtime.Context.t
  -> inference_identity:Neutral_turn.Identity.t
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> ?on_inference_observation:(Inference.Observation.t -> unit)
  -> ?prompt_file:string
  -> ?parallel_tool_calls:bool
  -> ?meta_refine:bool
  -> output_file:string
  -> unit
  -> unit

val run_completion_stream
  :  env:Eio_unix.Stdenv.base
  -> inference_context:Inference_runtime.Context.t
  -> inference_identity:Neutral_turn.Identity.t
  -> on_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> on_inference_completion:(Inference_client.Completion.t -> unit)
  -> ?on_inference_observation:(Inference.Observation.t -> unit)
  -> ?prompt_file:string
  -> ?on_transcript_event:(Transcript.Stream.t -> unit)
  -> ?on_history_tool_out:(History_entry.t -> unit)
  -> ?post_stream:In_memory_stream.post_stream
  -> ?on_final_history:(History_entry.t list -> unit)
  -> ?parallel_tool_calls:bool
  -> ?meta_refine:bool
  -> ?history_compaction:bool
  -> ?shell_manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> ?shell_approval_provider:Shell_runtime.Approval_broker.provider
  -> output_file:string
  -> unit
  -> unit
