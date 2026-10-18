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

module Prepared_inputs : sig
  type t

  module Materialized : sig
    type t

    val allocator : t -> History_entry.Allocator.t
    val initial_history : t -> History_entry.t list

    (** Require the inference result to preserve this preparation's exact
        identity-bearing initial prefix, then return only new semantic
        assistant text. *)
    val completion_text : t -> History_entry.t list -> string
  end

  (** Select the actual Parsed/document projection, child prompt directory and
      Config overrides while preserving the supplied Context's inference,
      submission and observation hooks. A foreign document projection rejects.
      No source is parsed or fetched. *)
  val select_prepared
    :  ctx:Eio_unix.Stdenv.base Ctx.t
    -> prompt_dir:Eio.Fs.dir_ty Eio.Path.t
    -> parsed:Prompt.Chat_markdown.Parsed.t
    -> ?document_projection:Prompt.Chat_markdown.Document_projection.t
    -> input:Prompt.Chat_markdown.content_item list
    -> unit
    -> t

  (** Select the same parsed elements, Config, input and prompt directory as
      [select_prepared], retaining the Context already selected and detached by
      the entered Model recipe. Preserve all its inference, submission and
      observation hooks without applying Config or deriving Context again.
      This pure preparation grants no authority: the host independently admits
      the authentic revision and replaces the comparison Context with the
      Root-returned Context before materialization. *)
  val select_prepared_model
    :  ctx:Eio_unix.Stdenv.base Ctx.t
    -> prompt_dir:Eio.Fs.dir_ty Eio.Path.t
    -> parsed:Prompt.Chat_markdown.Parsed.t
    -> ?document_projection:Prompt.Chat_markdown.Document_projection.t
    -> input:Prompt.Chat_markdown.content_item list
    -> unit
    -> t

  val context : t -> Eio_unix.Stdenv.base Ctx.t
  val elements : t -> Prompt.Chat_markdown.top_level_elements list
  val config : t -> Prompt.Chat_markdown.config

  (** Copy the selected preparation with a Root-admitted inference Context.
      Preserve its exact elements, input, selected Config, prompt directory,
      relation, identity, observations, recipe and all inherited dispatch,
      attempt and submission guards. This pure projection grants no inference
      authority; the trusted host obtains [inference_context] from its Root's
      lexical selected-child admission. Do not apply Config again. *)
  val with_inference_context : t -> inference_context:Inference_runtime.Context.t -> t

  (** Install the trusted selected Model recipe on an already selected child
      Context without applying its Config overrides a second time. This is a
      Context copy, not a source or execution grant. *)
  val with_model_recipe : t -> Moderation.Capabilities.model_recipe -> t

  (** Ordinary effectful Converter projection and canonical history identity.
      Prompt and caller content use distinct optional source runners. Omission
      preserves the existing dynamic content behavior; the saved developer
      child caller first admits literal-only AST/input and supplies a
      fail-closed nested-agent callback. This function is data preparation,
      not source/runtime or tool authority. *)
  val materialize
    :  t
    -> session_id:string
    -> ?source:string
    -> ?agent_source_runner:Tool.Agent_source_runner.t
    -> ?input_agent_source_runner:Tool.Agent_source_runner.t
    -> run_agent:
         (?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
          -> ?session_id:string
          -> ctx:Eio_unix.Stdenv.base Ctx.t
          -> string
          -> Prompt.Chat_markdown.content_item list
          -> string)
    -> unit
    -> Materialized.t

  (** Trusted upper callers supply two already authenticated projectors. They
      run in declaration-then-invocation order; an error prevents the second
      projection and history allocation. This function grants no source or
      execution authority and never selects the ambient Converter fallback. *)
  val materialize_with_projectors
    :  t
    -> session_id:string
    -> ?source:string
    -> project_declaration:
         (ctx:Eio_unix.Stdenv.base Ctx.t
          -> Prompt.Chat_markdown.top_level_elements list
          -> (Openai.Responses.Item.t list, Agent_protocol.Error.t) result)
    -> project_invocation:
         (ctx:Eio_unix.Stdenv.base Ctx.t
          -> Prompt.Chat_markdown.top_level_elements list
          -> (Openai.Responses.Item.t list, Agent_protocol.Error.t) result)
    -> unit
    -> (Materialized.t, Agent_protocol.Error.t) result
end

module Selected_moderator_start : sig
  type t =
    | No_moderator
    | Ready of In_memory_stream.moderator
    | Halted
    | Paused
    | End_session_requested of string
end

(** Replace only the named Model recipe in an existing moderator capability
    table. This is the same pure projection used by ordinary nested Agent
    execution; it grants no source, provider or tool authority. Other
    callbacks, including Tool.call/Process.run policy, retain their supplied
    behavior. *)
val capabilities_with_model_recipe
  :  recipe:Moderation.Capabilities.model_recipe
  -> Moderation.Capabilities.t
  -> Moderation.Capabilities.t

(** Construct the selected child's moderator from the same entry-based
    Session_start path used by ordinary Driver. It exposes initial End_session
    before a foreground provider turn and rejects unsupported mutable runtime
    requests instead of logging them as fulfilled. It handles already queued
    startup events one at a time through one manager drain bounded by the
    selected runtime policy cap, stopping on End_session. [Ready] means the
    moderator is runnable; if this drain reaches its cap, queued remainder
    stays for the next foreground safe point. The cap is per drain: foreground
    safe points and completion use their own existing bounds.
    [Pause_internal_event_drains] returns [Paused] before consuming a queued
    startup event. Startup does not wait
    for pending local jobs that may depend on the foreground turn. The
    selected runner registers [Ready] with its Model executor and uses the
    opt-in Model_completion_drain after foreground, before the enclosing
    Child_prepared callback returns. [event_handlers] is [None]; no
    transactional host handler is bypassed. All local jobs must be settled or
    cancelled and joined within that active callback. *)
val create_selected_moderator_entries
  :  env:Eio_unix.Stdenv.base
  -> session_id:string
  -> artifact:Moderator_manager.Registry.artifact option
  -> allocator:History_entry.Allocator.t
  -> history:History_entry.t list
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> capabilities:Moderation.Capabilities.t
  -> runtime_policy:Runtime_semantics.policy
  -> ?on_process_run:
       (Chatml_host_runtime.session
        -> command:string
        -> args:Chatml.Chatml_lang.value
        -> (string, string) result)
  -> unit
  -> (Selected_moderator_start.t, string) result

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

(** Run already admitted identity-bearing history through the same streaming
    inference loop used by script-bearing nested agents. The caller owns the
    selected [ctx], allocator, initial history, tool descriptors/dispatch and
    any actual moderator. [moderator = None] still uses the streaming path.
    The result is the full history with its original prefix; failures and
    cancellation propagate as in the ordinary nested-agent path.

    This entry does not construct Agent_runtime/native resources, convert
    ChatMarkdown content, Fetch sources, recursively run an Agent, or construct
    Model_executor. Supplied values are trusted composition data, not effect
    authority. The caller keeps their source/runtime/moderator owner live
    through this call. Inference target, identity, submission and observation
    hooks come from [ctx], with no implicit target re-selection.
    [on_runtime_request] forwards the lower stream's surfaced moderator
    requests in order, including foreground End_session. Omission preserves
    the existing lower default; the selected child supplies a strict handler
    and checks it again before follow-up work. [on_committed_entry] observes
    canonically accepted in-memory entries before the corresponding moderator
    request is surfaced. It is not a durable Store commit receipt. Tool outputs
    may reach both lower append callbacks; consumers deduplicate by
    History_entry.Id. It grants no write. *)
val run_stream_entries
  :  ctx:Eio_unix.Stdenv.base Ctx.t
  -> allocator:History_entry.Allocator.t
  -> history:History_entry.t list
  -> tools:Openai.Responses.Request.Tool.t list
  -> tool_tbl:(string, Ochat_function.runner) Base.Hashtbl.t
  -> moderator:In_memory_stream.moderator option
  -> history_compaction:bool
  -> response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?on_runtime_request:(Moderation.Runtime_request.t -> unit)
  -> ?on_committed_entry:(History_entry.t -> unit)
  -> ?observer:agent_observer
  -> ?source:string
  -> ?parent_call_id:string
  -> unit
  -> History_entry.t list

(** Semantic assistant text from identity-bearing history, shared by the
    ordinary nested-agent result and hosts consuming [run_stream_entries].
    Opaque reasoning, calls and results are never lowered into text. *)
val assistant_text : History_entry.t list -> string

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
