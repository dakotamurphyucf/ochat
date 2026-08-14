open! Core

(** Internal turn-driver implementation for moderated in-memory streams.

    The preferred public wrapper is {!Chat_response.Chatml_turn_driver}. For
    the canonical safe-point and effective-history semantics, see
    [docs-src/chatml-safe-point-and-effective-history.md].

    The Phase 2 bounded-turn contract that this module enforces in part is
    documented in [docs-src/chatml-budget-policy.md]. *)

module Safe_point_input : sig
  type t =
    { consume_entries : unit -> History_entry.t list
    ; consume_compatibility_text : unit -> string option
    }
end

type post_stream =
  sw:Eio.Switch.t
  -> inputs:Openai.Responses.Item.t list
  -> Openai.Responses.Response_stream.t Seq.t

(** Raised when an OpenAI stream emits no next event before its idle
    deadline. Each received event resets the deadline. *)
exception Openai_stream_idle_timeout of float

type moderator =
  { manager : Moderator_manager.t
  ; session_id : string
  ; session_meta : Jsonaf.t
  ; runtime_policy : Runtime_semantics.policy
  }

type pending_ui_request = Moderator_manager.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of
      { prompt : string
      ; choices : string array
      }

type moderated_tool_call =
  { call_item : Openai.Responses.Item.t
  ; kind : Tool_call.Kind.t
  ; name : string
  ; payload : string
  ; synthetic_result : Openai.Responses.Tool_output.Output.t option
  ; runtime_requests : Moderation.Runtime_request.t list
  }

val pending_ui_request : moderator -> pending_ui_request option

val resume_ui_request
  :  moderator
  -> response:string
  -> (Moderation.Outcome.t list, string) result

(** [prepare_turn_inputs ?safe_point_input ?moderator ~available_tools ~now_ms ~history]
    applies the explicit turn-start safe point before the next model call.

    The helper:
    {ol
    {- runs the moderator [turn_start] hook;}
    {- drains queued moderator internal events at the turn-start boundary;}
    {- computes effective request history through the moderator overlay; and}
    {- appends any compatibility-only safe-point text to the request after the
       turn-start boundary has decided the turn may proceed.}}

    [consume_entries] is used by the entry-native streaming loop after tool
    outputs complete; those entries are canonical and are sent on the next
    provider turn. [consume_compatibility_text] remains a request-only adapter
    for embedders using this raw helper.

    Without a moderator, [history] is forwarded unchanged unless
    [?safe_point_input] yields compatibility request text. *)
val prepare_turn_inputs
  :  moderator:moderator option
  -> ?safe_point_input:Safe_point_input.t
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> now_ms:int
  -> history:Openai.Responses.Item.t list
  -> unit
  -> (Openai.Responses.Item.t list, string) result

(** [finish_turn ?moderator ~available_tools ~now_ms ~history] applies the
    explicit end-of-turn safe point after a streamed turn completes.

    The helper runs the moderator [turn_end] hook, then drains queued
    moderator internal events before returning surfaced runtime requests.
    Without a moderator, it is a no-op returning [[]]. *)
val finish_turn
  :  moderator:moderator option
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> now_ms:int
  -> history:Openai.Responses.Item.t list
  -> (Moderation.Runtime_request.t list, string) result

(** [moderate_tool_call ...] applies the [pre_tool_call] moderation hook and
    returns the effective tool invocation together with any surfaced runtime
    requests. Rejected calls return a synthetic output payload instead of an
    executable invocation. *)
val moderate_tool_call
  :  moderator:moderator option
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> now_ms:int
  -> history:Openai.Responses.Item.t list
  -> kind:Tool_call.Kind.t
  -> name:string
  -> payload:string
  -> call_id:string
  -> item_id:string option
  -> (moderated_tool_call, string) result

(** [handle_tool_result ...] applies the post-tool safe point for [item].

    The helper runs [post_tool_response], emits [message_appended] for the
    canonical tool output when appropriate, drains queued moderator internal
    events at that safe point, and returns surfaced runtime requests. *)
val handle_tool_result
  :  moderator:moderator option
  -> available_tools:Openai.Responses.Request.Tool.t list
  -> now_ms:int
  -> history:Openai.Responses.Item.t list
  -> name:string
  -> kind:Tool_call.Kind.t
  -> item:Openai.Responses.Item.t
  -> (Moderation.Runtime_request.t list, string) result

(** [run_completion_stream_in_memory_entries ~env ~allocator ~history
    ~tools ()] streams a canonical conversation held entirely in memory.

    Compared to {!run_completion_stream} this helper:

    • Accepts an explicit identity-bearing [history] instead of reading a
      `.chatmd` file from disk.
    • Returns the *complete* history after all assistant turns and tool
      calls have been resolved.
    • Never touches the filesystem except for the persistent cache under
      `[~/.chatmd]`, making it suitable for unit-tests or server back-ends
      where direct file IO is undesirable.

    Optional callbacks mirror the streaming variant:

    • [?on_event] – invoked for each streaming event received from the
      OpenAI API (token deltas, item completions, …). Defaults to a no-op.
      Ordinary callback exceptions are suppressed.
    • [?on_sourced_event] – receives the same transient events with source
      attribution. It is isolated independently from [?on_event], and ordinary
      callback exceptions are suppressed.
    • [?on_fn_out] – executed after each tool call completes, allowing the
      caller to react to side-effects without waiting for the final assistant
      answer. This and [?on_tool_out] are canonical publication callbacks;
      their exceptions propagate.

    @param env      Standard Eio runtime environment.
    @param allocator Sole allocator for newly accepted canonical occurrences.
    @param history  Initial canonical conversation state.
    @param tools    Compile-time list of tool definitions visible to the
                    model.  Pass [[]] for none.
    @param tool_tbl Optional lookup table generated from [tools].  The
                    default builds a fresh table via
                    {!Ochat_function.functions} when omitted.
    @param temperature Temperature override forwarded the OpenAI request.
    @param max_output_tokens Hard cap on the number of tokens generated by
           the model per request.
    @param reasoning Optional reasoning settings forwarded to the API.
    @param moderator Optional session-scoped moderator runtime. When present,
           the driver applies [turn_start] before each model request,
           [message_appended] as canonical history items are produced,
           [pre_tool_call] and [post_tool_response] around tool execution,
           and [turn_end] after each streamed turn, using the moderator
           overlay to compute the effective request history. The Phase 2 budget
           contract for self-triggered turn limits and internal-event drain
           limits is documented in [docs-src/chatml-budget-policy.md].
    @param on_runtime_request Optional callback invoked for surfaced moderator
           runtime requests such as compaction or end-session notifications.

    @param history_compaction If [true], the function will compact the
           history so that multiple calls to the same file are replaced with a
           single call that points to the latest file content. Outputs for older calls are replaced with a
           place holder that points to the latest call output (stale) file content removed — see newer read_file output later

    @return The updated [history], i.e. the concatenation of the original
            [history] and every item produced during the streaming loop.

    @raise Any exception bubbled-up by the OpenAI client or user-supplied
           tool functions.  The function does **not** swallow errors. *)
val run_completion_stream_in_memory_entries
  :  env:Eio_unix.Stdenv.base
  -> ?datadir:Eio.Fs.dir_ty Eio.Path.t
  -> allocator:History_entry.Allocator.t
  -> history:History_entry.t list
  -> ?on_event:(Openai.Responses.Response_stream.t -> unit)
  -> ?on_sourced_event:(Sourced_response_event.t -> unit)
  -> ?on_history_event:(History_stream_event.t -> unit)
  -> ?on_fn_out:(Openai.Responses.Function_call_output.t -> unit)
  -> ?on_tool_out:(Openai.Responses.Item.t -> unit)
  -> ?on_history_tool_out:(History_entry.t -> unit)
  -> ?on_tool_execution:(Tool_execution_event.t -> unit)
  -> tools:Openai.Responses.Request.Tool.t list option
  -> ?tool_tbl:(string, Ochat_function.runner) Hashtbl.t
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?reasoning:Openai.Responses.Request.Reasoning.t
  -> ?moderator:moderator
  -> ?on_runtime_request:(Moderation.Runtime_request.t -> unit)
  -> ?history_compaction:bool
  -> ?parallel_tool_calls:bool
  -> ?meta_refine:bool
  -> ?safe_point_input:Safe_point_input.t
  -> ?model:Openai.Responses.Request.model
  -> ?prompt_cache_key:string
  -> ?prompt_cache_retention:string
  -> ?post_stream:post_stream
  -> ?source:string
  -> ?parent_call_id:string
  -> unit
  -> History_entry.t list
(** [run_completion_stream_in_memory_entries ~allocator ~history ...] uses
    [allocator] as the sole source of canonical IDs. Each identity-bearing
    stream callback is emitted only after its ID has been reserved, and the
    same ID appears in the returned history. Tool-call and tool-output entries
    remain distinct despite sharing a provider [call_id]. *)

module For_testing : sig
  (** [retry_stream_start ~sleep create_stream] retries parsing failures raised
      before the first event is available. Once an event is published, later
      failures propagate rather than replaying visible output. *)
  val retry_stream_start : sleep:(float -> unit) -> (unit -> 'a Seq.t) -> 'a Seq.t

  (** [with_stream_idle_timeout ~clock ~seconds events] times out each wait for
      the next event independently. *)
  val with_stream_idle_timeout
    :  clock:_ Eio.Time.clock
    -> seconds:float
    -> 'a Seq.t
    -> 'a Seq.t

  (** [notify_each observers value] invokes every observer despite ordinary
      observer failures. Eio cancellation propagates. *)
  val notify_each : ('a -> unit) list -> 'a -> unit

  (** [emit_tool_output] exposes canonical callback behavior for regression
      tests. Callback exceptions propagate. *)
  val emit_tool_output
    :  on_fn_out:(Openai.Responses.Function_call_output.t -> unit)
    -> on_tool_out:(Openai.Responses.Item.t -> unit)
    -> kind:[ `Function | `Custom ]
    -> call_id:string
    -> result:Openai.Responses.Tool_output.Output.t
    -> Openai.Responses.Item.t
end
