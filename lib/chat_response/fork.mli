(** Drive the built-in [fork] tool over identity-bearing history.

    Child history remains isolated. {!execute_entries} returns all new
    assistant-message text, not an extracted [===PERSIST===] section. The
    section markers are a prompt convention only. Live callbacks may expose
    child progress independently of the completed parent tool output. *)

module Invocation_id : sig
  type t

  val create : unit -> t
  val to_string : t -> string
end

(** [allocator ~parent_namespace invocation_id] creates an isolated child
    allocator. The invocation identity is distinct from provider IDs and tool
    [call_id]. *)
val allocator : parent_namespace:string -> Invocation_id.t -> History_entry.Allocator.t

(** [execute_entries ~env ~allocator ~history ~invocation_id ~call_id
    ~arguments ~tools ~tool_tbl ~on_event ~on_fn_out ()] runs a child to
    completion over the supplied parent history.

    Parent entries retain their IDs. Use {!allocator} with a fresh
    [invocation_id] for child-created entries. [arguments] is the JSON command
    and argument list accepted by [Definitions.Fork.input_of_string].
    [tools] is forwarded to the child; [tool_tbl] contains invocation-aware
    runners. Recursive fork dispatch does not require a self-entry in that
    table.

    [on_event] receives raw response events. [on_sourced_event], when supplied,
    additionally receives events tagged with the fork invocation and parent
    call ID. [on_tool_execution] observes child tool activity. [on_fn_out]
    receives cumulative assistant-text progress under [call_id], as well as
    nested function-call outputs under their own call IDs. Progress is not a
    persisted child continuation.

    Returns all assistant-message text created after the initial child
    history, joining content parts with spaces and messages with newlines.
    No [===RESULT===] or [===PERSIST===] extraction occurs. Only the returned
    text becomes the completed parent tool output; child history is not merged.

    The call blocks its fiber and inherits the caller's Eio cancellation
    context. Cancellation stops local nested work; it does not guarantee that
    a remote provider stops generation or billing. Errors and cancellation
    may raise instead of returning a reply. Optional model parameters are
    forwarded to each nested request. *)
val execute_entries
  :  env:Eio_unix.Stdenv.base
  -> allocator:History_entry.Allocator.t
  -> history:History_entry.t list
  -> invocation_id:Invocation_id.t
  -> call_id:string
  -> arguments:string
  -> tools:Openai.Responses.Request.Tool.t list
  -> tool_tbl:(string, Ochat_function.runner) Base.Hashtbl.t
  -> on_event:(Openai.Responses.Response_stream.t -> unit)
  -> ?on_sourced_event:(Sourced_response_event.t -> unit)
  -> ?on_tool_execution:(Tool_execution_event.t -> unit)
  -> on_fn_out:(Openai.Responses.Function_call_output.t -> unit)
  -> ?temperature:float
  -> ?max_output_tokens:int
  -> ?reasoning:Openai.Responses.Request.Reasoning.t
  -> unit
  -> string

(** [history_entries ~allocator ~history ~arguments ~call_id] preserves the
    parent entries and appends one child-owned synthetic instruction entry. *)
val history_entries
  :  allocator:History_entry.Allocator.t
  -> history:History_entry.t list
  -> arguments:string
  -> call_id:string
  -> History_entry.t list
