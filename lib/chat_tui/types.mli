(** Shared data types for the Ochat terminal UI.

    This module centralises the few fundamental types that every
    sub-module of {!Chat_tui} needs.  Moving them into their own
    compilation unit avoids cyclic dependencies between the renderer,
    controller and model layers.

    {1 Groups}

    • {b Chat transcript} – {!role} and {!message} model the OpenAI chat
      schema.

    • {b Streaming support} – {!msg_buffer} accumulates partial assistant
      output while the HTTP connection is still open.

    • {b Elm-style orchestration} – controllers emit {!cmd} values to
      request impure work (persistence, network calls, cancellation).  The
      pure part of the app applies {!patch} records to transform the
      immutable {!Chat_tui.Model.t}.  This mirrors the
      {i Model-View-Update} architecture.

    • {b Runtime settings} – {!settings} toggles features that influence how
      tools run at execution time.

    @canonical Chat_tui.Types *)

(** Role of a chat message.  Expected values are the strings mandated by
    the OpenAI API: ["system"], ["user"], ["assistant"], ["function"].
    The module does not validate the value. *)
type role = string

(** One chat message represented as [(role, content)]. *)
type message = role * string

(** Classification of a tool-output message.

    This metadata is derived {b only} in the TUI layer and is not
    persisted in the core chat history. It allows the renderer to
    choose specialised layouts or syntax-highlighting modes for
    particular built-in tools without baking those concerns into the
    runtime.

    The constructors are intentionally coarse so that additional
    per-tool detail can be layered on later without disturbing call
    sites. *)
type tool_output_kind =
  | Apply_patch
  | Read_file of { path : string option }
  | Read_directory of { path : string option }
  | Other of { name : string option }

(** Streaming-time buffer.  While we receive deltas from the OpenAI API we
    accumulate partial output in [buf] and remember the target index [index]
    into {!Chat_tui.Model.messages} so the UI can update incrementally. *)
type msg_buffer =
  { buf : Buffer.t (** Mutable accumulator for the streaming text. *)
  ; index : int
    (** Zero-based index into {!Chat_tui.Model.messages} where the fully
        assembled assistant message will be stored once streaming
        finishes. *)
  }

(** Commands produced by the pure controller and executed by a side-effecting
    runner.

    Values of this type carry thunks that start or stop IO-heavy operations
    such as persistence and streaming without forcing the controller itself
    to perform those effects. *)
type cmd =
  | Persist_session of (unit -> unit)
  (** Persist the current conversation by running [f] in a separate fibre. *)
  | Start_streaming of (unit -> unit)
  (** Kick off an OpenAI streaming request by executing [f]. *)
  | Cancel_streaming of (unit -> unit)
  (** Abort the in-flight streaming request by running [f]. *)

(** High-level, mostly pure updates to {!Chat_tui.Model.t}.

    Each constructor describes an abstract change to the UI state that is
    applied by {!Chat_tui.Model.apply_patch}.  Today patches are executed by
    mutating the model in place; a future refactor is expected to rebuild an
    immutable record instead.

    Defining the type here keeps it shared between the controller, model,
    and streaming code without introducing circular dependencies. *)
type patch =
  | Ensure_buffer of
      { id : string
      ; role : string
      }
  (** Ensure the buffer [id] exists, creating an empty entry with the
      supplied [role] and adding a placeholder message when necessary. *)
  | Append_text of
      { id : string
      ; role : string
      ; text : string
      }
  (** Append [text] to the buffer [id], allocating it on first use and
      updating the corresponding entry in {!Chat_tui.Model.messages}. *)
  | Set_function_name of
      { id : string
      ; name : string
      } (** Record the function [name] associated with streaming buffer [id]. *)
  | Set_function_output of
      { id : string
      ; output : string
      } (** Store the JSON [output] returned by the function call for [id]. *)
  | Update_reasoning_idx of
      { id : string
      ; idx : int
      } (** Update the last-seen reasoning summary index for buffer [id]. *)
  | Add_user_message of { text : string }
  (** Append the user's prompt [text] to {!Chat_tui.Model.messages}.

      The patch currently affects only the renderable transcript; callers
      that maintain a separate canonical history should update it via
      {!Chat_tui.Model.add_history_item}. *)
  | Add_placeholder_message of
      { role : string
      ; text : string
      }
  (** Append the transient placeholder [(role, text)].  The message is
      rendered only in the UI and is {b not} persisted. *)

(** User-togglable settings that influence runtime behaviour.  The record is
    intentionally minimal – future flags can extend it without breaking
    backwards compatibility by adding new optional fields. *)
type settings =
  { parallel_tool_calls : bool
    (** When [true] the assistant may request multiple tool calls in a single
        turn.  Each call will be executed concurrently by the runtime.  Set to
        [false] to fall back to sequential execution – useful for debugging or
        when using models that do not yet support the feature.  The default
        from {!default_settings} is [true]. *)
  }

(** [default_settings ()] returns the default {!settings} record.

    The helper exists to future-proof call-sites: new fields can be added
    to {!settings} later without breaking existing code.  Users are
    encouraged to start from the default and tweak only the flags they
    care about.

    Example – disable parallel tool calls while keeping other defaults:
    {[
      let cfg = default_settings () in
      { cfg with parallel_tool_calls = false }
    ]} *)
val default_settings : unit -> settings
