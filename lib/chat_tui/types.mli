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

    @canonical Chat_tui.Types *)

(** Role of a chat message.  Expected values are the strings mandated by
    the OpenAI API: ["system"], ["user"], ["assistant"], ["function"].
    The module does not validate the value. *)
type role = string

(** One chat message represented as [(role, content)]. *)
type message = role * string

(** Streaming-time buffer.  While we receive deltas from the OpenAI API we
    accumulate partial output in [text] and remember the target index into
    the [messages] list so the UI can update incrementally. *)
type msg_buffer =
  { text : string ref (** Mutable accumulator for the streaming text. *)
  ; index : int
    (** Index into {!Chat_tui.Model.messages} that will
                           hold the final message once streaming completes. *)
  }

type cmd =
  | Persist_session of (unit -> unit)
  (** Persist the current conversation by running [f] in a separate fibre. *)
  | Start_streaming of (unit -> unit)
  (** Kick off an OpenAI streaming request by executing the thunk. *)
  | Cancel_streaming of (unit -> unit) (** Abort the in-flight streaming request. *)

(* These constructors describe {e pure} modifications to the immutable
   [Model.t] record that will be executed by [Model.apply].  They are
   defined here – as an extension of the open variant – so that they can be
   shared between multiple compilation units without introducing circular
   dependencies.  *)

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
  (** Insert the user's prompt [text] into the chat history. *)
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
        when using models that do not yet support the feature. *)
  }

(** [default_settings ()] returns the default {!settings} record with
    [parallel_tool_calls] set to [true]. *)
val default_settings : unit -> settings
