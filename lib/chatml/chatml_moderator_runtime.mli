(** Host-side session runtime for ChatML moderator scripts.

    This module sits above the minimal ChatML core and provides the
    session-oriented runtime described in the moderator design:

    - scripts are compiled once and instantiated per session,
    - [on_event] returns {!Chatml_lang.task} values,
    - the host interprets those tasks via an operation registry,
    - local transactional effects are buffered and committed on success.

    The runtime is intentionally generic: embedders provide concrete
    handlers for tool/model/scheduling integration while the module
    supplies the common execution model. *)

open Chatml.Chatml_lang

module Builtin_surface = Chatml.Chatml_builtin_surface

(** Compiled script artifact produced by {!compile_script}. *)
type compiled_script

(** Per-session runtime instance holding durable state and execution
    queues. *)
type session

(** Diagnostic log level used by the default [Log.*] operation family. *)
type log_level =
  | Debug
  | Info
  | Warn
  | Error_level

(** Structured local turn mutations used by the default [Turn.*]
    handlers. *)
type turn_effect =
  | Prepend_system of string
  | Append_message of value
  | Replace_message of string * value
  | Delete_message of string
  | Halt of string

(** Structured tool-moderation actions used by the default [Tool.*]
    moderation handlers. *)
type tool_moderation =
  | Approve
  | Reject of string
  | Rewrite_args of value
  | Redirect of string * value

(** Runtime classification of task operations. *)
type op_kind =
  | Local_transactional
  | External_sync
  | External_async
  | Diagnostic

(** Definition of a host operation exposed to ChatML tasks. *)
type op_def =
  { name : string
  ; kind : op_kind
  ; perform : session -> value list -> (value, string) result
  ; phase_check : string -> (unit, string) result
  }

(** Runtime configuration shared across sessions for a given host/runtime
    embedding. *)
type runtime_config =
  { surface : Builtin_surface.surface
  ; operations : op_def list
  }

(** Names of the convention-based script entrypoints. *)
type compiled_entrypoints =
  { initial_state_name : string
  ; on_event_name : string
  }

(** Host callbacks used to build the standard moderator operation
    registry.  Handlers that are left at their defaults either act as
    no-ops for local/diagnostic behavior or return a clear ["... is not
    configured"] error for external integrations. *)
type default_handlers =
  { on_log : session -> level:log_level -> message:string -> (unit, string) result
  ; on_turn_effect : session -> turn_effect -> (unit, string) result
  ; on_tool_moderation : session -> tool_moderation -> (unit, string) result
  ; on_tool_call : session -> name:string -> args:value -> (value, string) result
  ; on_tool_spawn : session -> name:string -> args:value -> (string, string) result
  ; on_model_call : session -> recipe:string -> payload:value -> (value, string) result
  ; on_model_spawn : session -> recipe:string -> payload:value -> (string, string) result
  ; on_schedule_after_ms
      : session
      -> delay_ms:int
      -> payload:value
      -> (string, string) result
  ; on_schedule_cancel : session -> id:string -> (unit, string) result
  ; on_request_compaction : session -> (unit, string) result
  ; on_end_session : session -> reason:string -> (unit, string) result
  }

(** Render a log level using the names expected by human-facing
    diagnostics. *)
val string_of_log_level : log_level -> string

(** Phase check that accepts every phase. *)
val allow_all_phases : string -> (unit, string) result

(** Build a phase check that accepts only the listed phase names. *)
val require_phases : string list -> string -> (unit, string) result

(** Default handler bundle used by {!default_operations}. *)
val default_handlers : default_handlers

(** Construct the standard operation registry for moderator runtimes from a
    bundle of host callbacks. *)
val default_operations : ?handlers:default_handlers -> unit -> op_def list

(** Convenience constructor for a runtime configuration using the standard
    moderator surface and default operation registry. *)
val default_runtime_config
  :  ?surface:Builtin_surface.surface
  -> ?handlers:default_handlers
  -> unit
  -> runtime_config

(** Parse, typecheck, and resolve a script once.  The default surface is
    {!Builtin_surface.moderator_surface}. *)
val compile_script
  :  ?surface:Builtin_surface.surface
  -> source:string
  -> unit
  -> (compiled_script, string) result

(** Instantiate a compiled script in a fresh per-session environment and
    load the configured entrypoints. *)
val instantiate_session
  :  runtime_config
  -> compiled_script
  -> entrypoints:compiled_entrypoints
  -> (session, string) result

(** Current durable script state for the session. *)
val current_state : session -> value

(** Current phase while a handler is actively running, if any. *)
val current_phase : session -> string option

(** Local transactional effects buffered during the current handler
    execution. *)
val pending_local_effects : session -> eff list

(** All committed local transactional effects observed so far for the
    session, in execution order. *)
val committed_local_effects : session -> eff list

(** Buffered internal events currently queued for later delivery. *)
val queued_events : session -> value list

(** Whether the session has been ended by a committed runtime action. *)
val is_halted : session -> bool

(** Append an internal event to the current handler's transactional output
    buffer.  This is primarily useful for custom host operations. *)
val emit_internal_event : session -> value -> (unit, string) result

(** Request session termination from within the current handler's
    transactional context. *)
val request_session_end : session -> reason:string -> (unit, string) result

(** Handle one event:

    - invokes [on_event],
    - interprets the returned task,
    - commits buffered state/effects on success,
    - or rolls back local transactional buffers on failure. *)
val handle_event
  :  session
  -> context:value
  -> event:value
  -> (unit, string) result
