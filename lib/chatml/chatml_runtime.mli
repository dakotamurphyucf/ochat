open! Core

module Lang = Chatml.Chatml_lang
module Builtin_surface = Chatml.Chatml_builtin_surface

type compiled_script = Chatml_moderator_runtime.compiled_script
type session = Chatml_moderator_runtime.session

type log_level = Chatml_moderator_runtime.log_level =
  | Debug
  | Info
  | Warn
  | Error_level

type turn_effect = Chatml_moderator_runtime.turn_effect =
  | Prepend_system of string
  | Append_message of Lang.value
  | Replace_message of string * Lang.value
  | Delete_message of string
  | Halt of string

type tool_moderation = Chatml_moderator_runtime.tool_moderation =
  | Approve
  | Reject of string
  | Rewrite_args of Lang.value
  | Redirect of string * Lang.value

type local_effect = Chatml_moderator_runtime.local_effect =
  | Turn_effect of turn_effect
  | Tool_moderation_effect of tool_moderation
  | Ui_notification of string
  | Emit_internal_event of Lang.value
  | Request_compaction
  | Request_turn
  | End_session of string

type op_kind = Chatml_moderator_runtime.op_kind =
  | Local_transactional
  | External_sync
  | External_async
  | Diagnostic

type op_def = Chatml_moderator_runtime.op_def =
  { name : string
  ; kind : op_kind
  ; perform : session -> Lang.value list -> (Lang.value, string) result
  ; phase_check : string -> (unit, string) result
  }

type runtime_config = Chatml_moderator_runtime.runtime_config =
  { surface : Builtin_surface.surface
  ; operations : op_def list
  }

type compiled_entrypoints = Chatml_moderator_runtime.compiled_entrypoints =
  { initial_state_name : string
  ; on_event_name : string
  }

type default_handlers = Chatml_moderator_runtime.default_handlers =
  { on_log : session -> level:log_level -> message:string -> (unit, string) result
  ; on_turn_effect : session -> turn_effect -> (unit, string) result
  ; on_tool_moderation : session -> tool_moderation -> (unit, string) result
  ; on_ui_notify : session -> message:string -> (unit, string) result
  ; on_tool_call : session -> name:string -> args:Lang.value -> (Lang.value, string) result
  ; on_tool_spawn : session -> name:string -> args:Lang.value -> (string, string) result
  ; on_model_call :
      session -> recipe:string -> payload:Lang.value -> (Lang.value, string) result
  ; on_model_spawn :
      session -> recipe:string -> payload:Lang.value -> (string, string) result
  ; on_process_run :
      session -> command:string -> args:Lang.value -> (string, string) result
  ; on_schedule_after_ms :
      session -> delay_ms:int -> payload:Lang.value -> (string, string) result
  ; on_schedule_cancel : session -> id:string -> (unit, string) result
  ; on_request_compaction : session -> (unit, string) result
  ; on_end_session : session -> reason:string -> (unit, string) result
  ; on_request_turn : session -> (unit, string) result
  }

val string_of_log_level : log_level -> string
val allow_all_phases : string -> (unit, string) result
val require_phases : string list -> string -> (unit, string) result
val default_handlers : default_handlers
val default_operations : ?handlers:default_handlers -> unit -> op_def list

val default_runtime_config
  :  ?surface:Builtin_surface.surface
  -> ?handlers:default_handlers
  -> unit
  -> runtime_config

val compile_script
  :  ?surface:Builtin_surface.surface
  -> source:string
  -> unit
  -> (compiled_script, string) result

val compiled_surface : compiled_script -> Builtin_surface.surface

val instantiate_session
  :  runtime_config
  -> compiled_script
  -> entrypoints:compiled_entrypoints
  -> (session, string) result

val current_state : session -> Lang.value
val current_phase : session -> string option
val committed_local_effects : session -> Lang.eff list
val decode_local_effect : Lang.eff -> (local_effect, string) result
val decode_local_effects : Lang.eff list -> (local_effect list, string) result
val queued_events : session -> Lang.value list
val take_queued_event : session -> Lang.value option

val restore
  :  session
  -> state:Lang.value
  -> queued_events:Lang.value list
  -> halted:bool
  -> (unit, string) result

val is_halted : session -> bool
val emit_internal_event : session -> Lang.value -> (unit, string) result
val request_session_end : session -> reason:string -> (unit, string) result

val handle_event
  :  session
  -> context:Lang.value
  -> event:Lang.value
  -> (unit, string) result

val enqueue_internal_event : session -> Lang.value -> (unit, string) result
