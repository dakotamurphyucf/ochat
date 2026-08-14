open! Core
module CM = Prompt.Chat_markdown
module Moderation = Moderation
module Runtime = Chatml_host_runtime
module Res = Openai.Responses

module Registry : sig
  type artifact
  type t

  val empty : t
  val artifact_count : t -> int

  (** [compile_script registry script] returns the cached compiled artifact for
      [script] or compiles it once and caches the result. *)
  val compile_script
    :  ?surface:Chatml.Chatml_builtin_surface.surface
    -> t
    -> CM.script
    -> (t * artifact, string) result

  (** [of_elements registry elements] compiles any moderator scripts declared
      by [elements]. *)
  val of_elements
    :  ?surface:Chatml.Chatml_builtin_surface.surface
    -> t
    -> CM.top_level_elements list
    -> (t * artifact option, string) result

  val script_id : artifact -> string
  val source_hash : artifact -> string
end

type t

type pending_ui_request = Runtime.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of
      { prompt : string
      ; choices : string array
      }

type subscription

(** [subscribe_committed_changes t ~on_wakeup] subscribes to changes committed
    after subscription. Each change is enqueued before [on_wakeup] runs.
    Wakeup exceptions are isolated from moderation commits. Subscriptions do
    not replay changes restored from snapshots. *)
val subscribe_committed_changes : t -> on_wakeup:(unit -> unit) -> subscription

(** [drain_committed_changes subscription] removes and returns pending changes
    in commit order. *)
val drain_committed_changes : subscription -> Moderation.Overlay_change.t list

(** [unsubscribe subscription] ends [subscription]. It is idempotent and drops
    pending changes. *)
val unsubscribe : subscription -> unit

(** [overlay_revision t] returns the installed identity-overlay revision. *)
val overlay_revision : t -> int

(** [create ~artifact ~capabilities ?snapshot ()] instantiates a fresh runtime
    session for [artifact], optionally restoring persisted durable state. *)
val create
  :  artifact:Registry.artifact
  -> capabilities:Moderation.Capabilities.t
  -> ?on_process_run:
       (Runtime.session
        -> command:string
        -> args:Chatml.Chatml_lang.value
        -> (string, string) result)
  -> ?snapshot:Session.Moderator_snapshot.t
  -> unit
  -> (t, string) result

val create_entries
  :  artifact:Registry.artifact
  -> capabilities:Moderation.Capabilities.t
  -> allocator:History_entry.Allocator.t
  -> ?on_process_run:
       (Runtime.session
        -> command:string
        -> args:Chatml.Chatml_lang.value
        -> (string, string) result)
  -> ?snapshot:Session.Moderator_state.Identity_snapshot.t
  -> unit
  -> (t, string) result

(** [uses_allocator t allocator] is [true] when [t] commits canonical
    moderator entries through [allocator]. *)
val uses_allocator : t -> History_entry.Allocator.t -> bool

(** [history_allocator t] returns the allocator used by entry-native
    moderation, if configured. *)
val history_allocator : t -> History_entry.Allocator.t option

(** [handle_event t ... event] projects the current context, invokes the
    moderator runtime, updates the durable overlay, and returns only the newly
    committed outcome for this host event. Calls that execute, resume, drain,
    enqueue, or snapshot the same manager are serialized. *)
val handle_event
  :  t
  -> session_id:string
  -> now_ms:int
  -> history:Res.Item.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> event:Moderation.Event.t
  -> (Moderation.Outcome.t, string) result

val handle_event_entries
  :  t
  -> session_id:string
  -> now_ms:int
  -> history:History_entry.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> event:Moderation.Event.t
  -> (Moderation.Outcome.t, string) result

(** [pending_ui_request t] exposes the current live-session approval request,
    if the runtime is suspended waiting for UI input. *)
val pending_ui_request : t -> pending_ui_request option

(** [resume_ui_request t ~response] resumes the suspended moderator execution
    with [response] and returns any newly committed moderation outcomes from
    that resumed execution. *)
val resume_ui_request : t -> response:string -> (Moderation.Outcome.t list, string) result

(** [drain_internal_events t ...] replays queued internal events FIFO through
    phase [internal_event], stopping after [max_events]. *)
val drain_internal_events
  :  ?max_events:int
  -> t
  -> session_id:string
  -> now_ms:int
  -> history:Res.Item.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> (Moderation.Outcome.t list, string) result

val drain_internal_events_entries
  :  ?max_events:int
  -> t
  -> session_id:string
  -> now_ms:int
  -> history:History_entry.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> (Moderation.Outcome.t list, string) result

val effective_entries : t -> History_entry.t list -> Moderation.Effective_entry.t list
val effective_history_entries : t -> History_entry.t list -> History_entry.t list

(** [effective_items t history] applies the durable moderator overlay to the
    projected canonical history. *)
val effective_items : t -> Res.Item.t list -> Moderation.Item.t list

(** [effective_history t history] applies the durable moderator overlay and
    reconstructs OpenAI response items for model input and downstream
    consumers. *)
val effective_history : t -> Res.Item.t list -> (Res.Item.t list, string) result

(** [snapshot t] extracts a persisted moderator snapshot after any active
    manager execution has completed. *)
val snapshot : t -> (Session.Moderator_snapshot.t, string) result

val identity_snapshot : t -> (Session.Moderator_state.Identity_snapshot.t, string) result

(** [enqueue_internal_event t event] enqueues [event] after any active manager
    execution has completed for later replay via {!drain_internal_events}. *)
val enqueue_internal_event : t -> Chatml.Chatml_lang.value -> (unit, string) result
