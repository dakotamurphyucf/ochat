open! Core

(** Public moderator-manager boundary for embedders.

    This module exposes the durable moderator runtime in terms of the
    stabilized {!Chatml_moderation} vocabulary. It is the place to:
    - compile and cache moderator scripts,
    - instantiate or restore a moderator session,
    - obtain effective item/history projections,
    - snapshot durable state, and
    - enqueue internal events for later replay.

    For the canonical safe-point and effective-history semantics, see
    [docs-src/chatml-safe-point-and-effective-history.md].

    For the async completion and wakeup lifecycle that reinjects background
    work as queued internal events, see
    [docs-src/chatml-async-completion-lifecycle.md].

    For the optional UI-only notification and approval capability layer that
    sits above this durable boundary in interactive hosts, see
    [docs-src/chatml-ui-host-capabilities.md]. That layer adds a host-visible
    pending approval / resume boundary for live sessions without extending the
    durable moderator snapshot schema. *)

module Moderation = Chatml_moderation
module Res = Openai.Responses

module Registry : sig
  type artifact = Moderator_manager.Registry.artifact
  type t = Moderator_manager.Registry.t

  val empty : t
  val artifact_count : t -> int

  (** [compile_script registry script] returns the cached compiled artifact for
      [script] or compiles it once and caches the result. *)
  val compile_script : t -> Prompt.Chat_markdown.script -> (t * artifact, string) result

  (** [of_elements registry elements] compiles any moderator scripts declared
      by [elements]. *)
  val of_elements
    :  t
    -> Prompt.Chat_markdown.top_level_elements list
    -> (t * artifact option, string) result

  val script_id : artifact -> string
  val source_hash : artifact -> string
end

type t = Moderator_manager.t

type pending_ui_request = Moderator_manager.pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of { prompt : string; choices : string array }

(** [create ~artifact ~capabilities ?snapshot ()] instantiates a moderator
    session backed by the current runtime, optionally restoring
    [Session.Moderator_snapshot.t]. The [capabilities] argument uses the public
    {!Chatml_moderation.Capabilities} surface. *)
val create
  :  artifact:Registry.artifact
  -> capabilities:Moderation.Capabilities.t
  -> ?snapshot:Session.Moderator_snapshot.t
  -> unit
  -> (t, string) result

(** [handle_event t ... event] projects the current context, runs the moderator,
    updates the durable overlay, and returns the newly committed
    {!Chatml_moderation.Outcome.t} for this host event. *)
val handle_event
  :  t
  -> session_id:string
  -> now_ms:int
  -> history:Res.Item.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> event:Moderation.Event.t
  -> (Moderation.Outcome.t, string) result

(** [pending_ui_request t] exposes the current live-session approval prompt, if
    the moderator is suspended waiting for UI input. *)
val pending_ui_request : t -> pending_ui_request option

(** [resume_ui_request t ~response] resumes the suspended moderator execution
    with [response] and returns any newly committed outcomes from that resumed
    execution. *)
val resume_ui_request
  :  t
  -> response:string
  -> (Moderation.Outcome.t list, string) result

(** [drain_internal_events t ...] replays queued internal events FIFO through
    phase [internal_event], returning one {!Chatml_moderation.Outcome.t} per
    replayed event.

    This is the public replay boundary used after async work has already been
    reinjected into the moderator queue. Host wakeups and idle-drain timing
    stay outside this module. *)
val drain_internal_events
  :  ?max_events:int
  -> t
  -> session_id:string
  -> now_ms:int
  -> history:Res.Item.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> (Moderation.Outcome.t list, string) result

(** [effective_items t history] applies the durable moderator overlay to the
    canonical projected history and returns {!Chatml_moderation.Item.t} values. *)
val effective_items : t -> Res.Item.t list -> Moderation.Item.t list

(** [effective_history t history] applies the durable moderator overlay and
    reconstructs OpenAI response items for model input and downstream
    consumers. *)
val effective_history : t -> Res.Item.t list -> (Res.Item.t list, string) result

(** [snapshot t] extracts the persisted moderator snapshot for the current
    runtime session without changing the persistence format. *)
val snapshot : t -> (Session.Moderator_snapshot.t, string) result

(** [enqueue_internal_event t event] enqueues [event] for later replay via
    {!drain_internal_events}.

    Async producers such as `Model.spawn` completions use this queue boundary
    instead of mutating moderator state directly. *)
val enqueue_internal_event : t -> Chatml.Chatml_lang.value -> (unit, string) result
