open! Core

module CM = Prompt.Chat_markdown
module Moderation = Moderation
module Runtime = Chatml_moderator_runtime
module Res = Openai.Responses

module Registry : sig
  type artifact
  type t

  val empty : t
  val artifact_count : t -> int

  (** [compile_script registry script] returns the cached compiled artifact for
      [script] or compiles it once and caches the result. *)
  val compile_script : t -> CM.script -> (t * artifact, string) result

  (** [of_elements registry elements] compiles any moderator scripts declared
      by [elements]. *)
  val of_elements : t -> CM.top_level_elements list -> (t * artifact option, string) result

  val script_id : artifact -> string
  val source_hash : artifact -> string
end

type t

(** [create ~artifact ~capabilities ?snapshot ()] instantiates a fresh runtime
    session for [artifact], optionally restoring persisted durable state. *)
val create
  :  artifact:Registry.artifact
  -> capabilities:Moderation.Capabilities.t
  -> ?snapshot:Session.Moderator_snapshot.t
  -> unit
  -> (t, string) result

(** [handle_event t ... event] projects the current context, invokes the
    moderator runtime, updates the durable overlay, and returns only the newly
    committed outcome for this host event. *)
val handle_event
  :  t
  -> session_id:string
  -> now_ms:int
  -> history:Res.Item.t list
  -> available_tools:Res.Request.Tool.t list
  -> session_meta:Jsonaf.t
  -> event:Moderation.Event.t
  -> (Moderation.Outcome.t, string) result

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

(** [effective_messages t history] applies the durable moderator overlay to the
    projected canonical history. *)
val effective_messages : t -> Res.Item.t list -> Moderation.Message.t list

(** [snapshot t] extracts the persisted moderator snapshot for the current
    runtime session. *)
val snapshot : t -> (Session.Moderator_snapshot.t, string) result
