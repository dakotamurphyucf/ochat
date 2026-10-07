open! Core

(** Explicit selected synthetic ports for these offline constructor fixtures.
    Omitting the mock stream makes any actual dispatch fail, rather than silently
    returning an offline success. The namespace is fresh for each fixture owner. *)
type t = private
  { context : Inference_runtime.Context.t
  ; identity : Chat_response.Neutral_turn.Identity.t
  }

val create
  :  ?post_stream:Inference_fixture.post_stream
  -> config:Chat_response.Config.t
  -> unit
  -> t

(** Explicit offline host callbacks for an unobserved fixture runtime. Actual
    unexpected inference still fails through the configured synthetic stream. *)
val execution : t -> Inference_client.Execution.t

(** Selected synthetic auxiliary completion for fixtures exercising compaction
    ownership rather than summary quality. It returns a fixed finalized summary
    and retains the ordinary typed dispatch and observation boundary. *)
val compaction_execution : unit -> Inference_client.Execution.t

(** Install only the fixture's selected compaction execution. Any unexpected Turn
    dispatch fails instead of fabricating a foreground response. *)
val install_compaction_runtime
  :  Agent_session.Session_actor.t
  -> (unit, Agent_protocol.Error.t) Result.t
