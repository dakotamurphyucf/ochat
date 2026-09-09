open Core

type response =
  { outcome : Agent_protocol.Invocation.outcome
  ; script_invocation : Agent_protocol.Invocation.t option
    (** Resolved child record when returned by the executor. A timeout or host
        failure can leave an actor-owned child record even when this is None. *)
  ; runtime_requests : Chat_response.Moderation.Runtime_request.t list
  }

val name : string
val parameters : Jsonaf.t

(** Execute an actual native caller's request. Borrowed capabilities are the only
    authority ceiling. Validates requested limits/input, compiles without effects,
    then invokes the existing owned one-off service. Compilation diagnostics are
    source-bound structured failures. No Script child is admitted for rejected
    requests or failed compilation. The total timeout includes compilation and
    tool waits, with cooperative compiler cancellation and joined cleanup.

    The host must register this implementation with Tool_capability.Invocation_v1
    and consume [runtime_requests] at its owned response boundary. [execute] does
    not publish a provider output or make this tool available to a model. Public
    registration also requires authoring guidance/validation integration. *)
val execute
  :  ?observer:Agent_protocol.Invocation.observer
  -> env:Eio_unix.Stdenv.base
  -> policy:Chat_response.One_off_request.policy
  -> script_tools:Script_tool_calls.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> moderate_tool:
       (Agent_protocol.Invocation.t
        -> Chat_response.Moderation.Tool_call.t
        -> (Chat_response.Moderation.Outcome.t option, string) result)
  -> prepare_outcome:(Agent_protocol.Invocation.outcome -> (unit, string) result)
  -> Jsonaf.t
  -> (response, Agent_protocol.Error.t) result

(** Single versioned outcome envelope. Runtime requests and child invocation
    bookkeeping are host data, not embedded as a second model-visible result. *)
val output : response -> Openai.Responses.Tool_output.Output.t
