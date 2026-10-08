open! Core

(** Adapter from the shared foreground operation contract to Ochat's existing
    identity-bearing response loop. *)

module Config : sig
  type t =
    { env : Eio_unix.Stdenv.base
    ; inference_context : Inference_runtime.Context.t
    ; inference_identity : Chat_response.Neutral_turn.Identity.t
    ; on_inference_attempt : Inference_runtime.Attempt.t -> unit
    ; on_inference_completion : Inference_client.Completion.t -> unit
    ; on_inference_observation : Inference.Observation.t -> unit
    ; response_dir : Eio.Fs.dir_ty Eio.Path.t
    ; tools : Openai.Responses.Request.Tool.t list
    ; tool_tbl : (string, Ochat_function.runner) Hashtbl.t
    ; moderator : Chat_response.In_memory_stream.moderator option
    ; permission_profile : Permission_policy.t
    ; review_permission :
        Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result
    ; history_compaction : bool
    ; parallel_tool_calls : bool
    ; agent_page_classifications :
        (string * Chat_response.Tool_execution_event.agent_page_kind) list
    ; delegated_permission_tools : String.Set.t
    ; redact_tool_payload : name:string -> string -> string
    }
end

(* The optional authoring-context factory is trusted host configuration. It
   must use the operation's session/generation scope and actual admitted tools.
   Missing guidance is committed atomically after moderator preparation and
   before provider execution, without submission or item-appended events. The
   factory does not install helpers or resolve ChatMD policy declarations. *)

(** [create config] delivers the committed submitted user entry to the
    moderator exactly once before the first turn-start boundary. Moderator
    requests to end the session at that boundary skip provider execution.
    Automatic follow-up turns do not re-emit the submission event. The optional
    initial notification hook supplies only newly committed entries and claims
    restored wakes for the upcoming provider admission. New data receives its
    item-appended callbacks once; restored history is not appended or re-emitted.
    All committed data is retained if a callback ends the session. *)
val create
  :  ?runtime_policy:Chat_response.Runtime_semantics.policy
  -> ?authoring_context:
       (input:Operation_worker.Input.t
        -> (Chat_response.Authoring_materialization.t, Agent_protocol.Error.t) result)
  -> ?dispatch_tool:
       (input:Operation_worker.Input.t
        -> capabilities:Operation_worker.Capabilities.t
        -> Chat_response.In_memory_stream.Tool_dispatch.t)
  -> ?moderator_events:
       (input:Operation_worker.Input.t
        -> capabilities:Operation_worker.Capabilities.t
        -> ( Chat_response.In_memory_stream.moderator_event_handlers
             , Agent_protocol.Error.t )
             result)
  -> ?notification_input:
       (input:Operation_worker.Input.t
        -> unit
        -> ( Chat_response.In_memory_stream.Safe_point_input.batch
             , Agent_protocol.Error.t )
             result)
  -> ?initial_notification_input:
       (input:Operation_worker.Input.t
        -> unit
        -> ( Chat_response.In_memory_stream.Safe_point_input.batch
             , Agent_protocol.Error.t )
             result)
  -> Config.t
  -> Operation_worker.t
