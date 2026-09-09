open! Core

(** Adapter from the shared foreground operation contract to Ochat's existing
    identity-bearing response loop. *)

module Config : sig
  type t =
    { env : Eio_unix.Stdenv.base
    ; response_dir : Eio.Fs.dir_ty Eio.Path.t
    ; tools : Openai.Responses.Request.Tool.t list
    ; tool_tbl : (string, Ochat_function.runner) Hashtbl.t
    ; temperature : float option
    ; max_output_tokens : int option
    ; reasoning : Openai.Responses.Request.Reasoning.t option
    ; moderator : Chat_response.In_memory_stream.moderator option
    ; permission_profile : Permission_policy.t
    ; review_permission :
        Permission_policy.invocation
        -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result
    ; history_compaction : bool
    ; parallel_tool_calls : bool
    ; model : Openai.Responses.Request.model
    ; prompt_cache_key : string option
    ; prompt_cache_retention : string option
    ; post_stream : Chat_response.In_memory_stream.post_stream option
    ; agent_page_classifications :
        (string * Chat_response.Tool_execution_event.agent_page_kind) list
    ; delegated_permission_tools : String.Set.t
    ; redact_tool_payload : name:string -> string -> string
    }
end

(** [create config] delivers the committed submitted user entry to the
    moderator exactly once before the first turn-start boundary. Moderator
    requests to end the session at that boundary skip provider execution.
    Automatic follow-up turns do not re-emit the submission event. *)
val create
  :  ?dispatch_tool:
       (input:Operation_worker.Input.t
        -> capabilities:Operation_worker.Capabilities.t
        -> Chat_response.In_memory_stream.Tool_dispatch.t)
  -> ?moderator_events:
       (input:Operation_worker.Input.t
        -> capabilities:Operation_worker.Capabilities.t
        -> ( Chat_response.In_memory_stream.moderator_event_handlers
             , Agent_protocol.Error.t )
             result)
  -> Config.t
  -> Operation_worker.t
