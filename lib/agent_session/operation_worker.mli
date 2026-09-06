open! Core

(** Transport-neutral foreground execution contract. A worker receives an
    immutable operation snapshot and actor-backed capabilities; it never owns
    mutable session state. *)

module Input : sig
  type t =
    { session_id : Agent_protocol.Id.Session.t
    ; session_generation : int
    ; operation : Agent_protocol.Operation.t
    ; history : History_entry.t list
    }
end

module Capabilities : sig
  type t =
    { id_source : History_entry.Id_source.t
    ; commit_entry : History_entry.t -> (unit, Agent_protocol.Error.t) result
    ; commit_moderator : Jsonaf.t option -> (unit, Agent_protocol.Error.t) result
      (** Checkpoint committed moderator state for this active operation.
            Identical snapshots are no-ops; stale/cancelled workers are rejected. *)
    ; consume_deferred : unit -> (History_entry.t list, Agent_protocol.Error.t) result
    ; request_permission :
        permission:Agent_protocol.Permission.t
        -> timeout_seconds:float option
        -> fallback:Agent_protocol.Permission.choice
        -> review_on_timeout:
             (unit
              -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
               option
        -> (Agent_protocol.Permission.resolution, Agent_protocol.Error.t) result
    ; request_review :
        permission:Agent_protocol.Permission.t
        -> review:
             (unit
              -> (Permission_reviewer.Decision.t, Permission_reviewer.Error.t) result)
        -> (Agent_protocol.Permission.resolution, Agent_protocol.Error.t) result
    ; responder_available : unit -> bool
    ; invocation_granted : tool_name:string -> identity_digest:string -> bool
    ; publish_live :
        kind:Agent_protocol.Event.Recoverable.kind -> payload:Jsonaf.t -> unit
    }
end

module Summary : sig
  type t =
    { final_history : History_entry.t list
    ; runtime_requests : Chat_response.Moderation.Runtime_request.t list
    ; moderator_snapshot : Jsonaf.t option
    }
end

type cancellation = { reason : string }

type outcome =
  | Completed of Summary.t
  | Cancelled of cancellation
  | Failed of Agent_protocol.Error.t

type t

val create : run:(sw:Eio.Switch.t -> input:Input.t -> Capabilities.t -> outcome) -> t
val run : t -> sw:Eio.Switch.t -> input:Input.t -> Capabilities.t -> outcome
