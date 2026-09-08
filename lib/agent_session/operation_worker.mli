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
    ; commit_invocation_call :
        invocation:Agent_protocol.Invocation.t
        -> History_entry.t
        -> (unit, Agent_protocol.Error.t) result
      (** Atomically retain a model call and its Admitted invocation before any
          observer or dispatch runs. Failed persistence saves neither. Identical
          retries do not change a recorded outcome. This records intent, not tool
          authorization; dispatch still needs current ownership/policy checks. *)
    ; publish_invocation_output :
        invocation_id:Agent_protocol.Id.Invocation.t
        -> History_entry.t
        -> (unit, Agent_protocol.Error.t) result
      (** Atomically append a bound model invocation's initial result and retain
          its publication receipt. Requires a running owning operation and a
          recorded, validated/disclosed outcome. Same-occurrence retries are
          no-ops even after history compaction; another occurrence is rejected.
          Does not execute handlers or post-tool observations. *)
    ; commit_moderator : Jsonaf.t option -> (unit, Agent_protocol.Error.t) result
      (** Checkpoint committed moderator state for this active operation.
            Identical snapshots are no-ops; stale/cancelled workers are rejected. *)
    ; with_invocation :
        invocation:Agent_protocol.Invocation.t
        -> (dispatched:Agent_protocol.Invocation.t
            -> (Agent_protocol.Invocation.outcome, Agent_protocol.Error.t) result)
        -> (Agent_protocol.Invocation.t, Agent_protocol.Error.t) result
      (** Host-only foreground invocation lifecycle for native/standalone calls.
          Admits and dispatches, or dispatches an exact previously saved model-call
          intent. Runs the callback outside the actor and persists its validated/
          disclosed outcome. Does not borrow moderator
          state, serialize unrelated calls, authorize tools or publish history.
          A parent invocation must be a live callback of the same operation;
          background-job ownership uses a separate service. Callback errors and
          exceptions retain a bounded terminal failure; cancellation retains a
          cancelled result. Failed outcome persistence leaves interruption evidence
          for worker/restart cleanup, never retries external work. The host must
          recheck current capability, policy and disclosure in the callback. *)
    ; with_moderator_invocation :
        invocation:Agent_protocol.Invocation.t
        -> (dispatched:Agent_protocol.Invocation.t
            -> commit:
                 (resolved:Agent_protocol.Invocation.t
                  -> snapshot:Session.Moderator_state.Identity_snapshot.t
                  -> (unit, Agent_protocol.Error.t) result)
            -> (unit, Agent_protocol.Error.t) result)
        -> (unit, Agent_protocol.Error.t) result
      (** Scoped exclusive moderator handoff for an already authorized call.
          Atomically admits/dispatches the invocation, or dispatches an exact
          previously saved model-call intent, then runs the callback outside the
          actor. [commit] saves the resolution and proposed snapshot
          together before returning; call it from the manager's preparation hook.
          Successful preparation must be followed immediately by infallible,
          non-yielding runtime installation. The borrow remains held until the
          callback returns. Failure/cancellation records a terminal outcome if
          no resolution was committed. No provider output is published here.
          Current capability/policy admission must precede this trusted service;
          it supplies operation ownership, not tool authorization. Independent
          callers queue outside the actor and recheck operation ownership before
          admission. The callback must revalidate current authority before effects
          after any wait. Same-owner recursion and cross-owner acquisition cycles
          fail before admission through the shared execution coordinator. *)
    ; with_moderator_observation :
        invocation_id:Agent_protocol.Id.Invocation.t
        -> (observing:Agent_protocol.Invocation.t
            -> commit:
                 (resolved:Agent_protocol.Invocation.t
                  -> snapshot:Session.Moderator_state.Identity_snapshot.t
                  -> (unit, Agent_protocol.Error.t) result)
            -> (unit, Agent_protocol.Error.t) result)
        -> (unit, Agent_protocol.Error.t) result
      (** Exclusive foreground observation handoff. The actor reads retained
          Awaiting intent, checks current generation/operation and parent completion,
          and saves Observing before running the callback outside its mailbox.
          [commit] accepts only Observed with the exact observer source identity and
          saves receipt plus prospective moderator snapshot atomically. Call from
          the manager's preparation hook, then install without yielding. Failure
          or cancellation records a separate observation failure; native outcomes
          are never replaced. Same-owner recursion/cross-owner wait cycles fail.
          This does not install an idle drain or authorize tool calls by observers. *)
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
