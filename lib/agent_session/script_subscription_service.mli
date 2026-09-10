open Core

(** Narrow host adapter; each callback revalidates the actual actor borrow,
    source and generation. This service constructs data, never acquires authority. *)
type host =
  { create :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> kind:string
      -> lifetime_ms:int
      -> wake:Agent_protocol.Completion.wake
      -> completion_schema:Jsonaf.t option
      -> (int * Agent_protocol.Subscription.t, Agent_protocol.Error.t) result
  ; stage :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> previous:Agent_protocol.Subscription.t option
      -> next:Agent_protocol.Subscription.t
      -> (int, Agent_protocol.Error.t) result
  ; get :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> Agent_protocol.Id.Subscription.t
      -> (Agent_protocol.Subscription.t, Agent_protocol.Error.t) result
  ; select :
      Agent_protocol.Job.launch_owner
      -> Agent_protocol.Invocation.observer
      -> int list
      -> (unit, Agent_protocol.Error.t) result
  ; abort : Agent_protocol.Job.launch_owner -> int -> unit
  }

type t
type scope

(** Managed calls retain their opaque admitted registry execution, whose revision
    includes registry authority. It is not the raw compiled tool fingerprint. *)
type origin =
  | Direct of Chat_response.Extension_compiler.t * Agent_protocol.Invocation.t
  | Managed of Chat_response.Managed_tool_registry.execution

val create
  :  now:(unit -> Agent_protocol.Timestamp.t)
  -> limits:Staged_subscriptions.limits
  -> host:host
  -> t

(** Only a dispatched moderator tool supplies [originating]. Its compiled
    declaration supplies the completion schema. Event/observation scopes pass
    None and may operate on existing source-owned subscriptions, but not create
    new acknowledgements. All receipts are tracked before result projection;
    whole-handler failure aborts them newest-first. No late rejection follows
    an acknowledged save. *)
val with_scope
  :  t
  -> owner:Agent_protocol.Job.launch_owner
  -> source:Agent_protocol.Invocation.observer
  -> originating:origin option
  -> error:(string -> 'error)
  -> (scope -> ('a, 'error) result)
  -> ('a, 'error) result

val moderator_transaction : scope -> Chat_response.Subscription_operations.transaction

(** Pending must refer to a still-owned creation from this invocation's surviving
    transaction, not another readable subscription or a rolled-back reservation. *)
val validate_work : scope -> Agent_protocol.Id.Subscription.t -> (unit, string) result
