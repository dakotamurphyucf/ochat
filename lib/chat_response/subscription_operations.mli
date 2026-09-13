open Core

(** Moderator-only runtime adapter. Callbacks must enforce the live moderator
    owner, generation, schemas, quotas and lifetime policy. Registration alone
    grants no authority. Each mutation stages a distinct nonnegative receipt,
    including an idempotent terminal update that retains an earlier winner. *)
type handlers =
  { create :
      kind:string
      -> lifetime_ms:int option
      -> wake:Agent_protocol.Completion.wake
      -> (int * Agent_protocol.Id.Subscription.t, string) result
  ; get :
      Agent_protocol.Id.Subscription.t -> (Agent_protocol.Subscription.t, string) result
  ; finish :
      id:Agent_protocol.Id.Subscription.t
      -> expected_epoch:int
      -> Agent_protocol.Completion.t
      -> (int * Agent_protocol.Subscription.t, string) result
  ; arm :
      id:Agent_protocol.Id.Subscription.t
      -> expected_epoch:int
      -> timer_id:Agent_protocol.Id.Schedule.t option
      -> job_id:Agent_protocol.Id.Job.t option
      -> (int * Agent_protocol.Subscription.t, string) result
  ; rollback : int -> unit
  }

(** Prepare selects surviving receipts in execution order. Its returned
    acknowledgement is infallible and runs only after the owning durable save.
    The host must discard all staged mutations when the whole handler fails. *)
type transaction =
  { handlers : handlers
  ; prepare : int list -> (unit -> unit, string) result
  }

val dynamic_handlers : (unit -> transaction option) -> handlers

(** Mutation receipts are recorded privately; the builtin surface projects only
    the public result to ChatML. Reads observe the owner's provisional state.
    Catch rollback undoes mutations in reverse execution order. *)
val install
  :  ?control:Chatml.Chatml_lang.execution_control
  -> handlers:handlers
  -> Chatml_host_runtime.runtime_config
  -> Chatml_host_runtime.runtime_config

(** Reject malformed or duplicate receipts and preserve unrelated effect order.
    Selection by receipt never substitutes for host ownership validation. *)
val split_mutations
  :  Chatml.Chatml_lang.eff list
  -> (int list * Chatml.Chatml_lang.eff list, string) result
