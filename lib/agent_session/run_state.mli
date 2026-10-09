(** Bounded actor-owned local run index. The enclosing named carrier retains
    historical absence and every unknown field. No runtime/callback is retained. *)
type t [@@deriving sexp]

(** Compare public run evidence and pending action state with typed equality.
    Private delivery-frame retention/disposition and wire-presence bookkeeping
    do not change the client observation. No IO or validation is performed. *)
val equal_observation : t -> t -> bool

val empty : t
val installation : t -> Run_source_installation.t
val runs : t -> Agent_protocol.Run.t list
val intents : t -> Run_intent.t list
val receipts : t -> Agent_protocol.Run_receipt.t list
val job_deliveries : t -> Run_job_delivery.t list
val find_job_delivery : t -> Run_job_delivery.Key.t -> Run_job_delivery.t option

val enqueued_job_frame
  :  t
  -> frame:Chat_response.Background_delivery.t
  -> (Run_job_delivery.t option, Agent_protocol.Error.t) result

(** Internal immutable occurrence custody. Actor planners prove current source,
    actual completion and host authority before capture/enqueue/claim commits. *)
val add_job_delivery : t -> Run_job_delivery.t -> (t, Agent_protocol.Error.t) result

val replace_job_delivery : t -> Run_job_delivery.t -> (t, Agent_protocol.Error.t) result
val find : t -> Agent_protocol.Id.Run.t -> Agent_protocol.Run.t option

(** Same principal+request key returns the exact retained receipt only for the
    original digest. Source replacement never converts an old receipt to current
    authority; callers perform current visibility admission before lookup. *)
val receipt
  :  t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> key:Agent_protocol.Idempotency_key.t
  -> request_sha256:string
  -> (Agent_protocol.Run_receipt.t option, Agent_protocol.Error.t) result

(** Pending Continue decisions may coalesce at one actual admission. Wait/Finish
    cannot supersede unresolved earlier decisions or silently erase their intent. *)
val check_action
  :  t
  -> run_id:Agent_protocol.Id.Run.t
  -> action:Agent_protocol.Run_action.t
  -> (unit, Agent_protocol.Error.t) result

val commit
  :  t
  -> run:Agent_protocol.Run.t
  -> receipt:Agent_protocol.Run_receipt.t
  -> intent:Run_intent.t option
  -> (t, Agent_protocol.Error.t) result

(** Advance retained owner evidence/dispositions without issuing a new action.
    Every run and intent must already exist; immutable receipts are preserved and
    the complete index transition is validated before admission. *)
val advance
  :  t
  -> run:Agent_protocol.Run.t
  -> intents:Run_intent.t list
  -> (t, Agent_protocol.Error.t) result

(** Atomically claim a retained exact job delivery and consume its owning Wait.
    Both run evidence and carrier transition are validated as one final index. *)
val advance_claim
  :  t
  -> job_delivery:Run_job_delivery.t
  -> run:Agent_protocol.Run.t
  -> intents:Run_intent.t list
  -> (t, Agent_protocol.Error.t) result

(** One atomic source replacement retires all obsolete run scopes and pending
    action obligations. Checked ownership disposition is constructed by the actor
    from its actual owner state, not inferred by this index. *)
val replace_installation
  :  t
  -> installation:Run_source_installation.t
  -> retired_runs:Agent_protocol.Run.t list
  -> (t, Agent_protocol.Error.t) result

val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t

(** Check the actual structurally preserved run subtree against its outstanding
    occurrence reserves. Document adapters call this after adoption, so future
    metadata cannot consume space promised to an unresolved job Wait. *)
val validate_encoded_capacity : t -> Jsonaf.t -> (unit, Agent_protocol.Error.t) result

(** Host capacity adapter adds a nonnegative reserve derived from existing durable
    job retry policy. Uses the same actual raw subtree and outstanding Wait and
    carrier disposition reserves. *)
val validate_encoded_capacity_with_reserve
  :  t
  -> additional_bytes:int
  -> Jsonaf.t
  -> (unit, Agent_protocol.Error.t) result

val validate
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> (unit, Agent_protocol.Error.t) result

val validate_transition : previous:t -> t -> (unit, Agent_protocol.Error.t) result

(** Complete an administrative candidate's already-planned single installation
    rotation with its final captured source. Validates [previous] -> candidate
    retirement first and [previous] -> final again; preserves every run, intent
    and immutable receipt. The candidate must be exactly one epoch ahead. This
    joins planning within one replacement commit, not two durable installations. *)
val complete_replacement
  :  t
  -> previous:t
  -> source:Agent_protocol.Invocation.observer option
  -> (t, Agent_protocol.Error.t) result
