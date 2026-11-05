open! Core

(** Pure bounded planning and retention custody alongside the entry's sole cursor.
    Ready work remains separate from batch-private calendar selection. No target,
    protected creation or Run state is invented. Unsupported execution variants
    fail closed. Controls withdraw only known unadmitted work; retention may
    retire whole known-terminal bundles while preserving lifetime counts. *)
type t

module Summary : sig
  type t = private
    { consumed : int64
    ; omitted : int64
    ; withdrawn : int64
    ; settled : int64
    }
  [@@deriving equal, sexp_of]
end

module Reason : sig
  type t =
    | Expired
    | Policy_skip
    | Superseded
    | Pending_capacity
    | Control_withdrawal
  [@@deriving equal, sexp_of]
end

module Issue : sig
  type t =
    | Zone_unavailable
    | Coverage_unavailable
    | Timing_exhausted
    | Capacity_saturated
    | Initial_delay_missed
    | Delay_nominal_missed
    | Execution_interrupted
    | Delay_successor_exhausted
  [@@deriving equal, sexp_of]
end

(** Initial construction requires a new revision-zero recurrence identity and is valid only with the actual definition's initial cursor.
    Historical advanced cursors cannot reconstruct missing consumed evidence. *)
val initial : Recurrence_definition.t -> (t, Agent_protocol.Error.t) result

val ready : t -> Recurrence_nominal.t list

(** Exact retained Ready row, including its unadmitted status and compatible
    unknown fields. This is immutable DATA, not source, admission or execution
    authority. A plain nominal projection cannot replace this complete carrier
    when recording the occurrence's immutable execution intent. *)
val find_ready_json : t -> Recurrence_identity.Occurrence.t -> Jsonaf.t option

(** Unresolved protected rows remain retained through controls and history
    pruning. A known terminal execution releases pending occupancy; its complete
    reservation and association retire only with that execution's whole bundle
    under bounded terminal-detail retention. *)
val protected_children : t -> Recurrence_child_reservation.t list

(** Immutable linked locators. A retained terminal execution classifies their
    settled occupancy; locator data alone authenticates neither the child nor
    execution. Whole terminal-bundle retention may retire these details. *)
val child_associations : t -> Recurrence_child_association.t list

val unlinked_children : t -> Recurrence_child_reservation.t list

(** Attach once to the exact retained reservation. Identical replay is a no-op;
    replacement, erasure, or association without a reservation rejects. The
    actor separately authenticates actual child facts and persistence ACK.
    Later whole known-terminal retirement is owned by atomic settlement. *)
val link_child : t -> Recurrence_child_association.t -> (t, Agent_protocol.Error.t) result

(** Entry-wide union of Ready, unresolved executions and protected children.
    A linked child and its execution consume one claim. Settled data alone grants
    no permission to dispatch another occurrence. *)
val pending_count : t -> int

val executions : t -> Recurrence_execution.t list

val find_execution
  :  t
  -> Recurrence_identity.Occurrence.t
  -> Recurrence_execution.t option

val pending_execution_steps : t -> int

(** Record only the exact initial ordinal-zero request. Existing targets move
    their complete original Ready nominal; fresh targets bind the exact retained
    reservation and linked child. Exact replay returns the original row without
    moving work or incrementing counters. This is data, never target authority.
    All current and historical intents validate against their retained definition
    and child facts. Complete future proof/control capacity is preflighted. *)
val record_execution
  :  t
  -> definition:Recurrence_definition.t
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (t * Recurrence_execution.t, Agent_protocol.Error.t) result

(** Resolve the exact retained current predecessor and append one contiguous
    attempt in one pure candidate. Complete intent, audit evidence and compatible
    wrappers remain retained. Exact current-successor/predecessor/resolution DATA
    replay returns the unchanged carrier; later successors are not this replay.
    No no-effects, retry or target authority is issued. No pruning or occupancy,
    cursor, control or lifetime-count movement accompanies this operation. *)
val rotate_execution
  :  t
  -> definitions:Recurrence_definition.t list
  -> cursor:Recurrence_cursor.t
  -> intent:Recurrence_execution_intent.t
  -> previous:Recurrence_execution_attempt.t
  -> resolution:Recurrence_retry_resolution.t
  -> next:Recurrence_execution_attempt.t
  -> (t * Recurrence_execution.t, Agent_protocol.Error.t) result

(** Pure exact Ready-to-protected disposition. Retried occurrence returns its
    original identities; actor authentication and ACK remain separate. *)
val reserve_child
  :  t
  -> definition:Recurrence_definition.t
  -> control_revision:int64
  -> principal_id:Agent_protocol.Id.Principal.t
  -> child_session_id:Agent_protocol.Id.Session.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> occurrence:Recurrence_identity.Occurrence.t
  -> (t * Recurrence_child_reservation.t, Agent_protocol.Error.t) result

val candidates : t -> Recurrence_nominal.t list
val calendar_cutoff : t -> Time_ns.Alternate_sexp.t option
val lookahead : t -> Recurrence_nominal.t option
val alarm : t -> Recurrence_alarm.t option
val issue : t -> Issue.t option
val revision : t -> int64
val summary : t -> Summary.t

(** Match the retained source revisions, exact definition digest, high-water,
    counts, pending/batch/history bounds, cursor and selected alarm/lookahead.
    This validates metadata association, never source/lifecycle permission. *)
val validate
  :  t
  -> definitions:Recurrence_definition.t list
  -> cursor:Recurrence_cursor.t
  -> (unit, Agent_protocol.Error.t) result

(** One control transaction withdraws all known Ready and batch-private work and
    retires the active alarm. Same-definition control keeps unconsumed lookahead as inert cursor-associated
    metadata, including retirement; only a new-definition edit clears it. No admitted or unknown work is erased. *)
val withdraw
  :  t
  -> definition:Recurrence_definition.t
  -> (t, Agent_protocol.Error.t) result

(** Start another immutable revision while retaining older high-water/totals and
    referenced captured source identities. At most eight revisions, no eviction. *)
val adopt_definition
  :  t
  -> previous:Recurrence_definition.t
  -> definition:Recurrence_definition.t
  -> (t, Agent_protocol.Error.t) result

(** Complete preserved frontier storage is bounded to 2 MiB, including positive
    known withdrawal/alarm-retirement/counter growth. This does not enlarge its
    enclosing entry, catalog, host or journal profile. *)
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result

val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) result
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result
val limits : Document_schema.Limits.t

type frontier = t

module Work : sig
  (** Ephemeral checked candidate builder. Operations update bounded domain data
      only; complete carrier encoding/adoption occurs once at [finish]. The same
      original carrier retains compatible unknowns and explicit history custody.
      This is not an actor command, durable proof or permission token. *)
  type t

  type omission =
    { first_slot : int64
    ; last_slot : int64
    ; reason : Reason.t
    }

  val start
    :  frontier
    -> definition:Recurrence_definition.t
    -> cursor:Recurrence_cursor.t
    -> (t, Agent_protocol.Error.t) result

  (** Validate the exact timing-owner range against the old interval cursor and
      selected nominal identities; selected plus omitted ranges partition it.
      Checked arithmetic bounds work by selected/range metadata, never slot count. *)
  val consume_interval
    :  t
    -> range:Recurrence_rules.Cursor.Interval.due_range
    -> selected:Recurrence_nominal.t list
    -> omitted:omission list
    -> (t, Agent_protocol.Error.t) result

  (** Initial fixed-delay selection only; the actual execution owner must supply
      terminal evidence before any later rearm. *)
  val consume_initial_delay
    :  t
    -> nominal:Recurrence_nominal.t
    -> (t, Agent_protocol.Error.t) result

  (** Select the exact current fixed-delay cursor, including a later nominal
      whose deadline was validated against its retained completion basis. *)
  val consume_delay
    :  t
    -> nominal:Recurrence_nominal.t
    -> (t, Agent_protocol.Error.t) result

  (** Begin or continue one captured wall cutoff. Existing Ready rows stay Ready;
      only new private candidates are coalesced while the cutoff is unproven. *)
  val begin_calendar
    :  t
    -> cutoff:Time_ns.Alternate_sexp.t
    -> (t, Agent_protocol.Error.t) result

  val collect_calendar
    :  t
    -> nominal:Recurrence_nominal.t
    -> (t, Agent_protocol.Error.t) result

  (** Publish the bounded policy result after the caller has proved that search
      ended at its cutoff. This structural builder does not prove Calendar.next
      returned End; the due planner owns that evidence.
      Future lookahead is unconsumed and retains its selected Zone provenance. *)
  val finish_calendar
    :  t
    -> lookahead:Recurrence_nominal.t option
    -> (t, Agent_protocol.Error.t) result

  val set_lookahead
    :  t
    -> nominal:Recurrence_nominal.t
    -> (t, Agent_protocol.Error.t) result

  (** Retire before replacing; sequence high-water never decreases or reuses an
      alarm ID, including a new alarm for the same retained nominal after pause. *)
  val retire_alarm : t -> (t, Agent_protocol.Error.t) result

  val arm
    :  t
    -> slot:int64
    -> due_at:Time_ns.Alternate_sexp.t
    -> (t, Agent_protocol.Error.t) result

  val set_issue : t -> Issue.t option -> t

  (** Verify joint cursor/high-water/lookahead association and capacity before
      admitting the candidate. Increments planning revision once when changed.
      Caller still must preflight the enclosing actual persistence profiles. *)
  val finish
    :  t
    -> cursor:Recurrence_cursor.t
    -> (frontier, Agent_protocol.Error.t) result
end

(** Structural update fence for durable candidates. Retained definition marks,
    consumed/omitted/withdrawn/settled and alarm-sequence high-water cannot regress.
    previous_cursor belongs to the prior frontier; cursor belongs to incoming.
    Evidence changes require an exact complete settlement candidate and cannot
    fall through to ordinary due validation.
    This cannot authenticate a clock, Zone, lifecycle or emission; the actual
    owner must use an exact-basis due plan before publishing a due transition. *)
val validate_transition
  :  t
  -> incoming:t
  -> definitions:Recurrence_definition.t list
  -> previous_cursor:Recurrence_cursor.t
  -> cursor:Recurrence_cursor.t
  -> (unit, Agent_protocol.Error.t) result

(** Private complete storage-only withdrawal/counter witness for enclosing
    reservation composition. Never decode, publish or treat it as domain proof. *)
val capacity_projection : t -> (Jsonaf.t, Document_schema.Error.t) result

type settlement =
  { frontier : t
  ; cursor : Recurrence_cursor.t
  ; result : Recurrence_settlement.t
  }

(** One complete pure candidate: associate/settle, lifetime increment, bounded
    whole terminal-bundle retirement and completion-derived successor or hold.
    Exact retained evidence is a no-op before current eligibility. Missing detail
    below consumed high-water conveys no prior completion or receipt equality.
    The publisher authenticates every new target observation separately. *)
val import_execution
  :  t
  -> definitions:Recurrence_definition.t list
  -> cursor:Recurrence_cursor.t
  -> origin:Recurrence_run_origin.t
  -> terminal:Recurrence_run_reference.t option
  -> automatic:Recurrence_settlement.Automatic.t
  -> (settlement, Agent_protocol.Error.t) result

val delay_basis : t -> Recurrence_delay_basis.t option

type resumption =
  { frontier : t
  ; cursor : Recurrence_cursor.t
  ; next : Recurrence_settlement.Next.t
  }

(** Exact stored definitive basis plus Awaiting cursor, consumed once by a new
    ordinary-pause Enable control candidate. Entry restricts control eligibility;
    a duplicate Enable never calls this data operation. *)
val resume_delay
  :  t
  -> definitions:Recurrence_definition.t list
  -> cursor:Recurrence_cursor.t
  -> (resumption, Agent_protocol.Error.t) result
