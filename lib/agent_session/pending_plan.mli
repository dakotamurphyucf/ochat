(** Validated pure transition of the existing durable pending queue. The actor
    supplies actual host authority; this plan grants no persistence permission. *)
module Change : sig
  type t =
    | Enqueue of Pending_input_document.t list
    | Adopt of
        { boundary : Pending_eligibility.Boundary.t
        ; runtime_admission_open : bool
        }
    | Cancel of
        { history_id : Agent_protocol.History.Id.t
        ; expected_content_revision : Agent_protocol.History.Content_revision.t
        }
    | Replace_text of
        { history_id : Agent_protocol.History.Id.t
        ; expected_content_revision : Agent_protocol.History.Content_revision.t
        ; text : string
        }
    | Release of Agent_protocol.Pending_input.Terminal_proof.t
    | Retire of Pending_disposition.Retirement_reason.t
  [@@deriving sexp]
end

type t

(** Queue CAS is independent of unrelated streaming session revisions. Cancel
    and replace only operate on still-pending occurrences; already-adopted/retired
    results are obtained separately from truthful current disposition lookup. *)
val prepare
  :  Session_state.t
  -> expected_pending_revision:Agent_protocol.Pending_input.Revision.t
  -> change:Change.t
  -> limits:Document_schema.Limits.t
  -> retention:Pending_disposition.Retention.t
  -> (t, Agent_protocol.Error.t) result

val validate_basis : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
val revision : t -> Agent_protocol.Pending_input.Revision.t
val pending : t -> Pending_input_document.t list
val adopted_entries : t -> Agent_protocol.History.entry list
val dispositions : t -> Pending_disposition_document.t list
val requires_archive : t -> bool

(** Exact whole stored records removed by bounded retention. Publication must
    explicitly archive/retire their custody; this is never an implicit prune. *)
val expired_dispositions : t -> Pending_disposition_document.t list

(** Apply only to the exact admitted basis. Archive/custody and actor authority
    are additional publication obligations, never granted by this pure value. *)
val apply : t -> Session_state.t -> (Session_state.t, Agent_protocol.Error.t) result

(** Historical deferred_entries_adopted replay only. Every occurrence must carry
    the converted legacy Safe_boundary binding; modern timing cannot bypass the
    shared policy via this old tag. Custody is still transferred exactly. *)
val legacy_adoption
  :  Session_state.t
  -> limits:Document_schema.Limits.t
  -> (t, Agent_protocol.Error.t) result
