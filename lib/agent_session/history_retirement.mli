(** Pure exact-document custody admission shared by live persistence and delta
    replay BEFORE candidate extension-carrier encoding. Permits only the planned
    old target payload and retired canonical suffix, or the exact planned ordinary
    paired deletion, to leave the current document;
    envelope/unrelated unknown fields remain and exact old evidence is archived.
    No archival IO, runtime or actor authorization occurs here. *)
type t

val create
  :  Session_state_document.t
  -> edit:History_edit.t
  -> archive:Session_state.Compaction_archive.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val apply
  :  t
  -> next:Session_state.t
  -> limits:Document_schema.Limits.t
  -> (Session_state_document.t, Document_schema.Error.t) result

(** Shared live/replay entry point. Exactly one edit or deletion intent may retire history in
    a transaction. Ordinary deltas retain the existing strict preservation merge.
    With window tracking, the same transaction validates and stamps its new
    source locators after this retirement prestate check. *)
val admit
  :  Session_state_document.t
  -> delta:Session_delta.t
  -> next:Session_state.t
  -> limits:Document_schema.Limits.t
  -> (Session_state_document.t, Document_schema.Error.t) result

(** Move the exact pruned row envelopes into the checked compaction prestate;
    final publication admits the supplied next State before writing. *)
val admit_summary
  :  Session_state_document.t
  -> archive:Session_state.Compaction_archive.t
  -> next:Session_state.t
  -> limits:Document_schema.Limits.t
  -> (Session_state_document.t, Document_schema.Error.t) result

(** Verify the same exact prior universal carrier as the durable Rollover frame,
    then retain the selected entry envelopes and unrelated extension fields. *)
val admit_rollover
  :  Session_state_document.t
  -> Recovery_rollover.Commit.t
  -> next:Session_state.t
  -> limits:Document_schema.Limits.t
  -> (Session_state_document.t, Document_schema.Error.t) result
