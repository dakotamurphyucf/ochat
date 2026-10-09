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
    a transaction. Ordinary deltas retain the existing strict preservation merge. *)
val admit
  :  Session_state_document.t
  -> delta:Session_delta.t
  -> next:Session_state.t
  -> limits:Document_schema.Limits.t
  -> (Session_state_document.t, Document_schema.Error.t) result
