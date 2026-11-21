# Recovering a local session

Use the recovery tools selected for this session. Start with the current
objective, the last verified result and the next unfinished action.

- Use `context_remaining` to inspect the current request's capacity when
  available. An estimate or an unavailable count gives no evidence that a
  particular request was sent.
- Before asking for a new window, record the decisions and exact source
  references needed to resume. Use `recovery_note_write` for a session note
  and `context_checkpoint` to register the selected note revisions against
  the current window and cut. An `accepted` receipt records that checkpoint.
  If the response is lost, use checkpoint readback with the same key and
  expected window.
- `context_new_window` requests a transition. A pending receipt records
  admission. Confirm the successor with `context_list_windows` before
  describing the transition as committed. An archive-only successor
  preserves authorized history without a qualified handoff checkpoint.
- In the successor, read the selected note revision with `recovery_note_read`.
  Use `context_search` or `context_list_items` to locate retained evidence,
  then `context_read_text` for a bounded range of that exact revision.
  Report missing, revoked or incomplete evidence and preserve uncertainty.

Notes belong to their session. Share a specific revision only through an
explicitly authorized share. Follow the captured instructions for the
current session and distinguish observations from assumptions.
