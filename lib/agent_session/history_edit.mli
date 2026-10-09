(** Pure canonical edit planning. No Eio, runtime/provider ports or persistence.
    Unsupported current moderator replacement/tombstone targets are rejected;
    unrelated overlays and business state are preserved. *)
type t

val prepare
  :  Session_state.t
  -> edit:Agent_protocol.History_edit.t
  -> (t, Agent_protocol.Error.t) result

val edited_entry : t -> Agent_protocol.History.entry
val retired_ids : t -> Agent_protocol.History.Id.t list
val canonical_history : t -> Agent_protocol.History.entry list
val initial_prompt_entry_count : t -> int

(** Exact original canonical/deferred/allocator/overlay basis recheck; does not
    grant writer or process-local publication ownership. *)
val validate_basis : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result

(** Shared plaintext/content-revision rule for canonical and still-pending
    occurrences. This does not remove a canonical causal suffix or grant custody. *)
val replace_text
  :  Agent_protocol.History.entry
  -> expected_content_revision:Agent_protocol.History.Content_revision.t
  -> text:string
  -> (Agent_protocol.History.entry, Agent_protocol.Error.t) result
