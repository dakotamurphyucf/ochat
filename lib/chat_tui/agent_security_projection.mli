open! Core

(** Adapts redacted daemon security records to the existing TUI management
    page without exposing daemon stores or unredacted shell identities. *)

(** [snapshot ~current projection] replaces daemon-owned grant and policy
    fields while retaining client-local presentation metadata. *)
val snapshot
  :  current:Shell_security_page_state.snapshot
  -> Agent_protocol.Snapshot.t
  -> Shell_security_page_state.snapshot

(** [audit_page ~session_id page] converts a redacted daemon audit page into
    immutable rows ordered by descending durable sequence. Ordering applies to
    the supplied page only; this function does not fetch additional pages or
    alter the protocol's ascending cursor order. *)
val audit_page
  :  session_id:Agent_protocol.Id.Session.t
  -> Agent_protocol.Audit.t Agent_protocol.Page.t
  -> Shell_security_page_state.audit_page
