# `Chat_tui.Shell_security_page_state`

This module owns page-local Shell Security state: Overview/Runtimes/Grants/
Audit/Interrupted tab selection, immutable management snapshot, scroll and
selection identity, audit load generation, approval modal stages, grant
revocation modal, and moderator input modal.

Approval stages separate choice, prefix/durable confirmation, and denial-reason
editing. The manifest request carries the only enabled scopes. Closing a modal
does not mutate the composer or conversation history.

Selections use stable grant/request IDs and are repaired when a refreshed
snapshot removes an item. Chat history render caches are not stored here.
