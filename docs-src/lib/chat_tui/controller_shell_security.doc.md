# `Chat_tui.Controller_shell_security`

The controller owns Shell Security page and modal key routing. Active shell
approval has priority, followed by grant-revocation modal, moderator modal, and
page navigation. This ordering mirrors host pending-input priority.

Approval keys select only manifest-enabled scopes, require confirmation for
prefix/durable choices, support bounded denial text, and convert Escape into a
local denial rather than session quit. Page keys switch tabs, select/scroll,
refresh, and request revocation without touching Chat draft state.
