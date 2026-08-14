# `Chat_tui.Renderer_shell_approval`

Shell approval, grant revocation, and moderator input are centered overlays
composed after the base page. The renderer shows redacted command identity,
effects/policy/rationale, enabled scopes, queue count, optional detail, and
confirmation/denial stages.

Because overlays are post-composition UI state, opening or editing them does
not rebuild message presentation or enter history. Cursor placement is derived
from modal-local text state.
