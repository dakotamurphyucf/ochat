# `Chat_tui.Renderer_page_shell_security`

This full-screen renderer displays effective shell authority without sharing
Chat history layout state. It renders the five tabs, stable selections,
read-only audit timeline, interrupted warnings, and a persistent YOLO banner.

The page owns its `Notty_scroll_box`. Switching to/from it does not invalidate
Chat row caches, stop streaming, or mutate canonical history. Audit content is
already redacted before rendering.
