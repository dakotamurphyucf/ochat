# `Chat_tui.Shell_management_service`

This Eio-backed service performs read-only audit loading/replay and other shell
management I/O away from the render/reducer path. Results are immutable and
tagged by the generation/request that initiated them.

The reducer applies only the current generation, so a slow older load cannot
overwrite a newer refresh. Audit reconstruction is non-executing and bounded
to the page’s recent-request view.
