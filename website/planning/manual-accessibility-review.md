# P10 manual accessibility worksheet

Status: deferred from launch by explicit user direction on 2026-09-07. Retained as later follow-up, not a release blocker and not a completed review. Automated browser,
axe, ARIA-snapshot, keyboard-traversal and CSS-reflow evidence is recorded separately.
Do not mark these manual checks passed from that evidence alone.

Record candidate artifact SHA-256, origin, reviewer, date, device/OS, browser and
assistive-technology versions before starting. Use the homepage, first-agent
lesson, an application/source reader, long ChatML reference, and search dialog.

| Check | Procedure and expected result | Observation |
|---|---|---|
| Keyboard only | Reach main via skip link; traverse links, file selection/copy/wrap and playback controls. Focus stays visible; code/tables scroll with keys. | Pending human review |
| Dialogs | Open menu/search, enter a query, choose a result; Tab stays within modal, Escape closes and returns focus. | Pending human review |
| VoiceOver + Safari | Read headings/landmarks and controls; verify names, expanded/pressed states, selected file, search loading/errors, copy status and playback status. | Pending |
| NVDA + Firefox, if available | Repeat core article/search/source-reader flow; record unavailable equipment explicitly. | Availability not established |
| Native zoom | Use actual browser zoom at 400% and text enlargement at 200%; navigate and inspect long code without losing controls. | Pending; CSS emulation is separate |
| Mobile software keyboard | On a physical phone, focus search, type/edit query, scroll results, select result and dismiss keyboard. Input/results/focused controls stay reachable. | Pending |
| Themes/motion/contrast | Sample light/dark, reduced motion and forced colors; controls remain distinguishable and usable. | Automated checks pass; manual sampling pending |
| Sticky elements and alternatives | Follow deep links; focused headings/controls are unobscured; diagram/image alternatives convey purpose. | Automated checks pass; manual sampling pending |

Record defects with a route, interaction, expected/actual behavior, and evidence.
Retest fixes on the same device/technology. Transfer completed observations into
the approval record only for the reviewed production artifact.
