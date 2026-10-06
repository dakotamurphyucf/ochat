# Development-cycle design and audit records

These records specify implementation boundaries and preserve reviewed evidence.
They do not establish completion of the product features they describe.

| Record | Linear owner | Outcome |
| --- | --- | --- |
| [Provider and storage contracts](och-49/README.md) | [OCH-49](https://linear.app/ochat/issue/OCH-49) | Compile-checked design interfaces, JSON presence probes, ownership and migration ledger. |
| [ChatML correctness audit](och-70/README.md) | [OCH-70](https://linear.app/ochat/issue/OCH-70) | Five confirmed defect categories, executable observations, coverage and verification limits. |
| [Unified CLI and process contracts](och-108/README.md) | [OCH-108](https://linear.app/ochat/issue/OCH-108) | Command/lifetime decisions, current-process evidence and downstream implementation requirements. |

## Follow-up ownership

The audit's confirmed fixes are [OCH-154](https://linear.app/ochat/issue/OCH-154)
(modules versus records), [OCH-155](https://linear.app/ochat/issue/OCH-155)
(exact record coverage), [OCH-156](https://linear.app/ochat/issue/OCH-156)
(variant payload arity), [OCH-157](https://linear.app/ochat/issue/OCH-157)
(equality constraints) and [OCH-158](https://linear.app/ochat/issue/OCH-158)
(module initialization). [OCH-159](https://linear.app/ochat/issue/OCH-159)
owns bounded additional correctness verification. Current-defect expectations
must change to desired behavior as each fix lands; passing reproductions does
not mean the defects are fixed.

[OCH-160](https://linear.app/ochat/issue/OCH-160) owns shared host run state and
terminal receipts, reusing the session actor and persistence. OCH-112 consumes
this feature for headless workflows; CAP-R4 retains authored lifecycle policy.

Provider/storage refinements were applied to the existing OCH-50/51/52/53/55/56/
58/59/62/69/86 tickets; CLI refinements were applied to OCH-110/111/112/117.
The proposal JSON files preserve research handoff details. Live Linear issues
and their explicit dependencies track subsequent scope changes and completion.

All evidence records identify the audited source revision and local toolchain.
Local package versions differ from the CI lock; no exact CI-environment, full
soundness, future unified-artifact or release qualification is implied.
