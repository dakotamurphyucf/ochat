# CI qualification record

Tasks 17–19 are undergoing Linux qualification in
[PR #24](https://github.com/dakotamurphyucf/ochat/pull/24).
Final successful gate timings and protected-main publication results will be
recorded here after qualification; pending or failed runs are not speed results.

The baseline successful gate took 1,217 seconds with 1,856 summed validation
runner-seconds in [run 34186360736](https://github.com/dakotamurphyucf/ochat/actions/runs/34186360736).
It did not include the new normal/E2E framework tiers.

Initial Linux failures blocked the required gate and publication. Corrected
normal tests passed in run 34189116967; the complete PR-safe E2E tier passed in
run 34190368673. Neither alone qualifies a release. See the
[maintenance guide](ci-enforcement.md) for coverage, selection, cache operations,
and the platform issues addressed during qualification.

Browser sharding has not been introduced. Its necessity will be assessed using
successful cold/warm measurements while retaining every browser/environment.
