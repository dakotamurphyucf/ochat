# Scoped goal completion

`Goal.finish(intent)` is a transactional `task unit` on `moderator_v1` and
`delegated_moderator_v1`. It requests completion of the goal already associated
with the owning Workflow. It accepts no goal ID, Run ID, attachment or receipt.
The compiler surface does not grant authority to finish a goal.

The JSON intent has exactly three fields:

```json
{
  "expected_goal_revision": "7",
  "evidence": "The requested checks completed successfully.",
  "action": {
    "kind": "finish",
    "terminal": { "kind": "completed" },
    "relinquish": []
  }
}
```

The revision is a nonnegative canonical decimal string. Evidence is nonblank
validated UTF-8 text, at most 4KiB. The action must be an explicit Completed
Run Finish decision, with the existing bounded result-reference and exact
relinquishment rules. The host resolves the goal from its immutable actual Run
association and checks the current goal revision, principal, source installation,
session generation and actual callback execution. A decoded intent is data;
without that checked association the operation cannot commit.

The native adapter stages one private receipt through the same lexical owner as
`Run.finish`. `Task.catch` discards a rolled-back completion request. At most one
surviving goal request may be selected. Preparation seals it; rollback after
preparation, failed preparation and callback release invalidate it. The script
receives unit, never a receipt that it could reuse as authority. Conflicting Run
actions reject the callback checkpoint.

The actor commits the achievement request, exact actual Run Action receipt and
moderator checkpoint together. The objective remains Open while root work is
still joining. Only the same episode's actual retained matching Completed
terminal receipt closes it as Achieved. Failed, cancelled, limited or interrupted
outcomes preserve truthful Run evidence and hold an enabled objective for source
review. Existing operator holds keep their reason. Completed without a selected
achievement request leaves the objective Open. Objective edits reject while an
achievement request is unresolved; progress, budgets and explicit controls keep
their own rules.

This primitive and its checked staging adapter are foundations for controller
integration. The current metadata operator commands do not enable a goal or
admit associated execution. The owning controller must supply authenticated
Enabled admission and bind the goal in the same real Run admission transaction.
A complete authored goal workflow, including actual worker join and controller
controls, still requires that integration qualification. A script cannot
bootstrap it by inventing an association or starting held goal work.

Checked compiler/native staging and Catch rollback are covered in
`test/lifecycle_helpers/staging_tests.ml`; receipt custody is covered in
`test/agent_session/run_action_scope_tests.ml`. Pure reciprocal association and
terminal classification, plus actual actor recovery persistence, are covered in
`test/agent_session/goal_run_tests.ml`. These fixtures do not claim a passing
controller-admitted authored goal workflow.
