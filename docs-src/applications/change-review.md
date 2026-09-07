# Review a change against the requirements

Read a supplied patch and acceptance criteria, then produce findings tied to the changed lines.

Make your review checklist repeatable. Add a specialist or explicitly configured test command as a separate extension.

## The outcome

Input: A sample diff and requirements.

Output: A review checklist with suggested tests.

## Review the included patch

Complete [installation and provider setup](../agent-server/quickstart.md), extract
the change-review bundle, and run from its directory:

```sh
mkdir -p runs
ochat chat-completion -prompt-file agent.chatmd -output-file runs/review-01.chatmd
```

The prompt reads a small Python diff and the requirement to preserve internal
spacing. The proposed `"".join(name.split())` removes that spacing. A useful review
identifies that mismatch and proposes a test such as an input containing a
two-word name. The preview is an illustration, not a captured model review.

## Bring your own change

Replace the diff and requirements with the change you want reviewed. Add project
conventions to the instructions, and ask for evidence tied to changed lines.
This prompt can inspect the supplied files but cannot run tests or modify code.

## Add test feedback deliberately

Use the [shell tutorial](../agent-server/tutorials/shell-agent.md) to configure a
specific test command under the chosen host's permissions. An agent declaration
alone does not authorize execution. Keep static review findings separate from
actual test results, and review proposed edits before applying them.
A [specialist agent](../tutorials/specialist.md) can apply a separate review checklist.

[Explore another application](README.md) or [follow the tutorial curriculum](../tutorials/README.md).
