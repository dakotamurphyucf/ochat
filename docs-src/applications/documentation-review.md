# Find the gaps in your documentation

Let an explorer gather the context and a specialist identify what a new contributor needs.

Give the reviewer its own instructions, then reuse it from other agents. Inspect the complete recorded run.

## The outcome

Input: The sample Lantern setup guide.

Output: Three concrete findings and a next action.

## Inspect the workflow

The explorer reads the included Lantern setup guide and passes its contents to
`docs-reviewer.chatmd`. The specialist has its own instructions and no tools.
The explorer turns that feedback into a report for the maintainer. Neither
agent can edit files or run shell commands.

Use the execution viewer above to inspect the captured calls and their results.
The recording identifies its provider and scope. The original ChatMD transcript
is included with the source files below; a new model run can produce different
wording and findings.

## Run it yourself

Complete [installation and provider setup](../agent-server/quickstart.md).
Extract the documentation-review bundle and launch from its directory:

```sh
mkdir -p runs
ochat chat-completion -prompt-file explorer.chatmd -output-file runs/review-01.chatmd
```

The prompt already contains the request. Use a fresh output filename for each
independent run; an existing file participates in the batch resume workflow.
Both prompts select `gpt-4.1`; choose an available supported model in both if
needed. Model work, including the specialist, incurs provider charges.

## What a useful result contains

Check that findings refer to the supplied requirements, setup, or verification
sections. Missing commands should be identified as missing, not invented. The
maintainer decides which corrections are accurate before changing documentation.

## Adapt the reviewer

Replace the sample input, update the read root, and give the specialist your
review checklist. To turn findings into proposed edits, add editing tools as an
explicit next step and review the resulting diff. Follow the
[specialist tutorial](../tutorials/specialist.md) to understand relative paths
and what context the child actually receives.

[Explore another application](README.md) or [follow the tutorial curriculum](../tutorials/README.md).
