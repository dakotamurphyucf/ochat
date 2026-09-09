# X01: report aggregation

This is an internal qualification fixture for the opt-in `run_chatml` path.
General model-visible extension availability remains gated on A01.

The agent declares `run_chatml` and a `read_file` capability confined to
`${workspace}/reports`. Submit the contents of `aggregate.chatml` as `source`,
`["report-a.json", "report-b.json"]` as `input`, and `["read_file"]` as `tools`.
The script reads the reports, groups failed checks by name, and returns
`expected.json`. It handles the file reader's two metadata lines before parsing
the JSON body and fails if a report cannot be read or has an invalid shape.

The daemon test installs these reports in an isolated workspace. A separate
case requests `../secret.json` both through the script and through a direct
model tool call, compares the native denial outcomes, and checks that private
file content never reaches saved session state.

Run from the repository root:

```sh
dune build @test/chatml_composition/runtest
```

The provider is deterministic and offline. Each test uses one normal model
tool-call response and one normal continuation response. Script execution adds
no model request, moderator, background job, or session. The test checks both
the live session registry and the durable session store.
