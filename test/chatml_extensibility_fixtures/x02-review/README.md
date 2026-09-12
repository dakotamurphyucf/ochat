# X02: stateful review references

This executable acceptance fixture stores one review reference per revision in
the moderator state. `begin_review` returns the existing reference for a repeated
revision and allocates a new reference for a different revision. The references
identify records in this example; they are not child sessions or background jobs.

`agent.chatmd` registers the tools and captures `review.chatml` and both schemas.
The extra `unhandled_review` and `double_resolve` declarations exercise explicit
host errors. They are negative test cases, not suggested application tools.

The test in `test/agent_server_restart_test.ml` loads these exact files through
the real daemon's shared extension runtime. An offline
provider emits concurrent duplicate requests, another revision, and both negative
cases. The test checks canonical publication and successful operation completion.
It restarts the daemon, reuses existing references, and creates a third review.
Changing the live script between runs must not change the restored session's
pinned implementation or revision.

Run with `dune build @test/runtest-agent_server_restart_test`.
Build the complete directory with
`dune build @test/chatml_extensibility_fixtures/x02-review/bundle` and copy
`_build/default/test/chatml_extensibility_fixtures/x02-review/` to a new workspace.
Run `chat-tui --no-config --local -file agent.chatmd` there and ask it to begin
reviews for repeated and distinct revision strings. Local transient state lasts
for that host session. Configure this prompt on a durable daemon to try restart
recovery. Interactive conversations use your model; the test command is offline.
