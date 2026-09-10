# X03: background shell work with a later conversation result

This bundle is an offline acceptance fixture for the qualified extensibility-v1
daemon. Public authoring availability remains gated. Keep the ChatMD, ChatML,
schema and shell files together.

`begin_work` starts one owned `fixture_work` job and immediately returns its job ID
and Pending acknowledgement. The moderator retains the job-to-invocation mapping
and can process another input while the shell waits for `fixture-work.release`.
The release file is a fixture barrier, not a production scheduling mechanism.

After the job's terminal result is saved, the runtime delivers a scoped
`background_job_completed` data event. The script reads the result through
`Job.read_result`, then publishes a notification referencing both the original
invocation and job. Successful work requests one follow-up model turn. Failures,
cancellation and expiry publish data with No_wake; they do not silently restart
a stopped session. An authorized start can resume eligible delivery.

`background_shell_tests.ml` runs the actual shell process with a fake provider.
It proves that the initial acknowledgement precedes the later notification,
another input is answered while the job runs, success produces one notification
and one follow-up request, and cancellation reaps the real process. The shell's
external marker remains after cancellation; the runtime does not pretend to undo
completed effects. Restart retains the original attempt without reexecution.

`background_notification_failure_tests.ml` deliberately runs a shell effect inside
the completion handler and then fails. The original result and failed-event receipt
survive. New input and runtime reload do not repeat the effect or fabricate a
successful notification. The blocked subsequent operation is recorded explicitly.

Generic event delivery also has actor tests for exact source/result binding,
failed-save rollback, stale/forged attempts, duplicate claims and revocation of
the current tool selection before the compiled handler observes its result.
