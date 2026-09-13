# Child process setup

`Shell_access` applies optional OS limits and private-channel descriptor cleanup
directly during process spawning. The setup code is linked into the host; it does
not invoke a companion executable, start another OCaml runtime, or execute Ochat
again. Ordinary commands without OS limits or a private channel use the host's
existing Eio process manager.

## Limits and confinement

Configured CPU time, virtual memory, file size, and open-file limits set both the
soft and hard OS bounds in the child. They default to unset. Negative,
unrepresentable, unsupported, or unenforceable limits reject execution. The
limits apply before executing the sandbox launcher, so backend startup consumes
the same budget as the command. External backend templates can still forward
`resource_limit_args` to their own wrapper; that wrapper cannot raise the inherited
hard bounds. Wall/idle deadlines and output limits remain Eio runtime controls.

These limits do not change the parent process or other concurrent children.
They do not reserve hardware capacity: concurrent workloads still share CPU,
memory, and storage. They are not filesystem or network confinement. Required
confinement still needs the selected Seatbelt, bubblewrap, or approved external
backend.

Bubblewrap is resolved through the host's `PATH` to a canonical executable before
the child changes working directory. The child's selected environment does not
choose the sandbox launcher; explicit `execve` receives the resolved path.

## Private channels and process lifetime

Private channels require a supported verified backend. After mapping standard
streams and request/response pipes onto descriptors 0–4, child setup closes all
unrelated descriptors above 4. It preserves only a close-on-exec spawn-error pipe
until successful exec. Cleanup failures prevent execution. Bubblewrap receives
`--preserve-fds 2` to carry descriptors 3/4 into the confined command. No descriptor
flags or open handles are changed in the parent or another session.

Eio switches own child cancellation and reaping. Each process has its own PID,
exit promise, and lock; signaling cannot race reaping into signaling a reused PID.
Waiters use Eio's shared SIGCHLD condition and reap only their own child. Cancelling
one invocation leaves other invocations active. After fork, setup and Eio's
descriptor/cwd/exec actions run in C without OCaml callbacks or allocation.

## Implementation and portability

The implementation requires Eio 1.3 or newer and uses its private Unix fork-action
interface, so Eio upgrades must include native process integration checks. Linux
cleanup uses `close_range`
with a `/proc/self/fd` enumeration fallback. macOS uses the public
`proc_pidinfo(PROC_PIDLISTFDS)` interface with a fixed stack buffer, closing each
batch and repeating until all unrelated descriptors are gone. Apple's
[userspace wrapper](https://github.com/apple-oss-distributions/xnu/blob/main/libsyscall/wrappers/libproc/libproc.c)
invokes the kernel without userspace allocation; the
[kernel implementation](https://github.com/apple-oss-distributions/xnu/blob/main/bsd/kern/proc_info.c)
supports listing a prefix into a bounded buffer. Run the private-channel and
resource-limit integration checks on each target platform; a configured CI job
alone is not execution evidence.

The former `ochat-shell-resource-runner` executable and
`OCHAT_SHELL_RESOURCE_RUNNER` setting have been removed. Embeddings no longer pass
`resource_runner` to shell host or executor configuration. Runtime tool-resource
fingerprints advance to v2 because that executable is no longer part of the
execution identity; previously stored identities require revalidation.

See [shell limits/backends](../../guide/chatmd-shell-security.md),
[host setup](../../guide/chatmd-shell-host-integration.md), and
[helper protocol](../../bin/ochat_agent_helper.doc.md).
