# ochat-shell-resource-runner

This installed helper applies OS resource limits, then replaces itself with the
selected executable. It is normally invoked by the shell runtime, not by an LLM.
Build it with `dune build bin/ochat_shell_resource_runner.exe` or install Ochat so
`ochat-shell-resource-runner` is discoverable. `OCHAT_SHELL_RESOURCE_RUNNER` can
select an explicit helper path; protect that executable as part of host trust.

## Interface

```text
ochat-shell-resource-runner [--cpu SECONDS] [--memory BYTES]
  [--file-size BYTES] [--open-files COUNT] -- /absolute/executable [ARG ...]
```

These are integer OS limit values, not ChatMD strings such as `2GiB`. The shell
compiler/runtime lowers declared values into this interface. The helper does not
search PATH for the final executable, parse shell syntax, or create filesystem/
network confinement. Wall/idle timeouts and output bounds are runtime controls,
not additional helper flags.

Limits use the platform's available rlimit facilities. Memory support depends on
availability of the virtual-memory resource. An unavailable resource, malformed
argument, failed limit application, or exec failure terminates rather than
pretending the limit was enforced. Successful exec inherits the target's exit
behavior. Required confinement still needs the selected Seatbelt/bubblewrap or
approved external backend; the helper alone is not a sandbox.

See [shell limits/backends](../guide/chatmd-shell-security.md) and
[host setup](../guide/chatmd-shell-host-integration.md).
