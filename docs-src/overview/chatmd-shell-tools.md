# ChatMD shell tool declarations

Shell tools expose commands to a model through a named `<shell_access>`
runtime. The runtime—not the tool alone—controls resolution, capabilities,
sandboxing, policy, approval, interception, limits, secrets, and audit.

Start with `fixed` for a narrow operation or `structured` for general agent
shell access. Use `chain`, `raw`, and `script` only when their additional
semantics are required.

See also:

- [Shell runtime reference](chatmd-shell-runtime.md)
- [Security guide](../guide/chatmd-shell-security.md)
- [Worked examples](../guide/chatmd-shell-examples.md)

## Common attributes

```xml
<tool
    name="tool-name"
    type="shell"
    mode="fixed|structured|chain|raw|script"
    runtime="runtime-reference"
    description="Model-visible description"
    stdin="none|optional|required"
    rationale="none|optional|required"
    result="combined|stdout|structured"
    stream="finalized|sanitized"
    nonzero="result|error">
  ...
</tool>
```

`name`, `type="shell"`, `mode`, and `runtime` are required for the long form.
Tool names must be unique in a model request. The referenced runtime is fully
compiled and authorized before the tool is published.

Defaults:

- `stdin="none"`
- `rationale="none"` for fixed tools and `optional` for arbitrary tools
- `result="combined"`
- `stream="finalized"`
- `nonzero="result"`

`combined` returns finalized stdout followed by finalized stderr. `stdout`
returns stdout and leaves stderr as diagnostics. `structured` returns bounded,
sanitized JSON with status, channels, truncation, backend, runtime/manifest,
and per-command metadata.

`nonzero="result"` returns a safe result normally. `nonzero="error"` attaches
the same safe result to a tool error.

## Fixed command tools

Fixed tools declare the program and optional leading arguments. Model-supplied
arguments remain literal argv elements.

Compact compatibility syntax:

```xml
<tool
    name="search"
    command="rg --json"
    runtime="readonly"
    description="Search repository files"/>
```

Canonical long form:

```xml
<tool name="search" type="shell" mode="fixed" runtime="readonly">
  <command program="rg">
    <arg value="--json"/>
  </command>
  <arguments mode="required" min_count="1" max_count="20" max_item_bytes="4096"/>
</tool>
```

The default model schema is equivalent to:

```json
{
  "type": "object",
  "properties": {
    "arguments": {"type": "array", "items": {"type": "string"}},
    "rationale": {"type": "string"}
  },
  "required": ["arguments"],
  "additionalProperties": false
}
```

Control model arguments with:

```xml
<arguments mode="none"/>
<arguments mode="optional" max_count="20" max_item_bytes="4096"/>
<arguments mode="required" min_count="1" max_count="20"/>
```

Declared arguments precede model arguments. An argument such as
`TODO; rm -rf /` is one argv element; it does not execute `rm`.

### Fixed argument sources

```xml
<command program="curl">
  <arg value="--header"/>
  <secret_arg env="API_TOKEN" prefix="Authorization: Bearer "/>
  <path_arg base="workspace" path="config/query.json"/>
</command>
```

- `<arg value="..."/>` adds one literal argv element.
- `<secret_arg>` reads one environment value, prefixes it, registers it for
  redaction, and excludes the raw value from the canonical manifest.
- `<path_arg>` resolves and canonicalizes one path. `base` may name a standard
  path variable such as `workspace` or `source_dir`.

### Executable aliases

```xml
<resolver>
  <executable
      id="project-linter"
      path="${workspace}/tools/lint"
      sha256="..."
      trusted="true"/>
</resolver>

<tool name="lint" type="shell" mode="fixed" runtime="readonly">
  <command executable_ref="project-linter"/>
  <arguments mode="none"/>
</tool>
```

Aliases avoid PATH ambiguity and support hash pinning.

The compact `command` attribute is parsed by the conservative command parser
and must produce exactly one command. Pipelines and conditionals are rejected
in fixed mode. Prefer the child form for complex literal arguments.

## Structured arbitrary-command tools

```xml
<tool name="shell" type="shell" mode="structured" runtime="development"
      rationale="required" result="structured"
      description="Run a structured project command"/>
```

Model input:

```json
{
  "program": "dune",
  "arguments": ["build", "@runtest"],
  "rationale": "Verify the project after editing"
}
```

No shell parser runs. `program` and `arguments` become a structured command,
then pass through resolution, effects, capability checks, policy, approvals,
interceptors, backend selection, and audit. This is the recommended general
shell interface because argument strings cannot inject shell syntax.

## Conservative chain tools

```xml
<tool name="shell_chain" type="shell" mode="chain" runtime="development"
      rationale="required" result="structured"/>
```

Model input:

```json
{
  "command": "dune build && dune runtest | tee test.log",
  "rationale": "Build and capture test output"
}
```

Chain mode supports quoted/unquoted words, pipelines (`|`), sequence (`;`),
and conditionals (`&&`, `||`). It deliberately rejects:

- redirection and here-documents;
- `$()` and backticks;
- background jobs;
- grouping and subshells;
- variable and glob expansion;
- functions, aliases, and startup files.

Pipeline stages execute concurrently through Eio pipes. Each executable is
resolved, checked, approved, fingerprinted, and audited. Conditional branches
are prepared only when selected, so skipped branches do not prompt.
`pipefail` comes from the runtime.

Unsupported syntax is an error. It never causes an implicit `/bin/sh` or raw
shell fallback.

## Raw shell tools

```xml
<tool name="raw_shell" type="shell" mode="raw" runtime="raw-reviewed"
      executable="/bin/zsh" arguments_before_script='["-c"]'
      rationale="required" stdin="optional" result="structured"/>
```

Model input:

```json
{
  "script": "for f in lib/*.ml; do wc -l \"$f\"; done",
  "stdin": "",
  "rationale": "Count source lines"
}
```

The shell executable and argument prefix are fixed by ChatMD. The script is
one argument, not concatenated into the launch command. Raw mode is always
classified as arbitrary code, child-process capable, and conservatively
unknown. The runtime must declare those capabilities.

Raw approvals bind the shell fingerprint and complete script digest. Review
uses a bounded redacted preview; audit may record the digest rather than raw
script. Prefer required sandboxing and once-only approval.

## Script-file tools

```xml
<tool name="project_checks" type="shell" mode="script" runtime="checks"
      script="${source_dir}/scripts/check-project.sh" interpreter="/bin/sh"
      fixed_arguments='["--ci"]' verification="sha256"
      max_source_bytes="1048576" description="Run checked-in validation"/>
```

Model input contains literal `arguments`, optional stdin, and optional
rationale according to the common attributes. The script and interpreter are
resolved and fingerprinted. The script is loaded through Eio under the source
bound, hashed into the manifest/approval identity, and reverified before every
start. A changed script produces a typed failure; it is not silently accepted.

An executable script may omit an interpreter:

```xml
<tool name="project_checks" type="shell" mode="script" runtime="checks"
      script="${source_dir}/scripts/check-project" executable="true"/>
```

## Stdin

`stdin="optional"` or `required` adds a UTF-8 `stdin` field to the model
schema. The runtime checks `max_stdin` before approval and records only its
length and digest in approval/audit identity by default. Input is sent through
an Eio flow and is never interpolated into command text. For a pipeline it
reaches only the first stage.

## Rationale

Rationale appears in approval requests and audit metadata. It does not change
static policy unless a custom reviewer inspects it. It is untrusted model text,
so ochat bounds and terminal-sanitizes it.

## Structured result

`result="structured"` returns a canonical safe object similar to:

```json
{
  "request_id": "shell-42",
  "status": {"exited": 1},
  "stdout": "",
  "stderr": "File \"lib/x.ml\", line 4: error...\n",
  "stdout_truncated": false,
  "stderr_truncated": false,
  "backend": "macos-seatbelt",
  "runtime_id": "development",
  "manifest_sha256": "...",
  "commands": [
    {
      "program": "dune",
      "arguments": ["build"],
      "executable": "/opt/opam/default/bin/dune",
      "executable_sha256": "...",
      "status": {"exited": 1},
      "intercepted_by": null
    }
  ]
}
```

All strings have already passed output bounds, UTF-8 handling, terminal
sanitization, secret redaction, and configured output interceptors.

## Moderator `Process.run`

Moderator process access uses the same runtime registry:

```xml
<moderator_runtime shell_runtime="moderator-processes"/>

<script id="conversation" language="chatml" kind="moderator">
  ... Process.run("git", [|"status"; "--short"|]) ...
</script>
```

The call is structured argv and receives the same resolution, policy,
capability, approval, interception, limits, backend, output, and audit behavior
as a shell tool. Without `<moderator_runtime>`, `Process.run` is unavailable.

## Legacy command declarations

Existing declarations such as:

```xml
<tool name="rg" command="rg" description="Search files"/>
```

are desugared into a fixed shell tool and executed through the centralized
shell runtime path. Export may serialize the explicit shell form. No production
path falls back to the old direct custom-command runner.
