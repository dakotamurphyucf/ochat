# ChatMD shell runtime examples

These examples show complete use cases and the runtime behavior that matters
for security. Replace illustrative hashes and paths before use. Inspect every
manifest with `ochat shell inspect FILE -canonical` before authorizing it.

## 1. Read-only repository search

```xml
<shell_access id="search-runtime" extends="builtin:workspace-readonly@1">
  <policy default="deny" merge="replace">
    <rule id="allow-rg" action="allow">
      <all><basename value="rg"/><trusted_executable/></all>
    </rule>
  </policy>
</shell_access>

<tool name="search" type="shell" mode="fixed" runtime="search-runtime">
  <command program="rg"><arg value="--json"/></command>
  <arguments mode="required" min_count="1" max_count="20"/>
</tool>
```

`{"arguments":["TODO","lib"]}` resolves and checks `rg`, then executes one
literal argv. `{"arguments":["TODO; rm -rf /","lib"]}` searches for that
literal string; it cannot invoke `rm`. A request for Python cannot be expressed
by this fixed tool. `find -exec` is also unavailable because no `find` tool is
published and policy defaults to deny.

## 2. Development builds and tests

```xml
<shell_access id="development" extends="builtin:workspace-development@1" pipefail="true">
  <environment inherit="selected">
    <set name="PATH" value="/usr/bin:/bin"/>
    <path_env name="OPAM_SWITCH_PREFIX" suffix="bin" position="prepend" required="true"/>
  </environment>
  <policy default="ask">
    <rule id="allow-dune" action="allow"><basename value="dune"/></rule>
    <rule id="deny-opam-mutation" action="deny">
      <all><basename value="opam"/><argument value="install"/></all>
    </rule>
  </policy>
  <approvals provider="ui" unavailable="deny" scopes="once,exact_session"/>
</shell_access>

<tool name="shell" type="shell" mode="structured" runtime="development"
      rationale="required" result="structured"/>
<tool name="shell_chain" type="shell" mode="chain" runtime="development"
      rationale="required" result="structured"/>
```

`dune build && dune build @runtest` prepares the second branch only after the
first succeeds. Each pipeline stage has independent resolution and identity.
`pipefail` makes a failed stage fail its condition. `opam install pkg` matches
the hard deny and never opens approval. An unfamiliar `make generated-code`
request reaches the UI; exact-session approval applies only to that argv and
fingerprint.

## 3. Narrow Git status

```xml
<shell_access id="git-status" extends="builtin:workspace-readonly@1">
  <policy default="deny" merge="replace">
    <rule id="only-status" action="allow">
      <argv_prefix values="git,status,--short"/>
    </rule>
  </policy>
</shell_access>

<tool name="git_status" type="shell" mode="fixed" runtime="git-status">
  <command program="git"><arg value="status"/><arg value="--short"/></command>
  <arguments mode="none"/>
</tool>
```

The model receives no arguments field, so the command is exactly
`git status --short`.

## 4. Reviewed raw deployment shell

```xml
<shell_access id="deployment" cwd="${workspace}">
  <capabilities sandbox="required" network="true" child_processes="true"
                arbitrary_code="true" privilege_change="false">
    <read path="${workspace}"/><write path="${workspace}/dist"/>
  </capabilities>
  <environment inherit="selected">
    <set name="PATH" value="/usr/bin:/bin"/>
    <pass name="DEPLOY_TOKEN" required="true" secret="true"/>
  </environment>
  <policy default="deny">
    <rule id="review-zsh" action="ask"><resolved_path value="/bin/zsh"/></rule>
  </policy>
  <approvals provider="ui" unavailable="deny" scopes="once"/>
  <backends><seatbelt when="macos"/><bubblewrap when="linux"/></backends>
  <secrets><from_env name="DEPLOY_TOKEN"/></secrets>
</shell_access>

<tool name="deploy_shell" type="shell" mode="raw" runtime="deployment"
      executable="/bin/zsh" arguments_before_script='["-c"]'
      rationale="required" result="structured"/>
```

Every script receives one-time review. The token is passed but redacted from
display, output, and audit. Missing confinement fails; direct execution is not
inserted.

## 5. ChatML Python interceptor

```xml
<script id="python-policy" language="chatml" kind="shell_before_interceptor">
let initial_state = `State(0)
let on_event = fun event state ->
  let ctx = Shell.context(event) in
  if Shell.basename(ctx) != "python3" then Shell.continue()
  else Shell.rewrite(Runtime.source_path("tools/safe-python"), Shell.arguments(ctx))
</script>

<shell_access id="python-dev" extends="builtin:workspace-development@1">
  <policy default="ask">
    <rule id="safe-python" action="allow">
      <resolved_path value="${source_dir}/tools/safe-python"/>
    </rule>
  </policy>
  <interceptors>
    <interceptor id="python" phase="before" script="python-policy" failure="deny">
      <match><basename value="python3"/></match>
    </interceptor>
  </interceptors>
</shell_access>
```

The original command resolves and reaches the interceptor after hard-deny and
capability checks. The rewrite starts preparation again. `safe-python` must
resolve, fit capabilities, and match final policy. A rejecting action starts
neither executable and is audited.

## 6. Executable output sanitizer

```xml
<shell_access id="hook-worker" cwd="${source_dir}">
  <capabilities sandbox="required" network="false" child_processes="false"
                arbitrary_code="true" privilege_change="false">
    <read path="${source_dir}/hooks"/>
  </capabilities>
  <policy default="deny">
    <rule id="filter" action="allow">
      <resolved_path value="${source_dir}/hooks/filter-output"/>
    </rule>
  </policy>
  <approvals provider="none"/>
  <backends><seatbelt when="macos"/><bubblewrap when="linux"/></backends>
</shell_access>

<shell_access id="compiler" extends="builtin:workspace-development@1">
  <interceptors>
    <interceptor id="compiler-output" phase="after"
        executable="${source_dir}/hooks/filter-output" sha256="..."
        runtime="hook-worker" protocol="shell-hook-json-v1"
        timeout="2s" max_input="1MiB" max_output="256KiB" failure="deny"/>
  </interceptors>
</shell_access>
```

Compiler output is bounded, terminal-sanitized, and redacted before the hook.
The replacement is finalized again. Timeout, malformed JSON, replacement, or
excess output rejects the result rather than exposing unsanitized output.

## 7. Network API with secret redaction

```xml
<shell_access id="issue-api">
  <capabilities sandbox="required" network="true" child_processes="false"
                arbitrary_code="false" privilege_change="false"/>
  <environment inherit="selected">
    <set name="PATH" value="/usr/bin:/bin"/>
    <pass name="ISSUE_API_TOKEN" required="true" secret="true"/>
  </environment>
  <policy default="deny">
    <rule id="allow-api" action="allow">
      <all>
        <resolved_path value="/usr/bin/curl"/>
        <argument value="https://issues.example.com/api/query"/>
      </all>
    </rule>
  </policy>
  <secrets replacement="[TOKEN]"><from_env name="ISSUE_API_TOKEN"/></secrets>
  <backends><seatbelt when="macos"/><bubblewrap when="linux"/></backends>
</shell_access>

<tool name="query_issues" type="shell" mode="fixed" runtime="issue-api">
  <command program="/usr/bin/curl">
    <arg value="--fail-with-body"/><arg value="--silent"/>
    <arg value="--header"/>
    <secret_arg env="ISSUE_API_TOKEN" prefix="Authorization: Bearer "/>
    <arg value="https://issues.example.com/api/query"/>
  </command>
  <arguments mode="none"/>
</tool>
```

The manifest records a secret source/argument position but not its value. The
review and audit display `[TOKEN]`. The model cannot choose another endpoint.

## 8. Model reviewer with human fallback

```xml
<tool name="security-reviewer" agent="${source_dir}/agents/reviewer.chatmd" local/>

<script id="static-review" language="chatml" kind="shell_reviewer">
let initial_state = `State(0)
let on_event = fun event state -> Shell.defer()
</script>

<shell_access id="reviewed" extends="builtin:workspace-development@1">
  <policy default="ask">
    <rule id="deny-privileged" action="deny">
      <any><basename value="sudo"/><basename value="doas"/><basename value="su"/></any>
    </rule>
  </policy>
  <reviewers strategy="first_terminal">
    <reviewer id="static" kind="chatml" script="static-review" failure="deny"/>
    <reviewer id="model" kind="model" agent="security-reviewer" failure="deny"/>
    <reviewer id="human" kind="ui"/>
  </reviewers>
</shell_access>
```

Hard denial prevents any reviewer call. A valid `defer` reaches the next
reviewer. Malformed model output follows `failure="deny"`; it does not become
implicit defer. The reviewer model has no access to the shell being reviewed.

## 9. Compliance audit

```xml
<script id="audit-filter" language="chatml" kind="shell_audit_filter">
let initial_state = `State(0)
let on_event = fun event state -> Audit.keep(event)
</script>

<shell_access id="compliance" extends="builtin:workspace-development@1">
  <audit format="jsonl" path="${session_dir}/audit/shell.jsonl"
         content="redacted" failure="deny_start">
    <filter script="audit-filter" lifecycle="session"/>
  </audit>
</shell_access>
```

Secrets are redacted before and after the filter. If durable append is
unavailable, new commands do not start. `ochat shell audit validate` and
`replay` inspect the chain without running commands.

## 10. Shared imported runtime library

`runtime/common.chatmd`:

```xml
<shell_access id="readonly" extends="builtin:workspace-readonly@1"/>
<shell_access id="development" extends="builtin:workspace-development@1">
  <policy default="ask">
    <rule id="deny-install" action="deny">
      <any><argv_prefix values="opam,install"/><argv_prefix values="pip,install"/></any>
    </rule>
  </policy>
</shell_access>
```

Agent file:

```xml
<import src="../runtime/common.chatmd" namespace="team"/>
<tool name="search" type="shell" mode="fixed" command="rg --json"
      runtime="team:readonly"/>
<tool name="shell" type="shell" mode="structured" runtime="team:development"/>
```

Paths in the imported file use its source directory. Its digest and namespace
enter the manifest; edits invalidate old authorization.

## 11. Moderator process access

```xml
<shell_access id="classifier">
  <capabilities sandbox="required" network="false" child_processes="false"
                arbitrary_code="true" privilege_change="false">
    <read path="${source_dir}/classifiers"/>
  </capabilities>
  <resolver>
    <executable id="classifier-bin"
      path="${source_dir}/classifiers/message-classifier"
      sha256="..." trusted="true"/>
  </resolver>
  <policy default="deny">
    <rule id="classifier-only" action="allow">
      <resolved_path value="${source_dir}/classifiers/message-classifier"/>
    </rule>
  </policy>
  <approvals provider="none"/>
</shell_access>

<moderator_runtime shell_runtime="classifier"/>
```

Moderator `Process.run` uses structured argv and cannot bypass the runtime to
invoke `sh` or `curl`. The classifier is reverified before start.

## 12. Custom sandbox wrapper

```xml
<shell_access id="organization-sandbox">
  <capabilities sandbox="required" network="false" child_processes="true"
                arbitrary_code="true" privilege_change="false">
    <read path="${workspace}"/><write path="${workspace}/_build"/>
  </capabilities>
  <backends accept_declared_confinement="true">
    <external_backend id="org" executable="/opt/org/bin/sandbox-run"
        sha256="..." confinement="declared">
      <arg value="--cwd"/><cwd_value/><arg value="--"/><command_argv/>
    </external_backend>
  </backends>
  <policy default="ask"/>
</shell_access>
```

Authorization warns that ochat cannot verify the wrapper’s confinement.
Wrapper and target are fingerprinted. Cwd and command argv cannot inject
wrapper arguments because expansion is structured.

## 13. Reviewed direct local development

```xml
<shell_access id="local-direct" cwd="${workspace}">
  <capabilities sandbox="direct_unsafe" network="true" child_processes="true"
                arbitrary_code="true" privilege_change="false">
    <read path="${workspace}"/><write path="${workspace}"/>
  </capabilities>
  <backends><direct id="local"/></backends>
  <policy default="ask">
    <rule id="allow-dune" action="allow"><basename value="dune"/></rule>
    <rule id="deny-privileged" action="deny">
      <any><basename value="sudo"/><basename value="doas"/></any>
    </rule>
  </policy>
  <approvals provider="ui" unavailable="deny" scopes="once,exact_session"/>
  <audit format="jsonl" path="${session_dir}/direct-shell.jsonl"/>
</shell_access>
```

This is not confinement: roots/network are policy declarations under direct
execution. It retains ask/deny behavior, unlike the YOLO built-in.

## 14. YOLO mode

```xml
<shell_access id="yolo" extends="builtin:yolo@1"/>
<tool name="shell" type="shell" mode="structured" runtime="yolo"/>
<tool name="raw_shell" type="shell" mode="raw" runtime="yolo"
      executable="/bin/zsh" arguments_before_script='["-c"]'/>
```

YOLO uses direct execution, unrestricted host authority, allow-by-default
policy, and no command approval. It gives the model the user’s local process
privileges. Host policy may reject it. Output protection, cancellation,
reaping, executable observation, and manifest inspection still apply.

## 15. Required backend unavailable

```xml
<shell_access id="linux-only">
  <capabilities sandbox="required" network="false" child_processes="false"
                arbitrary_code="false" privilege_change="false"/>
  <backends><bubblewrap when="linux" executable="/usr/bin/bwrap"/></backends>
</shell_access>
```

On macOS, platform filtering leaves no backend. Compilation/instantiation
fails and dependent tools are not exposed. Seatbelt/direct are not inserted.

## 16. Recursive interceptor failure

```xml
<shell_access id="main">
  <interceptors>
    <interceptor id="filter" phase="after" executable="./filter"
        runtime="worker" protocol="shell-hook-json-v1" failure="deny"/>
  </interceptors>
</shell_access>
<shell_access id="worker" extends="main"/>
```

Dependency analysis reports `main -> filter -> worker -> extends main` and
refuses to instantiate either runtime.

## 17. Administrative ceiling failure

If ChatMD requests network-enabled `direct_unsafe` authority while host policy
forbids both network and direct execution, ochat reports both violations and
does not start the agent. It does not rewrite the manifest into a sandboxed,
network-disabled runtime.
