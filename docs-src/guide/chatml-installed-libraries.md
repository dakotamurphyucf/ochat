# Installed ChatML libraries

The installed `ochat` command carries reviewed ordinary ChatML source trees. It
can list their identities, show exact member bytes, and copy a complete tree into
a new directory. These operations need no repository checkout, provider account
or network request.

```sh
ochat libraries list
ochat libraries show lifecycle
ochat libraries show lifecycle -member examples/libraries/lifecycle/src/lifecycle.chatmli
ochat libraries export lifecycle -parent /existing/workspace -directory lifecycle-copy
```

`list` reports each actual installed resource ID, content digest, entry and title.
`show` emits a structural JSON inventory with portable member paths, exact SHA-256
hashes and byte lengths. Its project summaries come from the actual included
`chatml.project.json` declarations: namespaces, required tool names, environment
names and compiler requirements. `show -member` emits that member's exact bytes
without an added newline. Unknown resources and members fail; they never fall
back to a file, URL or newer version.

Export requires an explicit existing parent and one absent child name. The
example creates `/existing/workspace/lifecycle-copy`; its authored entry is
`examples/learning/lifecycle-composition/agent.chatmd` beneath that directory.
The tree preserves the ordinary relative Local dependencies among the pure
Lifecycle project, moderator-only Lifecycle_run adapter, example application and
separate Counter/Review component projects. Copy the complete tree before
selecting a module or configuring the example. Project preparation uses the
existing Local/Git resolver and current host admission; installed source lookup
adds no new dependency kind or implicit installation search path.

The default preparation anchor is the configured prompt's own directory. The
shared `examples/libraries` directory is a sibling outside that anchor, so the
host must explicitly admit it. For this export, add the following field to the
server configuration's `server` block before starting the daemon:

```lisp
(project_sources
  ((external_roots
    (/existing/workspace/lifecycle-copy/examples/libraries))))
```

Keep the prompt configured at the exported entry path above. The component
projects remain beneath that prompt's directory; the external grant covers only
the two shared library projects. Without the grant, preparation reports the
prompt unavailable even though all exported files exist. This administrative
source-preparation grant does not grant authoring-tool reads, register tools or
admit a workflow run. The full server-configuration
reference documents the distinct authoring access rules. Copying sources never adds this grant
automatically.

Export is a source copy. It does not prepare or run the example, register native
tools or create a workflow run. The example requires genuine actor-owned
`Workflow` startup and callback scopes; a host without those rejects native Run
requests. See [lifecycle composition](chatml-lifecycle-composition.md) and
[scoped run decisions](chatml-authoring-runs.md). Feature wrappers contribute their
actual sources to this same inventory when implemented.

Existing files, directories and symlinks at the destination are rejected. The
exporter creates directories with mode 0700 and files with mode 0600, retains
admitted directory descriptors, and creates each file exclusively without
truncation. It checks directory entry identity before and after opening and
checks retained entries again before reporting success. This binds writes to the
observed directory identity; it does not promise protection against privileged
namespace changes after the checked boundary.

Success follows all complete writes, file/directory syncs and identity checks.
An IO failure after destination creation reports an incomplete export and exits
with an error. Cancellation also exits without success and may leave a partial
tree. The exporter does not recursively remove that tree or overwrite it on
retry. Inspect or remove the partial destination explicitly, or select another
absent name. Export does not provide crash-atomic publication of a whole tree.

The linked inventory enforces resource, member, byte, metadata and work limits.
Paths are validated portable relative members; ambiguous case or file/directory
prefix collisions are rejected. Tree identity depends on exact selected content,
not the export location. Updating OChat changes new explicit exports; it does not
rewrite an earlier exported tree or a saved session. Program restoration uses
its captured sources and selected lock rather than newly installed bytes.
