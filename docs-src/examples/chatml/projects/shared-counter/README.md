# A captured project with one shared counter

This project is exercised through the public host preparation, graph compiler and
bounded execution APIs by `test/chatml_project_execution`. It has no network or
model dependency. The host supplies an opened directory anchor; submitting a
ChatML string alone never discovers this project.

```text
shared-counter/
├── chatml.project.json
├── src/
│   ├── main.chatml
│   ├── left.chatml
│   └── ignored.chatml           # explicitly excluded; deliberately invalid
├── branches/right.chatml       # explicit Right implementation override
├── variants/right.chatml       # alternate environment selects this as Right
└── vendor/counter/             # reusable Local dependency, two aliases
    ├── chatml.project.json
    └── src/
        ├── common.chatml
        └── common.chatmli      # checked public next/reveal; cell stays private
```

The root's `counter` and `counter_alias` dependencies select the same directory
and declaration. Their effective namespace and own environment agree, so native
preparation captures one dependency catalog. The `app` environment binds `Alias`
to `counter_alias.Common`; each branch's whole replacement environment binds
`Common` to `counter.Common`. The counter package selects its own `empty`
environment. An importer does not replace that package's scope.

```text
Main ──Left──────┐
  │             │
  ├────Right────┼── Common (one private mutable cell)
  │             │
  └────Alias────┘
```

`open Alias` exposes the checked public functions. Main's initializer consumes
counter value 1. The call then observes 2 through Left, 3 through Right, 4 through
the open alias, and 4 through reveal: its JSON result is `"13"`. Each independent
standalone run starts with a fresh counter. The explicit `alternate` environment
binds Right to Alternate instead, adding 100 and returning `"113"`.

The entry `src/main.chatml` is also a discovered catalog definition. It retains
one source/module identity and initializes once. The `to_string` initializers
are harmless sentinels: the integration fixture observes the real native calls
in dependency-first order, without changing their types or results. A separate
fixture compiles a divide-by-zero initializer successfully, then proves that
actual execution rejects it; graph checking does not execute initializers.

## Host preparation and execution

The host opens the example directory through Eio and creates a
`Chatml_local_roots.Anchor` with its chosen `/project` label. A
`Chatml_project_prepare.Host` retains that admitted anchor, with no Git store or
grants needed here. Keep the opened directory alive until preparation returns.
Host limits, the empty admitted tool registry and both policies are explicit;
configuration cannot grant native access or raise those limits.

The following is the actual API sequence used by the fixture. `host`, `policy`,
`limits` and `tools` are already admitted host values, and `request` is created
with Local locator `/project`, project `chatml.project.json`, selection
`Entry { name = "main"; environment = None }`, entry `Declared`, the authentic
One_off_v1 contract and explicit logical lock ID `selected.lock.json`.

```ocaml
let run_project host request ~env ~tools ~policy ~limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind captured =
    Chatml_project_prepare.prepare host request
      ~tools ~policy ~limits ~clock:env#mono_clock ~checkpoint
    |> Result.map_error ~f:(fun error -> `Preparation error)
  in
  let build = Chatml_project_prepare.build captured in
  let%bind program =
    Chatml_compilation.compile_prepared_with_policy
      (Chatml_project_build.prepared build)
      ~policy:(Bounded Chatml_compilation.default_limits)
      ~max_dependency_depth:32 ~env ~target:One_off_v1 ()
    |> Result.map_error ~f:(fun error -> `Compilation error)
  in
  Chatml_execution.run_prepared program ~env
    ~config:(Chatml_host_runtime.default_runtime_config
               ~surface:Chatml.Chatml_extension_surface.one_off_v1 ())
    ~entrypoint:"main"
    ~arguments:[Chatml.Chatml_lang.VVariant ("Null", [])] ()
  |> Result.map_error ~f:(fun error -> `Execution error)
```

This preserves each API's typed error domain. The executable fixture follows
this sequence and checks the actual results. There is no additional project
interpreter or automatic ChatMD loader in this example.

Use `environment = Some "alternate"` in the entry selection to choose the alternate
scope. A file's `file_environments` entry replaces its whole environment, including
the companion's independently selected empty scope.

## Fresh capture, retained code and locks

An independent Local preparation reads current files. Changing the counter's
initial cell from 0 to 10 changes a new run's result from `"13"` to `"53"`.
An already prepared or queued graph retains the earlier exact bytes. The fixture
removes every original source directory, restores the old selected lock plus its
other exact captures, then compiles and runs that graph with result `"13"`.

`Chatml_project_build.restore` receives the exact lock separately and all other
captures once. It verifies the complete inventory and selected metadata; it never
reads files or uses a cache/provider fallback. Runtime permission and tool
installation still use the current host's admission. Durable ChatMD custody and
child routing consume this boundary separately.

`prepare_and_publish` is available when the host also supplies an owned exclusive
publication target. It exposes a new selection only after complete preparation
and confirmed replacement. Failure after writer entry leaves the publication
owner uncertain; the host must not claim the old selection is current.

## Reusing a pinned Git catalog

A host may select the same reusable package through Git instead of Local. Replace
the dependency's source with a real admitted repository and its exact full commit:

```json
{"kind":"git","repository":"https://example.invalid/counter.git",
 "object_format":"sha1","commit":"<actual 40 lowercase hexadecimal characters>"}
```

This illustrates the wire shape; the placeholder is deliberately not a usable
pin and the included runnable project uses Local. SHA256 repositories require
`object_format: "sha256"` and 64 lowercase hexadecimal characters. Neither branch,
tag nor HEAD resolves implicitly. The host supplies a qualified helper profile,
store and exact repository grant; a source URL creates no network or credential
authority. Preparation runs on the selected runtime host, whose opened Local
anchors and Git grants determine available sources.

Initial_prepare accepts an authored full pin. Ordinary Build takes the prior
exact lock, refreshes Local files and preserves selected Git code. Changing Git
code selection requires explicit Update; a usable retained cache supports offline
queries of the already pinned objects. Requirements in a selected own environment
apply to every catalog independently. `requires.targets`, compiler family or an
exact contract hash can reject a new role even when its source is present.

Actual Git acquisition, offline reuse, nested prefetch gates, bounds, cancellation
and native authority qualification live in the corresponding host suites. This
example's execution suite adds the complete Local prepare/compile/run path rather
than simulating Git provenance. See the [declaration format](../../../../../lib/chatml_project/FORMAT.md)
and `lib/chatml_project_prepare/README.md` for selection and publication contracts.
