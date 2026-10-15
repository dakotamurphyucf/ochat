# Authoring source host (OCH-277)

`Authoring_source_host` is the host-only read and capture adapter for M5 editor
contexts. It is implemented as a separate module in `agent_server`; it is not yet
exposed over the direct, HTTP or stdio authoring protocol. Portable immutable
editor revisions and overlay closure validation belong to OCH-278 and OCH-264.

The adapter borrows `Project_source_host`'s **already admitted** configured
`external_roots`, paired with their original host prefixes. It borrows the exact
`Chatml_project_prepare.Policy` and `Limits` built at startup. It does not reopen
paths from configuration or reconstruct a second, wider source profile. A prompt
receipt's private original source root remains under `Prompt_source_owner.Scope`;
it is not an editor grant. Logical organization projects and runtime workspaces
also grant no source access.

The operator supplies a mandatory `Read_policy` callback. The default
`deny_all` policy admits no root. This callback decides access to a configured
root index for an authenticated actor; there is currently no generic
`Agent_protocol.Scope` that implicitly permits source reads. Only roots allowed
for that actor are listed. A selected root is host-incarnation-local and retains
the original actor lifetime. Opening a context, reading, browsing and capturing
recheck the original proof, the current request actor and the operator policy
for both actors.
An explicit `server.authoring_project_access` declaration can provide this policy
for selected Project contexts. It names an admitted administrative root, an exact
principal ID and authentication kind, and an exact portable project member plus
Entry/Library environment request mapped to a Standard compiler target. The
configuration is validated without opening another root. Startup then binds the
root names to the ordered anchors of one physical `Project_source_host`; the
bound policy retains only that host's comparison identity and immutable target
contracts. Installation rejects another host incarnation before opening source
or upload children, even when its path list and indices are identical. Every
later selection checks the current actor, while the source owner still checks
the original and current actors and physical root issuer. No configured root,
target name, protocol feature or equal path alone authorizes a read. Changing
these startup grants or target mappings requires restart; the config watcher
rejects a changed Server record before publication.
The context explicitly selects any additional permitted dependency roots;
unselected configured roots never enter its resolver attempt. Connection cleanup
calls `Context.close_and_wait` before the source host is closed.

The initial host capture is deliberately local-only. It calls the existing
`Chatml_project_prepare.prepare_sources` with only selected admitted anchors,
`Build_without_previous`, and **no** Git store or Git grants. A project that
needs Git acquisition is unavailable through this live host capture. An exact
previously captured Git/lock closure can use the separate immutable bundle
validation path; authoring capture never silently fetches or updates it. A
successful result retains `Chatml_project_build.t` and its exact
`Chatml_prepared_program` graph, with no live file descriptor, actor proof, run
permission or initializer execution. The source bytes are an immutable
observation, not an all-file atomic filesystem snapshot.

`list_directory` uses the selected retained root and the existing nonrecursive
`Chatml_local_source.fold_directory` operation. Every native directory entry is
charged before filtering names that cannot become portable source members; the
result contains path candidates, not proofs that files are readable. An explicit
`read_member` repeats descriptor-relative, no-follow regular-file admission and
returns exact bounded bytes. Neither operation writes a host file.

Host limits cap simultaneous contexts, attempts per context, returned manifest
rows, native directory entries per browse, bytes per explicit read and aggregate
explicit read bytes per context. The source preparation policy continues to cap
its own complete source count/bytes and work in each attempt. Attempts consume a
context allowance before work, even on failure or cancellation. A new context
starts a new allowance under current operator policy; these counters are a
host-selected lifetime bound, not a hidden global rate limit. Concurrent or
reentrant methods on one context reject `Busy`; root enumeration also admits
one active call and returns `Busy` to overlapping callers. Closing revokes admission,
cancels the active source operation and waits for its joined cleanup. The
owner's Eio switch release cancels and joins root enumeration and every retained
context; an explicit
close unregisters the release hook. Closing from the context's own active
operation or root enumeration is rejected to avoid self-join. The private
fiber-local activity stack keeps that guard through nested calls. Expected errors are finite typed
values; caller cancellation and unexpected checkpoint exceptions preserve
their original identity. Trusted operator policy callbacks are pure and
non-yielding; their exceptions also preserve identity and backtrace.

An authorized host-member document can explicitly capture its ChatMD import
closure for analysis. The current editor revision supplies the root bytes, so
unsaved edits are parsed without rereading the root file. The existing Generated
parser and source resolver discover imports, and the same retained local source
owner reads each new member descriptor-relative with no symlink following. A
loader-local cache returns the first exact bytes on repeated reads; parser read
and work limits still count those reads. Source, total bundle byte, file and
portable path/collision limits are installed before any new member is read or
retained, then the immutable bundle is validated again. The host also applies
its remaining per-context read allowance. The result pairs the genuine parser
inputs with the captured source IDs and bytes that supplied them; it retains no
owner, loader or further read capability. A missing import, malformed document,
limit or cancellation returns no partial closure. Supplied documents have no
host import grant. The language-service handler must adopt a fresh closure only
on explicit analysis or rebuild; ordinary prefix and query requests use a
previously adopted immutable closure and never reread imports. The captured
files are exact individual observations, not an atomic filesystem snapshot.
