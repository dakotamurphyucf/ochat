# Fixed Git host acquisition

This library owns a qualified stock Git 2.56.0 command profile, a private local
store, joined processes, offline raw object queries and atomic cache fill.
`chatml_git_host.mli` is the public authority and lifetime contract. Object kind,
Git hash, tree mode, package declaration and catalog validation belong to the
raw-object/capture caller. Cache presence alone does not prove a verified commit.

Host deployment inputs are trusted and stable: the selected Git/curl/TLS build,
a directory containing exactly `git`, `git-upload-pack`, `git-remote-https`, an
optional CA bundle, the private store native label/ancestors and exact local Git
repository grants. The helper directory is both nonempty `PATH` and
`GIT_EXEC_PATH`. There is no installed-Git search, dependency installation,
checkout, hook/filter execution, registry, arbitrary command API or sandbox
claim about an adversarial executable or its linked libraries. No source-owned
locator grants native authority. Platform system TLS trust is selected explicitly
when no CA bundle is supplied; ambient TLS/proxy/credential/config variables are
not inherited.

## Fixed commands and storage reservation

Initialization uses `init --bare --quiet --ref-format=files
--initial-branch=chatml --template=<owned-empty> --object-format=sha1|sha256 .`.
Local acquisition uses `fetch-pack --keep --depth=1 --no-progress <host-grant>
<full-pin>`. HTTPS uses `remote-https <route> <route>` with a fixed helper request:
`capabilities`, `option depth 1`, `option progress false`, `option object-format
true`, and `fetch <full-pin> <full-pin>`. Raw reads use
`--no-replace-objects --no-lazy-fetch cat-file --batch` with an exact hex ID on
stdin. No revision expressions or authored command fragments enter these calls.

Every command receives explicit protected configuration: `protocol.version=0`,
fetch/transfer fsck,
unpack limits, one pack/index worker, no reverse index, empty URI protocol list,
no server promisor acceptance, no credential helper or interactive credentials,
owned empty hooks directory, no cookie persistence or redirects, TLS verification
and only admitted HTTPS/file protocols. Optional private authorization is passed
only to HTTPS fetch in controlled per-command `GIT_CONFIG_COUNT`/`KEY_0`/`VALUE_0`
environment entries; it never appears in command argv or raw/init environments. HOME, XDG_CONFIG_HOME, TMPDIR, system/global config
files and all environment variables are selected from the owned workspace/profile.

For the audited profile, initialization has at most three regular payload inodes
(config, config replacement/probe families) plus the seven-byte symlink probe.
Fetch has one pack/index pair, one keep file and one shallow lock/replacement
family: at most four simultaneous payload inodes. Rename does not duplicate an
inode's payload. Strict fetch fsck selects index-pack even for an empty hostile
pack; shallow depth avoids the alternative shallow temporary-file branch. The fixed v0 negotiation has no packfile-URI section. The optional v2 downloader
is additionally absent from both sealed dispatch directories; the negative v2
fixture proves bounded rejection without further requests, rather than execution
of that downloader branch. Promisor acceptance is explicitly disabled and does
not write extra configuration. Failure and authentication retry retain the same finite stage.

With literal metadata file cap M and fetch file cap L, acquisition reserves
`max(3*M + 7, 4*M + 8*L)` before effects. The second term includes the completed
repository and one complete private cache-copy overlap. Existing cache inventory
has its own checked owner bound. Startup enumeration admits name retention against
both remaining work and managed-entry room before scanning. Retained checked
totals avoid repeated uncharged cache scans. Before workspace creation, cached
operations admit six entries (active, home, xdg, tmp, empty, config); fetched
operations admit a conservative 64-entry repository/workspace/copy overlap.
Native per-file limits apply to every inherited
Git child; no supplied cap is silently raised. This reservation bounds peak
simultaneously retained regular-file payload extents, **not** allocated filesystem
blocks, cumulative writes or an OS disk quota. Metadata, temporary HOME/TMP/config
and copied cache bytes stay within the same private managed stage.

Work admission uses conservative weighted adapter stages, not CPU instruction,
syscall or elapsed-time proof. Four units per reserved payload byte plus 1024
fixed units cover bounded metadata/inventory passes, short read/write/EOF work,
path/sort/setup/copy/cleanup stages. Each query additionally consumes command
request/output and fixed work from the same attempt ledger. A monotonic timeout
covers the entire bracket, including callback queries and prepublication checks.
`Repository.checkpoint` lets pure semantic loops poll the same live admission,
cancellation and monotonic deadline; failure poisons the attempt even when the
callback catches it. Callers remain responsible for cooperative polling; this
is not arbitrary CPU preemption. Protected joined cleanup can outlast that
operational deadline.

The fixed write-set audit follows stock Git 2.56.0 upstream sources:

- [`connect.c:243–270`](https://github.com/git/git/blob/v2.56.0/connect.c#L243) and
  [`gitprotocol-capabilities.adoc:191`](https://github.com/git/git/blob/v2.56.0/Documentation/gitprotocol-capabilities.adoc#L191)
  describe v0 object-format negotiation;
  [`t5551-http-fetch-smart.sh:667`](https://github.com/git/git/blob/v2.56.0/t/t5551-http-fetch-smart.sh#L667)
  exercises SHA256 over protocol v0.
- [`remote-curl.c:1168–1264`](https://github.com/git/git/blob/v2.56.0/remote-curl.c#L1168)
  rejects dumb shallow HTTP and invokes one fetch-pack for the request.
- [`fetch-pack.c:961–1115`](https://github.com/git/git/blob/v2.56.0/fetch-pack.c#L961)
  selects index-pack under fsck, the keep family and shallow input; its depth
  branch at [1255](https://github.com/git/git/blob/v2.56.0/fetch-pack.c#L1255)
  selects the shallow lock rather than an unrelated temporary shallow file.
- [`builtin/index-pack.c:1554–1656`](https://github.com/git/git/blob/v2.56.0/builtin/index-pack.c#L1554),
  [index creation at 2103](https://github.com/git/git/blob/v2.56.0/builtin/index-pack.c#L2103)
  and [`object-file.c`](https://github.com/git/git/blob/v2.56.0/object-file.c)
  finalize temporary pack/index names without copying their payload.
- [`promisor-remote.c:1233–1279`](https://github.com/git/git/blob/v2.56.0/promisor-remote.c#L1233)
  stops server-promisor processing under explicit `none` acceptance.

## Lifetime and publication

Native owners are allocated before acquisition, accept acquired resources before
raising runtime reentry, and close exactly once. Store release rejects admission
and joins active operations. Query fibers belong to the repository switch;
callback return with a pending query rejects the attempt and joins cleanup before
closing directory descriptors. Failure poisons the ledger, so catching a query
failure cannot authorize cache fill. Reentrant store operations reject `Busy`.

After successful caller verification, files are copied into exclusive private
files, synced and published by create-only rename. Results/registry updates are
allocated before the atomic boundary. Callback failure never publishes. After
that boundary, late cancellation or sync failure can leave verified cache bytes;
the operation never claims rollback or publishes a selected project lock/runtime.
Publication failure invalidates this store owner; release/reopen reconciles
bounded cache inventory before reuse.
Unknown stage ownership is rejected rather than recursively deleting caller data.
Cleanup can leave bounded unreferenced bytes, which cannot be reused as cache.

## Qualification

The focused tests require `OCHAT_TEST_GIT_2_56_DIRECTORY`, an explicit absolute
path containing the reviewed built Git and HTTPS helper. Missing qualification
input fails clearly. Tests use actual stock Git for SHA1/SHA256 and localhost TLS,
explicit private authorization, offline reuse, native file caps, shared output,
malformed cache rejection, query cleanup and checkpoint interruption. Finite v0
hostile responses include malformed/empty pack and excessive shallow metadata.
The empty-pack witness observes its 1072-byte index at the exact native cap and
no committed index one byte below; the requested commit remains absent, so both
attempts reject before semantic publication. The same excessive shallow response
reaches the semantic callback with a 4096-byte cap and fails under 2048, alongside
a stock control that succeeds at 2048. Independent reservation boundaries are
139263/139264/139265 bytes, and raw output is tested at N-1/N/N+1. Existing
private-storage tests cover the narrowly shared native root checks. Linux native
code is implemented. `.github/scripts/provision-git-host.sh` downloads
checksum-pinned Git 2.56.0 into disposable runner storage and prepares the fixed
helper inventory without replacing system Git. Required final milestone CI runs
the normal x64 suite and the ARM `git-native` actual Git/process/private-storage
aliases. Those Linux rows remain unqualified until that final CI is green; the
local macOS qualification uses the separately built candidate profile.
