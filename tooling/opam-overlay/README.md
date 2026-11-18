# OChat Eio source overlay

This small opam repository packages Eio `1.3+ochat.1` for source-checkout builds on Linux and POSIX systems. It contains package metadata, one patch and upstream license notices. It does not contain a copy of the Eio library or change the backend API.

All four packages download the original [Eio 1.3 release archive](https://github.com/ocaml-multicore/eio/releases/download/v1.3/eio-1.3.tbz), SHA256 `8ed5c13e6689f31c85dca5f12762d84b8cc0042a7b07d3e464df6eb4b72b3dfc`. Core `eio` applies `eio-release-report.patch`, SHA256 `c5bfc171d29566abff2bc59617169d366bf7047fbcfc0a2177a64bd6174de017`. Matching backend package versions satisfy upstream exact-version dependencies; their sources are unchanged. The metadata preserves upstream authors, license, dependency and build definitions. See [LICENSE.eio](LICENSE.eio).

The public `Eio.Switch.run_reported` extension preserves the existing switch cancellation, worker joining and queued release order while returning an immutable report of queued release-hook failures before exception aggregation. It does not grant resource-use permission or retry failed cleanup. Calls to `on_release` that execute immediately after queue closure remain outside this report. OChat imports only the public API.

Run `node scripts/bootstrap-opam.mjs scratch/ochat-toolchain` from the checkout with opam 2.5.2 and Node.js 18 or newer. The script creates a new isolated root and compiler switch, selects this overlay only in that switch, and prints the explicit environment command. It refuses an existing destination. It does not install project dependencies. To reuse a successfully created environment, load that printed environment and run the dependency command from the project quickstart; do not rerun bootstrap against the same destination.

CI selects this overlay before resolving the Linux lock. Overlay metadata, archive/patch hashes, the bootstrap and compiler/cache identity are checked together. Installed metadata is compared with this checkout's package definitions, and qualification evidence records the result. A cached version label alone is insufficient.

This overlay is distributed with the source checkout. Installing OChat solely from a public package registry additionally requires this repository to be selected; the custom Eio version is not an upstream release. Replace the overlay with a compatible upstream release once the extension is accepted and qualified. No external fork is required for checkout builds.

After changes to the patch or metadata, run the scoped dependency tests and qualify a fresh environment, including the upstream Eio tests and their optional `mdx` dependencies, before refreshing the lock/cache provenance. A small public-API probe alone does not qualify the upstream suite.
