# Build & installation troubleshooting

This project is a normal `opam` / `dune` workspace and should build on any
supported OCaml version with a working C toolchain. In a few environments,
notably Apple Silicon macOS, some transitive dependencies can still be
fragile. This page collects **known workarounds** so you do not have to hunt
through issue trackers when a fresh clone fails to build.

The list below is intentionally short and pragmatic; if you hit a different
problem, please check the upstream project first and then consider sending a
PR with an additional note here.

---

## Owl / OpenBLAS on Apple Silicon (macOS arm64)

Some parts of the search / vector‑DB stack depend on
[Owl](https://github.com/owlbarn/owl), which in turn uses OpenBLAS. On
Apple Silicon machines (M1/M2/M3) the default opam package can occasionally
fail to build or link due to OpenBLAS configuration issues.

Symptoms typically include build errors mentioning BLAS symbols or
architecture mismatches during `opam install . --deps-only` or `dune build`.

One workaround, adapted from
<https://github.com/owlbarn/owl/issues/597#issuecomment-1119470934>, is to
pin an Apple‑Silicon‑friendly Owl build and ensure `pkg-config` can locate
your OpenBLAS installation:

```sh
opam pin -n git+https://github.com/mseri/owl.git#arm64 --with-version=1.1.0
PKG_CONFIG_PATH="/opt/homebrew/opt/openblas/lib/pkgconfig" opam install owl.1.1.0
```

Notes:

- The `PKG_CONFIG_PATH` above assumes OpenBLAS is installed via Homebrew
  under `/opt/homebrew`. If you installed it elsewhere, adjust the path
  accordingly.
- The exact Owl version or pin target may change over time; if the commands
  above stop working, please refer to the latest guidance in the
  [upstream Owl issue tracker](https://github.com/owlbarn/owl/issues).
- Once Owl and its BLAS dependencies build successfully in your switch,
  `dune build` for this project should proceed normally.

