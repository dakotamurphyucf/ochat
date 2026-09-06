# Io — explicit-capability I/O helpers

[Interface](../../lib/io.mli) · [implementation](../../lib/io.ml).
Core is the standard library; these helpers use Eio for filesystem operations.
They are convenience wrappers, not the daemon's durable store or a sandbox.

## Filesystem helpers

`save_doc ~dir file text` truncates/writes; `append_doc` appends; both request
0600 for newly created files. They do not chmod existing files, atomically replace
them, fsync them, or coordinate concurrent writers. `load_doc` reads the entire
file. `delete_doc` calls unlink and raises if the file is missing.
`mkdir ?exists_ok ~dir subdir` creates parent directories with requested mode
0700. `directory` lists names and `is_dir` checks directory kind.

```ocaml
let file_roundtrip env =
  let dir = Eio.Stdenv.cwd env in
  Io.save_doc ~dir "example.txt" "hello";
  Io.append_doc ~dir "example.txt" "\n";
  assert (String.equal (Io.load_doc ~dir "example.txt") "hello\n")
```

`Io.( / )` appends a path component. `with_dir ~dir f` opens a scoped directory
capability and closes it after the callback; do not return live resources that
depend on it. `ensure_chatmd_dir ~cwd` creates/returns `cwd/.chatmd` with requested
mode 0700. This does not clean up artifacts or provide zero-artifact execution.

## General utilities

`to_res f` returns Ok or a formatted Eio exception string. It catches **all**
exceptions, including cancellation; callers requiring cancellation propagation
must not use it as their cancellation boundary. Error strings can contain
sensitive context. `run_main` wraps `Eio_main.run` and initializes the default
Mirage RNG.

## Logging

`log ~dir ?file text` appends the supplied string verbatim to `./logs.txt` by
default, requesting 0600 on creation. It does **not** add a newline or promise
whole-message atomicity across fibers. `console_log ~stdout text` copies verbatim
to the supplied sink. Neither helper redacts content.

<a id="http-helpers--ionet"></a>

## HTTP helpers – Io.Net

`Net.post` requires an owning switch; `Net.get` creates an internal switch.
`Default` consumes the whole body; `Raw f` lets the callback inspect/consume
the response while its resources remain live.

```ocaml
let post_example env =
  Eio.Switch.run (fun sw ->
    Io.Net.post Io.Net.Default
      ~net:(Eio.Stdenv.net env) ~host:"api.example.com"
      ~headers:(Cohttp.Header.init ()) ~path:"/v1/endpoint" ~sw
      {|{"json":true}|})
```

This is a placeholder endpoint, not an offline test. POST accepts explicit origins
or defaults a bare host to HTTPS. GET constructs HTTPS from its host/path.
`get_host` and `get_path` are URI helpers; the latter omits query/fragment.
`download_file` reads a whole response then uses `save_doc`.

**TLS certificates are not verified:** these wrappers install a null
authenticator. There is no authenticator argument on their public get/post API;
use a properly validating transport for untrusted networks. They do not enforce
a general response-size bound or HTTP-success status policy.

<a id="domainaware-worker-pools--iotask_pool"></a>

## Domain-aware worker pools – Io.Task_pool

`Task_pool` delegates submitted work to an Eio-owned worker domain. The queue
contains an input and a reply promise. Capacity zero means **synchronous
rendezvous**, not an unbounded queue.

```ocaml
let uppercase_example env =
  Eio.Switch.run (fun sw ->
    let module Pool = Io.Task_pool (struct
      type input = string
      type output = string
      let dm = Eio.Stdenv.domain_mgr env
      let stream = Eio.Stream.create 0
      let sw = sw
      let handler = Core.String.uppercase
    end) in
    Pool.spawn "uppercase";
    assert (String.equal (Pool.submit "abc") "ABC"))
```

`spawn` starts an owned daemon fiber/domain; `submit` waits for its promise.
This minimal demonstration is not a robust job scheduler: handlers must be
thread-safe, and handler exceptions fail the owning switch rather than yielding
a typed per-job result. Cancellation cannot preempt arbitrary CPU-only code.

<a id="example-echo-server--client"></a>

## Example echo server / client

`Server`, `Client` and `Run_server` are simple line-based networking demos.
They are not authenticated production agent transports. Use
[agent transports](../agent-server/README.md) for sessions and reconnects.

<a id="base64-datauris"></a>

## Base-64 data-URIs

`Io.Base64.file_to_data_uri ~dir file` loads a complete local file and encodes it
with a MIME prefix. The function is inside `Io.Base64`, not at the Io top level.
Encoding increases size and does not redact file contents. Supported image
extensions select a MIME type; unknown extensions default to image/jpeg.
The current helper prints filename/extension/MIME diagnostics to stdout, so it
is not a silent or privacy-preserving encoder.

## Known limitations

Whole-file/body helpers are unbounded unless their caller supplies a bound.
Plain writes, log appends and demo worker pools have none of the daemon's
transaction, retention or security contracts.
