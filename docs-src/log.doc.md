# `Log` – Minimal Structured Logger

This library provides a *tiny* yet surprisingly useful logger that writes
a single JSON object per line to a file (`run.log`).  The implementation
is fewer than 100 LoC but is good enough for CLI tools and experiments
where you want structured logs without pulling in heavy-weight
dependencies such as `logs`, `ocaml-lwt-log`, `ppx_log` or `opentelemetry`.

```text
{"ts": 1.686947959e9, "level": "INFO", "msg": "starting", "pid": 42342, "domain": 0}
{"ts": 1.686947960e9, "level": "DEBUG", "msg": "parse_start", "pid": 42342, "domain": 0}
{"ts": 1.686947961e9, "level": "DEBUG", "msg": "parse_end", "duration_ms": 37.5, "pid": 42342, "domain": 0}
```

Because each line is valid JSON you can pipe the output directly to tools
such as `jq`, `gron`, `lnav` or ingest it into Loki/Elasticsearch without
an extra formatting step.


## API Overview

| Value | Description |
|-------|-------------|
| `type level = [ 7Debug | 7Info | 7Warn | 7Error ]` | Log severity. |
| `emit ?ctx level string -> unit` | Emit a single entry. |
| `with_span ?ctx string (unit -> 'a) -> 'a` | Time the function and emit _start / _end (or _error) lines. |
| `heartbeat ~sw ~clock ~interval ~probe unit -> unit` | Fork a background fiber that calls `probe` and logs its output at regular intervals. |

All functions are thread-safe – a global `Eio.Mutex` guarantees that
entries coming from different domains or fibers never interleave within
a line.


## Function reference

### `emit`

```
val emit : ?ctx:(string * Jsonaf.t) list -> level -> string -> unit
```

Write a single log line. `ctx` is merged into the object **before** the
built-in fields so you can override them if needed.  The invariant is
that exactly one line is appended to *run.log* per call and the function
never raises.

Example:

```ocaml
Log.emit `Warn ~ctx:[ "file", `String "/tmp/data.csv" ] "skipped";
```


### `with_span`

```
val with_span : ?ctx:(string * Jsonaf.t) list -> string -> (unit -> 'a) -> 'a
```

Helps instrument a block of code with start/end events and automatically
records the duration (in milliseconds).  The function is exception-safe –
_end_ is logged only on success, _error_ otherwise.

```ocaml
let data =
  Log.with_span "load_and_parse" (fun () ->
    let raw = In_channel.read_all path in
    Parser.parse raw)
```


### `heartbeat`

```
val heartbeat :
  sw:Eio.Switch.t ->
  clock:Eio.Time.clock ->
  interval:float ->
  probe:(unit -> (string * Jsonaf.t) list) ->
  unit -> unit
```

Spawns a daemon fiber (attached to the provided switch) that sleeps for
`interval` seconds and then calls `probe` to obtain extra context before
logging the message *heartbeat* at `Info` level.

```ocaml
let () =
  Eio.Switch.run (fun sw ->
    let probe () =
      [ "rss", `Int (Unix.getpid () |> Ps.resident_set_size) ] in
    Log.heartbeat ~sw ~clock:Eio.Stdenv.clock ~interval:30.0 ~probe ())
```


## Integration tips

* **Streaming to stdout** – If you prefer your logs on stderr, use `tail
  -F run.log >&2` or symlink `/dev/fd/2`.  Writing to a file was chosen
  so that tooling can read partial lines while the program is running.
* **Rotation** – `Log` does *not* handle rotation.  Use `logrotate` or a
  container runtime feature.  The logger re-opens the file on every call
  so `tail -F` will follow across rotations.
* **Timestamps** – Unix epoch floats are easy to sort and compare but
  they lack readability.  You can render them with `date -d @<ts>` or in
  `jq`: `jq '.ts |= todate' run.log`.


## Limitations

* No filtering – the code always writes regardless of level.  Add your
  own conditional around `emit` if that matters.
* No async back-pressure – writes are synchronous; if the filesystem
  stalls your program will stall.
* The global mutex may become a contention point in heavily concurrent
  applications, though in practice the cost is usually dwarfed by the
  kernel write.


## Contributing

The module is intentionally simple.  Pull requests are welcome but please
keep the spirit of _tiny & dependency-free_.

