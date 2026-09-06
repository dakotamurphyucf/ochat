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
| `level` | One of Debug, Info, Warn, Error (polymorphic variants). |
| `emit ?ctx level string -> unit` | Emit a single entry. |
| `with_span ?ctx string (unit -> 'a) -> 'a` | Time the function and emit _start / _end (or _error) lines. |
| `heartbeat ~sw ~clock ~interval ~probe unit -> unit` | Fork a background fiber that calls `probe` and logs its output at regular intervals. |

An in-process `Eio.Mutex` serializes calls using this module; it is not a
cross-process file lock. Calls must execute within an Eio runtime, but the
existing channel writes themselves are blocking. This is a diagnostic logger,
not the daemon's durable security/audit store.


## Function reference

### `emit`

```
val emit : ?ctx:(string * Jsonaf.t) list -> level -> string -> unit
```

Append a JSON line to `run.log` in the process working directory. Creation mode
is 0644, subject to umask; there is no redaction. Protect the directory and do not
log secrets. Existing permissions are retained. `ctx` is concatenated before the
built-in fields: duplicate keys remain in the JSON object, so consumers may
interpret them differently. Use unique non-reserved keys rather than overriding
`ts`, `level`, `msg`, `pid`, or `domain`.

Opening, writing, serialization and cancellation can raise. No best-effort
exception suppression is implemented, and a failed write may leave a partial
line. A successful call flushes the channel but does not fsync the file.
Failures release the serialization lock without poisoning it; after repairing
the destination, later calls can succeed. A partial line is not repaired.

Example:

```ocaml
Log.emit `Warn ~ctx:[ "file", `String "/tmp/data.csv" ] "skipped";
```


### `with_span`

```
val with_span : ?ctx:(string * Jsonaf.t) list -> string -> (unit -> 'a) -> 'a
```

Helps instrument a block of code with start/end events and automatically
records elapsed wall-clock time (milliseconds). A start-log failure prevents
the callback from running; an end-log failure can replace a successful return.
If the callback raises, error logging is attempted before re-raising; an error
in that logging can mask the original exception. This is not exception-transparent.

```ocaml
let character_count text =
  Log.with_span "count_bytes" (fun () -> String.length text)
```


### `heartbeat`

```
val heartbeat :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  interval:float ->
  probe:(unit -> (string * Jsonaf.t) list) ->
  unit -> unit
```

Spawn a daemon fiber attached to the supplied switch. It calls `probe` and logs
immediately when scheduled, then sleeps for `interval` seconds and repeats.
Supply a positive finite interval; there is no local validation. Probe/logging
exceptions escape the fiber and can fail its switch. Shutdown cancels the fiber.

```ocaml
let monitor env ~sw =
  let probe () = [ "component", `String "example" ] in
  Log.heartbeat ~sw ~clock:(Eio.Stdenv.clock env) ~interval:30.0 ~probe ()
```


## Integration tips

* **Viewing logs** – use `tail -F run.log` from a separate terminal. Keep logs
  separate from protocol stdout; no configurable sink is supplied by this API.
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
