# `bin/eio_get` – SSE client for the `/mcp` endpoint

`eio_get` is a tiny command-line program that demonstrates how to stream
Server-Sent Events (SSE) with the [`Eio`](https://github.com/ocaml-multicore/eio)
and [`Piaf`](https://github.com/anmonteiro/piaf) libraries.  It opens a
persistent HTTP connection to the provided host, subscribes to the
`/mcp` path, and prints each JSON message it receives.

```text
$ ochat eio-get http://localhost:8080
{"role":"assistant","message":"hello"}
{"role":"assistant","message":"how can I help?"}
```

## High-level overview

1. **Logging** – [`setup_log`](#val-setup_log) configures coloured log
   output via `Logs` and `Fmt`.
2. **Connection** – [`request`](#val-request) creates a
   `Piaf.Client.t`, enabling redirect following and allowing insecure
   TLS certificates so the executable can be used against local dev
   servers.
3. **Streaming** – once the `GET /mcp` request is issued, the response
   body is copied into an [`Eio`](https://eio.trustedlogic.net/) pipe.
   A helper fibre feeds the pipe so the main fibre can run an
   [`Eio.Buf_read`](https://eio.trustedlogic.net/api/Eio/Buf_read/)
   parser.
4. **Parsing events** – events are separated by an empty line
   (standard SSE framing).  Within each event only lines that start
   with `data: ` are kept.  Payloads equal to `data: [DONE]` mark the
   end of the stream and are ignored.
5. **Output** – every JSON payload is parsed with
   [`Jsonaf.parse`](https://ocaml.janestreet.com/ocaml-core/latest/doc/jsonaf/) and
   pretty-printed to stdout.

## API reference

### `setup_log`

```ocaml
val setup_log : ?style_renderer:Fmt_tty.style_renderer -> Logs.level option -> unit
```

Initialise the global `Logs` reporter and set the minimum log level.

### `request`

```ocaml
val request :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  string ->
  (unit, Piaf.Error.t) result
```

Open a connection to `host` and consume the SSE stream produced by
`GET /mcp`.  The function blocks until the stream finishes and returns
`Ok ()`, or propagates the first `Piaf` error it encounters.

### Program entry-point

The `main` block simply:

1. Calls `setup_log` with `Debug` level.
2. Reads the positional `HOST` argument from the command line.
3. Runs `Eio_main.run` and delegates work to `request` within a fresh
   `Eio.Switch`.

## Usage

```
$ ochat eio-get HOST

# Example
$ ochat eio-get https://example.com
```

`HOST` must include the URI scheme (`http` or `https`) and optional
port number.  The program adds `/mcp` automatically.

## Limitations & future work

* Only the first `data:` field of each event is read; multi-line data
  blocks are concatenated without any delimiter.
* The client ignores the SSE `event:` and `id:` fields.
* TLS verification is disabled when the URI scheme is `https`.
* No reconnection strategy – if the server closes the stream the
  program exits.

Pull requests improving any of the above are welcome.

