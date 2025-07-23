# `Mcp_transport_stdio` – newline-delimited JSON over **stdio**

`Mcp_transport_stdio` provides the simplest possible wire-transport for the
Model-Context-Protocol: the client *spawns* the server process locally and
exchanges JSON packets over the process’ `stdin` / `stdout` pipes.  Because
every modern language can read and write lines on standard I/O, this
transport is ideal for rapid prototyping, unit-tests and CLI-based tools.

---

## 1  How it works

1. `connect` creates **two pipes** using `Eio.Process.pipe` and starts the
   child process with `Eio.Process.spawn`.
2. The parent keeps the *write* end of the *stdin* pipe and the *read* end
   of the *stdout* pipe; all other FDs are closed.
3. `send` serialises a `Jsonaf.t` value to UTF-8 with
   `Jsonaf.to_string` and appends a single `\n` newline.  The complete line
   is written to the child’s *stdin* using `Eio.Flow.copy_string`.
4. `recv` blocks until it can read a full line from the child’s *stdout*
   (`Eio.Buf_read.line`) and parses it with `Jsonaf.of_string`.
5. `close` shuts both pipe ends **and** waits for the child to exit in
   order to avoid zombie processes.

Concurrency safety is achieved with two independent `Eio.Mutex.t` guards:
one for the **reader** and one for the **writer**.  Multiple fibres can call
`send` concurrently without interleaving their output.

---

## 2  Public API

The module instantiates the
[`Mcp_transport_interface.TRANSPORT`](./mcp_transport_interface.mli)
signature:

```ocaml
type t

val connect :
  ?auth:bool -> sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> string -> t

val send   : t -> Jsonaf.t -> unit
val recv   : t -> Jsonaf.t

val is_closed : t -> bool
val close     : t -> unit

exception Connection_closed
```

### 2.1  URI scheme

The `uri` parameter of `connect` must start with the **`stdio:`** scheme
followed by the *exact* command line to run:

```text
stdio:python3 mcp_server.py --model llama
```

Whitespace is URL-encoded (`%20`) to avoid shell-parsing issues.

---

## 3  Examples

### 3.1  Ping-pong

```ocaml
open Core

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let conn =
      Mcp_transport_stdio.connect
        ~sw ~env "stdio:python3 -m mcp.example_server"
    in
    Mcp_transport_stdio.send conn (`String "ping");
    match Mcp_transport_stdio.recv conn with
    | `String "pong" -> printf "✓ server responded\n"
    | other -> eprintf "unexpected: %a\n" Jsonaf.pp other;
    Mcp_transport_stdio.close conn
```

### 3.2  Using multiple fibres

```ocaml
let demo_parallel_requests server_cmd =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let conn = Mcp_transport_stdio.connect ~sw ~env server_cmd in
    Eio.Fiber.all
      [ (fun () -> Mcp_transport_stdio.send conn (`String "a"))
      ; (fun () -> Mcp_transport_stdio.send conn (`String "b"))
      ];
    Mcp_transport_stdio.close conn
```

`send` is mutex-protected, so the two fibres cannot interleave their writes
and corrupt the wire protocol.

---

## 4  Behavioural contract

* **Blocking semantics** – `send` blocks until *all* bytes of the JSON line
  have been passed to the OS.  `recv` blocks until one full line is
  available.
* **Idempotent close** – calling `close` more than once is a no-op.
* **Error surface** – once `Connection_closed` is raised (e.g. because the
  child exited), `is_closed` becomes `true` and further `send` / `recv`
  calls re-raise the same exception.

---

## 5  Known limitations

* **No authentication hooks** – the optional `auth` flag is ignored for the
  stdio transport.
* **Single multiplexed channel** – you get one request and one response
  stream.  Higher-level code must implement message correlation.
* **Large payloads** – the internal `Eio.Buf_read` buffer is capped at
  10 MiB.  Adjust the constant in `mcp_transport_stdio.ml` if you really
  need to send bigger single JSON values.

---

## 6  Extending / debugging

The transport prints any *non-JSON* line it receives from the child to the
`Debug` logger and keeps running.  Enable `EIO_TRACE=1` to get a full dump
of all system calls and fibre scheduling decisions.



