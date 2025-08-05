# `Io` – Utilities for effect-based IO  

`Io` bundles small, self-contained helpers that are used pervasively in
the Ochat code-base.  The functions are thin wrappers around
[Eio](https://github.com/ocaml-multicore/eio)’s capabilities and were
collected in a single place to avoid repeating the same boiler-plate in
every module.

The module is not meant to grow into a fully-blown standard library –
the guiding principle is *“only what is needed right now”*.


## Table of contents

1. [Filesystem helpers](#filesystem-helpers)  
2. [General utilities](#general-utilities)  
3. [Logging](#logging)  
4. [HTTP helpers – `Io.Net`](#http-helpers--ionet)  
5. [Domain-aware worker pools – `Io.Task_pool`](#domainaware-worker-pools--iotask_pool)  
6. [Example echo server / client](#example-echo-server--client)  
7. [Base-64 data-URIs](#base64-datauris)


---

## Filesystem helpers

```ocaml
val ( / )      : ('d Eio.Path.t) -> string -> 'd Eio.Path.t
val save_doc   : dir:'d Eio.Path.t -> string -> string -> unit
val append_doc : dir:'d Eio.Path.t -> string -> string -> unit
val load_doc   : dir:'d Eio.Path.t -> string -> string
val delete_doc : dir:'d Eio.Path.t -> string -> unit
val mkdir      : ?exists_ok:bool -> dir:'d Eio.Path.t -> string -> unit
val directory  : dir:'d Eio.Path.t -> string -> string list
val is_dir     : dir:'d Eio.Path.t -> string -> bool
val with_dir   : dir:'d Eio.Path.t -> (Eio.Path.t -> 'a) -> 'a
```

Most projects work with a *lot* of small files: prompts, generated code
snippets, cached completions…  All helpers take a directory capability
so that callers stay in the capability world and do not rely on global
paths.

### Example – writing and reading a file

```ocaml
Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  Io.save_doc  ~dir:cwd "hello.txt" "Hello, Io!";
  let back = Io.load_doc ~dir:cwd "hello.txt" in
  assert (back = "Hello, Io!")
```

---

## General utilities

### `to_res`

```ocaml
val to_res : (unit -> 'a) -> ('a, string) result
```

`to_res f` executes `f ()` and converts its outcome to a
`Result.t`:

* returns `Ok v` on success where `v` is the value produced by
  the function;
* returns `Error msg` when `f` raises, with `msg` containing a
  human-readable description obtained through `Eio.Exn.pp`.

The helper is particularly handy at process or fibre boundaries where
you want to surface failures without using exceptions.

### `ensure_chatmd_dir`

```ocaml
val ensure_chatmd_dir : cwd:'d Eio.Path.t -> 'd Eio.Path.t
```

Ensures the hidden directory `.chatmd` exists under `cwd` (it is
created with mode `0o700` if missing) and returns an `Eio.Path.t`
capability to it.  Several high-level modules rely on this helper to
keep their scratch files out of the way.

---


## Logging

```ocaml
val log         : dir:'d Eio.Path.t -> ?file:string -> string -> unit
val console_log : stdout:Eio.Flow.sink -> string -> unit
```

`log` simply appends a line to a file (default `./logs.txt`).  The
counterpart `console_log` writes directly to `stdout` when the caller
does not want to open `Eio.Flow` by itself.


## HTTP helpers – `Io.Net`

`Io.Net` is a very thin façade over *cohttp-eio* that removes some of
the repetitive plumbing when dealing with the HTTPS happy-path.

```ocaml
Io.Net.post Io.Net.Default
  ~net:(Eio.Stdenv.net env)
  ~host:"api.example.com"
  ~headers:(Cohttp.Header.init ())
  ~path:"/v1/endpoint"
  "{ "json": true }"
```

`post` and `get` accept a *response descriptor* that tells them how to
consume the response body:

* `Default` – return the whole body as a string.  
* `Raw f` – give full control to a user-supplied consumer.

> **Security notice**: the underlying `Tls.Config` uses a *null*
> authenticator by default.  This is acceptable for quick prototypes but
> your production code must provide a real certificate validator.


## Domain-aware worker pools – `Io.Task_pool`

`Io.Task_pool` is an example of how to combine [`Eio.Domain_manager`]
with [`Eio.Stream`] to perform CPU-bound work in parallel without
blocking the event-loop.

```ocaml
module Pool = Io.Task_pool (struct
  type input  = string
  type output = string

  let dm     = Eio.Stdenv.domain_mgr env
  let stream = Eio.Stream.create 0    (* unbounded *)
  let sw     = Eio.Switch.create ()

  let handler s = String.uppercase_ascii s
end)

let () = Pool.spawn "upper" in
assert (Pool.submit "abc" = "ABC")
```


## Example echo server / client

The modules `Io.Server`, `Io.Client` and `Io.Run_server` serve as short,
compile-tested code samples that show how to wire a TCP server and a
couple of clients with Eio.


## Base-64 data-URIs

```ocaml
val file_to_data_uri : dir:'d Eio.Path.t -> string -> string
```

`file_to_data_uri ~dir file` loads `file` from `dir`, finds a MIME type
from the file extension and returns a `data:…;base64,` URI – handy when
talking to the OpenAI `images` API.


---

## Known limitations

* The TLS configuration does **not** validate certificates – do not ship
  code using `Io.Net` as-is to production.
* All filesystem helpers load the whole file in memory – this is fine
  for the short text snippets they are intended to handle but will not
  scale to megabytes.

