# `Ctx` – shared execution context

Module path: `lib/chat_response/ctx.ml`

`Ctx` is a **tiny, immutable record** that threads together the
capabilities needed by the *chat-response* sub-modules:

1. **`env`** – the Eio standard environment (usually
   `Eio_unix.Stdenv.base`).  We keep the type parameter open so that
   unit tests can inject lightweight stubs that merely respond to
   `#net`.
2. **`dir`** – root directory used when reading or writing files on
   behalf of the assistant (e.g. loading `{% include "foo.md" %}`).
3. **`tool_dir`** – current working directory applied when spawning a
   tool.  In interactive shells this tracks the user’s *actual* CWD so
   that shell-like helpers such as `!ls` behave intuitively.
4. **`cache`** – a bounded TTL-based LRU cache that memoises expensive
   operations such as HTTP fetches and nested agent calls.

```ocaml
type 'env t = {
  env      : 'env;                        (* Eio standard environment *)
  dir      : Eio.Fs.dir_ty Eio.Path.t;    (* file-system root          *)
  tool_dir : Eio.Fs.dir_ty Eio.Path.t;    (* cwd for spawned tools     *)
  cache    : Cache.t;                     (* shared memoisation store  *)
}
```

The value is **cheap to copy** (`<= 4 × native-int` words) and field
projections are O(1).

---

## Constructors

| Function | Purpose |
|----------|---------|
| `create ~env ~dir ~tool_dir ~cache` | Build an explicit context from its parts. |
| `of_env ~env ~cache` | Convenience helper that expands to:<br>`dir = Eio.Stdenv.fs  env`<br>`tool_dir = Eio.Stdenv.cwd env` |


## Accessors

| Function | Returns |
|----------|---------|
| `net      : _ t -> _ Eio.Net.t` | The network namespace (`env#net`). |
| `env      : 'e t -> 'e` | Raw access to the stored environment. |
| `dir      : _ t -> Eio.Path.t` | File-system root for local IO. |
| `tool_dir : _ t -> Eio.Path.t` | CWD used when executing tools. |
| `cache    : _ t -> Cache.t` | Shared TTL-LRU cache. |


## Quick example

```ocaml
Eio_main.run @@ fun env ->
  let cache = Cache.create ~max_size:128 () in
  let ctx = Ctx.of_env ~env ~cache in

  (* Read a local file relative to [ctx.dir] *)
  let markdown = Fetch.get ~ctx ~is_local:true "README.md" in

  (* Run an external tool in [ctx.tool_dir] *)
  let files = Tool.run ctx "ls" [ "-1" ] in

  (* Network request: uses [net ctx] *)
  let html = Fetch.get ~ctx "https://example.com" ~is_local:false in

  Format.printf "%s\n%s\n" markdown html
```


## Invariants & thread-safety

* `dir` and `tool_dir` must remain accessible for the lifetime of the
  context.  No checks are performed at construction time.
* The context itself is immutable but the **cache is not** – sharing a
  single cache between fibres may require external synchronisation
  depending on your consistency requirements.

## Limitations

* The module only provides *read-only* projections.  If you need a
  different `dir` or `tool_dir`, just call `create` again with the
  modified value.
* The record must travel explicitly; `Ctx` is purposefully not stored
  in a global to keep the codebase capability-safe.

