# `Ctx` — shared execution context

`lib/chat_response/ctx.ml`

This module defines a **minimally-typed record** that bundles together the
few capabilities needed by the Chat GPT driver stack.  Passing a single
value around keeps function signatures short and makes it possible to
swap implementations in tests.

---

## Type

```ocaml
type 'env t = {
  env   : 'env;                               (* Eio standard environment *)
  dir   : Eio.Fs.dir_ty Eio.Path.t;           (* base directory for IO  *)
  cache : Cache.t;                            (* memoised HTTP / agent  *)
}
```

*The polymorphic `'env` is usually [`Eio_unix.Stdenv.base`] but anything
that responds to `#net` will compile.*

### Invariants

* `dir` must stay accessible for the lifetime of the context.
* `cache` is mutable and **not thread-safe** — either use a private
  context per fibre or synchronise externally.

## Constructors

| Function | Purpose |
|----------|---------|
| `create ~env ~dir ~cache` | Build an explicit context. |
| `of_env ~env ~cache`      | Convenience helper that uses
  `Eio.Stdenv.fs env` as `dir`. |

## Accessors

* `net   : _ t -> _ Eio.Net.t` – Network namespace (shortcut for `env#net`).
* `env   : _ t -> 'env`       – Raw access to the stored environment.
* `dir   : _ t -> Eio.Path.t` – Filesystem root used by `Fetch` and friends.
* `cache : _ t -> Cache.t`    – Shared TTL-LRU store.

All accessors are O(1) field projections.

## Usage example

```ocaml
Eio_main.run @@ fun env ->
  let cache = Cache.create ~max_size:128 () in
  let ctx = Ctx.of_env ~env ~cache in
  let response = Fetch.get ~ctx "https://example.com" ~is_local:false in
  print_endline response
```

## Limitations / notes

* The module deliberately exposes only read-only projections.  If you
  need to change a field, build a new context instead.
* Sharing the same `Cache.t` between independent fibres may require
  additional locking if you rely on strong consistency.


