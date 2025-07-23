# Oauth2_server_storage – in-memory OAuth 2.0 token store

`Oauth2_server_storage` keeps track of the **access tokens** issued by a
simple OAuth 2.0 Authorisation Server.  It records each token’s absolute
expiration time and offers two O(1) operations:

| Function        | Purpose                                               |
|-----------------|-------------------------------------------------------|
| `insert`        | Register a freshly issued token.                      |
| `find_valid`    | Validate a token and lazily evict it once expired.    |

Internally the module relies on a single `String.Table` from Jane Street’s
*Core* standard library.  All data therefore live **exclusively in the current
OCaml process** – there is no persistence, cross-process visibility or
thread-safety.  The store is intended for

* unit or integration tests,
* demo servers,
* exploratory command-line tools.

It is **not** suitable for production systems that require high availability,
auditing, revocation lists or horizontal scaling.

---

## 1  API at a glance

```ocaml
val insert     : Oauth2_server_types.Token.t -> unit
val find_valid : string -> bool
val pp_table   : Format.formatter -> unit -> unit  (* debugging *)
```

### 1.1  `insert`

```ocaml
insert token
```

Adds `token.access_token` to the store and remembers that it expires after
`token.expires_in` seconds.  If the key already exists the old entry is
silently replaced.

### 1.2  `find_valid`

```ocaml
find_valid "abc123"  (* → true / false *)
```

Returns `true` when the given access-token is present *and* its expiry lies in
the future.  When the token is found but expired the function removes it first
and then returns `false`.

### 1.3  `pp_table`

Formats the whole table – one token per line – for interactive debugging.

---

## 2  Usage example

```ocaml
open Core

let () =
  (* Produce a dummy token that lives for three seconds. *)
  let now = Core_unix.gettimeofday () in
  let token : Oauth2_server_types.Token.t =
    { access_token = "abc123"
    ; token_type   = "Bearer"
    ; expires_in   = 3
    ; obtained_at  = now
    }
  in

  Oauth2_server_storage.insert token;

  assert (Oauth2_server_storage.find_valid "abc123");

  (* Wait until it expires … *)
  Core_unix.sleepf 3.1;

  assert (not (Oauth2_server_storage.find_valid "abc123"));
  printf "Token expired as expected!\n"
```

---

## 3  Design notes

1. **Eager eviction** – expired entries are removed on every lookup; this keeps
   the hot set small without a background sweeper thread.
2. **Clock source** – the implementation relies on the system clock via
   `Core_unix.gettimeofday`.  If the wall clock can jump backwards you may want
   to replace it with a monotonic clock.

---

## 4  Known limitations

1. No persistence or distribution – single-process only.
2. Not thread-safe – callers must serialise access when using domains/fibres.
3. Memory grows until tokens expire; issuing millions of long-lived tokens may
   impair performance.

