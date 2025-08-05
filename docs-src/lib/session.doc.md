# `Session` – Persistent chat conversation state

## Overview

`Session` groups together every piece of information that the *ochat*
assistant needs in order to resume a dialogue that spans several
invocations of the program:

* the *prompt file* that boot-straps the conversation;
* the full list of messages exchanged with OpenAI (`History.t`);
* a lightweight per-session **task list** (think micro TODOs);
* an open-ended **key/value store** for feature flags or UI state;
* the root of a *virtual file system* (VFS) used by plug-ins and
  on-the-fly generated artefacts.

Serialisation uses the `bin_io` format together with a tiny *schema
version* prefix.  When the OCaml data type evolves the module keeps
backward compatibility by providing *upgrade functions* under
`Session.Legacy` and bumping `Session.current_version`.

All helpers are *pure* – actual file I/O is delegated to
`Session.Io.File` which wraps the
[`Bin_prot_utils_eio`](bin_prot_utils_eio.doc.md) primitives so that the
usual `Eio` buffering and permissions apply.

The implementation does **not** do any locking; callers are responsible
for serialising access if they share a session between concurrent
domains/fibres.

---

## Quick API reference (simplified)

```ocaml
module Session : sig
  val current_version : int

  module History : sig
    type t = Openai.Responses.Item.t list
  end

  module Task : sig
    type state = Pending | In_progress | Done
    type t
    val create : ?id:string -> ?state:state -> title:string -> unit -> t
  end

  type t

  val create
    :  ?id:string
    -> prompt_file:string
    -> ?local_prompt_copy:string
    -> ?history:History.t
    -> ?tasks:Task.t list
    -> ?kv_store:(string * string) list
    -> ?vfs_root:string
    -> unit
    -> t

  val reset              : ?prompt_file:string -> t -> t
  val reset_keep_history : ?prompt_file:string -> t -> t

  module Io : sig
    module File : sig
      val read  : Eio.Fs.dir_ty Eio.Path.t -> t
      val write : Eio.Fs.dir_ty Eio.Path.t -> t -> unit
    end
  end
end
```

---

## Detailed semantics

### Creation

```ocaml
val create
  :  ?id:string
  -> prompt_file:string
  -> ?local_prompt_copy:string
  -> ?history:History.t
  -> ?tasks:Task.t list
  -> ?kv_store:(string * string) list
  -> ?vfs_root:string
  -> unit
  -> t
```

* `id` – 32-character hexadecimal digest.  When omitted a fresh ID is
  generated from the current wall-clock time combined with PRNG bits.
* `prompt_file` – absolute or project-relative path of the file that was
  fed to the model before the very first user message.
* `local_prompt_copy` – optional *relative* path to a copy of the
  prompt inside the session directory.  Useful when the original file
  lives outside of version control.
* `history` / `tasks` / `kv_store` – initial values, defaulting to the
  empty list.
* `vfs_root` – name of the top-level directory that tools should treat
  as the root of the virtual file system; defaults to `"vfs"`.

The function is *pure*: it only constructs an OCaml value.  Persist it
explicitly via `Session.Io.File.write` if you need durability.


### Resetting a session

`reset` and `reset_keep_history` return a *copy* of the supplied record
so the original value remains usable.

| Function | Effect on history              | Prompt path update |
| -------- | ------------------------------ | ------------------ |
| `reset`  | cleared (restarts the chat)    | honour arg         |
| `reset_keep_history` | preserved           | honour arg         |

Both functions keep the *id*, *tasks*, *kv_store* and *vfs_root*
unchanged.


### Task helpers

`Task.create` mirrors `Session.create` in the way it synthesises default
values and computes a stable identifier.  The life-cycle enumeration is
deliberately minimal – callers are free to attach richer semantics via
the key/value store.


### File I/O

```ocaml
module Session.Io.File : sig
  val read  : Eio.Fs.dir_ty Eio.Path.t -> Session.t
  val write : Eio.Fs.dir_ty Eio.Path.t -> Session.t -> unit
end
```

• `write path v` serialises `v` using `Bin_prot.Utils.bin_dump` with a
  header – the file is created with `0600` permissions or truncated if
  it already exists.

• `read path` reverses the process and *automatically upgrades* the
  value through all intermediary versions until it matches the latest
  schema (the operation cannot fail as long as the input comes from a
  previous `Session.write`).

Both functions run inside an `Eio` context and therefore take a typed
[`Eio.Path.t`](https://ocaml.github.io/eio/eio/Eio/Path/index.html)
value.


---

## Examples

### 1. Start a new session and save it

```ocaml
open Eio.Std

let () = Eio_main.run @@ fun env ->
  let prompt = "prompts/system.txt" in
  let session = Session.create ~prompt_file:prompt () in

  let dir = Eio.Stdenv.cwd env in
  let snapshot = Eio.Path.(dir / "snapshot.bin") in
  Session.Io.File.write snapshot session
```

### 2. Load an old snapshot and begin a fresh chat

```ocaml
open Eio.Std

let () = Eio_main.run @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  let snap = Eio.Path.(dir / "snapshot.bin") in

  let session = Session.Io.File.read snap in

  (* Forget previous messages but keep bookkeeping info *)
  let session = Session.reset ~prompt_file:"prompts/v2.txt" session in

  Session.Io.File.write snap session
```

---

## Limitations

1. **Concurrency** – the module is agnostic to multi-threading.  Protect
   access with a mutex if you plan to share a `Session.t` between
   domains.
2. **Forward compatibility** – upgrading works only *forwards* (old →
   new).  Downgrading a snapshot created by a newer binary is *not*
   supported.
3. **Large histories** – everything is kept in memory; very long chats
   will increase RAM usage proportionally.

---

*Module version&nbsp;>=* `current_version` **`%= {Session.current_version}`**.

