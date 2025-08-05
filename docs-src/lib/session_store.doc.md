# `Session_store` – On-disk persistence helper

This module offers the *persistence layer* for the [`Session`](session.doc.md)
record: it decides **where** a session lives on disk, **how** snapshots are
read or written, and provides a few high-level maintenance commands that the
CLI front-ends hook into (`--reset`, `--rebuild`, …).

Unlike `Session.Io.File`, which is a *plain* serializer/deserializer operating
on an [`Eio.Path.t`](https://ocaml.github.io/eio/eio/Eio/Path/index.html),
`Session_store` manages

* directory layout (`$HOME/.ochat/sessions/<id>`),
* unique identifier generation,
* schema migrations for **legacy snapshots**,
* advisory file locking so concurrent instances never step on each other’s
  toes, and
* a small archive mechanism that keeps older snapshots around when a reset or
  rebuild is requested.

---

## Quick reference

```ocaml
module Session_store : sig
  type id   = string
  type path = Eio.Fs.dir_ty Eio.Path.t

  val base_dir     : unit -> string
  val rel_path     : id -> string

  val ensure_dir   : env:Eio_unix.Stdenv.base -> id -> path
  val path         : env:Eio_unix.Stdenv.base -> id -> path

  val load_or_create
    :  env:Eio_unix.Stdenv.base
    -> prompt_file:string
    -> ?id:id
    -> ?new_session:bool
    -> unit
    -> Session.t

  val save         : env:Eio_unix.Stdenv.base -> Session.t -> unit
  val list         : env:Eio_unix.Stdenv.base -> (id * string) list

  val reset_session
    :  env:Eio_unix.Stdenv.base
    -> id:id
    -> ?prompt_file:string
    -> ?keep_history:bool
    -> unit
    -> unit

  val rebuild_session : env:Eio_unix.Stdenv.base -> id:id -> unit -> unit
end
```

All operations expect an `Eio_unix.Stdenv.base` value – the capability that
`Eio_main.run` hands to the entry-point of your program.

---

## 1. Directory layout & identifier strategy

* **Root** – by default everything lives under
  `$HOME/.ochat/sessions` (or `./.ochat/sessions` if `HOME` is not
  defined).  Override the environment variable if you want to relocate the
  whole tree.

* **Session directory** – the sub-directory name is the *identifier* `id`.  It
  is obtained via the following rules (in order):

  | Scenario                                   | Resulting `id` |
  | ------------------------------------------ | -------------- |
  | `load_or_create ~id:"my-name"`           | `"my-name"`   |
  | `~new_session:true` (no explicit id)       | random UUID-v4 |
  | neither of the above                       | `md5(prompt_file)` |

This scheme makes sure that **repeat executions using the same prompt but no
explicit flags resume the same conversation** – extremely handy when you are
iterating on a prompt interactively.

---

## 2. Reading or creating a session – `load_or_create`

```ocaml
val load_or_create
  :  env:Eio_unix.Stdenv.base
  -> prompt_file:string
  -> ?id:id
  -> ?new_session:bool
  -> unit
  -> Session.t
```

1. Determine the identifier (`id`) using the table above.
2. If `<dir>/snapshot.bin` exists:
   * read it via `Session.Io.File.read` – **automatic migration** upgrades old
     schemas on the fly;
   * return the resulting value.
3. Otherwise:
   * create the directory (permissions `0o700`);
   * copy *prompt_file* into it as `prompt.chatmd` (best-effort);
   * return `Session.create …` initialised with the correct metadata.

Note that **nothing is written back** – saving is an explicit action.

---

## 3. Saving – `save`

`save` performs two levels of safety:

1. **Advisory lock** – exclusive creation of `snapshot.bin.lock` aborts the
   program when the file already exists, preventing corruptions from multiple
   writers.
2. **Atomic replace** – `Session.Io.File.write` dumps the data to a temp file
   and `rename(2)`s it into place.

Call the function whenever you want the on-disk state to reflect the in-memory
value (for instance on a `Ctrl-S` binding or when the UI shuts down cleanly).

---

## 4. House-keeping helpers

### 4.1 `reset_session`

Archives the current snapshot (`archive/YYYYMMDD-HHMM.snapshot.bin`) and
creates a **new** one.  By default the history is wiped – pass
`~keep_history:true` if you only want to change the prompt while keeping the
conversation log.

### 4.2 `rebuild_session`

When you edited *prompt.chatmd* manually you can call `rebuild_session` to
start fresh **and** keep a backup of the old snapshot under `archive/`.

---

## 5. Example – minimal CLI wrapper

```ocaml
open Eio.Std

let () = Eio_main.run @@ fun env ->
  (* 1. Load or initialise a session *)
  let prompt = "prompts/system.chatmd" in
  let session =
    Session_store.load_or_create ~env ~prompt_file:prompt ()
  in

  (* … talk to OpenAI, mutate [session] … *)

  (* 2. Persist the new state *)
  Session_store.save ~env session
```

---

## 6. Limitations / future work

1. **Hard-coded location** – the module respects `HOME` but nothing else.  A
   proper configuration file would be nicer.
2. **No snapshot compaction** – every `save` re-writes the full record.  Large
   histories could benefit from incremental deltas.
3. **Locking granularity** – a single byte-file lock would avoid the extra
   inode and survive crashes better than the current "create & unlink"
   approach.

---

*Happy hacking!*  
*The Ochat team*

