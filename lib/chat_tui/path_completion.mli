(** Interactive path autocompletion for command-mode (temporary, blocking
    implementation).

    The current design keeps **zero per-prefix state** and relies on a very
    small in-memory cache to avoid hitting the file-system on every
    keystroke.  All I/O is performed through {!Eio.Path} so the caller can run
    it inside an {!Eio_main.run} domain without breaking cooperative
    scheduling.  The module will be replaced by a fully asynchronous,
    cache-backed version once command-mode stabilises, but the public API is
    expected to remain compatible. *)

(** Autocompletion context.

    A fresh value is required for every independent input field (e.g. the
    command-line at the bottom of the TUI) because it stores the browsing
    position for cycling through matches.  It is **not** tied to a specific
    directory or prefix – those are provided on every call. *)
type t

(** [create ()] allocates a new, empty autocompletion context.  Constant-time. *)
val create : unit -> t

(** [suggestions t ~fs ~cwd ~prefix] returns the **sorted** list of file-system
    entries in directory [cwd] (or [prefix]'s directory part) whose names
    start with the fragment part of [prefix].

    Behaviour and invariants:
    • At most **25** items are returned (the caller should truncate visually if
      fewer are needed).
    • Directory entries "." and ".." are never included.
    • Each element is the **bare entry name** (no path, no trailing [/]).  The
      caller is responsible for re-assembling the full path or adding a
      suffix.
    • Results are cached per-directory for 2 seconds (TTL) to keep the call
      O(log N) after the cache is warm.

    Complexity:
    O(log N + K) comparisons where N is the number of names in the directory
    and K ≤ 25.

    The cache is shared across all [t] instances. *)
val suggestions : t -> fs:'a Eio.Path.t -> cwd:string -> prefix:string -> string list

(** [next t ~dir] iterates over the list returned by the **most recent** call
    to {!suggestions}:

    • [`Fwd] moves the cursor forward (wrap-around).
    • [`Back] moves it backward (wrap-around).

    Return [None] if there is no cached result (e.g. you did not call
    {!suggestions} yet or you invoked {!reset}).  O(1). *)
val next : t -> dir:[ `Fwd | `Back ] -> string option

(** [reset t] clears the browsing cache so that the next call to {!next}
    returns [None].  Has no effect on the shared directory cache. *)
val reset : t -> unit
