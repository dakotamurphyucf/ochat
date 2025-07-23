(** Multi-file patch application (OCaml port of OpenAI’s reference
    «apply_patch» helper).

    This module parses patch descriptions in the “ChatGPT diff”
    flavour and applies them to an arbitrary workspace via
    user-provided callback functions.  The implementation is a
    near-literal rewrite of the reference Python/TypeScript code into
    OCaml with some additional Unicode–canonicalisation tweaks.  It is
    completely side-effect free apart from the functions you pass in –
    *all* I/O happens through those callbacks.  Consequently the
    module works equally well on a real file-system, an in-memory map
    (see {!test/apply_patch_test.ml}), or any other storage backend.

    {1 Patch format}

    The expected syntax is a simplified, but multi-file capable, diff
    inspired by `git apply`.  A patch starts with

    {v
    *** Begin Patch
    v}

    and must be terminated by

    {v
    *** End Patch
    v}

    In between, a sequence of *file sections* follows.  The
    declarative keywords recognised are:

    • {[*** Add File: <path>]}
    • {[*** Delete File: <path>]}
    • {[*** Update File: <path>]}
    • (optional) {[*** Move to: <new-path>]} – only valid directly
      after an [Update File] line.

    An *update* section contains one or more unified-diff hunks marked
    by `@@` pairs.  Context matching is fuzzy: the algorithm tolerates
    trailing whitespace differences, ASCII/Unicode punctuation
    mismatches (e.g. fancy quotes vs straight quotes), and can search the whole
    file if the exact line numbers drifted.  The amount of deviation
    encountered is returned as the second component of
    {!val:text_to_patch} for debugging.

    {1 Public interface}

    The library purposefully exposes only a single high-level helper
    besides the [Diff_error] exception – everything else is considered
    private implementation detail.  If you need lower-level access
    (e.g. to instrument commits) please open a feature request instead
    of relying on the internal types.
*)

(** Raised when the patch is malformed or cannot be applied to the
    provided workspace.  The payload describes the first error that
    was encountered. *)
exception Diff_error of string

(** [process_patch ~text ~open_fn ~write_fn ~remove_fn] applies the
    multi-file patch [text] to the workspace.

    The three callback functions abstract over the backing storage:

    • [open_fn path]   must return the current contents of [path].  It
      is called for every file that is updated or deleted.

    • [write_fn path contents] is invoked for each newly added file or
      updated destination.  When a file is renamed the *destination*
      path is passed.

    • [remove_fn path] must delete [path] from the workspace.  It is
      called for every [Delete] action and for the *source* path of a
      rename.

    On success the function returns the string {['Done!']}.  If any
    problem occurs a {!exception:Diff_error} is raised explaining the
    issue. *)
val process_patch
  :  text:string
  -> open_fn:(string -> string)
  -> write_fn:(string -> string -> unit)
  -> remove_fn:(string -> unit)
  -> string
