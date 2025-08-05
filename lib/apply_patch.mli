(** Apply *Ochat diff* patches against any storage backend.

    This module is an OCaml rewrite of OpenAI’s reference
    «apply_patch» helper and understands the *Ochat diff* dialect that
    frequently appears in code-generation prompts.  A patch can:

    • add new files;
    • delete existing files;
    • update one or more hunks of an existing file; and
    • rename a file while updating it.

    All I/O is delegated to the three callback functions supplied to
    {!val:process_patch}.  The implementation itself performs **no
    side-effects**, which makes it equally useful on a POSIX file
    system, an in-memory map, or any custom storage layer.  See
    {!file:docs-src/lib/apply_patch.doc.md} for an extended tutorial
    and design notes.

    {1 Patch syntax}

    A diff is bracketed by the markers

    {[ *** Begin Patch ]}
    …
    {[ *** End Patch ]}

    Between the markers a sequence of *file sections* follows; each
    starts with one of:

    • {[*** Add File: <path>]}
    • {[*** Delete File: <path>]}
    • {[*** Update File: <path>]}
    • *(optional)* {[*** Move to: <new-path>]} – only valid directly
      after an [Update File] line.

    Update sections contain one or more unified-diff hunks delimited
    by `@@`.  The algorithm employs *fuzzy* context matching – it
    ignores leading/trailing whitespace, canonicalises common smart
    punctuation via [Uunf] + [Uutf], and will search the whole file if
    line numbers drifted.

    {1 Exposed symbols}

    • {!exception:Diff_error} / {!val:error_to_string} – structured
      error reporting.
    • {!val:process_patch} – parse and apply a patch.
*)

(** Raised when the patch is malformed or cannot be applied.

     The exception carries a value of type {!Apply_patch_error.t} that
     pinpoints the exact problem (syntax error, missing file, context
     mismatch, …).  Use {!val:error_to_string} for a user-friendly
     diagnostic or pattern-match on the constructors to implement
     custom recovery logic. *)
exception Diff_error of Apply_patch_error.t

(** [error_to_string err] converts structured error [err] into a
     colour-free, multi-line diagnostic message similar to what
     [git apply] prints.  The function never raises. *)
val error_to_string : Apply_patch_error.t -> string

(** [process_patch ~text ~open_fn ~write_fn ~remove_fn] parses and
     applies the *Ochat diff* [text].

     {1 Callback contract}

     • [open_fn path]   – returns the current contents of [path].  It
       is invoked for every [Update] and [Delete] action.

     • [write_fn path contents] – must atomically replace or create
       [path] with [contents].  The function is called for every [Add]
       and [Update] destination (including the *new* path of a rename).

     • [remove_fn path] – deletes [path] from the workspace; called
       for every [Delete] action and for the *source* path of a rename.

     All three callbacks should be exception-free; any error they raise
     will propagate to the caller of [process_patch].

     {1 Return value}

     The function returns a tuple:

     – the literal string {e "Done!"} (kept for API compatibility);
     – a list of [(path, snippet)] pairs, one per affected file.
       Each [snippet] shows a small **line-numbered** window around the
       modified hunk(s) and is handy for logging or chat-ops
       confirmations.

     @raise Diff_error if the patch is syntactically invalid, refers to
       a non-existent file, fails context matching, or violates any
       other constraint described in the patch-format section above. *)
val process_patch
  :  text:string
  -> open_fn:(string -> string)
  -> write_fn:(string -> string -> unit)
  -> remove_fn:(string -> unit)
  -> string * (string * string) list
