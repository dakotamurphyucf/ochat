open! Core

(** Apply-patch failures.

    {!module:Apply_patch} raises {!exception:Diff_error} whenever it
    cannot parse or apply a patch.  The payload – a value of type {!t}
    – is deliberately *rich*: callers can either turn the error into a
    human-readable string via {!val:to_string} or pattern-match on the
    constructors to implement custom recovery strategies.

    The individual variants try to mirror Git terminology and embed
    enough context (path, line numbers, snippets) for a UI to display
    a precise diagnostic. *)

type t =
  | Syntax_error of
      { line : int
      ; text : string
      } (** Malformed diff header or hunk. *)
  | Missing_file of
      { path : string
      ; action : [ `Update | `Delete ]
      } (** Patch refers to a file that does not exist in the workspace. *)
  | File_exists of { path : string } (** Attempting to add a file that already exists. *)
  | Context_mismatch of
      { path : string
      ; expected : string list (** Context lines from the patch. *)
      ; fuzz : int (** Fuzz score accumulated during relaxed matching. *)
      ; snippet : string list (** Slice taken from the real file. *)
      } (** Context lines could not be matched in the target file. *)
  | Bounds_error of
      { path : string
      ; index : int
      ; len : int
      } (** Chunk index or bounds are invalid for the target file. *)

(** Raised by {!Apply_patch.process_patch} on the first error.  The
    exception is *not* wrapped in a generic [Failure] so that callers
    can destructure and react without additional allocation. *)
exception Diff_error of t

(** [to_string err] returns a colour-free, multi-line diagnostic that
    resembles the output of [git apply].  The message comprises:

    • a one-line summary;
    • the most relevant metadata (file path, line numbers, …);
    • when applicable, a small *Tips* section listing common
      copy-and-paste pitfalls.

    The function never raises and is therefore safe to call in an
    [exn_handler] or a catch-all. *)
val to_string : t -> string
