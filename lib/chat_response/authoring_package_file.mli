open Core

(** Host-owned, versioned JSON packages of captured author conventions. These
    files contain text, never source paths to resolve or code to execute. Reading
    a file does not expose its packages to any tool or enable an extension. *)
type t = private
  { source_file : string
  ; contents : string
  }
[@@deriving compare, equal, sexp]

(** A closed version-1 JSON object with [packages]. Each package has [help] and
    [topics], using the fields of [Authoring_metadata.help] and
    [Authoring_corpus.authored_topic]. [source_name] is a provenance label only.
    Shape parsing is bounded to 1 MiB per file; [packages] also checks aggregate
    budgets, dependencies and reserved names against the installed corpus. *)
val of_string : source_file:string -> string -> (t, string) result

val load : env:Eio_unix.Stdenv.base -> path:string -> (t, string) result

(** Validate the entire captured set, allowing dependencies between its files.
    At most 128 files and 4 MiB of encoded JSON are accepted, in addition to the
    corpus's package/topic/text bounds. No filesystem access occurs here. *)
val packages : t list -> (Authoring_corpus.authored_package list, string) result

(** Capture and validate a set of absolute file paths under the aggregate bounds.
    Empty selection performs no reads. Duplicate paths fail before opening files. *)
val load_many : env:Eio_unix.Stdenv.base -> paths:string list -> (t list, string) result
