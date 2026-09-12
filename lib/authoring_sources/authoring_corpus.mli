open Core

(** Trusted source manifest for topic assembly. The host must use a reviewed
    installed manifest; caller-supplied source names or review labels do not
    establish official semantics or authority. *)
type excerpt =
  { path : string
  ; heading : string
  ; include_children : bool
  }
[@@deriving sexp]

type review =
  | Pending
  | Audited of
      { excerpt_sha256 : string list
      ; evidence : string list
      }
[@@deriving sexp]

type specification =
  { id : string
  ; title : string
  ; prerequisites : string list
  ; surfaces : string list
  ; excerpts : excerpt list
  ; review : review
  }
[@@deriving sexp]

type fragment = private
  { source : excerpt
  ; document_sha256 : string
  ; sha256 : string
  ; text : string
  }

type topic = private
  { specification : specification
  ; fragments : fragment list
  ; sha256 : string
  }

type t

(** Extract a unique exact ATX heading and its body from maintained Markdown.
    Ignore headings inside backtick/tilde fences; preserve original source bytes.
    With [include_children], stop at the next heading of equal or lesser depth;
    otherwise stop at the next heading. Missing/duplicate headings and unclosed
    fences reject. This is not a general Markdown renderer or HTML parser. *)
val section
  :  text:string
  -> heading:string
  -> include_children:bool
  -> (string, string) result

(** Resolve every excerpt against installed source bytes and validate unique IDs,
    compiler surfaces, dependencies, dependency-surface compatibility and cycles.
    Audited entries pin the ordered excerpt hashes and name review/test evidence;
    changed excerpts reject until reviewed again. Hashes bind specification/review
    metadata, exact fragments and source revision.
    Pending topics remain inspectable; their presence cannot imply audit completion.
    No source or compiler implementation is executed. *)
val create : sources:Authoring_sources.t -> specification list -> (t, string) result

val identity : t -> string
val topics : t -> topic list

(** Unfiltered manifest inspection for trusted tooling. Model-facing lookup must
    also check the actual surface, capabilities and policy before returning text. *)
val topic : t -> id:string -> (topic, string) result

(** Stable prerequisite-first closure, preserving declared dependency/root order
    and deduplicating shared dependencies. The host supplies the actual compiler
    surface. Missing/duplicate roots or unavailable topics fail explicitly; no
    unrelated surface or truncated subset is substituted. This does not yet apply
    capability filtering or token budgets. *)
val assemble : t -> surface_id:string -> roots:string list -> (topic list, string) result

(** All topics still requiring semantic/example review. Empty means only that
    this manifest's entries are audited; it does not prove full feature coverage. *)
val pending : t -> string list

(** Initial seven source-pinned topics from the checked OCaml-differences guide,
    including the existing chatml.syntax.calls/chatml.types/chatml.tasks IDs.
    Shared prerequisite context identifies the examples as one-off candidates.
    This is the compact language foundation, not full language/runtime coverage
    or any of the five complete task packages. *)
val language_foundation : sources:Authoring_sources.t -> (t, string) result
