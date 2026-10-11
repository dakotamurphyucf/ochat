open Core
module Id = Chatml_git_host.Object_id

(** Private verified raw tree decoder, not source discovery or module resolution. *)
module Mode : sig
  type t =
    | Tree
    | Regular
    | Executable
    | Symlink
    | Gitlink
    | Unsupported
  [@@deriving equal, sexp_of]
end

module Entry : sig
  type t

  val name : t -> string
  val id : t -> Id.t
  val mode : t -> Mode.t
end

module Error : sig
  type t =
    | Wrong_kind
    | Malformed
    | Entry_limit
    | Name_limit
  [@@deriving equal, sexp_of]
end

type t

(** Exact raw mode SP name NUL binary object ID (20/32 bytes by format).
    Reject malformed/truncated entries, duplicate exact names, excessive name/
    entry bounds and invalid Git byte ordering (directory uses trailing '/').
    Names are bounded to1024 raw bytes before copying, matching Catalog discovery's
    existing component-input bound. Unsupported numeric modes preserve directory
    marker semantics for sorting: raw(mode land0170000)=0040000 uses '/', even
    when permission bits make the mode unsuitable for traversal. Parse raw octal
    mode with checked accumulation; never infer sorting from Mode.Unsupported.
    Preserve unselected valid raw modes: selected/
    traversed mode rejection belongs to the reader. No locale/UTF8 repair/path
    pattern matching. All input-sized scans/copies/comparisons poll. *)
val decode
  :  Git_object.t
  -> max_entries:int
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val entries : t -> Entry.t list
val find : t -> name:string -> checkpoint:(unit -> unit) -> Entry.t option
