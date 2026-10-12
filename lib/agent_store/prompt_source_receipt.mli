open Core

(** Immutable, bounded original operator-selection metadata. No native authority:
    never reopen [root_file] obtained from stored metadata. A live source owner
    requires separately admitted current operator roots. *)
type t [@@deriving equal, sexp_of]

(** Absolute POSIX root spelling, exact bytes, no NUL. The schema ceiling is4096
    bytes; admission applies the supplied nonnegative stricter cap before copying.
    No realpath, normalization or source IO. Checkpoint exceptions propagate. *)
val create
  :  definition_id:Agent_protocol.Id.Prompt_definition.t
  -> root_file:string
  -> max_path_bytes:int
  -> checkpoint:(unit -> unit)
  -> t Or_error.t

val definition_id : t -> Agent_protocol.Id.Prompt_definition.t
val root_file : t -> string

(** Computed once at admission: pinned length-coded v1 identity of ID + exact
    selected path; independent of file bytes, runtime policy and native identity. *)
val fingerprint : t -> string

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Document_schema.Error.t) Result.t
val shape : Document_schema.Shape.t
