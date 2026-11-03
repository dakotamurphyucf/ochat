open! Core

(** Immutable lower-store custody, not recurrence/definition validation or
    authority. The upper admission owner must validate both original JSON values
    with their actual domain constructors and bind them to the child/admission.
    No artifact payload, source acquisition, native resource or callback is kept.

    The complete universal carrier is bounded to 256 KiB, depth 64, 8192 fields
    and 16384 nodes. Its original reservation document is at most 64 KiB and
    depth 32; its original definition is at most 256 KiB and depth 32. These are
    upper bounds: the complete origin and enclosing delegation record must also
    fit their own profiles. Compatible nested/envelope fields and numeric lexemes
    remain exact. Unsupported required semantics reject. *)
type t [@@deriving equal, sexp]

(** [initial_spec_sha256] binds the upper owner's canonical initial Spec after
    normalizing only its delegation locator to absent. This avoids a cycle with
    the admission digest in that locator; the upper owner validates the actual
    reference separately. No semantic Spec validation occurs here. *)
val create
  :  reservation:Jsonaf.t
  -> definition:Jsonaf.t
  -> initial_spec_sha256:string
  -> (t, Document_schema.Error.t) result

val reservation : t -> Jsonaf.t
val definition : t -> Jsonaf.t
val initial_spec_sha256 : t -> string
val to_json : t -> Jsonaf.t
val limits : Document_schema.Limits.t
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) result
