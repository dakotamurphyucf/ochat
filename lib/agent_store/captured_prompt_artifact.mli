(** Immutable pure custody of one COMPLETE captured prompt artifact. This is
    consistency evidence, never permission to acquire, initialize or execute it.
    No I/O, publication, parser, native resources or registry are retained.

    The complete universal document is bounded to 16 MiB, depth 64, 4096 fields
    and 8192 nodes, including compatible unknowns. Its exact original manifest
    publication (at most 262144 decoded bytes), root, authored sources and private
    program/content payloads use canonical padded base64. All artifact bytes
    share the existing 256-item/8 MiB inventory. Decode preflights the complete
    encoded tree and ALL base64 lengths/counts/canonical padding before any
    decoded allocation or hashing. Manifest bytes are never reauthored.

    The enclosing owner must charge this COMPLETE child against its own host
    limits before saving or effects. A legal artifact can exceed a smaller host
    budget. Compatible unknown envelope/payload fields are retained by the
    universal carrier; unknown manifest metadata remains in its exact bytes. *)
type t

val create : Prompt_artifact_store.Artifact.t -> (t, Document_schema.Error.t) result
val value : t -> Prompt_artifact_store.Artifact.t
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) result
val shape : Document_schema.Shape.t
val limits : Document_schema.Limits.t
