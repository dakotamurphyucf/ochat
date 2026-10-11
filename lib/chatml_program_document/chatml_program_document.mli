open Core
module Prepared = Chatml_prepared_program
module Captured = Chatml_source

(** Pure durable metadata for a runtime-supplied complete graph.
    Source payload custody and project-receipt restoration remain separate owners.
    This codec does not import the compiler, host runtime or artifact filesystem. *)
module Error : sig
  type t =
    | Document of Document_schema.Error.t
    | Prepared of Prepared.Error.t
    | Invalid_metadata of string list
    | Missing_source of Captured.Id.t
    | Duplicate_source of Captured.Id.t
    | Unexpected_source of Captured.Id.t
    | Revision_mismatch of Captured.Id.t
    | Fingerprint_mismatch
    | Contract_mismatch
    | Limit_exceeded
end

module Source : sig
  (** Immutable expected payload identity from admitted metadata. No body or
      storage path. Revision and graph associations remain restore-validated. *)
  type t

  val id : t -> Captured.Id.t
  val digest : t -> Captured.Digest.t
  val byte_length : t -> int
end

type t

(** Author bounded named metadata from an already admitted graph. Retain exact
    source IDs/digests/lengths, environment, provenance, definitions/scopes and
    source-owned byte locations. No source bodies, compiled code, raw inference
    types, native capabilities, acquisition identity or checkpoint is retained.

    Opaque namespace/environment labels must preserve arbitrary bytes accepted
    by existing constructors; JSON encoding must not add a UTF-8 restriction.
    Reserve encoded lengths before constructing expanded representations.
    Optional fields remain absent, not null. The named document kind is
    [chatml.provided-program], version 1. Opaque labels use lowercase hexadecimal
    fields; paths retain their validated UTF-8 representation. Before hex or JSON
    expansion, checked preflight accounts for exact encoded scalar lengths and
    structural punctuation using the standard JSON measurement boundary, plus
    the existing document node, field and depth limits. *)
val capture
  :  Prepared.t
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val to_document : t -> Document_schema.Document.t

(** Admit the bounded current named document with the standard conversion and
    unknown-field ownership rules. Admission is structural; restoration below
    verifies associations and exact source bytes through current constructors.
    Retain the original admitted current document, including unknown fields and
    envelope metadata. Unknown required semantics or unsupported conversions fail.
    There is no editing operation that could ambiguously reassign unknown data. *)
val of_document
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val fingerprint : t -> Captured.Digest.t

(** Existing immutable inventory projection; no new allocation, source lookup or
    provider access. Callers admit exact bytes with Captured.create's expected
    digest before restoration. Structural admission does not prove that supplied
    payloads exist or that this inventory forms a valid complete graph. *)
val sources : t -> Source.t list

(** Exact payload inventory only. Check count, source bytes and metadata scalar
    bounds before reference validation or construction; [max_input_bytes] must
    be positive. Reject duplicate/surplus
    sources before constructing reference maps. Rebuild locations from
    actual source byte offsets and digest, not trusted stored line/column values.
    Reuse Prepared.create, then compare the retained expected fingerprint and
    authentic contract supplied by the existing host target owner. No parser,
    source lookup callback, filesystem, provider or initializer runs here.

    This restores explicitly supplied graphs, including honest Implicit or
    Project provenance. A selected project build instead uses
    Chatml_project_build.restore with its exact receipt: callers must not substitute this codec to skip that
    owner's project declaration/lock reconciliation. *)
val restore
  :  t
  -> sources:Captured.t list
  -> compiler_contract:Captured.Digest.t
  -> max_input_bytes:int
  -> checkpoint:(unit -> unit)
  -> (Prepared.t, Error.t) Result.t
