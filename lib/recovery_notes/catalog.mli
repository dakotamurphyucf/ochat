(** One immutable catalog per host-qualified session. This module commits no I/O
    and authorizes neither callers nor receipts. A host must authenticate each
    request, serialize mutation of the current catalog, and publish the new
    catalog and receipt together after durable admission. *)

module Note_path : sig
  type t [@@deriving compare, equal, sexp_of]

  (** Case-sensitive virtual path. One to eight slash-separated components,
      each at most 63 bytes using ASCII letters, digits, dot, hyphen or
      underscore. Empty, dot and dot-dot components are invalid. Total length
      is at most 240 bytes. *)
  val of_string : string -> (t, Agent_protocol.Error.t) result

  val to_string : t -> string
end

module Note_id : sig
  type t [@@deriving compare, equal, sexp_of]

  (** Positive, session-local ordinal allocated on creation. *)
  val of_int64 : int64 -> (t, Agent_protocol.Error.t) result

  val to_int64 : t -> int64
end

module Note_revision : sig
  type t [@@deriving compare, equal, sexp_of]

  (** Positive revision, starting at 1 for a new note. *)
  val of_int64 : int64 -> (t, Agent_protocol.Error.t) result

  val to_int64 : t -> int64
end

module Catalog_revision : sig
  type t [@@deriving compare, equal, sexp_of]

  (** Nonnegative session-local revision, starting at 0 for an empty catalog. *)
  val of_int64 : int64 -> (t, Agent_protocol.Error.t) result

  val to_int64 : t -> int64
end

module Content : sig
  type t [@@deriving equal]

  (** At most 64 KiB. Input must be valid UTF-8 and contain no NUL. Bytes are
      preserved verbatim, including line endings and combining scalars. *)
  val of_string : string -> (t, Agent_protocol.Error.t) result

  val to_string : t -> string
end

type selection =
  { id : Note_id.t
  ; revision : Note_revision.t
  }
[@@deriving equal, sexp_of]

type metadata = private
  { selection : selection
  ; path : Note_path.t
  ; byte_length : int
  }
[@@deriving sexp_of]

module Mutation_identity : sig
  (** Distinct namespaces within one catalog. The host must derive the
      authenticated principal or actual native invocation. These values identify
      requests and convey no current authority. A native identity is for one
      mutation within the invocation. *)
  type t =
    | Client of
        { principal : Agent_protocol.Id.Principal.t
        ; key : Agent_protocol.Idempotency_key.t
        }
    | Native of Agent_protocol.Id.Invocation.t
  [@@deriving compare, equal]
end

type edit =
  { expected : selection
  ; content : Content.t
  }

type mutation =
  | Create of
      { expected_catalog_revision : Catalog_revision.t
      ; path : Note_path.t
      ; content : Content.t
      }
  | Replace of edit
  | Append of edit
  | Delete of selection

module Receipt : sig
  type kind =
    | Created
    | Replaced
    | Appended
    | Deleted
  [@@deriving equal, sexp_of]

  (** Factual acknowledgment, containing no note body, path or transient proof.
      Deletion also advances the note revision and leaves no readable head. *)
  type t = private
    { kind : kind
    ; selection : selection
    ; catalog_revision : Catalog_revision.t
    }
  [@@deriving equal, sexp_of]
end

type read_error =
  | Note_not_found
  | Stale_note_revision of Note_revision.t
[@@deriving equal, sexp_of]

type mutation_error =
  | Note_precondition of read_error
  | Stale_catalog_revision of Catalog_revision.t
  | Path_already_exists
  | Empty_append
  | Note_byte_limit
  | Catalog_byte_limit
  | Catalog_entry_limit
  | Revision_exhausted
  | Idempotency_conflict
  | Invalid_identity
  | Document_error of Document_schema.Error.t
[@@deriving equal, sexp_of]

type t

val max_notes : int

type outcome =
  | Applied of t * Receipt.t
  | Replayed of Receipt.t

(** The host supplies the session owner. Invalid components in a typed session
    reference are rejected before creating a catalog. *)
val empty : owner:Agent_protocol.Session_ref.t -> (t, Agent_protocol.Error.t) result

val owner : t -> Agent_protocol.Session_ref.t
val revision : t -> Catalog_revision.t

(** Checks current head identity and exact revision. Held historical receipts
    cannot retrieve superseded or deleted content. *)
val read : t -> selection -> (Content.t, read_error) result

(** Current metadata in case-sensitive lexical path order. *)
val list : t -> metadata list

(** Historical body-free receipt of this exact mutation identity. An absent
    receipt conveys no proof. A retained receipt grants no source authority. *)
val retained_receipt : t -> Mutation_identity.t -> Receipt.t option

(** At most 128 live notes, 2 MiB of live text, and 512 receipts in one catalog.
    Check exact replay before the compare-and-set preconditions. An identity
    still in the receipt window with other arguments conflicts. After eviction,
    retry the original request unchanged. Its catalog revision has advanced for
    create. For edits and deletion, its note revision has advanced or its ID
    has been retired, even if a new note now occupies the same path. A caller
    can reuse an evicted identity for a new mutation with fresh preconditions.
    Invalid principal, key and invocation components fail admission before
    lookup in the receipt window.
    Append concatenates the exact nonempty suffix without adding a separator.
    A same-body replace deliberately advances the revision. A failed operation
    leaves the original catalog available for all further reads and retries. *)
val apply
  :  t
  -> identity:Mutation_identity.t
  -> mutation
  -> (outcome, mutation_error) result

(** V1 universal document. Decode checks expected owner, counters, unique
    identities, aggregate bounds and the consistency of the retained receipt
    window. Unknown fields are retained by stable record identity. A mutation
    that cannot preserve them, or exceeds encoded document limits, fails before
    returning a replacement catalog. *)
val of_document
  :  Document_schema.Document.t
  -> owner:Agent_protocol.Session_ref.t
  -> (t, Document_schema.Error.t) result

val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) result
