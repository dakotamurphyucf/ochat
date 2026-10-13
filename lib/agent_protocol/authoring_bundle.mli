(** Immutable bundle-upload wire values. These DTOs validate portable syntax and
    finite framing; they do not authorize an upload, prove source custody, close
    a project graph, or publish a bundle. The host reauthorizes the authenticated
    principal on every call. Storage (OCH-263) and closure validation (OCH-264)
    independently enforce their own limits and evidence. Explicit [None] fields
    are absent in JSON; explicit null, duplicate and unknown fields are errors. *)

module Bundle : sig
  module Wire_limits : sig
    (** Fixed codec ceilings. The initialized host may advertise and enforce
        lower principal/store limits; these constants do not reserve quota. *)
    val max_members : int

    val max_manifest_bytes : int
    val max_member_bytes : int
    val max_total_declared_bytes : int
    val max_chunk_bytes : int
  end

  module Owner : sig
    (** Echoed comparison coordinates from the authenticated upload owner.
        These labels carry no upload, read or execution authority. *)
    type t = private
      { host : Id.Server.t
      ; principal : Id.Principal.t
      }
    [@@deriving equal, sexp_of]

    val create : host:Id.Server.t -> principal:Id.Principal.t -> t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Member : sig
    (** One qualified source ID, SHA-256 digest and exact byte length. An open
        editor revision cannot be placed in an immutable upload manifest. *)
    type t [@@deriving equal, sexp_of]

    val create : source:Authoring_common.Source.t -> (t, Error.t) result
    val source : t -> Authoring_common.Source.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Manifest : sig
    (** Canonical complete member inventory, including the exact qualified root
        project source. [selection] names its entry and environment. The root
        source must occur exactly once. IDs are distinct; exact, ASCII-case
        and file-versus-directory prefix collisions within one namespace are
        rejected using [Source.Portable_path.first_collision]. Source bytes are
        never inferred from paths. Canonical order is by typed Source.Id comparison.
        Construction bounds member count and aggregate declared bytes before
        retaining the inventory; host policy may impose lower ceilings. *)
    type t [@@deriving equal, sexp_of]

    val create
      :  project_source:Chatml_source.Id.t
      -> selection:Authoring_context.Context.Selection.t
      -> members:Member.t list
      -> (t, Error.t) result

    val project_source : t -> Chatml_source.Id.t
    val selection : t -> Authoring_context.Context.Selection.t
    val members : t -> Member.t list

    (** Digest of a canonical versioned encoding containing the qualified
        project source, exact target/optional environment and every sorted
        member's qualified ID, digest and byte length. Never a proof that any
        source body was received or compiled. *)
    val fingerprint : t -> Chatml_source.Digest.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Begin : sig
    (** The optional parent is current-read provenance only. It is not an
        upload prerequisite. Host authorization binds the request to the
        authenticated principal and server; an upload-only caller may set it
        to None. Reusing [idempotency_key] with an identical complete request
        recovers the same upload without a second quota reservation. *)
    type request = private
      { parent_context : Authoring_context.Context.Expected.t option
      ; idempotency_key : Idempotency_key.t
      ; manifest : Manifest.t
      }
    [@@deriving sexp_of]

    type response [@@deriving sexp_of]

    val create_request
      :  ?parent_context:Authoring_context.Context.Expected.t
      -> idempotency_key:Idempotency_key.t
      -> manifest:Manifest.t
      -> unit
      -> request

    (** The host echoes exact request affinity. [expires_at] is host clock
        output, never a client lease extension. [max_chunk_bytes] is positive
        and no larger than the protocol's fixed decoded-chunk ceiling. *)
    val create_response
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> idempotency_key:Idempotency_key.t
      -> manifest_fingerprint:Chatml_source.Digest.t
      -> parent_context:Authoring_context.Context.Expected.t option
      -> max_chunk_bytes:int
      -> expires_at:Timestamp.t
      -> (response, Error.t) result

    val owner : response -> Owner.t
    val upload_id : response -> Id.Authoring_upload.t
    val idempotency_key : response -> Idempotency_key.t
    val manifest_fingerprint : response -> Chatml_source.Digest.t
    val parent_context : response -> Authoring_context.Context.Expected.t option
    val max_chunk_bytes : response -> int
    val expires_at : response -> Timestamp.t
    val request_to_json : request -> Jsonaf.t
    val request_of_json : Jsonaf.t -> (request, Error.t) result
    val response_to_json : response -> Jsonaf.t
    val response_of_json : Jsonaf.t -> (response, Error.t) result
  end

  module Chunk : sig
    (** One exact member byte offset and bounded binary payload. The wire uses
        base64 and checks encoded size before decoding, decoded size before
        retention, and offset + length without overflow. A repeat of already
        accepted identical bytes is safe; changed or partial-overlap repeats
        conflict in storage. The source ID is never a filesystem path. *)
    type request = private
      { upload_id : Id.Authoring_upload.t
      ; source : Chatml_source.Id.t
      ; byte_offset : int
      ; bytes : string
      }
    [@@deriving sexp_of]

    type response [@@deriving sexp_of]

    val create_request
      :  upload_id:Id.Authoring_upload.t
      -> source:Chatml_source.Id.t
      -> byte_offset:int
      -> bytes:string
      -> (request, Error.t) result

    val create_response
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> source:Chatml_source.Id.t
      -> accepted_until_byte:int
      -> (response, Error.t) result

    val owner : response -> Owner.t
    val upload_id : response -> Id.Authoring_upload.t
    val source : response -> Chatml_source.Id.t
    val accepted_until_byte : response -> int
    val request_to_json : request -> Jsonaf.t
    val request_of_json : Jsonaf.t -> (request, Error.t) result
    val response_to_json : response -> Jsonaf.t
    val response_of_json : Jsonaf.t -> (response, Error.t) result
  end

  module Rejection : sig
    (** Coarse safe status codes, with no source path, private policy or
        compiler diagnostic body. Detailed authorized diagnostics are separate. *)
    type t =
      | Invalid_closure
      | Unsupported_environment
      | Resource_limit
      | Internal_failure
    [@@deriving compare, equal, sexp_of]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Receipt : sig
    (** An immutable publication projection. Constructing/decoding this data
        does not mint the OCH-264 validated-closure proof or grant read/run
        authority. Only the proof-gated host publisher may expose Published. *)
    type t [@@deriving equal, sexp_of]

    val create
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> bundle_id:Id.Authoring_bundle.t
      -> manifest_fingerprint:Chatml_source.Digest.t
      -> project_source:Chatml_source.Id.t
      -> graph:Chatml_source.Digest.t
      -> compiler_contract:Chatml_source.Digest.t
      -> environment:string
      -> expires_at:Timestamp.t
      -> (t, Error.t) result

    val owner : t -> Owner.t
    val upload_id : t -> Id.Authoring_upload.t
    val bundle_id : t -> Id.Authoring_bundle.t
    val manifest_fingerprint : t -> Chatml_source.Digest.t
    val project_source : t -> Chatml_source.Id.t
    val graph : t -> Chatml_source.Digest.t
    val compiler_contract : t -> Chatml_source.Digest.t
    val environment : t -> string
    val expires_at : t -> Timestamp.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Status : sig
    type request = private { upload_id : Id.Authoring_upload.t } [@@deriving sexp_of]

    module Progress : sig
      type t [@@deriving equal, sexp_of]

      val create
        :  source:Chatml_source.Id.t
        -> accepted_until_byte:int
        -> (t, Error.t) result

      val source : t -> Chatml_source.Id.t
      val accepted_until_byte : t -> int
    end

    type state =
      | Staging of Progress.t list
      | Validating
      | Published of Receipt.t
      | Rejected of Rejection.t
      | Expired
    [@@deriving sexp_of]

    (** A status response always echoes the upload ID. Staging progress is
        bounded, ordered by qualified source ID and has no duplicate source.
        Published receipts must echo that same owner and upload ID. Status never renews
        expiry or retries commit/publication. *)
    type t [@@deriving sexp_of]

    val create_request : upload_id:Id.Authoring_upload.t -> request

    val create
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> state:state
      -> (t, Error.t) result

    val owner : t -> Owner.t
    val upload_id : t -> Id.Authoring_upload.t
    val state : t -> state
    val request_to_json : request -> Jsonaf.t
    val request_of_json : Jsonaf.t -> (request, Error.t) result
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Commit : sig
    (** An incomplete attempt is an error and records no commit key. Once
        complete, the key binds upload, manifest and exact payload digest;
        repeated same-key requests recover the same sealed claim/result.
        A different key conflicts. A successful response may still be pending
        closure validation and does not itself assert publication. *)
    type request = private
      { upload_id : Id.Authoring_upload.t
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp_of]

    type outcome =
      | Accepted_for_validation
      | Published of Receipt.t
      | Rejected of Rejection.t
    [@@deriving sexp_of]

    (** A Published receipt must echo this response's owner and upload ID. *)
    type response [@@deriving sexp_of]

    val create_request
      :  upload_id:Id.Authoring_upload.t
      -> idempotency_key:Idempotency_key.t
      -> request

    val create_response
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> idempotency_key:Idempotency_key.t
      -> outcome:outcome
      -> (response, Error.t) result

    val owner : response -> Owner.t
    val upload_id : response -> Id.Authoring_upload.t
    val idempotency_key : response -> Idempotency_key.t
    val outcome : response -> outcome
    val request_to_json : request -> Jsonaf.t
    val request_of_json : Jsonaf.t -> (request, Error.t) result
    val response_to_json : response -> Jsonaf.t
    val response_of_json : Jsonaf.t -> (response, Error.t) result
  end
end
