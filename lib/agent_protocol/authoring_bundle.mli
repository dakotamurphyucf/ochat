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
    module Route : sig
      type t [@@deriving equal]

      (** Owner-local selected dependency associations. Git origin is routing
          intent, never an acquisition grant. Actual declaration and prior-lock
          coherence belongs to the captured Project gate. *)
      val create
        :  declaration:Chatml_source.Id.t
        -> origin:Chatml_project.Lock.Catalog.origin
        -> dependencies:Chatml_project.Lock.Dependency_selection.t list
        -> (t, Error.t) result

      val declaration : t -> Chatml_source.Id.t
      val origin : t -> Chatml_project.Lock.Catalog.origin
      val dependencies : t -> Chatml_project.Lock.Dependency_selection.t list
    end

    module Project : sig
      module Captured_entry : sig
        type delivery =
          | Uploaded_member
          | Derived_script
        [@@deriving equal, sexp_of]

        type t [@@deriving equal]

        val create
          :  source:Authoring_common.Source.t
          -> delivery:delivery
          -> (t, Error.t) result

        val source : t -> Authoring_common.Source.t
        val delivery : t -> delivery
      end

      type entry =
        | Declared
        | Captured of Captured_entry.t

      type mode =
        | Exact_lock of Chatml_source.Id.t
        | Compose of
            { selected_previous_lock : Authoring_common.Source.t option
            ; generated_lock : Chatml_source.Id.t
            }

      type t [@@deriving equal]

      val create
        :  root_source:Chatml_source.Id.t
        -> selection:Authoring_context.Context.Selection.t
        -> entry:entry
        -> source_members:Chatml_source.Id.t list
        -> routes:Route.t list
        -> mode:mode
        -> (t, Error.t) result

      val root_source : t -> Chatml_source.Id.t
      val selection : t -> Authoring_context.Context.Selection.t
      val entry : t -> entry
      val source_members : t -> Chatml_source.Id.t list
      val routes : t -> Route.t list
      val mode : t -> mode
    end

    module Document_member : sig
      type t [@@deriving equal]

      val create
        :  source:Chatml_source.Id.t
        -> member:Source.Portable_path.t
        -> (t, Error.t) result

      val source : t -> Chatml_source.Id.t
      val member : t -> Source.Portable_path.t
    end

    module Script : sig
      module Selector : sig
        type t [@@deriving equal]

        (** Exact uploaded document owner/range and expected extracted body.
            No parser Binding or read authority is constructed here. *)
        val create
          :  registration_source:Chatml_source.Id.t
          -> declaration_source:Chatml_source.Id.t
          -> start_byte:int
          -> end_byte:int
          -> script_id:string
          -> body_digest:Chatml_source.Digest.t
          -> body_byte_length:int
          -> (t, Error.t) result

        val registration_source : t -> Chatml_source.Id.t
        val declaration_source : t -> Chatml_source.Id.t
        val start_byte : t -> int
        val end_byte : t -> int
        val script_id : t -> string
        val body_digest : t -> Chatml_source.Digest.t
        val body_byte_length : t -> int
        val to_json : t -> Jsonaf.t
        val of_json : Jsonaf.t -> (t, Error.t) result
      end

      type choice =
        | Source_only
        | Selected_project of Project.t

      type t [@@deriving equal]

      val create : selector:Selector.t -> choice:choice -> (t, Error.t) result
      val selector : t -> Selector.t
      val choice : t -> choice
    end

    module Chatmd_document : sig
      type t [@@deriving equal]

      (** [root_source] and both owner sources of every script selector must be
          present in [document_members]. Exact byte-range admission follows
          when the enclosing manifest supplies their immutable metadata. *)
      val create
        :  root_source:Chatml_source.Id.t
        -> document_members:Document_member.t list
        -> scripts:Script.t list
        -> (t, Error.t) result

      val root_source : t -> Chatml_source.Id.t
      val document_members : t -> Document_member.t list
      val scripts : t -> Script.t list
    end

    type basis =
      | Project of Project.t
      | Chatmd_document of Chatmd_document.t
    [@@deriving equal]

    type t [@@deriving equal, sexp_of]

    (** One canonical immutable inventory. Nested Project subsets can share
        only this single qualified ID/digest/length record. Project root and
        selected exact lock must occur in its subset. Top-level Project cannot
        use Derived_script; nested derived entry must match the enclosing script
        body digest/length and cannot collide with an uploaded member. Repeated
        derived IDs require exact same revision metadata. Decoder performs the
        same validation. Metadata never grants custody or execution.

        Fixed wire ceilings: <=1024 members, <=32MiB/member, <=512MiB total,
        <=4MiB manifest JSON; <=256 document members/scripts and <=4096 total
        nested metadata nodes. One aggregate admission ledger covers the whole
        manifest, not a renewed allowance per script. ChatMD document members
        obey <=1MiB each and <=8MiB jointly. The canonical v2 fingerprint
        includes tag, all basis fields and typed-sorted full member inventory. *)
    val create : basis:basis -> members:Member.t list -> (t, Error.t) result

    val basis : t -> basis
    val members : t -> Member.t list
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
    module Project : sig
      type t [@@deriving equal]

      (** Exact admitted Prepared input identity; not semantic check success or
          an execution grant. The selected lock is the validated Build's lock. *)
      val create
        :  root_source:Chatml_source.Id.t
        -> entry_source:Chatml_source.Id.t
        -> selected_lock:Chatml_source.Id.t
        -> input_fingerprint:Chatml_source.Digest.t
        -> compiler_contract:Chatml_source.Digest.t
        -> environment:string
        -> (t, Error.t) result

      val root_source : t -> Chatml_source.Id.t
      val entry_source : t -> Chatml_source.Id.t
      val selected_lock : t -> Chatml_source.Id.t
      val input_fingerprint : t -> Chatml_source.Digest.t
      val compiler_contract : t -> Chatml_source.Digest.t
      val environment : t -> string
    end

    module Script : sig
      (** Source-only is deliberately graph-free. Selected Project records
          remain separate per script even if source IDs and bytes overlap. *)
      type result =
        | Source_only of
            { selector : Manifest.Script.Selector.t
            ; body_digest : Chatml_source.Digest.t
            }
        | Selected_project of
            { selector : Manifest.Script.Selector.t
            ; body_digest : Chatml_source.Digest.t
            ; project : Project.t
            }
      [@@deriving equal]
    end

    module Chatmd_document : sig
      type t [@@deriving equal]

      val create
        :  root_source:Chatml_source.Id.t
        -> closure_fingerprint:Chatml_source.Digest.t
        -> scripts:Script.result list
        -> (t, Error.t) result

      val root_source : t -> Chatml_source.Id.t
      val closure_fingerprint : t -> Chatml_source.Digest.t
      val scripts : t -> Script.result list
    end

    type basis =
      | Project of Project.t
      | Chatmd_document of Chatmd_document.t
    [@@deriving equal]

    (** Constructing/decoding this DTO does not mint a validated-closure proof.
        Only the proof-gated publisher may expose Published. Shared owner/upload,
        manifest fingerprint and expiry apply to both bases. No document accessor
        invents a Project graph. Script results are bounded, canonical and unique. *)
    type t [@@deriving equal, sexp_of]

    val create
      :  owner:Owner.t
      -> upload_id:Id.Authoring_upload.t
      -> bundle_id:Id.Authoring_bundle.t
      -> manifest_fingerprint:Chatml_source.Digest.t
      -> basis:basis
      -> expires_at:Timestamp.t
      -> (t, Error.t) result

    val owner : t -> Owner.t
    val upload_id : t -> Id.Authoring_upload.t
    val bundle_id : t -> Id.Authoring_bundle.t
    val manifest_fingerprint : t -> Chatml_source.Digest.t
    val basis : t -> basis
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

    (** [Validating] covers a sealed claim pending a publisher as well as an
        active durable validation intent. It promises neither automatic
        scheduling nor eventual publication. A same-key commit retry is
        required to resume a sealed claim when publication was unavailable. *)

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
