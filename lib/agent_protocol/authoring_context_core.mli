open Authoring_common

module Context : sig
  module Selection : sig
    type target =
      | Entry of string
      | Library
    [@@deriving equal, sexp_of]

    (** [environment] absent selects the declared default; explicit JSON null
        is rejected. Names are bounded, nonempty, exact bytes. *)
    type t = private
      { target : target
      ; environment : string option
      }
    [@@deriving equal, sexp_of]

    val create : target:target -> environment:string option -> (t, Error.t) Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  type origin =
    | Host_project of
        { root : Id.Authoring_root.t
        ; dependency_roots : Id.Authoring_root.t list
        ; project_member : string
        ; selection : Selection.t
        }
    | Uploaded_project of Id.Authoring_bundle.t
    | Host_document of
        { root : Id.Authoring_root.t
        ; source : Source.t
        ; default_environment : string option
        }
    | Supplied_document of
        { source : Source.t
        ; default_environment : string option
        }
  [@@deriving equal, sexp_of]

  module Open_request : sig
    (** [Supplied_document] transports exact bytes by base64 and requires an
        independent document-admission grant. It creates a captured base
        source, not a host file or uploaded project. Other origins have no
        supplied content. No project graph/check success is required for either
        document-only origin. *)
    type t = private
      | Host_project of
          { root : Id.Authoring_root.t
          ; dependency_roots : Id.Authoring_root.t list
          ; project_member : string
          ; selection : Selection.t
          }
      | Uploaded_project of Id.Authoring_bundle.t
      | Host_document of
          { root : Id.Authoring_root.t
          ; member : string
          ; default_environment : string option
          }
      | Supplied_document of
          { member : string
          ; default_environment : string option
          ; contents : string
          }
    [@@deriving sexp_of]

    val create_host_project
      :  root:Id.Authoring_root.t
      -> dependency_roots:Id.Authoring_root.t list
      -> project_member:string
      -> selection:Selection.t
      -> (t, Error.t) Result.t

    val create_uploaded_project : Id.Authoring_bundle.t -> t

    val create_host_document
      :  root:Id.Authoring_root.t
      -> member:string
      -> default_environment:string option
      -> (t, Error.t) Result.t

    val create_supplied_document
      :  member:string
      -> default_environment:string option
      -> contents:string
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  type static_selection =
    { environment : string
    ; compiler_contract : Chatml_source.Digest.t
    }
  [@@deriving equal, sexp_of]

  (** A document-only static selection is host-admitted from an actual
      immutable catalog/compiler contract, never client-provided signatures.
      Absence still permits lexical and basic declaration syntax help; paths,
      templates and script references require separate source/catalog grants. *)

  type basis =
    | Project of
        { base_graph : Chatml_source.Digest.t
        ; selection : static_selection
        }
    | Document of
        { source : Source.t
        ; selection : static_selection option
        }
  [@@deriving equal, sexp_of]

  (** One connection-scoped context view. The host and nonce are selected by
      the server; [id] alone is not a transferable capability. Generation is
      nonnegative and advances on each edit/close. A document basis can be
      admitted without a successful project build; optional static selection
      exists only after the host authenticates that environment. It is not a
      certified analysis or a synthetic project graph. [origin] records the
      admitted base capture; [basis] reflects the current overlay revision.
      Host-document refresh may replace the base source revision under the same
      authorized root/member locator. A supplied document keeps its original
      base capture until a new context is opened. Generation is bounded by
      {!Authoring_common.Limits.maximum_wire_integer}.
      The host, not this DTO constructor, mints the fingerprint from exact
      custody/selection/source identity. *)
  type t [@@deriving sexp_of]

  val create
    :  id:Id.Authoring_context.t
    -> host:Id.Server.t
    -> origin:origin
    -> generation:int
    -> basis:basis
    -> basis_fingerprint:Chatml_source.Digest.t
    -> editable_sources:Source.t list
    -> (t, Error.t) Result.t

  val id : t -> Id.Authoring_context.t
  val host : t -> Id.Server.t
  val origin : t -> origin
  val generation : t -> int
  val basis : t -> basis
  val basis_fingerprint : t -> Chatml_source.Digest.t
  val editable_sources : t -> Source.t list
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t

  type view = t

  module Expected : sig
    (** CAS input for operations on one current context generation. A stale
        response cannot be applied after another document changes. *)
    type t = private
      { context_id : Id.Authoring_context.t
      ; host : Id.Server.t
      ; generation : int
      ; basis_fingerprint : Chatml_source.Digest.t
      }
    [@@deriving equal, sexp_of]

    val of_view : view -> t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Refresh_request : sig
    type policy =
      | Reject_open_documents
      | Discard_open_documents
    [@@deriving equal, sexp_of]

    type t = private
      { expected : Expected.t
      ; policy : policy
      }
    [@@deriving sexp_of]

    val create : expected:Expected.t -> policy:policy -> t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Close_request : sig
    type t = private { expected : Expected.t } [@@deriving sexp_of]

    val create : expected:Expected.t -> t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Close_result : sig
    (** Final generation of a closed context; no active view is returned. *)
    type t = private
      { id : Id.Authoring_context.t
      ; host : Id.Server.t
      ; generation : int
      }
    [@@deriving sexp_of]

    val create
      :  id:Id.Authoring_context.t
      -> host:Id.Server.t
      -> generation:int
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end
end
