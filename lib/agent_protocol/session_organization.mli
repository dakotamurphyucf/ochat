(** Host-qualified historical membership, independent of execution workspace,
    prompt, provider, grants and session-content authority. IDs in [Values] need
    not remain live; current existence/visibility belongs to the host projection. *)
module Values : sig
  type t = private
    { project_id : Id.Project.t option
    ; collection_ids : Id.Collection.t list
    }
  [@@deriving equal, sexp]

  val empty : t

  (** Reject duplicates and more than 128 collections. Normalize collection IDs
      by [Id.Collection.compare]; project and collection namespaces remain typed. *)
  val create
    :  project_id:Id.Project.t option
    -> collection_ids:Id.Collection.t list
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Patch : sig
  type project_change =
    | Keep
    | Set of Id.Project.t
    | Clear
  [@@deriving equal, sexp]

  type t [@@deriving sexp]

  (** Each operation list is bounded to 128 distinct IDs; overlapping add/remove
      is invalid. Apply enforces the total 128 collection bound. *)
  val create
    :  project:project_change
    -> add_collections:Id.Collection.t list
    -> remove_collections:Id.Collection.t list
    -> (t, Error.t) result

  (** Clear/remove absent IDs are no-ops; never resolves current host objects. *)
  val apply : t -> previous:Values.t -> (Values.t, Error.t) result

  (** Set/add references requiring group ownership authorization before fresh
      admission, receipt lookup or idempotent replay. Cleanup has no references. *)
  val requested_groups : t -> Values.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Request : sig
  type t =
    { host_id : Id.Server.t
    ; session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_metadata_revision : int64
    ; patch : Patch.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  (** Decode rejects negative revision and malformed/bounded patch values. *)
  val to_json : t -> Jsonaf.t

  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Query : sig
  module Project : sig
    type t =
      | Any
      | Unassigned
      | Project of Id.Project.t
    [@@deriving equal, sexp]
  end

  type t = private
    { project : Project.t
    ; collection_all_of : Id.Collection.t list
    }
  [@@deriving equal, sexp]

  val default : t

  (** Reject duplicate or more than 128 collection filters; normalize typed IDs. *)
  val create
    :  project:Project.t
    -> collection_all_of:Id.Collection.t list
    -> (t, Error.t) result

  (** [Any] and no collections needs no additional organization read grant. *)
  val requires_organization_view : t -> bool

  val matches : t -> effective:Values.t -> bool
  val to_fields : t -> (string * Jsonaf.t) list
  val of_fields : Json_codec.fields -> (t, Error.t) result
end
