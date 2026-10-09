(** CRUD requests are host-qualified. The initialized connection host must match.
    Updates/deletes require a nonnegative expected revision and a bounded key.
    List order is created_at ascending, then typed ID ascending. *)
module Create : sig
  type t =
    { host_id : Id.Server.t
    ; name : Organization_group.Name.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module List : sig
  type t =
    { host_id : Id.Server.t
    ; creator_principal_id : Id.Principal.t option
    ; page : Page.Request.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module type S = sig
  type id [@@deriving sexp]

  module Get : sig
    type t =
      { host_id : Id.Server.t
      ; id : id
      }
    [@@deriving sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Update : sig
    type t =
      { host_id : Id.Server.t
      ; id : id
      ; expected_revision : int64
      ; name : Organization_group.Name.t
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end

  module Delete : sig
    type t =
      { host_id : Id.Server.t
      ; id : id
      ; expected_revision : int64
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp]

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) result
  end
end

module Project : S with type id = Id.Project.t
module Collection : S with type id = Id.Collection.t
