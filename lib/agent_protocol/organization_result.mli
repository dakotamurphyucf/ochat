(** Exact organization mutation results; deletion retains the ID forever. *)
module Project_deleted : sig
  type t = private
    { id : Id.Project.t
    ; revision : int64
    ; deleted_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  val create
    :  id:Id.Project.t
    -> revision:int64
    -> deleted_at:Timestamp.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Collection_deleted : sig
  type t = private
    { id : Id.Collection.t
    ; revision : int64
    ; deleted_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  val create
    :  id:Id.Collection.t
    -> revision:int64
    -> deleted_at:Timestamp.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t =
  | Project_created of Organization_group.Project.t
  | Project_updated of Organization_group.Project.t
  | Project_deleted of Project_deleted.t
  | Collection_created of Organization_group.Collection.t
  | Collection_updated of Organization_group.Collection.t
  | Collection_deleted of Collection_deleted.t
[@@deriving equal, sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
