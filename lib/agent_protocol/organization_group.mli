(** Host-owned logical organization; never an execution workspace or authority grant.
    Names are nonempty UTF-8 without controls, bounded to 1024 bytes. *)
module Name : sig
  type t [@@deriving equal, sexp]

  val create : string -> (t, Error.t) result
  val to_string : t -> string
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module type S = sig
  type id [@@deriving equal, sexp]

  type t = private
    { id : id
    ; creator_principal_id : Id.Principal.t
    ; name : Name.t
    ; revision : int64
    ; created_at : Timestamp.t
    ; updated_at : Timestamp.t
    }
  [@@deriving equal, sexp]

  val create
    :  id:id
    -> creator_principal_id:Id.Principal.t
    -> name:Name.t
    -> revision:int64
    -> created_at:Timestamp.t
    -> updated_at:Timestamp.t
    -> (t, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Project : S with type id = Id.Project.t
module Collection : S with type id = Id.Collection.t
