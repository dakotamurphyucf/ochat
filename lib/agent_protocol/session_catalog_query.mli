(** Catalog ordering uses bytewise display-name comparison and ascending typed
    session-id tie breaking, independently of the primary direction. *)
module Sort : sig
  type field =
    | Created_at
    | Updated_at
    | Display_name
  [@@deriving compare, equal, sexp]

  type direction =
    | Ascending
    | Descending
  [@@deriving compare, equal, sexp]

  type t =
    { field : field
    ; direction : direction
    }
  [@@deriving compare, equal, sexp]

  val default : t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Archive_filter : sig
  type t =
    | Active
    | Archived
    | All
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
