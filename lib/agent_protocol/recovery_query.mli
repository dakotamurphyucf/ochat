(** Host-local recovery activation and lineage inspection. Session references
    always include a server. A continuation grants no permission. *)
val max_page_size : int

val max_page_bytes : int

module Features : sig
  val windows : string
end

module State : sig
  type t =
    | Open
    | Sealed
    | Expired
  [@@deriving equal, sexp_of]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Summary : sig
  type t =
    { scope : Recovery_window.Scope.t
    ; state : State.t
    }
  [@@deriving sexp_of]

  val page_to_json : t Page.t -> Jsonaf.t
  val page_of_json : Jsonaf.t -> (t Page.t, Error.t) result
end

module Enable : sig
  val method_name : string

  type t =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t
    ; expected_generation : int
    }

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module List_windows : sig
  val method_name : string

  type t =
    { session : Session_ref.t
    ; generation : int option
    ; state : State.t option
    ; page : Page.Request.t
    }

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
