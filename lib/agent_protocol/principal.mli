(** Authenticated identity and authorization summary. *)

type t =
  { id : Id.Principal.t
  ; authentication_kind : string
  ; scopes : Scope.Set.t
  ; attributes : (string * string) list
  }
[@@deriving sexp]

(** [create ~id ~authentication_kind ~scopes ~attributes] creates a principal.
    Authentication kinds use lowercase dotted identifiers and attribute keys
    must be unique and nonempty. *)
val create
  :  id:Id.Principal.t
  -> authentication_kind:string
  -> scopes:Scope.Set.t
  -> attributes:(string * string) list
  -> (t, Error.t) result

(** [has_scope t scope] reports whether [t] carries [scope]. *)
val has_scope : t -> Scope.t -> bool

(** [to_json t] encodes the transport-safe principal summary. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes a principal summary. *)
val of_json : Jsonaf.t -> (t, Error.t) result
