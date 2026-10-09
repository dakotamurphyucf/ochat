(** Catalog projection: archive state is registry-owned, and active ownership
    is evaluated at query time. Neither field is persisted in Session.t. Missing
    legacy wire fields decode to [false]/[None], never to creator ownership. *)
type t =
  { session : Session.t
  ; active_owner_principal_id : Id.Principal.t option
  ; archived : bool
  ; effective_organization : Session_organization.Values.t
  }
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
