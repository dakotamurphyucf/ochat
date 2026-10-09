(** Process-local signed search continuations. No server-side result set is
    retained. Restart or changed authority/query expires a cursor; a changed
    canonical catalog/organization basis requires an explicit refresh. *)
type t

type binding

module Position : sig
  type t = private
    { session : int
    ; entry : int
    }
  [@@deriving equal, sexp_of]

  (** Zero-based source positions, never hit offsets. At most 10000 catalog
      sessions and 65536 canonical entries per source session are supported. *)
  val create : session:int -> entry:int -> (t, Agent_protocol.Error.t) result

  val start : t
end

val create : unit -> t

(** The catalog has already been authorized, filtered and chronologically sorted.
    Bind exact current principal and all query fields except the cursor itself.
    Reject oversized source catalogs instead of silently truncating coverage. *)
val bind
  :  t
  -> principal:Agent_protocol.Principal.t
  -> query:Agent_protocol.Search_query.t
  -> organization_revision:int64
  -> catalog:Agent_protocol.Session_catalog.t list
  -> (binding, Agent_protocol.Error.t) result

val same_basis : binding -> binding -> bool

val resolve
  :  t
  -> binding
  -> Agent_protocol.Page.Cursor.t option
  -> (Position.t, Agent_protocol.Error.t) result

val issue
  :  t
  -> binding
  -> Position.t
  -> (Agent_protocol.Page.Cursor.t, Agent_protocol.Error.t) result

val refresh_required : unit -> Agent_protocol.Error.t
