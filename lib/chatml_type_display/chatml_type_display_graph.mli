open Core
module Frozen = Chatml_typechecker.Type_snapshot
module Budget = Chatml_type_display_budget

(** Frozen-owner graph and row projections. Every later revisit is charged to
    Budget.work; node/edge ceilings belong to the unique reachability scan. *)
type scan

val anchors
  :  Budget.session
  -> Frozen.t
  -> Frozen.Id.t
  -> checkpoint:(unit -> unit)
  -> scan

val incoming : scan -> Frozen.Id.t -> int

val quantifier_ambiguous
  :  scan
  -> string
  -> session:Budget.session
  -> checkpoint:(unit -> unit)
  -> bool

type row_view =
  { fields : (string * Frozen.Id.t) list
  ; tail : Frozen.Id.t option
  }

(** Returns None when flattening would erase an outer-to-inner override. *)
val view_row
  :  Budget.session
  -> Frozen.t
  -> Frozen.Id.t
  -> checkpoint:(unit -> unit)
  -> row_view option

val variant_fields
  :  Budget.session
  -> Frozen.t
  -> (string * Frozen.Id.t) list
  -> checkpoint:(unit -> unit)
  -> (string * Frozen.Id.t list) list option

(** Only scalar/variable leaves and acyclic links to them can omit anchors. *)
val trivial_leaf
  :  Budget.session
  -> Frozen.t
  -> Frozen.Node.t
  -> checkpoint:(unit -> unit)
  -> bool
