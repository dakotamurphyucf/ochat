open Core
module Inferred = Chatml_inferred_type
module Equality = Chatml.Chatml_equality_requirement
module Shape = Chatml.Chatml_shape_kind

module Input_group : sig
  (** Compiler-domain input only. One actual quantification boundary, possibly
      containing several related roots. Generalized variables with the same raw
      name share ONLY inside this group. Independent binding schemes require
      separate groups, even if both spell a variable 'a. Grouped host contracts
      can intentionally share quantified variables between their roots.
      Free inference cells share by physical identity across ALL groups.
      No input group is retained by a successful snapshot. *)
  type t

  val create : Inferred.typ list -> t
end

module Limits : sig
  (** Explicit positive output bounds, no process-global default. Nodes, input groups,
      quantifiers and recursive binders count toward max_nodes; root/child
      references toward max_edges; retained labels toward max_label_bytes.
      All aggregate arithmetic checks overflow before allocation. *)
  type t

  val create : max_nodes:int -> max_edges:int -> max_label_bytes:int -> t Or_error.t
end

module Id : sig
  (** Snapshot-local index, NOT a source identity or cross-snapshot type key.
      IDs carry a private immutable owner witness; find rejects foreign IDs.
      IDs are obtained from roots/children of the same snapshot. *)
  type t [@@deriving equal, sexp_of]

  (** The actual snapshot-local coordinate, stable only under the immutable
      snapshot's owner witness. This integer is not a type equality key or
      authority: [find] still rejects a foreign owner's ID first. It permits
      bounded index-based cycle/identity accounting without parsing sexps. *)
  val index : t -> int
end

module Quantifier_id : sig
  (** One quantified variable in one explicit Input_group. *)
  type t [@@deriving equal, sexp_of]

  (** Snapshot-local coordinate for display disambiguation, not authority. *)
  val index : t -> int
end

module Recursive_binder_id : sig
  (** Lexical binder identity; recursive source spelling is a display label. *)
  type t [@@deriving equal, sexp_of]

  (** Snapshot-local coordinate for display disambiguation, not authority. *)
  val index : t -> int
end

module Node : sig
  type variable =
    | Unquantified of
        { name : string
        ; equality : Equality.t option
        }
    | Generalized of
        { quantifier : Quantifier_id.t
        ; name : string
        ; equality : Equality.t option
        }
  [@@deriving sexp_of]

  (** Exact immutable projection, preserving raw row layers and their ordered
      override meaning. Generalized versus unquantified is explicit; an
      unquantified variable is NOT automatically called weak storage. Free
      cell identity is the shared node ID, never its printed name. Bound links
      preserve finite cyclic graphs without unfolding. Recursive binders are
      resolved lexically, not compared by source spelling.
      Function argument lists, tuple arity, variant payload lists and the
      record/module/field-receiver distinctions remain explicit. Internal row,
      payload and shape nodes remain internal kinds, not alleged value types. *)
  type t =
    | Variable of variable
    | Bound_link of Id.t
    | Function of Id.t list * Id.t
    | Constructor of string * Id.t list
    | Recursive of Recursive_binder_id.t * Id.t
    | Recursive_variable of Recursive_binder_id.t
    | Reference of Id.t
    | Record of Id.t
    | Module of Id.t
    | Field_receiver of Id.t * Id.t
    | Shape_kind of Shape.t
    | Variant of Id.t
    | Variant_payload of Id.t list
    | Tuple of Id.t list
    | Array of Id.t
    | Int
    | Float
    | Bool
    | String
    | Row of (string * Id.t) list * Id.t
    | Empty_row
    | Unit
  [@@deriving sexp_of]
end

module Error : sig
  type t =
    | Node_limit
    | Edge_limit
    | Label_byte_limit
    | Accounting_overflow
    | Unbound_recursive_variable of string
  [@@deriving sexp_of]
end

(** Immutable graph/labels/IDs only. No raw typ/tv refs, callback, mutable table,
    inference levels/machine, surface implementation or runtime value. *)
type t

(** Freeze all related roots together after the owning compiler has settled.
    The caller exclusively owns raw input until return; this is no concurrent
    mutation detector and performs no inference, generalization or unification.
    Copy contents into invocation-local staging and resolve recursive binders.
    Each physical Mu definition has one lexical binder per input group. Bound
    cells are projected in their lexical binder context, preserving shadowing
    and finite cycles; Free cells retain global sharing across those contexts.
    Stack-safe worklists and polling cover actual traversal/identity search,
    copying, comparisons and final publication. Callback exceptions preserve
    identity/backtrace; limits/cancellation never mutate inputs or publish t.
    Group/root order is preserved. IDs are local coordinates; cross-compilation
    semantic equality is not claimed by equal IDs or rendered strings. *)
val freeze
  :  Input_group.t list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val roots : t -> Id.t list list
val find : t -> Id.t -> Node.t option
val node_count : t -> int
val edge_count : t -> int
val label_bytes : t -> int

(** Snapshot-owned display labels; foreign IDs return None. *)
val recursive_binder_name : t -> Recursive_binder_id.t -> string option
