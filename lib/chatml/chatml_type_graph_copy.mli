open Core
open Chatml_inferred_type

(** Internal shared copying of mutable inference graphs. This module belongs to
    the typechecker library; it is not a service or language API. The REPL uses
    {!copy_detached} and cannot obtain original cells from its result. The
    existing module-interface probe uses {!copy_for_probe} so its commit logic
    keeps the same originals, before-images and copied cells. Both operations
    use one cycle- and sharing-preserving traversal. *)

module Limits : sig
  type t

  (** All limits are explicit and nonnegative. Counters compare before
      incrementing, so a maximum integer limit cannot overflow them. Zero
      disallows work in the corresponding dimension; the empty graph is valid.
      [max_work]
      also bounds physical-identity comparisons while finding copied cells;
      it is necessary because that search is currently linear. [max_depth]
      is at most 256, a conservative cap on the recursive implementation.
      The empty graph has depth zero; each root constructor starts at one. *)
  val create
    :  max_roots:int
    -> max_type_nodes:int
    -> max_variable_cells:int
    -> max_edges:int
    -> max_work:int
    -> max_depth:int
    -> t Or_error.t
end

module Error : sig
  type dimension =
    | Roots
    | Type_nodes
    | Variable_cells
    | Edges
    | Work
    | Depth
  [@@deriving compare, equal, sexp_of]

  type t = Limit_exceeded of dimension [@@deriving sexp_of]
end

(** [copy_detached roots ~limits ~checkpoint] copies all roots under one
    physical-cell identity memo and returns them in their original order.
    Every [Var] in the result refers to a new cell, never an input cell.
    Shared cells remain shared; [Bound] cycles close through copied cells;
    [Free] name, level and equality are preserved; immutable [Generic]
    parameters keep their names and equality obligations. Structural type nodes are traversed,
    including rows, modules, variants, recursive types and all field kinds.

    Root count, visited type constructors, distinct variable cells, traversed
    child/variable edges, work steps and depth are checked throughout. Repeated
    visits to a structural node or edge count again; a previously copied
    variable cell is not revisited through its [Bound] contents. A root is not
    an edge. A work step is charged for each root, type visit, traversed edge,
    new cell and physical-identity comparison, including copy allocation, so
    the present association-list implementation has honest bounded quadratic
    worst-case work in the number of cells. [checkpoint] is polled during
    graph traversal and identity search. It may raise; exceptions propagate.
    On a limit error or exception, originals remain untouched and no partial
    copy escapes. The caller exclusively owns the original roots while copying
    and must not concurrently mutate them. *)
val copy_detached
  :  typ list
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (typ list, Error.t) result

(** The probe view is private to the typechecker library. It retains original
    cells, their before-images and corresponding copies for the existing
    module-interface transaction. It must not be retained by REPL state. *)
type probe_copy

module Probe_cell : sig
  type t

  val original : t -> tv ref
  val before : t -> tv
  val copy : t -> tv ref
end

(** Preserve the existing probe's explicit checkpoint and caller-owned graph
    contract. Its compiler admission policy is unchanged by this extraction;
    only [copy_detached] admits an independently bounded retained REPL copy.
    A checkpoint exception leaves originals unchanged. *)
val copy_for_probe : typ list -> checkpoint:(unit -> unit) -> probe_copy

val probe_roots : probe_copy -> typ list
val probe_cells : probe_copy -> Probe_cell.t list

(** Build all projected contents for the existing probe commit before it tests
    original before-images and enters its short non-cancellable write phase.
    Fresh variables introduced into the copied graph get new projected cells,
    preserving cycles and avoiding post-commit aliases to mutable probe cells.
    This function never writes to originals; checkpoint exceptions leave them
    untouched. Probe owns the final stale-graph check and one-shot commit. *)
val prepare_probe_projection
  :  probe_copy
  -> checkpoint:(unit -> unit)
  -> (tv ref * tv) list
