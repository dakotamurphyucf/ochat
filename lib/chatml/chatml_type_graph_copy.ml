open Core
open Chatml_inferred_type

module Limits = struct
  type t =
    { max_roots : int
    ; max_type_nodes : int
    ; max_variable_cells : int
    ; max_edges : int
    ; max_work : int
    ; max_depth : int
    }

  let create
        ~max_roots
        ~max_type_nodes
        ~max_variable_cells
        ~max_edges
        ~max_work
        ~max_depth
    =
    let values =
      [ "max_roots", max_roots
      ; "max_type_nodes", max_type_nodes
      ; "max_variable_cells", max_variable_cells
      ; "max_edges", max_edges
      ; "max_work", max_work
      ; "max_depth", max_depth
      ]
    in
    match List.find values ~f:(fun (_, value) -> value < 0) with
    | Some (name, _) -> Or_error.errorf "%s must be nonnegative" name
    | None when max_depth > 256 ->
      Or_error.error_string "max_depth exceeds the conservative cap of 256"
    | None ->
      Ok { max_roots; max_type_nodes; max_variable_cells; max_edges; max_work; max_depth }
  ;;
end

module Error = struct
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

exception Limit_reached of Error.dimension

module Counts = struct
  type t =
    { limits : Limits.t
    ; mutable roots : int
    ; mutable type_nodes : int
    ; mutable variable_cells : int
    ; mutable edges : int
    ; mutable work : int
    }

  let create limits =
    { limits; roots = 0; type_nodes = 0; variable_cells = 0; edges = 0; work = 0 }
  ;;

  let work t =
    match t with
    | None -> ()
    | Some t ->
      if t.work >= t.limits.max_work then raise_notrace (Limit_reached Work);
      t.work <- t.work + 1
  ;;

  let charge t dimension =
    match t with
    | None -> ()
    | Some t ->
      work (Some t);
      let observed, maximum =
        match dimension with
        | Error.Roots -> t.roots, t.limits.max_roots
        | Type_nodes -> t.type_nodes, t.limits.max_type_nodes
        | Variable_cells -> t.variable_cells, t.limits.max_variable_cells
        | Edges -> t.edges, t.limits.max_edges
        | Work | Depth -> assert false
      in
      if observed >= maximum then raise_notrace (Limit_reached dimension);
      (match dimension with
       | Roots -> t.roots <- t.roots + 1
       | Type_nodes -> t.type_nodes <- t.type_nodes + 1
       | Variable_cells -> t.variable_cells <- t.variable_cells + 1
       | Edges -> t.edges <- t.edges + 1
       | Work | Depth -> assert false)
  ;;

  let depth t value =
    match t with
    | None -> ()
    | Some t -> if value > t.limits.max_depth then raise_notrace (Limit_reached Depth)
  ;;
end

module Probe_cell = struct
  type t =
    { original : tv ref
    ; before : tv
    ; copy : tv ref
    }

  let original t = t.original
  let before t = t.before
  let copy t = t.copy
end

type probe_copy =
  { roots : typ list
  ; cells : Probe_cell.t list
  }

let map_contents contents ~f =
  match contents with
  | Bound ty -> Bound (f ty)
  | Free _ -> contents
;;

let find_cell cells ~checkpoint ~counts ~key wanted =
  List.find cells ~f:(fun cell ->
    checkpoint ();
    Counts.work counts;
    phys_equal (key cell) wanted)
;;

let rec map_type ty ~checkpoint ~counts ~depth ~variable =
  checkpoint ();
  Counts.depth counts depth;
  Counts.charge counts Type_nodes;
  let map_child ty =
    Counts.charge counts Edges;
    map_type ty ~checkpoint ~counts ~depth:(depth + 1) ~variable
  in
  match ty with
  | Var cell ->
    Counts.charge counts Edges;
    Var (variable cell ~depth)
  | Fun (parameters, result) -> Fun (List.map parameters ~f:map_child, map_child result)
  | Con (name, arguments) -> Con (name, List.map arguments ~f:map_child)
  | Mu (name, body) -> Mu (name, map_child body)
  | Ref body -> Ref (map_child body)
  | Record row -> Record (map_child row)
  | Module row -> Module (map_child row)
  | Field_receiver (kind, row) -> Field_receiver (map_child kind, map_child row)
  | Variant row -> Variant (map_child row)
  | Variant_payload values -> Variant_payload (List.map values ~f:map_child)
  | Tuple values -> Tuple (List.map values ~f:map_child)
  | Array body -> Array (map_child body)
  | Row (fields, tail) -> Row (Env.map fields ~f:map_child, map_child tail)
  | Generic _
  | Rec_var _
  | Shape_kind _
  | TInt
  | TFloat
  | Boolean
  | String
  | Empty_row
  | Unit -> ty
;;

let copy_graph roots ~checkpoint ~counts =
  let cells = ref [] in
  let rec copy_variable original ~depth =
    match find_cell !cells ~checkpoint ~counts ~key:Probe_cell.original original with
    | Some cell -> Probe_cell.copy cell
    | None ->
      Counts.charge counts Variable_cells;
      let before = !original in
      let copy = ref (Bound Unit) in
      cells := { Probe_cell.original; before; copy } :: !cells;
      (* Register the new identity before visiting a Bound edge. *)
      copy
      := map_contents before ~f:(fun ty ->
           Counts.charge counts Edges;
           map_type ty ~checkpoint ~counts ~depth:(depth + 1) ~variable:copy_variable);
      copy
  in
  let roots =
    List.map roots ~f:(fun root ->
      checkpoint ();
      Counts.charge counts Roots;
      map_type root ~checkpoint ~counts ~depth:1 ~variable:copy_variable)
  in
  { roots; cells = !cells }
;;

let copy_detached roots ~limits ~checkpoint =
  try
    let copied = copy_graph roots ~checkpoint ~counts:(Some (Counts.create limits)) in
    Ok copied.roots
  with
  | Limit_reached dimension -> Error (Error.Limit_exceeded dimension)
;;

let copy_for_probe roots ~checkpoint = copy_graph roots ~checkpoint ~counts:None
let probe_roots t = t.roots
let probe_cells t = t.cells

let prepare_probe_projection t ~checkpoint =
  let fresh = ref [] in
  let rec project_variable copied ~depth =
    match find_cell t.cells ~checkpoint ~counts:None ~key:Probe_cell.copy copied with
    | Some cell -> Probe_cell.original cell
    | None ->
      (match find_cell !fresh ~checkpoint ~counts:None ~key:fst copied with
       | Some (_, projected) -> projected
       | None ->
         let projected = ref (Bound Unit) in
         fresh := (copied, projected) :: !fresh;
         projected
         := map_contents !copied ~f:(fun ty ->
              map_type
                ty
                ~checkpoint
                ~counts:None
                ~depth:(depth + 1)
                ~variable:project_variable);
         projected)
  in
  List.map t.cells ~f:(fun cell ->
    checkpoint ();
    ( Probe_cell.original cell
    , map_contents
        !(Probe_cell.copy cell)
        ~f:(fun ty ->
          map_type ty ~checkpoint ~counts:None ~depth:1 ~variable:project_variable) ))
;;
