open Core
open Chatml_inferred_type
module Copy = Chatml_type_graph_copy

module Error = struct
  type t =
    | Already_committed
    | Original_changed
    | Escaping_parameter of string
  [@@deriving sexp]
end

type t =
  { copy : Copy.probe_copy
  ; mutable committed : bool
  }

let create roots ~checkpoint =
  { copy = Copy.copy_for_probe roots ~checkpoint; committed = false }
;;

let types t = Copy.probe_roots t.copy

let escaping_parameter t ~rigid_names ~checkpoint =
  let seen = ref [] in
  let rec visit ty =
    checkpoint ();
    match ty with
    | Generic { name; _ } -> if Set.mem rigid_names name then Some name else None
    | Var cell ->
      if
        List.exists !seen ~f:(fun prior ->
          checkpoint ();
          phys_equal prior cell)
      then None
      else (
        seen := cell :: !seen;
        match !cell with
        | Free _ -> None
        | Bound ty -> visit ty)
    | Fun (parameters, result) -> visit_list (result :: parameters)
    | Con (_, values) | Tuple values | Variant_payload values -> visit_list values
    | Mu (_, body) | Ref body | Record body | Module body | Variant body | Array body ->
      visit body
    | Field_receiver (kind, row) -> visit_list [ kind; row ]
    | Row (fields, tail) -> visit_list (tail :: Map.data fields)
    | Rec_var _ | Shape_kind _ | TInt | TFloat | Boolean | String | Empty_row | Unit ->
      None
  and visit_list values = List.find_map values ~f:visit in
  List.find_map (Copy.probe_cells t.copy) ~f:(fun cell ->
    checkpoint ();
    match Copy.Probe_cell.before cell with
    | Free _ -> visit (Var (Copy.Probe_cell.copy cell))
    | Bound _ -> None)
;;

let commit t ~rigid_names ~checkpoint =
  if t.committed
  then Error Error.Already_committed
  else (
    match escaping_parameter t ~rigid_names ~checkpoint with
    | Some name -> Error (Error.Escaping_parameter name)
    | None ->
      let pending = Copy.prepare_probe_projection t.copy ~checkpoint in
      (* No checkpoints or allocation-producing type traversal after this final
         stale-graph check. Cancellation before it cannot publish a partial
         group of constraints. The caller owns these cells exclusively. *)
      if
        List.exists (Copy.probe_cells t.copy) ~f:(fun cell ->
          not (phys_equal !(Copy.Probe_cell.original cell) (Copy.Probe_cell.before cell)))
      then Error Error.Original_changed
      else (
        List.iter pending ~f:(fun (original, contents) -> original := contents);
        t.committed <- true;
        Ok ()))
;;
