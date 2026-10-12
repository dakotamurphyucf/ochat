open Core
open Chatml_inferred_type

module Error = struct
  type t =
    | Already_committed
    | Original_changed
    | Escaping_parameter of string
  [@@deriving sexp]
end

module Cell = struct
  type t =
    { original : tv ref
    ; before : tv
    ; copy : tv ref
    }
end

type t =
  { types : typ list
  ; cells : Cell.t list
  ; mutable committed : bool
  }

(* Variables are graph edges handled by the caller. Keeping the structural
   traversal here makes both copying directions cover the same type kinds. *)
let rec map_type ty ~checkpoint ~variable =
  checkpoint ();
  let map ty = map_type ty ~checkpoint ~variable in
  match ty with
  | Var cell -> Var (variable cell)
  | Fun (parameters, result) -> Fun (List.map parameters ~f:map, map result)
  | Con (name, arguments) -> Con (name, List.map arguments ~f:map)
  | Mu (name, body) -> Mu (name, map body)
  | Ref body -> Ref (map body)
  | Record row -> Record (map row)
  | Module row -> Module (map row)
  | Field_receiver (kind, row) -> Field_receiver (map kind, map row)
  | Variant row -> Variant (map row)
  | Variant_payload values -> Variant_payload (List.map values ~f:map)
  | Tuple values -> Tuple (List.map values ~f:map)
  | Array body -> Array (map body)
  | Row (fields, tail) -> Row (Env.map fields ~f:map, map tail)
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

let map_contents contents ~f =
  match contents with
  | Bound ty -> Bound (f ty)
  | Free _ -> contents
;;

let find_cell cells ~checkpoint ~key wanted =
  List.find cells ~f:(fun cell ->
    checkpoint ();
    phys_equal (key cell) wanted)
;;

let create roots ~checkpoint =
  let cells = ref [] in
  let rec copy_variable original =
    match find_cell !cells ~checkpoint ~key:(fun cell -> cell.Cell.original) original with
    | Some cell -> cell.copy
    | None ->
      let copy = ref (Bound Unit) in
      let before = !original in
      cells := { Cell.original; before; copy } :: !cells;
      (* Install the identity before traversing its contents, so cycles close
         through the copy rather than referring back to an original cell. *)
      copy := map_contents before ~f:(map_type ~checkpoint ~variable:copy_variable);
      copy
  in
  let types = List.map roots ~f:(map_type ~checkpoint ~variable:copy_variable) in
  { types; cells = !cells; committed = false }
;;

let types t = t.types

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
  List.find_map t.cells ~f:(fun cell ->
    checkpoint ();
    match cell.Cell.before with
    | Free _ -> visit (Var cell.copy)
    | Bound _ -> None)
;;

let prepare_commit t ~checkpoint =
  let fresh = ref [] in
  let rec project_variable copied =
    match find_cell t.cells ~checkpoint ~key:(fun cell -> cell.Cell.copy) copied with
    | Some cell -> cell.original
    | None ->
      (match find_cell !fresh ~checkpoint ~key:fst copied with
       | Some (_, projected) -> projected
       | None ->
         let projected = ref (Bound Unit) in
         fresh := (copied, projected) :: !fresh;
         projected
         := map_contents !copied ~f:(map_type ~checkpoint ~variable:project_variable);
         projected)
  in
  List.map t.cells ~f:(fun cell ->
    checkpoint ();
    ( cell.Cell.original
    , map_contents !(cell.copy) ~f:(map_type ~checkpoint ~variable:project_variable) ))
;;

let commit t ~rigid_names ~checkpoint =
  if t.committed
  then Error Error.Already_committed
  else (
    match escaping_parameter t ~rigid_names ~checkpoint with
    | Some name -> Error (Error.Escaping_parameter name)
    | None ->
      let pending = prepare_commit t ~checkpoint in
      (* No checkpoints or allocation-producing type traversal after this final
         stale-graph check. Cancellation before it cannot publish a partial
         group of constraints. The caller owns these cells exclusively. *)
      if
        List.exists t.cells ~f:(fun cell ->
          not (phys_equal !(cell.Cell.original) cell.before))
      then Error Error.Original_changed
      else (
        List.iter pending ~f:(fun (original, contents) -> original := contents);
        t.committed <- true;
        Ok ()))
;;
