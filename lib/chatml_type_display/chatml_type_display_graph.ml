open Core
module Frozen = Chatml_typechecker.Type_snapshot
module Budget = Chatml_type_display_budget
module Budgeted_labels = Budget.Budgeted_labels
open Budget.Completeness

let work = Budget.work
let node = Budget.node
let edge = Budget.edge

let iter_children node ~f =
  match node with
  | Frozen.Node.Variable _
  | Recursive_variable _
  | Shape_kind _
  | Int
  | Float
  | Bool
  | String
  | Empty_row
  | Unit -> ()
  | Bound_link id
  | Recursive (_, id)
  | Reference id
  | Record id
  | Module id
  | Variant id
  | Array id -> f id
  | Function (arguments, result) ->
    List.iter arguments ~f;
    f result
  | Constructor (_, arguments) | Variant_payload arguments | Tuple arguments ->
    List.iter arguments ~f
  | Field_receiver (record, modul) ->
    f record;
    f modul
  | Row (fields, tail) ->
    List.iter fields ~f:(fun (_, id) -> f id);
    f tail
;;

type scan =
  { incoming : int Int.Table.t
  ; quantifier_names : Int.Hash_set.t Budgeted_labels.t
  }

let anchors session graph root ~checkpoint =
  let incoming = Int.Table.create () in
  let visited = Int.Hash_set.create () in
  let quantifier_names = Budgeted_labels.create () in
  let stack = ref [ root ] in
  Hashtbl.set incoming ~key:(Frozen.Id.index root) ~data:1;
  while not (List.is_empty !stack) do
    work session ~checkpoint;
    match !stack with
    | [] -> ()
    | id :: rest ->
      stack := rest;
      let index = Frozen.Id.index id in
      work session ~checkpoint;
      if not (Hash_set.mem visited index)
      then (
        node session ~checkpoint;
        Hash_set.add visited index;
        match Frozen.find graph id with
        | None -> raise (Budget.Stop Work_limit)
        | Some value ->
          (match value with
           | Variable (Generalized { quantifier; name; _ }) ->
             work session ~checkpoint;
             let ids, _ =
               Budgeted_labels.find_or_add
                 quantifier_names
                 name
                 ~session
                 ~checkpoint
                 ~default:Int.Hash_set.create
             in
             Hash_set.add ids (Frozen.Quantifier_id.index quantifier)
           | _ -> ());
          iter_children value ~f:(fun child ->
            edge session ~checkpoint;
            let index = Frozen.Id.index child in
            work session ~checkpoint;
            let old = Hashtbl.find incoming index |> Option.value ~default:0 in
            Hashtbl.set incoming ~key:index ~data:(Int.min 2 (old + 1));
            stack := child :: !stack))
  done;
  { incoming; quantifier_names }
;;

type row_view =
  { fields : (string * Frozen.Id.t) list
  ; tail : Frozen.Id.t option
  }

let view_row session graph root ~checkpoint =
  let visited = Int.Hash_set.create () in
  let names = Budgeted_labels.create () in
  let fields = ref [] in
  let current = ref (Some root) in
  let tail = ref None in
  let duplicate = ref false in
  while Option.is_some !current && not !duplicate do
    work session ~checkpoint;
    match !current with
    | None -> ()
    | Some id ->
      let index = Frozen.Id.index id in
      if Hash_set.mem visited index
      then (
        tail := Some id;
        current := None)
      else (
        Hash_set.add visited index;
        match Frozen.find graph id with
        | None -> raise (Budget.Stop Work_limit)
        | Some (Row (layer, rest)) ->
          let rec add_fields = function
            | [] -> ()
            | (name, value) :: rest ->
              work session ~checkpoint;
              let _, added =
                Budgeted_labels.find_or_add
                  names
                  name
                  ~session
                  ~checkpoint
                  ~default:(fun () -> ())
              in
              if not added
              then duplicate := true
              else (
                fields := (name, value) :: !fields;
                add_fields rest)
          in
          add_fields layer;
          current := Some rest
        | Some (Bound_link child) -> current := Some child
        | Some Empty_row -> current := None
        | Some _ ->
          tail := Some id;
          current := None)
  done;
  if !duplicate
  then None
  else Some { fields = Budget.reverse_list session !fields ~checkpoint; tail = !tail }
;;

let variant_fields session graph fields ~checkpoint =
  let payloads = ref [] in
  let resolve_payload id =
    let visited = Int.Hash_set.create () in
    let current = ref (Some id) in
    let result = ref None in
    while Option.is_some !current do
      work session ~checkpoint;
      match !current with
      | None -> ()
      | Some id ->
        let index = Frozen.Id.index id in
        if Hash_set.mem visited index
        then current := None
        else (
          Hash_set.add visited index;
          match Frozen.find graph id with
          | Some (Bound_link child) -> current := Some child
          | Some (Variant_payload arguments) ->
            result := Some arguments;
            current := None
          | _ -> current := None)
    done;
    !result
  in
  let rec collect = function
    | [] -> Some (Budget.reverse_list session !payloads ~checkpoint)
    | (name, id) :: rest ->
      work session ~checkpoint;
      (match resolve_payload id with
       | None -> None
       | Some arguments ->
         payloads := (name, arguments) :: !payloads;
         collect rest)
  in
  collect fields
;;

let trivial_leaf session graph value ~checkpoint =
  let seen = Int.Hash_set.create () in
  let current = ref (Some value) in
  let trivial = ref false in
  while Option.is_some !current do
    work session ~checkpoint;
    match !current with
    | None -> ()
    | Some value ->
      (match value with
       | Frozen.Node.Variable _
       | Recursive_variable _
       | Shape_kind _
       | Int
       | Float
       | Bool
       | String
       | Empty_row
       | Unit ->
         trivial := true;
         current := None
       | Bound_link child ->
         let index = Frozen.Id.index child in
         if Hash_set.mem seen index
         then current := None
         else (
           Hash_set.add seen index;
           current := Frozen.find graph child)
       | _ -> current := None)
  done;
  !trivial
;;

let incoming scan id =
  Hashtbl.find scan.incoming (Frozen.Id.index id) |> Option.value ~default:0
;;

let quantifier_ambiguous scan name ~session ~checkpoint =
  Budgeted_labels.find scan.quantifier_names name ~session ~checkpoint
  |> Option.value_map ~default:false ~f:(fun ids -> Hash_set.length ids > 1)
;;
