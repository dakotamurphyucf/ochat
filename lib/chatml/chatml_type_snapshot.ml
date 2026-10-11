open Core
module Inferred = Chatml_inferred_type
module Equality = Chatml.Chatml_equality_requirement
module Shape = Chatml.Chatml_shape_kind

module Input_group = struct
  type t = Inferred.typ list

  let create roots = roots
end

module Limits = struct
  type t =
    { max_nodes : int
    ; max_edges : int
    ; max_label_bytes : int
    }

  let create ~max_nodes ~max_edges ~max_label_bytes =
    if max_nodes <= 0 || max_edges <= 0 || max_label_bytes <= 0
    then Or_error.error_string "Type snapshot limits must be positive"
    else Ok { max_nodes; max_edges; max_label_bytes }
  ;;
end

module Owner = struct
  (* An immutable fresh seal. The dynamic string allocation prevents constant
     lifting of this record; no global identity counter or mutable token. *)
  type t = { seal : string }

  let create () = { seal = String.init 1 ~f:(fun _ -> '\000') }
end

module Id = struct
  type t =
    { owner : Owner.t
    ; index : int
    }

  let equal left right =
    phys_equal left.owner right.owner && Int.equal left.index right.index
  ;;

  let sexp_of_t t = [%sexp (t.index : int)]
end

module Quantifier_id = Id
module Recursive_binder_id = Id

module Node = struct
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

module Error = struct
  type t =
    | Node_limit
    | Edge_limit
    | Label_byte_limit
    | Accounting_overflow
    | Unbound_recursive_variable of string
  [@@deriving sexp_of]
end

type t =
  { owner : Owner.t
  ; nodes : Node.t Int.Map.t
  ; roots : Id.t list list
  ; recursive_binders : string Int.Map.t
  ; node_count : int
  ; edge_count : int
  ; label_bytes : int
  }

let roots t = t.roots
let node_count t = t.node_count
let edge_count t = t.edge_count
let label_bytes t = t.label_bytes

let find t (id : Id.t) =
  if phys_equal t.owner id.owner then Map.find t.nodes id.index else None
;;

let recursive_binder_name t (id : Recursive_binder_id.t) =
  if phys_equal t.owner id.owner then Map.find t.recursive_binders id.index else None
;;

(* Staging remains compiler-domain-only. None of these raw pointers, refs or
   tables enter [t]. A nearest-binding map stabilizes on a repeated physical Mu
   definition, unlike an ever-growing binder stack during a type-cell cycle. *)
module Context = struct
  type t =
    { group : int
    ; scope : Recursive_binder_id.t String.Map.t
    }
end

module Bound = struct
  type t =
    { cell : Inferred.tv ref
    ; context : Context.t
    ; id : Id.t
    }
end

module Binder = struct
  type t =
    { definition : Inferred.typ
    ; group : int
    ; name : string
    ; id : Recursive_binder_id.t
    }
end

module Template = struct
  type t =
    | Bound_link
    | Function
    | Constructor of string
    | Recursive of Recursive_binder_id.t
    | Reference
    | Record
    | Module
    | Field_receiver
    | Variant
    | Variant_payload
    | Tuple
    | Array
    | Row of string list
end

module Job = struct
  type t =
    | Visit of Inferred.typ * Context.t * Id.t option ref
    | Finish of Id.t * Template.t * Id.t option ref list
end

let freeze groups ~limits ~checkpoint =
  let exception Stop of Error.t in
  let stop error = raise (Stop error) in
  let owner = Owner.create () in
  let node_count = ref 0 in
  let edge_count = ref 0 in
  let label_bytes = ref 0 in
  let reserve count amount maximum error =
    checkpoint ();
    if amount < 0 then stop Error.Accounting_overflow;
    if amount > maximum - !count then stop error;
    count := !count + amount
  in
  let reserve_node () = reserve node_count 1 limits.Limits.max_nodes Error.Node_limit in
  let reserve_edge () = reserve edge_count 1 limits.max_edges Error.Edge_limit in
  let copy_label name =
    let length = String.length name in
    reserve label_bytes length limits.max_label_bytes Error.Label_byte_limit;
    String.init length ~f:(fun index ->
      if Int.equal (index mod 1024) 0 then checkpoint ();
      name.[index])
  in
  let copy_equality = function
    | None -> None
    | Some Equality.Value -> Some Equality.Value
    | Some (Equality.Row excluded) ->
      Some
        (Equality.Row
           (Set.fold excluded ~init:String.Set.empty ~f:(fun copied label ->
              let label = copy_label label in
              checkpoint ();
              Set.add copied label)))
  in
  let reverse list =
    List.fold list ~init:[] ~f:(fun reversed value ->
      checkpoint ();
      value :: reversed)
  in
  let nodes = Hashtbl.create (module Int) in
  let recursive_binders = Hashtbl.create (module Int) in
  let quantifiers = ref Int.Map.empty in
  let free_cells = ref [] in
  let bound_cells = ref [] in
  let binders = ref [] in
  let pending = ref [] in
  let new_node () =
    reserve_node ();
    let id = { Id.owner; index = Hashtbl.length nodes } in
    Hashtbl.set nodes ~key:id.index ~data:None;
    id
  in
  let finish_node id node = Hashtbl.set nodes ~key:id.Id.index ~data:(Some node) in
  let same_context left right =
    Int.equal left.Context.group right.Context.group
    && Map.equal
         (fun left right ->
            checkpoint ();
            Recursive_binder_id.equal left right)
         left.scope
         right.scope
  in
  let find_free cell =
    List.find_map !free_cells ~f:(fun (original, id) ->
      checkpoint ();
      if phys_equal original cell then Some id else None)
  in
  let find_bound cell context =
    List.find_map !bound_cells ~f:(fun bound ->
      checkpoint ();
      if phys_equal bound.Bound.cell cell && same_context bound.context context
      then Some bound.id
      else None)
  in
  let binder definition group name =
    match
      List.find !binders ~f:(fun binder ->
        checkpoint ();
        Int.equal binder.Binder.group group && phys_equal binder.definition definition)
    with
    | Some binder -> binder.name, binder.id
    | None ->
      reserve_node ();
      let name = copy_label name in
      let id = { Id.owner; index = Hashtbl.length recursive_binders } in
      Hashtbl.set recursive_binders ~key:id.index ~data:name;
      binders := { Binder.definition; group; name; id } :: !binders;
      name, id
  in
  let quantifier group name =
    let names = Map.find !quantifiers group |> Option.value ~default:String.Map.empty in
    checkpoint ();
    match Map.find names name with
    | Some id -> id
    | None ->
      reserve_node ();
      let id = { Id.owner; index = !node_count - 1 } in
      quantifiers
      := Map.set !quantifiers ~key:group ~data:(Map.set names ~key:name ~data:id);
      id
  in
  let schedule_children id context template ~children ~last ~precharged =
    let children =
      List.fold children ~init:[] ~f:(fun staged raw ->
        if not precharged then reserve_edge ();
        (raw, ref None) :: staged)
    in
    let children =
      match last with
      | None -> children
      | Some raw ->
        reserve_edge ();
        (raw, ref None) :: children
    in
    let destinations =
      List.fold children ~init:[] ~f:(fun destinations (_, destination) ->
        checkpoint ();
        destination :: destinations)
    in
    pending := Job.Finish (id, template, destinations) :: !pending;
    List.iter children ~f:(fun (raw, destination) ->
      checkpoint ();
      pending := Job.Visit (raw, context, destination) :: !pending)
  in
  let split_last children =
    let rec loop reversed = function
      | [] -> failwith "Type snapshot: missing final child"
      | [ last ] -> reverse reversed, last
      | child :: rest ->
        checkpoint ();
        loop (child :: reversed) rest
    in
    loop [] children
  in
  let one = function
    | [ child ] -> child
    | _ -> failwith "Type snapshot: invalid unary template"
  in
  let build template children =
    match template with
    | Template.Bound_link -> Node.Bound_link (one children)
    | Function ->
      let parameters, result = split_last children in
      Node.Function (parameters, result)
    | Constructor name -> Node.Constructor (name, children)
    | Recursive binder -> Node.Recursive (binder, one children)
    | Reference -> Node.Reference (one children)
    | Record -> Node.Record (one children)
    | Module -> Node.Module (one children)
    | Field_receiver ->
      (match children with
       | [ kind; row ] -> Node.Field_receiver (kind, row)
       | _ -> failwith "Type snapshot: invalid field receiver template")
    | Variant -> Node.Variant (one children)
    | Variant_payload -> Node.Variant_payload children
    | Tuple -> Node.Tuple children
    | Array -> Node.Array (one children)
    | Row labels ->
      let fields, tail = split_last children in
      let fields =
        List.map2_exn labels fields ~f:(fun label field ->
          checkpoint ();
          label, field)
      in
      Node.Row (fields, tail)
  in
  let visit raw context destination =
    let id, fresh =
      match raw with
      | Inferred.Var cell ->
        (match !cell with
         | Free _ ->
           (match find_free cell with
            | Some id -> id, false
            | None ->
              let id = new_node () in
              free_cells := (cell, id) :: !free_cells;
              id, true)
         | Bound _ ->
           (match find_bound cell context with
            | Some id -> id, false
            | None ->
              let id = new_node () in
              bound_cells := { Bound.cell; context; id } :: !bound_cells;
              id, true))
      | _ -> new_node (), true
    in
    destination := Some id;
    if fresh
    then (
      let schedule template children =
        schedule_children id context template ~children ~last:None ~precharged:false
      in
      let leaf = finish_node id in
      match raw with
      | Inferred.Var { contents = Free { name; equality; level = _ } } ->
        let name = copy_label name in
        let equality = copy_equality equality in
        leaf (Node.Variable (Unquantified { name; equality }))
      | Var { contents = Bound bound } -> schedule Template.Bound_link [ bound ]
      | Generic { name; equality } ->
        let name = copy_label name in
        let equality = copy_equality equality in
        let quantifier = quantifier context.group name in
        leaf (Node.Variable (Generalized { quantifier; name; equality }))
      | Fun (parameters, result) ->
        schedule_children
          id
          context
          Template.Function
          ~children:parameters
          ~last:(Some result)
          ~precharged:false
      | Con (name, arguments) ->
        schedule (Template.Constructor (copy_label name)) arguments
      | Mu (name, body) ->
        let name, binder = binder raw context.group name in
        checkpoint ();
        let context =
          { context with scope = Map.set context.scope ~key:name ~data:binder }
        in
        schedule_children
          id
          context
          (Template.Recursive binder)
          ~children:[ body ]
          ~last:None
          ~precharged:false
      | Rec_var name ->
        checkpoint ();
        (match Map.find context.scope name with
         | Some binder -> leaf (Node.Recursive_variable binder)
         | None ->
           stop (Error.Unbound_recursive_variable (copy_label (String.prefix name 256))))
      | Ref body -> schedule Template.Reference [ body ]
      | Record row -> schedule Template.Record [ row ]
      | Module row -> schedule Template.Module [ row ]
      | Field_receiver (kind, row) -> schedule Template.Field_receiver [ kind; row ]
      | Shape_kind shape -> leaf (Node.Shape_kind shape)
      | Variant row -> schedule Template.Variant [ row ]
      | Variant_payload arguments -> schedule Template.Variant_payload arguments
      | Tuple components -> schedule Template.Tuple components
      | Array body -> schedule Template.Array [ body ]
      | TInt -> leaf Node.Int
      | TFloat -> leaf Node.Float
      | Boolean -> leaf Node.Bool
      | String -> leaf Node.String
      | Row (fields, tail) ->
        let labels, children =
          Map.fold fields ~init:([], []) ~f:(fun ~key ~data (labels, children) ->
            reserve_edge ();
            copy_label key :: labels, data :: children)
        in
        schedule_children
          id
          context
          (Template.Row (reverse labels))
          ~children:(reverse children)
          ~last:(Some tail)
          ~precharged:true
      | Empty_row -> leaf Node.Empty_row
      | Unit -> leaf Node.Unit)
  in
  try
    let root_destinations =
      List.foldi groups ~init:[] ~f:(fun group staged roots ->
        reserve_node ();
        let context = { Context.group; scope = String.Map.empty } in
        let roots =
          List.fold roots ~init:[] ~f:(fun roots raw ->
            reserve_edge ();
            let destination = ref None in
            pending := Job.Visit (raw, context, destination) :: !pending;
            destination :: roots)
        in
        reverse roots :: staged)
      |> reverse
    in
    (* Initial jobs were prepended; restore group/root order before traversal. *)
    pending := reverse !pending;
    while not (List.is_empty !pending) do
      checkpoint ();
      match !pending with
      | [] -> assert false
      | job :: rest ->
        pending := rest;
        (match job with
         | Job.Visit (raw, context, destination) -> visit raw context destination
         | Finish (id, template, destinations) ->
           let children =
             List.fold destinations ~init:[] ~f:(fun children destination ->
               checkpoint ();
               Option.value_exn !destination :: children)
             |> reverse
           in
           finish_node id (build template children))
    done;
    let nodes =
      Hashtbl.fold nodes ~init:Int.Map.empty ~f:(fun ~key ~data frozen ->
        checkpoint ();
        Map.set frozen ~key ~data:(Option.value_exn data))
    in
    let recursive_binders =
      Hashtbl.fold recursive_binders ~init:Int.Map.empty ~f:(fun ~key ~data frozen ->
        checkpoint ();
        Map.set frozen ~key ~data)
    in
    let roots =
      List.fold root_destinations ~init:[] ~f:(fun groups roots ->
        let roots =
          List.fold roots ~init:[] ~f:(fun roots destination ->
            checkpoint ();
            Option.value_exn !destination :: roots)
          |> reverse
        in
        roots :: groups)
      |> reverse
    in
    checkpoint ();
    Ok
      { owner
      ; nodes
      ; roots
      ; recursive_binders
      ; node_count = !node_count
      ; edge_count = !edge_count
      ; label_bytes = !label_bytes
      }
  with
  | Stop error -> Error error
;;
