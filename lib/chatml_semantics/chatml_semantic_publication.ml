open Core
module Prepared = Chatml_prepared_program
module Driver = Chatml_checked_graph
module Snapshot = Chatml_semantic_snapshot
module Identity = Chatml_semantic_identity
module Limits = Chatml_semantic_limits
module Budget = Chatml_semantic_budget
module R = Chatml_typechecker.Semantic
module B = Chatml_typechecker.Semantic_binding
module Frozen = Chatml_typechecker.Type_snapshot
module P = Snapshot.Private

let reverse items ~checkpoint =
  List.fold items ~init:[] ~f:(fun reversed item ->
    checkpoint ();
    item :: reversed)
;;

let map items ~checkpoint ~f =
  List.fold items ~init:[] ~f:(fun mapped item ->
    checkpoint ();
    f item :: mapped)
  |> reverse ~checkpoint
;;

let array items ~budget ~checkpoint =
  let length =
    List.fold items ~init:0 ~f:(fun count _ ->
      checkpoint ();
      count + 1)
  in
  Budget.admit budget ~facts:0 ~edges:length ~labels:0;
  let pending = ref items in
  Array.init length ~f:(fun _ ->
    checkpoint ();
    match !pending with
    | [] -> assert false
    | item :: rest ->
      pending := rest;
      item)
;;

let namespace = function
  | B.Namespace.Value -> Snapshot.Namespace.Value
  | Type -> Type
  | Type_parameter -> Type_parameter
;;

let kind = function
  | B.Kind.Value -> Snapshot.Symbol.Value
  | Parameter -> Parameter
  | Module -> Module
  | Type_alias -> Type_alias
  | Type_parameter -> Type_parameter
;;

type pending_view =
  | Expand of B.t
  | Finish of B.t

let publish_facts input ~facts ~parsed ~graph ~identity ~limits ~budget ~checkpoint =
  let groups = R.Settled.groups facts in
  let frozen =
    Frozen.freeze
      (map groups ~checkpoint ~f:R.Settled.Group.input)
      ~limits:(Limits.type_graph limits)
      ~checkpoint
    |> Result.map_error ~f:(fun error -> Chatml_semantic_error.Type_graph error)
    |> Budget.take_exn
  in
  let rec root_pairs raw frozen roots =
    checkpoint ();
    match raw, frozen with
    | [], [] -> roots
    | raw :: raw_rest, frozen :: frozen_rest ->
      let index = B.Private.root_index raw in
      if Map.mem roots index then invalid_arg "Semantic publication: repeated actual root";
      root_pairs raw_rest frozen_rest (Map.set roots ~key:index ~data:frozen)
    | [], _ :: _ | _ :: _, [] -> invalid_arg "Semantic publication: freezer root mismatch"
  in
  let rec group_pairs groups frozen roots =
    checkpoint ();
    match groups, frozen with
    | [], [] -> roots
    | group :: groups, frozen :: remaining ->
      group_pairs groups remaining (root_pairs (R.Settled.Group.roots group) frozen roots)
    | [], _ :: _ | _ :: _, [] ->
      invalid_arg "Semantic publication: freezer group mismatch"
  in
  let roots = group_pairs groups (Frozen.roots frozen) Int.Map.empty in
  let root id =
    checkpoint ();
    Map.find_exn roots (B.Private.root_index id)
  in
  let owner = P.Owner.create () in
  let raw_bindings = R.Settled.bindings facts in
  let _, symbols =
    List.fold raw_bindings ~init:(0, Int.Map.empty) ~f:(fun (index, ids) binding ->
      checkpoint ();
      Budget.admit budget ~facts:1 ~edges:1 ~labels:0;
      let id = P.symbol_id owner index in
      index + 1, Map.set ids ~key:(B.Private.symbol_index (B.symbol binding)) ~data:id)
  in
  let symbol id =
    checkpoint ();
    Map.find_exn symbols (B.Private.symbol_index id)
  in
  let _, contexts =
    List.fold
      (R.Settled.contexts facts)
      ~init:(0, Int.Map.empty)
      ~f:(fun (index, ids) context ->
        checkpoint ();
        Budget.admit budget ~facts:1 ~edges:1 ~labels:0;
        let value = P.context owner ~index ~source:context.R.Fact.source in
        index + 1, Map.set ids ~key:(B.Private.context_index context.id) ~data:value)
  in
  let context id =
    checkpoint ();
    Map.find_exn contexts (B.Private.context_index id)
  in
  let origin = function
    | B.Definition.Authored location -> Snapshot.Origin.Authored location
    | Implicit_specification_parameter location ->
      Implicit_specification_parameter location
    | Host { contract; member_path } ->
      let member_path =
        map member_path ~checkpoint ~f:(fun name ->
          Budget.admit budget ~facts:0 ~edges:1 ~labels:(String.length name);
          name)
      in
      Snapshot.Origin.Host { contract; member_path }
  in
  let principal =
    map raw_bindings ~checkpoint ~f:(fun binding ->
      Budget.admit budget ~facts:0 ~edges:1 ~labels:(String.length (B.name binding));
      let specifications =
        List.fold (R.Settled.signature_links facts) ~init:[] ~f:(fun selected link ->
          checkpoint ();
          if B.Symbol_id.equal link.R.Fact.implementation (B.symbol binding)
          then (
            Budget.admit budget ~facts:0 ~edges:1 ~labels:0;
            symbol link.specification :: selected)
          else selected)
        |> reverse ~checkpoint
      in
      P.symbol
        (symbol (B.symbol binding))
        ~name:(B.name binding)
        ~kind:(kind (B.kind binding))
        ~origin:(origin (B.definition binding))
        ~scheme:(Option.map (B.scheme binding) ~f:root)
        ~specifications)
  in
  (* Physical identity is only an immutable-node memo key, not symbol/type
     equivalence: two selected projections of one symbol remain different views.
     Worklist traversal is stack-safe for nested host/public module members. *)
  let views = ref Int.Map.empty in
  let find_view raw =
    let bucket =
      Map.find !views (B.Private.symbol_index (B.symbol raw)) |> Option.value ~default:[]
    in
    List.find_map bucket ~f:(fun (original, copied) ->
      checkpoint ();
      if phys_equal original raw then Some copied else None)
  in
  let get_view raw = find_view raw |> Option.value_exn in
  let ordered bindings =
    map bindings ~checkpoint ~f:(fun (name, binding) -> name, get_view binding)
    |> List.sort ~compare:(fun (left, _) (right, _) ->
      Chatml_semantic_query.compare_string left right ~checkpoint)
    |> map ~checkpoint ~f:snd
  in
  let copy_view requested =
    let rec walk pending =
      checkpoint ();
      match pending with
      | [] -> get_view requested
      | Expand raw :: rest ->
        (match find_view raw with
         | Some _ -> walk rest
         | None ->
           Budget.admit budget ~facts:1 ~edges:1 ~labels:(String.length (B.name raw));
           let pending = Finish raw :: rest in
           let pending =
             match B.members raw with
             | None -> pending
             | Some members ->
               let add pending (_, child) =
                 checkpoint ();
                 Budget.admit budget ~facts:0 ~edges:1 ~labels:0;
                 Expand child :: pending
               in
               List.fold
                 (B.Members.types members)
                 ~init:(List.fold (B.Members.values members) ~init:pending ~f:add)
                 ~f:add
           in
           walk pending)
      | Finish raw :: rest ->
        let values, types =
          match B.members raw with
          | None ->
            ( Snapshot.Unavailable No_nominal_members
            , Snapshot.Unavailable No_nominal_members )
          | Some members ->
            ( Snapshot.Available (ordered (B.Members.values members))
            , Snapshot.Available (ordered (B.Members.types members)) )
        in
        let copied =
          P.binding
            (symbol (B.symbol raw))
            ~name:(B.name raw)
            ~scheme:(Option.map (B.scheme raw) ~f:root)
            ~values
            ~types
        in
        views
        := Map.update
             !views
             (B.Private.symbol_index (B.symbol raw))
             ~f:(fun bucket -> (raw, copied) :: Option.value bucket ~default:[]);
        walk rest
    in
    walk [ Expand requested ]
  in
  let copied_contexts =
    map (R.Settled.contexts facts) ~checkpoint ~f:(fun row ->
      let transition =
        match row.R.Fact.transition with
        | Root -> P.Root
        | Enter parent ->
          Budget.admit budget ~facts:0 ~edges:1 ~labels:0;
          P.Enter (context parent)
        | Join { values; types } ->
          Budget.admit budget ~facts:0 ~edges:2 ~labels:0;
          P.Join
            { values = Option.map values ~f:context; types = Option.map types ~f:context }
        | Define { parent; namespace = selected; binding } ->
          Budget.admit budget ~facts:0 ~edges:2 ~labels:0;
          P.Define
            { parent = context parent
            ; namespace = namespace selected
            ; binding = copy_view binding
            }
      in
      { P.context = context row.id; transition })
  in
  let references =
    map (R.Settled.references facts) ~checkpoint ~f:(fun reference ->
      Budget.admit budget ~facts:1 ~edges:2 ~labels:0;
      let target =
        match reference.R.Fact.target with
        | Binding id -> Snapshot.Reference.Symbol (symbol id)
        | Configured_module id -> Configured_module id
        | Structural_label -> Structural_label
        | Unlocated_module_member -> Unlocated_module_member
        | Language_type name ->
          Budget.admit budget ~facts:0 ~edges:0 ~labels:(String.length name);
          Language_type name
      in
      P.reference
        ~context:(context reference.context)
        ~location:reference.location
        ~target
        ~inferred:(Option.map reference.inferred ~f:root))
  in
  let regions =
    map (R.Settled.regions facts) ~checkpoint ~f:(fun region ->
      Budget.admit budget ~facts:1 ~edges:2 ~labels:0;
      { P.context = context region.R.Fact.context
      ; location = region.location
      ; inferred = Option.map region.inferred ~f:root
      })
  in
  let exports =
    map (R.Settled.exports facts) ~checkpoint ~f:(fun exports ->
      Budget.admit budget ~facts:1 ~edges:1 ~labels:0;
      let copy bindings =
        map bindings ~checkpoint ~f:(fun (_, binding) -> copy_view binding)
      in
      ( exports.R.Fact.owner
      , P.exports
          ~source:exports.source
          ~values:(copy (B.Members.values exports.members))
          ~types:(copy (B.Members.types exports.members)) ))
  in
  let aliases =
    map (R.Settled.aliases facts) ~checkpoint ~f:(fun alias ->
      Budget.admit budget ~facts:1 ~edges:1 ~labels:0;
      let raw =
        List.find_exn raw_bindings ~f:(fun binding ->
          checkpoint ();
          B.Symbol_id.equal (B.symbol binding) alias.R.Fact.binding)
      in
      let parameters =
        map alias.parameters ~checkpoint ~f:(fun id ->
          Budget.admit budget ~facts:0 ~edges:1 ~labels:0;
          symbol id)
      in
      let name name =
        checkpoint ();
        Budget.admit
          budget
          ~facts:1
          ~edges:0
          ~labels:(String.length (Chatml.Chatml_syntax_name.name name))
      in
      name alias.declaration.name;
      List.iter alias.declaration.parameters ~f:name;
      let rec syntax pending =
        checkpoint ();
        match pending with
        | [] -> ()
        | node :: rest ->
          Budget.admit budget ~facts:1 ~edges:0 ~labels:0;
          let add pending child =
            checkpoint ();
            Budget.admit budget ~facts:0 ~edges:1 ~labels:0;
            child :: pending
          in
          let qualified qualified =
            List.iter qualified.Chatml.Chatml_type_expr.Name.modules ~f:name;
            name qualified.name
          in
          let pending =
            match node.Chatml.Chatml_type_expr.desc with
            | TEName qualified_name ->
              qualified qualified_name;
              rest
            | TEParameter parameter ->
              Budget.admit budget ~facts:0 ~edges:0 ~labels:(String.length parameter);
              rest
            | TEArrow (left, right) -> add (add rest left) right
            | TEFunction (parameters, result) ->
              List.fold parameters ~init:(add rest result) ~f:add
            | TETuple tuple ->
              List.fold (Chatml.Chatml_tuple.to_list tuple) ~init:rest ~f:add
            | TEConstr (qualified_name, arguments) ->
              qualified qualified_name;
              List.fold arguments ~init:rest ~f:add
            | TERecord fields ->
              List.fold fields ~init:rest ~f:(fun pending (label, ty) ->
                name label;
                add pending ty)
            | TEVariant variants ->
              List.fold variants ~init:rest ~f:(fun pending (tag, arguments) ->
                name tag;
                List.fold arguments ~init:pending ~f:add)
          in
          syntax pending
      in
      syntax [ alias.declaration.body ];
      P.alias
        (symbol alias.binding)
        ~parameters
        ~declaration:alias.declaration
        ~template:(B.scheme raw |> Option.value_exn |> root))
  in
  let host_requirements =
    map (R.Settled.host_requirements facts) ~checkpoint ~f:(fun requirement ->
      Budget.admit
        budget
        ~facts:1
        ~edges:2
        ~labels:(String.length requirement.R.Fact.name);
      P.host_requirement
        ~name:requirement.name
        ~binding:(symbol requirement.binding)
        ~required:(root requirement.required)
        ~contract:requirement.contract)
  in
  let checked_sources =
    map parsed ~checkpoint ~f:(fun parsed ->
      let sites = Driver.Parsed.sites parsed in
      Budget.admit budget ~facts:1 ~edges:1 ~labels:0;
      List.iter (Chatml.Chatml_syntax_sites.sites sites) ~f:(fun site ->
        checkpoint ();
        Budget.admit
          budget
          ~facts:1
          ~edges:1
          ~labels:(String.length (Chatml.Chatml_syntax_sites.Site.name site)));
      Chatml_source.id (Driver.Parsed.source parsed), Driver.Parsed.phase parsed)
  in
  let runtime_order =
    Chatml_dependency_graph.runtime_dependency_order graph ~checkpoint
  in
  List.iter runtime_order ~f:(fun _ ->
    checkpoint ();
    Budget.admit budget ~facts:0 ~edges:1 ~labels:0);
  let symbols = array principal ~budget ~checkpoint in
  let contexts = array copied_contexts ~budget ~checkpoint in
  checkpoint ();
  P.create
    owner
    ~identity
    ~prepared:input
    ~types:frozen
    ~graph
    ~runtime_order
    ~symbols
    ~contexts
    ~aliases
    ~references
    ~regions
    ~exports
    ~host_requirements
    ~checked_sources
;;

let publish input checked ~identity ~limits ~budget ~checkpoint =
  publish_facts
    input
    ~facts:(Driver.semantic_facts checked |> Option.value_exn)
    ~parsed:(Driver.parsed checked)
    ~graph:(Driver.graph checked)
    ~identity
    ~limits
    ~budget
    ~checkpoint
;;

let publish_provisional input checked ~identity ~limits ~budget ~checkpoint =
  publish_facts
    input
    ~facts:(Driver.Provisional.semantic_facts checked |> Option.value_exn)
    ~parsed:(Driver.Provisional.parsed checked)
    ~graph:(Driver.Provisional.graph checked)
    ~identity
    ~limits
    ~budget
    ~checkpoint
;;
