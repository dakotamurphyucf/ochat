open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Inferred = Chatml_inferred_type
module Binding = Chatml_semantic_binding
module Sites = Chatml.Chatml_syntax_sites
module Syntax = Chatml.Chatml_type_expr
module Frozen = Chatml_type_snapshot
module Static = Chatml.Chatml_static_surface
module Lookup = Chatml_configured_lookup
module Parse = Chatml.Chatml_parse
module Private = Binding.Private

module Limits = struct
  type t =
    { max_facts : int
    ; max_edges : int
    ; max_label_bytes : int
    ; max_work_steps : int
    }

  let create ~max_facts ~max_edges ~max_label_bytes ~max_work_steps =
    if max_facts <= 0 || max_edges <= 0 || max_label_bytes <= 0 || max_work_steps <= 0
    then Or_error.error_string "Semantic recording limits must be positive"
    else Ok { max_facts; max_edges; max_label_bytes; max_work_steps }
  ;;
end

module Error = struct
  type t =
    | Fact_limit
    | Edge_limit
    | Label_byte_limit
    | Work_limit
    | Accounting_overflow
    | Foreign_binding
    | Foreign_context
    | Foreign_site
    | Missing_sites
    | Ambiguous_site
    | Failed_invocation
    | Already_settled
    | Invalid_location
    | Foreign_unit
    | Foreign_signature_stage
    | Signature_already_committed
    | Foreign_group_stage
    | Group_already_committed
    | Unfinished_group
  [@@deriving sexp_of]
end

exception Rejected of Error.t

let take_exn = function
  | Ok value -> value
  | Error error -> raise (Rejected error)
;;

module Unit = struct
  type t =
    { owner : Private.Owner.t
    ; index : int
    ; context : Lookup.t
    ; source : Captured.t
    ; sites : Sites.t
    }
end

module Context = struct
  type t =
    { id : Binding.Context_id.t
    ; unit : Unit.t
    }
end

module Target = struct
  type t =
    | Binding of Binding.Symbol_id.t
    | Configured_module of Prepared.Module_id.t
    | Structural_label
    | Unlocated_module_member
    | Language_type of string
end

module Fact = struct
  type transition =
    | Root
    | Enter of Binding.Context_id.t
    | Join of
        { values : Binding.Context_id.t option
        ; types : Binding.Context_id.t option
        }
    | Define of
        { parent : Binding.Context_id.t
        ; namespace : Binding.Namespace.t
        ; binding : Binding.t
        }

  type context =
    { id : Binding.Context_id.t
    ; source : Captured.Id.t
    ; transition : transition
    }

  type reference =
    { context : Binding.Context_id.t
    ; location : Captured.Location.t
    ; target : Target.t
    ; inferred : Binding.Root_id.t option
    }

  type region =
    { context : Binding.Context_id.t
    ; location : Captured.Location.t
    ; inferred : Binding.Root_id.t option
    }

  type alias =
    { binding : Binding.Symbol_id.t
    ; parameters : Binding.Symbol_id.t list
    ; declaration : Syntax.Declaration.t
    }

  type signature_link =
    { implementation : Binding.Symbol_id.t
    ; specification : Binding.Symbol_id.t
    }

  type exports =
    { owner : Prepared.Module_id.t
    ; source : Captured.Id.t
    ; members : Binding.Members.t
    }

  type host_requirement =
    { contract : Captured.Digest.t
    ; name : string
    ; binding : Binding.Symbol_id.t
    ; required : Binding.Root_id.t
    }
end

module Group_stage = struct
  type t =
    { owner : Private.Owner.t
    ; id : Binding.Group_id.t
    ; mutable roots : (Binding.Root_id.t * Inferred.typ) list
    ; mutable committed : bool
    }

  let id t = t.id
end

module Signature_stage = struct
  type t =
    { owner : Private.Owner.t
    ; links : Fact.signature_link list
    ; mutable committed : bool
    }
end

module Settled = struct
  module Group = struct
    type t =
      { id : Binding.Group_id.t
      ; roots : Binding.Root_id.t list
      ; input : Frozen.Input_group.t
      }

    let id t = t.id
    let roots t = t.roots
    let input t = t.input
  end

  type t =
    { groups : Group.t list
    ; bindings : Binding.t list
    ; contexts : Fact.context list
    ; references : Fact.reference list
    ; regions : Fact.region list
    ; aliases : Fact.alias list
    ; signature_links : Fact.signature_link list
    ; exports : Fact.exports list
    ; host_requirements : Fact.host_requirement list
    }

  let groups t = t.groups
  let bindings t = t.bindings
  let contexts t = t.contexts
  let references t = t.references
  let regions t = t.regions
  let aliases t = t.aliases
  let signature_links t = t.signature_links
  let exports t = t.exports
  let host_requirements t = t.host_requirements
end

module Context_pair = struct
  module T = struct
    type t = int * int option * int option [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

module Host_baseline_state = struct
  type t =
    { surface : Static.t
    ; values : (string * Binding.t) list option
    ; aliases : Binding.t Int.Map.t
    }
end

type t =
  { input : Captured.Digest.t
  ; compiler_contract : Captured.Digest.t
  ; owner : Private.Owner.t
  ; limits : Limits.t
  ; mutable facts : int
  ; mutable edges : int
  ; mutable labels : int
  ; mutable work : int
  ; mutable failed : bool
  ; mutable settled : bool
  ; mutable next_id : int
  ; mutable joined : Context.t Map.M(Context_pair).t
  ; mutable root_groups : Binding.Group_id.t Int.Map.t
  ; mutable staged_groups : Group_stage.t list
  ; mutable groups : Settled.Group.t list
  ; mutable bindings : Binding.t list
  ; mutable contexts : Fact.context list
  ; mutable references : Fact.reference list
  ; mutable regions : Fact.region list
  ; mutable aliases : Fact.alias list
  ; mutable signatures : Signature_stage.t list
  ; mutable exports : Fact.exports list
  ; mutable host_requirements : Fact.host_requirement list
  ; mutable host_baseline : Host_baseline_state.t option
  }

module Frame = struct
  type recorder = t

  type t =
    { recorder : recorder
    ; unit : Unit.t
    }

  let recorder t = t.recorder
  let unit t = t.unit
end

let create prepared ~limits =
  { input = Prepared.fingerprint prepared
  ; compiler_contract = Prepared.compiler_contract prepared
  ; owner = Private.Owner.create ()
  ; limits
  ; facts = 0
  ; edges = 0
  ; labels = 0
  ; work = 0
  ; failed = false
  ; settled = false
  ; next_id = 0
  ; joined = Map.empty (module Context_pair)
  ; root_groups = Int.Map.empty
  ; staged_groups = []
  ; groups = []
  ; bindings = []
  ; contexts = []
  ; references = []
  ; regions = []
  ; aliases = []
  ; signatures = []
  ; exports = []
  ; host_requirements = []
  ; host_baseline = None
  }
;;

let discard t = t.failed <- true
let compiler_contract t = t.compiler_contract

let checked_add current addition =
  if addition < 0 || addition > Int.max_value - current
  then raise (Rejected Accounting_overflow);
  current + addition
;;

let admit t ~facts ~edges ~labels =
  let facts = checked_add t.facts facts in
  let edges = checked_add t.edges edges in
  let labels = checked_add t.labels labels in
  if facts > t.limits.max_facts then raise (Rejected Fact_limit);
  if edges > t.limits.max_edges then raise (Rejected Edge_limit);
  if labels > t.limits.max_label_bytes then raise (Rejected Label_byte_limit);
  t.facts <- facts;
  t.edges <- edges;
  t.labels <- labels
;;

let poll_work t checkpoint =
  let work = checked_add t.work 1 in
  if work > t.limits.max_work_steps then raise (Rejected Work_limit);
  t.work <- work;
  checkpoint ()
;;

let with_recording t ~checkpoint ~f =
  Chatml.Chatml_checkpoint.protect checkpoint ~f:(fun checkpoint ->
    try
      if t.failed then raise (Rejected Failed_invocation);
      if t.settled then raise (Rejected Already_settled);
      Ok (f ~checkpoint)
    with
    | Rejected error ->
      t.failed <- true;
      Error error
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      t.failed <- true;
      Stdlib.Printexc.raise_with_backtrace exn backtrace)
;;

let run t ~checkpoint ~f =
  with_recording t ~checkpoint ~f:(fun ~checkpoint ->
    let checkpoint () = poll_work t checkpoint in
    checkpoint ();
    f checkpoint)
;;

let poll t ~checkpoint = run t ~checkpoint ~f:(fun _ -> ())

let site_limits t ~checkpoint =
  run t ~checkpoint ~f:(fun _ ->
    let sites = t.limits.max_facts - t.facts - 1 in
    let bytes = t.limits.max_label_bytes - t.labels in
    if sites <= 0 then raise (Rejected Fact_limit);
    if bytes <= 0 then raise (Rejected Label_byte_limit);
    Sites.Limits.create
      ~max_sites:(Int.min sites Sys.max_array_length)
      ~max_name_bytes:bytes
    |> Or_error.ok_exn)
;;

let fresh t =
  let id = t.next_id in
  t.next_id <- checked_add id 1;
  id
;;

let check_unit t unit =
  if not (Private.Owner.equal t.owner unit.Unit.owner) then raise (Rejected Foreign_unit)
;;

let frame t unit ~checkpoint =
  run t ~checkpoint ~f:(fun _ ->
    check_unit t unit;
    { Frame.recorder = t; unit })
;;

let check_binding t binding =
  if not (Private.owns_symbol t.owner (Binding.symbol binding))
  then raise (Rejected Foreign_binding)
;;

module Host_baseline = struct
  let current t ~surface =
    match t.host_baseline with
    | Some cache when phys_equal cache.Host_baseline_state.surface surface -> Some cache
    | None | Some _ -> None
  ;;

  let replace t ~surface ~values ~aliases =
    t.host_baseline <- Some { Host_baseline_state.surface; values; aliases }
  ;;

  let check_host t binding =
    check_binding t binding;
    match Binding.definition binding with
    | Host { contract; member_path = _ }
      when Captured.Digest.equal contract t.compiler_contract -> ()
    | Authored _ | Implicit_specification_parameter _ | Host _ ->
      raise (Rejected Foreign_binding)
  ;;

  let values t ~surface ~checkpoint =
    run t ~checkpoint ~f:(fun _ ->
      Option.bind (current t ~surface) ~f:(fun cache -> cache.values))
  ;;

  let retain_values t ~surface rows ~checkpoint =
    run t ~checkpoint ~f:(fun checkpoint ->
      (match current t ~surface with
       | Some { values = Some _; _ } ->
         invalid_arg "Semantic_recorder.Host_baseline: duplicate values"
       | None | Some { values = None; _ } -> ());
      List.iter rows ~f:(fun (name, binding) ->
        checkpoint ();
        check_host t binding;
        if not (Private.equal_name name (Binding.name binding) ~checkpoint)
        then raise (Rejected Foreign_binding);
        admit t ~facts:0 ~edges:1 ~labels:0);
      let aliases =
        Option.value_map (current t ~surface) ~default:Int.Map.empty ~f:(fun cache ->
          cache.aliases)
      in
      replace t ~surface ~values:(Some rows) ~aliases)
  ;;

  let alias t ~surface ~ordinal ~checkpoint =
    run t ~checkpoint ~f:(fun _ ->
      Option.bind (current t ~surface) ~f:(fun cache -> Map.find cache.aliases ordinal))
  ;;

  let retain_alias t ~surface ~ordinal binding ~checkpoint =
    run t ~checkpoint ~f:(fun checkpoint ->
      if ordinal < 0 then invalid_arg "Semantic_recorder.Host_baseline: negative ordinal";
      let cache = current t ~surface in
      let aliases =
        Option.value_map cache ~default:Int.Map.empty ~f:(fun cache -> cache.aliases)
      in
      if Map.mem aliases ordinal
      then invalid_arg "Semantic_recorder.Host_baseline: duplicate alias";
      checkpoint ();
      check_host t binding;
      admit t ~facts:0 ~edges:1 ~labels:0;
      let values = Option.bind cache ~f:(fun cache -> cache.values) in
      replace t ~surface ~values ~aliases:(Map.set aliases ~key:ordinal ~data:binding))
  ;;
end

let check_root t root =
  if not (Private.owns_root t.owner root) then raise (Rejected Foreign_binding)
;;

let check_context t unit context =
  check_unit t unit;
  if
    (not (Private.owns_context t.owner context.Context.id))
    || not (phys_equal unit context.unit)
  then raise (Rejected Foreign_context)
;;

let source unit = unit.Unit.source
let context_id context = context.Context.id

let same_source left right =
  Captured.Id.equal (Captured.id left) (Captured.id right)
  && Captured.Digest.equal (Captured.digest left) (Captured.digest right)
;;

let make_unit t context source sites ~phase checkpoint =
  checkpoint ();
  if
    (not (same_source source (Lookup.source context)))
    || (not (Lookup.Phase.equal phase (Lookup.phase context)))
    || not (Captured.Digest.equal t.input (Lookup.input_fingerprint context))
  then raise (Rejected Foreign_unit);
  let sites =
    match sites with
    | None -> raise (Rejected Missing_sites)
    | Some sites -> sites
  in
  if not (same_source source (Sites.source sites)) then raise (Rejected Foreign_site);
  admit
    t
    ~facts:(checked_add 1 (Sites.site_count sites))
    ~edges:0
    ~labels:(Sites.name_bytes sites);
  { Unit.owner = t.owner; index = fresh t; context; source; sites }
;;

let implementation t context parsed ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    make_unit
      t
      context
      (Parse.Unit.source parsed)
      (Parse.Unit.sites parsed)
      ~phase:Implementation
      checkpoint)
;;

let interface t context parsed ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    make_unit
      t
      context
      (Parse.Interface.source parsed)
      (Parse.Interface.sites parsed)
      ~phase:Interface
      checkpoint)
;;

let location unit span checkpoint =
  match Captured.of_span unit.Unit.source span ~checkpoint with
  | Ok location -> location
  | Error _ -> raise (Rejected Invalid_location)
;;

let validate_location unit location =
  if
    (not
       (Captured.Id.equal
          (Captured.id unit.Unit.source)
          (Captured.Location.source location)))
    || not
         (Captured.Digest.equal
            (Captured.digest unit.source)
            (Captured.Location.source_digest location))
  then raise (Rejected Foreign_site)
;;

let named t unit ~role name ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_unit t unit;
    match Chatml.Chatml_syntax_name.site name with
    | None -> None
    | Some id ->
      (match Sites.find unit.Unit.sites id with
       | Some site
         when Sites.Role.equal role (Sites.Site.role site)
              && Private.equal_name
                   (Chatml.Chatml_syntax_name.name name)
                   (Sites.Site.name site)
                   ~checkpoint -> Some (Sites.Site.location site)
       | None | Some _ -> raise (Rejected Foreign_site)))
;;

let authored t unit ~role ~span ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_unit t unit;
    ignore (location unit span checkpoint : Captured.Location.t);
    match Sites.at_span unit.sites ~role ~checkpoint span with
    | Missing -> None
    | Ambiguous -> raise (Rejected Ambiguous_site)
    | Unique site -> Some (Sites.Site.location site))
;;

let enter_context t unit ~parent ~checkpoint =
  run t ~checkpoint ~f:(fun _ ->
    check_unit t unit;
    Option.iter parent ~f:(check_context t unit);
    admit t ~facts:1 ~edges:(Option.value_map parent ~default:0 ~f:(fun _ -> 1)) ~labels:0;
    let id = Private.context t.owner (fresh t) in
    let transition =
      match parent with
      | None -> Fact.Root
      | Some parent -> Enter parent.id
    in
    t.contexts <- { Fact.id; source = Captured.id unit.source; transition } :: t.contexts;
    { Context.id; unit })
;;

let join_context t unit ~values ~types ~checkpoint =
  run t ~checkpoint ~f:(fun _ ->
    check_unit t unit;
    Option.iter values ~f:(check_context t unit);
    Option.iter types ~f:(check_context t unit);
    let index context =
      Option.map context ~f:(fun context -> Private.context_index context.Context.id)
    in
    let key = unit.Unit.index, index values, index types in
    match Map.find t.joined key with
    | Some context -> context
    | None ->
      admit
        t
        ~facts:1
        ~edges:
          (Option.value_map values ~default:0 ~f:(fun _ -> 1)
           + Option.value_map types ~default:0 ~f:(fun _ -> 1))
        ~labels:0;
      let id = Private.context t.owner (fresh t) in
      let transition =
        Fact.Join
          { values = Option.map values ~f:context_id
          ; types = Option.map types ~f:context_id
          }
      in
      t.contexts
      <- { Fact.id; source = Captured.id unit.source; transition } :: t.contexts;
      let context = { Context.id; unit } in
      t.joined <- Map.set t.joined ~key ~data:context;
      context)
;;

let define_context t unit context ~namespace ~name ~binding ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_context t unit context;
    check_binding t binding;
    checkpoint ();
    if not (Private.equal_name name (Binding.name binding) ~checkpoint)
    then raise (Rejected Foreign_binding);
    admit t ~facts:1 ~edges:2 ~labels:0;
    let id = Private.context t.owner (fresh t) in
    let transition = Fact.Define { parent = context.id; namespace; binding } in
    t.contexts <- { Fact.id; source = Captured.id unit.source; transition } :: t.contexts;
    { Context.id; unit })
;;

let stage_group_raw t =
  admit t ~facts:1 ~edges:0 ~labels:0;
  let stage =
    { Group_stage.owner = t.owner
    ; id = Private.group t.owner (fresh t)
    ; roots = []
    ; committed = false
    }
  in
  t.staged_groups <- stage :: t.staged_groups;
  stage
;;

let check_group_stage t stage =
  if not (Private.Owner.equal t.owner stage.Group_stage.owner)
  then raise (Rejected Foreign_group_stage);
  if stage.committed then raise (Rejected Group_already_committed)
;;

let stage_root_raw t stage inferred checkpoint =
  check_group_stage t stage;
  checkpoint ();
  admit t ~facts:1 ~edges:1 ~labels:0;
  let root = Private.root t.owner (fresh t) in
  t.root_groups <- Map.set t.root_groups ~key:(Private.root_index root) ~data:stage.id;
  stage.roots <- (root, inferred) :: stage.roots;
  root
;;

let commit_group_raw t stage checkpoint =
  check_group_stage t stage;
  let roots, inputs =
    List.fold stage.roots ~init:([], []) ~f:(fun (roots, inputs) (root, inferred) ->
      checkpoint ();
      root :: roots, inferred :: inputs)
  in
  let group =
    { Settled.Group.id = stage.id; roots; input = Frozen.Input_group.create inputs }
  in
  t.groups <- group :: t.groups;
  stage.committed <- true;
  stage.roots <- []
;;

let stage_group t ~checkpoint = run t ~checkpoint ~f:(fun _ -> stage_group_raw t)

let stage_root t stage inferred ~checkpoint =
  run t ~checkpoint ~f:(stage_root_raw t stage inferred)
;;

let commit_group t stage ~checkpoint = run t ~checkpoint ~f:(commit_group_raw t stage)

let group_raw t roots checkpoint =
  let stage = stage_group_raw t in
  let roots =
    List.map roots ~f:(fun inferred -> stage_root_raw t stage inferred checkpoint)
  in
  commit_group_raw t stage checkpoint;
  stage.id, roots
;;

let group t roots ~checkpoint = run t ~checkpoint ~f:(group_raw t roots)

let check_scheme t checkpoint = function
  | None -> ()
  | Some (group, root) ->
    if not (Private.owns_group t.owner group) then raise (Rejected Foreign_binding);
    check_root t root;
    checkpoint ();
    (match Map.find t.root_groups (Private.root_index root) with
     | Some expected when Binding.Group_id.equal expected group -> ()
     | Some _ | None -> raise (Rejected Foreign_binding))
;;

let members t ~values ~types ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    let validate entries =
      List.iter entries ~f:(fun (name, binding) ->
        checkpoint ();
        check_binding t binding;
        let scheme =
          match Binding.group binding, Binding.scheme binding with
          | Some group, Some root -> Some (group, root)
          | None, None -> None
          | Some _, None | None, Some _ -> raise (Rejected Foreign_binding)
        in
        check_scheme t checkpoint scheme;
        if not (Private.equal_name name (Binding.name binding) ~checkpoint)
        then raise (Rejected Foreign_binding);
        admit t ~facts:0 ~edges:1 ~labels:0)
    in
    validate values;
    validate types;
    Private.members ~values ~types)
;;

let define t unit ~name ~kind ~definition ~scheme ~members ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_unit t unit;
    check_scheme t checkpoint scheme;
    (match definition with
     | Binding.Definition.Authored location | Implicit_specification_parameter location ->
       validate_location unit location
     | Host { contract = _; member_path } ->
       List.iter member_path ~f:(fun label ->
         checkpoint ();
         admit t ~facts:0 ~edges:1 ~labels:(String.length label)));
    admit
      t
      ~facts:1
      ~edges:(Option.value_map scheme ~default:0 ~f:(fun _ -> 1))
      ~labels:(String.length name);
    let binding =
      Private.create
        (Private.symbol t.owner (fresh t))
        ~name
        ~kind
        ~definition
        ~scheme
        ~members
    in
    t.bindings <- binding :: t.bindings;
    binding)
;;

let finish_scheme t binding ~scheme ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_binding t binding;
    check_scheme t checkpoint (Some scheme);
    admit t ~facts:0 ~edges:1 ~labels:0;
    let replacement = Private.with_scheme binding ~scheme in
    t.bindings
    <- List.map t.bindings ~f:(fun entry ->
         checkpoint ();
         if Binding.Symbol_id.equal (Binding.symbol binding) (Binding.symbol entry)
         then replacement
         else entry);
    replacement)
;;

let define_host t unit ~name ~kind ~member_path_rev ~scheme ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_unit t unit;
    check_scheme t checkpoint (Some scheme);
    List.iter member_path_rev ~f:(fun label ->
      checkpoint ();
      admit t ~facts:0 ~edges:1 ~labels:(String.length label));
    admit t ~facts:1 ~edges:1 ~labels:(String.length name);
    let member_path =
      List.fold member_path_rev ~init:[] ~f:(fun path name ->
        checkpoint ();
        name :: path)
    in
    let binding =
      Private.create
        (Private.symbol t.owner (fresh t))
        ~name
        ~kind
        ~definition:
          (Binding.Definition.Host { contract = t.compiler_contract; member_path })
        ~scheme:(Some scheme)
        ~members:None
    in
    t.bindings <- binding :: t.bindings;
    binding)
;;

let finish_members t binding members ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_binding t binding;
    List.iter (Binding.Members.values members) ~f:(fun (_, binding) ->
      checkpoint ();
      check_binding t binding);
    List.iter (Binding.Members.types members) ~f:(fun (_, binding) ->
      checkpoint ();
      check_binding t binding);
    admit t ~facts:0 ~edges:1 ~labels:0;
    let replacement = Private.with_members binding members in
    t.bindings
    <- List.map t.bindings ~f:(fun entry ->
         checkpoint ();
         if Binding.Symbol_id.equal (Binding.symbol binding) (Binding.symbol entry)
         then replacement
         else entry);
    replacement)
;;

let public_binding t ~implementation ~specification ~members ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_binding t implementation;
    check_binding t specification;
    let scheme =
      match Binding.group specification, Binding.scheme specification with
      | Some group, Some root -> group, root
      | None, _ | Some _, None -> raise (Rejected Foreign_binding)
    in
    check_scheme t checkpoint (Some scheme);
    admit t ~facts:1 ~edges:2 ~labels:0;
    Private.public_projection implementation ~scheme ~members)
;;

let root_of_type t inferred checkpoint =
  Option.map inferred ~f:(fun inferred ->
    let _, roots = group_raw t [ inferred ] checkpoint in
    List.hd_exn roots)
;;

let reference t unit ~context ~location ~(target : Target.t) ~inferred ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_context t unit context;
    validate_location unit location;
    (match target with
     | Binding symbol ->
       if not (Private.owns_symbol t.owner symbol) then raise (Rejected Foreign_binding)
     | Configured_module _ | Structural_label | Unlocated_module_member -> ()
     | Language_type name -> admit t ~facts:0 ~edges:0 ~labels:(String.length name));
    admit t ~facts:1 ~edges:2 ~labels:0;
    let inferred = root_of_type t inferred checkpoint in
    t.references
    <- { Fact.context = context.id; location; target; inferred } :: t.references)
;;

let region_raw t unit context span inferred checkpoint =
  check_context t unit context;
  let location = location unit span checkpoint in
  admit t ~facts:1 ~edges:1 ~labels:0;
  let inferred = root_of_type t inferred checkpoint in
  t.regions <- { Fact.context = context.id; location; inferred } :: t.regions
;;

let expression_root t unit ~context ~span ~root ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_context t unit context;
    check_root t root;
    let location = location unit span checkpoint in
    admit t ~facts:1 ~edges:2 ~labels:0;
    t.regions
    <- { Fact.context = context.id; location; inferred = Some root } :: t.regions)
;;

let expression t unit ~context ~span ~inferred ~checkpoint =
  run t ~checkpoint ~f:(region_raw t unit context span (Some inferred))
;;

let region t unit ~context ~span ~checkpoint =
  run t ~checkpoint ~f:(region_raw t unit context span None)
;;

let alias t unit ~binding ~parameters ~declaration ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_unit t unit;
    check_binding t binding;
    (* Account the actual original syntax retained by this metadata row, not a
       representative alias application. The worklist avoids source-sized stacks. *)
    let label name =
      checkpoint ();
      admit t ~facts:0 ~edges:0 ~labels:(String.length name)
    in
    let syntax_name name =
      label (Chatml.Chatml_syntax_name.name name);
      Option.iter (Chatml.Chatml_syntax_name.site name) ~f:(fun id ->
        match Sites.find unit.sites id with
        | None -> raise (Rejected Foreign_site)
        | Some _ -> ())
    in
    syntax_name declaration.Syntax.Declaration.name;
    List.iter declaration.parameters ~f:syntax_name;
    let rec walk = function
      | [] -> ()
      | syntax :: rest ->
        checkpoint ();
        ignore (location unit syntax.Syntax.span checkpoint : Captured.Location.t);
        (match syntax.desc with
         | TEParameter name ->
           label name;
           walk rest
         | TEName name ->
           List.iter name.modules ~f:syntax_name;
           syntax_name name.name;
           walk rest
         | TEConstr (name, arguments) ->
           List.iter name.modules ~f:syntax_name;
           syntax_name name.name;
           walk
             (List.fold arguments ~init:rest ~f:(fun rest argument ->
                checkpoint ();
                argument :: rest))
         | TEArrow (left, right) -> walk (left :: right :: rest)
         | TEFunction (arguments, result) ->
           walk
             (List.fold arguments ~init:(result :: rest) ~f:(fun rest argument ->
                checkpoint ();
                argument :: rest))
         | TETuple tuple ->
           walk
             (List.fold
                (Chatml.Chatml_tuple.to_list tuple)
                ~init:rest
                ~f:(fun rest argument ->
                  checkpoint ();
                  argument :: rest))
         | TERecord fields ->
           let rest =
             List.fold fields ~init:rest ~f:(fun rest (name, syntax) ->
               syntax_name name;
               syntax :: rest)
           in
           walk rest
         | TEVariant cases ->
           let rest =
             List.fold cases ~init:rest ~f:(fun rest (name, arguments) ->
               syntax_name name;
               List.fold arguments ~init:rest ~f:(fun rest syntax ->
                 checkpoint ();
                 syntax :: rest))
           in
           walk rest)
    in
    walk [ declaration.body ];
    let parameters =
      List.map parameters ~f:(fun parameter ->
        checkpoint ();
        check_binding t parameter;
        admit t ~facts:0 ~edges:1 ~labels:0;
        Binding.symbol parameter)
    in
    admit t ~facts:1 ~edges:1 ~labels:0;
    t.aliases
    <- { Fact.binding = Binding.symbol binding; parameters; declaration } :: t.aliases)
;;

let stage_signature t ~implementation ~specification ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    let rec walk links = function
      | [] -> links
      | (implementation, specification) :: rest ->
        let select namespace entries links rest =
          List.fold
            entries
            ~init:(links, rest)
            ~f:(fun (links, rest) (name, specification) ->
              checkpoint ();
              check_binding t specification;
              match Binding.Members.find implementation ~namespace ~name ~checkpoint with
              | None -> raise (Rejected Foreign_binding)
              | Some implementation ->
                check_binding t implementation;
                admit t ~facts:1 ~edges:2 ~labels:0;
                let link =
                  { Fact.implementation = Binding.symbol implementation
                  ; specification = Binding.symbol specification
                  }
                in
                let rest =
                  match Binding.members implementation, Binding.members specification with
                  | Some implementation, Some specification ->
                    (implementation, specification) :: rest
                  | None, None | Some _, None -> rest
                  | None, Some _ -> raise (Rejected Foreign_binding)
                in
                link :: links, rest)
        in
        let links, rest =
          select Value (Binding.Members.values specification) links rest
        in
        let links, rest = select Type (Binding.Members.types specification) links rest in
        walk links rest
    in
    let links = walk [] [ implementation, specification ] in
    admit t ~facts:1 ~edges:0 ~labels:0;
    let stage = { Signature_stage.owner = t.owner; links; committed = false } in
    t.signatures <- stage :: t.signatures;
    stage)
;;

let validate_signature_stage t stage =
  if t.failed
  then Error Error.Failed_invocation
  else if t.settled
  then Error Error.Already_settled
  else if not (Private.Owner.equal t.owner stage.Signature_stage.owner)
  then Error Foreign_signature_stage
  else if stage.committed
  then Error Signature_already_committed
  else Ok ()
;;

let commit_signature_exn t stage =
  (* Success is nonallocating. This is an invariant guard, not an expected
     admission failure: the trusted caller validated after the last callback. *)
  if
    t.failed
    || t.settled
    || stage.Signature_stage.committed
    || not (Private.Owner.equal t.owner stage.owner)
  then invalid_arg "Semantic_recorder.commit_signature_exn: unvalidated activation";
  stage.Signature_stage.committed <- true
;;

let exports t unit members ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    check_unit t unit;
    let count entries =
      List.iter entries ~f:(fun (_, binding) ->
        checkpoint ();
        check_binding t binding;
        admit t ~facts:0 ~edges:1 ~labels:0)
    in
    count (Binding.Members.values members);
    count (Binding.Members.types members);
    admit t ~facts:1 ~edges:0 ~labels:0;
    t.exports
    <- { Fact.owner = Lookup.owner unit.context
       ; source = Captured.id unit.source
       ; members
       }
       :: t.exports)
;;

let host_requirement t unit ~contract ~name ~binding ~required ~checkpoint =
  run t ~checkpoint ~f:(fun _ ->
    check_unit t unit;
    check_binding t binding;
    check_root t required;
    admit t ~facts:1 ~edges:2 ~labels:(String.length name);
    t.host_requirements
    <- { Fact.contract; name; binding = Binding.symbol binding; required }
       :: t.host_requirements)
;;

let settle t ~checkpoint =
  run t ~checkpoint ~f:(fun checkpoint ->
    List.iter t.staged_groups ~f:(fun stage ->
      checkpoint ();
      if not stage.Group_stage.committed then raise (Rejected Unfinished_group));
    let reverse entries =
      List.fold entries ~init:[] ~f:(fun reversed entry ->
        checkpoint ();
        entry :: reversed)
    in
    let signature_links =
      List.fold t.signatures ~init:[] ~f:(fun links stage ->
        checkpoint ();
        if stage.Signature_stage.committed
        then
          List.fold stage.links ~init:links ~f:(fun links link ->
            checkpoint ();
            link :: links)
        else links)
    in
    let result =
      { Settled.groups = reverse t.groups
      ; bindings = reverse t.bindings
      ; contexts = reverse t.contexts
      ; references = reverse t.references
      ; regions = reverse t.regions
      ; aliases = reverse t.aliases
      ; signature_links
      ; exports = reverse t.exports
      ; host_requirements = reverse t.host_requirements
      }
    in
    checkpoint ();
    t.settled <- true;
    result)
;;
