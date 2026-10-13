open Core
module Public = Chatml_configured_resolution.Public_module
module Roots = Chatml_dependency_session.Roots
module Scope = Chatml_value_scope
module Inferred = Chatml_inferred_type
module Graph_copy = Chatml_type_graph_copy

module Limits = struct
  type t =
    { max_bindings : int
    ; copy : Graph_copy.Limits.t
    ; max_type_roots : int
    ; max_name_work : int
    ; max_name_bytes : int
    ; max_name_depth : int
    }

  let create
        ~max_bindings
        ~max_type_roots
        ~max_type_nodes
        ~max_variable_cells
        ~max_type_edges
        ~max_copy_work
        ~max_copy_depth
        ~max_name_work
        ~max_name_bytes
    =
    if max_bindings < 0 || max_name_work < 0 || max_name_bytes < 0
    then Or_error.error_string "REPL root/name limits must be nonnegative"
    else
      Result.map
        (Graph_copy.Limits.create
           ~max_roots:max_type_roots
           ~max_type_nodes
           ~max_variable_cells
           ~max_edges:max_type_edges
           ~max_work:max_copy_work
           ~max_depth:max_copy_depth)
        ~f:(fun copy ->
          { max_bindings
          ; copy
          ; max_type_roots
          ; max_name_work
          ; max_name_bytes
          ; max_name_depth = max_copy_depth
          })
  ;;

  let max_bindings t = t.max_bindings
end

module Error = struct
  type copy_dimension =
    | Roots
    | Type_nodes
    | Variable_cells
    | Edges
    | Work
    | Depth
  [@@deriving compare, equal, sexp_of]

  type t =
    | Binding_limit
    | Copy_limit of copy_dimension
    | Name_limit
    | Name_counter_overflow
    | Invalid_module_scheme
    | Invalid_cache_seed of Roots.Error.t
  [@@deriving sexp_of]
end

module Name_seed = struct
  type t =
    { next_current_id : int
    ; next_mu_id : int
    }

  let next_current_id t = t.next_current_id
  let next_mu_id t = t.next_mu_id
end

module Copy = struct
  type t =
    { lexical : Scope.t
    ; modules : Public.t Roots.t
    ; name_seed : Name_seed.t
    }

  let lexical t = t.lexical
  let modules t = t.modules
  let name_seed t = t.name_seed
end

let copy_dimension = function
  | Graph_copy.Error.Roots -> Error.Roots
  | Type_nodes -> Type_nodes
  | Variable_cells -> Variable_cells
  | Edges -> Edges
  | Work -> Work
  | Depth -> Depth
;;

let generated_current_id name ~step =
  let length = String.length name in
  if length = 0
  then Ok None
  else (
    let first = name.[0] in
    if Char.(first < 'a' || first > 'z')
    then Ok None
    else if length = 1
    then Ok (Some (Char.to_int first - Char.to_int 'a'))
    else (
      let suffix = ref 0 in
      let valid = ref true in
      for index = 1 to length - 1 do
        step ();
        let char = name.[index] in
        if Char.(char < '0' || char > '9')
        then valid := false
        else if !valid
        then (
          let digit = Char.to_int char - Char.to_int '0' in
          if !suffix > (Int.max_value - digit) / 10
          then valid := false
          else suffix := (!suffix * 10) + digit)
      done;
      if (not !valid) || !suffix = 0 || Char.equal name.[1] '0'
      then Ok None
      else (
        let letter = Char.to_int first - Char.to_int 'a' in
        if !suffix > (Int.max_value - letter) / 26
        then Error Error.Name_counter_overflow
        else Ok (Some ((!suffix * 26) + letter)))))
;;

let generated_mu_id name ~step =
  let length = String.length name in
  if length = 0 || (length > 1 && Char.equal name.[0] '0')
  then Ok None
  else (
    let value = ref 0 in
    let valid = ref true in
    for index = 0 to length - 1 do
      step ();
      let char = name.[index] in
      if Char.(char < '0' || char > '9')
      then valid := false
      else if !valid
      then (
        let digit = Char.to_int char - Char.to_int '0' in
        if !value > (Int.max_value - digit) / 10
        then valid := false
        else value := (!value * 10) + digit)
    done;
    if !valid then Ok (Some !value) else Ok None)
;;

let name_seed roots ~limits ~checkpoint =
  let exception Rejected of Error.t in
  let steps = ref 0 in
  let name_bytes = ref 0 in
  let names = ref String.Set.empty in
  let seen = ref [] in
  let next_current_id = ref 0 in
  let next_mu_id = ref 0 in
  let step () =
    if !steps >= limits.Limits.max_name_work then raise (Rejected Error.Name_limit);
    Int.incr steps;
    checkpoint ()
  in
  let reserve name ~recursive_name =
    step ();
    let length = String.length name in
    if length > limits.max_name_bytes then raise (Rejected Error.Name_limit);
    if not (Set.mem !names name)
    then (
      if length > limits.max_name_bytes - !name_bytes
      then raise (Rejected Error.Name_limit);
      name_bytes := !name_bytes + length;
      names := Set.add !names name);
    let parsed =
      if recursive_name
      then generated_mu_id name ~step
      else generated_current_id name ~step
    in
    match parsed with
    | Error error -> raise (Rejected error)
    | Ok None -> ()
    | Ok (Some id) ->
      if id = Int.max_value then raise (Rejected Error.Name_counter_overflow);
      let next = if recursive_name then next_mu_id else next_current_id in
      next := Int.max !next (id + 1)
  in
  let rec scan depth typ =
    step ();
    if depth > limits.max_name_depth then raise (Rejected Error.Name_limit);
    let child = scan (depth + 1) in
    match typ with
    | Inferred.Generic { name; _ } -> reserve name ~recursive_name:false
    | Var cell ->
      let already_seen =
        List.exists !seen ~f:(fun old ->
          step ();
          phys_equal old cell)
      in
      if not already_seen
      then (
        seen := cell :: !seen;
        match !cell with
        | Free { name; _ } -> reserve name ~recursive_name:false
        | Bound typ -> child typ)
    | Mu (name, body) ->
      reserve name ~recursive_name:true;
      child body
    | Rec_var name -> reserve name ~recursive_name:true
    | Fun (arguments, result) ->
      List.iter arguments ~f:child;
      child result
    | Con (_, arguments) | Tuple arguments | Variant_payload arguments ->
      List.iter arguments ~f:child
    | Row (fields, tail) ->
      Inferred.Env.iter fields ~f:(fun _ field -> child field);
      child tail
    | Ref typ | Record typ | Module typ | Variant typ | Array typ -> child typ
    | Field_receiver (kind, row) ->
      child kind;
      child row
    | Shape_kind _ | TInt | TFloat | Boolean | String | Empty_row | Unit -> ()
  in
  try
    List.iter roots ~f:(scan 1);
    checkpoint ();
    Ok { Name_seed.next_current_id = !next_current_id; next_mu_id = !next_mu_id }
  with
  | Rejected error -> Error error
;;

let reverse_with_checkpoint values ~checkpoint =
  List.fold values ~init:[] ~f:(fun reversed value ->
    checkpoint ();
    value :: reversed)
;;

let append_with_checkpoint left right ~checkpoint =
  List.fold (reverse_with_checkpoint left ~checkpoint) ~init:right ~f:(fun tail value ->
    checkpoint ();
    value :: tail)
;;

let copy_detached ~lexical ~modules ~(limits : Limits.t) ~checkpoint =
  let open Result.Let_syntax in
  let%bind captured =
    Scope.Roots.capture lexical ~max_bindings:limits.max_bindings ~checkpoint
    |> Result.map_error ~f:(function `Binding_limit -> Error.Binding_limit)
  in
  let lexical_roots = Scope.Roots.schemes captured in
  let rows = Roots.checked_in_order modules in
  let lexical_count = Scope.Roots.count captured in
  if lexical_count > limits.max_type_roots
  then Error (Error.Copy_limit Error.Roots)
  else (
    let rec collect rows count reversed =
      checkpoint ();
      match rows with
      | [] -> Ok (reverse_with_checkpoint reversed ~checkpoint)
      | (_, public) :: tail ->
        if count >= limits.max_type_roots
        then Error (Error.Copy_limit Error.Roots)
        else collect tail (count + 1) (Public.value_scheme public :: reversed)
    in
    let%bind public_roots = collect rows lexical_count [] in
    let roots = append_with_checkpoint lexical_roots public_roots ~checkpoint in
    let%bind copied =
      Graph_copy.copy_detached roots ~limits:limits.copy ~checkpoint
      |> Result.map_error ~f:(fun (Graph_copy.Error.Limit_exceeded dimension) ->
        Error.Copy_limit (copy_dimension dimension))
    in
    let%bind name_seed = name_seed copied ~limits ~checkpoint in
    let rec split count copied lexical_rev =
      checkpoint ();
      if count = 0
      then Ok (reverse_with_checkpoint lexical_rev ~checkpoint, copied)
      else (
        match copied with
        | [] -> Error (Error.Copy_limit Error.Roots)
        | root :: rest -> split (count - 1) rest (root :: lexical_rev))
    in
    let%bind lexical_schemes, public_schemes = split lexical_count copied [] in
    let%bind lexical =
      Scope.Roots.with_schemes captured lexical_schemes ~checkpoint
      |> Result.map_error ~f:(function `Wrong_root_count -> Error.Copy_limit Error.Roots)
    in
    let rec rebind rows schemes reversed =
      checkpoint ();
      match rows, schemes with
      | [], [] -> Ok (reverse_with_checkpoint reversed ~checkpoint)
      | (id, public) :: rows, scheme :: schemes ->
        let%bind public =
          Public.with_value_scheme public scheme
          |> Result.map_error ~f:(function `Invalid_module_scheme ->
              Error.Invalid_module_scheme)
        in
        rebind rows schemes ((id, public) :: reversed)
      | _ -> Error (Error.Copy_limit Error.Roots)
    in
    let%bind checked = rebind rows public_schemes [] in
    let%map modules =
      Roots.rebind_checked modules ~checked ~checkpoint
      |> Result.map_error ~f:(fun error -> Error.Invalid_cache_seed error)
    in
    { Copy.lexical; modules; name_seed })
;;
