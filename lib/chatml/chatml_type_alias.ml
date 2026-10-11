open Core
open Chatml_inferred_type
module Syntax = Chatml.Chatml_type_expr
module Equivalence = Chatml_alias_equivalence

module Error = struct
  type t =
    { message : string
    ; span : Source.span
    }
end

module Alias = struct
  type 'scope t =
    | Closed of typ
    | Parameterized of
        { name : string
        ; parameters : string list
        ; body : Syntax.t
        ; environment : 'scope
        }

  let arity = function
    | Closed _ -> 0
    | Parameterized { parameters; _ } -> List.length parameters
  ;;
end

type t =
  { aliases : t Alias.t String.Map.t
  ; modules : t String.Map.t
  ; opened_aliases : t Alias.t String.Map.t
  ; opened_modules : t String.Map.t
  ; outer : t option
  }

let empty =
  { aliases = String.Map.empty
  ; modules = String.Map.empty
  ; opened_aliases = String.Map.empty
  ; opened_modules = String.Map.empty
  ; outer = None
  }
;;

let enter_scope t = { empty with outer = Some t }

let add_module t ~name ~exports =
  { t with modules = Map.set t.modules ~key:name ~data:exports }
;;

let declared_arity t ~name = Option.map (Map.find t.aliases name) ~f:Alias.arity
let declared_module t ~name = Map.find t.modules name

let exports t ~type_names ~module_names ~checkpoint =
  let select bindings names =
    List.fold_result names ~init:String.Map.empty ~f:(fun selected name ->
      checkpoint ();
      match Map.find bindings name with
      | None -> Error (Printf.sprintf "Unknown local type binding '%s'" name)
      | Some binding -> Ok (Map.set selected ~key:name ~data:binding))
  in
  let open Result.Let_syntax in
  let%bind aliases = select t.aliases type_names in
  let%map modules = select t.modules module_names in
  { empty with aliases; modules }
;;

let rec find_visible t ~bindings ~opened ~checkpoint name =
  checkpoint ();
  match Map.find (bindings t) name with
  | Some _ as found -> found
  | None ->
    (match Map.find (opened t) name with
     | Some _ as found -> found
     | None ->
       Option.bind t.outer ~f:(fun outer ->
         find_visible outer ~bindings ~opened ~checkpoint name))
;;

let find_module t ~checkpoint name =
  find_visible
    t
    ~bindings:(fun t -> t.modules)
    ~opened:(fun t -> t.opened_modules)
    ~checkpoint
    name
;;

let find_local_alias t ~checkpoint name =
  find_visible
    t
    ~bindings:(fun t -> t.aliases)
    ~opened:(fun t -> t.opened_aliases)
    ~checkpoint
    name
;;

let find_alias t ~checkpoint ({ Syntax.Name.modules; name } : Syntax.Name.t) =
  match modules with
  | [] -> find_local_alias t ~checkpoint name
  | first :: rest ->
    List.fold rest ~init:(find_module t ~checkpoint first) ~f:(fun scope name ->
      checkpoint ();
      Option.bind scope ~f:(fun scope -> Map.find scope.modules name))
    |> Option.bind ~f:(fun scope -> Map.find scope.aliases name)
;;

let open_module t ~name ~checkpoint =
  match find_module t ~checkpoint name with
  | None -> Ok t
  | Some scope ->
    let type_conflict =
      List.find (Map.keys scope.aliases) ~f:(fun name ->
        checkpoint ();
        Option.is_some (find_local_alias t ~checkpoint name))
    in
    let conflict =
      match type_conflict with
      | Some _ -> type_conflict
      | None ->
        List.find (Map.keys scope.modules) ~f:(fun name ->
          checkpoint ();
          Option.is_some (find_module t ~checkpoint name))
    in
    (match conflict with
     | Some member ->
       Error
         (Printf.sprintf "open %s would shadow existing type binding '%s'" name member)
     | None ->
       Ok
         { t with
           opened_aliases =
             Map.merge_skewed
               t.opened_aliases
               scope.aliases
               ~combine:(fun ~key:_ _ right -> right)
         ; opened_modules =
             Map.merge_skewed
               t.opened_modules
               scope.modules
               ~combine:(fun ~key:_ _ right -> right)
         })
;;

let primitive = function
  | "int" -> Some TInt
  | "float" -> Some TFloat
  | "bool" -> Some Boolean
  | "string" -> Some String
  | "unit" -> Some Unit
  | _ -> None
;;

let available t name =
  if Option.is_some (primitive name)
  then Error (Printf.sprintf "Cannot redefine primitive type '%s'" name)
  else if Map.mem t.aliases name
  then Error (Printf.sprintf "Duplicate type declaration '%s'" name)
  else Ok ()
;;

(* Alias templates are finite, immutable type trees. Inference variables must
   never be installed: reusing one would couple independent annotations. *)
let rec template_is_immutable ~checkpoint ty =
  checkpoint ();
  let template_is_immutable = template_is_immutable ~checkpoint in
  match ty with
  | Var _ | Generic _ -> false
  | Fun (parameters, result) ->
    List.for_all parameters ~f:template_is_immutable && template_is_immutable result
  | Con (_, arguments) | Tuple arguments | Variant_payload arguments ->
    List.for_all arguments ~f:template_is_immutable
  | Mu (_, body) | Ref body | Record body | Module body | Variant body | Array body ->
    template_is_immutable body
  | Field_receiver (kind, row) -> template_is_immutable kind && template_is_immutable row
  | Row (fields, tail) ->
    Map.for_all fields ~f:template_is_immutable && template_is_immutable tail
  | Rec_var _ | Shape_kind _ | TInt | TFloat | Boolean | String | Empty_row | Unit -> true
;;

let add_host t ~name ~body ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = available t name in
  if template_is_immutable ~checkpoint body
  then (
    let%bind () = Chatml_type_validity.check_contractive ~checkpoint body in
    Ok { t with aliases = Map.set t.aliases ~key:name ~data:(Alias.Closed body) })
  else
    Error
      (Printf.sprintf
         "Host type alias '%s' contains unbound parameters or mutable inference variables"
         name)
;;

exception Invalid of Error.t

let reject (syntax : Syntax.t) message = raise (Invalid { message; span = syntax.span })

let protect f =
  try Ok (f ()) with
  | Invalid error -> Error error
;;

(* Argument types can refer to the alias currently being declared. Retain
   lexical scopes when finding those free names, so an applied recursive alias
   can choose a binder that does not capture them. *)
let recursive_names ~checkpoint types =
  let rec collect scope (free, all) ty =
    checkpoint ();
    match ty with
    | Rec_var name ->
      (if Set.mem scope name then free else Set.add free name), Set.add all name
    | Mu (name, body) -> collect (Set.add scope name) (free, Set.add all name) body
    | Fun (parameters, result) ->
      List.fold (result :: parameters) ~init:(free, all) ~f:(collect scope)
    | Con (_, arguments) | Tuple arguments | Variant_payload arguments ->
      List.fold arguments ~init:(free, all) ~f:(collect scope)
    | Ref body | Record body | Module body | Variant body | Array body ->
      collect scope (free, all) body
    | Field_receiver (kind, row) -> collect scope (collect scope (free, all) kind) row
    | Row (fields, tail) ->
      Map.fold
        fields
        ~init:(collect scope (free, all) tail)
        ~f:(fun ~key:_ ~data acc -> collect scope acc data)
    | Var _ -> invalid_arg "Alias template contains an inference variable"
    | Generic _ | Shape_kind _ | TInt | TFloat | Boolean | String | Empty_row | Unit ->
      free, all
  in
  List.fold types ~init:(String.Set.empty, String.Set.empty) ~f:(collect String.Set.empty)
;;

let recursive_body ~checkpoint ~binder body =
  let free, _ = recursive_names ~checkpoint [ body ] in
  if Set.mem free binder then Mu (binder, body) else body
;;

let application_binder ~checkpoint name arguments =
  let free, all = recursive_names ~checkpoint arguments in
  if not (Set.mem free name)
  then name
  else (
    let rec choose index =
      checkpoint ();
      let candidate = name ^ "$" ^ Int.to_string index in
      if Set.mem all candidate then choose (index + 1) else candidate
    in
    choose 0)
;;

let ensure_unique syntax ~what ~checkpoint names =
  let seen = Hash_set.create (module String) in
  List.iter names ~f:(fun name ->
    checkpoint ();
    if Hash_set.mem seen name
    then reject syntax (Printf.sprintf "Duplicate %s '%s'" what name);
    Hash_set.add seen name)
;;

module Self = struct
  type t =
    { name : string
    ; parameters : string list
    ; binder : string
    ; check_regular : bool
    }
end

let parameter_name name = "'" ^ name

let rec convert (t : t) ~parameters ~self ~checkpoint syntax =
  let rec expression (syntax : Syntax.t) =
    checkpoint ();
    match syntax.desc with
    | TEName name -> constructor syntax name []
    | TEParameter name ->
      (match Map.find parameters name with
       | Some ty -> ty
       | None -> reject syntax (Printf.sprintf "Unbound type parameter '%s" name))
    | TEConstr (name, arguments) ->
      constructor syntax name (List.map arguments ~f:expression)
    | TEFunction (parameters, result) ->
      Fun (List.map parameters ~f:expression, expression result)
    | TEArrow (lhs, rhs) ->
      let lhs = expression lhs in
      let parameters, result =
        match expression rhs with
        | Fun (parameters, result) -> parameters, result
        | result -> [], result
      in
      (match lhs with
       | Unit -> Fun (parameters, result)
       | _ -> Fun (lhs :: parameters, result))
    | TERecord fields ->
      ensure_unique
        syntax
        ~what:"type record field label"
        ~checkpoint
        (List.map fields ~f:fst);
      Record
        (Row
           ( String.Map.of_alist_exn
               (List.map fields ~f:(fun (name, body) -> name, expression body))
           , Empty_row ))
    | TEVariant cases ->
      ensure_unique
        syntax
        ~what:"type variant constructor label"
        ~checkpoint
        (List.map cases ~f:fst);
      Variant
        (Row
           ( String.Map.of_alist_exn
               (List.map cases ~f:(fun (name, bodies) ->
                  name, Variant_payload (List.map bodies ~f:expression)))
           , Empty_row ))
  and constructor syntax (qualified : Syntax.Name.t) arguments =
    let name = Syntax.Name.to_string qualified in
    let arity expected =
      let actual = List.length arguments in
      if actual <> expected
      then
        reject
          syntax
          (Printf.sprintf
             "Type '%s' expects %d argument(s), received %d"
             name
             expected
             actual)
    in
    (* A zero-parameter alias may share a name with a built-in constructor.
       Applied [array] and [task] still name the built-in, including inside
       declarations such as [type array = int array]. *)
    let is_builtin_application =
      (not (List.is_empty arguments))
      && (String.equal name "array" || String.equal name "task")
    in
    match self with
    | Some ({ Self.name = self_name; parameters; binder; check_regular } : Self.t)
      when String.equal name self_name && not is_builtin_application ->
      arity (List.length parameters);
      if
        check_regular
        && not
             (List.for_all2_exn parameters arguments ~f:(fun parameter argument ->
                match argument with
                | Generic { name; equality = None } ->
                  String.equal name (parameter_name parameter)
                | _ -> false))
      then
        reject
          syntax
          (Printf.sprintf "Recursive type '%s' must preserve its parameters" name);
      Rec_var binder
    | _ ->
      (match primitive name, name, arguments with
       | Some ty, _, _ ->
         arity 0;
         ty
       | None, "array", _ :: _ ->
         arity 1;
         Array (List.hd_exn arguments)
       | None, "task", _ :: _ ->
         arity 1;
         Con ("task", arguments)
       | None, _, _ ->
         (match find_alias t ~checkpoint qualified with
          | None -> reject syntax (Printf.sprintf "Unknown type '%s'" name)
          | Some alias ->
            arity (Alias.arity alias);
            apply alias ~arguments ~checkpoint))
  in
  expression syntax

and apply alias ~arguments ~checkpoint =
  match alias with
  | Alias.Closed body -> body
  | Parameterized { name; parameters; body; environment } ->
    let binder = application_binder ~checkpoint name arguments in
    let bindings = String.Map.of_alist_exn (List.zip_exn parameters arguments) in
    (* A declaration has already passed its abstract-parameter regularity check.
       Elaborate in the captured namespace with concrete arguments: legacy arrow
       unit erasure and RHS flattening depend on those arguments. *)
    convert
      environment
      ~parameters:bindings
      ~self:(Some { Self.name; parameters; binder; check_regular = false })
      ~checkpoint
      body
    |> recursive_body ~checkpoint ~binder
;;

let declare
      t
      ({ Syntax.Declaration.name; parameters; body } : Syntax.Declaration.t)
      ~checkpoint
  =
  protect (fun () ->
    checkpoint ();
    (match available t name with
     | Ok () -> ()
     | Error message -> reject body message);
    if
      (not (List.is_empty parameters))
      && List.mem [ "array"; "task" ] name ~equal:String.equal
    then reject body (Printf.sprintf "Cannot redefine builtin type constructor '%s'" name);
    ensure_unique body ~what:"type parameter" ~checkpoint parameters;
    let template =
      convert
        t
        ~parameters:
          (String.Map.of_alist_exn
             (List.map parameters ~f:(fun parameter ->
                parameter, Generic { name = parameter_name parameter; equality = None })))
        ~self:(Some { Self.name; parameters; binder = name; check_regular = true })
        ~checkpoint
        body
    in
    let template = recursive_body ~checkpoint ~binder:name template in
    (match Chatml_type_validity.check_contractive ~checkpoint template with
     | Ok () -> ()
     | Error message -> reject body message);
    let definition =
      if List.is_empty parameters
      then Alias.Closed template
      else Alias.Parameterized { name; parameters; body; environment = t }
    in
    { t with aliases = Map.set t.aliases ~key:name ~data:definition })
;;

let elaborate t syntax ~checkpoint =
  protect (fun () -> convert t ~parameters:String.Map.empty ~self:None ~checkpoint syntax)
;;

(* Unlike [convert], this expansion retains the conditional behavior of legacy
   arrows. Alias equality and universal specification arity checks share this
   symbolic traversal; it never produces an inference type. *)
let symbolic_expansion ~fresh_binder ~checkpoint =
  let rec expression environment parameters self (syntax : Syntax.t) =
    checkpoint ();
    let expression = expression environment parameters self in
    match syntax.desc with
    | TEName name -> constructor environment self syntax name []
    | TEParameter name ->
      (match Map.find parameters name with
       | Some argument -> argument
       | None -> reject syntax (Printf.sprintf "Unbound type parameter '%s" name))
    | TEConstr (name, arguments) ->
      constructor environment self syntax name (List.map arguments ~f:expression)
    | TEFunction (parameters, result) ->
      Equivalence.Function (List.map parameters ~f:expression, expression result)
    | TEArrow (left, right) ->
      Equivalence.arrow (expression left) ~result:(expression right) ~checkpoint
    | TERecord fields ->
      Equivalence.Node
        ( Record
        , [ Equivalence.Row
              ( String.Map.of_alist_exn
                  (List.map fields ~f:(fun (name, body) -> name, expression body))
              , Equivalence.Node (Empty_row, []) )
          ] )
    | TEVariant cases ->
      Equivalence.Node
        ( Variant
        , [ Equivalence.Row
              ( String.Map.of_alist_exn
                  (List.map cases ~f:(fun (name, bodies) ->
                     ( name
                     , Equivalence.Node (Variant_payload, List.map bodies ~f:expression) )))
              , Equivalence.Node (Empty_row, []) )
          ] )
  and constructor environment self syntax qualified arguments =
    let name = Syntax.Name.to_string qualified in
    let builtin_application =
      (not (List.is_empty arguments))
      && (String.equal name "array" || String.equal name "task")
    in
    match self with
    | Some (self_name, binder) when String.equal name self_name && not builtin_application
      -> Equivalence.Recursive_variable binder
    | _ ->
      (match primitive name, name, arguments with
       | Some ty, _, _ -> closed syntax ty
       | None, "array", _ :: _ -> Equivalence.Node (Array, arguments)
       | None, "task", _ :: _ -> Equivalence.Node (Con "task", arguments)
       | None, _, _ ->
         (match find_alias environment ~checkpoint qualified with
          | Some alias -> apply alias arguments
          | None -> reject syntax (Printf.sprintf "Unknown type '%s'" name)))
  and closed syntax ty =
    match Equivalence.of_closed_type ty ~fresh_binder ~checkpoint with
    | Ok ty -> ty
    | Error message -> reject syntax message
  and apply alias arguments =
    checkpoint ();
    match alias with
    | Alias.Closed ty ->
      (* Closed templates are already validated. Their source location is never
         needed for a projection failure, which is an internal invariant. *)
      (match Equivalence.of_closed_type ty ~fresh_binder ~checkpoint with
       | Ok ty -> ty
       | Error message -> failwith message)
    | Parameterized { name; parameters; body; environment } ->
      let binder = fresh_binder () in
      let parameters = String.Map.of_alist_exn (List.zip_exn parameters arguments) in
      expression environment parameters (Some (name, binder)) body
      |> Equivalence.recursive ~binder ~checkpoint
  in
  expression, apply
;;

let symbolic_application alias ~arguments ~fresh_binder ~checkpoint =
  let _, apply = symbolic_expansion ~fresh_binder ~checkpoint in
  apply alias arguments
;;

let require_fixed_function_arity syntax expanded ~checkpoint =
  let reject_ambiguous () =
    reject
      syntax
      "Universal value specifications require fixed function arity; use explicit \
       fun(...) -> ... for parameter-dependent legacy arrows"
  in
  let rec check expanded =
    checkpoint ();
    match expanded with
    | Equivalence.Parameter _ | Recursive_variable _ -> ()
    | Function (parameters, result) ->
      List.iter parameters ~f:check;
      check result
    | Legacy_function (arguments, result) ->
      List.iter arguments ~f:(function
        | Equivalence.Required argument -> check argument
        | Unless_unit _ -> reject_ambiguous ());
      (match result with
       | Equivalence.Direct result -> check result
       | Flatten _ -> reject_ambiguous ())
    | Node (_, arguments) -> List.iter arguments ~f:check
    | Row (fields, tail) ->
      Map.iter fields ~f:check;
      check tail
    | Recursive (_, body) -> check body
  in
  check expanded
;;

let elaborate_scheme t syntax ~equality_parameters ~fresh_name ~checkpoint =
  protect (fun () ->
    let rec collect names (syntax : Syntax.t) =
      checkpoint ();
      match syntax.desc with
      | TEName _ -> names
      | TEParameter name -> Set.add names name
      | TEArrow (left, right) -> collect (collect names left) right
      | TEFunction (parameters, result) ->
        List.fold parameters ~init:(collect names result) ~f:collect
      | TEConstr (_, arguments) -> List.fold arguments ~init:names ~f:collect
      | TERecord fields ->
        List.fold fields ~init:names ~f:(fun names (_, field) -> collect names field)
      | TEVariant cases ->
        List.fold cases ~init:names ~f:(fun names (_, arguments) ->
          List.fold arguments ~init:names ~f:collect)
    in
    let names = collect String.Set.empty syntax in
    ensure_unique syntax ~what:"equality requirement" ~checkpoint equality_parameters;
    let equality_names =
      List.fold equality_parameters ~init:String.Set.empty ~f:(fun equalities name ->
        checkpoint ();
        if not (Set.mem names name)
        then
          reject syntax (Printf.sprintf "Equality parameter '%s is not quantified" name);
        Set.add equalities name)
    in
    let parameters =
      Set.fold names ~init:String.Map.empty ~f:(fun parameters name ->
        checkpoint ();
        let equality =
          if Set.mem equality_names name then Some Equality_requirement.Value else None
        in
        Map.set parameters ~key:name ~data:(Generic { name = fresh_name (); equality }))
    in
    let declared = convert t ~parameters ~self:None ~checkpoint syntax in
    let symbolic_parameters =
      Set.to_list names
      |> List.mapi ~f:(fun index name -> name, Equivalence.Parameter index)
      |> String.Map.of_alist_exn
    in
    let next_binder = ref 0 in
    let fresh_binder () =
      checkpoint ();
      let binder = !next_binder in
      Int.incr next_binder;
      binder
    in
    let expression, _ = symbolic_expansion ~fresh_binder ~checkpoint in
    let expanded = expression t symbolic_parameters None syntax in
    require_fixed_function_arity syntax expanded ~checkpoint;
    declared)
;;

let check_declared_equivalent t ~other ~name ~checkpoint =
  checkpoint ();
  match Map.find t.aliases name, Map.find other.aliases name with
  | Some actual, Some expected ->
    let arity = Alias.arity actual in
    if arity <> Alias.arity expected
    then Error (Printf.sprintf "Type alias '%s' has different parameter counts" name)
    else (
      let next_binder = ref 0 in
      let fresh_binder () =
        checkpoint ();
        let binder = !next_binder in
        Int.incr next_binder;
        binder
      in
      let arguments = List.init arity ~f:(fun index -> Equivalence.Parameter index) in
      let expand alias =
        symbolic_application alias ~arguments ~fresh_binder ~checkpoint
      in
      match
        protect (fun () ->
          Equivalence.equivalent (expand actual) (expand expected) ~checkpoint)
      with
      | Error error -> Error error.message
      | Ok true -> Ok ()
      | Ok false ->
        Error
          (Printf.sprintf
             "Cannot establish universal equivalence of type alias '%s': normalized \
              symbolic expansions differ"
             name))
  | None, _ | _, None -> Error (Printf.sprintf "Module does not declare type '%s'" name)
;;
