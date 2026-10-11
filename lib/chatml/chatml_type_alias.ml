open Core
open Chatml_inferred_type
module Syntax = Chatml.Chatml_type_expr

module Error = struct
  type t =
    { message : string
    ; span : Source.span
    }
end

module Alias = struct
  type t =
    | Closed of typ
    | Parameterized of
        { name : string
        ; parameters : string list
        ; body : Syntax.t
        ; environment : t String.Map.t
        }

  let arity = function
    | Closed _ -> 0
    | Parameterized { parameters; _ } -> List.length parameters
  ;;
end

type t = Alias.t String.Map.t

let empty = String.Map.empty

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
  else if Map.mem t name
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
    Ok (Map.set t ~key:name ~data:(Alias.Closed body)))
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
  and constructor syntax name arguments =
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
         (match Map.find t name with
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
    Map.set t ~key:name ~data:definition)
;;

let elaborate t syntax ~checkpoint =
  protect (fun () -> convert t ~parameters:String.Map.empty ~self:None ~checkpoint syntax)
;;
