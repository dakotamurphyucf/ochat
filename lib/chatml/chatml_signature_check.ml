module Syntax_name = Chatml.Chatml_syntax_name
open Core
open Chatml_inferred_type
module Syntax = Chatml.Chatml_module_signature
module Type_alias = Chatml_type_alias
module Probe = Chatml_type_probe

module Engine = struct
  type t =
    { instantiate : typ -> typ
    ; unify : typ -> typ -> (unit, string) result
    ; fresh_name : unit -> string
    ; checkpoint : unit -> unit
    }
end

module Visibility = Chatml.Chatml_module_visibility

module Error = struct
  type t = Chatml_diagnostic.t
end

type t =
  { values : typ Env.t
  ; types : Type_alias.t
  ; visibility : Visibility.t
  }

let values t = t.values
let types t = t.types
let visibility t = t.visibility

exception Rejected of Error.t

let alias_result = function
  | Ok value -> value
  | Error error -> raise (Rejected error)
;;

let check ?source ?lookup engine ~values ~types ~outer_types ~signature ~span =
  let { Engine.instantiate; unify; fresh_name; checkpoint } = engine in
  let reject span message =
    let diagnostic = Chatml_diagnostic.create ~span message in
    let diagnostic =
      match source with
      | None -> diagnostic
      | Some source -> Chatml.Chatml_owned_source.attach source diagnostic ~checkpoint
    in
    raise (Rejected diagnostic)
  in
  let located span = function
    | Ok value -> value
    | Error message -> reject span message
  in
  let rigids = ref String.Set.empty in
  let new_rigid_name () =
    let name = fresh_name () in
    rigids := Set.add !rigids name;
    name
  in
  let probe = Probe.create [ Module (Row (values, Empty_row)) ] ~checkpoint in
  let copied_values =
    match Probe.types probe with
    | [ Module (Row (fields, Empty_row)) ] -> fields
    | _ -> assert false
  in
  let match_type item_span ~name actual expected =
    match unify actual expected with
    | Ok () -> ()
    | Error message ->
      reject item_span (Printf.sprintf "Signature member '%s': %s" name message)
  in
  let rec check_module ~implementation ~implementation_types ~outer_types signature =
    let local_types = ref (Type_alias.enter_scope outer_types) in
    let selected = ref Env.empty in
    let type_names = ref [] in
    let module_names = ref [] in
    let visibility = ref [] in
    let implementation_value span name =
      if Map.mem !selected name
      then reject span (Printf.sprintf "Duplicate signature member '%s'" name);
      match Map.find implementation name with
      | Some value -> value
      | None -> reject span (Printf.sprintf "Module does not export '%s'" name)
    in
    List.iter signature ~f:(fun (item : Syntax.item) ->
      checkpoint ();
      match item.desc with
      | Type declaration ->
        local_types
        := Type_alias.declare ?source ?lookup !local_types declaration ~checkpoint
           |> alias_result;
        let count = List.length declaration.parameters in
        (match
           Type_alias.declared_arity
             implementation_types
             ~name:(Syntax_name.name declaration.name)
         with
         | Some arity when arity = count -> ()
         | Some arity ->
           reject
             item.span
             (Printf.sprintf
                "Type '%s' has %d parameter(s); signature requires %d"
                (Syntax_name.name declaration.name)
                arity
                count)
         | None ->
           reject
             item.span
             (Printf.sprintf
                "Module does not declare type '%s'"
                (Syntax_name.name declaration.name)));
        Type_alias.check_declared_equivalent
          ?source
          implementation_types
          ~other:!local_types
          ~name:(Syntax_name.name declaration.name)
          ~span:item.span
          ~checkpoint
        |> alias_result;
        type_names := Syntax_name.name declaration.name :: !type_names
      | Value { name; annotation; requirements } ->
        let name = Syntax_name.name name in
        let actual = implementation_value item.span name in
        let equality_parameters =
          List.map requirements ~f:(fun (requirement : Syntax.Requirement.t) ->
            checkpoint ();
            if not (String.equal (Syntax_name.name requirement.name) "equality")
            then
              reject
                requirement.span
                (Printf.sprintf
                   "Unknown signature constraint '%s'"
                   (Syntax_name.name requirement.name));
            Syntax_name.name requirement.parameter)
        in
        let expected =
          Type_alias.elaborate_scheme
            ?source
            ?lookup
            !local_types
            annotation
            ~equality_parameters
            ~fresh_name:new_rigid_name
            ~checkpoint
          |> alias_result
        in
        match_type item.span ~name (instantiate actual) expected;
        selected := Map.set !selected ~key:name ~data:expected;
        visibility := Visibility.Value name :: !visibility
      | Module { name; signature } ->
        let name = Syntax_name.name name in
        let actual = implementation_value item.span name in
        let fields =
          match actual with
          | Module (Row (fields, Empty_row)) -> fields
          | _ ->
            reject
              item.span
              (Printf.sprintf
                 "Signature member '%s' is not a statically known module"
                 name)
        in
        let nested_types =
          match Type_alias.declared_module implementation_types ~name with
          | Some types -> types
          | None ->
            reject
              item.span
              (Printf.sprintf "Missing type namespace for module '%s'" name)
        in
        let nested =
          check_module
            ~implementation:fields
            ~implementation_types:nested_types
            ~outer_types:!local_types
            signature
        in
        selected
        := Map.set !selected ~key:name ~data:(Module (Row (nested.values, Empty_row)));
        local_types := Type_alias.add_module !local_types ~name ~exports:nested.types;
        module_names := name :: !module_names;
        visibility := Visibility.Module (name, nested.visibility) :: !visibility);
    let types =
      Type_alias.exports
        !local_types
        ~type_names:!type_names
        ~module_names:!module_names
        ~checkpoint
      |> located span
    in
    { values = !selected; types; visibility = List.rev !visibility }
  in
  try
    let checked =
      check_module
        ~implementation:copied_values
        ~implementation_types:types
        ~outer_types
        signature
    in
    match Probe.commit probe ~rigid_names:!rigids ~checkpoint with
    | Ok () -> Ok checked
    | Error (Escaping_parameter _) ->
      reject span "Signature would generalize an existing monomorphic type variable"
    | Error Original_changed ->
      reject span "Implementation types changed during signature checking"
    | Error Already_committed -> assert false
  with
  | Rejected error -> Error error
;;

let check_captured interface ?lookup engine ~values ~types ~outer_types =
  let source = Chatml.Chatml_parse.Interface.source interface in
  let checkpoint = engine.Engine.checkpoint in
  (* This whole-unit range is constructed before checking or committing cells. *)
  let span =
    Chatml_source.locate
      source
      ~start_byte:0
      ~end_byte:(Chatml_source.byte_length source)
      ~checkpoint
    |> Or_error.ok_exn
    |> Chatml_source.Location.span
  in
  check
    ~source
    ?lookup
    engine
    ~values
    ~types
    ~outer_types
    ~signature:(Chatml.Chatml_parse.Interface.signature interface)
    ~span
;;
