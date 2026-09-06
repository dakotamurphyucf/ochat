open! Core
module Diagnostic = Chatmd_shell_spec.Diagnostic

type t =
  { source : Chatmd_shell_spec.Source_ref.t
  ; path : string list
  ; values : Chatmd_ast.attribute list
  }

let error source path code message = Diagnostic.error ~source ~path ~code message

let duplicate_name attributes =
  List.find_a_dup (List.map attributes ~f:fst) ~compare:String.compare
;;

let unknown_name attributes allowed =
  let allowed = String.Set.of_list allowed in
  List.find_map attributes ~f:(fun (name, _) ->
    Option.some_if (not (Set.mem allowed name)) name)
;;

let create ~source ~path ~allowed values =
  match duplicate_name values, unknown_name values allowed with
  | Some name, _ ->
    Error (error source path "shell.duplicate_attribute" ("duplicate attribute: " ^ name))
  | None, Some name ->
    Error (error source path "shell.unknown_attribute" ("unknown attribute: " ^ name))
  | None, None -> Ok { source; path; values }
;;

let find t name = List.Assoc.find t.values name ~equal:String.equal

let optional t name =
  match find t name with
  | None -> Ok None
  | Some (Some value) -> Ok (Some value)
  | Some None ->
    Error
      (error
         t.source
         (t.path @ [ name ])
         "shell.attribute_value_required"
         ("attribute requires a value: " ^ name))
;;

let required t name =
  match optional t name with
  | Error diagnostic -> Error diagnostic
  | Ok (Some value) when not (String.is_empty value) -> Ok value
  | Ok _ ->
    Error
      (error
         t.source
         (t.path @ [ name ])
         "shell.missing_attribute"
         ("missing required attribute: " ^ name))
;;

let to_values t =
  List.map t.values ~f:(fun (name, value) -> name, Option.value value ~default:"")
;;
