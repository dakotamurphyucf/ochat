open Core
module B = Chatml_builtin_spec
module S = Chatml_builtin_surface
module E = Chatml_extension_surface

type kind =
  | Global
  | Module_export
  | Type_alias
  | Entrypoint
[@@deriving equal, sexp_of]

type item =
  { kind : kind
  ; name : string
  ; scheme : B.ty
  }
[@@deriving sexp_of]

type t =
  { surface_id : string
  ; modules : string list
  ; items : item list
  }
[@@deriving sexp_of]

let kind_name = function
  | Global -> "global"
  | Module_export -> "module_export"
  | Type_alias -> "type_alias"
  | Entrypoint -> "entrypoint"
;;

let unique namespace names =
  match List.find_a_dup names ~compare:String.compare with
  | None -> Ok ()
  | Some name -> Error (sprintf "duplicate %s name: %s" namespace name)
;;

let of_surface ~surface_id ~entrypoints (surface : S.surface) =
  let open Result.Let_syntax in
  let%bind () =
    match String.is_empty surface_id with
    | true -> Error "surface identity must not be empty"
    | false -> Ok ()
  in
  let modules = List.map surface.modules ~f:(fun (m : B.builtin_module) -> m.name) in
  let%bind () =
    unique
      "value/module"
      (modules @ List.map surface.globals ~f:(fun (v : B.builtin) -> v.name))
  in
  let%bind () =
    unique
      "type alias"
      (List.map surface.type_aliases ~f:(fun (a : S.builtin_type_alias) -> a.name))
  in
  let%bind () = unique "entrypoint" (List.map entrypoints ~f:fst) in
  let%bind exports =
    List.map surface.modules ~f:(fun (m : B.builtin_module) ->
      let%map () =
        unique
          ("export in " ^ m.name)
          (List.map m.exports ~f:(fun (v : B.builtin) -> v.name))
      in
      List.map m.exports ~f:(fun (v : B.builtin) ->
        { kind = Module_export; name = m.name ^ "." ^ v.name; scheme = v.scheme }))
    |> Result.all
    |> Result.map ~f:List.concat
  in
  let items =
    exports
    @ List.map surface.globals ~f:(fun (v : B.builtin) ->
      { kind = Global; name = v.name; scheme = v.scheme })
    @ List.map surface.type_aliases ~f:(fun (a : S.builtin_type_alias) ->
      { kind = Type_alias; name = a.name; scheme = a.body })
    @ List.map entrypoints ~f:(fun (name, scheme) -> { kind = Entrypoint; name; scheme })
  in
  let items =
    List.sort items ~compare:(fun left right ->
      match String.compare (kind_name left.kind) (kind_name right.kind) with
      | 0 -> String.compare left.name right.name
      | order -> order)
  in
  Ok { surface_id; modules = List.sort modules ~compare:String.compare; items }
;;

let standard () =
  [ "core", S.core_surface, []
  ; "moderator", S.moderator_surface, []
  ; "ui_moderator", S.ui_moderator_surface, []
  ; "shell_context", S.shell_context_surface, []
  ; "shell_matcher", S.shell_matcher_surface, []
  ; "shell_reviewer", S.shell_reviewer_surface, []
  ; "shell_before_interceptor", S.shell_before_interceptor_surface, []
  ; "shell_after_interceptor", S.shell_after_interceptor_surface, []
  ; "shell_effect", S.shell_effect_surface, []
  ; "shell_audit", S.shell_audit_surface, []
  ; "one_off_v1", E.one_off_v1, E.one_off_entrypoints
  ; "tool_v1", E.tool_v1, E.tool_entrypoints
  ; "moderator_v1", E.moderator_v1, E.moderator_entrypoints
  ; "delegated_moderator_v1", E.delegated_moderator_v1, E.moderator_entrypoints
  ]
  |> List.map ~f:(fun (surface_id, surface, entrypoints) ->
    of_surface ~surface_id ~entrypoints surface)
  |> Result.all
;;

let to_json t =
  `Object
    [ "version", `Number "1"
    ; "surface", `String t.surface_id
    ; "scheme_format", `String "chatml-builtin-sexp-v1"
    ; "modules", `Array (List.map t.modules ~f:(fun name -> `String name))
    ; ( "items"
      , `Array
          (List.map t.items ~f:(fun item ->
             `Object
               [ "kind", `String (kind_name item.kind)
               ; "name", `String item.name
               ; "scheme", `String (B.sexp_of_ty item.scheme |> Sexp.to_string_mach)
               ])) )
    ]
;;

let reference_notation =
  "Reference notation, not copyable ChatML type annotations: (a, b) -> c is one \
   two-argument function called f(x, y); () -> c is a zero-argument function. 'a is a \
   shared type variable within a signature; ..'r is an open row tail. array<t>, ref<t> \
   and name<t> describe type constructors. mu r. body binds a recursive type; r refers \
   to that binder. A variant `Tag(a, b) has two payload arguments; payload(a, b) \
   elsewhere describes internal payload structure, not arbitrary tuple expressions. \
   Named aliases are defined in type_alias entries. Entrypoints are definitions your \
   script supplies. Builtin signatures describe compiler support, not permission to \
   execute effects."
;;

let reference_items t =
  let key ty = B.sexp_of_ty ty |> Sexp.to_string_mach in
  let aliases =
    List.filter_map t.items ~f:(fun item ->
      match item.kind with
      | Type_alias -> Some (key item.scheme, item.name)
      | Global | Module_export | Entrypoint -> None)
    |> String.Map.of_alist_reduce ~f:(fun a b ->
      match Int.compare (String.length a) (String.length b) with
      | 0 -> String.min a b
      | n when n < 0 -> a
      | _ -> b)
  in
  let rec render ~binders ~expand ty =
    let alias =
      match expand with
      | true -> None
      | false -> Map.find aliases (key ty)
    in
    match alias with
    | Some name -> name
    | None ->
      let nested = render ~binders ~expand:false in
      let args xs = String.concat ~sep:", " (List.map xs ~f:nested) in
      (match ty with
       | B.TVar name -> "'" ^ name
       | TCon (name, []) -> name
       | TCon (name, xs) -> name ^ "<" ^ args xs ^ ">"
       | TInt -> "int"
       | TFloat -> "float"
       | TBool -> "bool"
       | TString -> "string"
       | TUnit -> "unit"
       | TArray item -> "array<" ^ nested item ^ ">"
       | TRef item -> "ref<" ^ nested item ^ ">"
       | TTuple xs -> "payload(" ^ args xs ^ ")"
       | TFun (xs, result) -> "(" ^ args xs ^ ") -> " ^ nested result
       | TRecord row -> "{ " ^ render_row ~binders ~variant:false row ^ " }"
       | TVariant row -> "[ " ^ render_row ~binders ~variant:true row ^ " ]"
       | TMu (name, body) ->
         let display = "rec" ^ Int.to_string (List.length binders) in
         "mu "
         ^ display
         ^ ". "
         ^ render ~binders:((name, display) :: binders) ~expand:false body
       | TRec_var name ->
         List.Assoc.find binders name ~equal:String.equal |> Option.value ~default:name)
  and render_row ~binders ~variant row =
    let rec flatten fields = function
      | B.TRow_empty -> List.rev fields, None
      | TRow_var name -> List.rev fields, Some name
      | TRow_extend (next, tail) -> flatten (List.rev_append next fields) tail
    in
    let fields, tail = flatten [] row in
    let nested = render ~binders ~expand:false in
    let fields =
      List.map fields ~f:(fun (name, ty) ->
        match variant, ty with
        | false, _ -> name ^ ": " ^ nested ty
        | true, B.TUnit -> "`" ^ name
        | true, TTuple args ->
          "`" ^ name ^ "(" ^ String.concat ~sep:", " (List.map args ~f:nested) ^ ")"
        | true, _ -> "`" ^ name ^ "(" ^ nested ty ^ ")")
    in
    let parts = fields @ Option.to_list (Option.map tail ~f:(fun name -> "..'" ^ name)) in
    String.concat
      ~sep:
        (match variant with
         | true -> " | "
         | false -> "; ")
      parts
  in
  List.map t.items ~f:(fun item ->
    let expand =
      match item.kind with
      | Type_alias -> true
      | _ -> false
    in
    `Object
      [ "kind", `String (kind_name item.kind)
      ; "name", `String item.name
      ; "signature", `String (render ~binders:[] ~expand item.scheme)
      ])
;;
