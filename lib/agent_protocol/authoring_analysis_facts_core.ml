open! Core
open Authoring_analysis_location

module Diagnostic = struct
  module Related = struct
    type t =
      { site : Site.t
      ; message : string
      }

    let create ~site ~message =
      let* message = bounded_string ~max:4096 message in
      Ok { site; message }
    ;;

    let site t = t.site
    let message t = t.message
    let to_json t = obj [ "site", Site.to_json t.site; "message", str t.message ]

    let of_json json =
      let* fields = C.fields ~allowed:[ "site"; "message" ] json in
      let* site = field fields "site" Site.of_json in
      let* message = field fields "message" (C.string ~max_bytes:4096) in
      create ~site ~message
    ;;
  end

  type t =
    { message : string
    ; site : Site.t option
    ; related : Related.t list
    }

  let create ~message ~site ~related =
    let* message = bounded_string ~max:4096 message in
    let rec bounded remaining = function
      | [] -> Ok { message; site; related }
      | _ when remaining = 0 -> C.invalid "too many related diagnostic sites"
      | _ :: rest -> bounded (remaining - 1) rest
    in
    bounded 32 related
  ;;

  let message t = t.message
  let site t = t.site
  let related t = t.related

  let to_json t =
    obj
      ([ "message", str t.message; "related", C.array Related.to_json t.related ]
       @ C.optional_field "site" t.site Site.to_json)
  ;;

  let of_json json =
    let* fields = C.fields ~allowed:[ "message"; "site"; "related" ] json in
    let* message = field fields "message" (C.string ~max_bytes:4096) in
    let* site = opt fields "site" Site.of_json in
    let* related = field fields "related" (list ~max:32 Related.of_json) in
    create ~message ~site ~related
  ;;
end

module Syntax_site = struct
  type role =
    | Value_reference
    | Value_binder
    | Module_binder
    | Module_reference
    | Type_binder
    | Type_parameter_binder
    | Type_reference
    | Signature_value_binder
    | Field_label
    | Variant_tag
    | Constraint_name
    | Constraint_parameter_reference
  [@@deriving compare, equal, sexp_of]

  let role_pairs =
    [ "value_reference", Value_reference
    ; "value_binder", Value_binder
    ; "module_binder", Module_binder
    ; "module_reference", Module_reference
    ; "type_binder", Type_binder
    ; "type_parameter_binder", Type_parameter_binder
    ; "type_reference", Type_reference
    ; "signature_value_binder", Signature_value_binder
    ; "field_label", Field_label
    ; "variant_tag", Variant_tag
    ; "constraint_name", Constraint_name
    ; "constraint_parameter_reference", Constraint_parameter_reference
    ]
  ;;

  let role_to_json role =
    str
      (List.find_map role_pairs ~f:(fun (name, item) ->
         if equal_role item role then Some name else None)
       |> Option.value_exn)
  ;;

  let role_of_json = enum ~name:"analysis syntax role" role_pairs

  type t =
    { role : role
    ; name : string
    ; site : Site.t
    }

  let create ~role ~name ~site =
    let* name = bounded_string ~max:1024 name in
    Ok { role; name; site }
  ;;

  let role t = t.role
  let name t = t.name
  let site t = t.site

  let to_json t =
    obj [ "role", role_to_json t.role; "name", str t.name; "site", Site.to_json t.site ]
  ;;

  let of_json json =
    let* fields = C.fields ~allowed:[ "role"; "name"; "site" ] json in
    let* role = field fields "role" role_of_json in
    let* name = field fields "name" (C.string ~max_bytes:1024) in
    let* site = field fields "site" Site.of_json in
    create ~role ~name ~site
  ;;
end
