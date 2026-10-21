open! Core
module C = Authoring_codec
module A = Authoring_analysis_core
module Source = Authoring_common.Source
module Digest = Chatml_source.Digest

let ( let* ) result f = Result.bind result ~f
let obj fields = `Object fields
let str value = `String value
let field fields name decode = C.required_as fields name decode
let opt fields name decode = C.optional_as fields name decode
let list ~max decode json = C.list ~max_items:max decode json
let enum ~name choices = Json_codec.enum ~name choices

module Query = struct
  include Authoring_analysis_query_base.Query

  module Display = struct
    type format = Chatml_display_v1 [@@deriving compare, equal, sexp_of]

    type kind =
      | Type
      | Signature
    [@@deriving compare, equal, sexp_of]

    type completeness =
      | Complete
      | Truncated of [ `Work_limit | `Byte_limit ]
    [@@deriving compare, equal, sexp_of]

    type t =
      { format : format
      ; kind : kind
      ; completeness : completeness
      ; text : string
      }

    let create ~format ~kind ~completeness ~text =
      let* text = A.bounded_string ~max:A.Limits.max_display_bytes text in
      Ok { format; kind; completeness; text }
    ;;

    let format t = t.format
    let kind t = t.kind
    let completeness t = t.completeness
    let text t = t.text

    let to_json t =
      obj
        [ "format", str "chatml_display_v1"
        ; ( "kind"
          , str
              (match t.kind with
               | Type -> "type"
               | Signature -> "signature") )
        ; ( "completeness"
          , match t.completeness with
            | Complete -> str "complete"
            | Truncated `Work_limit -> str "truncated_work_limit"
            | Truncated `Byte_limit -> str "truncated_byte_limit" )
        ; "text", str t.text
        ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "format"; "kind"; "completeness"; "text" ] json in
      let* format = field fields "format" (C.string ~max_bytes:32) in
      let* () =
        if String.equal format "chatml_display_v1"
        then Ok ()
        else C.invalid "unknown type display format"
      in
      let* kind =
        field
          fields
          "kind"
          (enum ~name:"display kind" [ "type", Type; "signature", Signature ])
      in
      let* completeness = field fields "completeness" (C.string ~max_bytes:32) in
      let* completeness =
        match completeness with
        | "complete" -> Ok Complete
        | "truncated_work_limit" -> Ok (Truncated `Work_limit)
        | "truncated_byte_limit" -> Ok (Truncated `Byte_limit)
        | _ -> C.invalid "unknown type display completeness"
      in
      let* text =
        field fields "text" (C.utf8_text ~max_bytes:A.Limits.max_display_bytes)
      in
      create ~format:Chatml_display_v1 ~kind ~completeness ~text
    ;;
  end

  module Item = struct
    type symbol_kind =
      | Value
      | Parameter
      | Module
      | Type_alias
      | Type_parameter
    [@@deriving compare, equal, sexp_of]

    let symbol_kind_pairs =
      [ "value", Value
      ; "parameter", Parameter
      ; "module", Module
      ; "type_alias", Type_alias
      ; "type_parameter", Type_parameter
      ]
    ;;

    let symbol_kind_to_json kind =
      str
        (List.find_map symbol_kind_pairs ~f:(fun (name, item) ->
           if equal_symbol_kind item kind then Some name else None)
         |> Option.value_exn)
    ;;

    let symbol_kind_of_json = enum ~name:"analysis symbol kind" symbol_kind_pairs

    type origin =
      | Authored of A.Site.t
      | Implicit_specification_parameter of A.Site.t
      | Host of
          { contract : Digest.t
          ; member_path : string list
          }

    let origin_to_json = function
      | Authored site -> obj [ "kind", str "authored"; "site", A.Site.to_json site ]
      | Implicit_specification_parameter site ->
        obj
          [ "kind", str "implicit_specification_parameter"; "site", A.Site.to_json site ]
      | Host { contract; member_path } ->
        obj
          [ "kind", str "host"
          ; "contract", C.digest_json contract
          ; "member_path", C.array str member_path
          ]
    ;;

    let origin_of_json json =
      let* fields =
        C.fields ~allowed:[ "kind"; "site"; "contract"; "member_path" ] json
      in
      let* kind = field fields "kind" (C.string ~max_bytes:40) in
      match kind with
      | "authored" | "implicit_specification_parameter" ->
        let* _ = C.fields ~allowed:[ "kind"; "site" ] json in
        let* site = field fields "site" A.Site.of_json in
        Ok
          (if String.equal kind "authored"
           then Authored site
           else Implicit_specification_parameter site)
      | "host" ->
        let* _ = C.fields ~allowed:[ "kind"; "contract"; "member_path" ] json in
        let* contract = field fields "contract" C.digest in
        let* member_path =
          field fields "member_path" (list ~max:32 (C.string ~max_bytes:1024))
        in
        Ok (Host { contract; member_path })
      | _ -> C.invalid "unknown symbol origin"
    ;;

    type symbol =
      { id : int
      ; name : string
      ; kind : symbol_kind
      ; origin : origin
      ; principal_signature : Display.t option
      ; specification_ids : int list
      }

    let symbol ~id ~name ~kind ~origin ~principal_signature ~specification_ids =
      let* _ = A.bounded_nonnegative id in
      let* name = A.bounded_string ~max:1024 name in
      let* () = A.Analyze.bounded_list ~max:64 specification_ids in
      let* () =
        Result.all_unit
          (List.map specification_ids ~f:(fun id ->
             Result.map (A.bounded_nonnegative id) ~f:ignore))
      in
      let* () =
        match origin with
        | Host { member_path; _ } ->
          let* () = A.Analyze.bounded_list ~max:32 member_path in
          Result.all_unit
            (List.map member_path ~f:(fun name ->
               Result.map (A.bounded_string ~max:1024 name) ~f:ignore))
        | _ -> Ok ()
      in
      Ok { id; name; kind; origin; principal_signature; specification_ids }
    ;;

    let symbol_id t = t.id
    let symbol_name t = t.name
    let symbol_kind t = t.kind
    let symbol_origin t = t.origin
    let principal_signature t = t.principal_signature
    let symbol_specification_ids t = t.specification_ids

    let symbol_to_json t =
      obj
        ([ "id", C.integer t.id
         ; "name", str t.name
         ; "kind", symbol_kind_to_json t.kind
         ; "origin", origin_to_json t.origin
         ; "specification_ids", C.array C.integer t.specification_ids
         ]
         @ C.optional_field "principal_signature" t.principal_signature Display.to_json)
    ;;

    let symbol_of_json json =
      let* fields =
        C.fields
          ~allowed:
            [ "id"; "name"; "kind"; "origin"; "principal_signature"; "specification_ids" ]
          json
      in
      let* id = field fields "id" C.nonnegative in
      let* name = field fields "name" (C.string ~max_bytes:1024) in
      let* kind = field fields "kind" symbol_kind_of_json in
      let* origin = field fields "origin" origin_of_json in
      let* principal_signature = opt fields "principal_signature" Display.of_json in
      let* specification_ids =
        field fields "specification_ids" (list ~max:64 C.nonnegative)
      in
      symbol ~id ~name ~kind ~origin ~principal_signature ~specification_ids
    ;;

    type target =
      | Symbol of int
      | Configured_module of string
      | Structural_label
      | Unlocated_module_member
      | Language_type of string

    let target_to_json = function
      | Symbol id -> obj [ "kind", str "symbol"; "id", C.integer id ]
      | Configured_module key ->
        obj [ "kind", str "configured_module"; "module_key", str key ]
      | Structural_label -> obj [ "kind", str "structural_label" ]
      | Unlocated_module_member -> obj [ "kind", str "unlocated_module_member" ]
      | Language_type name -> obj [ "kind", str "language_type"; "name", str name ]
    ;;

    let target_of_json json =
      let* fields = C.fields ~allowed:[ "kind"; "id"; "module_key"; "name" ] json in
      let* kind = field fields "kind" (C.string ~max_bytes:32) in
      match kind with
      | "symbol" ->
        let* _ = C.fields ~allowed:[ "kind"; "id" ] json in
        let* id = field fields "id" C.nonnegative in
        Ok (Symbol id)
      | "configured_module" ->
        let* _ = C.fields ~allowed:[ "kind"; "module_key" ] json in
        let* key = field fields "module_key" (C.string ~max_bytes:1024) in
        Ok (Configured_module key)
      | "structural_label" | "unlocated_module_member" ->
        let* _ = C.fields ~allowed:[ "kind" ] json in
        Ok
          (if String.equal kind "structural_label"
           then Structural_label
           else Unlocated_module_member)
      | "language_type" ->
        let* _ = C.fields ~allowed:[ "kind"; "name" ] json in
        let* name = field fields "name" (C.string ~max_bytes:1024) in
        Ok (Language_type name)
      | _ -> C.invalid "unknown reference target"
    ;;

    type reference =
      { selection : A.Site.t
      ; target : target
      ; inferred_type : Display.t option
      }

    let reference ~selection ~target ~inferred_type =
      let* () =
        match target with
        | Symbol id -> Result.map (A.bounded_nonnegative id) ~f:ignore
        | Configured_module key | Language_type key ->
          Result.map (A.bounded_string ~max:1024 key) ~f:ignore
        | Structural_label | Unlocated_module_member -> Ok ()
      in
      Ok { selection; target; inferred_type }
    ;;

    let reference_selection t = t.selection
    let reference_target t = t.target
    let reference_inferred_type t = t.inferred_type

    let reference_to_json t =
      obj
        ([ "selection", A.Site.to_json t.selection; "target", target_to_json t.target ]
         @ C.optional_field "inferred_type" t.inferred_type Display.to_json)
    ;;

    let reference_of_json json =
      let* fields = C.fields ~allowed:[ "selection"; "target"; "inferred_type" ] json in
      let* selection = field fields "selection" A.Site.of_json in
      let* target = field fields "target" target_of_json in
      let* inferred_type = opt fields "inferred_type" Display.of_json in
      reference ~selection ~target ~inferred_type
    ;;

    type binding =
      { symbol : int
      ; name : string
      ; selected_signature : Display.t option
      ; value_members : binding page found
      ; type_members : binding page found
      }

    type budget =
      { mutable nodes_left : int
      ; mutable label_bytes_left : int
      }

    let budget () = { nodes_left = 2048; label_bytes_left = 256 * 1024 }

    let charge budget ~nodes ~label_bytes =
      if nodes > budget.nodes_left || label_bytes > budget.label_bytes_left
      then C.invalid "analysis binding tree exceeds aggregate limit"
      else (
        budget.nodes_left <- budget.nodes_left - nodes;
        budget.label_bytes_left <- budget.label_bytes_left - label_bytes;
        Ok ())
    ;;

    let rec scan_binding budget depth t =
      if depth > 6
      then C.invalid "analysis binding member depth exceeds limit"
      else (
        let display_bytes =
          Option.value_map t.selected_signature ~default:0 ~f:(fun display ->
            String.length (Display.text display))
        in
        let* () =
          charge budget ~nodes:1 ~label_bytes:(String.length t.name + display_bytes)
        in
        let* () = scan_members budget (depth + 1) t.value_members in
        scan_members budget (depth + 1) t.type_members)

    and scan_members budget depth = function
      | Unavailable _ -> Ok ()
      | Available page -> scan_page budget depth page

    and scan_page budget depth page =
      let* () = A.Analyze.bounded_list ~max:A.Limits.max_query_items page.items in
      let rec loop = function
        | [] -> Ok ()
        | binding :: rest ->
          let* () = scan_binding budget depth binding in
          loop rest
      in
      loop page.items
    ;;

    let binding ~symbol ~name ~selected_signature ~value_members ~type_members =
      let* _ = A.bounded_nonnegative symbol in
      let* name = A.bounded_string ~max:1024 name in
      let value = { symbol; name; selected_signature; value_members; type_members } in
      let* () = scan_binding (budget ()) 0 value in
      Ok value
    ;;

    let binding_symbol t = t.symbol
    let binding_name t = t.name
    let binding_selected_signature t = t.selected_signature
    let binding_value_members t = t.value_members
    let binding_type_members t = t.type_members

    let rec binding_to_json t =
      obj
        ([ "symbol", C.integer t.symbol
         ; "name", str t.name
         ; "value_members", found_to_json (page_to_json binding_to_json) t.value_members
         ; "type_members", found_to_json (page_to_json binding_to_json) t.type_members
         ]
         @ C.optional_field "selected_signature" t.selected_signature Display.to_json)
    ;;

    let rec binding_of_json_at depth json =
      if depth > 6
      then C.invalid "binding member depth exceeds limit"
      else
        let* fields =
          C.fields
            ~allowed:
              [ "symbol"; "name"; "selected_signature"; "value_members"; "type_members" ]
            json
        in
        let* symbol = field fields "symbol" C.nonnegative in
        let* name = field fields "name" (C.string ~max_bytes:1024) in
        let* selected_signature = opt fields "selected_signature" Display.of_json in
        let member_decoder =
          found_of_json
            (page_of_json ~max:A.Limits.max_query_items (binding_of_json_at (depth + 1)))
        in
        let* value_members = field fields "value_members" member_decoder in
        let* type_members = field fields "type_members" member_decoder in
        binding ~symbol ~name ~selected_signature ~value_members ~type_members
    ;;

    let binding_of_json json = binding_of_json_at 0 json

    type exports =
      { values : binding page
      ; types : binding page
      }

    let exports ~values ~types =
      let budget = budget () in
      let* () = scan_page budget 0 values in
      let* () = scan_page budget 0 types in
      Ok { values; types }
    ;;

    let export_values t = t.values
    let export_types t = t.types

    let exports_to_json t =
      obj
        [ "values", page_to_json binding_to_json t.values
        ; "types", page_to_json binding_to_json t.types
        ]
    ;;

    let exports_of_json json =
      let* fields = C.fields ~allowed:[ "values"; "types" ] json in
      let* values =
        field fields "values" (page_of_json ~max:A.Limits.max_query_items binding_of_json)
      in
      let* types =
        field fields "types" (page_of_json ~max:A.Limits.max_query_items binding_of_json)
      in
      exports ~values ~types
    ;;

    type configured_module =
      { name : string
      ; module_key : string
      ; declared_at : A.Site.t
      ; exports : exports found
      ; shadowed_by : int option
      }

    let scan_configured budget t =
      let* () =
        charge
          budget
          ~nodes:1
          ~label_bytes:(String.length t.name + String.length t.module_key)
      in
      match t.exports with
      | Unavailable _ -> Ok ()
      | Available exports ->
        let* () = scan_page budget 0 exports.values in
        scan_page budget 0 exports.types
    ;;

    let configured_module ~name ~module_key ~declared_at ~exports ~shadowed_by =
      let* name = A.bounded_string ~max:1024 name in
      let* module_key = A.bounded_string ~max:1024 module_key in
      let* () =
        match shadowed_by with
        | None -> Ok ()
        | Some id -> Result.map (A.bounded_nonnegative id) ~f:ignore
      in
      let value = { name; module_key; declared_at; exports; shadowed_by } in
      let* () = scan_configured (budget ()) value in
      Ok value
    ;;

    let configured_module_name t = t.name
    let configured_module_key t = t.module_key
    let configured_module_declared_at t = t.declared_at
    let configured_module_exports t = t.exports
    let configured_module_shadowed_by t = t.shadowed_by

    let configured_module_to_json t =
      obj
        ([ "name", str t.name
         ; "module_key", str t.module_key
         ; "declared_at", A.Site.to_json t.declared_at
         ; "exports", found_to_json exports_to_json t.exports
         ]
         @ C.optional_field "shadowed_by" t.shadowed_by C.integer)
    ;;

    let configured_module_of_json json =
      let* fields =
        C.fields
          ~allowed:[ "name"; "module_key"; "declared_at"; "exports"; "shadowed_by" ]
          json
      in
      let* name = field fields "name" (C.string ~max_bytes:1024) in
      let* module_key = field fields "module_key" (C.string ~max_bytes:1024) in
      let* declared_at = field fields "declared_at" A.Site.of_json in
      let* exports = field fields "exports" (found_of_json exports_of_json) in
      let* shadowed_by = opt fields "shadowed_by" C.nonnegative in
      configured_module ~name ~module_key ~declared_at ~exports ~shadowed_by
    ;;
  end
end
