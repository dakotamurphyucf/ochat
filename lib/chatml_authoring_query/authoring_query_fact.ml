open Core
module Captured = Chatml_source
module Semantics = Chatml_semantics
module Snapshot = Semantics.Snapshot
module Wire = Agent_protocol.Authoring_analysis
module Budget = Authoring_query_budget
module Site = Authoring_query_site
module Error = Authoring_query_error
module Display = Chatml_type_display

type t =
  { snapshot : Snapshot.t
  ; budget : Budget.t
  ; sites : Site.Site_projection.t
  ; max_page_items : int
  }

let create snapshot ~budget ~sites ~max_page_items =
  { snapshot; budget; sites; max_page_items }
;;

let checkpoint t () = Budget.checkpoint t.budget
let label t value = Budget.label t.budget value
let wire result = Result.map_error result ~f:(fun error -> Error.Wire error)

let bounded_map t ~max values ~f =
  let rec loop count reversed = function
    | [] -> Ok (List.rev reversed)
    | _ when count >= max -> Error Error.Projection_limit
    | value :: tail ->
      Budget.checkpoint t.budget;
      Result.bind (f value) ~f:(fun projected ->
        loop (count + 1) (projected :: reversed) tail)
  in
  loop 0 [] values
;;

let symbol_id t id =
  Budget.checkpoint t.budget;
  match Snapshot.symbol t.snapshot id with
  | Ok _ -> Ok (Snapshot.Symbol_id.index id)
  | Error Snapshot.Error.Foreign_symbol -> Error Error.Foreign_symbol
  | Error error -> Error (Error.Query error)
;;

let project_site t location =
  Site.project (Snapshot.sources t.snapshot) location ~projection:t.sites ~budget:t.budget
;;

let display t root ~kind =
  let open Result.Let_syntax in
  let allowance =
    Int.min (Budget.remaining_labels t.budget) Wire.Limits.max_display_bytes
  in
  let%bind rendered =
    Display.Session.render
      (Budget.display_session t.budget)
      ~max_bytes:allowance
      (Snapshot.types t.snapshot)
      ~root
      ~kind
      ~checkpoint:(checkpoint t)
    |> Result.map_error ~f:(fun error -> Error.Display error)
  in
  let text = Display.text rendered in
  Budget.charge_display t.budget text;
  let kind =
    match Display.kind rendered with
    | Type -> Wire.Query.Display.Type
    | Signature -> Wire.Query.Display.Signature
  in
  let completeness =
    match Display.completeness rendered with
    | Complete -> Wire.Query.Display.Complete
    | Truncated Byte_limit -> Truncated `Byte_limit
    | Truncated (Node_limit | Edge_limit | Depth_limit | Work_limit) ->
      Truncated `Work_limit
  in
  Wire.Query.Display.create ~format:Chatml_display_v1 ~kind ~completeness ~text |> wire
;;

let maybe_display t root ~kind =
  match root with
  | None -> Ok None
  | Some root -> Result.map (display t root ~kind) ~f:Option.some
;;

let symbol_kind = function
  | Snapshot.Symbol.Value -> Wire.Query.Item.Value
  | Parameter -> Parameter
  | Module -> Module
  | Type_alias -> Type_alias
  | Type_parameter -> Type_parameter
;;

let origin t = function
  | Snapshot.Origin.Authored location ->
    Result.map (project_site t location) ~f:(fun site -> Wire.Query.Item.Authored site)
  | Implicit_specification_parameter location ->
    Result.map (project_site t location) ~f:(fun site ->
      Wire.Query.Item.Implicit_specification_parameter site)
  | Host { contract; member_path } ->
    bounded_map t ~max:32 member_path ~f:(fun segment -> Ok (label t segment))
    |> Result.map ~f:(fun member_path -> Wire.Query.Item.Host { contract; member_path })
;;

let symbol t value =
  let open Result.Let_syntax in
  let%bind id = symbol_id t (Snapshot.Symbol.id value) in
  let name = label t (Snapshot.Symbol.name value) in
  let%bind origin = origin t (Snapshot.Symbol.origin value) in
  let%bind principal_signature =
    maybe_display t (Snapshot.Symbol.scheme value) ~kind:Display.Kind.Signature
  in
  let%bind specification_ids =
    bounded_map t ~max:64 (Snapshot.Symbol.specifications value) ~f:(symbol_id t)
  in
  Wire.Query.Item.symbol
    ~id
    ~name
    ~kind:(symbol_kind (Snapshot.Symbol.kind value))
    ~origin
    ~principal_signature
    ~specification_ids
  |> wire
;;

let module_key t module_id =
  let id = Chatml_prepared_program.Module_id.implementation module_id in
  let namespace = label t (Captured.Id.package_namespace id) in
  let path = label t (Captured.Id.relative_path id) in
  String.concat
    [ label t (Int.to_string (String.length namespace))
    ; label t ":"
    ; namespace
    ; label t ":"
    ; path
    ]
;;

let reference_target t = function
  | Snapshot.Reference.Symbol id ->
    Result.map (symbol_id t id) ~f:(fun id -> Wire.Query.Item.Symbol id)
  | Configured_module module_id ->
    Ok (Wire.Query.Item.Configured_module (module_key t module_id))
  | Structural_label -> Ok Wire.Query.Item.Structural_label
  | Unlocated_module_member -> Ok Wire.Query.Item.Unlocated_module_member
  | Language_type name -> Ok (Wire.Query.Item.Language_type (label t name))
;;

let reference t value =
  let open Result.Let_syntax in
  let%bind selection = project_site t (Snapshot.Reference.selection value) in
  let%bind target = reference_target t (Snapshot.Reference.target value) in
  let%bind inferred_type =
    maybe_display t (Snapshot.Reference.inferred_type value) ~kind:Display.Kind.Type
  in
  Wire.Query.Item.reference ~selection ~target ~inferred_type |> wire
;;

let unavailable = function
  | Snapshot.Unavailable.Not_checked -> Wire.Query.Not_checked
  | No_recorded_context -> No_recorded_context
  | No_recorded_type -> No_recorded_type
  | No_nominal_members -> No_nominal_members
;;

let rec binding t value ~depth =
  let open Result.Let_syntax in
  if depth > Budget.max_member_depth t.budget
  then Error Error.Projection_limit
  else (
    let%bind symbol = symbol_id t (Snapshot.Binding.symbol value) in
    let name = label t (Snapshot.Binding.name value) in
    let%bind selected_signature =
      maybe_display t (Snapshot.Binding.scheme value) ~kind:Display.Kind.Signature
    in
    let%bind value_members = binding_fact t (Snapshot.Binding.values value) ~depth in
    let%bind type_members = binding_fact t (Snapshot.Binding.types value) ~depth in
    Wire.Query.Item.binding ~symbol ~name ~selected_signature ~value_members ~type_members
    |> wire)

and binding_fact t fact ~depth =
  match fact with
  | Snapshot.Unavailable reason -> Ok (Wire.Query.Unavailable (unavailable reason))
  | Available members ->
    Result.map
      (binding_page t members ~depth:(depth + 1))
      ~f:(fun page -> Wire.Query.Available page)

and binding_page t members ~depth =
  let rec loop count reversed = function
    | [] -> Ok { Wire.Query.items = List.rev reversed; truncated = false }
    | _ when count >= t.max_page_items ->
      Ok { Wire.Query.items = List.rev reversed; truncated = true }
    | _ when depth > Budget.max_member_depth t.budget ->
      Ok { Wire.Query.items = List.rev reversed; truncated = true }
    | value :: tail ->
      if (not (Budget.member_edge t.budget)) || not (Budget.item t.budget)
      then Ok { Wire.Query.items = List.rev reversed; truncated = true }
      else
        Result.bind (binding t value ~depth) ~f:(fun projected ->
          loop (count + 1) (projected :: reversed) tail)
  in
  loop 0 [] members
;;

let exports t exports =
  let open Result.Let_syntax in
  let%bind values = binding_page t (Snapshot.Exports.values exports) ~depth:0 in
  let%bind types = binding_page t (Snapshot.Exports.types exports) ~depth:0 in
  Wire.Query.Item.exports ~values ~types |> wire
;;

let configured_module t value =
  let open Result.Let_syntax in
  let name = label t (Snapshot.Configured_module.name value) in
  let module_key = module_key t (Snapshot.Configured_module.module_id value) in
  let%bind declared_at = project_site t (Snapshot.Configured_module.declared_at value) in
  let%bind exports =
    match Snapshot.Configured_module.exports value with
    | Snapshot.Unavailable reason -> Ok (Wire.Query.Unavailable (unavailable reason))
    | Available value ->
      Result.map (exports t value) ~f:(fun value -> Wire.Query.Available value)
  in
  let%bind shadowed_by =
    match Snapshot.Configured_module.shadowed_by value with
    | None -> Ok None
    | Some id -> Result.map (symbol_id t id) ~f:Option.some
  in
  Wire.Query.Item.configured_module ~name ~module_key ~declared_at ~exports ~shadowed_by
  |> wire
;;
