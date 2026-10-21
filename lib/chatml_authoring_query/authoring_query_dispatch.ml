open Core
module Captured = Chatml_source
module Semantics = Chatml_semantics
module Snapshot = Semantics.Snapshot
module Wire = Agent_protocol.Authoring_analysis
module Budget = Authoring_query_budget
module Evidence = Authoring_query_evidence
module Fact = Authoring_query_fact
module Site = Authoring_query_site
module Error = Authoring_query_error

let evidence_error = function
  | Evidence.Evidence_mismatch -> Error.Evidence_mismatch
  | Stale_source -> Stale_source
  | Source_unavailable -> Source_unavailable
;;

let budget_error = function
  | Budget.Work_limit -> Error.Work_limit
  | Label_byte_limit -> Label_byte_limit
  | Member_edge_limit -> Member_edge_limit
;;

let query result = Result.map_error result ~f:(fun error -> Error.Query error)

let admit_payload request payload =
  Wire.Query.create ~request ~payload
  |> Result.map_error ~f:(fun error -> Error.Wire error)
  |> Result.map ~f:(fun _ -> payload)
;;

let limits budget request =
  Snapshot.Query_limits.create
    ~max_results:
      (Int.min (Budget.max_items budget) (Wire.Query.Request.max_items request))
    ~max_work_steps:(Budget.max_work_steps budget)
  |> Or_error.ok_exn
;;

let page budget ~max_items ~truncated ~charge_item items ~f =
  let rec collect count reversed = function
    | [] -> Ok { Wire.Query.items = List.rev reversed; truncated }
    | _ :: _ when count >= max_items ->
      Ok { Wire.Query.items = List.rev reversed; truncated = true }
    | item :: rest ->
      if charge_item && not (Budget.item budget)
      then Ok { Wire.Query.items = List.rev reversed; truncated = true }
      else (
        let () = Budget.checkpoint budget in
        Result.bind (f item) ~f:(fun projected ->
          collect (count + 1) (projected :: reversed) rest))
  in
  collect 0 [] items
;;

let reserve_root_items budget ~max_items items =
  let rec loop count = function
    | [] -> count
    | _ when count >= max_items -> count
    | _ :: tail -> if Budget.item budget then loop (count + 1) tail else count
  in
  loop 0 items
;;

let unavailable = function
  | Snapshot.Unavailable.Not_checked -> Wire.Query.Not_checked
  | No_recorded_context -> No_recorded_context
  | No_recorded_type -> No_recorded_type
  | No_nominal_members -> No_nominal_members
;;

let found_page budget request fact ~f =
  match fact with
  | Snapshot.Unavailable reason -> Ok (Wire.Query.Unavailable (unavailable reason))
  | Available page_value ->
    Result.map
      (page
         budget
         ~max_items:(Wire.Query.Request.max_items request)
         ~truncated:page_value.Snapshot.truncated
         ~charge_item:true
         page_value.Snapshot.items
         ~f)
      ~f:(fun page_value -> Wire.Query.Available page_value)
;;

let selected_page budget request selection ~charge_ambiguous_items ~f =
  match selection with
  | Snapshot.Missing reason -> Ok (Wire.Query.Missing (unavailable reason))
  | Selected value -> Result.map (f value) ~f:(fun value -> Wire.Query.Selected value)
  | Ambiguous page_value ->
    Result.map
      (page
         budget
         ~max_items:(Wire.Query.Request.max_items request)
         ~truncated:page_value.Snapshot.truncated
         ~charge_item:charge_ambiguous_items
         page_value.Snapshot.items
         ~f)
      ~f:(fun page_value -> Wire.Query.Ambiguous page_value)
;;

let namespace = function
  | Wire.Query.Value -> Snapshot.Namespace.Value
  | Type -> Type
  | Type_parameter -> Type_parameter
;;

let project_snapshot snapshot source request ~sites ~budget =
  let open Result.Let_syntax in
  let byte_offset = Wire.Query.Request.byte_offset request in
  let%bind location =
    Captured.locate
      source
      ~start_byte:byte_offset
      ~end_byte:byte_offset
      ~checkpoint:(fun () -> Budget.checkpoint budget)
    |> Result.map_error ~f:(fun _ -> Error.Stale_source)
  in
  let%bind point = Snapshot.point snapshot location |> query in
  let query_limits = limits budget request in
  let checkpoint () = Budget.checkpoint budget in
  let facts =
    Fact.create
      snapshot
      ~budget
      ~sites
      ~max_page_items:(Wire.Query.Request.max_items request)
  in
  match Wire.Query.Request.kind request with
  | References ->
    let%bind references =
      Snapshot.references_at snapshot point ~limits:query_limits ~checkpoint |> query
    in
    let%map found = found_page budget request references ~f:(Fact.reference facts) in
    Wire.Query.References_result found
  | Definitions ->
    let%bind definitions =
      Snapshot.definitions_at snapshot point ~limits:query_limits ~checkpoint |> query
    in
    let%map found = found_page budget request definitions ~f:(Fact.symbol facts) in
    Wire.Query.Definitions_result found
  | Type_at ->
    let%bind selected =
      Snapshot.type_at snapshot point ~limits:query_limits ~checkpoint |> query
    in
    let%bind () =
      match selected with
      | Snapshot.Selected _ when not (Budget.item budget) -> Error Error.Projection_limit
      | Selected _ | Missing _ | Ambiguous _ -> Ok ()
    in
    let render root = Fact.display facts root ~kind:Chatml_type_display.Kind.Type in
    let%map selected =
      selected_page budget request selected ~charge_ambiguous_items:true ~f:render
    in
    Wire.Query.Type_result selected
  | Candidates namespace_value ->
    let%bind contexts =
      Snapshot.context_at snapshot point ~limits:query_limits ~checkpoint |> query
    in
    let selected context =
      let%bind candidates =
        Snapshot.candidates
          snapshot
          context
          ~namespace:(namespace namespace_value)
          ~limits:query_limits
          ~checkpoint
        |> query
      in
      let reserved =
        reserve_root_items
          budget
          ~max_items:(Wire.Query.Request.max_items request)
          candidates.Snapshot.items
      in
      page
        budget
        ~max_items:reserved
        ~truncated:candidates.Snapshot.truncated
        ~charge_item:false
        candidates.Snapshot.items
        ~f:(fun value -> Fact.binding facts value ~depth:0)
    in
    let%map selected =
      selected_page budget request contexts ~charge_ambiguous_items:false ~f:selected
    in
    Wire.Query.Candidates_result selected
  | Configured_modules ->
    let%bind contexts =
      Snapshot.context_at snapshot point ~limits:query_limits ~checkpoint |> query
    in
    let selected context =
      let%bind modules =
        Snapshot.configured_modules snapshot context ~limits:query_limits ~checkpoint
        |> query
      in
      let reserved =
        reserve_root_items
          budget
          ~max_items:(Wire.Query.Request.max_items request)
          modules.Snapshot.items
      in
      page
        budget
        ~max_items:reserved
        ~truncated:modules.Snapshot.truncated
        ~charge_item:false
        modules.Snapshot.items
        ~f:(Fact.configured_module facts)
    in
    let%map selected =
      selected_page budget request contexts ~charge_ambiguous_items:false ~f:selected
    in
    Wire.Query.Configured_modules_result selected
  | Syntax_at -> Error Error.Unsupported_kind
;;

let project evidence ~request ~sites ~limits ~checkpoint =
  let budget = Budget.create limits ~checkpoint in
  Budget.run budget ~f:(fun () ->
    match Wire.Query.Request.kind request with
    | Syntax_at -> Error Error.Unsupported_kind
    | References | Definitions | Type_at | Candidates _ | Configured_modules ->
      Result.bind
        (Evidence.semantic evidence request ~budget |> Result.map_error ~f:evidence_error)
        ~f:(fun (snapshot, source) ->
          project_snapshot snapshot source request ~sites ~budget))
  |> Result.map_error ~f:budget_error
  |> Result.join
  |> Result.bind ~f:(admit_payload request)
;;

let syntax_role = function
  | Semantics.Sites.Role.Value_reference -> Wire.Syntax_site.Value_reference
  | Value_binder -> Value_binder
  | Module_binder -> Module_binder
  | Module_reference -> Module_reference
  | Type_binder -> Type_binder
  | Type_parameter_binder -> Type_parameter_binder
  | Type_reference -> Type_reference
  | Signature_value_binder -> Signature_value_binder
  | Field_label -> Field_label
  | Variant_tag -> Variant_tag
  | Constraint_name -> Constraint_name
  | Constraint_parameter_reference -> Constraint_parameter_reference
;;

let syntax_at partial ~request ~sites ~limits ~checkpoint =
  let budget = Budget.create limits ~checkpoint in
  Budget.run budget ~f:(fun () ->
    let open Result.Let_syntax in
    if not (Wire.Query.equal_kind (Wire.Query.Request.kind request) Syntax_at)
    then Error Error.Unsupported_kind
    else (
      let%bind captured =
        Evidence.syntax partial request ~budget |> Result.map_error ~f:evidence_error
      in
      let rec select = function
        | [] -> Ok None
        | syntax :: tail ->
          Budget.checkpoint budget;
          let source = Semantics.Partial.Syntax.source syntax in
          if Budget.equal_source_id budget (Captured.id source) (Captured.id captured)
          then
            if Captured.Digest.equal (Captured.digest source) (Captured.digest captured)
            then Ok (Some (Semantics.Partial.Syntax.sites syntax))
            else Error Error.Stale_source
          else select tail
      in
      let%bind selected = select (Semantics.Partial.parsed partial) in
      match selected with
      | None -> Ok (Wire.Query.Syntax_result (Wire.Query.Unavailable No_syntax))
      | Some table ->
        let offset = Wire.Query.Request.byte_offset request in
        let rec collect count reversed = function
          | [] -> Ok { Wire.Query.items = List.rev reversed; truncated = false }
          | item :: tail ->
            Budget.checkpoint budget;
            let location = Semantics.Sites.Site.location item in
            let span = Captured.Location.span location in
            if span.left.offset <= offset && offset < span.right.offset
            then
              if count >= Wire.Query.Request.max_items request || not (Budget.item budget)
              then Ok { Wire.Query.items = List.rev reversed; truncated = true }
              else (
                let%bind site =
                  Site.project
                    (Semantics.Partial.sources partial)
                    location
                    ~projection:sites
                    ~budget
                in
                let name = Budget.label budget (Semantics.Sites.Site.name item) in
                let%bind projected =
                  Wire.Syntax_site.create
                    ~role:(syntax_role (Semantics.Sites.Site.role item))
                    ~name
                    ~site
                  |> Result.map_error ~f:(fun error -> Error.Wire error)
                in
                collect (count + 1) (projected :: reversed) tail)
            else collect count reversed tail
        in
        let%map page = collect 0 [] (Semantics.Sites.sites table) in
        Wire.Query.Syntax_result (Wire.Query.Available page)))
  |> Result.map_error ~f:budget_error
  |> Result.join
  |> Result.bind ~f:(admit_payload request)
;;
