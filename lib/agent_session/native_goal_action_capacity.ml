open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec

(* Each actual Invocation subtree is merged with its real retained fields using
   the same shape as State. Maximum over later phases and addition over disjoint
   records uses one input scan with keyed lookups and a fixed number of probes.
   These documents are capacity DATA only, never decoded as domain evidence. *)
let growth ~invocations document ~limits =
  let open Result.Let_syntax in
  let%bind actions =
    let actions =
      List.filter_map invocations ~f:(fun invocation ->
        Option.map invocation.P.Invocation.native_goal_action ~f:(fun action ->
          P.Id.Invocation.to_string invocation.context.id, action))
    in
    match Map.of_alist (module String) actions with
    | `Ok actions -> Ok actions
    | `Duplicate_key id ->
      Error
        (D.Error.Invalid_field
           { path = [ "invocations"; id ]; reason = "duplicate native Goal Invocation" })
  in
  if Map.is_empty actions
  then Ok Capacity_growth.zero
  else (
    (* In the complete State document an Invocation is at depth4, while the
       component document places its payload at depth2. Preserve that exact
       depth allowance; byte/field/node differences cancel the fixed envelope. *)
    let%bind child_limits =
      D.Limits.create
        ~max_bytes:(D.Limits.max_bytes limits)
        ~max_depth:(D.Limits.max_depth limits - 2)
        ~max_fields:(D.Limits.max_fields limits)
        ~max_nodes:(D.Limits.max_nodes limits)
    in
    let%bind codec =
      D.Domain_codec.create
        ~limits:child_limits
        ~kind:(D.Document.kind document)
        ~version:(D.Document.version document)
        ~shape:Session_record_shapes.invocation
        ~supported_semantics:Session_storage_semantics.supported
        ~decode:(fun json -> Ok json)
        ~encode:(fun json -> Ok json)
    in
    let%bind invocations =
      Agent_store.Document_fields.required
        (D.Document.payload document)
        "invocations"
        Agent_store.Document_fields.array
    in
    List.fold_result invocations ~init:Capacity_growth.zero ~f:(fun total raw ->
      let%bind id =
        Agent_store.Document_fields.required raw "id" Agent_store.Document_fields.string
      in
      match Map.find actions id with
      | None -> Ok total
      | Some action ->
        let projections = P.Goal_native_action_intent.capacity_projections action in
        (match projections with
         | [] -> Ok total
         | _ ->
           let%bind component =
             D.Document.create
               ~limits:child_limits
               ~kind:(D.Document.kind document)
               ~version:(D.Document.version document)
               ~payload:raw
           in
           let%bind carrier = D.Domain_codec.decode codec component in
           let payload = D.Extension_carrier.value carrier in
           let%bind () =
             match D.Json.field payload ~name:"native_goal_action" with
             | Value _ -> Ok ()
             | Absent | Null ->
               Error
                 (D.Error.Invalid_field
                    { path = [ "invocations"; id; "native_goal_action" ]
                    ; reason = "admitted native Goal action is missing"
                    })
           in
           let%bind original =
             Capacity_growth.measure (D.Document.json component) ~limits:child_limits
             |> X.document_result
           in
           let%bind growth =
             List.fold_result
               projections
               ~init:Capacity_growth.zero
               ~f:(fun growth action ->
                 let%bind projected_payload =
                   match payload with
                   | `Object fields ->
                     Ok
                       (`Object
                           (List.map fields ~f:(fun (name, value) ->
                              ( name
                              , if String.equal name "native_goal_action"
                                then action
                                else value ))))
                   | _ ->
                     Error
                       (D.Error.Invalid_field
                          { path = [ "invocations"; id ]
                          ; reason = "expected Invocation object"
                          })
                 in
                 let%bind projected =
                   D.Domain_codec.encode
                     codec
                     (D.Extension_carrier.with_value carrier projected_payload)
                 in
                 let%map measured =
                   Capacity_growth.measure
                     (D.Document.json projected)
                     ~limits:child_limits
                   |> X.document_result
                 in
                 Capacity_growth.maximum
                   growth
                   (Capacity_growth.positive_difference measured ~previous:original))
           in
           Capacity_growth.add total growth |> X.document_result)))
;;
