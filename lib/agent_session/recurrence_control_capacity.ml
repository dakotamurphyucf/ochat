open! Core
module D = Document_schema
module X = Persistence_codec

type t =
  { growth : Capacity_growth.t
  ; catalog : Jsonaf.t option
  }

let growth t = t.growth
let projected_catalog t = t.catalog

let replace json changes =
  match json with
  | `Object fields ->
    `Object
      (List.filter fields ~f:(fun (name, _) ->
         not (List.Assoc.mem changes name ~equal:String.equal))
       @ changes)
  | _ -> assert false
;;

let capture (state : Session_state.t) ~shape ~document ~limits =
  let open Result.Let_syntax in
  match state.recurrence_catalog with
  | None -> Ok { growth = Capacity_growth.zero; catalog = None }
  | Some catalog ->
    let%bind reservation = Recurrence_catalog.reservation catalog |> X.document_result in
    if Recurrence_catalog.Reservation.required_transition_steps reservation = 0
    then Ok { growth = Capacity_growth.zero; catalog = None }
    else (
      let%bind projected =
        Recurrence_catalog.capacity_projection catalog |> X.document_result
      in
      let%bind codec =
        D.Domain_codec.create
          ~limits
          ~kind:(D.Document.kind document)
          ~version:(D.Document.version document)
          ~shape
          ~supported_semantics:Queue_provenance_semantics.supported
          ~decode:(fun json -> Ok json)
          ~encode:(fun json -> Ok json)
      in
      let%bind carrier = D.Domain_codec.decode codec document in
      let payload = D.Extension_carrier.value carrier in
      let counters =
        match D.Json.field payload ~name:"counters" with
        | Value counters ->
          replace
            counters
            [ "revision", `String (Int64.to_string Int64.max_value)
            ; "transaction_sequence", `String (Int64.to_string Int64.max_value)
            ; "event_sequence", `String (Int64.to_string Int64.max_value)
            ]
        | Absent | Null -> assert false
      in
      let identity =
        match D.Json.field payload ~name:"identity" with
        | Value identity ->
          replace identity [ "updated_at", `String "2099-12-31T23:59:59.999999999Z" ]
        | Absent | Null -> assert false
      in
      let%bind projected =
        D.Domain_codec.encode
          codec
          (D.Extension_carrier.with_value
             carrier
             (replace
                payload
                [ "recurrence_catalog", projected
                ; "counters", counters
                ; "identity", identity
                ]))
      in
      let%bind before =
        Capacity_growth.measure (D.Document.json document) ~limits |> X.document_result
      in
      let%bind after =
        Capacity_growth.measure (D.Document.json projected) ~limits |> X.document_result
      in
      let catalog =
        match D.Json.field (D.Document.payload projected) ~name:"recurrence_catalog" with
        | Value catalog -> catalog
        | Absent | Null -> assert false
      in
      Ok
        { growth = Capacity_growth.positive_difference after ~previous:before
        ; catalog = Some catalog
        })
;;
