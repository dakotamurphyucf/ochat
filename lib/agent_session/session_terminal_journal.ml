open! Core
module P = Agent_protocol
module D = Document_schema

let protocol_error error =
  P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error))
;;

let domain result = Result.map_error result ~f:protocol_error

let replace json changes =
  match json with
  | `Object fields ->
    `Object
      (List.filter fields ~f:(fun (name, _) ->
         not (List.Assoc.mem changes name ~equal:String.equal))
       @ changes)
  | _ -> assert false
;;

let validate (state : Session_state.t) ~terminal ~limits ~max_payload_length =
  let open Result.Let_syntax in
  match Run_terminal_document.projections terminal with
  | [] -> Ok ()
  | projections ->
    let%bind goal =
      match state.goal_catalog with
      | None -> Ok Capacity_growth.zero
      | Some catalog -> Goal_catalog.reserved_growth catalog
    in
    let%bind additional =
      Capacity_growth.add (Run_terminal_document.additional_growth terminal) goal
    in
    let%bind limits =
      D.Limits.create
        ~max_bytes:(Int.min (D.Limits.max_bytes limits) max_payload_length)
        ~max_depth:(D.Limits.max_depth limits)
        ~max_fields:(D.Limits.max_fields limits)
        ~max_nodes:(D.Limits.max_nodes limits)
      |> domain
    in
    let%bind remaining = Capacity_growth.remaining_limits additional limits in
    let decimal = `String (Int64.to_string Int64.max_value) in
    let timestamp = `String "2099-12-31T23:59:59.999999999Z" in
    let summary =
      P.Session.to_json (Session_state.summary state)
      |> fun json ->
      replace
        json
        [ "revision", decimal; "latest_event_sequence", decimal; "updated_at", timestamp ]
    in
    (* Recovery changes public Run observation, so Session_transition.apply
       emits exactly this existing session.updated payload. It changes neither
       moderator projection nor extension statuses. No worker output is assumed. *)
    let%bind event =
      D.Document.create
        ~limits
        ~kind:"session.event"
        ~version:2
        ~payload:
          (`Object
              [ "session_id", P.Id.Session.to_json state.identity.session_id
              ; "sequence", decimal
              ; "revision", decimal
              ; "timestamp", timestamp
              ; "kind", `String "session.updated"
              ; "visibility", `String "full"
              ; "payload", summary
              ])
      |> domain
    in
    List.fold_result projections ~init:() ~f:(fun () projected ->
      let payload = D.Document.payload projected in
      let run =
        match D.Json.field payload ~name:"run_state" with
        | Value run -> run
        | Absent | Null -> assert false
      in
      let goal =
        match D.Json.field payload ~name:"goal_catalog" with
        | Value goal -> Some goal
        | Absent | Null -> None
      in
      let%bind delta =
        Session_delta_document.project_run_retirement ~run ~goal ~limits |> domain
      in
      let%bind transaction =
        Agent_store.Transaction.Capacity.document
          ~limits
          ~session_id:state.identity.session_id
          ~generation:state.identity.generation
          ~delta
          ~durable_events:[ event ]
        |> domain
      in
      D.Json.validate ~limits:remaining (D.Document.json transaction) |> domain)
;;
