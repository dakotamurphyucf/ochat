open! Core
module D = Document_schema
module P = Agent_protocol

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.create
      Resource_limit
      ~message:(Sexp.to_string_hum (D.Error.sexp_of_t error))
      ~retryable:false
      ())
;;

let validate (state : Session_state.t) ~limits ~max_payload_length =
  let open Result.Let_syntax in
  match state.instruction_selection with
  | None -> Ok ()
  | Some selection ->
    let summary = Instruction_selection.summary selection in
    if (not summary.project_selected) && summary.skills = 0
    then Ok ()
    else (
      let%bind raw = Instruction_selection.to_jsonaf selection in
      let%bind limits =
        D.Limits.create
          ~max_bytes:(Int.min (D.Limits.max_bytes limits) max_payload_length)
          ~max_depth:(D.Limits.max_depth limits)
          ~max_fields:(D.Limits.max_fields limits)
          ~max_nodes:(D.Limits.max_nodes limits)
        |> domain
      in
      (* Future Clear retains all captures, payloads, receipts and unknowns, adds
         one bounded receipt and removes active references. Keeping those current
         references here is conservative. The same proven receipt+decimal reserve
         used by the selection/document owner bounds its positive future growth. *)
      let%bind remaining =
        Instruction_selection_capacity.remaining_limits ~active:true limits |> domain
      in
      let%bind delta =
        Session_delta_document.project_instruction_clear raw ~limits |> domain
      in
      let%bind status =
        P.Instruction_status.create
          ~revision:Int64.(summary.revision + 1L)
          ~project_selected:false
          ~project_files:0
          ~skills:0
          ~retained_captures:summary.retained_captures
          ~retained_receipts:(summary.retained_receipts + 1)
      in
      (* Use the real typed event and durable-document serializer. Maximal host
         counters/timestamp cover every later Clear anchor, independently of the
         selection receipt reserve and the transaction encoder's own maxima. *)
      let%bind timestamp = P.Timestamp.of_string "2099-12-31T23:59:59.999999999Z" in
      let session =
        { (Session_state.summary state) with
          revision = Int64.max_value
        ; latest_event_sequence = Int64.max_value
        ; updated_at = timestamp
        }
      in
      let event =
        P.Event.Durable.of_payload
          ~session_id:state.identity.session_id
          ~sequence:Int64.max_value
          ~revision:Int64.max_value
          ~timestamp
          (Session_updated session)
      in
      let%bind event = P.Event.Durable.with_instruction_status event status in
      let%bind event = Durable_event_document.create event ~limits |> domain in
      (* The selected native Clear enters the internal actor command route. Its
         own durable receipt is in the delta; no external server-request audit is
         fabricated. Any future audited route must qualify its own envelope. *)
      let%bind transaction =
        Agent_store.Transaction.Capacity.document
          ~limits
          ~session_id:state.identity.session_id
          ~generation:state.identity.generation
          ~delta
          ~durable_events:[ Durable_event_document.document event ]
        |> domain
      in
      D.Json.validate ~limits:remaining (D.Document.json transaction) |> domain)
;;
