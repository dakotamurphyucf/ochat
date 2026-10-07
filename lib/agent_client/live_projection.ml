open! Core
module P = Agent_protocol
module D = Document_schema
module Tool = P.Activity.Tool

let error code message = P.Error.create code ~message ~retryable:true ()
let invalid message = error Invalid_state message
let exhausted message = error Snapshot_required message

module Operation_id = struct
  include P.Id.Operation
  include Comparator.Make (P.Id.Operation)
end

module Activity_key = struct
  include P.Activity.Key
  include Comparator.Make (P.Activity.Key)
end

module Receipt_key = struct
  module T = struct
    type t = P.Id.Operation.t * int64 [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let document_limits max_bytes =
  Transcript.Admission.limits ~max_bytes
  |> Result.map_error ~f:(fun reason -> Sexp.to_string_hum (D.Error.sexp_of_t reason))
  |> Result.ok_or_failwith
;;

module Limits = struct
  type t =
    { max_operations : int
    ; max_receipts : int
    ; max_future_events : int
    ; max_event_bytes : int
    ; max_total_bytes : int
    ; document_limits : D.Limits.t
    ; retained_document_limits : D.Limits.t
    ; draft_limits : Transcript.Draft.Limits.t
    }

  let create
        ~max_operations
        ~max_receipts
        ~max_future_events
        ~max_event_bytes
        ~max_total_bytes
        ~draft_limits
    =
    if
      List.exists
        [ max_operations
        ; max_receipts
        ; max_future_events
        ; max_event_bytes
        ; max_total_bytes
        ]
        ~f:(fun bound -> bound <= 0)
    then Error (P.Error.invalid_request "live projection limits must be positive")
    else if max_event_bytes > Int.max_value / 4 || max_total_bytes > Int.max_value / 4
    then
      Error
        (P.Error.invalid_request
           "live projection byte limits exceed checked accounting range")
    else
      Ok
        { max_operations
        ; max_receipts
        ; max_future_events
        ; max_event_bytes
        ; max_total_bytes
        ; document_limits = document_limits max_event_bytes
        ; retained_document_limits = document_limits max_total_bytes
        ; draft_limits
        }
  ;;

  let default =
    let max_event_bytes = 16 * 1024 * 1024 in
    let max_total_bytes = 64 * 1024 * 1024 in
    let draft_limits =
      Transcript.Draft.Limits.create
        ~max_scopes:64
        ~max_items:1_024
        ~max_parts:8_192
        ~max_unknown_events:256
        ~max_retained_bytes:max_total_bytes
        ~document_limits:(document_limits max_event_bytes)
      |> Result.ok_or_failwith
    in
    create
      ~max_operations:16
      ~max_receipts:1_024
      ~max_future_events:256
      ~max_event_bytes
      ~max_total_bytes
      ~draft_limits
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> Result.ok_or_failwith
  ;;
end

type continuity =
  | Complete
  | Incomplete of { first_missing_sequence : int64 }
[@@deriving equal, sexp_of]

type operation_view =
  { operation_id : P.Id.Operation.t
  ; continuity : continuity
  ; drafts : Transcript.Draft.t
  ; activities : Tool.summary list
  }

type activity =
  { summary : Tool.summary
  ; bytes : int
  ; prefix_observed : bool
  }

type operation =
  { high_water : int64
  ; last_anchor : int64
  ; continuity : continuity
  ; drafts : Transcript.Draft.t
  ; tools : (P.Activity.Key.t, activity, Activity_key.comparator_witness) Map.t
  }

type receipt =
  { event : P.Event.Recoverable.t
  ; json : Jsonaf.t
  ; bytes : int
  }

type pending =
  { receipt : receipt
  ; first_missing : int64 option
  }

type fence =
  { operation_id : P.Id.Operation.t
  ; high_water : int64
  }

type t =
  { limits : Limits.t
  ; operations : (P.Id.Operation.t, operation, Operation_id.comparator_witness) Map.t
  ; receipts : (Receipt_key.t, receipt, Receipt_key.comparator_witness) Map.t
  ; receipt_order : Receipt_key.t list
  ; future : pending list
  ; fences : fence list
  ; terminal : (P.Id.Operation.t * operation) option
  }

let empty ?(limits = Limits.default) () =
  { limits
  ; operations = Map.empty (module Operation_id)
  ; receipts = Map.empty (module Receipt_key)
  ; receipt_order = []
  ; future = []
  ; fences = []
  ; terminal = None
  }
;;

let operation_view operation_id (operation : operation) =
  { operation_id
  ; continuity = operation.continuity
  ; drafts = operation.drafts
  ; activities =
      Map.data operation.tools |> List.map ~f:(fun activity -> activity.summary)
  }
;;

let operations t =
  Map.to_alist t.operations
  |> List.map ~f:(fun (operation_id, operation) -> operation_view operation_id operation)
;;

let terminal_view t =
  Option.map t.terminal ~f:(fun (operation_id, operation) ->
    operation_view operation_id operation)
;;

let activities t =
  Map.data t.operations
  |> List.concat_map ~f:(fun operation ->
    Map.data operation.tools |> List.map ~f:(fun activity -> activity.summary))
;;

let operation_bytes operation =
  Transcript.Draft.retained_bytes operation.drafts
  + Map.fold operation.tools ~init:0 ~f:(fun ~key:_ ~data sum -> sum + data.bytes)
;;

let receipt_key event = event.P.Event.Recoverable.operation_id, event.operation_sequence

let retained_bytes t =
  let receipts =
    Map.fold t.receipts ~init:0 ~f:(fun ~key:_ ~data sum -> sum + data.bytes)
  in
  let future =
    List.fold t.future ~init:0 ~f:(fun sum pending ->
      if Map.mem t.receipts (receipt_key pending.receipt.event)
      then sum
      else sum + pending.receipt.bytes)
  in
  receipts
  + future
  + Map.fold t.operations ~init:0 ~f:(fun ~key:_ ~data sum -> sum + operation_bytes data)
  + Option.value_map t.terminal ~default:0 ~f:(fun (_, operation) ->
    operation_bytes operation)
;;

let new_operation t =
  { high_water = 0L
  ; last_anchor = 0L
  ; continuity = Complete
  ; drafts = Transcript.Draft.create ~limits:t.limits.draft_limits
  ; tools = Map.empty (module Activity_key)
  }
;;

let find_operation t id =
  Map.find t.operations id |> Option.value_or_thunk ~default:(fun () -> new_operation t)
;;

let current id = function
  | Some operation -> P.Id.Operation.equal id operation.P.Operation.id
  | None -> false
;;

let fenced t id =
  List.exists t.fences ~f:(fun fence -> P.Id.Operation.equal id fence.operation_id)
;;

let measure t json =
  D.Json.validate_and_measure ~limits:t.limits.document_limits json
  |> Result.map_error ~f:(fun _ -> exhausted "live event exceeds bounded JSON admission")
;;

let measure_retained t json =
  D.Json.validate_and_measure ~limits:t.limits.retained_document_limits json
  |> Result.map_error ~f:(fun _ ->
    exhausted "live retained content exceeds bounded JSON admission")
;;

let within_budget t =
  if retained_bytes t > t.limits.max_total_bytes
  then Error (exhausted "live projection retained byte budget exceeded")
  else if
    (Map.length t.operations + if Option.is_some t.terminal then 1 else 0)
    > t.limits.max_operations
  then Error (exhausted "live projection operation capacity exceeded")
  else if List.length t.future > t.limits.max_future_events
  then Error (exhausted "live projection future anchor capacity exceeded")
  else Ok ()
;;

let clear t = empty ~limits:t.limits ()

let seed_activity t ~operation_id summaries =
  let open Result.Let_syntax in
  let%bind () =
    if fenced t operation_id
    then Error (exhausted "snapshot activity attempts to reopen a terminal operation")
    else Ok ()
  in
  let previous = find_operation t operation_id in
  let prior_tool_bytes =
    Map.fold previous.tools ~init:0 ~f:(fun ~key:_ ~data sum -> sum + data.bytes)
  in
  let available = t.limits.max_total_bytes - (retained_bytes t - prior_tool_bytes) in
  let%bind tools, _ =
    List.fold_result
      summaries
      ~init:(Map.empty (module Activity_key), 0)
      ~f:(fun (tools, retained) summary ->
        if Map.mem tools summary.Tool.key
        then Error (invalid "snapshot activity key is duplicated")
        else (
          let%bind bytes = measure_retained t (Tool.summary_to_json summary) in
          if bytes > available - retained
          then Error (exhausted "snapshot activity retained byte budget exceeded")
          else
            Ok
              ( Map.set
                  tools
                  ~key:summary.key
                  ~data:{ summary; bytes; prefix_observed = false }
              , retained + bytes )))
  in
  let operation = { previous with tools } in
  let candidate =
    { t with operations = Map.set t.operations ~key:operation_id ~data:operation }
  in
  let%map () = within_budget candidate in
  candidate
;;

let replace_snapshot t ~active_operation summaries =
  let operations =
    Map.map t.operations ~f:(fun operation ->
      { operation with
        drafts =
          (Transcript.Draft.create ~limits:t.limits.draft_limits
           |> fun drafts -> Transcript.Draft.mark_gap drafts ~scope:None)
      ; tools = Map.empty (module Activity_key)
      })
  in
  let t = { t with operations; terminal = None } in
  match active_operation with
  | None -> Result.map (within_budget t) ~f:(fun () -> t)
  | Some operation -> seed_activity t ~operation_id:operation.P.Operation.id summaries
;;

let evict_receipts t =
  let rec evict receipts order =
    if List.length order <= t.limits.max_receipts
    then receipts, order
    else (
      match order with
      | [] -> receipts, []
      | key :: rest -> evict (Map.remove receipts key) rest)
  in
  let receipts, receipt_order = evict t.receipts t.receipt_order in
  { t with receipts; receipt_order }
;;

let mark_gap operation first_missing_sequence =
  let open Result.Let_syntax in
  let continuity =
    match operation.continuity with
    | Complete -> Incomplete { first_missing_sequence }
    | Incomplete _ as continuity -> continuity
  in
  let%map tools =
    Map.fold
      operation.tools
      ~init:(Ok (Map.empty (module Activity_key)))
      ~f:(fun ~key ~data:activity result ->
        let%bind tools = result in
        let summary = activity.summary in
        let channels =
          List.map summary.channels ~f:(fun channel -> { channel with complete = false })
        in
        let%map summary =
          Tool.summary
            summary.key
            ~descriptor:summary.descriptor
            ~channels
            ~state:summary.state
          |> Result.map_error ~f:(fun error ->
            exhausted ("gapped tool summary rejected: " ^ error.P.Error.message))
        in
        (* false has one more encoded byte than true. Reserve that growth explicitly. *)
        let growth =
          List.count activity.summary.channels ~f:(fun channel -> channel.complete)
        in
        Map.set
          tools
          ~key
          ~data:{ summary; bytes = activity.bytes + growth; prefix_observed = false })
  in
  { operation with
    continuity
  ; drafts = Transcript.Draft.mark_gap operation.drafts ~scope:None
  ; tools
  }
;;

let reconcile drafts canonical_history =
  List.fold_result (Transcript.Draft.items drafts) ~init:drafts ~f:(fun drafts item ->
    match item.descriptor.scope.relation, item.state with
    | Transcript.Scope.Nested _, _ | Root, Partial _ -> Ok drafts
    | Root, Finalized entry ->
      (match
         List.find canonical_history ~f:(fun canonical ->
           History_entry.Id.equal canonical.P.Public.History.id (History_entry.id entry))
       with
       | None -> Ok drafts
       | Some { body = Visible _ | Redacted _; _ } -> Ok drafts
       | Some { body = Full payload; _ } ->
         if
           Jsonaf.exactly_equal
             (History_entry.Payload.to_json payload)
             (History_entry.Payload.to_json (History_entry.payload entry))
         then
           Ok (Transcript.Draft.remove_item drafts (Transcript.Item.key item.descriptor))
         else Error (invalid "finalized live payload conflicts with durable history")))
;;

let same_descriptor left right =
  Jsonaf.exactly_equal (Tool.to_json (Started left)) (Tool.to_json (Started right))
;;

let apply_tool t operation event ~budget =
  let open Result.Let_syntax in
  let key = Tool.key event in
  let previous = Map.find operation.tools key in
  let descriptor =
    Option.bind previous ~f:(fun activity -> activity.summary.descriptor)
  in
  let channels =
    Option.value_map previous ~default:[] ~f:(fun activity -> activity.summary.channels)
  in
  let state =
    Option.value_map previous ~default:Tool.Running ~f:(fun activity ->
      activity.summary.state)
  in
  let make descriptor channels state = Tool.summary key ~descriptor ~channels ~state in
  let%bind summary =
    match event with
    | Tool.Started next ->
      let%bind () =
        match descriptor with
        | Some previous when not (same_descriptor previous next) ->
          Error (invalid "tool activity descriptor changed")
        | None | Some _ -> Ok ()
      in
      make (Some next) channels state
    | Finished { outcome; output; _ } ->
      (match state with
       | Finished previous ->
         let candidate = make descriptor channels (Finished { outcome; output }) in
         let%bind candidate = candidate in
         let previous_event : Tool.event =
           Finished { key; outcome = previous.outcome; output = previous.output }
         in
         let%bind () =
           if Jsonaf.exactly_equal (Tool.to_json previous_event) (Tool.to_json event)
           then Ok ()
           else Error (invalid "tool activity terminal outcome changed")
         in
         Ok candidate
       | Running -> make descriptor channels (Finished { outcome; output }))
    | Progress { progress; _ } ->
      (match state with
       | Finished _ -> Error (invalid "tool activity progress followed terminal state")
       | Running ->
         let previous_channel =
           List.find channels ~f:(fun channel ->
             P.Activity.Progress.equal_channel channel.channel progress.channel)
         in
         let old_text =
           Option.value_map previous_channel ~default:"" ~f:(fun channel -> channel.text)
         in
         let%bind old_charge = measure_retained t (`String old_text) in
         let update = progress.update in
         let fragment =
           match update with
           | Append text | Replace text -> text
         in
         let%bind fragment_charge = measure_retained t (`String fragment) in
         let text_charge =
           match update with
           | Append _ -> old_charge + fragment_charge - 2
           | Replace _ -> fragment_charge
         in
         let complete =
           match update with
           | Append _ ->
             Option.value_map
               previous_channel
               ~default:
                 (Option.value_map previous ~default:false ~f:(fun activity ->
                    activity.prefix_observed))
               ~f:(fun channel -> channel.complete)
           | Replace _ -> true
         in
         let empty_channel : Tool.channel_text =
           { channel = progress.channel; text = ""; complete }
         in
         let other_channels =
           List.filter channels ~f:(fun channel ->
             not (P.Activity.Progress.equal_channel channel.channel progress.channel))
         in
         let%bind empty_summary =
           make descriptor (empty_channel :: other_channels) Running
         in
         let%bind base_charge = measure_retained t (Tool.summary_to_json empty_summary) in
         (* Exact escaped-text growth is admitted before concatenation. *)
         let%bind () =
           if base_charge + text_charge - 2 > budget
           then Error (exhausted "tool activity retained byte budget exceeded")
           else Ok ()
         in
         let text, complete =
           match update with
           | Append fragment ->
             ( old_text ^ fragment
             , Option.value_map
                 previous_channel
                 ~default:
                   (Option.value_map previous ~default:false ~f:(fun activity ->
                      activity.prefix_observed))
                 ~f:(fun channel -> channel.complete) )
           | Replace text -> text, true
         in
         let channel : Tool.channel_text =
           { channel = progress.channel; text; complete }
         in
         let channels =
           channel
           :: List.filter channels ~f:(fun previous ->
             not (P.Activity.Progress.equal_channel previous.channel progress.channel))
         in
         make descriptor channels Running)
  in
  let%bind bytes = measure_retained t (Tool.summary_to_json summary) in
  if bytes > budget
  then Error (exhausted "tool activity retained byte budget exceeded")
  else (
    let prefix_observed =
      match event with
      | Started _ -> true
      | Progress _ | Finished _ ->
        Option.value_map previous ~default:false ~f:(fun activity ->
          activity.prefix_observed)
    in
    Ok
      { operation with
        tools = Map.set operation.tools ~key ~data:{ summary; bytes; prefix_observed }
      })
;;

let apply_ready t pending ~canonical_history =
  let open Result.Let_syntax in
  let event = pending.receipt.event in
  let id = event.operation_id in
  let operation = find_operation t id in
  let%bind operation =
    match pending.first_missing with
    | None -> Ok operation
    | Some first_missing -> mark_gap operation first_missing
  in
  let other_bytes = retained_bytes t - operation_bytes (find_operation t id) in
  let%bind operation =
    match event.payload with
    | P.Event.Recoverable.Transcript stream ->
      let tool_bytes =
        Map.fold operation.tools ~init:0 ~f:(fun ~key:_ ~data sum -> sum + data.bytes)
      in
      let budget = t.limits.max_total_bytes - other_bytes - tool_bytes in
      let%bind drafts, _ =
        Transcript.Draft.apply operation.drafts ~max_retained_bytes:budget stream
        |> Result.map_error ~f:(fun reason ->
          exhausted ("live draft rejected: " ^ reason))
      in
      let%map drafts = reconcile drafts canonical_history in
      { operation with drafts }
    | Tool_activity activity ->
      let key = Tool.key activity in
      let old_bytes =
        Map.find operation.tools key
        |> Option.value_map ~default:0 ~f:(fun tool -> tool.bytes)
      in
      let remaining_tools =
        Map.fold operation.tools ~init:0 ~f:(fun ~key:_ ~data sum -> sum + data.bytes)
        - old_bytes
      in
      let budget =
        t.limits.max_total_bytes
        - other_bytes
        - Transcript.Draft.retained_bytes operation.drafts
        - remaining_tools
      in
      apply_tool t operation activity ~budget
  in
  let candidate = { t with operations = Map.set t.operations ~key:id ~data:operation } in
  let%map () = within_budget candidate in
  candidate
;;

let apply t ~durable_sequence ~active_operation ~canonical_history event =
  let open Result.Let_syntax in
  let id = event.P.Event.Recoverable.operation_id in
  let key = receipt_key event in
  let json = P.Event.Recoverable.to_json event in
  let retained =
    match Map.find t.receipts key with
    | Some _ as receipt -> receipt
    | None ->
      List.find_map t.future ~f:(fun pending ->
        if Receipt_key.compare (receipt_key pending.receipt.event) key = 0
        then Some pending.receipt
        else None)
  in
  match retained with
  | Some receipt ->
    if Jsonaf.exactly_equal receipt.json json
    then Ok t
    else Error (invalid "live sequence receipt conflicts with its retained event")
  | None ->
    let previous = (find_operation t id).high_water in
    if fenced t id || Int64.(event.operation_sequence <= previous)
    then Ok t
    else if Int64.(event.operation_sequence <= 0L || event.anchor_sequence < 0L)
    then Error (invalid "live sequence or anchor is outside its domain")
    else if Int64.(event.anchor_sequence < (find_operation t id).last_anchor)
    then Error (invalid "live durable anchor regressed")
    else if
      Int64.(event.anchor_sequence <= durable_sequence)
      && not (current id active_operation)
    then Ok t
    else (
      let%bind bytes = measure t json in
      let receipt = { event; json; bytes } in
      let first_missing =
        if Int64.(event.operation_sequence > previous + 1L)
        then Some Int64.(previous + 1L)
        else None
      in
      let operation =
        { (find_operation t id) with
          high_water = event.operation_sequence
        ; last_anchor = event.anchor_sequence
        }
      in
      let candidate =
        { t with
          operations = Map.set t.operations ~key:id ~data:operation
        ; receipts = Map.set t.receipts ~key ~data:receipt
        ; receipt_order = t.receipt_order @ [ key ]
        }
        |> evict_receipts
      in
      let pending = { receipt; first_missing } in
      let candidate =
        if Int64.(event.anchor_sequence > durable_sequence)
        then { candidate with future = candidate.future @ [ pending ] }
        else candidate
      in
      let%bind () = within_budget candidate in
      if Int64.(event.anchor_sequence > durable_sequence)
      then Ok candidate
      else apply_ready candidate pending ~canonical_history)
;;

let terminal_id event active_operation =
  match event.P.Public.Durable.body with
  | Full (Shared payload) | Filtered (Shared payload) ->
    (match P.Public.Durable.Shared_payload.value payload with
     | Operation_completed operation
     | Operation_failed operation
     | Operation_cancelled operation
     | Operation_interrupted operation -> Some operation.P.Operation.id
     | Session_created _
     | Session_state_changed _
     | Session_updated _
     | Attachment_owner_changed _
     | History_message_deferred _
     | History_appended _
     | History_replaced _
     | Moderator_overlay_changed _
     | Moderator_notification _
     | Permission_requested _
     | Permission_resolved _
     | Grant_created _
     | Grant_revoked _
     | Operation_started _
     | Job_state_changed _
     | Schedule_created _
     | Schedule_state_changed _
     | Schedule_cancelled _
     | Prompt_upgraded _
     | Workspace_state_changed _
     | Session_error _ -> None)
  | Hidden ->
    (match event.kind with
     | Operation_completed
     | Operation_failed
     | Operation_cancelled
     | Operation_interrupted ->
       Option.map active_operation ~f:(fun operation -> operation.P.Operation.id)
     | Session_created
     | Session_state_changed
     | Session_updated
     | Attachment_owner_changed
     | History_message_deferred
     | History_appended
     | History_replaced
     | Moderator_overlay_changed
     | Moderator_notification
     | Permission_requested
     | Permission_resolved
     | Grant_created
     | Grant_revoked
     | Operation_started
     | Job_state_changed
     | Schedule_created
     | Schedule_state_changed
     | Schedule_cancelled
     | Prompt_upgraded
     | Workspace_state_changed
     | Session_error -> None)
  | Full
      ( History_message_deferred _
      | History_appended _
      | History_replaced _
      | Moderator_overlay_changed _ )
  | Filtered
      ( History_message_deferred _
      | History_appended _
      | History_replaced _
      | Moderator_overlay_changed _ ) -> None
;;

let advance_durable t event ~previous_operation ~active_operation ~canonical_history =
  let open Result.Let_syntax in
  let t =
    if P.Event.Durable.equal_kind event.P.Public.Durable.kind Operation_started
    then { t with terminal = None }
    else t
  in
  let terminal = terminal_id event previous_operation in
  let t =
    match terminal with
    | None -> t
    | Some id ->
      let high_water = (find_operation t id).high_water in
      { t with
        operations = Map.remove t.operations id
      ; terminal =
          Option.map (Map.find t.operations id) ~f:(fun operation -> id, operation)
      ; future =
          List.filter t.future ~f:(fun pending ->
            not (P.Id.Operation.equal pending.receipt.event.operation_id id))
      ; fences =
          List.take
            ({ operation_id = id; high_water } :: t.fences)
            t.limits.max_operations
      }
  in
  let ready, future =
    List.partition_tf t.future ~f:(fun pending ->
      Int64.(pending.receipt.event.anchor_sequence <= event.sequence))
  in
  let t = { t with future } in
  let%bind t =
    List.fold_result ready ~init:t ~f:(fun t pending ->
      if
        fenced t pending.receipt.event.operation_id
        || not (current pending.receipt.event.operation_id active_operation)
      then Ok t
      else apply_ready t pending ~canonical_history)
  in
  let%bind operations =
    Map.fold
      t.operations
      ~init:(Ok (Map.empty (module Operation_id)))
      ~f:(fun ~key ~data accumulated ->
        let%bind accumulated = accumulated in
        let%map drafts = reconcile data.drafts canonical_history in
        Map.set accumulated ~key ~data:{ data with drafts })
  in
  let%bind terminal =
    match t.terminal with
    | None -> Ok None
    | Some (id, operation) ->
      let%map drafts = reconcile operation.drafts canonical_history in
      Some (id, { operation with drafts })
  in
  let candidate = { t with operations; terminal } in
  let%map () = within_budget candidate in
  candidate
;;
