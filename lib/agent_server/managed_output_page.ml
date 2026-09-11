open Core
module P = Agent_protocol
module M = Agent_session.Managed_submission
module Cursor = Managed_output_cursor

let operation (receipt : M.t) =
  match receipt.status with
  | Assigned id | Terminal (Some id, _) -> Some id
  | Deferred | Ready | Terminal (None, _) -> None
;;

let correlations (state : Agent_session.Session_state.t) =
  let index = Hashtbl.create (module P.History.Id) in
  List.iter state.managed_submissions ~f:(fun receipt ->
    List.iter receipt.M.output_ids ~f:(fun id ->
      Hashtbl.add_multi index ~key:id ~data:receipt));
  index
;;

let output index (entry : P.History.entry) =
  let receipts = Option.value (Hashtbl.find index entry.id) ~default:[] in
  let receipt_ids =
    List.map receipts ~f:(fun receipt -> receipt.M.history_id)
    |> List.sort ~compare:P.History.Id.compare
  in
  let operations =
    List.filter_map receipts ~f:operation
    |> List.dedup_and_sort ~compare:P.Id.Operation.compare
  in
  let entry =
    match entry.redacted with
    | true -> { entry with payload = `Null }
    | false -> entry
  in
  `Object
    [ "history", P.History.entry_to_json entry
    ; "submission_ids", `Array (List.map receipt_ids ~f:P.History.Id.to_json)
    ; "operation_ids", `Array (List.map operations ~f:P.Id.Operation.to_json)
    ]
;;

let utf8_boundary text index =
  let rec before index =
    match
      index > 0 && index < String.length text && Char.to_int text.[index] land 0xc0 = 0x80
    with
    | true -> before (index - 1)
    | false -> index
  in
  before index
;;

let read
      signer
      ~(state : Agent_session.Session_state.t)
      ~receipt_id
      ~cursor
      ~history_epoch
      ~limit
      ~max_bytes
  =
  let open Result.Let_syntax in
  let%bind () =
    match limit > 0 && limit <= 128 && max_bytes > 0 with
    | true -> Ok ()
    | false -> Error (P.Error.invalid_request "invalid output page limits")
  in
  let%bind relationship =
    Result.of_option
      state.spec.delegation
      ~error:(P.Error.invalid_request "output reading requires a managed child")
  in
  let%bind receipt =
    match receipt_id with
    | None -> Ok None
    | Some id ->
      List.find state.managed_submissions ~f:(fun receipt ->
        P.History.Id.equal receipt.M.history_id id)
      |> Result.of_option
           ~error:(P.Error.invalid_request "submission receipt is unavailable")
      |> Result.map ~f:Option.some
  in
  let retained =
    List.drop
      state.conversation.canonical_history
      state.conversation.initial_prompt_entry_count
    |> List.filter ~f:(fun entry ->
      match entry.P.History.role, entry.kind with
      | Assistant, Message -> true
      | _ -> false)
  in
  let%bind () =
    match receipt with
    | None -> Ok ()
    | Some receipt ->
      let retained_ids =
        Hash_set.of_list
          (module P.History.Id)
          (List.map retained ~f:(fun entry -> entry.P.History.id))
      in
      let missing =
        List.count receipt.output_ids ~f:(fun id -> not (Hash_set.mem retained_ids id))
      in
      (match missing with
       | 0 -> Ok ()
       | _ ->
         Error
           (P.Error.create
              Snapshot_required
              ~message:"Some submission output is no longer in retained history."
              ~retryable:false
              ~data:
                (`Object
                    [ "missing_output_count", `Number (Int.to_string missing)
                    ; "snapshot_required", `True
                    ])
              ()))
  in
  let selected =
    match receipt with
    | None -> retained
    | Some receipt ->
      let ids = Hash_set.of_list (module P.History.Id) receipt.output_ids in
      List.filter retained ~f:(fun entry -> Hash_set.mem ids entry.P.History.id)
  in
  let index = correlations state in
  let records = List.map selected ~f:(output index) in
  let entries = List.map records ~f:Jsonaf.to_string in
  let%bind () =
    match List.for_all entries ~f:Stdlib.String.is_valid_utf_8 with
    | true -> Ok ()
    | false -> Error (P.Error.invalid_request "stored output is not valid UTF-8")
  in
  let context : Cursor.context =
    { relationship
    ; generation = state.identity.generation
    ; compaction_generation = state.conversation.compaction_generation
    ; history_epoch
    ; access_revision = "assistant-output.v1"
    ; query =
        Option.value_map receipt_id ~default:Cursor.All_outputs ~f:(fun id ->
          Submission id)
    }
  in
  let%bind position = Cursor.resolve signer ~context ~entries cursor in
  let count = List.length entries in
  let receipt_json = Option.value_map receipt ~default:`Null ~f:M.to_json in
  let response position items =
    let%map next_cursor = Cursor.issue signer ~context ~entries position in
    `Object
      [ "version", `Number "1"
      ; "session_id", P.Id.Session.to_json state.identity.session_id
      ; "generation", `Number (Int.to_string state.identity.generation)
      ; "revision", `Number (Int64.to_string state.counters.revision)
      ; "receipt", receipt_json
      ; "items", `Array items
      ; "next_cursor", P.Page.Cursor.to_json next_cursor
      ; ("caught_up", if Int.equal position.Cursor.entry count then `True else `False)
      ; ( "history_compacted"
        , if state.conversation.compaction_generation > 0 then `True else `False )
      ; ("snapshot", if Option.is_none cursor then `True else `False)
      ]
  in
  let fits position items =
    Result.map (response position items) ~f:(fun json ->
      String.length (Jsonaf.to_string json) <= max_bytes)
  in
  let rec collect position remaining items =
    match
      remaining, List.nth entries position.Cursor.entry, List.nth records position.entry
    with
    | 0, _, _ | _, None, _ -> response position items
    | _, Some text, Some record ->
      let next = { Cursor.entry = position.entry + 1; byte = 0 } in
      let full = `Object [ "kind", `String "output"; "value", record ] in
      let%bind full_fits =
        match position.byte with
        | 0 -> fits next (items @ [ full ])
        | _ -> Ok false
      in
      (match full_fits with
       | true -> collect next (remaining - 1) (items @ [ full ])
       | false ->
         let length = String.length text in
         let fragment stop =
           let complete = Int.equal stop length in
           let next =
             match complete with
             | true -> next
             | false -> { position with byte = stop }
           in
           let value =
             `Object
               [ "kind", `String "output_fragment"
               ; ( "entry_id"
                 , P.History.Id.to_json (List.nth_exn selected position.entry).id )
               ; "encoding", `String "output_record_json"
               ; "byte_offset", `Number (Int.to_string position.byte)
               ; "total_bytes", `Number (Int.to_string length)
               ; ("complete", if complete then `True else `False)
               ; ( "text"
                 , `String
                     (String.sub text ~pos:position.byte ~len:(stop - position.byte)) )
               ]
           in
           next, value
         in
         let rec search low high best =
           match low > high with
           | true -> Ok best
           | false ->
             let mid = low + ((high - low) / 2) in
             let stop = utf8_boundary text mid in
             (match stop > position.byte with
              | false -> search (mid + 1) high best
              | true ->
                let next, value = fragment stop in
                let%bind fits = fits next (items @ [ value ]) in
                (match fits with
                 | true -> search (mid + 1) high (Some (next, value))
                 | false -> search low (mid - 1) best))
         in
         (* Completing an entry shortens its cursor, so the final fragment can
            fit even when a slightly shorter partial fragment does not. Check
            completion separately before searching the monotone partial range. *)
         let completed_position, completed_fragment = fragment length in
         let%bind completed_fits =
           fits completed_position (items @ [ completed_fragment ])
         in
         let%bind chosen =
           match completed_fits with
           | true -> Ok (Some (completed_position, completed_fragment))
           | false -> search (position.byte + 1) (length - 1) None
         in
         (match chosen, items with
          | Some (next, value), _ -> response next (items @ [ value ])
          | None, _ :: _ -> response position items
          | None, [] ->
            Error
              (P.Error.invalid_request
                 "host output page limit cannot fit a response fragment")))
    | _ -> Error (P.Error.invalid_request "inconsistent output projection")
  in
  let%bind result = collect position limit [] in
  match String.length (Jsonaf.to_string result) <= max_bytes with
  | true -> Ok result
  | false ->
    Error (P.Error.invalid_request "host output page limit cannot fit response metadata")
;;
