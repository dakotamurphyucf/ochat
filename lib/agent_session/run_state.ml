open! Core
module P = Agent_protocol
module J = P.Json_codec
module X = Persistence_codec

module Run_id = struct
  include P.Id.Run
  include Comparator.Make (P.Id.Run)
end

module Receipt_key = struct
  module T = struct
    type t = P.Id.Principal.t * P.Idempotency_key.t [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

type t =
  { installation : Run_source_installation.t
  ; runs : P.Run.t Map.M(Run_id).t
  ; intents : Run_intent.t Map.M(Receipt_key).t
  ; receipts : P.Run_receipt.t Map.M(Receipt_key).t
  ; job_deliveries : Run_job_deliveries.t
  }

let empty =
  { installation = Run_source_installation.initial
  ; runs = Map.empty (module Run_id)
  ; intents = Map.empty (module Receipt_key)
  ; receipts = Map.empty (module Receipt_key)
  ; job_deliveries = Run_job_deliveries.empty
  }
;;

let equal_observation t other =
  Run_source_installation.equal t.installation other.installation
  && Map.equal P.Run.equal t.runs other.runs
  && Map.equal Run_intent.equal t.intents other.intents
  && Map.equal P.Run_receipt.equal t.receipts other.receipts
;;

let installation t = t.installation
let runs t = Map.data t.runs
let intents t = Map.data t.intents
let receipts t = Map.data t.receipts
let job_deliveries t = Run_job_deliveries.entries t.job_deliveries
let find_job_delivery t key = Run_job_deliveries.find t.job_deliveries key

let enqueued_job_frame t ~frame =
  Run_job_deliveries.enqueued_frame t.job_deliveries ~frame
;;

let find t id = Map.find t.runs id
let receipt_key (receipt : P.Run_receipt.t) = receipt.principal_id, receipt.key

let receipt t ~principal_id ~key ~request_sha256 =
  match Map.find t.receipts (principal_id, key) with
  | None -> Ok None
  | Some receipt ->
    if String.equal receipt.request_sha256 request_sha256
    then Ok (Some receipt)
    else
      Error (P.Error.invalid_request "run request key was committed with another digest")
;;

let current_source t (source : P.Run_source.t) =
  match Run_source_installation.captured t.installation ~generation:source.generation with
  | Error _ -> false
  | Ok installed -> P.Run_source.equal installed source
;;

let validate_job_deliveries t =
  let open Result.Let_syntax in
  let%bind pending_waits =
    Map.fold
      t.intents
      ~init:(Ok (Map.empty (module Run_id)))
      ~f:(fun ~key:_ ~data:intent checked ->
        let%bind waits = checked in
        match intent.Run_intent.disposition, intent.action with
        | Pending, Wait wake ->
          if Map.mem waits intent.receipt.run_id
          then Error (P.Error.invalid_request "run has multiple pending wake obligations")
          else Ok (Map.set waits ~key:intent.receipt.run_id ~data:wake)
        | Pending, (Continue | Finish _) | (Consumed _ | Retired), _ -> Ok waits)
  in
  (* Private Run constructors already reject duplicate terminal work. Cache by
     typed work identity once per run instead of repeatedly scanning its graph. *)
  let proof_maps =
    Map.map t.runs ~f:(fun run ->
      Map.of_alist_exn
        (module P.Run_work)
        (List.map run.P.Run.terminal_work ~f:(fun proof ->
           proof.P.Run_work.Terminal.work, proof)))
  in
  List.fold_result (job_deliveries t) ~init:() ~f:(fun () delivery ->
    match find t (Run_job_delivery.run_id delivery) with
    | None ->
      Error (P.Error.invalid_request "run job occurrence references an absent run")
    | Some run ->
      let frame = Run_job_delivery.frame delivery in
      let%bind work =
        P.Run_work.create
          ~key:(Retained (Job { id = frame.job_id; attempt = frame.attempt }))
          ~generation:frame.generation
      in
      let proof =
        Option.bind (Map.find proof_maps run.id) ~f:(fun proofs -> Map.find proofs work)
      in
      Run_job_delivery.validate_owner
        delivery
        ~run
        ~proof
        ~pending_wait:(Map.find pending_waits run.id)
        ~current_source:(current_source t run.source))
;;

let check_receipt (run : P.Run.t) (receipt : P.Run_receipt.t) =
  if
    P.Id.Run.equal run.id receipt.run_id
    && P.Id.Principal.equal run.principal_id receipt.principal_id
    && P.Run_source.equal run.source receipt.source
    && Int64.equal run.revision receipt.run_revision
  then Ok ()
  else Error (P.Error.invalid_request "run receipt does not match committed run revision")
;;

let check_intent (receipt : P.Run_receipt.t) = function
  | None -> Ok ()
  | Some (intent : Run_intent.t) ->
    if P.Run_receipt.equal intent.receipt receipt
    then Ok ()
    else Error (P.Error.invalid_request "run intent does not match its receipt")
;;

let to_jsonaf t =
  `Object
    ([ "installation", Run_source_installation.to_jsonaf t.installation
     ; "runs", X.list_json P.Run.to_json (runs t)
     ; "intents", X.list_json Run_intent.to_jsonaf (intents t)
     ; "receipts", X.list_json P.Run_receipt.to_json (receipts t)
     ]
     @ Run_job_deliveries.fields t.job_deliveries)
;;

let reserved_bytes t =
  let unresolved_job_bytes =
    Map.fold t.intents ~init:0 ~f:(fun ~key:_ ~data:intent bytes ->
      match intent.Run_intent.disposition, intent.action with
      | Pending, Wait wake ->
        (match Run_job_delivery.Key.of_wake wake with
         | None -> bytes
         | Some key ->
           (match Run_job_deliveries.find t.job_deliveries key with
            | Some _ -> bytes
            | None -> bytes + Run_job_delivery.reservation_bytes))
      | Pending, (Continue | Finish _)
      | (Consumed _ | Retired), (Continue | Wait _ | Finish _) -> bytes)
  in
  unresolved_job_bytes + Run_job_deliveries.reserved_bytes t.job_deliveries
;;

let validate_encoded_capacity_with_reserve t ~additional_bytes json =
  let open Result.Let_syntax in
  let%bind () =
    if additional_bytes < 0 || additional_bytes > P.Run_limits.max_document_bytes
    then Error (P.Error.invalid_request "run metadata reserve is out of range")
    else Ok ()
  in
  let reserved_keys =
    Map.fold
      t.intents
      ~init:
        (Set.of_list
           (module Run_job_delivery.Key)
           (List.map (job_deliveries t) ~f:Run_job_delivery.key))
      ~f:(fun ~key:_ ~data:intent keys ->
        match intent.Run_intent.disposition, intent.action with
        | Pending, Wait wake ->
          Option.value_map
            (Run_job_delivery.Key.of_wake wake)
            ~default:keys
            ~f:(Set.add keys)
        | Pending, (Continue | Finish _)
        | (Consumed _ | Retired), (Continue | Wait _ | Finish _) -> keys)
  in
  let%bind () = P.Run_limits.check_count (Set.length reserved_keys) in
  let max_bytes = P.Run_limits.max_document_bytes - reserved_bytes t - additional_bytes in
  if max_bytes <= 0
  then Error (P.Error.invalid_request "run occurrence metadata reserve exhausted")
  else P.Json_codec.validate_limits ~max_bytes ~max_depth:P.Run_limits.max_depth json
;;

let validate_encoded_capacity t json =
  validate_encoded_capacity_with_reserve t ~additional_bytes:0 json
;;

let validate_document_size t = validate_encoded_capacity t (to_jsonaf t)

let compatible_pending left right =
  match left, right with
  | P.Run_action.Continue, P.Run_action.Continue -> true
  | (Continue | Wait _ | Finish _), (Continue | Wait _ | Finish _) -> false
;;

let check_action t ~run_id ~action =
  List.fold_result (intents t) ~init:() ~f:(fun () intent ->
    match intent.Run_intent.disposition with
    | Consumed _ | Retired -> Ok ()
    | Pending ->
      if
        (not (P.Id.Run.equal intent.receipt.run_id run_id))
        || compatible_pending intent.action action
      then Ok ()
      else Error (P.Error.invalid_request "run has an unresolved incompatible action"))
;;

let commit t ~run ~receipt ~intent =
  let open Result.Let_syntax in
  let%bind () = P.Run.validate run in
  let%bind () = check_receipt run receipt in
  let%bind () = check_intent receipt intent in
  let key = receipt_key receipt in
  match Map.find t.receipts key with
  | Some previous ->
    if P.Run_receipt.equal previous receipt
    then Ok t
    else Error (P.Error.invalid_request "run receipt key has already been committed")
  | None ->
    let%bind () =
      match intent with
      | None -> Ok ()
      | Some intent ->
        (match intent.Run_intent.disposition with
         | Consumed _ | Retired -> Ok ()
         | Pending -> check_action t ~run_id:run.id ~action:intent.action)
    in
    let%bind () =
      if current_source t run.source
      then Ok ()
      else Error (P.Error.invalid_request "run source installation is no longer current")
    in
    let%bind () =
      match Map.find t.runs run.id with
      | None ->
        if Int64.equal run.revision 0L && P.Run.Lifecycle.equal run.lifecycle Admitted
        then Ok ()
        else Error (P.Error.invalid_request "new run requires an admitted revision zero")
      | Some previous -> P.Run.validate_transition ~previous run
    in
    let%bind () = P.Run_limits.check_count (Map.length t.receipts + 1) in
    let%bind () =
      P.Run_limits.check_count (Map.length t.runs + if Map.mem t.runs run.id then 0 else 1)
    in
    let intents =
      match intent with
      | None -> t.intents
      | Some intent -> Map.set t.intents ~key ~data:intent
    in
    let next =
      { t with
        runs = Map.set t.runs ~key:run.id ~data:run
      ; receipts = Map.set t.receipts ~key ~data:receipt
      ; intents
      ; job_deliveries =
          (match run.lifecycle with
           | Terminal _ ->
             Run_job_deliveries.retire_run
               t.job_deliveries
               ~run_id:run.id
               ~reason:Run_terminal
           | Admitted | Active | Waiting _ -> t.job_deliveries)
      }
    in
    let%bind () = validate_job_deliveries next in
    Result.map (validate_document_size next) ~f:(fun () -> next)
;;

let replace_installation t ~installation ~retired_runs =
  let open Result.Let_syntax in
  let%bind () = P.Run_limits.check_count (List.length retired_runs) in
  if Run_source_installation.equal t.installation installation
  then
    if List.is_empty retired_runs
    then Ok t
    else
      Error (P.Error.invalid_request "unchanged source installation cannot retire runs")
  else if Int64.(installation.epoch <> t.installation.epoch + 1L)
  then Error (P.Error.invalid_request "source installation must advance exactly once")
  else (
    let%bind replacements =
      match
        Map.of_alist
          (module Run_id)
          (List.map retired_runs ~f:(fun (run : P.Run.t) -> run.id, run))
      with
      | `Ok map -> Ok map
      | `Duplicate_key _ -> Error (P.Error.invalid_request "duplicate retired run")
    in
    let%bind runs =
      Map.fold t.runs ~init:(Ok t.runs) ~f:(fun ~key ~data:previous accumulated ->
        let%bind runs = accumulated in
        match previous.lifecycle with
        | Terminal _ ->
          if Map.mem replacements key
          then Error (P.Error.invalid_request "source retirement changed a terminal run")
          else Ok runs
        | Admitted | Active | Waiting _ ->
          (match Map.find replacements key with
           | None ->
             Error (P.Error.invalid_request "source replacement omitted a live run")
           | Some next ->
             let%bind () = P.Run.validate_transition ~previous next in
             (match next.lifecycle with
              | Terminal Interrupted -> Ok (Map.set runs ~key ~data:next)
              | Terminal (Completed _ | Failed _ | Cancelled | Limited)
              | Admitted | Active | Waiting _ ->
                Error
                  (P.Error.invalid_request
                     "source replacement requires interrupted run custody"))))
    in
    if Map.existsi replacements ~f:(fun ~key ~data:_ -> not (Map.mem t.runs key))
    then Error (P.Error.invalid_request "source retirement introduced an unknown run")
    else (
      let job_deliveries =
        Map.fold runs ~init:t.job_deliveries ~f:(fun ~key:_ ~data:run retained ->
          match run.P.Run.lifecycle with
          | Terminal _ ->
            Run_job_deliveries.retire_run retained ~run_id:run.id ~reason:Source_change
          | Admitted | Active | Waiting _ -> retained)
      in
      let next =
        { t with
          installation
        ; runs
        ; intents = Map.map t.intents ~f:Run_intent.retire
        ; job_deliveries
        }
      in
      let%bind () = validate_job_deliveries next in
      let%map () = validate_document_size next in
      next))
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind () =
    P.Json_codec.validate_limits
      ~max_bytes:P.Run_limits.max_document_bytes
      ~max_depth:P.Run_limits.max_depth
      json
  in
  let%bind fields = J.fields json in
  let%bind installation =
    J.required_as fields "installation" Run_source_installation.of_jsonaf
  in
  let%bind runs = J.required_as fields "runs" (P.Run_limits.list P.Run.of_json) in
  let%bind receipts =
    J.required_as fields "receipts" (P.Run_limits.list P.Run_receipt.of_json)
  in
  let%bind intents =
    J.required_as fields "intents" (P.Run_limits.list Run_intent.of_jsonaf)
  in
  let unique_map comparator entries =
    match Map.of_alist comparator entries with
    | `Ok map -> Ok map
    | `Duplicate_key _ ->
      Error (P.Error.invalid_request "duplicate durable run index key")
  in
  let%bind runs =
    unique_map (module Run_id) (List.map runs ~f:(fun (run : P.Run.t) -> run.id, run))
  in
  let%bind receipts =
    unique_map
      (module Receipt_key)
      (List.map receipts ~f:(fun receipt -> receipt_key receipt, receipt))
  in
  let%bind intents =
    unique_map
      (module Receipt_key)
      (List.map intents ~f:(fun (intent : Run_intent.t) ->
         receipt_key intent.receipt, intent))
  in
  let%bind job_deliveries =
    Run_job_deliveries.of_field
      (List.Assoc.find (J.to_alist fields) ~equal:String.equal "job_deliveries")
  in
  let t = { installation; runs; receipts; intents; job_deliveries } in
  let%bind () = validate_encoded_capacity t json in
  let%bind () =
    Map.fold receipts ~init:(Ok ()) ~f:(fun ~key:_ ~data:receipt result ->
      let%bind () = result in
      match find t receipt.run_id with
      | None ->
        Error (P.Error.invalid_request "durable receipt references an unknown run")
      | Some run ->
        if
          P.Id.Principal.equal run.principal_id receipt.principal_id
          && P.Run_source.equal run.source receipt.source
          && Int64.(receipt.run_revision <= run.revision)
        then Ok ()
        else Error (P.Error.invalid_request "durable receipt differs from its run"))
  in
  let%bind () =
    Map.fold intents ~init:(Ok ()) ~f:(fun ~key ~data:intent result ->
      let%bind () = result in
      let%bind () =
        match Map.find receipts key with
        | Some receipt when P.Run_receipt.equal receipt intent.receipt -> Ok ()
        | Some _ | None ->
          Error (P.Error.invalid_request "durable intent lacks its exact receipt")
      in
      match intent.disposition with
      | Consumed _ | Retired -> Ok ()
      | Pending ->
        if current_source t intent.receipt.source
        then Ok ()
        else
          Error (P.Error.invalid_request "pending intent belongs to an obsolete source"))
  in
  let%bind _ =
    Map.fold
      intents
      ~init:(Ok (Map.empty (module Run_id)))
      ~f:(fun ~key:_ ~data:intent accumulated ->
        let%bind pending = accumulated in
        match intent.Run_intent.disposition with
        | Consumed _ | Retired -> Ok pending
        | Pending ->
          let key = intent.receipt.run_id in
          (match Map.find pending key with
           | None -> Ok (Map.set pending ~key ~data:intent.action)
           | Some action when compatible_pending action intent.action -> Ok pending
           | Some _ ->
             Error (P.Error.invalid_request "incompatible retained pending run actions")))
  in
  let%bind () = validate_job_deliveries t in
  let%map () =
    Map.fold runs ~init:(Ok ()) ~f:(fun ~key:_ ~data:run result ->
      let%bind () = result in
      match run.lifecycle with
      | Terminal _ -> Ok ()
      | Admitted | Active | Waiting _ ->
        if current_source t run.source
        then Ok ()
        else Error (P.Error.invalid_request "live run belongs to an obsolete source"))
  in
  t
;;

let shape =
  X.shape_exn
    [ "installation", Run_source_installation.shape
    ; "runs", X.array_shape_exn ~identity_field:"id" Run_record_shapes.run
    ; "intents", X.array_shape_exn Run_record_shapes.intent
    ; "receipts", X.array_shape_exn Run_record_shapes.receipt
    ; "job_deliveries", Run_job_deliveries.shape
    ]
;;

let validate t ~session_id ~generation =
  let%bind.Result () = validate_document_size t in
  let%bind.Result () = validate_job_deliveries t in
  Map.fold t.runs ~init:(Ok ()) ~f:(fun ~key:_ ~data:run accumulated ->
    let%bind.Result () = accumulated in
    if not (P.Id.Session.equal (P.Session_ref.session_id run.session) session_id)
    then Error (P.Error.invalid_request "run index belongs to another session")
    else (
      match run.lifecycle with
      | Terminal _ -> Ok ()
      | Admitted | Active | Waiting _ ->
        if Int.equal run.source.generation generation
        then Ok ()
        else Error (P.Error.invalid_request "live run belongs to another generation")))
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () =
    Run_job_deliveries.validate_transition
      ~previous:previous.job_deliveries
      next.job_deliveries
  in
  let%bind () = validate_job_deliveries next in
  let rotated =
    not (Run_source_installation.equal previous.installation next.installation)
  in
  let%bind () =
    if
      (not rotated)
      || Int64.(
           previous.installation.epoch < max_value
           && next.installation.epoch = previous.installation.epoch + 1L)
    then Ok ()
    else
      Error
        (P.Error.invalid_request "run source installation transition skipped an epoch")
  in
  let%bind () =
    Map.fold previous.receipts ~init:(Ok ()) ~f:(fun ~key ~data:receipt accumulated ->
      let%bind () = accumulated in
      match Map.find next.receipts key with
      | Some retained when P.Run_receipt.equal receipt retained -> Ok ()
      | Some _ | None ->
        Error (P.Error.invalid_request "immutable run receipt disappeared or changed"))
  in
  let%bind () =
    Map.fold previous.intents ~init:(Ok ()) ~f:(fun ~key ~data:intent accumulated ->
      let%bind () = accumulated in
      match Map.find next.intents key with
      | None -> Error (P.Error.invalid_request "durable run intent disappeared")
      | Some retained ->
        if
          not
            (P.Run_receipt.equal intent.receipt retained.receipt
             && P.Id.Moderator_execution.equal intent.execution_id retained.execution_id
             && P.Run_action.equal intent.action retained.action)
        then Error (P.Error.invalid_request "durable run intent identity changed")
        else (
          match intent.disposition, retained.disposition with
          | Pending, (Consumed _ | Retired) -> Ok ()
          | Pending, Pending when not rotated -> Ok ()
          | Consumed left, Consumed right
            when Option.equal P.Id.Operation.equal left right -> Ok ()
          | Retired, Retired -> Ok ()
          | Pending, Pending
          | Consumed _, (Pending | Consumed _ | Retired)
          | Retired, (Pending | Consumed _) ->
            Error (P.Error.invalid_request "durable run intent disposition regressed")))
  in
  let%map () =
    Map.fold previous.runs ~init:(Ok ()) ~f:(fun ~key ~data:run accumulated ->
      let%bind () = accumulated in
      match Map.find next.runs key with
      | None -> Error (P.Error.invalid_request "durable run disappeared")
      | Some retained ->
        let%bind () = P.Run.validate_transition ~previous:run retained in
        (match rotated, run.lifecycle, retained.lifecycle with
         | false, _, _ | true, Terminal _, _ -> Ok ()
         | true, (Admitted | Active | Waiting _), Terminal Interrupted -> Ok ()
         | ( true
           , (Admitted | Active | Waiting _)
           , ( Admitted | Active | Waiting _
             | Terminal (Completed _ | Failed _ | Cancelled | Limited) ) ) ->
           Error
             (P.Error.invalid_request
                "source replacement did not interrupt former live run")))
  in
  ()
;;

let advance_owner t ~job_delivery ~(run : P.Run.t) ~intents =
  let open Result.Let_syntax in
  let%bind () = P.Run_limits.check_count (List.length intents) in
  let%bind () =
    if Map.mem t.runs run.id
    then Ok ()
    else Error (P.Error.invalid_request "owner evidence references an unknown run")
  in
  let%bind intents =
    List.fold_result intents ~init:t.intents ~f:(fun retained intent ->
      let key = receipt_key intent.Run_intent.receipt in
      if Map.mem retained key
      then Ok (Map.set retained ~key ~data:intent)
      else Error (P.Error.invalid_request "owner evidence introduced an unknown intent"))
  in
  let%bind job_deliveries =
    match job_delivery with
    | None -> Ok t.job_deliveries
    | Some delivery -> Run_job_deliveries.replace t.job_deliveries delivery
  in
  let job_deliveries =
    match run.lifecycle with
    | Terminal _ ->
      Run_job_deliveries.retire_run job_deliveries ~run_id:run.id ~reason:Run_terminal
    | Admitted | Active | Waiting _ -> job_deliveries
  in
  let next =
    { t with runs = Map.set t.runs ~key:run.id ~data:run; intents; job_deliveries }
  in
  let%bind () = validate_transition ~previous:t next in
  let%map () = validate_document_size next in
  next
;;

let advance t ~run ~intents = advance_owner t ~job_delivery:None ~run ~intents

let advance_claim t ~job_delivery ~run ~intents =
  advance_owner t ~job_delivery:(Some job_delivery) ~run ~intents
;;

let add_job_delivery t delivery =
  let open Result.Let_syntax in
  let%bind job_deliveries = Run_job_deliveries.add t.job_deliveries delivery in
  let next = { t with job_deliveries } in
  let%bind () = validate_job_deliveries next in
  let%map () = validate_document_size next in
  next
;;

let replace_job_delivery t delivery =
  let open Result.Let_syntax in
  let%bind job_deliveries = Run_job_deliveries.replace t.job_deliveries delivery in
  let next = { t with job_deliveries } in
  let%bind () = validate_job_deliveries next in
  let%map () = validate_document_size next in
  next
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_jsonaf t)

let t_of_sexp sexp =
  match of_jsonaf (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
;;

let complete_replacement t ~previous ~source =
  let open Result.Let_syntax in
  let%bind () = validate_transition ~previous t in
  let%bind () =
    if
      Int64.equal previous.installation.epoch Int64.max_value
      || not (Int64.equal t.installation.epoch (Int64.succ previous.installation.epoch))
    then
      Error
        (P.Error.invalid_request "replacement candidate has no single planned rotation")
    else Ok ()
  in
  let%bind installation =
    Run_source_installation.apply previous.installation ~change:(Reset source)
  in
  let next = { t with installation } in
  let%bind () = validate_transition ~previous next in
  let%map () = validate_document_size next in
  next
;;
