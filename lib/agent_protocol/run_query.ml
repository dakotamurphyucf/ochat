open! Core
module J = Json_codec

let invalid message = Error (Protocol_error.invalid_request message)

let bounded json =
  Extension_codec.validate_json
    ~max_bytes:Run_limits.max_document_bytes
    ~max_depth:Run_limits.max_depth
    json
;;

let nullable decode = function
  | `Null -> Ok None
  | json -> Result.map (decode json) ~f:Option.some
;;

module Request = struct
  type t =
    { session : Session_ref.t
    ; page : Page.Request.t
    }

  let create ~session ~page =
    let%bind.Result page =
      Page.Request.create ~limit:page.Page.Request.limit ?cursor:page.cursor ()
    in
    Ok { session; page }
  ;;

  let to_json t =
    `Object (("session", Session_ref.to_json t.session) :: Page.Request.to_fields t.page)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = bounded json in
    let%bind fields = J.fields json in
    let%bind session = J.required_as fields "session" Session_ref.of_json in
    let%bind page = Page.Request.of_fields fields in
    create ~session ~page
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Lookup_request = struct
  type t =
    { session : Session_ref.t
    ; run_id : Id.Run.t
    }

  let to_json t =
    `Object
      [ "session", Session_ref.to_json t.session; "run_id", Id.Run.to_json t.run_id ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = bounded json in
    let%bind fields = J.fields json in
    let%bind session = J.required_as fields "session" Session_ref.of_json in
    let%map run_id = J.required_as fields "run_id" Id.Run.of_json in
    { session; run_id }
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module View = struct
  (* Reserve the exact Available wrapper bytes and one object depth so every
     admitted View also fits its enclosing Outcome without a second ceiling. *)
  let bounded json =
    let overhead =
      String.length
        (Jsonaf.to_string (`Object [ "kind", `String "available"; "view", `Null ]))
      - String.length (Jsonaf.to_string `Null)
    in
    Extension_codec.validate_json
      ~max_bytes:(Run_limits.max_document_bytes - overhead)
      ~max_depth:(Run_limits.max_depth - 1)
      json
  ;;

  type t =
    { run : Run.t
    ; result_references : Run_result_reference.t list
    ; pending_action : Run_action.t option
    ; admission_receipt : Run_receipt.t option
    ; terminal_receipt : Run_receipt.t option
    ; session_revision : int64
    ; event_sequence : int64
    }

  let receipt_matches (run : Run.t) (receipt : Run_receipt.t) =
    Id.Run.equal run.id receipt.run_id
    && Id.Principal.equal run.principal_id receipt.principal_id
    && Run_source.equal run.source receipt.source
    && Int64.(receipt.run_revision <= run.revision)
  ;;

  let reference_matches (run : Run.t) reference =
    let proof key generation ~matches =
      Int.equal generation run.source.generation
      && List.exists run.terminal_work ~f:(fun evidence ->
        Run_work.Key.equal evidence.Run_work.Terminal.work.key key
        && Int.equal evidence.work.generation generation
        && matches evidence.outcome)
    in
    match reference with
    | Run_result_reference.Job result ->
      Id.Session.equal result.session_id (Session_ref.session_id run.session)
      && proof
           (Retained (Job { id = result.job_id; attempt = result.attempt }))
           result.generation
           ~matches:(fun outcome ->
             match result.outcome, outcome with
             | Stored_completion.Succeeded, Run_work.Terminal.Succeeded
             | Failed, (Failed | Limited | Interrupted)
             | Cancelled, Cancelled
             | Expired, Limited -> true
             | Succeeded, (Failed | Cancelled | Limited | Interrupted | Unconfirmed)
             | Failed, (Succeeded | Cancelled | Unconfirmed)
             | Cancelled, (Succeeded | Failed | Limited | Interrupted | Unconfirmed)
             | Expired, (Succeeded | Failed | Cancelled | Interrupted | Unconfirmed) ->
               false)
    | Operation result ->
      proof
        (Operation result.operation_id)
        result.generation
        ~matches:(Run_work.Terminal.equal_outcome Succeeded)
  ;;

  let optional encode = Option.value_map ~default:`Null ~f:encode

  let to_json t =
    `Object
      [ "run", Run.to_json t.run
      ; ( "result_references"
        , `Array (List.map t.result_references ~f:Run_result_reference.to_json) )
      ; "pending_action", optional Run_action.to_json t.pending_action
      ; "admission_receipt", optional Run_receipt.to_json t.admission_receipt
      ; "terminal_receipt", optional Run_receipt.to_json t.terminal_receipt
      ; "session_revision", `String (Int64.to_string t.session_revision)
      ; "event_sequence", `String (Int64.to_string t.event_sequence)
      ]
  ;;

  let create
        ~run
        ~result_references
        ~pending_action
        ~admission_receipt
        ~terminal_receipt
        ~session_revision
        ~event_sequence
    =
    let open Result.Let_syntax in
    let%bind () = Run.validate run in
    let%bind () = Run_limits.check_count (List.length result_references) in
    let%bind () =
      List.fold_result result_references ~init:() ~f:(fun () reference ->
        let%bind () = Run_result_reference.validate reference in
        if reference_matches run reference
        then Ok ()
        else invalid "retained result differs from exact owned terminal evidence")
    in
    let%bind _ =
      List.fold_result
        result_references
        ~init:(Set.empty (module Run_work))
        ~f:(fun seen reference ->
          let key, generation =
            match reference with
            | Run_result_reference.Job result ->
              ( Run_work.Key.Retained
                  (Job { id = result.job_id; attempt = result.attempt })
              , result.generation )
            | Operation result ->
              Run_work.Key.Operation result.operation_id, result.generation
          in
          let%bind work = Run_work.create ~key ~generation in
          if Set.mem seen work
          then invalid "duplicate retained run result occurrence"
          else Ok (Set.add seen work))
    in
    let%bind () =
      let succeeded key generation =
        List.exists run.terminal_work ~f:(fun evidence ->
          Run_work.Key.equal evidence.Run_work.Terminal.work.key key
          && Int.equal evidence.work.generation generation
          && Run_work.Terminal.equal_outcome evidence.outcome Succeeded)
      in
      match run.lifecycle with
      | Terminal (Completed (Some (Run_result_reference.Job result))) ->
        if
          Id.Session.equal result.session_id (Session_ref.session_id run.session)
          && Int.equal result.generation run.source.generation
          && succeeded
               (Retained (Job { id = result.job_id; attempt = result.attempt }))
               result.generation
        then Ok ()
        else invalid "run result does not belong to successful owned job occurrence"
      | Terminal (Completed (Some (Run_result_reference.Operation result))) ->
        if
          Int.equal result.generation run.source.generation
          && succeeded (Operation result.operation_id) result.generation
        then Ok ()
        else invalid "run result does not belong to successful owned operation"
      | Admitted | Active | Waiting _
      | Terminal (Completed None | Failed _ | Cancelled | Limited | Interrupted) -> Ok ()
    in
    let%bind () =
      match pending_action with
      | None -> Ok ()
      | Some action -> Run_action.validate action
    in
    let%bind () =
      if Int64.(session_revision < 0L || event_sequence < 0L)
      then invalid "negative run observation revision"
      else Ok ()
    in
    let%bind () =
      match admission_receipt with
      | None -> Ok ()
      | Some receipt ->
        if receipt_matches run receipt && Run_receipt.Kind.equal receipt.kind Admission
        then Ok ()
        else invalid "run admission receipt does not match the observed run"
    in
    let%bind () =
      match terminal_receipt, run.lifecycle with
      | None, _ -> Ok ()
      | Some receipt, Terminal _ ->
        if receipt_matches run receipt && Run_receipt.Kind.equal receipt.kind Terminal
        then Ok ()
        else invalid "run terminal receipt does not match the observed run"
      | Some _, (Admitted | Active | Waiting _) ->
        invalid "nonterminal run has a terminal receipt"
    in
    let%bind () =
      match pending_action, run.lifecycle with
      | None, (Admitted | Active | Waiting _ | Terminal _) -> Ok ()
      | Some Continue, Active | Some (Finish _), Active -> Ok ()
      | Some (Wait wake), Waiting retained ->
        if Run_wake.equal wake retained
        then Ok ()
        else invalid "pending wake differs from committed waiting occurrence"
      | Some (Continue | Finish _), (Admitted | Waiting _ | Terminal _)
      | Some (Wait _), (Admitted | Active | Terminal _) ->
        invalid "pending action differs from committed run lifecycle"
    in
    let t =
      { run
      ; result_references
      ; pending_action
      ; admission_receipt
      ; terminal_receipt
      ; session_revision
      ; event_sequence
      }
    in
    let%map () = bounded (to_json t) in
    t
  ;;

  let revision_of_json = function
    | `String encoded ->
      (match Int64.of_string_opt encoded with
       | Some value
         when Int64.(value >= 0L) && String.equal encoded (Int64.to_string value) ->
         Ok value
       | Some _ | None ->
         invalid "run observation revision must be canonical nonnegative decimal")
    | _ -> invalid "run observation revision must be a decimal string"
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = bounded json in
    let%bind fields = J.fields json in
    let%bind run = J.required_as fields "run" Run.of_json in
    let%bind result_references =
      J.required_as
        fields
        "result_references"
        (Run_limits.list Run_result_reference.of_json)
    in
    let%bind pending_action =
      J.required_as fields "pending_action" (nullable Run_action.of_json)
    in
    let%bind admission_receipt =
      J.required_as fields "admission_receipt" (nullable Run_receipt.of_json)
    in
    let%bind terminal_receipt =
      J.required_as fields "terminal_receipt" (nullable Run_receipt.of_json)
    in
    let%bind session_revision =
      J.required_as fields "session_revision" revision_of_json
    in
    let%bind event_sequence = J.required_as fields "event_sequence" revision_of_json in
    create
      ~run
      ~result_references
      ~pending_action
      ~admission_receipt
      ~terminal_receipt
      ~session_revision
      ~event_sequence
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end

module Outcome = struct
  type t =
    | Available of View.t
    | Unavailable of Id.Run.t

  let available view = Available view
  let unavailable id = Unavailable id

  let to_json = function
    | Available view -> `Object [ "kind", `String "available"; "view", View.to_json view ]
    | Unavailable id ->
      `Object [ "kind", `String "unavailable"; "run_id", Id.Run.to_json id ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = bounded json in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    match kind with
    | "available" -> Result.map (J.required_as fields "view" View.of_json) ~f:available
    | "unavailable" ->
      Result.map (J.required_as fields "run_id" Id.Run.of_json) ~f:unavailable
    | _ -> invalid "unsupported run observation outcome"
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok value -> value
    | Error error -> Sexplib.Conv.of_sexp_error error.message sexp
  ;;
end
