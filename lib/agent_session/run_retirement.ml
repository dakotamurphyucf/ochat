open! Core
module P = Agent_protocol

let interrupted (run : P.Run.t) ~evidence ~now =
  let open Result.Let_syntax in
  if Int64.equal run.revision Int64.max_value
  then Error (P.Error.invalid_request "run revision exhausted during custody retirement")
  else (
    let revision = Int64.succ run.revision in
    let%bind () = P.Run_limits.check_count (List.length evidence) in
    let previous =
      Set.of_list
        (module P.Run_work)
        (List.map run.terminal_work ~f:(fun proof -> proof.P.Run_work.Terminal.work))
    in
    let owned = Set.of_list (module P.Run_work) run.owned_work in
    let added =
      Set.of_list
        (module P.Run_work)
        (List.map evidence ~f:(fun proof -> proof.P.Run_work.Terminal.work))
    in
    let%bind () =
      if
        Set.length added = List.length evidence
        && List.for_all evidence ~f:(fun (proof : P.Run_work.Terminal.t) ->
          Set.mem owned proof.work
          && (not (Set.mem previous proof.work))
          && Int64.equal proof.revision revision)
      then Ok ()
      else
        Error (P.Error.invalid_request "retirement evidence is not new exact owned work")
    in
    let known = Set.union previous added in
    let%bind added =
      List.filter run.owned_work ~f:(fun work -> not (Set.mem known work))
      |> List.map ~f:(fun work ->
        P.Run_work.Terminal.create ~work ~outcome:Unconfirmed ~revision)
      |> Result.all
    in
    P.Run.create
      ~id:run.id
      ~session:run.session
      ~principal_id:run.principal_id
      ~source:run.source
      ~mode:run.mode
      ~lifecycle:(Terminal Interrupted)
      ~revision
      ~owned_work:run.owned_work
      ~relinquished_work:run.relinquished_work
      ~terminal_work:(run.terminal_work @ evidence @ added)
      ~created_at:run.created_at
      ~updated_at:now)
;;

let interrupt_with_evidence index ~run_id ~evidence ~session_revision ~now =
  let open Result.Let_syntax in
  let%bind run =
    Result.of_option
      (Run_state.find index run_id)
      ~error:(P.Error.invalid_request "retired run is absent")
  in
  match run.P.Run.lifecycle with
  | Terminal _ ->
    if List.is_empty evidence
    then Ok index
    else
      Error
        (P.Error.invalid_request "terminal run cannot acquire new retirement evidence")
  | Admitted | Active | Waiting _ ->
    let%bind run = interrupted run ~evidence ~now in
    let intents =
      List.filter (Run_state.intents index) ~f:(fun intent ->
        P.Id.Run.equal intent.Run_intent.receipt.run_id run.id)
      |> List.map ~f:Run_intent.retire
    in
    let%bind index = Run_state.advance index ~run ~intents in
    let%bind key =
      P.Idempotency_key.of_string
        ("run-interrupted:"
         ^ P.Id.Run.to_string run.id
         ^ ":"
         ^ Int64.to_string run.revision)
    in
    let request_sha256 =
      P.Run.Terminal.to_json Interrupted
      |> Jsonaf.to_string
      |> Digestif.SHA256.digest_string
      |> Digestif.SHA256.to_hex
    in
    let%bind receipt =
      P.Run_receipt.create
        ~run_id:run.id
        ~principal_id:run.principal_id
        ~source:run.source
        ~key
        ~request_sha256
        ~kind:Terminal
        ~run_revision:run.revision
        ~session_revision
        ~committed_at:now
    in
    Run_state.commit index ~run ~receipt ~intent:None
;;

let interrupt_run index ~run_id ~session_revision ~now =
  interrupt_with_evidence index ~run_id ~evidence:[] ~session_revision ~now
;;

let recover index ~session_revision ~now =
  List.fold_result (Run_state.runs index) ~init:index ~f:(fun index run ->
    interrupt_run index ~run_id:run.P.Run.id ~session_revision ~now)
;;

let replace index ~change ~session_revision ~now =
  let open Result.Let_syntax in
  let%bind installation =
    Run_source_installation.apply (Run_state.installation index) ~change
  in
  if Run_source_installation.equal installation (Run_state.installation index)
  then Ok index
  else (
    let%bind index = recover index ~session_revision ~now in
    Run_state.replace_installation index ~installation ~retired_runs:[])
;;
