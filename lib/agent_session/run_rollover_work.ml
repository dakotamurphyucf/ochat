open! Core
module P = Agent_protocol

type owner =
  | Unowned
  | Unresolved of P.Run.t
  | Retired

let conflict message = P.Error.create Conflict ~message ~retryable:false ()

let operation_work (state : Session_state.t) id =
  P.Run_work.create ~key:(Operation id) ~generation:state.identity.generation
;;

let unresolved (run : P.Run.t) work =
  (match run.lifecycle with
   | Terminal _ -> false
   | Admitted | Active | Waiting _ -> true)
  && List.mem run.owned_work work ~equal:P.Run_work.equal
  && not
       (List.exists run.terminal_work ~f:(fun proof -> P.Run_work.equal proof.work work))
;;

let owner state ~predecessor =
  let open Result.Let_syntax in
  let%bind work = operation_work state predecessor in
  let%map previous = Goal_work_attribution.run_of_work state work in
  match previous with
  | None -> Unowned
  | Some run when unresolved run work -> Unresolved run
  | Some _ -> Retired
;;

let dispatch_owner state ~predecessor =
  let open Result.Let_syntax in
  match%bind owner state ~predecessor with
  | Unowned -> Ok None
  | Unresolved run -> Ok (Some run)
  | Retired -> Error (conflict "rollover predecessor has no live Run obligation")
;;

let transfer state ~successor ~previous ~run ~now =
  let open Result.Let_syntax in
  let%bind next = operation_work state successor in
  let%bind () =
    match%bind Goal_work_attribution.run_of_work state next with
    | None -> Ok ()
    | Some _ -> Error (conflict "rollover successor already belongs to a Run")
  in
  if Int64.equal run.P.Run.revision Int64.max_value
  then Error (conflict "run revision exhausted")
  else (
    let%bind changed =
      P.Run.create
        ~id:run.id
        ~session:run.session
        ~principal_id:run.principal_id
        ~source:run.source
        ~mode:run.mode
        ~lifecycle:run.lifecycle
        ~revision:(Int64.succ run.revision)
        ~owned_work:
          (List.map run.owned_work ~f:(fun work ->
             if P.Run_work.equal work previous then next else work))
        ~relinquished_work:(previous :: run.relinquished_work)
        ~terminal_work:run.terminal_work
        ~created_at:run.created_at
        ~updated_at:now
    in
    let%bind index =
      Result.of_option
        state.Session_state.run_state
        ~error:(conflict "rollover Run index disappeared")
    in
    Run_state.advance index ~run:changed ~intents:[])
;;

let prepare state ~predecessor ~successor ~now =
  let open Result.Let_syntax in
  if P.Id.Operation.equal predecessor successor
  then Error (conflict "a rollover must select a fresh foreground Operation")
  else (
    match%bind dispatch_owner state ~predecessor with
    | None -> Ok None
    | Some run ->
      let%bind previous = operation_work state predecessor in
      let%map index = transfer state ~successor ~previous ~run ~now in
      Some (run.id, index))
;;

let adds_operation_relinquishment ~previous next =
  List.exists (Run_state.runs next) ~f:(fun run ->
    let prior =
      Option.bind previous ~f:(fun index -> Run_state.find index run.P.Run.id)
    in
    if Option.exists prior ~f:(phys_equal run)
    then false
    else (
      let existing =
        Option.value_map
          prior
          ~default:(Set.empty (module P.Run_work))
          ~f:(fun run -> Set.of_list (module P.Run_work) run.relinquished_work)
      in
      List.exists run.relinquished_work ~f:(fun work ->
        match work.P.Run_work.key with
        | Retained _ -> false
        | Operation _ -> not (Set.mem existing work))))
;;

let validate state ~predecessor ~successor proposed =
  let open Result.Let_syntax in
  let invalid () =
    P.Error.invalid_request "rollover Run transfer differs from its exact admission"
  in
  let%bind owner = dispatch_owner state ~predecessor in
  let%bind run = Result.of_option owner ~error:(invalid ()) in
  let%bind changed =
    Result.of_option (Run_state.find proposed run.id) ~error:(invalid ())
  in
  let%bind previous = operation_work state predecessor in
  let%bind expected = transfer state ~successor ~previous ~run ~now:changed.updated_at in
  if Run_state.equal expected proposed then Ok () else Error (invalid ())
;;
