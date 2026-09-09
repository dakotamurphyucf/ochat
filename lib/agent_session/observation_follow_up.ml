open Core
module P = Agent_protocol
module I = P.Invocation
module E = P.Moderator_execution

type action =
  | Checkpoint
  | Stop of string
  | Compact
  | Turn
[@@deriving sexp_of]

type t =
  { action : action
  ; invocations : I.t list
  ; events : E.t list
  }

let pending_observation (invocation : I.t) =
  match invocation.observation with
  | Some
      { status = Observed
      ; follow_up = Some (Pending_follow_up _ | Compaction_accepted_follow_up _)
      ; _
      } -> true
  | _ -> false
;;

let pending_handler (invocation : I.t) =
  match invocation.handler_intent with
  | Some { follow_up = Pending_follow_up _ | Compaction_accepted_follow_up _; _ } -> true
  | _ -> false
;;

let pending invocation = pending_observation invocation || pending_handler invocation

let pending_event (event : E.t) =
  match event.status, event.intent with
  | Completed _, Some (Pending | Waiting_compaction _) -> true
  | _ -> false
;;

let discard_events events ~reason =
  List.filter events ~f:pending_event
  |> List.map ~f:(fun event -> E.discard_intent event ~reason)
  |> Result.all
;;

let event_delta (event : E.t) =
  match event.intent with
  | Some (Discarded _) -> Session_delta.Moderator_execution_reconciled event
  | _ -> Session_delta.Moderator_execution_changed event
;;

let discard_event_compaction events ~operation_id ~reason =
  List.filter events ~f:(fun event ->
    match event.E.intent with
    | Some (Waiting_compaction id) -> P.Id.Operation.equal operation_id id
    | _ -> false)
  |> discard_events ~reason
;;

let discard invocations ~reason =
  List.filter invocations ~f:pending
  |> List.map ~f:(fun invocation ->
    let open Result.Let_syntax in
    let%bind invocation =
      match pending_observation invocation with
      | true -> I.discard_observation_follow_up invocation ~reason
      | false -> Ok invocation
    in
    match pending_handler invocation with
    | true -> I.discard_handler_intent invocation ~reason
    | false -> Ok invocation)
  |> Result.all
;;

let requests (invocation : I.t) =
  match invocation.observation with
  | Some { follow_up = Some (Pending_follow_up requests); _ } -> Some requests
  | Some { follow_up = Some (Compaction_accepted_follow_up requests); _ } ->
    Some { requests with request_compaction = false }
  | _ -> None
;;

let delta (invocation : I.t) =
  match invocation.handler_intent, invocation.observation with
  | Some { follow_up = Discarded_follow_up _; _ }, _
  | _, Some { follow_up = Some (Discarded_follow_up _); _ } ->
    Session_delta.Invocation_reconciled invocation
  | _ -> Session_delta.Invocation_changed invocation
;;

let discard_compaction invocations ~operation_id ~reason =
  List.filter_map invocations ~f:(fun invocation ->
    let observation =
      match invocation.I.observation with
      | Some
          { follow_up = Some (Compaction_accepted_follow_up _)
          ; compaction_operation_id
          ; _
          } ->
        Option.value_map
          compaction_operation_id
          ~default:true
          ~f:(P.Id.Operation.equal operation_id)
      | _ -> false
    in
    let handler =
      match invocation.handler_intent with
      | Some
          { follow_up = Compaction_accepted_follow_up _
          ; compaction_operation_id = Some id
          } -> P.Id.Operation.equal id operation_id
      | _ -> false
    in
    match observation || handler with
    | false -> None
    | true ->
      Some
        (let open Result.Let_syntax in
         let%bind invocation =
           match observation with
           | true -> I.discard_observation_follow_up invocation ~reason
           | false -> Ok invocation
         in
         match handler with
         | true -> I.discard_handler_intent invocation ~reason
         | false -> Ok invocation))
  |> Result.all
;;

type entry =
  | Observation of I.t
  | Handler of I.t
  | Event of E.t

let entry_requests = function
  | Observation invocation -> requests invocation
  | Handler invocation ->
    (match invocation.handler_intent with
     | Some { follow_up = Pending_follow_up requests; _ } -> Some requests
     | Some { follow_up = Compaction_accepted_follow_up requests; _ } ->
       Some { requests with request_compaction = false }
     | _ -> None)
  | Event event ->
    (match event.requests, event.intent with
     | Some requests, Some Pending -> Some requests
     | Some requests, Some (Waiting_compaction _) ->
       Some { requests with request_compaction = false }
     | _ -> None)
;;

let entry_owner = function
  | Observation invocation | Handler invocation ->
    let c = invocation.context in
    c.session_id, c.generation, Option.map invocation.observation ~f:(fun o -> o.observer)
  | Event event ->
    let c = event.context in
    c.session_id, c.generation, Some c.source
;;

let entry_order = function
  | Observation invocation | Handler invocation ->
    invocation.context.created_at, P.Id.Invocation.to_string invocation.context.id
  | Event event ->
    event.context.created_at, P.Id.Moderator_execution.to_string event.context.id
;;

type update =
  | Apply
  | Discard of string
  | Accept_compaction of P.Id.Operation.t

let discard_entry entry ~reason = Ok (entry, Discard reason)
let apply_entry entry = Ok (entry, Apply)
let accept_compaction entry ~operation_id = Ok (entry, Accept_compaction operation_id)

let result action updates =
  let open Result.Let_syntax in
  let%map invocations, events =
    List.fold_result
      updates
      ~init:(String.Map.empty, [])
      ~f:(fun (saved, events) (entry, update) ->
        match entry with
        | Observation invocation | Handler invocation ->
          let key = P.Id.Invocation.to_string invocation.context.id in
          let current = Option.value (Map.find saved key) ~default:invocation in
          let%map next =
            match entry, update with
            | Observation _, Apply -> I.apply_observation_follow_up current
            | Observation _, Discard reason ->
              I.discard_observation_follow_up current ~reason
            | Observation _, Accept_compaction operation_id ->
              I.accept_observation_compaction current ~operation_id
            | Handler _, Apply -> I.apply_handler_intent current
            | Handler _, Discard reason -> I.discard_handler_intent current ~reason
            | Handler _, Accept_compaction operation_id ->
              I.accept_handler_compaction current ~operation_id
            | Event _, _ -> assert false
          in
          Map.set saved ~key ~data:next, events
        | Event event ->
          let%map next =
            match update with
            | Apply -> E.apply_intent event
            | Discard reason -> E.discard_intent event ~reason
            | Accept_compaction operation_id -> E.accept_compaction event ~operation_id
          in
          saved, next :: events)
  in
  { action; invocations = Map.data invocations; events = List.rev events }
;;

let current_entries ~(state : Session_state.t) ~observer =
  (List.filter state.invocations ~f:pending_observation
   |> List.map ~f:(fun i -> Observation i))
  @ (List.filter state.moderator_executions ~f:pending_event
     |> List.map ~f:(fun e -> Event e))
  |> List.filter ~f:(fun entry ->
    let session_id, generation, owner = entry_owner entry in
    generation = state.identity.generation
    && P.Id.Session.equal session_id state.identity.session_id
    && Option.exists owner ~f:(I.equal_observer observer))
;;

let turn_only entry =
  match entry with
  | Observation
      { observation = Some { follow_up = Some (Pending_follow_up requests); _ }; _ }
  | Event { requests = Some requests; intent = Some Pending; _ } ->
    requests.request_turn
    && (not requests.request_compaction)
    && Option.is_none requests.end_session
  | _ -> false
;;

let admit_turn ~state ~observer =
  current_entries ~state ~observer
  |> List.filter ~f:turn_only
  |> List.map ~f:apply_entry
  |> Result.all
  |> Result.bind ~f:(result Turn)
;;

let finish_foreground ~state ~observer ~failed =
  current_entries ~state ~observer
  |> List.filter ~f:(fun entry ->
    turn_only entry
    || (failed
        && Option.exists (entry_requests entry) ~f:(fun requests ->
          Option.is_none requests.end_session)))
  |> List.map ~f:(fun entry ->
    discard_entry entry ~reason:"foreground worker ended without admitting this request")
  |> Result.all
  |> Result.bind ~f:(result Checkpoint)
;;

let handler_readiness ~(state : Session_state.t) invocation =
  let rec owner seen (invocation : I.t) =
    let key = P.Id.Invocation.to_string invocation.context.id in
    match Set.mem seen key with
    | true -> `Invalid
    | false ->
      let seen = Set.add seen key in
      (match
         ( invocation.context.parent_job
         , invocation.context.parent_invocation
         , invocation.parent_event )
       with
       | Some id, _, _ -> `Job id
       | None, Some parent, _ ->
         (match
            List.find state.invocations ~f:(fun invocation ->
              P.Id.Invocation.equal invocation.context.id parent)
          with
          | None -> `Invalid
          | Some parent -> owner seen parent)
       | None, None, Some id ->
         (match
            List.find state.moderator_executions ~f:(fun event ->
              P.Id.Moderator_execution.equal event.context.id id)
          with
          | None -> `Invalid
          | Some event ->
            (match event.context.job with
             | None -> `Unbound
             | Some job -> `Job job.job_id))
       | None, None, None -> `Unbound)
  in
  match owner String.Set.empty invocation with
  | `Invalid -> `Discard "handler invocation has no retained owner"
  | `Unbound -> `Ready
  | `Job id ->
    (match List.find state.jobs ~f:(fun job -> P.Id.Job.equal job.id id) with
     | None -> `Discard "handler job is no longer retained"
     | Some job ->
       (match job.status with
        | Queued | Running | Waiting_permission _ -> `Wait
        | Succeeded | Failed _ -> `Ready
        | Cancelled | Interrupted _ -> `Discard "handler job was cancelled or interrupted"))
;;

let plan ~(state : Session_state.t) ~observer ~halted ~compaction_operation_id =
  let open Result.Let_syntax in
  let pending =
    (List.filter state.invocations ~f:pending_observation
     |> List.map ~f:(fun i -> Observation i))
    @ (List.filter state.invocations ~f:pending_handler
       |> List.map ~f:(fun i -> Handler i))
    @ (List.filter state.moderator_executions ~f:pending_event
       |> List.map ~f:(fun e -> Event e))
    |> List.filter ~f:(function
      | Handler invocation when invocation.context.generation = state.identity.generation
        ->
        (match handler_readiness ~state invocation with
         | `Wait -> false
         | `Ready | `Discard _ -> true)
      | _ -> true)
    |> List.sort ~compare:(fun a b ->
      let time_a, id_a = entry_order a
      and time_b, id_b = entry_order b in
      match P.Timestamp.compare time_a time_b with
      | 0 -> String.compare id_a id_b
      | order -> order)
  in
  let current, obsolete =
    List.partition_tf pending ~f:(fun entry ->
      let session_id, generation, owner = entry_owner entry in
      generation = state.identity.generation
      && P.Id.Session.equal session_id state.identity.session_id
      && (match entry with
          | Observation
              { observation =
                  Some
                    { follow_up = Some (Compaction_accepted_follow_up _)
                    ; compaction_operation_id = None
                    ; _
                    }
              ; _
              } -> false
          | _ -> true)
      &&
      match entry, owner, observer with
      | Handler invocation, _, _
        when match handler_readiness ~state invocation with
             | `Discard _ -> true
             | `Wait | `Ready -> false -> false
      | Handler _, None, _ -> true
      | _, Some owner, Some observer -> I.equal_observer owner observer
      | _ -> false)
  in
  let%bind obsolete =
    List.map obsolete ~f:(fun entry ->
      let reason =
        match entry with
        | Observation _ -> "observation owner is no longer installed"
        | Handler invocation ->
          (match handler_readiness ~state invocation with
           | `Discard reason -> reason
           | `Wait | `Ready -> "handler owner is no longer installed")
        | Event _ -> "moderator event owner is no longer installed"
      in
      discard_entry entry ~reason)
    |> Result.all
  in
  match
    List.find_map current ~f:(fun entry ->
      Option.bind (entry_requests entry) ~f:(fun requests -> requests.end_session))
  with
  | Some reason ->
    let%bind entries =
      List.map current ~f:(fun entry ->
        match
          Option.bind (entry_requests entry) ~f:(fun requests -> requests.end_session)
        with
        | Some _ -> apply_entry entry
        | None -> discard_entry entry ~reason:"moderator ended session")
      |> Result.all
    in
    result (Stop reason) (obsolete @ entries)
  | None when halted ->
    let%bind entries =
      List.map current ~f:(fun entry -> discard_entry entry ~reason:"moderator is halted")
      |> Result.all
    in
    result Checkpoint (obsolete @ entries)
  | None ->
    let compact =
      List.filter current ~f:(fun entry ->
        Option.exists (entry_requests entry) ~f:(fun requests ->
          requests.request_compaction))
    in
    (match compact with
     | _ :: _ ->
       let%bind entries =
         List.map compact ~f:(fun entry ->
           match entry_requests entry with
           | Some { request_turn = true; _ } ->
             accept_compaction entry ~operation_id:compaction_operation_id
           | _ -> apply_entry entry)
         |> Result.all
       in
       result Compact (obsolete @ entries)
     | [] ->
       let%bind entries = List.map current ~f:apply_entry |> Result.all in
       result
         (match current with
          | [] -> Checkpoint
          | _ -> Turn)
         (obsolete @ entries))
;;
