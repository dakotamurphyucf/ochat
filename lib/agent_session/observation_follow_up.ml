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

let pending (invocation : I.t) =
  match invocation.observation with
  | Some
      { status = Observed
      ; follow_up = Some (Pending_follow_up _ | Compaction_accepted_follow_up _)
      ; _
      } -> true
  | _ -> false
;;

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
  |> List.map ~f:(fun invocation -> I.discard_observation_follow_up invocation ~reason)
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
  match invocation.observation with
  | Some { follow_up = Some (Discarded_follow_up _); _ } ->
    Session_delta.Invocation_reconciled invocation
  | _ -> Session_delta.Invocation_changed invocation
;;

let discard_compaction invocations ~operation_id ~reason =
  List.filter invocations ~f:(fun invocation ->
    match invocation.I.observation with
    | Some
        { follow_up = Some (Compaction_accepted_follow_up _); compaction_operation_id; _ }
      ->
      Option.value_map
        compaction_operation_id
        ~default:true
        ~f:(P.Id.Operation.equal operation_id)
    | _ -> false)
  |> discard ~reason
;;

type entry =
  | Observation of I.t
  | Event of E.t

let entry_requests = function
  | Observation invocation -> requests invocation
  | Event event ->
    (match event.requests, event.intent with
     | Some requests, Some Pending -> Some requests
     | Some requests, Some (Waiting_compaction _) ->
       Some { requests with request_compaction = false }
     | _ -> None)
;;

let entry_owner = function
  | Observation invocation ->
    let c = invocation.context in
    c.session_id, c.generation, Option.map invocation.observation ~f:(fun o -> o.observer)
  | Event event ->
    let c = event.context in
    c.session_id, c.generation, Some c.source
;;

let entry_order = function
  | Observation invocation ->
    invocation.context.created_at, P.Id.Invocation.to_string invocation.context.id
  | Event event ->
    event.context.created_at, P.Id.Moderator_execution.to_string event.context.id
;;

let discard_entry entry ~reason =
  match entry with
  | Observation invocation ->
    I.discard_observation_follow_up invocation ~reason
    |> Result.map ~f:(fun i -> Observation i)
  | Event event -> E.discard_intent event ~reason |> Result.map ~f:(fun e -> Event e)
;;

let apply_entry = function
  | Observation invocation ->
    I.apply_observation_follow_up invocation |> Result.map ~f:(fun i -> Observation i)
  | Event event -> E.apply_intent event |> Result.map ~f:(fun e -> Event e)
;;

let accept_compaction entry ~operation_id =
  match entry with
  | Observation invocation ->
    I.accept_observation_compaction invocation ~operation_id
    |> Result.map ~f:(fun i -> Observation i)
  | Event event ->
    E.accept_compaction event ~operation_id |> Result.map ~f:(fun e -> Event e)
;;

let result action entries =
  { action
  ; invocations =
      List.filter_map entries ~f:(function
        | Observation i -> Some i
        | Event _ -> None)
  ; events =
      List.filter_map entries ~f:(function
        | Event e -> Some e
        | Observation _ -> None)
  }
;;

let plan ~(state : Session_state.t) ~observer ~halted ~compaction_operation_id =
  let open Result.Let_syntax in
  let pending =
    (List.filter state.invocations ~f:pending |> List.map ~f:(fun i -> Observation i))
    @ (List.filter state.moderator_executions ~f:pending_event
       |> List.map ~f:(fun e -> Event e))
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
      match owner, observer with
      | Some owner, Some observer -> I.equal_observer owner observer
      | _ -> false)
  in
  let%bind obsolete =
    List.map obsolete ~f:(fun entry ->
      let reason =
        match entry with
        | Observation _ -> "observation owner is no longer installed"
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
    let%map entries =
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
    let%map entries =
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
       let%map entries =
         List.map compact ~f:(fun entry ->
           match entry_requests entry with
           | Some { request_turn = true; _ } ->
             accept_compaction entry ~operation_id:compaction_operation_id
           | _ -> apply_entry entry)
         |> Result.all
       in
       result Compact (obsolete @ entries)
     | [] ->
       let%map entries = List.map current ~f:apply_entry |> Result.all in
       result
         (match current with
          | [] -> Checkpoint
          | _ -> Turn)
         (obsolete @ entries))
;;
