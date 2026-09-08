open Core
module P = Agent_protocol
module I = P.Invocation

type action =
  | Checkpoint
  | Stop of string
  | Compact
  | Turn
[@@deriving sexp_of]

type t =
  { action : action
  ; invocations : I.t list
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

let plan ~(state : Session_state.t) ~observer ~halted ~compaction_operation_id =
  let open Result.Let_syntax in
  let pending =
    List.filter state.invocations ~f:pending
    |> List.sort ~compare:(fun a b ->
      match P.Timestamp.compare a.context.created_at b.context.created_at with
      | 0 -> P.Id.Invocation.compare a.context.id b.context.id
      | order -> order)
  in
  let current, obsolete =
    List.partition_tf pending ~f:(fun invocation ->
      invocation.context.generation = state.identity.generation
      && P.Id.Session.equal invocation.context.session_id state.identity.session_id
      &&
      match invocation.observation, observer with
      | ( Some
            { follow_up = Some (Compaction_accepted_follow_up _)
            ; compaction_operation_id = None
            ; _
            }
        , _ ) -> false
      | Some observation, Some observer -> I.equal_observer observation.observer observer
      | _ -> false)
  in
  let%bind obsolete =
    discard obsolete ~reason:"observation owner is no longer installed"
  in
  match
    List.find_map current ~f:(fun invocation ->
      Option.bind (requests invocation) ~f:(fun requests -> requests.end_session))
  with
  | Some reason ->
    let%map invocations =
      List.map current ~f:(fun invocation ->
        match
          Option.bind (requests invocation) ~f:(fun requests -> requests.end_session)
        with
        | Some _ -> I.apply_observation_follow_up invocation
        | None ->
          I.discard_observation_follow_up invocation ~reason:"moderator ended session")
      |> Result.all
    in
    { action = Stop reason; invocations = obsolete @ invocations }
  | None when halted ->
    let%map invocations = discard current ~reason:"moderator is halted" in
    { action = Checkpoint; invocations = obsolete @ invocations }
  | None ->
    let compact =
      List.filter current ~f:(fun invocation ->
        Option.exists (requests invocation) ~f:(fun requests ->
          requests.request_compaction))
    in
    (match compact with
     | _ :: _ ->
       let%map invocations =
         List.map compact ~f:(fun invocation ->
           match requests invocation with
           | Some { request_turn = true; _ } ->
             I.accept_observation_compaction
               invocation
               ~operation_id:compaction_operation_id
           | _ -> I.apply_observation_follow_up invocation)
         |> Result.all
       in
       { action = Compact; invocations = obsolete @ invocations }
     | [] ->
       let%map invocations =
         List.map current ~f:I.apply_observation_follow_up |> Result.all
       in
       { action = (if List.is_empty current then Checkpoint else Turn)
       ; invocations = obsolete @ invocations
       })
;;
