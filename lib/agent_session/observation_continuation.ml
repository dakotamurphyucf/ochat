open! Core
module P = Agent_protocol
module I = P.Invocation
module Policy_source = Policy_standalone_source.Classification

module Parent = struct
  type t =
    | Available of Execution_custody.Admission.t
    | Expired
    | Unavailable
end

type t =
  { invocation : I.t
  ; source : P.Run_source.t option
  ; continuation : Execution_custody.Continuation.t option
  ; policy_source : Policy_source.t
  }

let policy_source t = t.policy_source
let failure message = Error (P.Error.create Conflict ~message ~retryable:false ())

let parent t =
  match t.continuation with
  | None -> Parent.Unavailable
  | Some Expired -> Parent.Expired
  | Some (Current admission) -> Parent.Available admission
;;

let stage_parent = function
  | Parent.Unavailable -> Ok None
  | Expired -> Ok (Some Execution_custody.Continuation.Expired)
  | Available admission ->
    Execution_custody.Continuation.transfer_completed admission
    |> Result.map ~f:Option.some
;;

let validate_policy_source (invocation : I.t) ~source = function
  | Policy_source.Unknown -> Ok ()
  | Standalone_unrelated descriptor ->
    let open Result.Let_syntax in
    let%bind member =
      Policy_cause_data.Member.invocation
        invocation.context.id
        ~generation:invocation.context.generation
    in
    let%bind () =
      match descriptor.Policy_standalone_source.subject with
      | Member actual when Policy_cause_data.Member.equal actual member -> Ok ()
      | Member _ | Root_operation _ -> failure "observation standalone subject differs"
    in
    if
      P.Id.Session.equal descriptor.session_id invocation.context.session_id
      && Int.equal descriptor.generation invocation.context.generation
      && Option.exists
           invocation.inference_origin
           ~f:(P.Inference_origin.equal descriptor.origin)
      &&
      match descriptor.source.installation, source with
      | Some captured, Some current ->
        P.Run_source.equal captured current
        && Option.exists
             descriptor.source.observer
             ~f:(P.Invocation.equal_observer current.observer)
      | None, _ | Some _, None -> false
    then Ok ()
    else failure "observation standalone origin or full source differs"
;;

let stage_completed invocation ~source ~parent ~policy_source =
  let open Result.Let_syntax in
  let%bind () = I.validate invocation in
  match invocation.I.status, invocation.observation with
  | (Resolved _ | Published _), Some { status = Awaiting; _ } ->
    let%bind () = validate_policy_source invocation ~source policy_source in
    let%map continuation = stage_parent parent in
    Some { invocation; source; continuation; policy_source }
  | ( (Admitted | Dispatching | Resolved _ | Published _)
    , (None | Some { status = Observing | Observed | Observation_failed _; _ }) ) ->
    Ok None
  | (Admitted | Dispatching), Some { status = Awaiting; _ } ->
    failure "observation continuation requires its completed native result"
;;

let same_result t invocation =
  I.equal t.invocation invocation
  || Result.is_ok (I.validate_transition ~previous:(Some t.invocation) invocation)
;;

let matches t ~source invocation =
  Option.equal P.Run_source.equal t.source source
  && same_result t invocation
  && Result.is_ok (validate_policy_source invocation ~source t.policy_source)
;;

let stage_claim t ~source ~previous ~observing =
  let open Result.Let_syntax in
  let%bind expected = I.claim_observation previous in
  let%bind () =
    if matches t ~source previous && I.equal expected observing
    then Ok ()
    else failure "observation continuation does not own this exact claim"
  in
  let%map continuation = stage_parent (parent t) in
  { invocation = observing
  ; source = t.source
  ; continuation
  ; policy_source = t.policy_source
  }
;;

let stage_follow_up t ~resolved =
  let open Result.Let_syntax in
  let%bind () =
    if
      same_result t resolved
      && Result.is_ok (validate_policy_source resolved ~source:t.source t.policy_source)
    then (
      match resolved.I.observation with
      | Some { status = Observed; _ } -> Ok ()
      | None | Some { status = Awaiting | Observing | Observation_failed _; _ } ->
        failure "observation follow-up requires its completed observation")
    else failure "observation follow-up changed its original result"
  in
  let%map continuation = stage_parent (parent t) in
  { invocation = resolved
  ; source = t.source
  ; continuation
  ; policy_source = t.policy_source
  }
;;

let admission t =
  match t.continuation with
  | Some (Current admission) ->
    Execution_custody.Admission.check admission |> Result.map ~f:(fun () -> admission)
  | Some Expired | None ->
    Error
      (P.Error.create
         Permission_denied
         ~message:"original observation admission is unavailable or expired"
         ~retryable:false
         ())
;;

let retire t = Option.iter t.continuation ~f:Execution_custody.Continuation.retire

module Selection = struct
  type observation = t

  type t =
    { observations : observation list
    ; custody : Execution_custody.t option
    ; others : Execution_custody.t list
    }

  let custody t = t.custody

  let bindings t =
    List.map t.observations ~f:(fun value -> value.invocation.context.id, value)
  ;;

  let compatible left right = Option.equal Execution_custody.same_origin left right

  let denied () =
    Error
      (P.Error.create
         Permission_denied
         ~message:"observation follow-up has missing or mixed original admissions"
         ~retryable:false
         ())
  ;;

  let create ~observations ~other_custodies =
    let open Result.Let_syntax in
    let%bind admissions = List.map observations ~f:admission |> Result.all in
    match admissions with
    | [] -> denied ()
    | first :: rest ->
      let custody = Execution_custody.Admission.custody first in
      let%bind () =
        if
          List.for_all rest ~f:(fun value ->
            compatible custody (Execution_custody.Admission.custody value))
        then Ok ()
        else denied ()
      in
      let%map others =
        List.map other_custodies ~f:(function
          | Some other when compatible custody (Some other) -> Ok other
          | None | Some _ -> denied ())
        |> Result.all
      in
      { observations; custody; others }
  ;;

  let check_current t =
    let open Result.Let_syntax in
    let%bind () =
      List.fold_result t.observations ~init:() ~f:(fun () observation ->
        admission observation |> Result.map ~f:ignore)
    in
    List.fold_result t.others ~init:() ~f:(fun () custody ->
      Execution_custody.actor custody |> Result.map ~f:ignore)
  ;;
end
