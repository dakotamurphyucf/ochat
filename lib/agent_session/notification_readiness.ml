open Core
module P = Agent_protocol

type blockage =
  | Waiting of string
  | Rejected of string
[@@deriving equal, sexp]

let waiting message = Error (Waiting message)
let rejected message = Error (Rejected message)

let check ~invocations ~jobs ~events (delivery : P.Delivery.t) =
  let open Result.Let_syntax in
  let find_invocation id =
    List.find invocations ~f:(fun value ->
      P.Id.Invocation.equal value.P.Invocation.context.id id)
    |> Result.of_option ~error:(Rejected "notification invocation is not retained")
  in
  let same_session session_id generation =
    match
      P.Id.Session.equal session_id delivery.context.session_id
      && Int.equal generation delivery.context.generation
    with
    | true -> Ok ()
    | false -> rejected "notification ancestry crosses session or generation"
  in
  let matching_work outcome =
    match delivery.context.work, outcome with
    | None, _ -> Ok ()
    | Some expected, P.Invocation.Pending (actual, _)
      when P.Invocation.equal_work expected actual -> Ok ()
    | _ -> rejected "delivery work differs from the originating acknowledgement"
  in
  let enter seen key =
    match Set.mem seen key with
    | true -> rejected "notification acknowledgement ancestry contains a cycle"
    | false -> Ok (Set.add seen key)
  in
  let rec invocation seen id =
    let%bind seen = enter seen ("invocation:" ^ P.Id.Invocation.to_string id) in
    let%bind value = find_invocation id in
    let c = value.context in
    let%bind () = same_session c.session_id c.generation in
    let%bind () =
      match value.publication_discarded with
      | Some _ -> rejected "notification ancestor publication was discarded"
      | None -> Ok ()
    in
    let%bind () =
      match c.origin, value.status with
      | _, (P.Invocation.Admitted | Dispatching) ->
        waiting "notification ancestor has no recorded outcome"
      | Model, Resolved _ -> waiting "initial acknowledgement is not published"
      | Model, Published _ | _, (Resolved _ | Published _) -> Ok ()
    in
    match c.origin, c.parent_invocation, c.parent_job, value.parent_event with
    | Model, None, None, None -> Ok ()
    | Model, _, _, _ -> rejected "model acknowledgement has conflicting ancestry"
    | _, Some id, None, None -> invocation seen id
    | _, None, Some id, None -> job seen id
    | _, None, None, Some id -> event seen id
    | _, None, None, None -> Ok ()
    | _ -> rejected "notification invocation has conflicting parents"
  and job seen id =
    let%bind seen = enter seen ("job:" ^ P.Id.Job.to_string id) in
    let%bind value =
      List.find jobs ~f:(fun value -> P.Id.Job.equal value.P.Job.id id)
      |> Result.of_option ~error:(Rejected "notification ancestor job is not retained")
    in
    let%bind () = same_session value.session_id value.generation in
    match value.launch with
    | None -> Ok ()
    | Some launch -> owner seen launch.owner
  and event seen id =
    let%bind seen = enter seen ("event:" ^ P.Id.Moderator_execution.to_string id) in
    let%bind value =
      List.find events ~f:(fun value ->
        P.Id.Moderator_execution.equal value.P.Moderator_execution.context.id id)
      |> Result.of_option ~error:(Rejected "notification ancestor event is not retained")
    in
    let%bind () = same_session value.context.session_id value.context.generation in
    let%bind () =
      match value.status with
      | P.Moderator_execution.Running -> waiting "notification creator has not committed"
      | Completed _ -> Ok ()
      | Failed _ | Interrupted _ ->
        rejected "notification ancestor event did not complete"
    in
    match value.context.job with
    | None -> Ok ()
    | Some parent -> job seen parent.job_id
  and owner seen = function
    | P.Job.Invocation id -> invocation seen id
    | Moderator_event id -> event seen id
  in
  match delivery.context.ownership with
  | None ->
    (match delivery.context.invocation_id with
     | None -> Ok ()
     | Some id ->
       let%bind value = find_invocation id in
       let%bind () =
         match delivery.completion_projection with
         | None -> Ok ()
         | Some _ -> invocation String.Set.empty id
       in
       (match value.status with
        | P.Invocation.Published outcome -> matching_work outcome
        | Admitted | Dispatching | Resolved _ ->
          waiting "initial acknowledgement is not published"))
  | Some ownership ->
    let%bind () =
      match delivery.context.invocation_id with
      | None -> Ok ()
      | Some id ->
        let%bind value = find_invocation id in
        let%bind () =
          match value.status with
          | P.Invocation.Admitted | Dispatching ->
            waiting "notification origin has no recorded outcome"
          | Resolved outcome | Published outcome -> matching_work outcome
        in
        invocation String.Set.empty id
    in
    let%bind () = owner String.Set.empty ownership.creator in
    (match delivery.context.work with
     | Some (P.Invocation.Job id) -> job String.Set.empty id
     | None | Some (Subscription _) -> Ok ())
;;
