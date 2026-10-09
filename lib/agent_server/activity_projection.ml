open! Core
module P = Agent_protocol
module A = P.Session_activity
module W = P.Session_work

type t =
  { work : Work_projection.t
  ; server_id : P.Id.Server.t
  ; now : P.Timestamp.t
  }

let create ~server_id ~principal ~now =
  { work = Work_projection.create ~server_id ~principal; server_id; now }
;;

let observe t (snapshot : P.Snapshot.t) ~(catalog : P.Session_catalog.t) ~transient ~usage
  =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Id.Session.equal snapshot.session.id catalog.session.id
      && Int.equal snapshot.session.generation catalog.session.generation
      && Int64.equal snapshot.revision catalog.session.revision
      && Int64.equal snapshot.session.revision snapshot.revision
    then Ok ()
    else
      Error
        (P.Error.create
           Conflict
           ~message:"activity changed during observation; refresh required"
           ~data:(`Object [ "refresh_required", `True ])
           ~retryable:false
           ())
  in
  let%bind work = Work_projection.work t.work snapshot in
  let attention entity reason ?(expired = false) () =
    A.Attention.create ~entity ~reason ~unresolved:true ~expired
  in
  let%bind permissions =
    List.filter_map snapshot.permissions ~f:(fun (permission : P.Permission.t) ->
      if
        Int.equal permission.generation snapshot.session.generation
        && P.Permission.equal_state permission.state Pending
      then Some permission
      else None)
    |> List.map ~f:(fun (permission : P.Permission.t) ->
      if not (P.Id.Session.equal permission.session_id snapshot.session.id)
      then
        Error (P.Error.invalid_request "activity permission belongs to another session")
      else (
        let expired =
          Option.value_map permission.expires_at ~default:false ~f:(fun deadline ->
            P.Timestamp.compare deadline t.now <= 0)
        in
        attention (Permission permission.id) Approval ~expired ()))
    |> Result.all
  in
  let%bind work_attention =
    List.concat_map work ~f:(fun (row : W.t) ->
      let failure =
        match row.status with
        | Failed | Interrupted -> [ attention (Work row.key) Failure () ]
        | Accepted
        | Running
        | Waiting_approval
        | Waiting_work
        | Succeeded
        | Cancelled
        | Unsupported -> []
      in
      let completion =
        match row.status, row.delivery with
        | (Succeeded | Failed | Cancelled | Interrupted), Pending ->
          [ attention (Work row.key) Completion_pending () ]
        | (Accepted | Running | Waiting_approval | Waiting_work | Unsupported), _
        | ( (Succeeded | Failed | Cancelled | Interrupted)
          , (Not_applicable | Acknowledged | Discarded) ) -> []
      in
      failure @ completion)
    |> Result.all
  in
  let%bind session_attention =
    let input = if snapshot.halted then [ attention Session Input_required () ] else [] in
    let failure =
      if
        Option.is_some snapshot.failure
        ||
        match snapshot.session.observed_state with
        | Failed _ -> true
        | Stopped
        | Queued_for_slot
        | Starting
        | Recovering
        | Idle
        | Running_turn _
        | Compacting _
        | Waiting_for_permission _
        | Stopping -> false
      then [ attention Session Failure () ]
      else []
    in
    Result.all (input @ failure)
  in
  let%bind operation_attention =
    match snapshot.session.active_operation with
    | Some operation ->
      (match operation.state with
       | Failed _ | Interrupted _ ->
         Result.map (attention (Operation operation.id) Failure ()) ~f:(fun row ->
           [ row ])
       | Starting | Running | Cancelling | Completed | Cancelled -> Ok [])
    | None -> Ok []
  in
  let%bind summary =
    P.Session_activity_summary.of_catalog catalog ~server_id:t.server_id
  in
  A.create
    ~summary
    ~attention:(permissions @ work_attention @ session_attention @ operation_attention)
    ~work_count:(List.length work)
    ~transient
    ~usage
;;
