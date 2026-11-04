open! Core
module P = Agent_protocol

module Moderator_binding = struct
  type t =
    | No_captured_moderator
    | Captured of
        { source : P.Run_source.t
        ; manager : Chat_response.Moderator_manager.t option
        }
end

module Row = struct
  type phase =
    | Active of Execution_custody.Admission.t
    | Terminal of Execution_custody.Continuation.t

  type t =
    { job : P.Job.t
    ; binding : Moderator_binding.t
    ; phase : phase
    }

  let admission t =
    match t.phase with
    | Active admission -> Ok admission
    | Terminal _ ->
      Error (P.Error.invalid_request "terminal Job custody cannot admit active work")
  ;;

  let binding t = t.binding

  let retire t =
    match t.phase with
    | Active admission -> Execution_custody.Admission.retire admission
    | Terminal continuation -> Execution_custody.Continuation.retire continuation
  ;;

  let same_permission t other =
    match t.phase, other.phase with
    | Active left, Active right -> phys_equal left right
    | Terminal (Current left), Terminal (Current right) -> phys_equal left right
    | Terminal Expired, Terminal Expired -> true
    | Active _, Terminal _
    | Terminal _, Active _
    | Terminal (Current _), Terminal Expired
    | Terminal Expired, Terminal (Current _) -> false
  ;;
end

type t = Row.t list

let empty = []
let is_empty = List.is_empty
let invalid message = Error (P.Error.invalid_request message)
let retire t = List.iter t ~f:Row.retire

let same_job (left : P.Job.t) (right : P.Job.t) =
  (* Compare the exact persisted record. A query overlay, including progress,
     must not silently replace the actor's acknowledged Job. *)
  Jsonaf.exactly_equal (P.Job.to_json left) (P.Job.to_json right)
;;

let same_creation (left : P.Job.t) (right : P.Job.t) =
  P.Id.Job.equal left.id right.id
  && P.Id.Session.equal left.session_id right.session_id
  && Int.equal left.generation right.generation
  && P.Job.equal_kind left.kind right.kind
  && Jsonaf.exactly_equal left.payload right.payload
  && Jsonaf.exactly_equal
       (P.Job.retry_to_json left.retry_policy)
       (P.Job.retry_to_json right.retry_policy)
  && P.Timestamp.equal left.created_at right.created_at
  && Option.equal P.Job.equal_launch left.launch right.launch
  && Option.equal P.Inference_origin.equal left.inference_origin right.inference_origin
;;

let find t ~(job : P.Job.t) =
  match List.find t ~f:(fun (row : Row.t) -> P.Id.Job.equal row.job.id job.id) with
  | None -> Ok None
  | Some row ->
    if same_job row.job job
    then Ok (Some row)
    else invalid "Job custody differs from its admitted identity or attempt"
;;

let install_retained previous ~incoming =
  List.iter previous ~f:(fun (row : Row.t) ->
    if not (List.exists incoming ~f:(fun (next : Row.t) -> Row.same_permission row next))
    then Row.retire row);
  incoming
;;

let validate_binding (job : P.Job.t) binding =
  let open Result.Let_syntax in
  let captured_source =
    match binding with
    | Moderator_binding.No_captured_moderator -> None
    | Captured { source; manager = _ } -> Some source
  in
  let%bind () =
    match captured_source with
    | None -> Ok ()
    | Some source ->
      let%bind () = P.Run_source.validate source in
      if Int.equal source.generation job.generation
      then Ok ()
      else invalid "Job custody source generation differs from its admitted Job"
  in
  match Option.bind job.launch ~f:(fun launch -> launch.P.Job.moderator_source) with
  | None -> Ok ()
  | Some observer ->
    if
      Option.exists captured_source ~f:(fun source ->
        P.Invocation.equal_observer source.observer observer)
    then Ok ()
    else invalid "Job custody source differs from its captured launch observer"
;;

let validate_identity (job : P.Job.t) =
  let open Result.Let_syntax in
  let%bind _ = P.Id.Job.of_string (P.Id.Job.to_string job.id) in
  let%bind _ = P.Id.Session.of_string (P.Id.Session.to_string job.session_id) in
  if job.generation >= 0 && job.attempt >= 0
  then Ok ()
  else invalid "Job custody requires nonnegative generation and attempt"
;;

let validate_fresh (job : P.Job.t) ~binding =
  let open Result.Let_syntax in
  let%bind () = validate_identity job in
  let%bind () = P.Job.validate_inference_origin_transition ~previous:None job in
  let%bind () = P.Job.validate_result job in
  let%bind () = validate_binding job binding in
  let%bind () =
    match job.delivery with
    | Pending | Not_required -> Ok ()
    | Delivered _ | Discarded _ ->
      invalid "fresh Job custody cannot adopt a resolved delivery"
  in
  let%bind () =
    match job.status with
    | Queued
      when Int.equal job.attempt 0
           && Option.is_none job.started_at
           && Option.is_none job.completed_at
           && Option.is_none job.result -> Ok ()
    | Queued
    | Running
    | Waiting_permission _
    | Waiting_completion _
    | Succeeded
    | Failed _
    | Cancelled
    | Interrupted _ -> invalid "fresh Job custody requires a queued attempt zero"
  in
  Ok ()
;;

module Fresh = struct
  type phase =
    | Staged
    | Acknowledged
    | Discarded

  type t =
    { row : Row.t
    ; mutable phase : phase
    }

  let binding t = t.row.binding

  let stage ~job ~binding ~parent =
    let open Result.Let_syntax in
    let%bind () = validate_fresh job ~binding in
    let%map admission = Execution_custody.Admission.transfer parent in
    { row = { Row.job; binding; phase = Active admission }; phase = Staged }
  ;;

  let discard t =
    match t.phase with
    | Staged ->
      t.phase <- Discarded;
      Row.retire t.row
    | Acknowledged | Discarded -> ()
  ;;

  let selected_row t ~job =
    let open Result.Let_syntax in
    match t.phase with
    | Acknowledged | Discarded -> invalid "staged Job custody is no longer selectable"
    | Staged ->
      if same_job t.row.job job
      then (
        let%bind admission = Row.admission t.row in
        let%map () = Execution_custody.Admission.check admission in
        t.row)
      else invalid "selected Job differs from its staged custody"
  ;;
end

module Update = struct
  type inventory = t

  type t =
    { basis : inventory
    ; candidate : inventory
    ; staged : Fresh.t list
    ; completed : Execution_custody.Continuation.t list
    ; mutable acknowledged : bool
    }

  let check_basis t ~current =
    if phys_equal t.basis current
    then Ok ()
    else invalid "Job custody inventory changed before commit"
  ;;

  let check_before_commit t ~current =
    let open Result.Let_syntax in
    let%bind () = check_basis t ~current in
    List.fold_result t.staged ~init:() ~f:(fun () fresh ->
      Fresh.selected_row fresh ~job:fresh.row.job |> Result.map ~f:ignore)
  ;;

  let discard t =
    if not t.acknowledged
    then (
      List.iter t.staged ~f:Fresh.discard;
      List.iter t.completed ~f:Execution_custody.Continuation.retire)
  ;;

  let install t =
    let incoming = install_retained t.basis ~incoming:t.candidate in
    List.iter t.staged ~f:(fun (fresh : Fresh.t) -> fresh.phase <- Acknowledged);
    t.acknowledged <- true;
    incoming
  ;;
end

let prepare_retirement t =
  { Update.basis = t; candidate = []; staged = []; completed = []; acknowledged = false }
;;

let same_binding (left : Moderator_binding.t) (right : Moderator_binding.t) =
  match left, right with
  | Moderator_binding.No_captured_moderator, No_captured_moderator -> true
  | Captured left, Captured right ->
    P.Run_source.equal left.source right.source
    && Option.equal phys_equal left.manager right.manager
  | No_captured_moderator, Captured _ | Captured _, No_captured_moderator -> false
;;

let validate_constructor_handoff
      (previous : Moderator_binding.t)
      (next : Moderator_binding.t)
  =
  match previous, next with
  | Moderator_binding.No_captured_moderator, (No_captured_moderator | Captured _) -> Ok ()
  | Captured _, No_captured_moderator ->
    invalid "constructor handoff cannot remove captured Job source"
  | Captured previous, Captured next ->
    if
      P.Run_source.equal previous.source next.source
      &&
      match previous.manager, next.manager with
      | None, (None | Some _) -> true
      | Some previous, Some next -> phys_equal previous next
      | Some _, None -> false
    then Ok ()
    else invalid "constructor handoff cannot replace another Job source or Manager"
;;

let prepare_constructor_handoff t ~jobs ~binding =
  let open Result.Let_syntax in
  let seen = Hash_set.create (module P.Id.Job) in
  let%map candidate =
    List.fold_result jobs ~init:t ~f:(fun candidate job ->
      let%bind () = validate_identity job in
      let%bind () = validate_binding job binding in
      let%bind () =
        if Hash_set.mem seen job.P.Job.id
        then invalid "constructor Job handoff occurs more than once"
        else (
          Hash_set.add seen job.id;
          Ok ())
      in
      let%bind original = find t ~job in
      match original with
      | None -> Ok candidate
      | Some original ->
        let%map () = validate_constructor_handoff original.binding binding in
        if same_binding original.binding binding
        then candidate
        else
          List.map candidate ~f:(fun row ->
            if phys_equal row original then { row with binding } else row))
  in
  if phys_equal candidate t
  then None
  else
    Some
      { Update.basis = t; candidate; staged = []; completed = []; acknowledged = false }
;;

let retain_runtime t ~manager =
  List.filter t ~f:(fun (row : Row.t) ->
    match row.binding with
    | No_captured_moderator | Captured { source = _; manager = None } -> true
    | Captured { source = _; manager = Some previous } ->
      Option.exists manager ~f:(phys_equal previous))
;;

let prepare_selected t ~selected =
  let open Result.Let_syntax in
  let seen = Hash_set.create (module P.Id.Job) in
  List.iter t ~f:(fun (row : Row.t) -> Hash_set.add seen row.job.id);
  let%map rows =
    List.map selected ~f:(fun (job, fresh) ->
      let%bind row = Fresh.selected_row fresh ~job in
      if Hash_set.mem seen row.job.id
      then invalid "duplicate Job custody admission"
      else (
        Hash_set.add seen row.job.id;
        Ok row))
    |> Result.all
  in
  { Update.basis = t
  ; candidate = t @ rows
  ; staged = List.map selected ~f:snd
  ; completed = []
  ; acknowledged = false
  }
;;

let prepare_fresh t ~job ~binding ~parent =
  let open Result.Let_syntax in
  let%bind fresh = Fresh.stage ~job ~binding ~parent in
  let accepted = ref false in
  Exn.protect
    ~finally:(fun () -> if not !accepted then Fresh.discard fresh)
    ~f:(fun () ->
      let%map update = prepare_selected t ~selected:[ job, fresh ] in
      accepted := true;
      update)
;;

let prepare_phase t ~(previous : P.Job.t) ~(next : P.Job.t) ~validate =
  let open Result.Let_syntax in
  let%bind () = validate_identity previous in
  let%bind () = validate_identity next in
  let%bind () =
    if same_creation previous next && P.Job.equal_delivery previous.delivery next.delivery
    then Ok ()
    else invalid "Job custody phase update changes its immutable creation"
  in
  let%bind () =
    P.Job.validate_inference_origin_transition ~previous:(Some previous) next
  in
  let%bind () = P.Job.validate_result next in
  let%bind () = validate () in
  let%bind original = find t ~job:previous in
  let%map () =
    match original with
    | None -> Ok ()
    | Some row -> Row.admission row |> Result.map ~f:ignore
  in
  let candidate =
    match original with
    | None -> t
    | Some original ->
      List.map t ~f:(fun row ->
        if phys_equal row original then { row with job = next } else row)
  in
  { Update.basis = t; candidate; staged = []; completed = []; acknowledged = false }
;;

let prepare_claim t ~(previous : P.Job.t) ~(claimed : P.Job.t) =
  prepare_phase t ~previous ~next:claimed ~validate:(fun () ->
    match previous.status, claimed.status with
    | Queued, Running
      when previous.attempt < Int.max_value
           && Int.equal claimed.attempt (previous.attempt + 1)
           && Option.is_some claimed.started_at
           && Option.is_none claimed.next_run_at
           && Option.is_none claimed.completed_at
           && Option.is_none claimed.result -> Ok ()
    | ( ( Queued
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ )
      , ( Queued
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ ) ) ->
      invalid "Job custody claim must acknowledge exactly the next running attempt")
;;

let prepare_retry t ~(previous : P.Job.t) ~(queued : P.Job.t) =
  prepare_phase t ~previous ~next:queued ~validate:(fun () ->
    let retry_allowed =
      match previous.retry_policy with
      | Never -> false
      | Safe_retry { max_attempts; _ } | Idempotent { max_attempts; _ } ->
        previous.attempt < max_attempts
    in
    match previous.status, queued.status with
    | Running, Queued
      when retry_allowed
           && Int.equal previous.attempt queued.attempt
           && Option.is_none queued.started_at
           && Option.is_some queued.next_run_at
           && Option.is_none queued.completed_at -> Ok ()
    | ( ( Queued
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ )
      , ( Queued
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ ) ) ->
      invalid "Job custody retry must retain the acknowledged attempt")
;;

let prepare_defer t ~(previous : P.Job.t) ~(waiting : P.Job.t) =
  prepare_phase t ~previous ~next:waiting ~validate:(fun () ->
    match previous.status, waiting.status with
    | Running, Waiting_completion _
      when Int.equal previous.attempt waiting.attempt
           && Option.equal P.Timestamp.equal previous.started_at waiting.started_at
           && Option.equal P.Timestamp.equal previous.next_run_at waiting.next_run_at
           && Option.is_none waiting.completed_at
           && Option.is_none waiting.result -> Ok ()
    | ( ( Queued
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ )
      , ( Queued
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ ) ) ->
      invalid "Job custody defer must retain its exact running attempt")
;;

let is_terminal (job : P.Job.t) =
  match job.status with
  | Succeeded | Failed _ | Cancelled | Interrupted _ -> true
  | Queued | Running | Waiting_permission _ | Waiting_completion _ -> false
;;

let validate_terminal ~(previous : P.Job.t) ~(terminal : P.Job.t) =
  let open Result.Let_syntax in
  let%bind () = validate_identity previous in
  let%bind () = validate_identity terminal in
  let%bind () =
    if same_creation previous terminal && Int.equal previous.attempt terminal.attempt
    then Ok ()
    else invalid "terminal Job custody changes its admitted creation or attempt"
  in
  let%bind () =
    P.Job.validate_inference_origin_transition ~previous:(Some previous) terminal
  in
  let%bind () = P.Job.validate_result terminal in
  if is_terminal terminal && Option.is_some terminal.completed_at
  then Ok ()
  else invalid "terminal Job custody requires a completed terminal record"
;;

let transfer_row (row : Row.t) =
  match row.phase with
  | Active admission | Terminal (Current admission) ->
    Execution_custody.Continuation.transfer_completed admission
  | Terminal Expired -> Ok Execution_custody.Continuation.Expired
;;

let prepare_terminals t ~completed =
  let open Result.Let_syntax in
  let staged = ref [] in
  let accepted = ref false in
  Exn.protect
    ~finally:(fun () ->
      if not !accepted then List.iter !staged ~f:Execution_custody.Continuation.retire)
    ~f:(fun () ->
      let seen = Hash_set.create (module P.Id.Job) in
      let%map candidate =
        List.fold_result completed ~init:t ~f:(fun candidate (previous, terminal) ->
          let%bind () = validate_terminal ~previous ~terminal in
          let%bind () =
            if Hash_set.mem seen previous.id
            then invalid "duplicate terminal Job custody disposition"
            else (
              Hash_set.add seen previous.id;
              Ok ())
          in
          let%bind original = find t ~job:previous in
          match original with
          | None -> Ok candidate
          | Some original ->
            let%map replacement =
              match terminal.delivery with
              | Pending ->
                let%map continuation = transfer_row original in
                staged := continuation :: !staged;
                Some { original with job = terminal; phase = Terminal continuation }
              | Not_required | Delivered _ | Discarded _ -> Ok None
            in
            List.filter_map candidate ~f:(fun row ->
              if phys_equal row original then replacement else Some row))
      in
      accepted := true;
      { Update.basis = t
      ; candidate
      ; staged = []
      ; completed = !staged
      ; acknowledged = false
      })
;;

let transfer_delivery t ~manager ~source ~job =
  let open Result.Let_syntax in
  let%bind row = find t ~job in
  match row with
  | None -> Ok None
  | Some { phase = Active _; _ } -> invalid "Job delivery requires terminal custody"
  | Some ({ phase = Terminal _; _ } as row) ->
    (match row.binding with
     | Captured { source = captured; manager = Some owner }
       when P.Run_source.equal captured source && phys_equal owner manager ->
       Result.map (transfer_row row) ~f:Option.some
     | No_captured_moderator | Captured _ ->
       invalid "Job delivery acquired a different source or Manager")
;;

let retain t ~jobs =
  List.filter t ~f:(fun (row : Row.t) ->
    List.exists jobs ~f:(fun (job : P.Job.t) ->
      same_job row.job job
      &&
      match row.phase with
      | Active _ ->
        (match job.status with
         | Queued | Running | Waiting_permission _ | Waiting_completion _ -> true
         | Succeeded | Failed _ | Cancelled | Interrupted _ -> false)
      | Terminal _ ->
        is_terminal job
        &&
          (match job.delivery with
          | Pending -> true
          | Not_required | Delivered _ | Discarded _ -> false)))
;;
