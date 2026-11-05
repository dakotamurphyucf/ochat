open! Core
module P = Agent_protocol

let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

module Value = struct
  type t =
    | Edit of Goal_command.t
    | Resume of P.Goal_native_resume_intent.t
end

module Entry = struct
  type phase =
    | Native_pending
    | Native_completed
    | Recorded
    | Selected
    | Retained
    | Closed

  type t =
    { invocation_id : P.Id.Invocation.t
    ; value : Value.t
    ; mutable admission : Execution_custody.Admission.t option
    ; authorize : Operator_authorization.t -> Session_state.t -> (unit, P.Error.t) result
    ; mutable phase : phase
    }

  let create ~invocation_id ~value ~admission ~authorize =
    { invocation_id
    ; value
    ; admission = Some admission
    ; authorize
    ; phase = Native_pending
    }
  ;;

  let invocation_id t = t.invocation_id
  let value t = t.value

  let actor t =
    let open Result.Let_syntax in
    let%bind admission =
      Result.of_option
        t.admission
        ~error:
          (P.Error.invalid_request "native Goal action no longer owns its command loan")
    in
    let%bind () = Execution_custody.Admission.check admission in
    let%bind custody =
      Result.of_option
        (Execution_custody.Admission.custody admission)
        ~error:
          (P.Error.create
             Permission_denied
             ~message:"native Goal action requires authenticated original custody"
             ~retryable:false
             ())
    in
    Execution_custody.actor custody
  ;;

  let authorize t actor state = t.authorize actor state

  let check t state =
    let open Result.Let_syntax in
    let%bind actor = actor t in
    Goal_command_admission.check_write actor ~authorize:(t.authorize actor) state
  ;;

  let completed t =
    match t.phase with
    | Native_pending -> t.phase <- Native_completed
    | Native_completed | Recorded | Selected | Retained | Closed -> ()
  ;;

  let discard t =
    Option.iter t.admission ~f:Execution_custody.Admission.retire;
    t.admission <- None;
    t.phase <- Closed
  ;;

  let retain t =
    match t.phase with
    | Selected -> t.phase <- Retained
    | Native_pending | Native_completed | Recorded | Retained | Closed -> ()
  ;;

  let discard_speculative t =
    match t.phase with
    | Retained -> ()
    | Native_pending | Native_completed | Recorded | Selected | Closed -> discard t
  ;;

  let select_completed t =
    match t.phase with
    | Native_completed ->
      t.phase <- Selected;
      Ok ()
    | Native_pending | Recorded | Selected | Retained | Closed ->
      conflict "native Goal result cannot be selected twice"
  ;;

  let take_admission t =
    match t.phase, t.admission with
    | (Selected | Retained), Some admission ->
      t.admission <- None;
      Ok admission
    | _ -> conflict "native Goal action has no selected loan to transfer"
  ;;
end

type phase =
  | Open
  | Prepared of int list
  | Closed

type t =
  { mutable phase : phase
  ; mutable next : int
  ; mutable entries : Entry.t Int.Map.t
  ; mutable scope : Chat_response.Goal_native_scope.t option
  }

let create () = { phase = Open; next = 0; entries = Int.Map.empty; scope = None }

let install_scope t scope =
  match t.phase, t.scope with
  | Open, None ->
    t.scope <- Some scope;
    Ok ()
  | _ -> conflict "Goal callback routing was already installed or closed"
;;

let matches_scope t incoming =
  match t.scope, incoming with
  | Some expected, Some incoming ->
    Chat_response.Goal_native_scope.same expected incoming
    && Chat_response.Goal_native_scope.is_open expected
  | _ -> false
;;

let close t =
  Map.iter t.entries ~f:Entry.discard_speculative;
  t.entries <- Int.Map.empty;
  t.phase <- Closed;
  Option.iter t.scope ~f:Chat_response.Goal_native_scope.close
;;

let stage t entry =
  match t.phase with
  | Prepared _ | Closed ->
    Entry.discard entry;
    conflict "Goal callback transaction is sealed"
  | Open ->
    if t.next = Int.max_value || Map.length t.entries >= 256
    then (
      Entry.discard entry;
      conflict "Goal callback provisional receipt capacity exhausted")
    else (
      let ticket = t.next in
      t.next <- t.next + 1;
      t.entries <- Map.set t.entries ~key:ticket ~data:entry;
      Ok ticket)
;;

let deliverable t ticket =
  match t.phase, Map.find t.entries ticket with
  | Open, Some { Entry.phase = Native_completed | Recorded; _ } -> Ok true
  | Open, Some { Entry.phase = Native_pending | Selected | Retained | Closed; _ }
  | Open, None -> Ok false
  | Prepared _, _ | Closed, _ -> conflict "Goal callback transaction is sealed"
;;

let accept t ticket =
  match t.phase, Map.find t.entries ticket with
  | Open, Some entry ->
    (match entry.Entry.phase with
     | Native_completed ->
       entry.phase <- Recorded;
       Ok ()
     | Native_pending | Recorded | Selected | Retained | Closed ->
       conflict "Goal receipt did not come from its completed native result")
  | _ -> conflict "Goal receipt is not issued by this callback"
;;

let rollback t ticket =
  match Map.find t.entries ticket with
  | None -> ()
  | Some entry ->
    Entry.discard entry;
    t.entries <- Map.remove t.entries ticket;
    (match t.phase with
     | Prepared _ -> close t
     | Open | Closed -> ())
;;

let prepare t tickets =
  let open Result.Let_syntax in
  match t.phase with
  | Prepared _ | Closed -> conflict "Goal receipt selection is one-shot"
  | Open ->
    let checked =
      List.fold_result tickets ~init:Int.Set.empty ~f:(fun seen ticket ->
        if Set.mem seen ticket
        then conflict "duplicate Goal callback receipt"
        else (
          match Map.find t.entries ticket with
          | Some { Entry.phase = Recorded; _ } -> Ok (Set.add seen ticket)
          | _ -> conflict "Goal receipt is foreign, rolled back or incomplete"))
    in
    (match checked with
     | Error _ as failure ->
       close t;
       failure
     | Ok selected ->
       let resumes =
         List.count tickets ~f:(fun ticket ->
           match (Map.find_exn t.entries ticket).Entry.value with
           | Resume _ -> true
           | Edit _ -> false)
       in
       let%bind () =
         if resumes <= 1
         then Ok ()
         else (
           close t;
           conflict "callback cannot select multiple Goal Resume commands")
       in
       Map.iteri t.entries ~f:(fun ~key ~data ->
         if Set.mem selected key then data.Entry.phase <- Selected else Entry.discard data);
       t.entries <- Map.filter_keys t.entries ~f:(Set.mem selected);
       t.phase <- Prepared tickets;
       Ok ())
;;

let for_commit t =
  match t.phase with
  | Prepared tickets -> Ok (List.map tickets ~f:(Map.find_exn t.entries))
  | Open when Map.is_empty t.entries -> Ok []
  | Open | Closed ->
    conflict "native Goal actions were not selected by the callback transaction"
;;

let recorded t =
  match t.phase with
  | Open ->
    Ok
      (Map.data t.entries
       |> List.filter ~f:(fun entry ->
         match entry.Entry.phase with
         | Recorded -> true
         | Native_pending | Native_completed | Selected | Retained | Closed -> false))
  | Prepared _ | Closed -> conflict "Goal callback is no longer staging"
;;

let selected t =
  match t.phase with
  | Prepared tickets -> Ok (List.map tickets ~f:(Map.find_exn t.entries))
  | Open | Closed -> conflict "Goal callback selection is not prepared"
;;

module Plan = struct
  type t =
    { revision : int64
    ; session_id : P.Id.Session.t
    ; generation : int
    ; entries : Entry.t list
    ; projected : Session_state.t
    ; deltas : Session_delta.t list
    ; resumes : Entry.t list
    }

  let state t = t.projected
  let deltas t = t.deltas
  let resumes t = t.resumes

  let prepare entries ~(state : Session_state.t) ~session ~now =
    let open Result.Let_syntax in
    let%bind projected, deltas, resumes =
      List.fold_result
        entries
        ~init:(state, [], [])
        ~f:(fun (state, deltas, resumes) entry ->
          let%bind () = Entry.check entry state in
          let%bind actor = Entry.actor entry in
          match Entry.value entry with
          | Edit request ->
            let%bind prepared =
              Goal_command_admission.prepare
                state
                ~actor
                ~authorize:(entry.Entry.authorize actor)
                ~session
                ~request
                ~now
            in
            (match prepared.delta with
             | None -> Ok (state, deltas, resumes)
             | Some delta ->
               let%map state = Session_delta.apply state delta in
               state, delta :: deltas, resumes)
          | Resume intent ->
            let request = P.Goal_native_resume_intent.request intent in
            let%bind goal =
              Option.bind state.goal_catalog ~f:(fun catalog ->
                Goal_catalog.find catalog (Goal_resume_request.goal_id request))
              |> Result.of_option
                   ~error:(P.Error.invalid_request "native Goal Resume target is absent")
            in
            let%bind () =
              match P.Goal.state goal with
              | Open (Held Operator_pause) ->
                denied "native Goal Resume cannot clear an operator pause"
              | Open (Enabled | Held _) | Closed _ -> Ok ()
            in
            Ok (state, deltas, entry :: resumes))
    in
    Ok
      { revision = state.counters.revision
      ; session_id = state.identity.session_id
      ; generation = state.identity.generation
      ; entries
      ; projected
      ; deltas = List.rev deltas
      ; resumes = List.rev resumes
      }
  ;;

  let check t (state : Session_state.t) =
    let open Result.Let_syntax in
    let%bind () =
      if
        Int64.equal t.revision state.counters.revision
        && P.Id.Session.equal t.session_id state.identity.session_id
        && Int.equal t.generation state.identity.generation
      then Ok ()
      else conflict "Goal callback candidate was displaced"
    in
    List.fold_result t.entries ~init:() ~f:(fun () entry -> Entry.check entry state)
  ;;
end
