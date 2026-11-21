open! Core
module P = Agent_protocol
module B = Recovery_rollover_batch
module R = Recovery_effective_record
module H = Inference_ledger.Handle
module T = Inference.Event.Terminal
module Authored = Recovery_authored_checkpoint
module Checkpoint = Recovery_rollover.Checkpoint
module Evidence = Recovery_rollover_evidence

let conflict message = Error (P.Error.create Conflict ~message ~retryable:true ())

let capture state ~scope ~operation live =
  let module S = Chat_response.Settled_tool_batch in
  let open Result.Let_syntax in
  let%bind record, handle, terminal = Evidence.require_attempt state ~scope ~operation in
  let actual = S.attempt live in
  if
    Transcript.Scope.equal (Inference_runtime.Attempt.scope actual) (H.scope handle)
    && Inference.Observation.Observation_id.equal
         (Inference_runtime.Attempt.accounting_id actual)
         (H.accounting_id handle)
    && T.equal (S.terminal live) terminal
  then
    B.create
      ~operation
      ~ordinal:(H.ordinal handle)
      ~source_revision:(R.source_revision record)
      (List.map (S.pairs live) ~f:(fun pair ->
         B.{ call = pair.call; output = pair.output }))
  else conflict "settled batch does not match the returned host attempt"
;;

let matching_invocations (state : Session_state.t) calls =
  let call_ids = Hash_set.of_list (module P.History.Id) calls in
  let by_call = Hashtbl.create (module P.History.Id) in
  let%map.Result () =
    List.fold_result state.invocations ~init:() ~f:(fun () invocation ->
      match invocation.context.call_entry_id with
      | Some id when Hash_set.mem call_ids id ->
        (match Hashtbl.add by_call ~key:id ~data:invocation with
         | `Ok -> Ok ()
         | `Duplicate -> conflict "multiple invocations claim one settled call")
      | _ -> Ok ())
  in
  by_call
;;

let native_control state invocations (pair : B.pair) =
  Option.exists
    (Hashtbl.find invocations pair.call)
    ~f:(fun (invocation : P.Invocation.t) ->
      let name = invocation.context.tool_name in
      let usual =
        (String.equal name "context_remaining" || String.equal name "context_new_window")
        && String.equal
             invocation.context.implementation_revision
             (Chatmd_shell_spec.Source_ref.digest
                ("ochat.recovery.context.native.v1:" ^ name))
      in
      let authored =
        Option.bind state.Session_state.recovery_rollover ~f:Recovery_rollover.checkpoint
        |> Option.exists ~f:(fun checkpoint ->
          match Checkpoint.recipe checkpoint with
          | Authored binding -> Authored.matches_invocation binding invocation
          | Owner_selection _ -> false)
      in
      (usual || authored) && Evidence.published state pair invocation)
;;

let validate (state : Session_state.t) ~scope ~requester (batch : B.t) =
  let open Result.Let_syntax in
  let%bind () =
    match
      state.active_operation, state.lifecycle.observed, state.runtime_initialization
    with
    | Some operation, Running_turn running, Ready
      when P.Session.equal_desired_state state.lifecycle.desired Running
           && P.Id.Operation.equal running batch.operation
           && P.Id.Operation.equal operation.id batch.operation
           && Int.equal operation.generation state.identity.generation
           &&
           match operation.kind, operation.state with
           | Turn _, Running -> true
           | _ -> false -> Ok ()
    | _ -> conflict "settled batch no longer owns the foreground window"
  in
  let%bind record, handle, _ =
    Evidence.require_attempt state ~scope ~operation:batch.operation
  in
  let%bind () =
    if
      Int64.equal (R.source_revision record) batch.source_revision
      && Int64.equal (H.ordinal handle) batch.ordinal
    then Ok ()
    else conflict "settled batch attempt has been superseded"
  in
  let history = state.conversation.canonical_history in
  let%bind () =
    if Stdlib.List.compare_length_with history Recovery_window_record.max_members > 0
    then conflict "settled batch exceeds the recovery window bound"
    else Ok ()
  in
  let calls = List.map batch.pairs ~f:(fun pair -> pair.B.call) in
  let outputs = List.map batch.pairs ~f:(fun pair -> pair.B.output) in
  let ids = calls @ outputs in
  let selected = Hash_set.of_list (module P.History.Id) ids in
  let by_id = Hashtbl.create (module P.History.Id) in
  let first = List.hd_exn calls in
  let prefix, suffix =
    List.split_while history ~f:(fun entry -> not (P.History.Id.equal entry.id first))
  in
  let%bind () =
    let prepared_ids =
      List.map (R.members record) ~f:(fun member ->
        P.Recovery_window.Citation.occurrence (R.Member.citation member))
    in
    if
      List.exists prepared_ids ~f:(Hash_set.mem selected)
      || List.exists prefix ~f:(fun entry -> Hash_set.mem selected entry.id)
    then conflict "settled batch reuses a preceding history occurrence"
    else Ok ()
  in
  let ordered =
    List.filter_map suffix ~f:(fun entry ->
      match entry.kind with
      | Tool_call | Tool_output ->
        Hashtbl.set by_id ~key:entry.id ~data:entry;
        Some entry.id
      | Message | Reasoning | Other -> None)
  in
  let%bind () =
    if List.equal P.History.Id.equal ids ordered
    then Ok ()
    else conflict "settled calls or results are absent, duplicated, or out of order"
  in
  let%bind invocations = matching_invocations state calls in
  let%bind () =
    List.fold_result batch.pairs ~init:() ~f:(fun () pair ->
      let%bind () =
        Evidence.pair_is_bound
          ~call:(Hashtbl.find_exn by_id pair.call)
          ~output:(Hashtbl.find_exn by_id pair.output)
      in
      match Hashtbl.find invocations pair.call with
      | None -> Ok ()
      | Some invocation when Evidence.published state pair invocation -> Ok ()
      | Some _ -> conflict "model invocation has not acknowledged its exact result")
  in
  let%map () =
    match requester with
    | Recovery_rollover.Requester.Operator _ -> Ok ()
    | Native { operation; invocation; _ } ->
      if
        P.Id.Operation.equal operation batch.operation
        && List.exists batch.pairs ~f:(fun pair ->
          Option.exists (Hashtbl.find invocations pair.B.call) ~f:(fun row ->
            P.Id.Invocation.equal row.context.id invocation
            && String.equal row.context.tool_name "context_new_window"))
      then Ok ()
      else conflict "requesting invocation is outside its actual settled batch"
  in
  if
    List.equal P.History.Id.equal ids (List.map suffix ~f:(fun entry -> entry.id))
    && List.for_all batch.pairs ~f:(native_control state invocations)
  then Some prefix
  else None
;;
