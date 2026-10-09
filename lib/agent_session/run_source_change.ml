open! Core
module P = Agent_protocol

let prepare (state : Session_state.t) ~delta ~now =
  let rec selected ~index ~moderator = function
    | Session_delta.Batch changes ->
      List.fold changes ~init:(index, moderator) ~f:(fun (index, moderator) change ->
        selected ~index ~moderator change)
    | Created candidate -> candidate.run_state, Some candidate.moderator
    | Run_state_changed incoming -> Some incoming, moderator
    | Moderator_changed incoming -> index, Some incoming
    (* Only explicit installed-state mutations select a source. Other deltas
       preserve their existing ownership semantics and order. *)
    | _ -> index, moderator
  in
  let index, moderator = selected ~index:state.run_state ~moderator:None delta in
  let open Result.Let_syntax in
  match index, moderator with
  | None, _ | Some _, None -> Ok delta
  | Some index, Some incoming ->
    let%bind observer = Moderator_checkpoint.observer incoming in
    let previous = (Run_state.installation index).source in
    if Option.equal P.Invocation.equal_observer previous observer
    then Ok delta
    else if Int64.equal state.counters.revision Int64.max_value
    then
      Error
        (P.Error.invalid_request "session revision exhausted during source replacement")
    else (
      let change =
        match observer with
        | Some observer -> Run_source_installation.Change.Replace observer
        | None -> Remove
      in
      let%map index =
        match Session_replacement_delta.classify delta, state.run_state with
        | Some _, Some original
          when Int64.(
                 (Run_state.installation index).epoch
                 > (Run_state.installation original).epoch) ->
          (* Reset planning already retired the old scope. Its temporary removed
             source was never installed; join the final captured source into the
             same single durable rotation without changing retirement evidence. *)
          Run_state.complete_replacement index ~previous:original ~source:observer
        | (Some _ | None), (Some _ | None) ->
          Run_retirement.replace
            index
            ~change
            ~session_revision:(Int64.succ state.counters.revision)
            ~now
      in
      match Session_replacement_delta.classify delta with
      | Some replacement ->
        let candidate = Session_replacement_delta.state replacement in
        Session_replacement_delta.with_state
          replacement
          { candidate with run_state = Some index }
      | None -> Session_delta.Batch [ delta; Run_state_changed index ])
;;
