open Core
module P = Agent_protocol
module D = Agent_store.Delegation_store
module A = Agent_session.Session_actor

let stop_owned ?parent_stop_epoch ~clock ~delegations ~reference ~actor ~runtime () =
  (* This is also called from a cancelled parent lease's finalizer. Protect the
     private lookup itself, including waiting for the ledger lock. *)
  Eio.Cancel.protect (fun () ->
    let open Result.Let_syntax in
    let%bind record =
      D.resolve delegations reference
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    in
    let%bind () =
      match record.admission.lifetime with
      | Owned -> Ok ()
      | Independent _ ->
        Error
          (P.Error.create
             Permission_denied
             ~message:
               "delegation.lifetime: independent child is outside owned stop propagation"
             ~retryable:false
             ())
    in
    (* Once cancellation is accepted, the resource owner must not leave while a
     worker or moderator is still unwinding. No actor/registry/owner lock is held
     through this wait. A failed stop does not grant permission to retire tools. *)
    let%bind _ =
      match parent_stop_epoch with
      | None -> A.stop_delegated actor ~reference ~mode:Cancel
      | Some epoch -> A.stop_delegated_at_epoch actor ~reference ~epoch
    in
    let rec quiescent () =
      let%bind state =
        A.with_quiescent_state actor ~f:(fun state ->
          match state.lifecycle.desired, state.lifecycle.observed with
          | Stopped, Stopped -> Ok (Agent_session.Session_state.summary state)
          | _ ->
            Error
              (P.Error.create
                 Conflict
                 ~message:"delegation.stop: child resumed during owned stop propagation"
                 ~retryable:true
                 ()))
      in
      match state with
      | Some summary -> Ok summary
      | None ->
        Eio.Time.sleep clock 0.01;
        quiescent ()
    in
    let%bind summary = quiescent () in
    let%map () = Runtime_owner.unload_and_wait runtime in
    summary)
;;
