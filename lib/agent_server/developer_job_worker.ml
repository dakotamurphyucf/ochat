open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Scope = Agent_session.Developer_job_scope
module Selected = Developer_job_selected

let ( let* ) value f = Result.bind value ~f

type terminal =
  { after : P.Developer_job.State.t
  ; selected : Selected.t option
  }

type close_policy =
  | Retire_unclaimed
  | Retain_unclaimed

type quarantined =
  | Unentered_claim of unentered_claim
  | Claim of
      { prepared : Store.Prepared.t
      ; lease : Job_capacity.lease
      ; handle : Store.handle
      }
  | Transition of
      { prepared : Store.Prepared.t
      ; lease : Job_capacity.lease
      ; handle : Store.handle
      ; retry : transition_retry
      }
  | Live_claim of
      { claim : Store.Claim.t
      ; terminal : terminal
      ; lease : Job_capacity.lease
      ; handle : Store.handle
      }

and transition_retry =
  | Entered of
      { claim : Store.Claim.t
      ; terminal : terminal
      ; revoked_after : P.Developer_job.State.t
      }
  | Unentered of Store.Claim.t

and unentered_claim =
  { claim : Store.Claim.t
  ; lease : Job_capacity.lease
  ; handle : Store.handle
  }

type finalization =
  | Final of Store.Installed.t
  | Pending of quarantined

type acknowledgement =
  | Mismatch
  | Acknowledged
  | Unexpected of exn * Stdlib.Printexc.raw_backtrace

type reconciliation =
  | Unresolved of quarantined ref
  | Settled
  | Settled_with_exception of exn * Stdlib.Printexc.raw_backtrace

type lease_stage =
  | Capacity_only
  | Claim_prepared of Store.Prepared.t
  | Claim_issued of Store.Prepared.t
  | Callback_finished of Store.Claim.t * terminal
  | Terminal_prepared of Store.Prepared.t * transition_retry
  | Retained

type call =
  { mutable cancel : Eio.Cancel.t option
  ; mutable claim : Store.Claim.t option
  ; finished : unit Eio.Promise.t
  ; finish : unit Eio.Promise.u
  }

type t =
  { store : Store.t
  ; capacity : Job_capacity.t
  ; scope : Scope.t
  ; project_source : Chatml_source.Id.t
  ; workspace_conflict_domain : string
  ; now : unit -> P.Timestamp.t
  ; max_quarantined : int
  ; max_active : int
  ; close_policy : close_policy
  ; domain_id : int
  ; mutable current : Store.handle
  ; mutable quarantined : quarantined ref list
  ; mutable calls : call list
  ; mutable reconciling : bool
  ; mutable closed : bool
  }

let call_key : t Eio.Fiber.key = Eio.Fiber.create_key ()
let call_identity_key : call Eio.Fiber.key = Eio.Fiber.create_key ()
let invalid message = Error (P.Error.invalid_request message)

let error_of_store error =
  let code, message, retryable =
    match error with
    | Store.Error.Conflict | Prewrite_conflict ->
      P.Error.Conflict, "developer job changed before admission", true
    | Foreign_or_stale ->
      P.Error.Lease_stale, "developer job store handle is stale", false
    | Limit_exceeded ->
      P.Error.Resource_limit, "developer job store limit was reached", false
    | Closed -> P.Error.Invalid_state, "developer job store is closed", false
    | Store _ -> P.Error.Persistence_error, "developer job store operation failed", true
  in
  P.Error.create code ~message ~retryable ()
;;

let issuer t = t.scope
let same_domain t = Int.equal t.domain_id (Stdlib.Domain.self () :> int)

let capacity_key t job =
  Job_capacity.Key.developer
    ~owner:(P.Developer_job.owner job)
    ~project_source:t.project_source
    ~workspace_conflict_domain:t.workspace_conflict_domain
    ~kind:(P.Developer_job.kind job)
    ~nested_depth:(P.Developer_job.Launch.nested_depth (P.Developer_job.launch job))
;;

let quarantine t value =
  if List.length t.quarantined >= t.max_quarantined
  then invalid "developer job unresolved-claim registry is full"
  else (
    t.quarantined <- ref value :: t.quarantined;
    Ok ())
;;

let close_lease_stage t ~handle ~lease stage =
  match !stage with
  | Capacity_only -> Job_capacity.release lease
  | Claim_prepared prepared | Claim_issued prepared ->
    t.quarantined <- ref (Claim { prepared; lease; handle }) :: t.quarantined
  | Callback_finished (claim, terminal) ->
    t.quarantined <- ref (Live_claim { claim; terminal; lease; handle }) :: t.quarantined
  | Terminal_prepared (prepared, retry) ->
    t.quarantined <- ref (Transition { prepared; lease; handle; retry }) :: t.quarantined
  | Retained -> ()
;;

let with_owned_lease t ~handle lease f =
  let stage = ref Capacity_only in
  let close () =
    Eio.Cancel.protect (fun () -> close_lease_stage t ~handle ~lease stage)
  in
  match f stage with
  | value ->
    close ();
    value
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try close () with
     | _ -> ());
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let rec settle_transition
          ~handle
          ~claim
          ~terminal
          ~revoked_after
          ~lease
          ~attempts
          ~on_prepared
          ~on_stale
  =
  let after = terminal.after in
  let retry = Entered { claim; terminal; revoked_after } in
  let retry_stale () =
    on_stale ();
    settle_transition
      ~handle
      ~claim
      ~terminal
      ~revoked_after
      ~lease
      ~attempts:(attempts - 1)
      ~on_prepared
      ~on_stale
  in
  if attempts <= 0
  then Ok (Pending (Live_claim { claim; terminal; lease; handle }))
  else (
    match Store.observe handle with
    | Error _ -> Ok (Pending (Live_claim { claim; terminal; lease; handle }))
    | Ok expected ->
      (match
         Store.prepare_claimed_transition handle ~expected ~claim ~after ~revoked_after
       with
       | Error Store.Error.Conflict ->
         settle_transition
           ~handle
           ~claim
           ~terminal
           ~revoked_after
           ~lease
           ~attempts:(attempts - 1)
           ~on_prepared
           ~on_stale
       | Error _ -> Ok (Pending (Live_claim { claim; terminal; lease; handle }))
       | Ok prepared ->
         on_prepared prepared;
         (match Store.commit_claimed_transition handle prepared with
          | Ok installed -> Ok (Final installed)
          | Error Store.Error.Prewrite_conflict -> retry_stale ()
          | Error _ ->
            (match Store.reconcile_claimed_transition handle prepared with
             | Ok (Store.Installed installed) -> Ok (Final installed)
             | Ok Store.Absent ->
               (match Store.commit_claimed_transition handle prepared with
                | Ok installed -> Ok (Final installed)
                | Error Store.Error.Prewrite_conflict -> retry_stale ()
                | Error _ -> Ok (Pending (Transition { prepared; lease; handle; retry })))
             | Error Store.Error.Prewrite_conflict -> retry_stale ()
             | Ok Store.Diverged | Error _ ->
               Ok (Pending (Transition { prepared; lease; handle; retry }))))))
;;

let with_registered_call t f =
  let finished, finish = Eio.Promise.create () in
  let call = { cancel = None; claim = None; finished; finish } in
  t.calls <- call :: t.calls;
  Exn.protect
    ~finally:(fun () ->
      call.cancel <- None;
      t.calls <- List.filter t.calls ~f:(fun item -> not (phys_equal item call));
      Eio.Promise.resolve call.finish ())
    ~f:(fun () ->
      Eio.Cancel.sub (fun context ->
        call.cancel <- Some context;
        Eio.Fiber.with_binding call_key t (fun () ->
          Eio.Fiber.with_binding call_identity_key call (fun () -> f call))))
;;

let terminal_state t claim status =
  let state = P.Developer_job.state (Store.Claim.job claim) in
  { state with
    status
  ; completed_at = Some (t.now ())
  ; result = None
  ; retry_failure = None
  }
;;

let revoked_after t claim (after : P.Developer_job.State.t) =
  match after.status with
  | P.Job.Succeeded | Failed _ | Cancelled | Interrupted _ -> after
  | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
    terminal_state t claim (P.Job.Interrupted "epoch changed")
;;

let normalize_callback_state t ~claim proposed =
  let job = Store.Claim.job claim in
  let shape = P.Developer_job.with_state job proposed in
  let issued = P.Developer_job.state job in
  let same_attempt = Int.equal proposed.attempt issued.attempt in
  let same_start =
    match proposed.status with
    | P.Job.Queued -> Option.is_none proposed.started_at
    | Running
    | Waiting_permission _
    | Waiting_completion _
    | Succeeded
    | Failed _
    | Cancelled
    | Interrupted _ ->
      Option.equal P.Timestamp.equal proposed.started_at issued.started_at
  in
  let ended =
    match proposed.P.Developer_job.State.status with
    | P.Job.Queued
    | Waiting_completion _
    | Succeeded
    | Failed _
    | Cancelled
    | Interrupted _ -> true
    | Running | Waiting_permission _ -> false
  in
  if Result.is_error shape || (not ended) || (not same_attempt) || not same_start
  then (
    let error =
      P.Error.create
        P.Error.Invalid_state
        ~message:"developer job callback returned an invalid end state"
        ~retryable:false
        ()
    in
    terminal_state t claim (P.Job.Failed error), Some error)
  else proposed, None
;;

let selected_matches_claim selected ~claim ~proposed =
  phys_equal (Selected.claim selected) claim
  && Selected.same_state (Selected.proposed selected) proposed
;;

let selected_matches_installed selected installed =
  let claim = Selected.claim selected in
  let original = Store.Claim.job claim in
  match
    Store.find_job (Store.Installed.observation installed) (P.Developer_job.id original)
  with
  | None -> false
  | Some installed_job ->
    P.Developer_job.same_immutable original installed_job
    && Selected.same_state (P.Developer_job.state installed_job) (Selected.after selected)
;;

let acknowledge_selected terminal installed =
  match terminal.selected with
  | None -> Acknowledged
  | Some selected when not (selected_matches_installed selected installed) -> Mismatch
  | Some selected ->
    (* A typed cleanup error leaves the marker for retention. An unexpected
       exception remains observable after the installed claim is retired. *)
    (match
       Eio.Cancel.protect (fun () -> Selected.acknowledge_installed selected installed)
     with
     | Ok () | Error _ -> Acknowledged
     | exception exn -> Unexpected (exn, Stdlib.Printexc.get_raw_backtrace ()))
;;

let ensure_selected terminal =
  match terminal.selected with
  | None -> Ok ()
  | Some selected -> Selected.ensure_attempted selected
;;

let with_claimed_attempt t ~expected ~lookup ?select f =
  if t.closed || not (same_domain t)
  then invalid "developer job worker is closed or on a foreign domain"
  else if t.reconciling
  then invalid "developer job claims are being reconciled"
  else if
    List.length t.calls >= t.max_active
    || List.length t.quarantined + List.length t.calls >= t.max_quarantined
  then invalid "developer job unresolved claims block admission"
  else (
    match Store.Job_lookup.value lookup with
    | None -> Ok None
    | Some job ->
      if
        not
          (P.Execution_owner.equal_developer
             (P.Developer_job.owner job)
             (Store.owner t.current))
      then invalid "developer job belongs to another epoch"
      else if
        match (P.Developer_job.state job).status with
        | P.Job.Queued ->
          Option.value_map
            (P.Developer_job.state job).next_run_at
            ~default:false
            ~f:(fun due -> P.Timestamp.compare (t.now ()) due < 0)
        | Running
        | Waiting_permission _
        | Waiting_completion _
        | Succeeded
        | Failed _
        | Cancelled
        | Interrupted _ -> true
      then Ok None
      else
        with_registered_call t (fun call ->
          let handle = t.current in
          if
            t.closed
            || not
                 (P.Execution_owner.equal_developer
                    (Store.owner handle)
                    (P.Developer_job.owner job))
          then invalid "developer job epoch changed before capacity admission"
          else
            let* key = capacity_key t job in
            let* host_job = P.Host_job.of_developer job in
            match Job_capacity.try_acquire_host_job t.capacity key ~job:host_job with
            | Error error -> Error error
            | Ok None -> Ok None
            | Ok (Some lease) ->
              with_owned_lease t ~handle lease (fun stage ->
                if t.closed
                then invalid "developer job worker closed before claim"
                else
                  let* prepared =
                    Store.prepare_job_claim handle ~expected ~lookup ~now:(t.now ())
                    |> Result.map_error ~f:error_of_store
                  in
                  stage := Claim_prepared prepared;
                  let claim_result =
                    match Store.commit_job_claim handle prepared with
                    | Ok claimed -> Ok (Store.Claimed claimed)
                    | Error Store.Error.Prewrite_conflict ->
                      Error Store.Error.Prewrite_conflict
                    | Error _ -> Store.reconcile_job_claim handle prepared
                  in
                  match claim_result with
                  | Error Store.Error.Prewrite_conflict ->
                    stage := Capacity_only;
                    Error
                      (P.Error.create
                         P.Error.Conflict
                         ~message:"developer job changed before the claim write"
                         ~retryable:true
                         ())
                  | Ok Store.Claim_absent ->
                    stage := Capacity_only;
                    Ok None
                  | Ok Store.Claim_diverged | Error _ ->
                    invalid "developer job claim requires recovery"
                  | Ok (Store.Claimed claimed) ->
                    let claim = Store.Claimed.claim claimed in
                    stage := Claim_issued prepared;
                    let finish terminal =
                      let revoked_after = revoked_after t claim terminal.after in
                      stage := Callback_finished (claim, terminal);
                      let* () = ensure_selected terminal in
                      match
                        settle_transition
                          ~handle
                          ~claim
                          ~terminal
                          ~revoked_after
                          ~lease
                          ~attempts:3
                          ~on_prepared:(fun prepared ->
                            stage
                            := Terminal_prepared
                                 (prepared, Entered { claim; terminal; revoked_after }))
                          ~on_stale:(fun () ->
                            stage := Callback_finished (claim, terminal))
                      with
                      | Ok (Final installed) ->
                        (match acknowledge_selected terminal installed with
                         | Acknowledged ->
                           stage := Capacity_only;
                           Ok (Store.Installed.observation installed)
                         | Mismatch ->
                           invalid "installed developer result differs from selection"
                         | Unexpected (exn, backtrace) ->
                           stage := Capacity_only;
                           Stdlib.Printexc.raise_with_backtrace exn backtrace)
                      | Ok (Pending pending) ->
                        let* () = quarantine t pending in
                        stage := Retained;
                        invalid "developer job completion requires recovery"
                      | Error error -> Error error
                    in
                    let callback_started = ref false in
                    let callback_result =
                      match
                        Scope.with_claim t.scope ~handle ~claim (fun ~active ~sw ->
                          call.claim <- Some claim;
                          callback_started := true;
                          f ~active ~sw)
                      with
                      | result -> result
                      | exception exn ->
                        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                        if !callback_started
                        then (
                          let status =
                            match exn with
                            | Eio.Cancel.Cancelled _ -> P.Job.Cancelled
                            | _ ->
                              P.Job.Failed
                                (P.Error.create
                                   P.Error.Internal_error
                                   ~message:"developer job callback failed"
                                   ~retryable:false
                                   ())
                          in
                          try
                            Eio.Cancel.protect (fun () ->
                              ignore
                                (finish
                                   { after = terminal_state t claim status
                                   ; selected = None
                                   }
                                 : (_, P.Error.t) result))
                          with
                          | _ -> ());
                        Stdlib.Printexc.raise_with_backtrace exn backtrace
                    in
                    (match callback_result with
                     | Error error -> Error error
                     | Ok (Ok proposed) ->
                       stage
                       := Callback_finished
                            ( claim
                            , { after =
                                  terminal_state
                                    t
                                    claim
                                    (P.Job.Interrupted "completion check interrupted")
                              ; selected = None
                              } );
                       let after, contract_error =
                         normalize_callback_state t ~claim proposed
                       in
                       (match contract_error with
                        | Some error ->
                          let* _ = finish { after; selected = None } in
                          Error error
                        | None ->
                          let selection =
                            match select with
                            | None -> Ok None
                            | Some select ->
                              (match select ~claim ~proposed:after with
                               | result -> result
                               | exception exn ->
                                 let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                                 let status =
                                   match exn with
                                   | Eio.Cancel.Cancelled _ -> P.Job.Cancelled
                                   | _ ->
                                     P.Job.Failed
                                       (P.Error.create
                                          P.Error.Internal_error
                                          ~message:"developer result selection failed"
                                          ~retryable:false
                                          ())
                                 in
                                 (try
                                    Eio.Cancel.protect (fun () ->
                                      ignore
                                        (finish
                                           { after = terminal_state t claim status
                                           ; selected = None
                                           }
                                         : (_, P.Error.t) result))
                                  with
                                  | _ -> ());
                                 Stdlib.Printexc.raise_with_backtrace exn backtrace)
                          in
                          (match selection with
                           | Error error ->
                             let* _ =
                               finish
                                 { after = terminal_state t claim (P.Job.Failed error)
                                 ; selected = None
                                 }
                             in
                             Error error
                           | Ok selected ->
                             let valid =
                               Option.value_map selected ~default:true ~f:(fun selected ->
                                 selected_matches_claim selected ~claim ~proposed:after)
                             in
                             if not valid
                             then (
                               let error =
                                 P.Error.invalid_request
                                   "selected result belongs to another claimed completion"
                               in
                               let* _ =
                                 finish
                                   { after = terminal_state t claim (P.Job.Failed error)
                                   ; selected = None
                                   }
                               in
                               Error error)
                             else (
                               let terminal =
                                 { after =
                                     Option.value_map
                                       selected
                                       ~default:after
                                       ~f:Selected.after
                                 ; selected
                                 }
                               in
                               let* observed = finish terminal in
                               Ok
                                 (Store.find_job
                                    observed
                                    (P.Developer_job.id (Store.Claim.job claim))))))
                     | Ok (Error error) ->
                       let* _ =
                         finish
                           { after = terminal_state t claim (P.Job.Failed error)
                           ; selected = None
                           }
                       in
                       Error error))))
;;

module Cancellation = struct
  type t =
    | No_live_claim
    | Requested_self
    | Joined
end

let call_matches_job t job call =
  Option.value_map call.claim ~default:false ~f:(fun claim ->
    Store.Claim.issued_by claim t.store
    && P.Developer_job.same_immutable (Store.Claim.job claim) job
    && Int.equal (Store.Claim.attempt claim) (P.Developer_job.state job).attempt)
;;

let exact_registered_call t job =
  match List.filter t.calls ~f:(call_matches_job t job) with
  | [] -> Ok None
  | [ call ] -> Ok (Some call)
  | _ -> invalid "multiple physical callbacks own one developer job attempt"
;;

let same_call_fiber call =
  Option.value_map (Eio.Fiber.get call_identity_key) ~default:false ~f:(fun current ->
    phys_equal current call)
;;

let cancel_claimed t ~job =
  if not (same_domain t)
  then invalid "developer job cancellation belongs to another domain"
  else
    let* call = exact_registered_call t job in
    match call with
    | None -> Ok Cancellation.No_live_claim
    | Some call ->
      (match call.cancel with
       | None -> invalid "developer job callback has no cancellation context"
       | Some context ->
         Eio.Cancel.cancel context Exit;
         if same_call_fiber call
         then Ok Cancellation.Requested_self
         else (
           Eio.Promise.await call.finished;
           Ok Cancellation.Joined))
;;

let join_ended_attempt t ~job =
  if not (same_domain t)
  then invalid "developer job wait belongs to another domain"
  else if not (List.is_empty t.quarantined)
  then invalid "developer job claim or terminal write remains quarantined"
  else
    let* call = exact_registered_call t job in
    match call with
    | None -> Ok ()
    | Some call when same_call_fiber call ->
      invalid "developer job callback cannot join itself"
    | Some call ->
      Eio.Promise.await call.finished;
      if List.is_empty t.quarantined
      then Ok ()
      else invalid "developer job terminal write remains quarantined"
;;

let advance_epoch t ~new_current =
  if t.closed || not (same_domain t)
  then invalid "developer job worker is closed or on a foreign domain"
  else (
    match Scope.advance_epoch t.scope ~new_current with
    | Error error -> Error error
    | Ok () ->
      t.current <- new_current;
      Job_capacity.retire_previous_epochs
        t.capacity
        ~current_owner:(Store.owner new_current);
      Ok ())
;;

let settle_unentered t ~handle ~claim ~lease =
  match Store.observe handle with
  | Error _ -> Unentered_claim { claim; lease; handle }
  | Ok expected ->
    (match
       Store.prepare_unentered_claim_cleanup
         handle
         ~expected
         ~claim
         ~status:(`Interrupted "worker entry was not admitted")
         ~now:(t.now ())
     with
     | Error _ -> Unentered_claim { claim; lease; handle }
     | Ok prepared -> Transition { prepared; lease; handle; retry = Unentered claim })
;;

let settle_installed slot ~lease ~retry installed =
  let acknowledgement =
    match retry with
    | Entered { terminal; _ } -> acknowledge_selected terminal installed
    | Unentered _ -> Acknowledged
  in
  match acknowledgement with
  | Mismatch -> Unresolved slot
  | Acknowledged ->
    Job_capacity.release lease;
    Settled
  | Unexpected (exn, backtrace) ->
    Job_capacity.release lease;
    Settled_with_exception (exn, backtrace)
;;

let retry_entered t slot ~handle ~claim ~terminal ~revoked_after ~lease =
  match ensure_selected terminal with
  | Error _ -> Unresolved slot
  | Ok () ->
    (match
       settle_transition
         ~handle
         ~claim
         ~terminal
         ~revoked_after
         ~lease
         ~attempts:3
         ~on_prepared:(fun prepared ->
           slot
           := Transition
                { prepared
                ; lease
                ; handle
                ; retry = Entered { claim; terminal; revoked_after }
                })
         ~on_stale:(fun () -> slot := Live_claim { claim; terminal; lease; handle })
     with
     | Ok (Final installed) ->
       settle_installed
         slot
         ~lease
         ~retry:(Entered { claim; terminal; revoked_after })
         installed
     | Error _ -> Unresolved slot
     | Ok (Pending next) ->
       slot := next;
       Unresolved slot)
;;

let retry_transition t slot ~handle ~lease = function
  | Entered { claim; terminal; revoked_after } ->
    retry_entered t slot ~handle ~claim ~terminal ~revoked_after ~lease
  | Unentered claim ->
    slot := settle_unentered t ~handle ~claim ~lease;
    Unresolved slot
;;

let reconcile_entry t slot =
  match !slot with
  | Claim { prepared; lease; handle } ->
    (match Store.reconcile_job_claim handle prepared with
     | Ok Store.Claim_absent | Error Store.Error.Prewrite_conflict ->
       Job_capacity.release lease;
       Settled
     | Ok (Store.Claimed claimed) ->
       slot := settle_unentered t ~handle ~claim:(Store.Claimed.claim claimed) ~lease;
       Unresolved slot
     | Ok Store.Claim_diverged | Error _ -> Unresolved slot)
  | Unentered_claim { claim; lease; handle } ->
    slot := settle_unentered t ~handle ~claim ~lease;
    Unresolved slot
  | Transition { prepared; lease; handle; retry } ->
    (match Store.reconcile_claimed_transition handle prepared with
     | Ok (Store.Installed installed) -> settle_installed slot ~lease ~retry installed
     | Ok Store.Absent ->
       let prepared_ready =
         match retry with
         | Entered { terminal; _ } -> Result.is_ok (ensure_selected terminal)
         | Unentered _ -> true
       in
       if not prepared_ready
       then Unresolved slot
       else (
         match Store.commit_claimed_transition handle prepared with
         | Ok installed -> settle_installed slot ~lease ~retry installed
         | Error Store.Error.Prewrite_conflict ->
           retry_transition t slot ~handle ~lease retry
         | Error _ -> Unresolved slot)
     | Error Store.Error.Prewrite_conflict -> retry_transition t slot ~handle ~lease retry
     | Ok Store.Diverged | Error _ -> Unresolved slot)
  | Live_claim { claim; terminal; lease; handle } ->
    retry_entered
      t
      slot
      ~handle
      ~claim
      ~terminal
      ~revoked_after:(revoked_after t claim terminal.after)
      ~lease
;;

let reconcile_quarantined t =
  if (not (same_domain t)) || t.reconciling
  then invalid "developer job reconciliation has a foreign or busy owner"
  else (
    t.reconciling <- true;
    Exn.protect
      ~finally:(fun () -> t.reconciling <- false)
      ~f:(fun () ->
        let batch = t.quarantined in
        t.quarantined <- [];
        let settled = ref [] in
        let unresolved = ref batch in
        let rec run = function
          | [] -> unresolved := []
          | slot :: rest ->
            unresolved := slot :: rest;
            let result = reconcile_entry t slot in
            (match result with
             | Unresolved slot -> settled := slot :: !settled
             | Settled | Settled_with_exception _ -> ());
            unresolved := rest;
            (match result with
             | Settled_with_exception (exn, backtrace) ->
               Stdlib.Printexc.raise_with_backtrace exn backtrace
             | Settled | Unresolved _ -> run rest)
        in
        let remaining =
          match run batch with
          | () -> List.rev !settled
          | exception exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            t.quarantined <- List.rev !settled @ !unresolved @ t.quarantined;
            Stdlib.Printexc.raise_with_backtrace exn backtrace
        in
        t.quarantined <- remaining @ t.quarantined;
        if List.is_empty t.quarantined
        then Ok ()
        else invalid "developer job claims remain quarantined"))
;;

let close_and_wait t =
  if not (same_domain t)
  then invalid "developer job worker belongs to another domain"
  else if
    Option.value_map (Eio.Fiber.get call_key) ~default:false ~f:(fun current ->
      phys_equal current t)
  then invalid "developer job worker cannot close itself"
  else (
    let calls = t.calls in
    t.closed <- true;
    let scope_close = Scope.close_and_wait t.scope in
    Eio.Cancel.protect (fun () ->
      List.iter calls ~f:(fun call ->
        Option.iter call.cancel ~f:(fun context -> Eio.Cancel.cancel context Exit));
      List.iter calls ~f:(fun call -> Eio.Promise.await call.finished));
    match scope_close with
    | Error error -> Error error
    | Ok () ->
      (match t.close_policy with
       | Retire_unclaimed ->
         Job_capacity.close_developer_run t.capacity ~owner:(Store.owner t.current)
       | Retain_unclaimed -> ());
      reconcile_quarantined t)
;;

let create
      ?(close_policy = Retire_unclaimed)
      ~sw
      ~store
      ~initial
      ~capacity
      ~project_source
      ~workspace_conflict_domain
      ~now
      ~max_active
      ~max_quarantined
      ()
  =
  if
    max_active <= 0
    || max_quarantined < max_active
    || String.is_empty workspace_conflict_domain
    || not (Store.issued_by store initial)
  then invalid "invalid developer job worker owner or bounds"
  else (
    match Scope.create ~store ~initial ~max_active with
    | Error error -> Error error
    | Ok scope ->
      let t =
        { store
        ; capacity
        ; scope
        ; project_source
        ; workspace_conflict_domain
        ; now
        ; max_quarantined
        ; max_active
        ; close_policy
        ; domain_id = (Stdlib.Domain.self () :> int)
        ; current = initial
        ; quarantined = []
        ; calls = []
        ; reconciling = false
        ; closed = false
        }
      in
      Eio.Switch.check sw;
      Eio.Switch.on_release sw (fun () ->
        Eio.Cancel.protect (fun () ->
          match close_and_wait t with
          | Ok () -> ()
          | Error _ -> failwith "developer job worker shutdown is incomplete"));
      Ok t)
;;
