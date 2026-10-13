open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Scope = Agent_session.Developer_job_scope

let ( let* ) value f = Result.bind value ~f

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
      ; after : P.Developer_job.State.t
      ; lease : Job_capacity.lease
      ; handle : Store.handle
      }

and transition_retry =
  | Entered of
      { claim : Store.Claim.t
      ; after : P.Developer_job.State.t
      ; revoked_after : P.Developer_job.State.t
      }
  | Unentered of Store.Claim.t

and unentered_claim =
  { claim : Store.Claim.t
  ; lease : Job_capacity.lease
  ; handle : Store.handle
  }

type finalization =
  | Final of Store.Observation.t
  | Pending of quarantined

type lease_stage =
  | Capacity_only
  | Claim_prepared of Store.Prepared.t
  | Claim_issued of Store.Prepared.t
  | Callback_finished of Store.Claim.t * P.Developer_job.State.t
  | Terminal_prepared of Store.Prepared.t * transition_retry
  | Retained

type call =
  { mutable cancel : Eio.Cancel.t option
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
  ; domain_id : int
  ; mutable current : Store.handle
  ; mutable quarantined : quarantined ref list
  ; mutable calls : call list
  ; mutable reconciling : bool
  ; mutable closed : bool
  }

let call_key : t Eio.Fiber.key = Eio.Fiber.create_key ()
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
  | Callback_finished (claim, after) ->
    t.quarantined <- ref (Live_claim { claim; after; lease; handle }) :: t.quarantined
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
          ~after
          ~revoked_after
          ~lease
          ~attempts
          ~on_prepared
          ~on_stale
  =
  let retry = Entered { claim; after; revoked_after } in
  let retry_stale () =
    on_stale ();
    settle_transition
      ~handle
      ~claim
      ~after
      ~revoked_after
      ~lease
      ~attempts:(attempts - 1)
      ~on_prepared
      ~on_stale
  in
  if attempts <= 0
  then Ok (Pending (Live_claim { claim; after; lease; handle }))
  else (
    match Store.observe handle with
    | Error _ -> Ok (Pending (Live_claim { claim; after; lease; handle }))
    | Ok expected ->
      (match
         Store.prepare_claimed_transition handle ~expected ~claim ~after ~revoked_after
       with
       | Error Store.Error.Conflict ->
         settle_transition
           ~handle
           ~claim
           ~after
           ~revoked_after
           ~lease
           ~attempts:(attempts - 1)
           ~on_prepared
           ~on_stale
       | Error _ -> Ok (Pending (Live_claim { claim; after; lease; handle }))
       | Ok prepared ->
         on_prepared prepared;
         (match Store.commit_claimed_transition handle prepared with
          | Ok installed -> Ok (Final (Store.Installed.observation installed))
          | Error Store.Error.Prewrite_conflict -> retry_stale ()
          | Error _ ->
            (match Store.reconcile_claimed_transition handle prepared with
             | Ok (Store.Installed installed) ->
               Ok (Final (Store.Installed.observation installed))
             | Ok Store.Absent ->
               (match Store.commit_claimed_transition handle prepared with
                | Ok installed -> Ok (Final (Store.Installed.observation installed))
                | Error Store.Error.Prewrite_conflict -> retry_stale ()
                | Error _ -> Ok (Pending (Transition { prepared; lease; handle; retry })))
             | Error Store.Error.Prewrite_conflict -> retry_stale ()
             | Ok Store.Diverged | Error _ ->
               Ok (Pending (Transition { prepared; lease; handle; retry }))))))
;;

let with_registered_call t f =
  let finished, finish = Eio.Promise.create () in
  let call = { cancel = None; finished; finish } in
  t.calls <- call :: t.calls;
  Exn.protect
    ~finally:(fun () ->
      call.cancel <- None;
      t.calls <- List.filter t.calls ~f:(fun item -> not (phys_equal item call));
      Eio.Promise.resolve call.finish ())
    ~f:(fun () ->
      Eio.Cancel.sub (fun context ->
        call.cancel <- Some context;
        Eio.Fiber.with_binding call_key t (fun () -> f ())))
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

let with_claimed_attempt t ~expected ~lookup f =
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
        with_registered_call t (fun () ->
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
                    let finish after =
                      let revoked_after = revoked_after t claim after in
                      stage := Callback_finished (claim, after);
                      match
                        settle_transition
                          ~handle
                          ~claim
                          ~after
                          ~revoked_after
                          ~lease
                          ~attempts:3
                          ~on_prepared:(fun prepared ->
                            stage
                            := Terminal_prepared
                                 (prepared, Entered { claim; after; revoked_after }))
                          ~on_stale:(fun () -> stage := Callback_finished (claim, after))
                      with
                      | Ok (Final observed) ->
                        stage := Capacity_only;
                        Ok observed
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
                                (finish (terminal_state t claim status)
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
                            , terminal_state
                                t
                                claim
                                (P.Job.Interrupted "completion check interrupted") );
                       let after, contract_error =
                         normalize_callback_state t ~claim proposed
                       in
                       stage := Callback_finished (claim, after);
                       let* observed = finish after in
                       (match contract_error with
                        | Some error -> Error error
                        | None ->
                          Ok
                            (Store.find_job
                               observed
                               (P.Developer_job.id (Store.Claim.job claim))))
                     | Ok (Error error) ->
                       let* _ = finish (terminal_state t claim (P.Job.Failed error)) in
                       Error error))))
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

let retry_entered t slot ~handle ~claim ~after ~revoked_after ~lease =
  match
    settle_transition
      ~handle
      ~claim
      ~after
      ~revoked_after
      ~lease
      ~attempts:3
      ~on_prepared:(fun prepared ->
        slot
        := Transition
             { prepared; lease; handle; retry = Entered { claim; after; revoked_after } })
      ~on_stale:(fun () -> slot := Live_claim { claim; after; lease; handle })
  with
  | Ok (Final _) ->
    Job_capacity.release lease;
    None
  | Ok (Pending next) ->
    slot := next;
    Some slot
  | Error _ -> Some slot
;;

let retry_transition t slot ~handle ~lease = function
  | Entered { claim; after; revoked_after } ->
    retry_entered t slot ~handle ~claim ~after ~revoked_after ~lease
  | Unentered claim ->
    slot := settle_unentered t ~handle ~claim ~lease;
    Some slot
;;

let reconcile_entry t slot =
  match !slot with
  | Claim { prepared; lease; handle } ->
    (match Store.reconcile_job_claim handle prepared with
     | Ok Store.Claim_absent | Error Store.Error.Prewrite_conflict ->
       Job_capacity.release lease;
       None
     | Ok (Store.Claimed claimed) ->
       slot := settle_unentered t ~handle ~claim:(Store.Claimed.claim claimed) ~lease;
       Some slot
     | Ok Store.Claim_diverged | Error _ -> Some slot)
  | Unentered_claim { claim; lease; handle } ->
    slot := settle_unentered t ~handle ~claim ~lease;
    Some slot
  | Transition { prepared; lease; handle; retry } ->
    (match Store.reconcile_claimed_transition handle prepared with
     | Ok (Store.Installed _) ->
       Job_capacity.release lease;
       None
     | Ok Store.Absent ->
       (match Store.commit_claimed_transition handle prepared with
        | Ok _ ->
          Job_capacity.release lease;
          None
        | Error Store.Error.Prewrite_conflict ->
          retry_transition t slot ~handle ~lease retry
        | Error _ -> Some slot)
     | Error Store.Error.Prewrite_conflict -> retry_transition t slot ~handle ~lease retry
     | Ok Store.Diverged | Error _ -> Some slot)
  | Live_claim { claim; after; lease; handle } ->
    retry_entered
      t
      slot
      ~handle
      ~claim
      ~after
      ~revoked_after:(revoked_after t claim after)
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
            (match reconcile_entry t slot with
             | None -> ()
             | Some slot -> settled := slot :: !settled);
            unresolved := rest;
            run rest
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
      Job_capacity.close_developer_run t.capacity ~owner:(Store.owner t.current);
      reconcile_quarantined t)
;;

let create
      ~sw
      ~store
      ~initial
      ~capacity
      ~project_source
      ~workspace_conflict_domain
      ~now
      ~max_active
      ~max_quarantined
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
          ignore (close_and_wait t : (unit, P.Error.t) result)));
      Ok t)
;;
