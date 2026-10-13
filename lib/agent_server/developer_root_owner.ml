open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Capability = Chat_response.Tool_capability
module Request = Chat_response.Background_request

module Limits = struct
  type t =
    { max_active_runs : int
    ; max_staged_jobs : int
    ; max_active_workers : int
    ; max_quarantined_workers : int
    }

  let create
        ~max_active_runs
        ~max_staged_jobs
        ~max_active_workers
        ~max_quarantined_workers
    =
    if
      max_active_runs <= 0
      || max_staged_jobs <= 0
      || max_active_workers <= 0
      || max_quarantined_workers < max_active_workers
      || max_active_runs > 4096
      || max_staged_jobs > 16384
      || max_active_workers > 16384
      || max_quarantined_workers > 16384
    then Or_error.error_string "invalid developer root owner limits"
    else
      Ok { max_active_runs; max_staged_jobs; max_active_workers; max_quarantined_workers }
  ;;
end

type state =
  | Live
  | Closing
  | Quarantined
  | Closed

type staged =
  { job : P.Developer_job.t
  ; reservation : Job_capacity.reservation
  }

type call =
  { mutable cancel : Eio.Cancel.t option
  ; finished : unit Eio.Promise.t
  ; finish : unit Eio.Promise.u
  }

type admission =
  { call : call
  ; mutable claim : Store.Run_claim.t option
  ; mutable worker : Developer_job_worker.t option
  }

type registry =
  { store : Store.t
  ; capacity : Job_capacity.t
  ; now : unit -> P.Timestamp.t
  ; limits : Limits.t
  ; switch : Eio.Switch.t
  ; domain_id : int
  ; owners : (P.Id.Developer_run.t, slot) Hashtbl.t
  ; mutable closed : bool
  }

and slot =
  | Admitting of admission
  | Owned of t

and t =
  { registry : registry
  ; actor : Operator_authorization.t
  ; capture : Chatml_developer_capture.t
  ; owner : P.Execution_owner.developer
  ; handle : Store.handle
  ; run_claim : Store.Run_claim.t
  ; worker : Developer_job_worker.t
  ; project_source : Chatml_source.Id.t
  ; workspace_conflict_domain : string
  ; job_policy : Chat_response.One_off_request.policy
  ; mutable selected : Capability.t
  ; mutable selection_revision : int
  ; mutable state : state
  ; mutable root_call : call option
  ; mutable active : action option
  ; mutable pending : pending option
  ; mutable reconciling : bool
  }

and action =
  { issuer : t
  ; snapshot : current
  ; staged : (P.Id.Job.t, staged) Hashtbl.t
  ; mutable stage_in_flight : int
  ; mutable sealed : selection option
  ; mutable live : bool
  ; call : call
  }

and selection =
  { issuer : action
  ; jobs : P.Developer_job.t list
  ; selected_ids : P.Id.Job.t list
  }

and current =
  { issuer : t
  ; selected : Capability.t
  ; selection_revision : int
  }

and pending =
  { prepared : Store.Prepared.t
  ; handoff : Store.Handoff.t
  ; action : action
  }

type run = t

let action_key : action Eio.Fiber.key = Eio.Fiber.create_key ()
let root_call_key : t Eio.Fiber.key = Eio.Fiber.create_key ()
let admission_key : registry Eio.Fiber.key = Eio.Fiber.create_key ()
let invalid message = Error (P.Error.invalid_request message)
let ( let* ) result f = Result.bind result ~f
let same_domain registry = Int.equal registry.domain_id (Stdlib.Domain.self () :> int)

let error_of_store error =
  let code, message, retryable =
    match error with
    | Store.Error.Conflict | Prewrite_conflict ->
      P.Error.Conflict, "developer run changed before admission", true
    | Foreign_or_stale -> P.Error.Lease_stale, "developer run owner is stale", false
    | Limit_exceeded -> P.Error.Resource_limit, "developer run limit was reached", false
    | Closed -> P.Error.Invalid_state, "developer run store is closed", false
    | Store _ -> P.Error.Persistence_error, "developer run store is unavailable", true
  in
  P.Error.create code ~message ~retryable ()
;;

let selected_within ~registry selected =
  List.fold_result (Capability.references selected) ~init:() ~f:(fun () reference ->
    Capability.resolve registry ~id:reference.id ~fingerprint:reference.fingerprint
    |> Result.map ~f:ignore
    |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message))
;;

let project_source capture =
  let prepared = Chatml_developer_capture.prepared capture in
  match Chatml_prepared_program.provenance prepared with
  | Project { declaration; lock = _ } -> declaration
  | Implicit -> Chatml_prepared_program.entry prepared
;;

let owner t = t.owner
let capture t = t.capture

let check_current_local t (witness : current) ~allow_pending =
  if
    (not (phys_equal witness.issuer t))
    || (not (same_domain t.registry))
    || t.registry.closed
    || (not (Int.equal witness.selection_revision t.selection_revision))
    || (not (phys_equal witness.selected t.selected))
    || ((not allow_pending) && Option.is_some t.pending)
    || t.reconciling
    || (not (Operator_authorization.is_current t.actor))
    ||
    match t.state with
    | Live -> false
    | Closing | Quarantined | Closed -> true
  then invalid "developer run authority changed"
  else Ok ()
;;

let check_effect_now t witness = check_current_local t witness ~allow_pending:false

let current t =
  if not (same_domain t.registry)
  then invalid "developer run owner belongs to another domain"
  else
    let* active = Store.is_latest_active t.handle |> Result.map_error ~f:error_of_store in
    if not active
    then invalid "developer run epoch is not active"
    else (
      let witness =
        { issuer = t; selected = t.selected; selection_revision = t.selection_revision }
      in
      Result.map (check_effect_now t witness) ~f:(fun () -> witness))
;;

module Current = struct
  type nonrec t = current

  let selected (t : t) = t.selected
end

module Selection = struct
  type nonrec t = selection

  let jobs (t : t) = t.jobs
end

let capacity_key t job =
  Job_capacity.Key.developer
    ~owner:t.owner
    ~project_source:t.project_source
    ~workspace_conflict_domain:t.workspace_conflict_domain
    ~kind:(P.Developer_job.kind job)
    ~nested_depth:(P.Developer_job.Launch.nested_depth (P.Developer_job.launch job))
;;

let same_action_fiber action =
  Option.value_map (Eio.Fiber.get action_key) ~default:false ~f:(fun current ->
    phys_equal current action)
;;

module Action = struct
  type nonrec t = action

  let stage_background t ~request ~checkpoint =
    let issuer = t.issuer in
    if (not (same_domain issuer.registry)) || (not (same_action_fiber t)) || not t.live
    then invalid "developer root action is not live in this scope"
    else if Option.is_some t.sealed
    then invalid "developer root action selection is sealed"
    else if
      Hashtbl.length t.staged + t.stage_in_flight
      >= issuer.registry.limits.max_staged_jobs
    then
      Error
        (P.Error.create
           P.Error.Resource_limit
           ~message:"developer root staged job limit was reached"
           ~retryable:false
           ())
    else (
      t.stage_in_flight <- t.stage_in_flight + 1;
      Exn.protect
        ~finally:(fun () -> t.stage_in_flight <- t.stage_in_flight - 1)
        ~f:(fun () ->
          let* prepared =
            Agent_session.Developer_root_job_admission.background
              ~owner:issuer.owner
              ~capture_fingerprint:(Chatml_developer_capture.fingerprint issuer.capture)
              ~selected:t.snapshot.selected
              ~policy:issuer.job_policy
              ~request
              ~created_at:(issuer.registry.now ())
              ~checkpoint
          in
          let* () = check_effect_now issuer t.snapshot in
          if (not t.live) || Option.is_some t.sealed
          then invalid "developer root action changed during job preparation"
          else (
            let job = Agent_session.Developer_root_job_admission.Prepared.job prepared in
            let id = P.Developer_job.id job in
            let* () =
              if Hashtbl.mem t.staged id
              then invalid "developer root job ID was already staged"
              else Ok ()
            in
            let* key = capacity_key issuer job in
            let* host_job = P.Host_job.of_developer job in
            let* reserved =
              Job_capacity.reserve_host_job issuer.registry.capacity key ~job:host_job
            in
            match reserved with
            | None ->
              Error
                (P.Error.create
                   P.Error.Resource_limit
                   ~message:"shared job capacity is full"
                   ~retryable:true
                   ())
            | Some reservation ->
              Hashtbl.set t.staged ~key:id ~data:{ job; reservation };
              Ok id)))
  ;;

  let seal t ~selected =
    if
      (not (same_domain t.issuer.registry))
      || (not (same_action_fiber t))
      || (not t.live)
      || Option.is_some t.sealed
    then invalid "developer root action cannot be sealed"
    else if t.stage_in_flight > 0
    then invalid "developer root job preparation is still active"
    else (
      let seen = Hash_set.create (module P.Id.Job) in
      let* jobs, _count =
        List.fold_result selected ~init:([], 0) ~f:(fun (jobs, count) id ->
          if count >= t.issuer.registry.limits.max_staged_jobs
          then invalid "developer root selection exceeds its bound"
          else if Hash_set.mem seen id
          then invalid "duplicate developer root selection"
          else (
            Hash_set.add seen id;
            match Hashtbl.find t.staged id with
            | None -> invalid "developer root selection is not staged here"
            | Some staged -> Ok (staged.job :: jobs, count + 1)))
      in
      let value = { issuer = t; jobs = List.rev jobs; selected_ids = selected } in
      t.sealed <- Some value;
      Ok value)
  ;;
end

let narrow_selection t ~names =
  if not (same_domain t.registry)
  then invalid "developer root owner belongs to another domain"
  else (
    match t.state with
    | Closing | Quarantined | Closed -> invalid "developer root owner is closed"
    | Live ->
      if not (Operator_authorization.is_current t.actor)
      then invalid "developer root actor expired"
      else
        let* selected =
          Capability.select t.selected ~names
          |> Result.map_error ~f:(fun error -> P.Error.invalid_request error.message)
        in
        if Int.equal t.selection_revision Int.max_value
        then invalid "developer root selection revision exhausted"
        else (
          t.selection_revision <- t.selection_revision + 1;
          t.selected <- selected;
          Ok ()))
;;

let abort_staged action =
  Hashtbl.iter action.staged ~f:(fun staged -> Job_capacity.abort staged.reservation)
;;

let settle_staged action selection = function
  | Store.Settlement.Installed _ ->
    let selected_ids = Hash_set.of_list (module P.Id.Job) selection.selected_ids in
    Hashtbl.iteri action.staged ~f:(fun ~key:id ~data:staged ->
      if Hash_set.mem selected_ids id
      then Job_capacity.publish staged.reservation
      else Job_capacity.abort staged.reservation)
  | Absent -> abort_staged action
  | Prewrite_conflict | Diverged -> ()
;;

let same_pending t pending =
  Option.value_map t.pending ~default:false ~f:(fun current ->
    phys_equal current.prepared pending.prepared
    && phys_equal current.handoff pending.handoff
    && phys_equal current.action pending.action)
;;

let reconcile_selected t pending =
  let* settlement =
    Store.reconcile_and_settle t.handle pending.prepared pending.handoff
    |> Result.map_error ~f:error_of_store
  in
  if not (same_pending t pending)
  then
    Error
      (P.Error.create
         P.Error.Conflict
         ~message:"developer root reconciliation result is stale"
         ~retryable:true
         ())
  else (
    (match settlement with
     | Store.Settlement.Installed _ | Absent | Prewrite_conflict -> t.pending <- None
     | Diverged -> t.state <- Quarantined);
    Ok settlement)
;;

let reconcile_pending t =
  if not (same_domain t.registry)
  then invalid "developer root owner belongs to another domain"
  else if Option.is_some t.root_call
  then invalid "developer root action is still active"
  else if t.reconciling
  then
    Error
      (P.Error.create
         P.Error.Conflict
         ~message:"developer root reconciliation is already active"
         ~retryable:true
         ())
  else (
    match t.pending with
    | None -> Ok ()
    | Some pending ->
      t.reconciling <- true;
      Exn.protect
        ~finally:(fun () -> t.reconciling <- false)
        ~f:(fun () ->
          let* settlement = reconcile_selected t pending in
          match settlement with
          | Store.Settlement.Installed _ | Absent -> Ok ()
          | Prewrite_conflict ->
            abort_staged pending.action;
            Ok ()
          | Diverged -> invalid "developer root transaction remains quarantined"))
;;

let selected_transaction t action selection ~phase =
  let rec attempt remaining =
    if remaining <= 0
    then invalid "developer root selected transaction changed repeatedly"
    else
      let* expected = Store.observe t.handle |> Result.map_error ~f:error_of_store in
      let* prepared =
        Store.prepare_root_selected t.handle ~expected ~phase ~selected:selection.jobs
        |> Result.map_error ~f:error_of_store
      in
      let handoff = Store.Handoff.create ~settle:(settle_staged action selection) in
      t.pending <- Some { prepared; handoff; action };
      let admit () =
        let exact_pending =
          Option.value_map t.pending ~default:false ~f:(fun pending ->
            phys_equal pending.prepared prepared
            && phys_equal pending.handoff handoff
            && phys_equal pending.action action)
        in
        if not exact_pending
        then false
        else (
          match check_current_local t action.snapshot ~allow_pending:true with
          | Error _ -> false
          | Ok () ->
            (match phase with
             | Store.Checkpoint.Active -> ()
             | Interrupted | Closed -> t.state <- Closing);
            true)
      in
      let result =
        Store.commit_and_settle t.handle prepared handoff ~admit
        |> Result.map_error ~f:error_of_store
      in
      match result with
      | Ok (Store.Settlement.Installed _) ->
        t.pending <- None;
        Ok ()
      | Ok Absent ->
        t.pending <- None;
        invalid "developer root selection was not committed"
      | Ok Prewrite_conflict ->
        t.pending <- None;
        attempt (remaining - 1)
      | Ok Diverged ->
        t.state <- Quarantined;
        invalid "developer root transaction remains quarantined"
      | Error original ->
        (match reconcile_selected t { prepared; handoff; action } with
         | Ok (Store.Settlement.Installed _) -> Ok ()
         | Ok Prewrite_conflict -> attempt (remaining - 1)
         | Ok Absent | Ok Diverged | Error _ -> Error original)
  in
  attempt 3
;;

let with_root_action t ~phase f =
  if not (same_domain t.registry)
  then invalid "developer root owner belongs to another domain"
  else if Option.is_some t.root_call || Option.is_some t.pending || t.reconciling
  then invalid "developer root action is already active or unresolved"
  else (
    let finished, finish = Eio.Promise.create () in
    let call = { cancel = None; finished; finish } in
    t.root_call <- Some call;
    Exn.protect
      ~finally:(fun () ->
        Option.iter t.active ~f:(fun action ->
          if phys_equal action.call call
          then (
            action.live <- false;
            if Option.is_none t.pending then abort_staged action));
        call.cancel <- None;
        t.active <- None;
        t.root_call <- None;
        Eio.Promise.resolve finish ())
      ~f:(fun () ->
        Eio.Cancel.sub (fun cancel ->
          call.cancel <- Some cancel;
          Eio.Fiber.with_binding root_call_key t (fun () ->
            let* snapshot = current t in
            let action =
              { issuer = t
              ; snapshot
              ; staged = Hashtbl.create (module P.Id.Job)
              ; stage_in_flight = 0
              ; sealed = None
              ; live = true
              ; call
              }
            in
            t.active <- Some action;
            let* value, (selection : selection) =
              Eio.Switch.run (fun sw ->
                Eio.Fiber.with_binding action_key action (fun () -> f ~action ~sw))
            in
            if
              (not (phys_equal selection.issuer action))
              || (not
                    (Option.value_map
                       action.sealed
                       ~default:false
                       ~f:(phys_equal selection)))
              || action.stage_in_flight > 0
            then invalid "developer root action returned a foreign selection"
            else
              let* () = check_effect_now t snapshot in
              let* () =
                match phase, selection.jobs with
                | Store.Checkpoint.Active, _ | (Interrupted | Closed), [] -> Ok ()
                | (Interrupted | Closed), _ :: _ ->
                  invalid "closed developer epoch cannot select queued jobs"
              in
              Result.map (selected_transaction t action selection ~phase) ~f:(fun () ->
                value)))))
;;

let close_owner t =
  if not (same_domain t.registry)
  then invalid "developer root owner belongs to another domain"
  else if
    Option.value_map (Eio.Fiber.get root_call_key) ~default:false ~f:(fun current ->
      phys_equal current t)
  then invalid "developer root action cannot close its own owner"
  else (
    match t.state with
    | Closed -> Ok ()
    | Live | Closing | Quarantined ->
      t.state <- Closing;
      let root_call = t.root_call in
      Eio.Cancel.protect (fun () ->
        Option.iter root_call ~f:(fun call ->
          Option.iter call.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit);
          Eio.Promise.await call.finished));
      let pending_result =
        match reconcile_pending t with
        | value -> `Returned value
        | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      let worker_result =
        match
          Eio.Cancel.protect (fun () -> Developer_job_worker.close_and_wait t.worker)
        with
        | value -> `Returned value
        | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      let* () =
        match pending_result, worker_result with
        | `Returned (Ok ()), `Returned (Ok ()) -> Ok ()
        | `Raised (exn, backtrace), _ | _, `Raised (exn, backtrace) ->
          Exn.raise_with_original_backtrace exn backtrace
        | `Returned (Error error), _ | _, `Returned (Error error) -> Error error
      in
      Job_capacity.close_developer_run t.registry.capacity ~owner:t.owner;
      let* () =
        Store.Run_claim.release t.run_claim |> Result.map_error ~f:error_of_store
      in
      t.state <- Closed;
      (match Hashtbl.find t.registry.owners t.owner.run_id with
       | Some (Owned current) when phys_equal current t ->
         Hashtbl.remove t.registry.owners t.owner.run_id
       | None | Some (Admitting _) | Some (Owned _) -> ());
      Ok ())
;;

let close_and_wait = close_owner

module Registry = struct
  type nonrec t = registry

  let cleanup_admission t run_id (admission : admission) =
    let* () =
      match admission.worker with
      | None -> Ok ()
      | Some worker ->
        let* () = Developer_job_worker.close_and_wait worker in
        admission.worker <- None;
        Ok ()
    in
    let* () =
      match admission.claim with
      | None -> Ok ()
      | Some claim ->
        let* () = Store.Run_claim.release claim |> Result.map_error ~f:error_of_store in
        admission.claim <- None;
        Ok ()
    in
    (match Hashtbl.find t.owners run_id with
     | Some (Admitting current) when phys_equal current admission ->
       Hashtbl.remove t.owners run_id
     | None | Some (Admitting _) | Some (Owned _) -> ());
    Ok ()
  ;;

  let close_and_wait t =
    if not (same_domain t)
    then invalid "developer root registry belongs to another domain"
    else if
      Option.value_map (Eio.Fiber.get admission_key) ~default:false ~f:(fun current ->
        phys_equal current t)
      || Option.value_map (Eio.Fiber.get root_call_key) ~default:false ~f:(fun current ->
        phys_equal current.registry t)
    then invalid "developer root admission cannot close its own registry"
    else (
      t.closed <- true;
      let admissions =
        Hashtbl.data t.owners
        |> List.filter_map ~f:(function
          | Admitting admission -> Some admission.call
          | Owned _ -> None)
      in
      Eio.Cancel.protect (fun () ->
        List.iter admissions ~f:(fun call ->
          Option.iter call.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit);
          Eio.Promise.await call.finished));
      let owners =
        Hashtbl.data t.owners
        |> List.filter_map ~f:(function
          | Admitting _ -> None
          | Owned owner -> Some owner)
      in
      let abandoned_admissions =
        Hashtbl.to_alist t.owners
        |> List.filter_map ~f:(fun (run_id, slot) ->
          match slot with
          | Admitting admission -> Some (run_id, admission)
          | Owned _ -> None)
      in
      let first_error = ref None in
      let first_exception = ref None in
      Eio.Cancel.protect (fun () ->
        List.iter abandoned_admissions ~f:(fun (run_id, admission) ->
          match cleanup_admission t run_id admission with
          | Ok () -> ()
          | Error error -> if Option.is_none !first_error then first_error := Some error
          | exception exn ->
            if Option.is_none !first_exception
            then first_exception := Some (exn, Stdlib.Printexc.get_raw_backtrace ()));
        List.iter owners ~f:(fun owner ->
          match close_owner owner with
          | Ok () -> ()
          | Error error -> if Option.is_none !first_error then first_error := Some error
          | exception exn ->
            if Option.is_none !first_exception
            then first_exception := Some (exn, Stdlib.Printexc.get_raw_backtrace ())));
      match !first_exception, !first_error with
      | Some (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace
      | None, Some error -> Error error
      | None, None -> Ok ())
  ;;

  let create ~sw ~store ~capacity ~now ~limits =
    let t =
      { store
      ; capacity
      ; now
      ; limits
      ; switch = sw
      ; domain_id = (Stdlib.Domain.self () :> int)
      ; owners = Hashtbl.create (module P.Id.Developer_run)
      ; closed = false
      }
    in
    Eio.Switch.check sw;
    (* The Store hook was registered when the caller opened it. Eio runs
       release hooks in reverse registration order, so this joins root actions
       and releases run claims before the Store hook. Explicit host shutdown
       still reports any unresolved quarantine to its caller. *)
    Eio.Switch.on_release sw (fun () ->
      match close_and_wait t with
      | Ok () -> ()
      | Error _ -> failwith "developer root registry shutdown is incomplete");
    t
  ;;

  let admit
        t
        ~actor
        ~capture
        ~registry
        ~selected
        ~job_policy
        ~handle
        ~workspace_conflict_domain
    =
    if t.closed || not (same_domain t)
    then invalid "developer root registry is closed or foreign"
    else (
      let owner = Store.owner handle in
      let run_id = owner.run_id in
      if
        Hashtbl.mem t.owners run_id || Hashtbl.length t.owners >= t.limits.max_active_runs
      then invalid "developer run owner slot is unavailable"
      else (
        let finished, finish = Eio.Promise.create () in
        let call = { cancel = None; finished; finish } in
        let admission = { call; claim = None; worker = None } in
        Hashtbl.set t.owners ~key:run_id ~data:(Admitting admission);
        let retained = ref false in
        let perform () =
          Eio.Cancel.sub (fun cancel ->
            call.cancel <- Some cancel;
            Eio.Fiber.with_binding admission_key t (fun () ->
              let* token =
                Store.Run_claim.acquire ~store:t.store ~handle
                |> Result.map_error ~f:error_of_store
              in
              admission.claim <- Some token;
              let principal = Operator_authorization.principal actor in
              let* () =
                if
                  P.Id.Principal.equal principal.id owner.principal_id
                  && Operator_authorization.is_current actor
                  && Chatml_source.Digest.equal
                       (Chatml_developer_capture.fingerprint capture)
                       (Store.capture_fingerprint handle)
                  && not (String.is_empty workspace_conflict_domain)
                then Ok ()
                else invalid "developer root actor or capture does not match"
              in
              let* () = selected_within ~registry selected in
              let project_source = project_source capture in
              let* worker =
                Developer_job_worker.create
                  ~close_policy:Retain_unclaimed
                  ~sw:t.switch
                  ~store:t.store
                  ~initial:handle
                  ~capacity:t.capacity
                  ~project_source
                  ~workspace_conflict_domain
                  ~now:t.now
                  ~max_active:t.limits.max_active_workers
                  ~max_quarantined:t.limits.max_quarantined_workers
                  ()
              in
              admission.worker <- Some worker;
              let* active =
                Store.is_latest_active handle |> Result.map_error ~f:error_of_store
              in
              let* () =
                if active && (not t.closed) && Operator_authorization.is_current actor
                then Ok ()
                else invalid "developer root admission expired"
              in
              let run =
                { registry = t
                ; actor
                ; capture
                ; owner
                ; handle
                ; run_claim = token
                ; worker
                ; project_source
                ; workspace_conflict_domain
                ; job_policy
                ; selected
                ; selection_revision = 0
                ; state = Live
                ; root_call = None
                ; active = None
                ; pending = None
                ; reconciling = false
                }
              in
              Hashtbl.set t.owners ~key:run_id ~data:(Owned run);
              retained := true;
              Ok run))
        in
        let outcome =
          match perform () with
          | value -> `Returned value
          | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
        in
        let cleanup =
          if !retained
          then `Returned (Ok ())
          else (
            match Eio.Cancel.protect (fun () -> cleanup_admission t run_id admission) with
            | value -> `Returned value
            | exception exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
        in
        call.cancel <- None;
        Eio.Promise.resolve finish ();
        match outcome, cleanup with
        | `Returned (Ok run), _ -> Ok run
        | `Returned (Error original), `Returned (Ok ()) -> Error original
        | `Returned (Error _), `Returned (Error cleanup) -> Error cleanup
        | `Returned (Error _), `Raised (exn, backtrace) ->
          Exn.raise_with_original_backtrace exn backtrace
        | `Raised (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace))
  ;;
end
