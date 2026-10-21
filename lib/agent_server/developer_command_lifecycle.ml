open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Receipt = Store.Command.Receipt
module Row = Store.Command.Row
module Root = Developer_root_owner
module Startup = Developer_command_startup
module Custody = Developer_command_store_custody
module Fallback = Developer_command_fallback

let error code message ~retryable = P.Error.create code ~message ~retryable ()
let busy () = error P.Error.Conflict "developer command is busy" ~retryable:true

let stale () =
  error P.Error.Lease_stale "developer command owner changed" ~retryable:false
;;

let denied () =
  error P.Error.Permission_denied "developer command denied" ~retryable:false
;;

let closed () =
  error P.Error.Invalid_state "developer command owner is closed" ~retryable:false
;;

let limited () =
  error P.Error.Resource_limit "developer command limit reached" ~retryable:false
;;

let unavailable () =
  error P.Error.Persistence_error "developer command unavailable" ~retryable:true
;;

let store_error = function
  | Store.Error.Conflict | Prewrite_conflict -> busy ()
  | Foreign_or_stale -> stale ()
  | Limit_exceeded -> limited ()
  | Closed -> closed ()
  | Store _ -> unavailable ()
;;

let store result = Result.map_error result ~f:store_error

module Disclosure_policy = struct
  type t = { authorize : Operator_authorization.t -> Row.t -> bool }

  let create ~authorize = { authorize }
end

module Control_policy = struct
  type t = { authorize : Operator_authorization.t -> Row.t -> bool }

  let create ~authorize = { authorize }
end

module Recovery_adapter = struct
  type t =
    { recover :
        claim:Store.Run_claim.t
        -> handle:Store.handle
        -> window:Store.Recovery.t
        -> (unit, P.Error.t) result
    }

  let create ~recover = { recover }
end

type phase =
  | Cold
  | Recovering
  | Ready
  | Quarantined

type t =
  { sw : Eio.Switch.t
  ; store : Store.t
  ; domain : int
  ; max_runs : int
  ; max_active_commands : int
  ; fallbacks : Fallback.t
  ; disclosure : Disclosure_policy.t
  ; control : Control_policy.t
  ; recovery_adapter : Recovery_adapter.t
  ; on_committed : (Row.t -> unit) option
  ; on_joined_terminal : (Row.t -> unit) option
  ; runs : (P.Id.Developer_run.t, run_entry) Hashtbl.t
  ; mutable occupied : int
  ; mutable closed : bool
  }

and run_entry =
  { issuer : t
  ; run_id : P.Id.Developer_run.t
  ; mutation_gate : Eio.Mutex.t
  ; mutable phase : phase
  ; mutable slot : slot option
  ; mutable active : active list
  ; mutable pending : Custody.t option
  ; mutable unlaunched : unlaunched option
  ; mutable recovery : Startup.Acquired.t option
  ; mutable claim_only : Store.Run_claim.t option
  ; mutable finished : Startup.Acquired.t option
  ; mutable transferred_claim : (Store.handle * Store.Run_claim.t) option
  ; mutable persisted : bool
  ; mutable retained_charge : bool
  }

and slot =
  { issuer : t
  ; run : run_entry
  ; mutable live : bool
  ; mutable transferred : bool
  ; borrowed_charge : bool
  ; mutable unlaunched : unlaunched option
  }

and unlaunched =
  { handle : Store.handle
  ; view : view
  ; receipt : Receipt.t
  }

and view =
  { issuer : t
  ; run_id : P.Id.Developer_run.t
  ; row : Row.t
  ; store_view : Store.Command.View.t option
  ; launch_slot : slot option
  ; mutable launch_consumed : bool
  }

and active =
  { issuer : t
  ; run : run_entry
  ; handle : Store.handle
  ; mutable view : view
  ; generation : unit ref
  ; finished : unit Eio.Promise.t
  ; finish : unit Eio.Promise.u
  ; mutable cancel : Eio.Cancel.t option
  ; mutable cancel_requested : bool
  ; mutable live : bool
  ; mutable retired : bool
  ; mutable selected : Receipt.t option
  ; mutable terminal : (view, P.Error.t) result option
  ; mutable private_failure : (exn * Stdlib.Printexc.raw_backtrace) option
  ; mutable started : bool
  ; mutable joined_notified : bool
  }

and started =
  { active : active
  ; root : Root.run
  ; generation : unit ref
  ; actor : started_actor
  ; mutable consumed : bool
  }

and started_actor =
  | Live_actor
  | Repl_task_actor of Operator_authorization.t

and preparing =
  { active : active
  ; actor : Operator_authorization.t
  ; generation : unit ref
  ; mutable binding_attempted : bool
  }

let active_key : active Eio.Fiber.key = Eio.Fiber.create_key ()

module Slot = struct
  type nonrec t = slot
end

module View = struct
  type nonrec t = view

  let row t = t.row

  let receipt t =
    match Row.phase t.row with
    | Row.Phase.Terminal { receipt; _ } -> Some receipt
    | Reserved | Waiting_permission | Execution_started -> None
  ;;
end

let on_domain t = Int.equal t.domain (Stdlib.Domain.self () :> int)
let current_actor actor = Operator_authorization.is_current actor

let same_principal actor row =
  P.Id.Principal.equal
    (Operator_authorization.principal actor).id
    (Row.owner row).principal_id
;;

let same_run_view t run_id (view : view) =
  phys_equal view.issuer t && P.Id.Developer_run.equal view.run_id run_id
;;

let same_row a b = Document_schema.Json.equal (Row.to_json a) (Row.to_json b)

let view_of_store t run_id (value : Store.Command.View.t) =
  { issuer = t
  ; run_id
  ; row = Store.Command.View.row value
  ; store_view = Some value
  ; launch_slot = None
  ; launch_consumed = true
  }
;;

let fresh_reservation_view t slot value =
  { (view_of_store t slot.run.run_id value) with
    launch_slot = Some slot
  ; launch_consumed = false
  }
;;

let safe_view t run_id row =
  { issuer = t
  ; run_id
  ; row
  ; store_view = None
  ; launch_slot = None
  ; launch_consumed = true
  }
;;

let create_with_hook
      ~on_committed
      ~on_joined_terminal
      ~sw
      ~store:run_store
      ~max_active_commands
      ~disclosure
      ~control
      ~recovery
  =
  let open Result.Let_syntax in
  let%bind limits = store (Store.limits run_store) in
  let max_runs = Store.Limits.max_runs limits in
  let max_per_run =
    Agent_store.Developer_command_limits.max_commands (Store.Limits.commands limits)
  in
  if
    max_active_commands <= 0
    || max_runs <= 0
    || max_per_run <= 0
    || max_active_commands / max_runs > max_per_run
    || (max_active_commands / max_runs = max_per_run && max_active_commands % max_runs > 0)
    || Option.is_some (Eio.Switch.get_error sw)
  then Error (limited ())
  else (
    let%map fallbacks = Fallback.create limits in
    { sw
    ; store = run_store
    ; domain = (Stdlib.Domain.self () :> int)
    ; max_runs
    ; max_active_commands
    ; fallbacks
    ; disclosure
    ; control
    ; recovery_adapter = recovery
    ; on_committed
    ; on_joined_terminal
    ; runs = Hashtbl.create (module P.Id.Developer_run)
    ; occupied = 0
    ; closed = false
    })
;;

let create ~sw ~store ~max_active_commands ~disclosure ~control ~recovery =
  create_with_hook
    ~on_committed:None
    ~on_joined_terminal:None
    ~sw
    ~store
    ~max_active_commands
    ~disclosure
    ~control
    ~recovery
;;

let notify_committed t row = Option.iter t.on_committed ~f:(fun notify -> notify row)

let entry t run_id =
  match Hashtbl.find t.runs run_id with
  | Some entry -> Ok entry
  | None ->
    if Hashtbl.length t.runs >= t.max_runs
    then Error (limited ())
    else (
      let entry =
        { issuer = t
        ; run_id
        ; mutation_gate = Eio.Mutex.create ()
        ; phase = Cold
        ; slot = None
        ; active = []
        ; pending = None
        ; unlaunched = None
        ; recovery = None
        ; claim_only = None
        ; finished = None
        ; transferred_claim = None
        ; persisted = false
        ; retained_charge = false
        }
      in
      Hashtbl.set t.runs ~key:run_id ~data:entry;
      Ok entry)
;;

let slot_current t (slot : slot) =
  on_domain t
  && (not t.closed)
  && phys_equal slot.issuer t
  && slot.live
  && (not slot.transferred)
  && Option.value_map slot.run.slot ~default:false ~f:(phys_equal slot)
;;

let with_mutation run f =
  Eio.Mutex.lock run.mutation_gate;
  Exn.protect ~f ~finally:(fun () ->
    Eio.Cancel.protect (fun () -> Eio.Mutex.unlock run.mutation_gate))
;;

let update_active_view run (value : Store.Command.View.t) =
  let row = Store.Command.View.row value in
  List.iter run.active ~f:(fun active ->
    if P.Idempotency_key.equal (Row.key active.view.row) (Row.key row)
    then active.view <- view_of_store run.issuer run.run_id value)
;;

let reconcile_retained_unlocked run =
  match run.pending with
  | None -> Ok ()
  | Some pending ->
    let open Result.Let_syntax in
    let result = store (Custody.reconcile pending) in
    if Result.is_error result then run.phase <- Quarantined;
    let%bind resolution = result in
    (match resolution with
     | Custody.Installed view ->
       run.pending <- None;
       update_active_view run view;
       if Option.is_none run.recovery then run.phase <- Ready;
       Ok ()
     | Retired_absent ->
       run.pending <- None;
       (match Custody.kind pending with
        | Reserve -> run.unlaunched <- None
        | Start | Finish -> ());
       if Option.is_none run.recovery then run.phase <- Ready;
       Ok ())
;;

let reconcile_retained run =
  let result = with_mutation run (fun () -> reconcile_retained_unlocked run) in
  if
    Result.is_ok result
    && run.retained_charge
    && Option.is_none run.slot
    && List.is_empty run.active
    && Option.is_none run.pending
    && Option.is_none run.unlaunched
    && Option.is_none run.recovery
    && Option.is_none run.claim_only
  then (
    run.retained_charge <- false;
    run.issuer.occupied <- run.issuer.occupied - 1);
  result
;;

let best_effort_reconcile_unlocked run =
  try
    Eio.Cancel.protect (fun () ->
      ignore (reconcile_retained_unlocked run : (unit, P.Error.t) result))
  with
  | _ -> ()
;;

let read_snapshot t run_id ~principal ~key =
  let open Result.Let_syntax in
  let%bind root = store (Store.Result_root.find_existing t.store run_id) in
  match root with
  | None -> Ok None
  | Some root -> store (Store.Result_root.find_command_snapshot root ~principal ~key)
;;

let disclose_snapshot t run_id ~actor row =
  if not (current_actor actor)
  then Error (denied ())
  else (
    match row with
    | None -> Ok None
    | Some row ->
      if
        same_principal actor row
        && t.disclosure.authorize actor row
        && current_actor actor
      then Ok (Some (safe_view t run_id row))
      else Error (denied ()))
;;

let snapshot t run_id ~actor ~key =
  let open Result.Let_syntax in
  if not (current_actor actor)
  then Error (denied ())
  else (
    let principal = (Operator_authorization.principal actor).id in
    let%bind row = read_snapshot t run_id ~principal ~key in
    disclose_snapshot t run_id ~actor row)
;;

(* Only Store I/O runs under the mutation gate. Host currentness/disclosure
   callbacks remain outside it, including when they reject the resulting row. *)
let observe t ~actor ~run_id ~key =
  let open Result.Let_syntax in
  if not (on_domain t)
  then Error (stale ())
  else if t.closed
  then Error (closed ())
  else if not (current_actor actor)
  then Error (denied ())
  else (
    let principal = (Operator_authorization.principal actor).id in
    let read () =
      if t.closed then Error (closed ()) else read_snapshot t run_id ~principal ~key
    in
    let%bind row =
      match Hashtbl.find t.runs run_id with
      | None -> read ()
      | Some run -> with_mutation run read
    in
    if t.closed
    then Error (closed ())
    else (
      let%bind view = disclose_snapshot t run_id ~actor row in
      if t.closed then Error (closed ()) else Ok view))
;;

let release_clean_claim run claim =
  match store (Store.Run_claim.release claim) with
  | Ok () ->
    run.claim_only <- None;
    Ok ()
  | Error _ as error ->
    run.claim_only <- Some claim;
    run.phase <- Quarantined;
    error
;;

let finish_recovery run acquired =
  let open Result.Let_syntax in
  let outcome =
    Startup.finish
      acquired
      ~recover:run.issuer.recovery_adapter.recover
      ~map_store_error:store_error
  in
  match outcome with
  | Ok () ->
    run.recovery <- None;
    run.claim_only <- None;
    run.finished <- Some acquired;
    run.phase <- Ready;
    Ok ()
  | Error _ as error ->
    run.phase <- Quarantined;
    error
;;

let ensure_startup_recovered t (slot : slot) =
  let open Result.Let_syntax in
  if not (slot_current t slot)
  then Error (stale ())
  else (
    let run = slot.run in
    match run.phase, run.recovery with
    | Ready, None -> Ok ()
    | Quarantined, None when Option.is_some run.finished -> Error (busy ())
    | Quarantined, Some acquired -> finish_recovery run acquired
    | Recovering, _ -> Error (busy ())
    | (Cold | Quarantined), None ->
      let%bind () = reconcile_retained run in
      let%bind () =
        match run.claim_only with
        | None -> Ok ()
        | Some claim -> release_clean_claim run claim
      in
      run.phase <- Recovering;
      (try
         let outcome =
           match
             Startup.open_existing t.store ~run_id:run.run_id ~on_claim:(fun claim ->
               run.claim_only <- Some claim)
           with
           | Error error ->
             let original = store_error error in
             (match run.claim_only with
              | None -> ()
              | Some claim ->
                (try
                   ignore (release_clean_claim run claim : (unit, P.Error.t) result)
                 with
                 | _ ->
                   run.claim_only <- Some claim;
                   run.phase <- Quarantined));
             Error original
           | Ok None ->
             run.phase <- Ready;
             Ok ()
           | Ok (Some acquired) ->
             run.persisted <- true;
             run.recovery <- Some acquired;
             run.claim_only <- None;
             finish_recovery run acquired
         in
         match outcome with
         | Ok () -> Ok ()
         | Error _ as error ->
           if Option.is_none run.recovery && Option.is_none run.claim_only
           then run.phase <- Cold;
           error
       with
       | exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         if Option.is_none run.recovery && Option.is_none run.claim_only
         then run.phase <- Cold
         else run.phase <- Quarantined;
         Stdlib.Printexc.raise_with_backtrace exn backtrace)
    | (Ready | Cold), Some _ -> Error (busy ()))
;;

let ready_epoch t (slot : slot) =
  if not (slot_current t slot)
  then Error (stale ())
  else (
    match slot.run.phase, slot.run.recovery, slot.run.claim_only with
    | Ready, None, None ->
      (match slot.run.finished with
       | None -> Ok None
       | Some acquired ->
         let handle = Startup.Acquired.handle acquired in
         if
           Store.Recovery.issued_finished
             (Startup.Acquired.window acquired)
             ~claim:(Startup.Acquired.claim acquired)
             ~handle
         then Ok (Some handle)
         else Error (busy ()))
    | Cold, None, None -> Ok None
    | (Ready | Cold | Recovering | Quarantined), _, _ -> Error (busy ()))
;;

let release_finished_epoch t (slot : slot) =
  if not (slot_current t slot)
  then Error (stale ())
  else (
    let run = slot.run in
    if
      (not (List.is_empty run.active))
      || Option.is_some run.pending
      || Option.is_some run.unlaunched
      || Option.is_some run.recovery
      || Option.is_some run.claim_only
      || Option.is_some run.transferred_claim
    then Error (busy ())
    else (
      match run.finished with
      | None -> Ok ()
      | Some acquired ->
        (match store (Store.Run_claim.release (Startup.Acquired.claim acquired)) with
         | Ok () ->
           run.finished <- None;
           Ok ()
         | Error _ as error ->
           run.phase <- Quarantined;
           error)))
;;

let confirm_transferred_retired t (slot : slot) ~handle =
  if (not (slot_current t slot)) || not (Store.issued_by t.store handle)
  then Error (stale ())
  else (
    let run = slot.run in
    if
      (not (List.is_empty run.active))
      || Option.is_some run.pending
      || Option.is_some run.unlaunched
      || Option.is_some run.recovery
      || Option.is_some run.claim_only
    then Error (busy ())
    else (
      match run.transferred_claim with
      | Some (issued, claim)
        when phys_equal issued handle && Store.Run_claim.is_released claim ->
        run.transferred_claim <- None;
        Ok ()
      | None -> Ok ()
      | Some _ -> Error (busy ())))
;;

let ensure_admitted_claim t (slot : slot) ~handle =
  let open Result.Let_syntax in
  if (not (slot_current t slot)) || not (Store.issued_by t.store handle)
  then Error (stale ())
  else (
    let run = slot.run in
    if not (P.Id.Developer_run.equal run.run_id (Store.owner handle).run_id)
    then Error (stale ())
    else (
      match run.finished, run.transferred_claim with
      | Some acquired, None ->
        if
          phys_equal (Startup.Acquired.handle acquired) handle
          && Store.Recovery.issued_finished
               (Startup.Acquired.window acquired)
               ~claim:(Startup.Acquired.claim acquired)
               ~handle
        then Ok ()
        else Error (busy ())
      | None, Some (issued, claim) ->
        if phys_equal issued handle && not (Store.Run_claim.is_released claim)
        then Ok ()
        else Error (busy ())
      | Some _, Some _ -> Error (busy ())
      | None, None ->
        if
          (not (List.is_empty run.active))
          || Option.is_some run.pending
          || Option.is_some run.unlaunched
        then Error (busy ())
        else (
          try
            let%bind () =
              match run.claim_only with
              | None -> Ok ()
              | Some claim -> release_clean_claim run claim
            in
            match run.recovery with
            | Some acquired ->
              if phys_equal (Startup.Acquired.handle acquired) handle
              then finish_recovery run acquired
              else Error (busy ())
            | None ->
              run.phase <- Recovering;
              (match
                 Startup.open_admitted t.store ~handle ~on_claim:(fun claim ->
                   run.claim_only <- Some claim)
               with
               | Error error ->
                 let original = store_error error in
                 (match run.claim_only with
                  | None -> run.phase <- Ready
                  | Some claim ->
                    (match release_clean_claim run claim with
                     | Ok () -> run.phase <- Ready
                     | Error _ -> run.phase <- Quarantined));
                 Error original
               | Ok acquired ->
                 run.recovery <- Some acquired;
                 run.claim_only <- None;
                 run.persisted <- true;
                 finish_recovery run acquired)
          with
          | exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            run.phase
            <- (if Option.is_none run.recovery && Option.is_none run.claim_only
                then Ready
                else Quarantined);
            Stdlib.Printexc.raise_with_backtrace exn backtrace)))
;;

let active_current (active : active) =
  on_domain active.issuer
  && active.live
  && (not active.issuer.closed)
  && Option.value_map (Eio.Fiber.get active_key) ~default:false ~f:(phys_equal active)
  && List.exists active.run.active ~f:(phys_equal active)
;;

let same_active_root (active : active) root =
  Root.issued_by_store root active.issuer.store
  && P.Execution_owner.equal_developer (Root.owner root) (Store.owner active.handle)
  && Chatml_source.Digest.equal
       (Root.Source.fingerprint (Root.source root))
       (Store.capture_fingerprint active.handle)
  && current_actor (Root.original_actor root)
;;

module Active = struct
  type nonrec t = active

  module Started = struct
    type nonrec t = started

    let issued_by_root (token : t) root =
      active_current token.active
      && phys_equal token.generation token.active.generation
      && phys_equal token.root root
      && same_active_root token.active root
    ;;

    let consume_for token ~root =
      if not (issued_by_root token root)
      then Error (stale ())
      else if token.consumed
      then Error (busy ())
      else (
        token.consumed <- true;
        Ok ())
    ;;
  end

  module Preparing = struct
    type nonrec t = preparing

    let issued_started (t : t) (started : Started.t) =
      active_current t.active
      && phys_equal t.active started.active
      && phys_equal t.generation started.generation
      && t.binding_attempted
      && Started.issued_by_root started started.root
    ;;

    let bind_root t ~root =
      if t.binding_attempted
      then Error (busy ())
      else (
        t.binding_attempted <- true;
        let same_now () =
          active_current t.active
          && phys_equal t.generation t.active.generation
          && Operator_authorization.is_current t.actor
          && phys_equal t.actor (Root.original_actor root)
          && same_active_root t.active root
        in
        if not (same_now ())
        then Error (stale ())
        else
          let open Result.Let_syntax in
          let%bind current = Root.current root in
          let%bind () = Root.check_effect_now root current in
          if same_now ()
          then
            Ok
              { active = t.active
              ; root
              ; generation = t.generation
              ; actor = Live_actor
              ; consumed = false
              }
          else Error (stale ()))
    ;;

    let bind_repl_root t ~root ~borrowed ~checkpoint =
      if t.binding_attempted
      then Error (busy ())
      else (
        t.binding_attempted <- true;
        let actor = Developer_repl_epoch.current_actor borrowed in
        let original = Developer_repl_epoch.original_actor borrowed in
        let selection = Developer_repl_epoch.source_selection borrowed in
        let same_now () =
          active_current t.active
          && phys_equal t.generation t.active.generation
          && phys_equal t.actor actor
          && phys_equal original (Root.original_actor root)
          && same_active_root t.active root
          && (match Root.source root with
              | Root.Source.Repl source -> phys_equal source selection
              | Command _ -> false)
          && Result.is_ok
               (Developer_method_authorization.check_repl_task_now
                  ~original
                  ~current:actor)
        in
        if not (same_now ())
        then Error (stale ())
        else
          let open Result.Let_syntax in
          let%bind checked =
            Developer_capture_host.Source_selection.Shared.check_current
              selection
              ~actor
              ~checkpoint
          in
          let%bind current = Root.current root in
          let%bind () = Root.check_effect_now root current in
          let%bind () = Developer_repl_epoch.check_now borrowed in
          let%bind () =
            Developer_capture_host.Source_selection.Shared.check_now
              selection
              ~checked
              ~actor
          in
          if same_now ()
          then
            Ok
              { active = t.active
              ; root
              ; generation = t.generation
              ; actor = Repl_task_actor actor
              ; consumed = false
              }
          else Error (stale ()))
    ;;
  end

  let view (t : t) = t.view

  let check_started_now (active : t) ~(started : Started.t) ~root ~actor =
    if
      active_current active
      && phys_equal active started.active
      && Started.issued_by_root started root
      &&
      match started.actor with
      | Live_actor ->
        phys_equal actor (Root.original_actor root)
        && Operator_authorization.is_current actor
      | Repl_task_actor issued ->
        phys_equal actor issued
        && Result.is_ok
             (Developer_method_authorization.check_repl_task_now
                ~original:(Root.original_actor root)
                ~current:actor)
    then Ok ()
    else Error (stale ())
  ;;

  let start_preparation t ~actor ~admit =
    let open Result.Let_syntax in
    if
      (not (active_current t))
      || (not (Operator_authorization.is_current actor))
      || not (same_principal actor t.view.row)
    then Error (stale ())
    else if
      t.started
      || not
           (match Row.kind t.view.row with
            | Live_run | Repl_task_run -> true
            | Check | Fixture_run -> false)
    then Error (busy ())
    else (
      let committed = ref None in
      let outcome =
        with_mutation t.run (fun () ->
          let%bind () = reconcile_retained_unlocked t.run in
          match t.view.store_view with
          | None -> Error (stale ())
          | Some previous ->
            let%bind prepared =
              store
                (Store.Command.prepare_change
                   t.handle
                   ~previous
                   ~change:Row.Change.Start_execution)
            in
            (match prepared with
             | Store.Command.Unchanged _ -> Error (busy ())
             | Prepared token ->
               let pending =
                 Custody.create ~handle:t.handle ~token ~kind:Start ~admit:(fun () ->
                   active_current t
                   && Operator_authorization.is_current actor
                   && same_principal actor t.view.row
                   && admit ())
               in
               t.run.pending <- Some pending;
               (match store (Custody.commit pending ~remaining:3) with
                | Error _ as error -> error
                | Ok installed ->
                  t.run.pending <- None;
                  t.view <- view_of_store t.issuer t.run.run_id installed.view;
                  committed := Some t.view.row;
                  (match installed.execution_start with
                   | None -> Error (busy ())
                   | Some acknowledged ->
                     let%bind () =
                       store (Store.Command.Execution_start.consume acknowledged)
                     in
                     t.started <- true;
                     Ok
                       { active = t
                       ; actor
                       ; generation = t.generation
                       ; binding_attempted = false
                       }))))
      in
      Option.iter !committed ~f:(notify_committed t.issuer);
      outcome)
  ;;

  let start_live t ~root ~admit =
    let open Result.Let_syntax in
    if not (active_current t && same_active_root t root)
    then Error (stale ())
    else (
      let%bind current = Root.current root in
      let%bind () = Root.check_effect_now root current in
      let%bind preparing =
        start_preparation t ~actor:(Root.original_actor root) ~admit:(fun () ->
          same_active_root t root
          && Result.is_ok (Root.check_effect_now root current)
          && admit ())
      in
      Preparing.bind_root preparing ~root)
  ;;

  let transfer_finished_claim t ~handle ~accept =
    if not (active_current t && phys_equal t.handle handle)
    then Error (stale ())
    else (
      match t.run.finished, t.run.transferred_claim with
      | Some acquired, None when phys_equal (Startup.Acquired.handle acquired) handle ->
        let claim = Startup.Acquired.claim acquired in
        let window = Startup.Acquired.window acquired in
        if not (Store.Recovery.issued_finished window ~claim ~handle)
        then Error (busy ())
        else (
          match accept ~claim ~window with
          | Error _ as error -> error
          | Ok () ->
            t.run.finished <- None;
            t.run.transferred_claim <- Some (handle, claim);
            Ok ())
      | None, _ | Some _, _ -> Error (busy ()))
  ;;
end

let reserve t (slot : slot) ~handle ~key ~kind ~request_fingerprint ~admit =
  if not (slot_current t slot)
  then Error (stale ())
  else if Option.is_some slot.unlaunched || Option.is_some slot.run.unlaunched
  then Error (busy ())
  else if
    (not (Store.issued_by t.store handle))
    || not (P.Id.Developer_run.equal (Store.owner handle).run_id slot.run.run_id)
  then Error (stale ())
  else (
    let committed = ref None in
    let outcome =
      with_mutation slot.run (fun () ->
        let open Result.Let_syntax in
        let%bind () = reconcile_retained_unlocked slot.run in
        if
          (not (slot_current t slot))
          ||
          match slot.run.phase with
          | Ready -> false
          | Cold | Recovering | Quarantined -> true
        then Error (stale ())
        else (
          let%bind prepared =
            store
              (Store.Command.prepare_reservation handle ~key ~kind ~request_fingerprint)
          in
          match prepared with
          | Store.Command.Unchanged value ->
            if slot_current t slot && admit ()
            then Ok (view_of_store t slot.run.run_id value)
            else Error (stale ())
          | Prepared token ->
            let receipt = Fallback.interrupted t.fallbacks kind in
            slot.run.unlaunched
            <- Some
                 { handle
                 ; view = safe_view t slot.run.run_id (Store.Command.Pending.after token)
                 ; receipt
                 };
            let pending =
              Custody.create ~handle ~token ~kind:Reserve ~admit:(fun () ->
                slot_current t slot && admit ())
            in
            slot.run.pending <- Some pending;
            (match store (Custody.commit pending ~remaining:3) with
             | Ok installed ->
               slot.run.pending <- None;
               slot.run.unlaunched <- None;
               slot.run.persisted <- true;
               let view = fresh_reservation_view t slot installed.view in
               slot.unlaunched <- Some { handle; view; receipt };
               committed := Some view.row;
               Ok view
             | Error _ as error ->
               best_effort_reconcile_unlocked slot.run;
               error
             | exception exn ->
               let backtrace = Stdlib.Printexc.get_raw_backtrace () in
               best_effort_reconcile_unlocked slot.run;
               Stdlib.Printexc.raise_with_backtrace exn backtrace)))
    in
    Option.iter !committed ~f:(notify_committed t);
    outcome)
;;

let conservative_receipt t (active : active) ~cancelled ~typed_failure =
  Ok (Fallback.choose t.fallbacks (Row.kind active.view.row) ~cancelled ~typed_failure)
;;

let receipt_for_outcome t (active : active) = function
  | Ok receipt when Receipt.Kind.equal (Receipt.kind receipt) (Row.kind active.view.row)
    -> Ok receipt
  | Ok _ | Error _ ->
    conservative_receipt t active ~cancelled:active.cancel_requested ~typed_failure:true
;;

let finish_terminal t ~run ~handle ~previous receipt =
  Eio.Cancel.protect (fun () ->
    let committed = ref None in
    let outcome =
      with_mutation run (fun () ->
        let open Result.Let_syntax in
        let%bind () = reconcile_retained_unlocked run in
        let key = Row.key previous.row in
        let rec attempt remaining =
          let%bind observed = store (Store.Command.observe handle ~key) in
          let%bind observed =
            match observed with
            | Some value -> Ok value
            | None -> Error (stale ())
          in
          let current_row = Store.Command.View.row observed in
          match Row.phase current_row with
          | Row.Phase.Terminal { receipt = installed; _ }
            when Row.same_identity current_row previous.row
                 && Receipt.equal installed receipt ->
            Ok (view_of_store t run.run_id observed)
          | Reserved | Waiting_permission | Execution_started ->
            if not (same_row current_row previous.row)
            then Error (stale ())
            else (
              match
                Store.Command.prepare_change
                  handle
                  ~previous:observed
                  ~change:(Row.Change.Finish receipt)
              with
              | Error Store.Error.Conflict when remaining > 0 -> attempt (remaining - 1)
              | Error error -> Error (store_error error)
              | Ok (Store.Command.Unchanged value) ->
                Ok (view_of_store t run.run_id value)
              | Ok (Prepared token) ->
                let pending =
                  Custody.create ~handle ~token ~kind:Finish ~admit:(fun () ->
                    on_domain t
                    && Store.issued_by t.store handle
                    && P.Execution_owner.equal_developer
                         (Row.owner previous.row)
                         (Store.owner handle))
                in
                run.pending <- Some pending;
                (match store (Custody.commit pending ~remaining:3) with
                 | Ok installed ->
                   run.pending <- None;
                   committed := Some (Store.Command.View.row installed.view);
                   Ok (view_of_store t run.run_id installed.view)
                 | Error _ as error ->
                   best_effort_reconcile_unlocked run;
                   error
                 | exception exn ->
                   let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                   best_effort_reconcile_unlocked run;
                   Stdlib.Printexc.raise_with_backtrace exn backtrace))
          | Terminal _ -> Error (busy ())
        in
        attempt 3)
    in
    Option.iter !committed ~f:(notify_committed t);
    outcome)
;;

let finish_active t (active : active) receipt =
  match
    finish_terminal t ~run:active.run ~handle:active.handle ~previous:active.view receipt
  with
  | Ok view ->
    active.view <- view;
    Ok view
  | Error _ as error -> error
;;

let settle_unlaunched t run (selected : unlaunched) =
  run.unlaunched <- Some selected;
  let outcome =
    let open Result.Let_syntax in
    let%bind () = reconcile_retained run in
    if Option.is_none run.unlaunched
    then Ok ()
    else (
      let%map _ =
        finish_terminal
          t
          ~run
          ~handle:selected.handle
          ~previous:selected.view
          selected.receipt
      in
      ())
  in
  match outcome with
  | Ok _ ->
    run.unlaunched <- None;
    if Option.is_none run.recovery && Option.is_none run.pending then run.phase <- Ready;
    Ok ()
  | Error _ as error ->
    run.phase <- Quarantined;
    error
;;

let with_slot t ~run_id ~f =
  if not (on_domain t)
  then Error (stale ())
  else if t.closed
  then Error (closed ())
  else
    let open Result.Let_syntax in
    let%bind run = entry t run_id in
    match run.slot with
    | Some _ -> Error (busy ())
    | None ->
      let borrowed_charge = run.retained_charge in
      if (not borrowed_charge) && t.occupied >= t.max_active_commands
      then Error (busy ())
      else (
        let slot =
          { issuer = t
          ; run
          ; live = true
          ; transferred = false
          ; borrowed_charge
          ; unlaunched = None
          }
        in
        run.slot <- Some slot;
        if not borrowed_charge then t.occupied <- t.occupied + 1;
        Exn.protect
          ~finally:(fun () ->
            Eio.Cancel.protect (fun () ->
              slot.live <- false;
              (match run.slot with
               | Some current when phys_equal current slot -> run.slot <- None
               | None | Some _ -> ());
              if not slot.transferred
              then
                if
                  Option.is_some run.pending
                  || Option.is_some run.unlaunched
                  || Option.is_some run.recovery
                  || Option.is_some run.claim_only
                then (
                  if List.is_empty run.active then run.retained_charge <- true;
                  if (not (List.is_empty run.active)) && not borrowed_charge
                  then t.occupied <- t.occupied - 1)
                else (
                  run.retained_charge <- false;
                  t.occupied <- t.occupied - 1);
              if
                (not run.persisted)
                && List.is_empty run.active
                && Option.is_none run.pending
                && Option.is_none run.unlaunched
                && Option.is_none run.recovery
                && Option.is_none run.claim_only
                && Option.is_none run.slot
              then Hashtbl.remove t.runs run_id))
          ~f:(fun () ->
            let outcome =
              try
                let prepared =
                  match run.unlaunched with
                  | None -> Ok ()
                  | Some selected -> settle_unlaunched t run selected
                in
                `Returned (Result.bind prepared ~f:(fun () -> f slot))
              with
              | exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
            in
            let cleanup =
              try
                Eio.Cancel.protect (fun () ->
                  match slot.unlaunched with
                  | None ->
                    (match run.unlaunched with
                     | None -> Ok ()
                     | Some selected -> settle_unlaunched t run selected)
                  | Some selected ->
                    slot.unlaunched <- None;
                    settle_unlaunched t run selected)
              with
              | _ ->
                run.phase <- Quarantined;
                Error (unavailable ())
            in
            match outcome with
            | `Returned (Ok value) -> Result.map cleanup ~f:(fun () -> value)
            | `Returned (Error error) -> Error error
            | `Raised (exn, backtrace) ->
              Stdlib.Printexc.raise_with_backtrace exn backtrace))
;;

let observation_ready (run : run_entry) =
  match run.phase with
  | Cold | Recovering | Quarantined -> false
  | Ready ->
    Option.is_none run.slot
    && (not run.retained_charge)
    && Option.is_none run.pending
    && Option.is_none run.unlaunched
    && Option.is_none run.recovery
    && Option.is_none run.claim_only
;;

let with_recovered_observation t ~run_id ~f =
  if not (on_domain t)
  then Error (stale ())
  else if t.closed
  then Error (closed ())
  else (
    match Hashtbl.find t.runs run_id with
    | Some run when observation_ready run ->
      (* A passive read must not reserve an admission slot merely to reread a
         live callback. Keep genuine admission/recovery custody on the original
         path; the callback still performs its fresh read and authorization. *)
      f ()
    | Some _ | None ->
      with_slot t ~run_id ~f:(fun slot ->
        let open Result.Let_syntax in
        let%bind () = ensure_startup_recovered t slot in
        f ()))
;;

let retire_active t (active : active) =
  if (not active.retired) && Option.is_none active.run.pending
  then (
    active.retired <- true;
    active.run.active
    <- List.filter active.run.active ~f:(fun item -> not (phys_equal item active));
    t.occupied <- t.occupied - 1)
;;

let notify_joined_terminal t (active : active) =
  if (not active.live) && not active.joined_notified
  then (
    match active.terminal with
    | Some (Ok view) ->
      (match Row.phase view.row with
       | Row.Phase.Terminal _ ->
         active.joined_notified <- true;
         (match t.on_joined_terminal with
          | None -> ()
          | Some notify ->
            (* Trace publication is observation only. It cannot strand the
               durable Active or mask its authoritative terminal result. *)
            (try notify view.row with
             | _ -> ()))
       | Reserved | Waiting_permission | Execution_started -> ())
    | None | Some (Error _) -> ())
;;

let launch t (slot : slot) ~handle ~reserved ~f =
  if
    (not (slot_current t slot))
    ||
    match slot.run.phase with
    | Ready -> false
    | Cold | Recovering | Quarantined -> true
  then Error (stale ())
  else if
    (not (Store.issued_by t.store handle))
    || (not (same_run_view t slot.run.run_id reserved))
    || reserved.launch_consumed
    || (not (Option.value_map reserved.launch_slot ~default:false ~f:(phys_equal slot)))
    || (not
          (Option.value_map slot.unlaunched ~default:false ~f:(fun selected ->
             phys_equal selected.view reserved && phys_equal selected.handle handle)))
    || (not
          (P.Execution_owner.equal_developer
             (Store.owner handle)
             (Row.owner reserved.row)))
    || not
         (Chatml_source.Digest.equal
            (Store.capture_fingerprint handle)
            (Row.capture_fingerprint reserved.row))
  then Error (stale ())
  else (
    match reserved.store_view, Row.phase reserved.row with
    | Some _, Row.Phase.Reserved ->
      (* The pinned Eio [Fiber.fork] silently skips a cancelled switch. Check
         before transferring this reservation into Active custody, with no
         yield before the fork itself. *)
      Eio.Switch.check t.sw;
      if
        Option.is_some slot.run.pending
        || List.exists slot.run.active ~f:(fun active ->
          P.Idempotency_key.equal (Row.key active.view.row) (Row.key reserved.row))
      then Error (busy ())
      else (
        let finished, finish = Eio.Promise.create () in
        let active =
          { issuer = t
          ; run = slot.run
          ; handle
          ; view = reserved
          ; generation = ref ()
          ; finished
          ; finish
          ; cancel = None
          ; cancel_requested = false
          ; live = true
          ; retired = false
          ; selected = None
          ; terminal = None
          ; private_failure = None
          ; started = false
          ; joined_notified = false
          }
        in
        slot.run.active <- active :: slot.run.active;
        reserved.launch_consumed <- true;
        slot.unlaunched <- None;
        if slot.borrowed_charge then slot.run.retained_charge <- false;
        slot.transferred <- true;
        match
          Eio.Fiber.fork ~sw:t.sw (fun () ->
            Exn.protect
              ~finally:(fun () ->
                active.live <- false;
                active.cancel <- None;
                notify_joined_terminal t active;
                (match active.terminal with
                 | Some (Ok _) -> retire_active t active
                 | None | Some (Error _) -> ());
                Eio.Promise.resolve active.finish ())
              ~f:(fun () ->
                let outcome =
                  try
                    Exn.protect
                      ~finally:(fun () -> active.cancel <- None)
                      ~f:(fun () ->
                        Eio.Cancel.sub (fun cancel ->
                          active.cancel <- Some cancel;
                          if active.cancel_requested then Eio.Cancel.cancel cancel Exit;
                          Eio.Switch.run (fun sw ->
                            Eio.Fiber.with_binding active_key active (fun () ->
                              f active ~sw))))
                  with
                  | exn ->
                    active.private_failure
                    <- Some (exn, Stdlib.Printexc.get_raw_backtrace ());
                    Error (unavailable ())
                in
                let terminal =
                  try
                    Eio.Cancel.protect (fun () ->
                      let open Result.Let_syntax in
                      let%bind receipt = receipt_for_outcome t active outcome in
                      active.selected <- Some receipt;
                      finish_active t active receipt)
                  with
                  | exn ->
                    active.private_failure
                    <- Some (exn, Stdlib.Printexc.get_raw_backtrace ());
                    Error (unavailable ())
                in
                active.terminal <- Some terminal))
        with
        | () -> Ok reserved
        | exception exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          active.live <- false;
          active.private_failure <- Some (exn, backtrace);
          let terminal =
            try
              Eio.Cancel.protect (fun () ->
                let open Result.Let_syntax in
                let%bind receipt =
                  conservative_receipt t active ~cancelled:false ~typed_failure:false
                in
                active.selected <- Some receipt;
                finish_active t active receipt)
            with
            | _ -> Error (unavailable ())
          in
          active.terminal <- Some terminal;
          notify_joined_terminal t active;
          (match terminal with
           | Ok _ -> retire_active t active
           | Error _ -> ());
          Eio.Promise.resolve active.finish ();
          Stdlib.Printexc.raise_with_backtrace exn backtrace)
    | Some _, (Waiting_permission | Execution_started | Terminal _) | None, _ ->
      Error (busy ()))
;;

let active_for_key t run_id key =
  match Hashtbl.find t.runs run_id with
  | None -> None
  | Some run ->
    List.find run.active ~f:(fun active ->
      P.Idempotency_key.equal (Row.key active.view.row) key)
;;

let terminal_view t ~actor ~run_id ~key =
  let open Result.Let_syntax in
  let%bind observed = observe t ~actor ~run_id ~key in
  match observed with
  | Some view when Option.is_some (View.receipt view) -> Ok view
  | None | Some _ -> Error (busy ())
;;

let await_terminal t ~actor ~run_id ~key =
  let open Result.Let_syntax in
  if not (on_domain t && current_actor actor)
  then Error (denied ())
  else (
    match active_for_key t run_id key with
    | Some active ->
      if Option.value_map (Eio.Fiber.get active_key) ~default:false ~f:(phys_equal active)
      then Error (busy ())
      else if
        not
          (same_principal actor active.view.row
           && t.disclosure.authorize actor active.view.row)
      then Error (denied ())
      else (
        Eio.Promise.await active.finished;
        if
          not
            (current_actor actor
             && same_principal actor active.view.row
             && t.disclosure.authorize actor active.view.row)
        then Error (denied ())
        else (
          match active.terminal with
          | Some (Error error) -> Error error
          | None | Some (Ok _) -> terminal_view t ~actor ~run_id ~key))
    | None ->
      let%bind before = observe t ~actor ~run_id ~key in
      (match before with
       | None -> Error (stale ())
       | Some view when Option.is_some (View.receipt view) -> Ok view
       | Some _ ->
         with_slot t ~run_id ~f:(fun slot ->
           let%bind () = ensure_startup_recovered t slot in
           terminal_view t ~actor ~run_id ~key)))
;;

let cancel_and_wait t ~actor ~run_id ~key =
  let open Result.Let_syntax in
  if not (on_domain t && current_actor actor)
  then Error (denied ())
  else (
    match active_for_key t run_id key with
    | None -> Error (stale ())
    | Some active ->
      if
        (not active.live)
        || Option.value_map
             (Eio.Fiber.get active_key)
             ~default:false
             ~f:(phys_equal active)
      then Error (stale ())
      else if
        not
          (same_principal actor active.view.row
           && t.control.authorize actor active.view.row)
      then Error (denied ())
      else (
        active.cancel_requested <- true;
        Option.iter active.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit);
        Eio.Promise.await active.finished;
        if
          not
            (current_actor actor
             && t.control.authorize actor active.view.row
             && t.disclosure.authorize actor active.view.row)
        then Error (denied ())
        else (
          match active.terminal with
          | Some (Error error) -> Error error
          | None | Some (Ok _) -> terminal_view t ~actor ~run_id ~key)))
;;

let retry_finished_active t (active : active) =
  if active.live || active.retired
  then Ok ()
  else (
    match active.selected with
    | None -> Error (busy ())
    | Some receipt ->
      (match finish_active t active receipt with
       | Ok view ->
         active.terminal <- Some (Ok view);
         notify_joined_terminal t active;
         retire_active t active;
         Ok ()
       | Error error ->
         active.terminal <- Some (Error error);
         Error error))
;;

let settle_joined_run t run =
  let pending = reconcile_retained run in
  let abandonment =
    Result.bind pending ~f:(fun () ->
      match run.unlaunched with
      | None -> Ok ()
      | Some selected -> settle_unlaunched t run selected)
  in
  let settled = Result.bind abandonment ~f:(fun () -> reconcile_retained run) in
  let retried =
    List.map run.active ~f:(retry_finished_active t) |> List.find_map ~f:Result.error
  in
  match settled, retried, run.pending, run.recovery, run.claim_only, run.unlaunched with
  | Ok (), None, None, None, None, None when List.is_empty run.active -> Ok ()
  | Error error, _, _, _, _, _ -> Error error
  | Ok (), Some error, _, _, _, _ -> Error error
  | Ok (), None, _, _, _, _ -> Error (busy ())
;;

let settle_finished_claim run =
  let open Result.Let_syntax in
  let%bind () =
    match run.finished with
    | None -> Ok ()
    | Some acquired ->
      let%bind () = store (Store.Run_claim.release (Startup.Acquired.claim acquired)) in
      run.finished <- None;
      Ok ()
  in
  match run.transferred_claim with
  | None -> Ok ()
  | Some (_, claim) when Store.Run_claim.is_released claim ->
    run.transferred_claim <- None;
    Ok ()
  | Some _ -> Error (busy ())
;;

let check_retirement t (slot : slot) ~actor =
  if not (slot_current t slot)
  then Error (stale ())
  else if
    Option.value_map (Eio.Fiber.get active_key) ~default:false ~f:(fun active ->
      phys_equal active.run slot.run)
  then Error (busy ())
  else if
    not
      (current_actor actor
       && List.for_all slot.run.active ~f:(fun active ->
         same_principal actor active.view.row && t.control.authorize actor active.view.row)
      )
  then Error (denied ())
  else Ok ()
;;

let has_active t (slot : slot) =
  if slot_current t slot
  then Ok (not (List.is_empty slot.run.active))
  else Error (stale ())
;;

let retire_run t (slot : slot) ~actor =
  let open Result.Let_syntax in
  let%bind () = check_retirement t slot ~actor in
  let run = slot.run in
  Eio.Cancel.protect (fun () ->
    let active = run.active in
    List.iter active ~f:(fun call ->
      call.cancel_requested <- true;
      Option.iter call.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit));
    List.iter active ~f:(fun call -> Eio.Promise.await call.finished);
    if not (slot_current t slot) then Error (stale ()) else settle_joined_run t run)
;;

let lookup t (slot : slot) ~actor ~key ~kind ~request_fingerprint =
  let open Result.Let_syntax in
  if not (slot_current t slot)
  then Error (stale ())
  else if not (current_actor actor)
  then Error (denied ())
  else (
    let%bind () = reconcile_retained slot.run in
    let%bind found = snapshot t slot.run.run_id ~actor ~key in
    match found with
    | None -> Ok None
    | Some view ->
      if
        not
          (Receipt.Kind.equal (Row.kind view.row) kind
           && Chatml_source.Digest.equal
                (Row.request_fingerprint view.row)
                request_fingerprint)
      then Error (busy ())
      else (
        let%bind () =
          match
            List.find slot.run.active ~f:(fun active ->
              P.Idempotency_key.equal (Row.key active.view.row) key)
          with
          | Some active when not active.live ->
            Eio.Cancel.protect (fun () -> retry_finished_active t active)
          | None | Some _ -> Ok ()
        in
        let%bind refreshed = snapshot t slot.run.run_id ~actor ~key in
        if not (slot_current t slot)
        then Error (stale ())
        else (
          match refreshed with
          | Some refreshed
            when Row.same_identity refreshed.row view.row
                 && Receipt.Kind.equal (Row.kind refreshed.row) kind
                 && Chatml_source.Digest.equal
                      (Row.request_fingerprint refreshed.row)
                      request_fingerprint -> Ok (Some refreshed)
          | None | Some _ -> Error (stale ()))))
;;

module Unlaunched = struct
  type disposition =
    | No_durable_row
    | Terminal_row
end

let reconcile_unlaunched t (slot : slot) ~actor ~key ~kind ~request_fingerprint ~handle =
  let open Result.Let_syntax in
  let check_local () =
    if
      (not (slot_current t slot))
      || Option.is_some slot.unlaunched
      || Option.is_some slot.run.pending
      || Option.is_some slot.run.unlaunched
      || Option.is_some slot.run.recovery
      || Option.is_some slot.run.claim_only
      || List.exists slot.run.active ~f:(fun active ->
        P.Idempotency_key.equal (Row.key active.view.row) key)
      ||
      match slot.run.phase with
      | Ready | Cold -> false
      | Recovering | Quarantined -> true
    then Error (busy ())
    else Ok ()
  in
  let%bind () =
    if slot_current t slot then reconcile_retained slot.run else Error (stale ())
  in
  let%bind () = check_local () in
  let%bind found = snapshot t slot.run.run_id ~actor ~key in
  let%bind () = check_local () in
  match found with
  | None -> Ok Unlaunched.No_durable_row
  | Some view ->
    let row = view.row in
    (match handle, Row.phase row with
     | Some handle, Row.Phase.Terminal _
       when Store.issued_by t.store handle
            && P.Execution_owner.equal_developer (Row.owner row) (Store.owner handle)
            && Receipt.Kind.equal (Row.kind row) kind
            && Chatml_source.Digest.equal
                 (Row.request_fingerprint row)
                 request_fingerprint -> Ok Terminal_row
     | None, _
     | Some _, (Reserved | Waiting_permission | Execution_started)
     | Some _, Row.Phase.Terminal _ -> Error (busy ()))
;;

let stop_and_join t =
  if not (on_domain t)
  then Error (stale ())
  else if Hashtbl.exists t.runs ~f:(fun run -> Option.is_some run.slot)
  then Error (busy ())
  else if
    Option.value_map (Eio.Fiber.get active_key) ~default:false ~f:(fun active ->
      phys_equal active.issuer t)
  then Error (busy ())
  else (
    t.closed <- true;
    Eio.Cancel.protect (fun () ->
      let active = Hashtbl.data t.runs |> List.concat_map ~f:(fun run -> run.active) in
      List.iter active ~f:(fun call ->
        call.cancel_requested <- true;
        Option.iter call.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit));
      List.iter active ~f:(fun call -> Eio.Promise.await call.finished);
      let outcomes = Hashtbl.data t.runs |> List.map ~f:(settle_joined_run t) in
      match List.find_map outcomes ~f:Result.error with
      | None -> Ok ()
      | Some error -> Error error))
;;

let close_and_wait t =
  let open Result.Let_syntax in
  let%bind () = stop_and_join t in
  let outcomes = Hashtbl.data t.runs |> List.map ~f:settle_finished_claim in
  match List.find_map outcomes ~f:Result.error with
  | None -> Ok ()
  | Some error -> Error error
;;
