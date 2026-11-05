open! Core

type background_lease =
  { cancel : unit -> unit
  ; finished : unit Eio.Promise.t
  ; finish : unit Eio.Promise.u
  ; survives_stop : bool
  ; mutable resources : Agent_session.Runtime_builder.t option
  }

module Source_stage_binding = struct
  type t =
    | Initial of Agent_session.Session_actor.Source_binding.t
    | Existing_goal of Agent_session.Session_actor.Goal_continuation.Source_binding.t
    | Existing_goal_execution of
        Agent_session.Session_actor.Goal_execution_runtime.Source_binding.t

  let close = function
    | Initial binding -> Agent_session.Session_actor.Source_binding.close binding
    | Existing_goal binding ->
      Agent_session.Session_actor.Goal_continuation.Source_binding.close binding
    | Existing_goal_execution binding ->
      Agent_session.Session_actor.Goal_execution_runtime.Source_binding.close binding
  ;;
end

type source_stage =
  { domain : int
  ; runtime : Agent_session.Runtime_builder.t option
  ; mutable binding : Source_stage_binding.t option
  ; mutable continuation : Agent_session.Session_actor.Runtime_construction.t option
  ; mutable closed : bool
  ; mutable retire_installed : Agent_session.Runtime_builder.t option
  ; mutable handoff_runtime : Agent_session.Runtime_builder.t option
  }

let close_source_stage stage =
  stage.closed <- true;
  Option.iter stage.binding ~f:Source_stage_binding.close;
  Option.iter stage.continuation ~f:(fun continuation ->
    Agent_session.Session_actor.Runtime_construction.close continuation)
;;

let check_prepared_stage stage ~basis =
  if not (Int.equal stage.domain (Stdlib.Domain.self () :> int))
  then
    Error
      (Agent_protocol.Error.invalid_request
         "prepared source belongs to another Eio domain")
  else if stage.closed || Option.is_some stage.continuation
  then
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"prepared source lifetime is closed or consumed"
         ~retryable:false
         ())
  else basis ()
;;

module Prepared = struct
  type t =
    { stage : source_stage
    ; binding : Agent_session.Session_actor.Source_binding.t
    ; preparation : Agent_session.Run_preparation.t
    ; basis : unit -> (unit, Agent_protocol.Error.t) result
    }

  let binding t = t.binding

  let check_basis t =
    let open Result.Let_syntax in
    let%bind () = check_prepared_stage t.stage ~basis:t.basis in
    Agent_session.Session_actor.Source_binding.check_basis t.binding
  ;;
end

module Prepared_goal_continuation = struct
  type t =
    { stage : source_stage
    ; binding : Agent_session.Session_actor.Goal_continuation.Source_binding.t
    ; basis : unit -> (unit, Agent_protocol.Error.t) result
    }

  let binding t = t.binding

  let check_basis t =
    let open Result.Let_syntax in
    let%bind () = check_prepared_stage t.stage ~basis:t.basis in
    Agent_session.Session_actor.Goal_continuation.Source_binding.check_basis t.binding
  ;;
end

module Prepared_goal_execution = struct
  type t =
    { stage : source_stage
    ; binding : Agent_session.Session_actor.Goal_execution_runtime.Source_binding.t
    ; basis : unit -> (unit, Agent_protocol.Error.t) result
    }

  let binding t = t.binding

  let check_basis t =
    let open Result.Let_syntax in
    let%bind () = check_prepared_stage t.stage ~basis:t.basis in
    Agent_session.Session_actor.Goal_execution_runtime.Source_binding.check_basis
      t.binding
  ;;
end

type cleanup_outcome = (unit, exn * Stdlib.Printexc.raw_backtrace) result

type unload_outcome =
  ((unit, Agent_protocol.Error.t) result, exn * Stdlib.Printexc.raw_backtrace) result

exception Cleanup_failed of Agent_protocol.Error.t

module Scope_outcome = struct
  module Cleanup = struct
    module Failure = struct
      type t =
        | Protocol of Agent_protocol.Error.t
        | Raised of exn * Stdlib.Printexc.raw_backtrace
    end

    type status =
      | Never_acquired
      | Joined
      | Failed of Failure.t * Failure.t list

    type t = status

    let status t = t
    let failed failure = Failed (failure, [])

    let combine first second =
      match first, second with
      | Failed (failure, rest), Failed (next, remaining) ->
        Failed (failure, rest @ (next :: remaining))
      | Failed _, (Never_acquired | Joined) -> first
      | (Never_acquired | Joined), Failed _ -> second
      | Never_acquired, Never_acquired -> Never_acquired
      | (Never_acquired | Joined), (Never_acquired | Joined) -> Joined
    ;;

    let of_result ~acquired = function
      | Ok (Ok ()) -> if acquired then Joined else Never_acquired
      | Ok (Error failure) -> Failed (Protocol failure, [])
      | Error (exn, backtrace) -> Failed (Raised (exn, backtrace), [])
    ;;
  end

  type 'a primary =
    | Returned of ('a, Agent_protocol.Error.t) result
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  type 'a t =
    { primary : 'a primary
    ; cleanup : Cleanup.t
    }

  let primary t = t.primary
  let cleanup t = t.cleanup

  let capture f =
    match f () with
    | result -> Returned result
    | exception exn -> Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  ;;

  let resume t =
    match t.primary with
    | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
    | Returned (Error _ as failure) -> failure
    | Returned (Ok _ as result) ->
      (match t.cleanup with
       | Never_acquired | Joined -> result
       | Failed (Protocol failure, _) -> Error failure
       | Failed (Raised (exn, backtrace), _) ->
         Exn.raise_with_original_backtrace exn backtrace)
  ;;
end

type deferred_builder =
  source:Agent_session.Runtime_builder.Source.t
  -> construction:Agent_session.Session_actor.Runtime_construction.t
  -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result

type builder =
  | Ordinary of (unit -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  | For_runs of
      (Agent_session.Run_preparation.t option
       -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)
  | For_authenticated_runs of
      (original_actor:Operator_authorization.t option
       -> Agent_session.Run_preparation.t option
       -> (Agent_session.Runtime_builder.t, Agent_protocol.Error.t) result)

type t =
  { actor : Agent_session.Session_actor.t
  ; runtime_source_owner : Agent_session.Runtime_source_owner.Identity.t option
  ; build : builder
  ; build_deferred : deferred_builder option
  ; before_unload : (closing:bool -> (unit, Agent_protocol.Error.t) result) option
  ; mutex : Eio.Mutex.t
  ; moderator_work_mutex : Eio.Mutex.t
  ; mutable runtime : Agent_session.Runtime_builder.t option
  ; mutable prepared_source : source_stage option
  ; mutable activation_handoff : bool
  ; domain : int
  ; mutable closed : bool
  ; mutable close_finished : bool
  ; mutable foreground_retirement :
      Agent_session.Session_actor.Runtime_retirement.t option
  ; mutable unloading : unload_outcome Eio.Promise.t option
  ; mutable background_leases : background_lease list
  ; mutable retired_resources : Agent_session.Runtime_builder.t list
  }

let stage_has_activation_handoff stage =
  Option.exists stage.continuation ~f:(fun continuation ->
    Agent_session.Session_actor.Runtime_construction.handoff_acknowledged continuation)
;;

let has_activation_handoff t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "Runtime_owner.has_activation_handoff: foreign domain";
  t.activation_handoff || Option.exists t.prepared_source ~f:stage_has_activation_handoff
;;

module Execution_access = struct
  type t =
    { stage : source_stage
    ; runtime : Agent_session.Runtime_builder.t
    ; borrow : Agent_session.Session_actor.Runtime_construction_resource.t
    }
end

let execution_access_is_current t (access : Execution_access.t) =
  Int.equal access.stage.domain (Stdlib.Domain.self () :> int)
  && (not access.stage.closed)
  && Option.exists t.prepared_source ~f:(phys_equal access.stage)
  && Option.exists t.runtime ~f:(phys_equal access.runtime)
  && Option.exists access.stage.handoff_runtime ~f:(phys_equal access.runtime)
  && Result.is_ok
       (Agent_session.Session_actor.Runtime_construction_resource.check_basis
          access.borrow)
;;

(* A root worker can request its actual resources before its submitting caller
   resumes. Only the Actor's immutable same-activation ACK permits coexistence
   with that old lexical stage; installation alone grants no such admission. *)
let prepared_source_blocks_runtime ?execution_access t =
  match execution_access, t.prepared_source with
  | Some access, _ -> not (execution_access_is_current t access)
  | None, None -> false
  | None, Some stage ->
    not
      (match stage.continuation, stage.handoff_runtime, t.runtime with
       | Some continuation, Some installed, Some current ->
         phys_equal installed current
         && Agent_session.Session_actor.Runtime_construction.handoff_acknowledged
              continuation
       | None, _, _ | _, None, _ | _, _, None -> false)
;;

let create_internal
      ~runtime_source_owner
      ~before_unload
      ~actor
      ~initial
      ~build
      ~build_deferred
  =
  { actor
  ; runtime_source_owner
  ; build
  ; build_deferred
  ; before_unload
  ; mutex = Eio.Mutex.create ()
  ; moderator_work_mutex = Eio.Mutex.create ()
  ; runtime = initial
  ; prepared_source = None
  ; activation_handoff = false
  ; domain = (Stdlib.Domain.self () :> int)
  ; closed = false
  ; close_finished = false
  ; foreground_retirement = None
  ; unloading = None
  ; background_leases = []
  ; retired_resources = []
  }
;;

let create_with_unload ~before_unload ~actor ~initial ~build =
  create_internal
    ~runtime_source_owner:None
    ~before_unload:(Some before_unload)
    ~actor
    ~initial
    ~build:(Ordinary build)
    ~build_deferred:None
;;

let create ~actor ~initial ~build =
  create_internal
    ~runtime_source_owner:None
    ~before_unload:None
    ~actor
    ~initial
    ~build:(Ordinary build)
    ~build_deferred:None
;;

let create_with_run_unload ~before_unload ~actor ~initial ~build =
  create_internal
    ~runtime_source_owner:None
    ~before_unload:(Some before_unload)
    ~actor
    ~initial
    ~build:(For_runs build)
    ~build_deferred:None
;;

let create_with_authenticated_run_unload ~before_unload ~actor ~initial ~build =
  create_internal
    ~runtime_source_owner:None
    ~before_unload:(Some before_unload)
    ~actor
    ~initial
    ~build:(For_authenticated_runs build)
    ~build_deferred:None
;;

let create_with_deferred_run_unload
      ~source_owner
      ~before_unload
      ~actor
      ~initial
      ~build
      ~build_deferred
  =
  create_internal
    ~runtime_source_owner:
      (Some (Agent_session.Runtime_source_owner.identity source_owner))
    ~before_unload:(Some before_unload)
    ~actor
    ~initial
    ~build:(For_authenticated_runs build)
    ~build_deferred:(Some build_deferred)
;;

let is_loaded t = Eio.Mutex.use_ro t.mutex (fun () -> Option.is_some t.runtime)
let belongs_to_actor t actor = phys_equal t.actor actor

let install ?run_preparation ?original_actor t (runtime : Agent_session.Runtime_builder.t)
  =
  let open Result.Let_syntax in
  let%bind () =
    match runtime.automatic_turn_policy with
    | None -> Ok ()
    | Some policy ->
      (match run_preparation with
       | None ->
         Agent_session.Session_actor.enable_automatic_turn_budget
           ?actor:original_actor
           t.actor
           policy
       | Some preparation ->
         Agent_session.Session_actor.enable_run_constructor_turn_budget
           t.actor
           ~preparation
           policy)
  in
  let%bind _ =
    match run_preparation with
    | None ->
      let%bind state = Agent_session.Session_actor.state t.actor in
      let%bind moderator =
        Agent_session.Moderator_checkpoint.reuse_unchanged_snapshot
          ~previous:state.moderator
          ~incoming:(runtime.moderator_snapshot ())
      in
      Agent_session.Session_actor.change_moderator ?actor:original_actor t.actor moderator
    | Some preparation ->
      Agent_session.Session_actor.checkpoint_run_moderator
        ?moderator_manager:runtime.moderator_manager
        t.actor
        ~preparation
        (runtime.moderator_snapshot ())
  in
  let%map () =
    Agent_session.Session_actor.set_runtime_worker
      ?actor:original_actor
      ?moderator_manager:runtime.moderator_manager
      t.actor
      ~worker:(Some runtime.worker)
      ~inference:(Some (Agent_session.Runtime_builder.inference_execution runtime))
  in
  t.runtime <- Some runtime;
  t.foreground_retirement <- None
;;

let closed_error () =
  Agent_protocol.Error.create
    Server_shutting_down
    ~message:"session runtime owner is closed"
    ~retryable:true
    ()
;;

(* Host admission callbacks can fail while consulting another actor or store.
   Propagate their exception after releasing the mutex, so cleanup remains usable. *)
let with_owner_lock t ~protect f =
  let outcome =
    Eio.Mutex.use_rw ~protect t.mutex (fun () ->
      match f () with
      | result -> Ok result
      | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let check_original_actor = function
  | Some actor when not (Operator_authorization.is_current actor) ->
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:"runtime initialization authentication is no longer current"
         ~retryable:false
         ())
  | Some _ | None -> Ok ()
;;

let ensure_loaded_locked ?execution_access ?run_preparation ?original_actor t =
  let open Result.Let_syntax in
  let original_actor =
    Option.first_some
      original_actor
      (Option.bind run_preparation ~f:Agent_session.Run_preparation.original_actor)
  in
  let%bind () = check_original_actor original_actor in
  let check (runtime : Agent_session.Runtime_builder.t) =
    match runtime.check_execution with
    | None -> Ok ()
    | Some check -> check ()
  in
  match t.closed, t.runtime with
  | true, _ -> Error (closed_error ())
  | false, _ when prepared_source_blocks_runtime ?execution_access t ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"session runtime has an exclusive prepared source lifetime"
         ~retryable:true
         ())
  | false, _ when Option.is_some t.unloading ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"session runtime is waiting for background cleanup"
         ~retryable:true
         ())
  | false, Some _ when Option.is_some t.foreground_retirement ->
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"session runtime retirement must finish before execution resumes"
         ~retryable:true
         ())
  | false, Some runtime -> check runtime
  | false, None ->
    let built =
      match t.build, run_preparation, original_actor with
      | Ordinary build, None, None -> build ()
      | For_runs build, preparation, None -> build preparation
      | For_authenticated_runs build, preparation, original_actor ->
        build ~original_actor preparation
      | Ordinary _, Some _, None | (Ordinary _ | For_runs _), _, Some _ ->
        Error
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"runtime constructor does not support scoped run admission"
             ~retryable:false
             ())
    in
    (match built with
     | Error _ as failure -> failure
     | Ok runtime ->
       (match
          Result.bind (check_original_actor original_actor) ~f:(fun () ->
            install ?run_preparation ?original_actor t runtime)
        with
        | Ok () -> check runtime
        | Error _ as failure ->
          (* The admitted install failure remains primary; exact constructor
             cleanup and the original resource close still run. *)
          (try Eio.Cancel.protect runtime.close with
           | cleanup ->
             (try
                Eio.traceln
                  "runtime cleanup after failed install: %s"
                  (Exn.to_string cleanup)
              with
              | _ -> ()));
          failure))
;;

let ensure_loaded t =
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_actor.exclude_effect_deferred_execution t.actor in
  with_owner_lock t ~protect:true (fun () -> ensure_loaded_locked t)
;;

let ensure_loaded_authenticated t ~actor =
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_actor.exclude_effect_deferred_execution t.actor in
  with_owner_lock t ~protect:true (fun () -> ensure_loaded_locked ~original_actor:actor t)
;;

let background_busy () =
  Agent_protocol.Error.create
    Conflict
    ~message:"background execution still owns the loaded runtime"
    ~retryable:true
    ()
;;

let resources_borrowed t runtime =
  List.exists t.background_leases ~f:(fun lease ->
    Option.exists lease.resources ~f:(phys_equal runtime))
;;

let release_runtime_locked t runtime =
  match resources_borrowed t runtime with
  | true -> t.retired_resources <- runtime :: t.retired_resources
  | false -> runtime.Agent_session.Runtime_builder.close ()
;;

let release_retired_resources_locked t runtime =
  match
    (not (resources_borrowed t runtime))
    && List.exists t.retired_resources ~f:(phys_equal runtime)
  with
  | false -> ()
  | true ->
    t.retired_resources
    <- List.filter t.retired_resources ~f:(fun retained ->
         not (phys_equal runtime retained));
    runtime.Agent_session.Runtime_builder.close ()
;;

(* Remove the reference before invoking cleanup, so a failed close cannot leave
   the same runtime available for a second retirement or poison the owner mutex. *)
let retire_runtime_locked t =
  match
    (match
       Agent_session.Session_actor.set_runtime_worker t.actor ~worker:None ~inference:None
     with
     | Ok () -> ()
     | Error error -> raise (Cleanup_failed error));
    let previous = t.runtime in
    t.runtime <- None;
    Option.iter previous ~f:(release_runtime_locked t)
  with
  | () -> Ok ()
  | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
;;

let raise_cleanup = function
  | Ok () -> ()
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let foreground_retirement_finished t =
  Option.exists t.foreground_retirement ~f:(fun retirement ->
    Agent_session.Session_actor.Runtime_retirement.is_finished retirement
    &&
    match Agent_session.Session_actor.Runtime_retirement.await retirement with
    | Ok () -> true
    | Error error -> raise (Cleanup_failed error))
;;

(* A successful unloaded retirement is historical DATA. Join outside the mutex,
   then clear only that exact handle while the runtime is still absent. *)
let completed_unloaded_retirement t =
  let candidate =
    with_owner_lock t ~protect:true (fun () ->
      match t.closed, t.unloading, t.runtime, t.foreground_retirement with
      | false, None, None, Some retirement
        when Agent_session.Session_actor.Runtime_retirement.is_finished retirement ->
        Some retirement
      | (true | false), (Some _ | None), (Some _ | None), (Some _ | None) -> None)
  in
  Option.bind candidate ~f:(fun retirement ->
    match Agent_session.Session_actor.Runtime_retirement.await retirement with
    | Ok () -> Some retirement
    | Error _ -> None)
;;

let forget_completed_retirement_locked t completed_retirement =
  match
    completed_retirement, t.foreground_retirement, t.closed, t.unloading, t.runtime
  with
  | Some completed, Some current, false, None, None when phys_equal completed current ->
    t.foreground_retirement <- None
  | (Some _ | None), (Some _ | None), (true | false), (Some _ | None), (Some _ | None) ->
    ()
;;

let source_lane_idle t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "Runtime_owner.source_lane_idle: foreign domain";
  Eio.Mutex.use_ro t.mutex (fun () ->
    (not t.closed)
    && Option.is_none t.unloading
    && Option.is_none t.prepared_source
    && List.is_empty t.background_leases
    && Option.value_map t.foreground_retirement ~default:true ~f:(fun retirement ->
      Agent_session.Session_actor.Runtime_retirement.is_finished retirement
      && Result.is_ok (Agent_session.Session_actor.Runtime_retirement.await retirement)))
;;

module Lease_admission = struct
  type ('value, 'result) t =
    | No_work of 'result
    | Borrow of 'value * Agent_session.Runtime_builder.t option
end

let with_lease_reported_internal
      ?execution_access
      ?(retire = ignore)
      t
      ~survives_stop
      ~admit
      f
  =
  let completed = ref None in
  let acquired = ref false in
  try
    Eio.Cancel.sub (fun context ->
      let active = Atomic.make true in
      let finished, finish = Eio.Promise.create () in
      let lease =
        { finished
        ; finish
        ; survives_stop
        ; resources = None
        ; cancel =
            (fun () ->
              match Atomic.get active with
              | true ->
                retire ();
                Eio.Cancel.cancel context Exit
              | false -> ())
        }
      in
      let admitted =
        with_owner_lock t ~protect:true (fun () ->
          let open Result.Let_syntax in
          let%bind () =
            if prepared_source_blocks_runtime ?execution_access t
            then Error (background_busy ())
            else Ok ()
          in
          let%map decision = admit () in
          match decision with
          | Lease_admission.No_work _ -> decision
          | Borrow (_, resources) ->
            lease.resources <- resources;
            t.background_leases <- lease :: t.background_leases;
            acquired := true;
            decision)
      in
      let report =
        match admitted with
        | Error _ as failure ->
          Atomic.set active false;
          { Scope_outcome.primary = Returned failure; cleanup = Never_acquired }
        | Ok (Lease_admission.No_work result) ->
          (* No work acquired ownership or exposed the cancellation callback. Keep
         caller cancellation observable without publishing a background lease. *)
          Atomic.set active false;
          let primary =
            Scope_outcome.capture (fun () ->
              Eio.Fiber.check ();
              Ok result)
          in
          { Scope_outcome.primary; cleanup = Never_acquired }
        | Ok (Borrow (runtime, _)) ->
          let cleanup () =
            (* Cleanup must survive cancellation while waiting for another owner. *)
            Eio.Cancel.protect (fun () ->
              Atomic.set active false;
              retire ();
              Exn.protect
                ~finally:(fun () -> Eio.Promise.resolve lease.finish ())
                ~f:(fun () ->
                  with_owner_lock t ~protect:true (fun () ->
                    t.background_leases
                    <- List.filter t.background_leases ~f:(fun current ->
                         not (phys_equal current lease));
                    Option.iter lease.resources ~f:(release_retired_resources_locked t);
                    match t.closed, t.background_leases, t.before_unload, t.unloading with
                    | true, [], None, None when foreground_retirement_finished t ->
                      let result = retire_runtime_locked t in
                      (match result with
                       | Ok () -> t.close_finished <- true
                       | Error _ -> ());
                      result
                    | _ -> Ok ())
                  |> raise_cleanup))
          in
          let primary =
            Scope_outcome.capture (fun () ->
              Eio.Fiber.check ();
              let result = f runtime in
              (match result with
               | Error _ -> ()
               | Ok _ -> Eio.Fiber.check ());
              result)
          in
          let cleaned =
            try
              cleanup ();
              Ok (Ok ())
            with
            | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
          in
          { Scope_outcome.primary
          ; cleanup = Scope_outcome.Cleanup.of_result ~acquired:true cleaned
          }
      in
      completed := Some report;
      report)
  with
  | exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    let primary = Scope_outcome.Raised (exn, backtrace) in
    let cleanup =
      match !completed with
      | Some report -> Scope_outcome.cleanup report
      | None ->
        if !acquired
        then Scope_outcome.Cleanup.Failed (Raised (exn, backtrace), [])
        else Never_acquired
    in
    { Scope_outcome.primary; cleanup }
;;

let with_lease_internal ?execution_access ?retire t ~survives_stop ~admit f =
  with_lease_reported_internal ?execution_access ?retire t ~survives_stop ~admit f
  |> Scope_outcome.resume
;;

let with_lease_reported ?execution_access ?retire t ~survives_stop ~admit f =
  with_lease_reported_internal
    ?execution_access
    ?retire
    t
    ~survives_stop
    ~admit:(fun () ->
      Result.map (admit ()) ~f:(fun (value, resources) ->
        Lease_admission.Borrow (value, resources)))
    f
;;

let with_lease ?execution_access ?retire t ~survives_stop ~admit f =
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_actor.exclude_effect_deferred_execution t.actor in
  with_lease_internal
    ?execution_access
    ?retire
    t
    ~survives_stop
    ~admit:(fun () ->
      Result.map (admit ()) ~f:(fun (value, resources) ->
        Lease_admission.Borrow (value, resources)))
    f
;;

let with_runtime_lease ?execution_access ?run_preparation t ~retain_resources f =
  with_lease
    ?execution_access
    t
    ~survives_stop:retain_resources
    ~admit:(fun () ->
      let open Result.Let_syntax in
      let%bind () = ensure_loaded_locked ?execution_access ?run_preparation t in
      let%map () =
        match execution_access with
        | None -> Ok ()
        | Some access ->
          (* Runtime validation may invoke its own execution checks. Recheck the
             exact borrow after those checks, before publishing this lease. *)
          if execution_access_is_current t access
          then Ok ()
          else Error (background_busy ())
      in
      let runtime = Option.value_exn t.runtime in
      runtime, if retain_resources then Some runtime else None)
    f
;;

let loaded_runtime_basis_locked t runtime =
  if t.closed
  then Error (closed_error ())
  else if
    Option.is_some t.unloading
    || Option.is_some t.foreground_retirement
    || prepared_source_blocks_runtime t
    || not (Option.exists t.runtime ~f:(phys_equal runtime))
  then Error (background_busy ())
  else Ok ()
;;

let check_runtime_execution (runtime : Agent_session.Runtime_builder.t) =
  match runtime.check_execution with
  | None -> Ok ()
  | Some check -> check ()
;;

let with_loaded_runtime_reported_internal t f =
  with_lease_reported
    t
    ~survives_stop:false
    ~admit:(fun () ->
      let open Result.Let_syntax in
      let%bind runtime =
        match t.closed, t.unloading, t.foreground_retirement, t.runtime with
        | true, _, _, _ -> Error (closed_error ())
        | false, Some _, _, _ | false, None, Some _, _ -> Error (background_busy ())
        | false, None, None, None ->
          Error
            (Agent_protocol.Error.create
               Invalid_state
               ~message:"session runtime is not already loaded"
               ~retryable:true
               ())
        | false, None, None, Some runtime -> Ok runtime
      in
      Ok (runtime, Some runtime))
    (fun runtime ->
       let open Result.Let_syntax in
       (* Delegation checks may read Store/Actor state. The same actual lease
          retains this physical runtime while that IO runs outside our mutex. *)
       let%bind () = check_runtime_execution runtime in
       Eio.Fiber.check ();
       let%bind () =
         with_owner_lock t ~protect:true (fun () -> loaded_runtime_basis_locked t runtime)
       in
       Eio.Fiber.check ();
       f runtime)
;;

let with_loaded_runtime t f =
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_actor.exclude_effect_deferred_execution t.actor in
  with_loaded_runtime_reported_internal t f |> Scope_outcome.resume
;;

let check_source_owner t source_owner =
  match
    ( t.runtime_source_owner
    , Agent_session.Session_actor.runtime_source_owner_identity t.actor )
  with
  | Some owner, Some actor
    when Agent_session.Runtime_source_owner.matches source_owner owner
         && Agent_session.Runtime_source_owner.matches source_owner actor -> Ok ()
  | (Some _ | None), (Some _ | None) ->
    Error
      (Agent_protocol.Error.create
         Permission_denied
         ~message:"source lifetime belongs to another Factory"
         ~retryable:false
         ())
;;

let with_loaded_source_capabilities_reported t ~source_owner f =
  match Scope_outcome.capture (fun () -> check_source_owner t source_owner) with
  | Raised (exn, backtrace) ->
    { Scope_outcome.primary = Raised (exn, backtrace); cleanup = Never_acquired }
  | Returned (Error failure) ->
    { Scope_outcome.primary = Returned (Error failure); cleanup = Never_acquired }
  | Returned (Ok ()) ->
    with_loaded_runtime_reported_internal t (fun runtime ->
      let open Result.Let_syntax in
      let unavailable message =
        Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
      in
      let%bind native =
        Result.of_option
          runtime.Agent_session.Runtime_builder.native_runtime
          ~error:(unavailable "delegation parent has no loaded native bindings")
      in
      let%bind () =
        if Lazy.is_val native.capabilities
        then Ok ()
        else
          Error
            (unavailable
               "delegation parent native bindings have not already been realized")
      in
      let%bind capabilities =
        Lazy.force native.capabilities
        |> Result.map_error ~f:(fun error ->
          Agent_protocol.Error.create
            Permission_denied
            ~message:error.Chat_response.Tool_capability.message
            ~retryable:false
            ())
      in
      f capabilities)
;;

let with_background_runtime t f = with_runtime_lease t ~retain_resources:false f

let with_execution_runtime t ~owner f =
  let open Result.Let_syntax in
  let%bind selected =
    with_owner_lock t ~protect:true (fun () ->
      if not (prepared_source_blocks_runtime t)
      then Ok None
      else (
        match t.prepared_source, t.runtime with
        | Some stage, Some runtime when not stage.closed ->
          (match stage.continuation, stage.handoff_runtime with
           | Some continuation, Some installed when phys_equal installed runtime ->
             Ok (Some (stage, runtime, continuation))
           | _ -> Error (background_busy ()))
        | _ -> Error (background_busy ())))
  in
  match selected with
  | None -> with_background_runtime t f
  | Some (stage, runtime, continuation) ->
    (* Selection can refresh the Actor's Store and must stay outside our mutex.
       The resulting token is a pure, exact execution-borrow check; identifiers
       supplied by a caller do not themselves authorize access. *)
    let%bind borrow =
      Agent_session.Session_actor.runtime_construction_resource
        t.actor
        ~construction:continuation
        ~owner
    in
    let execution_access : Execution_access.t = { stage; runtime; borrow } in
    with_runtime_lease ~execution_access t ~retain_resources:false f
;;

module Loaded_job_runtime = struct
  type 'a t =
    | No_runtime
    | Reused of 'a
end

let with_loaded_goal_job_runtime t ~owner f =
  let module Actor = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let completed_retirement = completed_unloaded_retirement t in
  let%bind selected =
    with_owner_lock t ~protect:true (fun () ->
      forget_completed_retirement_locked t completed_retirement;
      match t.closed, t.unloading, t.foreground_retirement, t.runtime with
      | true, _, _, _ -> Error (closed_error ())
      | false, Some _, _, _ | false, None, Some _, _ -> Error (background_busy ())
      | false, None, None, runtime ->
        if prepared_source_blocks_runtime t
        then Error (background_busy ())
        else Ok runtime)
  in
  match selected with
  | None -> Ok Loaded_job_runtime.No_runtime
  | Some runtime ->
    let%bind borrow =
      Actor.job_runtime_resource
        t.actor
        ~owner
        ~worker:runtime.worker
        ~inference:runtime.inference_execution
        ~moderator_manager:runtime.moderator_manager
    in
    let%bind result =
      with_lease
        t
        ~survives_stop:false
        ~admit:(fun () ->
          let%bind () = loaded_runtime_basis_locked t runtime in
          let%map () = Actor.Runtime_construction_resource.check_basis borrow in
          runtime, Some runtime)
        (fun runtime ->
           (* The physical cancellation lease protects resources while delegated
             authority may perform IO. No callback or Actor RPC runs under the
             Owner mutex, and no runtime escapes before the final exact proof. *)
           let%bind () = check_runtime_execution runtime in
           Eio.Fiber.check ();
           let%bind current =
             Actor.job_runtime_resource
               t.actor
               ~owner
               ~worker:runtime.worker
               ~inference:runtime.inference_execution
               ~moderator_manager:runtime.moderator_manager
           in
           let%bind () =
             with_owner_lock t ~protect:true (fun () ->
               let%bind () = loaded_runtime_basis_locked t runtime in
               Actor.Runtime_construction_resource.check_basis current)
           in
           Eio.Fiber.check ();
           f runtime)
    in
    Ok (Loaded_job_runtime.Reused result)
;;

let with_prepared_run_runtime t ~preparation f =
  let open Result.Let_syntax in
  let%bind () =
    Agent_session.Session_actor.check_prepared_run_execution t.actor ~preparation
  in
  with_runtime_lease ~run_preparation:preparation t ~retain_resources:false f
;;

let with_delegation_resources t f = with_runtime_lease t ~retain_resources:true f

let with_auxiliary_execution_lifetime t f =
  with_lease
    t
    ~survives_stop:false
    ~admit:(fun () ->
      match t.closed, t.unloading with
      | true, _ -> Error (closed_error ())
      | false, Some _ -> Error (background_busy ())
      | false, None ->
        Ok
          ( Option.map t.runtime ~f:Agent_session.Runtime_builder.inference_execution
          , t.runtime ))
    f
;;

let with_resource_lifetime t f =
  with_lease
    t
    ~survives_stop:true
    ~admit:(fun () ->
      match t.closed, t.unloading with
      | true, _ -> Error (closed_error ())
      | false, Some _ -> Error (background_busy ())
      | false, None -> Ok ((), None))
    f
;;

let moderation_source t =
  with_background_runtime t Agent_session.Runtime_builder.moderation_source
;;

let prepare_delegated_tool t ~delegation ~event ~authorize =
  with_background_runtime t (fun runtime ->
    let module A = Agent_session.Session_actor in
    let module M = Chat_response.Moderator_manager in
    let open Result.Let_syntax in
    let unavailable () =
      Error
        (Agent_protocol.Error.create
           Invalid_state
           ~message:
             "delegation.owner_mediation_unavailable: parent policy runtime is \
              unavailable"
           ~retryable:false
           ())
    in
    match runtime.Agent_session.Runtime_builder.moderator_manager with
    | None -> unavailable ()
    | Some manager when Option.is_none (M.extension_definition manager) -> unavailable ()
    | Some manager ->
      let history = ref [] in
      let claim ~notifications ~snapshot handle =
        A.with_delegated_moderator_event
          ~notifications
          t.actor
          ~delegation
          ~event
          ~authorize
          ~snapshot
          (fun ~executing ~model_jobs ~allocate_inference ~event ~execute ~commit ->
             let%bind state = A.state t.actor in
             let%bind entries =
               Agent_session.History_codec.all_of_protocol
                 state.conversation.canonical_history
             in
             history := entries;
             handle ~executing ~model_jobs ~allocate_inference ~event ~execute ~commit)
      in
      let result =
        Agent_session.Moderator_event.run_delegated
          ~model_recipes:runtime.model_recipes
          ~event
          ~claim
          ~run_actions:(A.run_actions t.actor)
          ?script_tools:runtime.moderator_script_tools
          ~manager
          ~history:(fun () -> !history)
          ~available_tools:runtime.moderator_tools
          ~session_meta:`Null
          ~now:runtime.now
          ()
      in
      (* A committed parent intent remains the parent's responsibility even if
         child authority was lost before the policy reply could be disclosed. *)
      let%bind _ = A.apply_moderator_follow_up t.actor in
      let%bind result = result in
      (match result with
       | Some { receipt = { decision = Some decision; _ }; _ } -> Ok decision
       | None | Some _ -> unavailable ()))
;;

let unload_locked t =
  match t.background_leases, t.runtime with
  | _ :: _, _ -> Error (background_busy ())
  | [], None -> Ok ()
  | [], Some runtime ->
    let open Result.Let_syntax in
    let%map () =
      Agent_session.Session_actor.set_runtime_worker t.actor ~worker:None ~inference:None
    in
    t.runtime <- None;
    release_runtime_locked t runtime
;;

let unload t =
  with_owner_lock t ~protect:true (fun () ->
    match t.unloading with
    | Some _ -> Error (background_busy ())
    | None -> unload_locked t)
;;

let prepare_dependency_stop t =
  match t.before_unload with
  | None -> Ok ()
  | Some prepare -> prepare ~closing:false
;;

let prepare_dependency_close t =
  match t.before_unload with
  | None -> Ok ()
  | Some prepare -> prepare ~closing:true
;;

let unload_and_wait t =
  (* A committed stop must join cleanup before closing runtime/workspace resources.
     Waiting with the mutex held would prevent the lease finalizers from releasing. *)
  Eio.Cancel.protect (fun () ->
    let open Result.Let_syntax in
    let%bind disposition =
      with_owner_lock t ~protect:true (fun () ->
        match t.unloading with
        | Some finished -> Ok (`Join finished)
        | None ->
          let finished, finish = Eio.Promise.create () in
          Option.iter t.prepared_source ~f:close_source_stage;
          t.unloading <- Some finished;
          Ok (`Retire (t.background_leases, t.closed, finish)))
    in
    let outcome =
      match disposition with
      | `Join finished -> Eio.Promise.await finished
      | `Retire (leases, closing, finish) ->
        let outcome =
          try
            let result =
              (* Foreground cancellation precedes dependency cleanup: that
                 cleanup may itself be waiting for a foreground-owned child. *)
              let%bind retirement =
                Agent_session.Session_actor.retire_runtime_worker t.actor ~closing
              in
              with_owner_lock t ~protect:true (fun () ->
                t.foreground_retirement <- Some retirement);
              let%bind () =
                match t.before_unload with
                | None -> Ok ()
                | Some prepare -> prepare ~closing
              in
              let leases =
                List.filter leases ~f:(fun lease -> closing || not lease.survives_stop)
              in
              List.iter leases ~f:(fun lease -> lease.cancel ());
              List.iter leases ~f:(fun lease -> Eio.Promise.await lease.finished);
              let%bind () =
                Agent_session.Session_actor.Runtime_retirement.await retirement
              in
              with_owner_lock t ~protect:true (fun () -> retire_runtime_locked t)
              |> raise_cleanup;
              Ok ()
            in
            Ok result
          with
          | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
        in
        with_owner_lock t ~protect:true (fun () ->
          t.unloading <- None;
          Eio.Promise.resolve finish outcome);
        outcome
    in
    match outcome with
    | Ok result -> result
    | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace)
;;

let with_source_stage_reported t f =
  let reserved = ref None in
  let lease_cleanup = ref Scope_outcome.Cleanup.Never_acquired in
  let retire () = Option.iter !reserved ~f:close_source_stage in
  let primary =
    try
      let completed_retirement = completed_unloaded_retirement t in
      let report =
        with_lease_reported
          t
          ~survives_stop:false
          ~retire
          ~admit:(fun () ->
            forget_completed_retirement_locked t completed_retirement;
            match t.closed, t.unloading, t.foreground_retirement, t.prepared_source with
            | true, _, _, _ -> Error (closed_error ())
            | false, Some _, _, _ | false, None, Some _, _ | false, None, None, Some _ ->
              Error (background_busy ())
            | false, None, None, None when not (List.is_empty t.background_leases) ->
              Error (background_busy ())
            | false, None, None, None ->
              let stage =
                { domain = (Stdlib.Domain.self () :> int)
                ; runtime = t.runtime
                ; binding = None
                ; continuation = None
                ; closed = false
                ; retire_installed = None
                ; handoff_runtime = None
                }
              in
              reserved := Some stage;
              t.prepared_source <- Some stage;
              Ok (stage, t.runtime))
          f
      in
      lease_cleanup := Scope_outcome.cleanup report;
      Scope_outcome.primary report
    with
    | exn -> Scope_outcome.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  let cleanup () =
    Eio.Cancel.protect (fun () ->
      retire ();
      let clear_owned_stage () =
        with_owner_lock t ~protect:true (fun () ->
          match t.prepared_source, !reserved with
          | Some current, Some owned when phys_equal current owned ->
            t.activation_handoff
            <- t.activation_handoff || stage_has_activation_handoff owned;
            t.prepared_source <- None
          | Some _, (Some _ | None) | None, (Some _ | None) -> ())
      in
      Exn.protect ~finally:clear_owned_stage ~f:(fun () ->
        let open Result.Let_syntax in
        let%bind retire_runtime =
          with_owner_lock t ~protect:true (fun () ->
            match !reserved with
            | None -> Ok false
            | Some owned ->
              (match owned.retire_installed with
               | None -> Ok false
               | Some installed ->
                 (match t.prepared_source, t.runtime with
                  | Some current, Some runtime
                    when phys_equal current owned && phys_equal installed runtime ->
                    Ok true
                  | Some current, None when phys_equal current owned -> Ok false
                  | Some _, (Some _ | None) | None, (Some _ | None) ->
                    Error
                      (Agent_protocol.Error.create
                         Conflict
                         ~message:"installed runtime retirement owner was displaced"
                         ~retryable:false
                         ()))))
        in
        (* The old stage stays registered across the join, excluding a foreign
           reload after this lease has released but before retirement begins. *)
        if retire_runtime then unload_and_wait t else Ok ()))
  in
  let cleaned =
    try Ok (cleanup ()) with
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  { Scope_outcome.primary
  ; cleanup =
      Scope_outcome.Cleanup.combine
        !lease_cleanup
        (Scope_outcome.Cleanup.of_result ~acquired:(Option.is_some !reserved) cleaned)
  }
;;

let source_stage_basis t (stage : source_stage) () =
  if
    t.closed
    || Option.is_some t.unloading
    || Option.is_some t.foreground_retirement
    || (not (Option.exists t.prepared_source ~f:(phys_equal stage)))
    || not (Option.equal phys_equal t.runtime stage.runtime)
  then
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"prepared runtime owner was displaced"
         ~retryable:false
         ())
  else Ok ()
;;

let consume_source_selection t (stage : source_stage) attempted ~label =
  let open Result.Let_syntax in
  let%bind () =
    if Int.equal stage.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else
      Error
        (Agent_protocol.Error.invalid_request (label ^ " belongs to another Eio domain"))
  in
  let current =
    with_owner_lock t ~protect:true (fun () ->
      if
        (not !attempted)
        && (not stage.closed)
        && (not t.closed)
        && Option.is_none t.unloading
        && Option.is_none t.foreground_retirement
        && Option.exists t.prepared_source ~f:(phys_equal stage)
        && Option.equal phys_equal t.runtime stage.runtime
        && Option.is_none stage.binding
        && Option.is_none stage.continuation
      then (
        (* Consume selection before the Actor call can yield or fail. *)
        attempted := true;
        true)
      else false)
  in
  if current
  then Ok ()
  else
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:(label ^ " was closed, displaced or already selected")
         ~retryable:false
         ())
;;

let prepare_source t stage ~preparation ~source =
  let open Result.Let_syntax in
  let%bind binding =
    Eio.Cancel.protect (fun () ->
      let result =
        Agent_session.Session_actor.bind_run_source t.actor ~preparation ~source
      in
      Result.iter result ~f:(fun binding ->
        stage.binding <- Some (Initial binding);
        if stage.closed then Agent_session.Session_actor.Source_binding.close binding);
      result)
  in
  let prepared : Prepared.t =
    { stage; binding; preparation; basis = source_stage_basis t stage }
  in
  let%bind () = Prepared.check_basis prepared in
  Eio.Fiber.check ();
  Ok prepared
;;

let with_prepared_source_reported t ~preparation ~source f =
  with_source_stage_reported t (fun stage ->
    let open Result.Let_syntax in
    let%bind prepared = prepare_source t stage ~preparation ~source in
    f prepared)
;;

let with_prepared_source t ~preparation ~source f =
  with_prepared_source_reported t ~preparation ~source f |> Scope_outcome.resume
;;

module Deferred_source = struct
  type owner = t

  type t =
    { owner : owner
    ; stage : source_stage
    ; deferred : Agent_session.Run_preparation.Deferred.t
    ; retry : Agent_session.Session_actor.Recurrence_retry.t option
    ; source_attempted : bool ref
    }

  let preparation t = t.deferred
  let retry t = t.retry

  let prepare t ~source =
    let open Result.Let_syntax in
    let%bind () =
      consume_source_selection t.owner t.stage t.source_attempted ~label:"deferred source"
    in
    prepare_source
      t.owner
      t.stage
      ~preparation:(Agent_session.Run_preparation.Deferred.preparation t.deferred)
      ~source
  ;;

  module Decision = struct
    type 'a t =
      | Retained of Agent_protocol.Run_receipt.t
      | Fresh of 'a
  end

  module Report = struct
    type 'a t =
      { deferred : Agent_session.Run_preparation.Deferred.t option
      ; retry : Agent_session.Session_actor.Recurrence_retry.t option
      ; outcome : 'a Decision.t Scope_outcome.t
      }

    let deferred t = t.deferred
    let retry t = t.retry
    let outcome t = t.outcome
  end
end

module Deferred_issuance = struct
  type t =
    | Initial of Agent_session.Run_admission_request.Headless.t
    | Retry of
        { observation : Agent_session.Session_actor.No_effects_observation.t
        ; intent : Agent_session.Recurrence_execution_intent.t
        ; previous : Agent_session.Recurrence_execution_attempt.t
        ; request : Agent_session.Run_admission_request.Headless.t
        }
    | Resume of Agent_session.Session_actor.Recurrence_retry.t

  type decision =
    | Retained of Agent_protocol.Run_receipt.t
    | Issued of
        { deferred : Agent_session.Run_preparation.Deferred.t
        ; retry : Agent_session.Session_actor.Recurrence_retry.t option
        }

  let issue t target ~actor ~authorize =
    let module Actor = Agent_session.Session_actor in
    match t with
    | Initial request ->
      Actor.begin_effect_deferred_headless_preparation target ~actor ~authorize ~request
      |> Result.map ~f:(function
        | Agent_session.Run_preparation.Deferred.Decision.Retained receipt ->
          Retained receipt
        | Prepare deferred -> Issued { deferred; retry = None })
    | Retry { observation; intent; previous; request } ->
      Actor.begin_recurrence_retry_preparation
        target
        ~observation
        ~actor
        ~authorize
        ~intent
        ~previous
        ~request
      |> Result.map ~f:(fun (retry, deferred) -> Issued { deferred; retry = Some retry })
    | Resume retry ->
      Actor.resume_recurrence_retry_preparation target ~retry ~actor ~authorize
      |> Result.map ~f:(fun (retry, deferred) -> Issued { deferred; retry = Some retry })
  ;;
end

let with_deferred_source_reported t ~source_owner ~actor ~authorize ~issuance f =
  let deferred = ref None in
  let retry = ref None in
  let outcome =
    match Scope_outcome.capture (fun () -> check_source_owner t source_owner) with
    | Raised (exn, backtrace) ->
      { Scope_outcome.primary = Raised (exn, backtrace); cleanup = Never_acquired }
    | Returned (Error failure) ->
      { Scope_outcome.primary = Returned (Error failure); cleanup = Never_acquired }
    | Returned (Ok ()) ->
      with_source_stage_reported t (fun stage ->
        let open Result.Let_syntax in
        let%bind decision =
          Eio.Cancel.protect (fun () ->
            let result = Deferred_issuance.issue issuance t.actor ~actor ~authorize in
            Result.iter result ~f:(function
              | Deferred_issuance.Issued issued ->
                (* Capture the actual pair before pending cancellation can escape,
                   including when source selection never invokes its callback. *)
                deferred := Some issued.deferred;
                retry := issued.retry
              | Retained _ -> ());
            result)
        in
        match decision with
        | Retained receipt -> Ok (Deferred_source.Decision.Retained receipt)
        | Issued issued ->
          let source =
            { Deferred_source.owner = t
            ; stage
            ; deferred = issued.deferred
            ; retry = issued.retry
            ; source_attempted = ref false
            }
          in
          Eio.Fiber.check ();
          Result.map (f source) ~f:(fun result -> Deferred_source.Decision.Fresh result))
  in
  { Deferred_source.Report.deferred = !deferred; retry = !retry; outcome }
;;

let with_effect_deferred_source_reported t ~source_owner ~actor ~authorize ~request f =
  with_deferred_source_reported
    t
    ~source_owner
    ~actor
    ~authorize
    ~issuance:(Deferred_issuance.Initial request)
    f
;;

let with_recurrence_retry_source_reported
      t
      ~source_owner
      ~observation
      ~actor
      ~authorize
      ~intent
      ~previous
      ~request
      f
  =
  with_deferred_source_reported
    t
    ~source_owner
    ~actor
    ~authorize
    ~issuance:(Deferred_issuance.Retry { observation; intent; previous; request })
    f
;;

let resume_recurrence_retry_source_reported t ~source_owner ~retry ~actor ~authorize f =
  with_deferred_source_reported
    t
    ~source_owner
    ~actor
    ~authorize
    ~issuance:(Deferred_issuance.Resume retry)
    f
;;

module Held_runtime_residency = struct
  type t =
    | Local of Agent_session.Runtime_builder.t
    | Reused of Agent_session.Runtime_builder.t
    | Installed of Agent_session.Runtime_builder.t
    | Handed_off of Agent_session.Runtime_builder.t
end

let bind_existing_goal_source owner stage attempted ~label ~source ~bind ~wrap =
  let open Result.Let_syntax in
  let%bind () = consume_source_selection owner stage attempted ~label in
  Eio.Cancel.protect (fun () ->
    let result = bind source in
    Result.iter result ~f:(fun binding ->
      let owned = wrap binding in
      stage.binding <- Some owned;
      if stage.closed then Source_stage_binding.close owned);
    result)
;;

module Goal_resume_source = struct
  type owner = t

  type t =
    { owner : owner
    ; stage : source_stage
    ; preparation : Agent_session.Session_actor.Goal_continuation.Preparation.t
    ; source_attempted : bool ref
    }

  let preparation t = t.preparation

  let prepare t ~source =
    let module Actor = Agent_session.Session_actor in
    let open Result.Let_syntax in
    let%bind binding =
      bind_existing_goal_source
        t.owner
        t.stage
        t.source_attempted
        ~label:"Goal resume source"
        ~source
        ~bind:(fun source ->
          Actor.bind_goal_continuation_source
            t.owner.actor
            ~preparation:t.preparation
            ~source)
        ~wrap:(fun binding -> Source_stage_binding.Existing_goal binding)
    in
    let prepared : Prepared_goal_continuation.t =
      { stage = t.stage; binding; basis = source_stage_basis t.owner t.stage }
    in
    let%bind () = Prepared_goal_continuation.check_basis prepared in
    Eio.Fiber.check ();
    Ok prepared
  ;;

  module Decision = struct
    type 'a t =
      | Retained of Agent_session.Goal_command_receipt.t
      | Fresh of 'a
  end

  module Report = struct
    type 'a t =
      { preparation : Agent_session.Session_actor.Goal_continuation.Preparation.t option
      ; outcome : 'a Decision.t Scope_outcome.t
      }

    let preparation t = t.preparation
    let outcome t = t.outcome
  end
end

let with_goal_resume_source_reported t ~source_owner ~actor ~authorize ~session ~request f
  =
  let module Actor = Agent_session.Session_actor in
  let preparation = ref None in
  let outcome =
    match Scope_outcome.capture (fun () -> check_source_owner t source_owner) with
    | Raised (exn, backtrace) ->
      { Scope_outcome.primary = Raised (exn, backtrace); cleanup = Never_acquired }
    | Returned (Error failure) ->
      { Scope_outcome.primary = Returned (Error failure); cleanup = Never_acquired }
    | Returned (Ok ()) ->
      with_source_stage_reported t (fun stage ->
        let open Result.Let_syntax in
        let%bind decision =
          Eio.Cancel.protect (fun () ->
            let result =
              Actor.begin_goal_resume_preparation
                t.actor
                ~actor
                ~authorize
                ~session
                ~request
            in
            Result.iter result ~f:(function
              | Actor.Goal_continuation.Resume_issuance.Prepare issued ->
                preparation := Some issued
              | Retained _ -> ());
            result)
        in
        match decision with
        | Retained receipt -> Ok (Goal_resume_source.Decision.Retained receipt)
        | Prepare issued ->
          let source =
            { Goal_resume_source.owner = t
            ; stage
            ; preparation = issued
            ; source_attempted = ref false
            }
          in
          Eio.Fiber.check ();
          Result.map (f source) ~f:(fun value -> Goal_resume_source.Decision.Fresh value))
  in
  { Goal_resume_source.Report.preparation = !preparation; outcome }
;;

module Goal_execution_source = struct
  type owner = t

  type t =
    { owner : owner
    ; stage : source_stage
    ; preparation : Agent_session.Session_actor.Goal_execution_runtime.Preparation.t
    ; source_attempted : bool ref
    }

  let prepare t ~source =
    let module Actor = Agent_session.Session_actor in
    let open Result.Let_syntax in
    let%bind binding =
      bind_existing_goal_source
        t.owner
        t.stage
        t.source_attempted
        ~label:"Goal execution source"
        ~source
        ~bind:(fun source ->
          Actor.bind_goal_execution_runtime_source
            t.owner.actor
            ~preparation:t.preparation
            ~source)
        ~wrap:(fun binding -> Source_stage_binding.Existing_goal_execution binding)
    in
    let prepared : Prepared_goal_execution.t =
      { stage = t.stage; binding; basis = source_stage_basis t.owner t.stage }
    in
    let%bind () = Prepared_goal_execution.check_basis prepared in
    Eio.Fiber.check ();
    Ok prepared
  ;;

  module Decision = struct
    type 'a t =
      | No_work
      | Suspended_goal of Agent_session.Goal_execution_selection.Suspension.t
      | Fresh of 'a
  end

  module Report = struct
    type 'a t =
      { preparation :
          Agent_session.Session_actor.Goal_execution_runtime.Preparation.t option
      ; outcome : 'a Decision.t Scope_outcome.t
      }

    let preparation t = t.preparation
    let outcome t = t.outcome
  end
end

module Goal_execution_issuance = struct
  type t =
    | Wake of Agent_protocol.Run_wake.t
    | Claimed_job of Agent_session.Session_actor.Job_runtime_owner.t

  let issue t actor =
    let module Actor = Agent_session.Session_actor in
    match t with
    | Wake wake -> Actor.begin_goal_wake_preparation actor ~wake
    | Claimed_job owner ->
      Actor.begin_goal_job_runtime_preparation actor ~owner
      |> Result.map ~f:(fun preparation ->
        Actor.Goal_execution_runtime.Wake_issuance.Prepare preparation)
  ;;
end

let with_goal_execution_source_reported t ~source_owner ~issuance f =
  let module Actor = Agent_session.Session_actor in
  let preparation = ref None in
  let outcome =
    match Scope_outcome.capture (fun () -> check_source_owner t source_owner) with
    | Raised (exn, backtrace) ->
      { Scope_outcome.primary = Raised (exn, backtrace); cleanup = Never_acquired }
    | Returned (Error failure) ->
      { Scope_outcome.primary = Returned (Error failure); cleanup = Never_acquired }
    | Returned (Ok ()) ->
      with_source_stage_reported t (fun stage ->
        let open Result.Let_syntax in
        let%bind decision =
          Eio.Cancel.protect (fun () ->
            let result = Goal_execution_issuance.issue issuance t.actor in
            Result.iter result ~f:(function
              | Actor.Goal_execution_runtime.Wake_issuance.Prepare issued ->
                preparation := Some issued
              | No_work | Suspended_goal _ -> ());
            result)
        in
        match decision with
        | No_work -> Ok Goal_execution_source.Decision.No_work
        | Suspended_goal suspension -> Ok (Suspended_goal suspension)
        | Prepare issued ->
          let source =
            { Goal_execution_source.owner = t
            ; stage
            ; preparation = issued
            ; source_attempted = ref false
            }
          in
          Eio.Fiber.check ();
          Result.map (f source) ~f:(fun result ->
            Goal_execution_source.Decision.Fresh result))
  in
  { Goal_execution_source.Report.preparation = !preparation; outcome }
;;

let with_goal_wake_source_reported t ~source_owner ~wake f =
  with_goal_execution_source_reported
    t
    ~source_owner
    ~issuance:(Goal_execution_issuance.Wake wake)
    f
;;

let with_goal_job_source_reported t ~source_owner ~owner f =
  with_goal_execution_source_reported
    t
    ~source_owner
    ~issuance:(Goal_execution_issuance.Claimed_job owner)
    f
;;

module Runtime_request = struct
  module Actor = Agent_session.Session_actor

  type _ t =
    | Initial :
        { prepared : Prepared.t
        ; held : Actor.Held_run.t
        ; checked : Actor.Admitted_activation.Checked.t
        }
        -> Actor.Admitted_activation.Continuation.t t
    | Existing_goal :
        { prepared : Prepared_goal_continuation.t
        ; continuation : Actor.Goal_continuation.t
        }
        -> Actor.Goal_continuation.t t
    | Existing_goal_execution :
        { prepared : Prepared_goal_execution.t
        ; execution : Actor.Goal_execution_runtime.t
        }
        -> Actor.Goal_execution_runtime.t t

  let stage : type a. a t -> source_stage = function
    | Initial { prepared; _ } -> prepared.stage
    | Existing_goal { prepared; _ } -> prepared.stage
    | Existing_goal_execution { prepared; _ } -> prepared.stage
  ;;

  let basis : type a. a t -> unit -> (unit, Agent_protocol.Error.t) result = function
    | Initial { prepared; _ } -> prepared.basis
    | Existing_goal { prepared; _ } -> prepared.basis
    | Existing_goal_execution { prepared; _ } -> prepared.basis
  ;;

  let check_prepared : type a. a t -> (unit, Agent_protocol.Error.t) result = function
    | Initial { prepared; _ } -> Prepared.check_basis prepared
    | Existing_goal { prepared; continuation } ->
      let open Result.Let_syntax in
      let%bind () =
        Actor.Goal_continuation.check_source_binding
          continuation
          ~binding:prepared.binding
      in
      Prepared_goal_continuation.check_basis prepared
    | Existing_goal_execution { prepared; execution } ->
      let open Result.Let_syntax in
      let%bind () =
        Actor.Goal_execution_runtime.check_source_binding
          execution
          ~binding:prepared.binding
      in
      Prepared_goal_execution.check_basis prepared
  ;;

  let source : type a. a t -> Agent_session.Runtime_builder.Source.t = function
    | Initial { prepared; _ } -> Actor.Source_binding.source prepared.binding
    | Existing_goal { prepared; _ } ->
      Actor.Goal_continuation.Source_binding.source prepared.binding
    | Existing_goal_execution { prepared; _ } ->
      Actor.Goal_execution_runtime.Source_binding.source prepared.binding
  ;;

  let acquire : type a. Actor.t -> a t -> (a, Agent_protocol.Error.t) result =
    fun actor -> function
    | Initial { prepared; held; checked } ->
      Actor.begin_held_activation actor ~binding:prepared.binding ~held ~checked
    | Existing_goal { prepared; continuation } ->
      Actor.Goal_continuation.check_source_binding continuation ~binding:prepared.binding
      |> Result.map ~f:(fun () -> continuation)
    | Existing_goal_execution { prepared; execution } ->
      Actor.Goal_execution_runtime.check_source_binding
        execution
        ~binding:prepared.binding
      |> Result.map ~f:(fun () -> execution)
  ;;

  let construction : type a. a t -> a -> Actor.Runtime_construction.t =
    fun request active ->
    match request with
    | Initial _ -> Actor.Runtime_construction.of_admitted active
    | Existing_goal _ -> Actor.Runtime_construction.of_goal active
    | Existing_goal_execution _ ->
      Actor.Runtime_construction.of_existing_goal_execution active
  ;;

  let begin_cold
    : type a.
      Actor.t
      -> a t
      -> Actor.Runtime_construction.t
      -> (unit, Agent_protocol.Error.t) result
    =
    fun actor request construction ->
    match request with
    | Initial _ -> Ok ()
    | Existing_goal _ | Existing_goal_execution _ ->
      Actor.begin_goal_runtime_construction actor ~construction
  ;;
end

let with_runtime_construction_internal (type a) t (request : a Runtime_request.t) f =
  let module Actor = Agent_session.Session_actor in
  let module Construction = Actor.Runtime_construction in
  let open Result.Let_syntax in
  let stage = Runtime_request.stage request in
  let owner_basis () =
    if
      t.closed
      || Option.is_some t.unloading
      || Option.is_some t.foreground_retirement
      || (not (Option.exists t.prepared_source ~f:(phys_equal stage)))
      || not (Option.equal phys_equal t.runtime stage.runtime)
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"prepared source belongs to another or displaced runtime owner"
           ~retryable:false
           ())
    else Runtime_request.basis request ()
  in
  let%bind () = Runtime_request.check_prepared request in
  let%bind () = owner_basis () in
  let%bind () =
    match stage.runtime, t.build_deferred with
    | None, None ->
      Error
        (Agent_protocol.Error.create
           Invalid_state
           ~message:"runtime owner has no verified-source deferred constructor"
           ~retryable:false
           ())
    | Some _, (Some _ | None) | None, Some _ -> Ok ()
  in
  let construction = ref None in
  let residency : Held_runtime_residency.t option ref = ref None in
  let consumed_basis context =
    if
      (not (Int.equal stage.domain (Stdlib.Domain.self () :> int)))
      || stage.closed
      || not (Option.exists stage.continuation ~f:(phys_equal context))
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"consumed runtime source owner is closed or displaced"
           ~retryable:false
           ())
    else owner_basis ()
  in
  let check_effect context =
    let%bind () = consumed_basis context in
    let%bind () = Construction.refresh context in
    let%bind () = Construction.check_policy context in
    let%bind () = Construction.check_timing context in
    let%bind () = Construction.check_authority context in
    let%bind () = consumed_basis context in
    Construction.check_basis context
  in
  let cleanup () =
    Eio.Cancel.protect (fun () ->
      let ended =
        match !construction with
        | None -> Ok (Ok ())
        | Some context ->
          Construction.close context;
          (try Ok (Actor.end_runtime_construction t.actor ~construction:context) with
           | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
      in
      let closed =
        match !residency with
        | None | Some (Reused _ | Handed_off _) -> Ok ()
        | Some (Installed runtime) ->
          if Option.exists !construction ~f:Construction.handoff_acknowledged
          then (
            (* Actual adoption survives a lost publication/reply for all three
               final outcomes, including a joined Wait or terminal settlement. *)
            residency := Some (Handed_off runtime);
            Ok ())
          else (
            stage.retire_installed <- Some runtime;
            Ok ())
        | Some (Local runtime) ->
          residency := None;
          (try
             runtime.Agent_session.Runtime_builder.close ();
             Ok ()
           with
           | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
      in
      match ended, closed with
      | Error failure, _ -> Error failure
      | Ok (Error error), _ -> Ok (Error error)
      | Ok (Ok ()), Error failure -> Error failure
      | Ok (Ok ()), Ok () -> Ok (Ok ()))
  in
  let result =
    try
      let result =
        let%bind active, context =
          Eio.Cancel.protect (fun () ->
            let%map active = Runtime_request.acquire t.actor request in
            let context = Runtime_request.construction request active in
            construction := Some context;
            stage.continuation <- Some context;
            if stage.closed then Construction.close context;
            active, context)
        in
        Eio.Fiber.check ();
        let%bind runtime =
          match stage.runtime with
          | Some runtime ->
            residency := Some (Held_runtime_residency.Reused runtime);
            Ok runtime
          | None ->
            let%bind () = check_effect context in
            let%bind () =
              match request with
              | Runtime_request.Initial _ -> Ok ()
              | Existing_goal _ | Existing_goal_execution _ ->
                let%bind () = Runtime_request.begin_cold t.actor request context in
                check_effect context
            in
            (match t.build_deferred with
             | None ->
               Error
                 (Agent_protocol.Error.create
                    Invalid_state
                    ~message:"runtime owner has no verified-source deferred constructor"
                    ~retryable:false
                    ())
             | Some build ->
               let result =
                 build ~source:(Runtime_request.source request) ~construction:context
               in
               Result.iter result ~f:(fun runtime ->
                 residency := Some (Held_runtime_residency.Local runtime));
               result)
        in
        let%bind () =
          match runtime.Agent_session.Runtime_builder.check_execution with
          | None -> Ok ()
          | Some check -> check ()
        in
        let%bind () = check_effect context in
        f runtime active context residency
      in
      (match result with
       | Error _ -> ()
       | Ok _ -> Eio.Fiber.check ());
      Ok result
    with
    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  let cleaned = cleanup () in
  match result, cleaned with
  | Error (exn, backtrace), _ -> Exn.raise_with_original_backtrace exn backtrace
  | Ok (Error _ as failure), _ -> failure
  | Ok result, Ok (Ok ()) -> result
  | Ok (Ok _), Ok (Error error) -> Error error
  | Ok (Ok _), Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let with_held_runtime_internal t ~prepared ~held ~checked f =
  with_runtime_construction_internal
    t
    (Runtime_request.Initial { prepared; held; checked })
    f
;;

let with_held_runtime t ~prepared ~held ~checked f =
  with_held_runtime_internal
    t
    ~prepared
    ~held
    ~checked
    (fun runtime continuation _construction _residency -> f runtime continuation)
;;

let with_held_recurrence_runtime t ~prepared ~held ~checked f =
  with_held_runtime
    t
    ~prepared
    ~held
    ~checked:(Agent_session.Session_actor.Recurrence_activation.Checked.admitted checked)
    f
;;

let install_runtime_for_request
      (type a)
      t
      (request : a Runtime_request.t)
      ~runtime
      ~construction
      ~residency
  =
  let module Actor = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let stage = Runtime_request.stage request in
  let unavailable () =
    Agent_protocol.Error.create
      Conflict
      ~message:"held runtime installation owner was displaced"
      ~retryable:false
      ()
  in
  let%bind () =
    with_owner_lock t ~protect:true (fun () ->
      if
        t.closed
        || Option.is_some t.unloading
        || Option.is_some t.foreground_retirement
        || stage.closed
        || (not (Option.exists t.prepared_source ~f:(phys_equal stage)))
        || (not (Option.equal phys_equal t.runtime stage.runtime))
        || not (Option.exists stage.continuation ~f:(phys_equal construction))
      then Error (unavailable ())
      else Runtime_request.basis request ())
  in
  let install () =
    Actor.install_runtime_construction
      ?automatic_turn_policy:runtime.Agent_session.Runtime_builder.automatic_turn_policy
      ?moderator_manager:runtime.moderator_manager
      t.actor
      ~construction
      ~worker:runtime.worker
      ~inference:(Agent_session.Runtime_builder.inference_execution runtime)
  in
  match !residency with
  | Some (Held_runtime_residency.Reused existing) when phys_equal existing runtime ->
    (match request with
     | Initial _ -> Ok ()
     | Existing_goal _ | Existing_goal_execution _ -> install ())
  | Some (Local existing) when phys_equal existing runtime ->
    Eio.Cancel.protect (fun () ->
      let%bind () = install () in
      with_owner_lock t ~protect:true (fun () ->
        (* Preserve a concurrent newer retirement; the Actor owns this worker
           after its ACK even when Stop closed fresh admission meanwhile. *)
        t.runtime <- Some runtime;
        stage.handoff_runtime <- Some runtime;
        residency := Some (Held_runtime_residency.Installed runtime));
      Ok ())
  | None | Some (Local _ | Reused _ | Installed _ | Handed_off _) ->
    Error (unavailable ())
;;

module Held_input_runtime = struct
  module Phase = struct
    type t =
      | Available
      | Submitted
      | Closed
  end

  type owner = t

  type t =
    { owner : owner
    ; prepared : Prepared.t
    ; runtime : Agent_session.Runtime_builder.t
    ; continuation : Agent_session.Session_actor.Admitted_activation.Continuation.t
    ; construction : Agent_session.Session_actor.Runtime_construction.t
    ; request :
        Agent_session.Session_actor.Admitted_activation.Continuation.t Runtime_request.t
    ; residency : Held_runtime_residency.t option ref
    ; admission : Agent_protocol.Run_receipt.t
    ; mutable phase : Phase.t
    }

  let unavailable () =
    Agent_protocol.Error.create
      Conflict
      ~message:"held input runtime is closed or already submitted"
      ~retryable:false
      ()
  ;;

  let submit t ~prepare =
    let module A = Agent_session.Session_actor in
    let open Result.Let_syntax in
    let%bind () =
      match t.phase with
      | Available
        when Int.equal t.prepared.stage.domain (Stdlib.Domain.self () :> int)
             && (not t.prepared.stage.closed)
             && Option.exists t.owner.prepared_source ~f:(phys_equal t.prepared.stage)
             && Option.exists t.prepared.stage.continuation ~f:(phys_equal t.construction)
        ->
        t.phase <- Submitted;
        Ok ()
      | Available | Submitted | Closed -> Error (unavailable ())
    in
    A.with_held_input
      t.owner.actor
      ~continuation:t.continuation
      ~prepare:(fun ~scope _execution ->
        Eio.Fiber.check ();
        prepare ~runtime:t.runtime ~scope)
      (fun ~scope ~input ->
         let%bind () =
           install_runtime_for_request
             t.owner
             t.request
             ~runtime:t.runtime
             ~construction:t.construction
             ~residency:t.residency
         in
         Eio.Fiber.check ();
         (* The root ACK precedes this bookkeeping. Keep the exact original
            Admission, rather than manufacturing a receipt from the submission. *)
         Eio.Cancel.protect (fun () ->
           let%map _submission = A.submit_held_run_input t.owner.actor ~scope ~input in
           t.residency := Some (Handed_off t.runtime);
           t.admission))
  ;;
end

let with_held_input_runtime t ~prepared ~held ~checked f =
  with_held_runtime_internal
    t
    ~prepared
    ~held
    ~checked
    (fun runtime continuation construction residency ->
       prepared.stage.handoff_runtime <- Some runtime;
       let input_runtime : Held_input_runtime.t =
         { owner = t
         ; prepared
         ; runtime
         ; continuation
         ; construction
         ; request = Runtime_request.Initial { prepared; held; checked }
         ; residency
         ; admission = Agent_session.Session_actor.Held_run.admission held
         ; phase = Available
         }
       in
       Exn.protect
         ~finally:(fun () -> input_runtime.phase <- Closed)
         ~f:(fun () -> f input_runtime))
;;

let start_held_authored_runtime t ~prepared ~held ~checked =
  with_held_runtime_internal
    t
    ~prepared
    ~held
    ~checked
    (fun runtime continuation construction residency ->
       let open Result.Let_syntax in
       let%bind startup = Agent_session.Runtime_builder.authored_start runtime in
       prepared.stage.handoff_runtime <- Some runtime;
       let%bind () =
         install_runtime_for_request
           t
           (Runtime_request.Initial { prepared; held; checked })
           ~runtime
           ~construction
           ~residency
       in
       Eio.Fiber.check ();
       (* The Actor joins the actual lifecycle callback and releases the aggregate.
       Do not shield that work from cancellation. Its immutable ACK also lets
       ordinary scoped cleanup reconcile a lost post-commit reply. *)
       let result =
         Agent_session.Session_actor.start_held_authored_run
           t.actor
           ~continuation
           ~startup
       in
       Result.iter result ~f:(fun _ ->
         residency := Some (Held_runtime_residency.Handed_off runtime));
       result)
;;

let with_joined_existing_goal_runtime
      (type a)
      t
      (request : a Runtime_request.t)
      ~run_resume
      f
  =
  let module Actor = Agent_session.Session_actor in
  let module Construction = Actor.Runtime_construction in
  with_runtime_construction_internal
    t
    request
    (fun runtime active construction residency ->
       let open Result.Let_syntax in
       let%bind resume =
         match !residency with
         | Some (Held_runtime_residency.Reused _) -> Ok None
         | Some (Local _) ->
           Agent_session.Runtime_builder.runtime_resume runtime
           |> Result.map ~f:Option.some
         | None | Some (Installed _ | Handed_off _) ->
           Error
             (Agent_protocol.Error.create
                Conflict
                ~message:"Goal runtime residency changed before installation"
                ~retryable:false
                ())
       in
       (Runtime_request.stage request).handoff_runtime <- Some runtime;
       let%bind () =
         install_runtime_for_request t request ~runtime ~construction ~residency
       in
       Eio.Fiber.check ();
       let%bind () =
         match resume with
         | None -> Ok ()
         | Some resume -> run_resume active resume
       in
       let result = f runtime active construction in
       if Construction.handoff_acknowledged construction
       then residency := Some (Held_runtime_residency.Handed_off runtime);
       result)
;;

let resume_goal_episode t ~prepared ~continuation =
  let module Actor = Agent_session.Session_actor in
  with_joined_existing_goal_runtime
    t
    (Runtime_request.Existing_goal { prepared; continuation })
    ~run_resume:(fun continuation resume ->
      Actor.run_goal_runtime_resume t.actor ~continuation ~resume)
    (fun _runtime continuation _construction ->
       (* A joined lifecycle may really Wait or settle. A command receipt alone
         does not permit another Operation; preserve its actual final decision. *)
       Result.bind (Actor.continue_goal_episode t.actor ~continuation) ~f:(function
         | (Actor.Goal_resume_result.Resumed _ | Retained _) as result -> Ok result
         | Resume_failed { receipt = _; error } | Rejected error | Uncertain error ->
           Error error))
;;

let with_goal_job_runtime t ~prepared ~execution f =
  let module Actor = Agent_session.Session_actor in
  let open Result.Let_syntax in
  let%bind () =
    match Actor.Goal_execution_runtime.kind execution with
    | Claimed_job -> Ok ()
    | Queued_wake | Subscription_wake ->
      Error
        (Agent_protocol.Error.invalid_request
           "Goal Job runtime requires its actual Job owner")
  in
  with_joined_existing_goal_runtime
    t
    (Runtime_request.Existing_goal_execution { prepared; execution })
    ~run_resume:(fun execution resume ->
      Actor.run_goal_execution_runtime_resume t.actor ~execution ~resume)
    (fun runtime execution _construction ->
       let%bind () = Actor.acknowledge_goal_job_runtime t.actor ~execution in
       Eio.Fiber.check ();
       f runtime)
;;

let with_unloaded t f =
  let result =
    (* An actor request continues after its caller cancels. Keep the owner until
       its bounded checkpoint actually returns, rather than permitting reload
       while that actor is still inspecting/deleting cache-dependent artifacts. *)
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      try
        Ok
          (match t.closed, t.unloading, t.runtime, t.background_leases with
           | false, None, None, [] -> Result.map (f ()) ~f:Option.some
           | _ -> Ok None)
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let reserve_inactive_close t =
  with_owner_lock t ~protect:true (fun () ->
    match t.closed, t.unloading, t.runtime, t.background_leases with
    | false, None, None, [] ->
      t.closed <- true;
      true
    | _ -> false)
;;

let retire_after_administration t =
  Eio.Cancel.protect (fun () ->
    (match
       Agent_session.Session_actor.set_runtime_worker t.actor ~worker:None ~inference:None
     with
     | Ok () -> ()
     | Error error -> raise (Cleanup_failed error));
    let previous = t.runtime in
    t.runtime <- None;
    Option.iter previous ~f:(fun runtime ->
      ignore
        (Result.try_with runtime.Agent_session.Runtime_builder.close : (unit, exn) result)))
;;

let with_administration t f =
  let result =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      try
        Ok
          (if t.closed
           then Error (closed_error ())
           else if Option.is_some t.unloading || not (List.is_empty t.background_leases)
           then Error (background_busy ())
           else (
             match f () with
             | Error _ as failure -> failure
             | Ok _ as result ->
               retire_after_administration t;
               result))
      with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let retire_administration t ~validate ~commit =
  with_owner_lock t ~protect:false (fun () ->
    let open Result.Let_syntax in
    let%bind () =
      if t.closed
      then Error (closed_error ())
      else if Option.is_some t.unloading || not (List.is_empty t.background_leases)
      then Error (background_busy ())
      else validate ()
    in
    let%bind () = unload_locked t in
    commit ())
;;

let reinitialize_administration t ~validate ~commit ~before_initialize =
  with_owner_lock t ~protect:false (fun () ->
    let open Result.Let_syntax in
    let%bind () =
      if t.closed
      then Error (closed_error ())
      else if Option.is_some t.unloading || not (List.is_empty t.background_leases)
      then Error (background_busy ())
      else validate ()
    in
    let%bind () = unload_locked t in
    let%bind () = commit () in
    before_initialize ();
    let%bind () = ensure_loaded_locked t in
    let%bind state = Agent_session.Session_actor.state t.actor in
    let%map () =
      match state.lifecycle.desired with
      | Stopped -> unload_locked t
      | Running -> Ok ()
    in
    Agent_session.Session_state.summary state)
;;

let parse_user_content t ~id content =
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_actor.exclude_effect_deferred_execution t.actor in
  Eio.Mutex.use_ro t.mutex (fun () ->
    match t.closed, t.runtime with
    | true, _ -> Error (closed_error ())
    | false, _ when prepared_source_blocks_runtime t -> Error (background_busy ())
    | false, Some runtime -> runtime.parse_user_content ~id content
    | false, None ->
      Error
        (Agent_protocol.Error.create
           Invalid_state
           ~message:"session runtime is not loaded"
           ~retryable:true
           ()))
;;

let with_activity (runtime : Agent_session.Runtime_builder.t) f =
  match runtime.activity with
  | None -> f ()
  | Some activity ->
    (try Agent_session.Runtime_activity.run activity f with
     | Agent_session.Runtime_activity.Closed ->
       Error
         (Agent_protocol.Error.create
            Interrupted
            ~message:"generated runtime execution scope is closed"
            ~retryable:false
            ())
     | Eio.Cancel.Cancelled _ as exn ->
       (match Eio.Fiber.is_cancelled () with
        | true -> raise exn
        | false ->
          Error
            (Agent_protocol.Error.create
               Interrupted
               ~message:"generated runtime execution scope was cancelled"
               ~retryable:false
               ())))
;;

let with_cancellable_access t f =
  (* Moderator checkpoints may reauthorize delegated calls through this owner.
     Keep the runtime alive with a cancellable lease, without holding the owner
     mutex while waiting for or executing a moderator checkpoint. Unload cancels
     and joins the lease before closing its runtime. The separate work mutex
     preserves serialization between delivery and idle draining without blocking
     queries of the runtime's immutable moderation identity. *)
  match
    with_background_runtime t (fun runtime ->
      Eio.Mutex.use_ro t.moderator_work_mutex (fun () ->
        with_activity runtime (fun () -> f runtime)))
  with
  | result -> result
  | exception (Eio.Cancel.Cancelled _ as exn) ->
    (* Retirement cancels its delivery lease, not the shared scheduler. Let
       caller/shutdown cancellation propagate, but keep a local interruption
       from cancelling the daemon switch and its still-needed actor loops. *)
    (match Eio.Fiber.is_cancelled () with
     | true -> raise exn
     | false ->
       Error
         (Agent_protocol.Error.create
            Interrupted
            ~message:"runtime delivery was cancelled"
            ~retryable:false
            ()))
;;

let submit_ingress t ~producer ~registration_id ~namespace ~key ~payload =
  with_cancellable_access t (fun runtime ->
    let module A = Agent_session.Session_actor in
    let open Result.Let_syntax in
    let%bind runtime, source =
      match runtime.moderator_script_tools with
      | Some _ ->
        Option.bind
          runtime.moderator_manager
          ~f:Chat_response.Moderator_manager.invocation_observer
        |> Option.map ~f:(fun source -> runtime, source)
        |> Result.of_option
             ~error:
               (Agent_protocol.Error.invalid_request
                  "ingress requires a qualified moderator")
      | _ -> Error (Agent_protocol.Error.invalid_request "ingress runtime is unavailable")
    in
    A.with_moderator_checkpoint t.actor (fun () ->
      let%bind decision =
        A.prepare_ingress_submission
          t.actor
          ~source
          ~producer
          ~registration_id
          ~namespace
          ~key
          ~payload
      in
      match decision with
      | Duplicate receipt -> Ok receipt
      | Enqueue proposal ->
        let%bind frame = Agent_session.Ingress_submission.frame proposal in
        let%bind payload =
          Chat_response.Ingress_delivery.capture frame
          |> Chatml.Chatml_value_codec.Snapshot.of_value
          |> Result.map ~f:Chatml.Chatml_value_codec.Snapshot.to_jsonaf
          |> Result.map_error ~f:Agent_protocol.Error.invalid_request
        in
        let committed = ref None in
        let%bind _ =
          runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
            let%map receipt =
              A.commit_ingress_submission t.actor proposal ~before ~snapshot
            in
            committed := Some receipt)
        in
        Result.of_option
          !committed
          ~error:
            (Agent_protocol.Error.invalid_request
               "ingress queue did not commit a receipt")))
;;

let deliver_schedule t (schedule : Agent_protocol.Schedule.t) =
  with_cancellable_access t (fun runtime ->
    let open Result.Let_syntax in
    let%bind () =
      match schedule.ownership with
      | None -> Ok ()
      | Some ownership ->
        (match
           Option.bind
             runtime.moderator_manager
             ~f:Chat_response.Moderator_manager.invocation_observer
         with
         | Some source
           when Agent_protocol.Invocation.equal_observer source ownership.source -> Ok ()
         | _ ->
           Error
             (Agent_protocol.Error.create
                Permission_denied
                ~message:"timer belongs to a different moderator source"
                ~retryable:false
                ()))
    in
    let%bind payload =
      match schedule.ownership with
      | None -> Ok schedule.payload
      | Some _ ->
        let module V = Chatml.Chatml_value_codec in
        Chat_response.Schedule_delivery.capture schedule
        |> Result.bind ~f:V.Snapshot.of_value
        |> Result.map ~f:V.Snapshot.to_jsonaf
        |> Result.map_error ~f:Agent_protocol.Error.invalid_request
    in
    Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
      runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
        Agent_session.Session_actor.complete_schedule
          ~expected:before
          ~expected_schedule:schedule
          ?moderator_manager:runtime.moderator_manager
          t.actor
          ~schedule_id:schedule.id
          ~generation:schedule.generation
          ~moderator_snapshot:
            (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
        |> Result.map ~f:ignore)
      |> Result.map ~f:ignore))
;;

let fail_idle_moderator t failure =
  ignore
    (Agent_session.Session_actor.fail_idle_moderator t.actor failure
     : (unit, Agent_protocol.Error.t) result);
  Error failure
;;

let complete_idle_moderator t drain =
  match Agent_session.Session_actor.complete_idle_moderator t.actor drain with
  | Ok () -> Ok drain.Agent_session.Runtime_builder.remaining_events
  | Error failure -> fail_idle_moderator t failure
;;

let idle_callback_limit t =
  let open Result.Let_syntax in
  let%map state = Agent_session.Session_actor.state t.actor in
  match state.automatic_turn_budget with
  | None -> 32
  | Some budget ->
    (match
       Chat_response.Automatic_turn_policy.has_pause_condition
         budget.policy
         Pause_internal_event_drains
     with
     | true -> 0
     | false -> Int.min 32 budget.policy.budget.max_internal_event_drain)
;;

let drain_loaded_legacy_events t runtime =
  match Agent_session.Session_actor.claim_idle_moderator t.actor with
  | Error _ as failure -> failure
  | Ok None -> Ok false
  | Ok (Some history) ->
    (match runtime.Agent_session.Runtime_builder.drain_internal_events history with
     | Ok drain -> complete_idle_moderator t drain
     | Error failure -> fail_idle_moderator t failure)
;;

let drain_loaded_queued_events
      ~max_events
      t
      (runtime : Agent_session.Runtime_builder.t)
      manager
  =
  let module A = Agent_session.Session_actor in
  let module M = Chat_response.Moderator_manager in
  let open Result.Let_syntax in
  let history = ref [] in
  let claim ~snapshot handle =
    A.with_current_idle_queued_moderator_event_tools
      t.actor
      ~snapshot
      (fun
          ~executing
           ~model_jobs
           ~allocate_inference
           ~retirement_reason
           ~event
           ~execute
           ~commit
         ->
         let%bind state = A.state t.actor in
         let%bind entries =
           Agent_session.History_codec.all_of_protocol
             state.conversation.canonical_history
         in
         history := entries;
         handle
           ~executing
           ~model_jobs
           ~allocate_inference
           ~retirement_reason
           ~event
           ~execute
           ~commit)
  in
  let rec loop remaining handled =
    match remaining with
    | 0 -> Ok true
    | _ ->
      let%bind state = A.state t.actor in
      let blocked =
        Option.exists (M.invocation_observer manager) ~f:(fun observer ->
          Agent_session.Queued_moderator_event.has_unsettled_claim ~state ~observer)
      in
      if blocked
      then Ok handled
      else (
        let%bind outcome =
          Agent_session.Moderator_event.run_queued_idle
            ~model_recipes:runtime.model_recipes
            ~claim
            ~run_actions:(A.run_actions t.actor)
            ?script_tools:runtime.Agent_session.Runtime_builder.moderator_script_tools
            ~manager
            ~history:(fun () -> !history)
            ~available_tools:runtime.moderator_tools
            ~session_meta:`Null
            ~now:runtime.now
            ()
        in
        match outcome with
        | None -> Ok handled
        | Some outcome ->
          (match
             Chat_response.Runtime_semantics.should_end_session outcome.runtime_requests
           with
           | Some _ -> Ok true
           | None -> loop (remaining - 1) true))
  in
  loop max_events false
;;

let drain_loaded_owned_legacy_events ~max_events t drain =
  let module A = Agent_session.Session_actor in
  let module B = Agent_session.Runtime_builder in
  let open Result.Let_syntax in
  let rec loop remaining handled =
    match remaining with
    | 0 -> Ok true
    | _ ->
      let%bind state = A.state t.actor in
      (* Legacy source identity is real checkpoint DATA, not a v1 invocation
         capability. Use the same retained failed/interrupted-head guard as
         admission before EACH selection, including callbacks after a yield. *)
      let%bind observer = B.moderator_snapshot_observer state.moderator in
      let%bind halted = B.moderator_snapshot_is_halted state.moderator in
      let%bind observer =
        observer
        |> Result.of_option
             ~error:
               (Agent_protocol.Error.create
                  Conflict
                  ~message:"installed legacy drain requires its retained moderator source"
                  ~retryable:false
                  ())
      in
      if
        halted
        || Agent_session.Queued_moderator_event.has_unsettled_claim ~state ~observer
      then Ok handled
      else (
        let%bind history =
          Agent_session.History_codec.all_of_protocol state.conversation.canonical_history
        in
        let%bind outcomes = drain ~max_events:1 ~history in
        match outcomes with
        | [] -> Ok handled
        | [ (outcome : Chat_response.Moderation.Outcome.t) ] ->
          (match
             Chat_response.Runtime_semantics.should_end_session outcome.runtime_requests
           with
           | Some _ -> Ok true
           | None -> loop (remaining - 1) true)
        | _ ->
          Error
            (Agent_protocol.Error.create
               Conflict
               ~message:"installed legacy drain exceeded its selected head budget"
               ~retryable:false
               ()))
  in
  loop max_events false
;;

let drain_loaded_idle_moderator t runtime =
  let open Result.Let_syntax in
  let%bind max_events = idle_callback_limit t in
  match max_events with
  | 0 -> Ok false
  | _ ->
    (match runtime.Agent_session.Runtime_builder.moderator_manager with
     | Some manager
       when Option.is_some (Chat_response.Moderator_manager.extension_definition manager)
       ->
       let open Result.Let_syntax in
       let%bind more = drain_loaded_queued_events ~max_events t runtime manager in
       let%map applied = Agent_session.Session_actor.apply_moderator_follow_up t.actor in
       more || applied
     | Some _ ->
       (match runtime.legacy_events with
        | Some drain ->
          let%bind more = drain_loaded_owned_legacy_events ~max_events t drain in
          let%map applied =
            Agent_session.Session_actor.apply_moderator_follow_up t.actor
          in
          more || applied
        | None -> Eio.Cancel.protect (fun () -> drain_loaded_legacy_events t runtime))
     | None -> Ok false)
;;

let pending_observation state observer =
  List.exists state.Agent_session.Session_state.invocations ~f:(fun invocation ->
    invocation.context.generation = state.identity.generation
    && Agent_protocol.Id.Session.equal
         invocation.context.session_id
         state.identity.session_id
    &&
    match invocation.observation, invocation.status with
    | Some { observer = owner; status = Awaiting; _ }, (Resolved _ | Published _) ->
      Agent_protocol.Invocation.equal_observer owner observer
    | _ -> false)
;;

let drain_loaded_observations t runtime =
  let open Result.Let_syntax in
  let%bind max_observations = idle_callback_limit t in
  match max_observations with
  | 0 -> Ok false
  | _ ->
    (match
       Option.bind
         runtime.Agent_session.Runtime_builder.moderator_manager
         ~f:(fun manager ->
           Option.map
             (Chat_response.Moderator_manager.invocation_observer manager)
             ~f:(fun observer -> manager, observer))
     with
     | None -> Ok false
     | Some (manager, observer) ->
       let history = ref [] in
       let with_history handle =
         let%bind state = Agent_session.Session_actor.state t.actor in
         let%bind entries =
           Agent_session.History_codec.all_of_protocol
             state.conversation.canonical_history
         in
         history := entries;
         handle ()
       in
       let drain =
         match
           ( runtime.moderator_script_tools
           , Chat_response.Moderator_manager.extension_definition manager )
         with
         | Some script_tools, Some definition ->
           Agent_session.Moderator_observation.drain_idle_with_tools
             ~model_recipes:runtime.model_recipes
             ~max_observations
             ~script_tools
             ~definition
             ~claim:(fun handle ->
               Agent_session.Session_actor.with_idle_moderator_observation_tools
                 t.actor
                 ~observer
                 (fun ~observing ~capabilities ~execute ~commit ->
                    with_history (fun () ->
                      handle ~observing ~capabilities ~execute ~commit)))
         | _ ->
           Agent_session.Moderator_observation.drain_idle
             ~model_recipes:runtime.model_recipes
             ~max_observations
             ~on_tool_call:(fun ~name:_ ~args:_ ->
               Ok
                 (Chat_response.Moderation.Capabilities.Tool_error
                    "invocation.unavailable"))
             ~claim:(fun handle ->
               Agent_session.Session_actor.with_idle_moderator_observation
                 t.actor
                 ~observer
                 (fun ~observing ~capabilities ~commit ->
                    with_history (fun () -> handle ~observing ~capabilities ~commit)))
       in
       let%map drain =
         drain
           ~manager
           ~history:(fun () -> !history)
           ~available_tools:runtime.moderator_tools
           ~session_meta:`Null
           ~now:runtime.now
           ()
       in
       drain.budget_exhausted)
;;

let drain_loaded_notifications runtime =
  match runtime.Agent_session.Runtime_builder.moderator_activation with
  | Some activation when activation.pending () -> Ok false
  | _ ->
    (match runtime.idle_notifications with
     | None -> Ok false
     | Some deliver -> deliver ())
;;

let snapshot_has_pending_events t =
  let open Result.Let_syntax in
  let%bind state = Agent_session.Session_actor.state t.actor in
  if
    Agent_protocol.Session.equal_desired_state state.lifecycle.desired Running
    && (match state.lifecycle.observed with
        | Agent_protocol.Session.Idle -> true
        | Stopped
        | Queued_for_slot
        | Starting
        | Recovering
        | Running_turn _
        | Compacting _
        | Waiting_for_permission _
        | Stopping
        | Failed _ -> false)
    && Option.is_none state.active_operation
    && (not state.halted)
    && Option.is_none state.failure
  then (
    let%bind queued =
      Agent_session.Runtime_builder.moderator_snapshot_has_queued_events state.moderator
    in
    let%bind halted =
      Agent_session.Runtime_builder.moderator_snapshot_is_halted state.moderator
    in
    let%bind eligibility =
      Agent_session.Pending_eligibility.create
        state
        ~boundary:Idle_start
        ~runtime_admission_open:true
    in
    let%bind pending =
      Agent_session.Pending_eligibility.eligible_prefix
        eligibility
        state.conversation.deferred_user_entries
    in
    let%map observer =
      Agent_session.Runtime_builder.moderator_snapshot_observer state.moderator
    in
    Option.exists t.runtime ~f:(fun runtime ->
      Option.exists runtime.moderator_activation ~f:(fun activation ->
        activation.pending ()))
    || (queued
        && (not halted)
        && not
             (Option.exists observer ~f:(fun observer ->
                Agent_session.Queued_moderator_event.has_unsettled_claim ~state ~observer))
       )
    || ((not halted) && not (List.is_empty pending))
    || List.exists state.invocations ~f:Agent_session.Observation_follow_up.pending
    || List.exists
         state.moderator_executions
         ~f:Agent_session.Observation_follow_up.pending_event
    || ((not halted) && Option.exists observer ~f:(pending_observation state))
    || ((not halted) && Agent_session.Notification_delivery.has_idle_work state))
  else Ok false
;;

let drain_idle_moderator_leased t =
  with_lease_internal
    t
    ~survives_stop:false
    ~admit:(fun () ->
      let open Result.Let_syntax in
      let%bind pending = snapshot_has_pending_events t in
      match pending with
      | false -> Ok (Lease_admission.No_work false)
      | true ->
        let%map () = ensure_loaded_locked t in
        Lease_admission.Borrow (t.runtime, None))
    (function
      | None -> Ok false
      | Some runtime ->
        let open Result.Let_syntax in
        let%bind () =
          Agent_session.Session_actor.exclude_effect_deferred_execution t.actor
        in
        Eio.Mutex.use_ro t.moderator_work_mutex (fun () ->
          with_activity runtime (fun () ->
            let%bind notifications = drain_loaded_notifications runtime in
            let%bind applied =
              match notifications with
              | true -> Ok true
              | false -> Agent_session.Session_actor.apply_moderator_follow_up t.actor
            in
            if applied
            then Ok true
            else (
              let%bind activated =
                match runtime.moderator_activation with
                | None -> Ok false
                | Some activation -> activation.run ()
              in
              if activated
              then Ok true
              else (
                let%bind more_observations = drain_loaded_observations t runtime in
                let%bind applied =
                  Agent_session.Session_actor.apply_moderator_follow_up t.actor
                in
                if applied
                then Ok true
                else (
                  let%map more_events = drain_loaded_idle_moderator t runtime in
                  more_observations || more_events))))))
;;

let drain_idle_moderator t =
  let outcome =
    match drain_idle_moderator_leased t with
    | result -> Ok result
    | exception (Eio.Cancel.Cancelled _ as exn) ->
      Error (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  match outcome with
  | Ok result -> result
  | Error (exn, backtrace) ->
    (* Stopping one idle session cancels its owned event sub-context. Once that
       scope has unwound, the scheduler's caller may still be live. Propagating
       that local cancellation out of its worker would fail the shared daemon
       switch and strand other requests. Caller/shutdown cancellation must still
       propagate. The event borrow has already recorded its interruption. *)
    (match Eio.Fiber.is_cancelled () with
     | true -> Exn.raise_with_original_backtrace exn backtrace
     | false ->
       Error
         (Agent_protocol.Error.create
            Interrupted
            ~message:"idle moderator execution was cancelled"
            ~retryable:false
            ()))
;;

let with_locked_runtime t f =
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_actor.exclude_effect_deferred_execution t.actor in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let open Result.Let_syntax in
    let%bind () = ensure_loaded_locked t in
    match t.runtime with
    | Some runtime -> f runtime
    | None ->
      Error
        (Agent_protocol.Error.create
           Internal_error
           ~message:"runtime load completed without an installed runtime"
           ~retryable:false
           ()))
;;

module For_testing = struct
  let with_loaded_runtime t f =
    Eio.Mutex.use_ro t.moderator_work_mutex (fun () ->
      with_locked_runtime t (fun _ -> f ()))
  ;;
end

let execute_model_job
      t
      ~allocate_inference
      ~inference_context
      ~capture_recipe_target
      ~recipe
      ~payload
  =
  with_background_runtime t (fun runtime ->
    runtime.Agent_session.Runtime_builder.execute_model_job
      ~allocate_inference
      ~inference_context
      ~capture_recipe_target
      ~recipe
      ~payload)
;;

type background_result =
  | Completed of Agent_protocol.Completion.t
  | Pending of Agent_protocol.Job.dependency

let execute_background_job t (job : Agent_protocol.Job.t) =
  with_background_runtime t (fun runtime ->
    let open Result.Let_syntax in
    let%bind executor =
      Result.of_option
        runtime.background_executor
        ~error:
          (Agent_protocol.Error.create
             Invalid_state
             ~message:"generic background execution is not configured"
             ~retryable:false
             ())
    in
    let%bind request =
      Chat_response.Background_request.of_json ~policy:executor.policy job.payload
    in
    let%bind deadline =
      Result.try_with (fun () ->
        Time_ns.add
          (Agent_protocol.Timestamp.to_time_ns job.created_at)
          (Time_ns.Span.of_sec
             (Chat_response.Background_request.policy request).execution.wall_seconds)
        |> Agent_protocol.Timestamp.of_time_ns)
      |> Result.map_error ~f:(fun _ ->
        Agent_protocol.Error.invalid_request
          "background deadline is outside the supported timestamp range")
    in
    match Agent_protocol.Timestamp.compare deadline (executor.now ()) <= 0 with
    | true -> Ok (Completed Agent_protocol.Completion.Expired)
    | false ->
      let%bind result =
        Agent_session.Session_actor.with_job_execution
          t.actor
          ~job_id:job.id
          ~generation:job.generation
          ~attempt:job.attempt
          ~deadline:(Some deadline)
          (fun execution ->
             let job = execution.job in
             let is_halted () =
               match Agent_session.Session_actor.state t.actor with
               | Error _ -> true
               | Ok state ->
                 state.halted
                 || Option.is_some state.failure
                 || Agent_protocol.Session.equal_desired_state
                      state.lifecycle.desired
                      Stopped
                 || not
                      (List.exists state.jobs ~f:(fun current ->
                         Agent_protocol.Id.Job.equal current.id job.id
                         && Int.equal current.generation job.generation
                         && Int.equal current.attempt job.attempt
                         &&
                         match current.status with
                         | Running | Waiting_permission _ -> true
                         | _ -> false))
             in
             executor.run
               ~job
               ~deadline
               ~execute:execution.execute
               ~moderator_execute:execution.moderator_execute
               ~claim_event:execution.claim_event
               ~is_halted
               ~request)
      in
      let%bind () =
        match result.runtime_requests with
        | [] -> Ok ()
        | _ ->
          Error
            (Agent_protocol.Error.create
               Invalid_state
               ~message:"background runtime request consumption is not installed"
               ~retryable:false
               ())
      in
      (match result.pending, result.resolved.status with
       | Some target, Resolved (Complete _) ->
         (match target.invocation.status, target.invocation.context.deadline with
          | Resolved (Pending (work, _)), Some deadline ->
            let policy = Chat_response.Background_request.policy request in
            Ok
              (Pending
                 { invocation_id = target.invocation.context.id
                 ; work
                 ; deadline
                 ; completion_schema = target.completion_schema
                 ; max_output_bytes = policy.max_output_bytes
                 ; max_output_depth = policy.execution.max_depth
                 })
          | _ ->
            Error
              (Agent_protocol.Error.invalid_request
                 "background Pending target has no owned work or deadline"))
       | None, Resolved (Complete value) ->
         Ok (Completed (Agent_protocol.Completion.Succeeded value))
       | _, Resolved (Fail error) ->
         Ok (Completed (Agent_protocol.Completion.Failed error))
       | _, Resolved (Cancelled reason) ->
         Ok (Completed (Agent_protocol.Completion.Cancelled reason))
       | _ ->
         Error
           (Agent_protocol.Error.create
              Invalid_state
              ~message:"background execution did not produce a terminal outcome"
              ~retryable:false
              ())))
;;

let deliver_model_job_completion t (job : Agent_protocol.Job.t) =
  with_cancellable_access t (fun runtime ->
    Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
      runtime.enqueue_model_job_completion job ~prepare:(fun ~before ~snapshot ->
        Agent_session.Session_actor.deliver_job
          ~expected:before
          ~expected_job:job
          ?moderator_manager:runtime.moderator_manager
          t.actor
          ~job_id:job.id
          ~generation:job.generation
          ~moderator_snapshot:
            (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
        |> Result.map ~f:ignore)
      |> Result.map ~f:ignore))
;;

let deliver_moderated_background_job_completion
      t
      (runtime : Agent_session.Runtime_builder.t)
      (job : Agent_protocol.Job.t)
  =
  let open Result.Let_syntax in
  Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
    let%bind state = Agent_session.Session_actor.state t.actor in
    let%bind retired =
      Agent_session.Session_actor.retire_obsolete_moderator_delivery
        t.actor
        ~revision:state.counters.revision
        ~job
    in
    match retired with
    | true -> Ok ()
    | false ->
      let%bind observer =
        match runtime.moderator_manager with
        | None ->
          Error
            (Agent_protocol.Error.invalid_request
               "background completion requires its actual moderator")
        | Some manager ->
          (match Chat_response.Moderator_manager.invocation_observer manager with
           | Some observer -> Ok observer
           | None ->
             Chat_response.Moderator_manager.identity_snapshot manager
             |> Result.map_error ~f:Agent_protocol.Error.invalid_request
             |> Result.map ~f:(fun snapshot ->
               { Agent_protocol.Invocation.script_id = snapshot.script_id
               ; source_sha256 = snapshot.script_source_hash
               }))
      in
      let%bind frame = Agent_session.Background_job_event.frame ~state ~observer job in
      let%bind payload =
        Chat_response.Background_delivery.capture frame
        |> Chatml.Chatml_value_codec.Snapshot.of_value
        |> Result.map ~f:Chatml.Chatml_value_codec.Snapshot.to_jsonaf
        |> Result.map_error ~f:Agent_protocol.Error.invalid_request
      in
      runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
        Agent_session.Session_actor.deliver_job
          ~expected:before
          ~expected_job:job
          ?moderator_manager:runtime.moderator_manager
          t.actor
          ~job_id:job.id
          ~generation:job.generation
          ~moderator_snapshot:
            (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
        |> Result.map ~f:ignore)
      |> Result.map ~f:ignore)
;;

let deliver_run_job_completion t delivery =
  with_cancellable_access t (fun runtime ->
    let open Result.Let_syntax in
    Agent_session.Session_actor.with_moderator_checkpoint t.actor (fun () ->
      let%bind payload =
        Agent_session.Run_job_delivery.frame delivery
        |> Chat_response.Background_delivery.capture
        |> Chatml.Chatml_value_codec.Snapshot.of_value
        |> Result.map ~f:Chatml.Chatml_value_codec.Snapshot.to_jsonaf
        |> Result.map_error ~f:Agent_protocol.Error.invalid_request
      in
      runtime.enqueue_internal_event payload ~prepare:(fun ~before ~snapshot ->
        Agent_session.Session_actor.enqueue_run_job_delivery
          t.actor
          ~delivery
          ~before
          ~after:(Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
      |> Result.map ~f:ignore))
;;

let deliver_background_job_completion t (job : Agent_protocol.Job.t) =
  with_cancellable_access t (fun runtime ->
    let open Result.Let_syntax in
    let%bind state = Agent_session.Session_actor.state t.actor in
    let standalone =
      match job.launch with
      | Some { owner = Invocation id; _ } ->
        List.exists state.invocations ~f:(fun invocation ->
          Agent_protocol.Id.Invocation.equal invocation.context.id id
          && Option.is_some invocation.completion_contract)
      | _ -> false
    in
    match standalone, runtime.standalone_completion with
    | true, Some deliver -> deliver job
    | true, None ->
      Error
        (Agent_protocol.Error.invalid_request "standalone completion adapter unavailable")
    | false, _ -> deliver_moderated_background_job_completion t runtime job)
;;

let close t =
  if Eio.Mutex.use_ro t.mutex (fun () -> t.close_finished)
  then ()
  else (
    let leases =
      Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
        if not t.closed then t.foreground_retirement <- None;
        t.closed <- true;
        Option.iter t.prepared_source ~f:close_source_stage;
        match t.before_unload, t.unloading, t.background_leases with
        | Some _, _, _ -> []
        | None, _, leases -> leases)
    in
    (match Agent_session.Session_actor.retire_runtime_worker t.actor ~closing:true with
     | Error error -> raise (Cleanup_failed error)
     | Ok retirement ->
       with_owner_lock t ~protect:true (fun () ->
         t.foreground_retirement <- Some retirement));
    List.iter leases ~f:(fun lease -> lease.cancel ());
    let retired =
      with_owner_lock t ~protect:true (fun () ->
        match t.before_unload, t.unloading, t.background_leases with
        | None, None, [] when foreground_retirement_finished t ->
          let result = retire_runtime_locked t in
          (match result with
           | Ok () -> t.close_finished <- true
           | Error _ -> ());
          result
        | _ -> Ok ())
    in
    raise_cleanup retired)
;;

let close_and_wait t =
  Eio.Cancel.protect (fun () ->
    match Eio.Mutex.use_ro t.mutex (fun () -> t.close_finished) with
    | true -> ()
    | false ->
      close t;
      (match unload_and_wait t with
       | Ok () ->
         (* Close can overlap an already accepted ordinary unload, which preserves
            delegation resources. Admission is now closed: drain any retained
            leases too before allowing the actor or its writer to be retired. *)
         let leases = Eio.Mutex.use_ro t.mutex (fun () -> t.background_leases) in
         List.iter leases ~f:(fun lease -> lease.cancel ());
         List.iter leases ~f:(fun lease -> Eio.Promise.await lease.finished);
         Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.close_finished <- true)
       | Error error -> raise (Cleanup_failed error)))
;;
