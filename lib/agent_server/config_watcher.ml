open Core

module State = struct
  type t =
    | Available
    | Busy
    | Uncertain
    | Closed
  [@@deriving equal, sexp_of]
end

module Certainty = struct
  type t =
    | Rejected
    | Uncertain
  [@@deriving equal, sexp_of]
end

module Failure = struct
  type t =
    | Rejected of Config.Diagnostic.t list
    | Uncertain of Config.Diagnostic.t list
  [@@deriving sexp_of]

  let diagnostics = function
    | Rejected diagnostics | Uncertain diagnostics -> diagnostics
  ;;
end

module Operation = struct
  type t =
    | Reload
    | Update of Agent_protocol.Prompt_dependency_selection.Update_request.t
end

module Prepared_result = struct
  type t =
    | Reloaded of Config_diff.t
    | Dependencies_updated of Agent_protocol.Prompt_dependency_selection.Update_result.t
end

type hooks =
  { prepare :
      Operation.t
      -> Config_diff.t
      -> Config.t
      -> checkpoint:(unit -> unit)
      -> (Prepared_result.t, Failure.t) Result.t
  ; publish : Operation.t -> Config_diff.t -> Config.t -> (unit, Failure.t) Result.t
  ; commit : Operation.t -> Config_diff.t -> Config.t -> unit
  ; discard_prepared : unit -> Certainty.t
  ; audit : Operation.t -> Config_diff.t -> unit
  ; retire : unit -> unit
  }

type installed =
  { config : Config.t
  ; mtime : float option
  }

type pending =
  { completed : unit Eio.Promise.t
  ; resolve_completed : unit Eio.Promise.u
  ; mutable started : bool
  ; mutable context : Eio.Cancel.t option
  ; mutable cancel_requested : bool
  ; mutable acknowledged : bool
  ; mutable committed : bool
  ; mutable discarded : Certainty.t option
  ; mutable next_state : State.t
  }

type t =
  { domain_id : int
  ; env : Eio_unix.Stdenv.base
  ; path : string
  ; hooks : hooks
  ; activity : Agent_session.Runtime_activity.t
  ; mutex : Eio.Mutex.t
  ; mutable installed : installed
  ; mutable state : State.t
  ; mutable active : pending option
  ; mutable last_error : Config.Diagnostic.t list option
  ; stop : unit Eio.Stream.t
  }

let check_domain t =
  if not (Int.equal t.domain_id (Stdlib.Domain.self () :> int))
  then invalid_arg "Config_watcher: operation belongs to another Eio domain"
;;

let diagnostic t code message =
  Config.Diagnostic.
    { code
    ; config_path = "$"
    ; source_file = t.path
    ; message
    ; remediation = "Reconcile the catalog publication or restart the daemon."
    }
;;

let denied t = function
  | State.Available -> None
  | Busy ->
    Some
      (Failure.Rejected
         [ diagnostic t "config.reload_busy" "a configuration publication is in progress"
         ])
  | Uncertain ->
    Some
      (Failure.Uncertain
         [ diagnostic t "config.reload_uncertain" "catalog publication is uncertain" ])
  | Closed ->
    Some
      (Failure.Rejected [ diagnostic t "config.reload_closed" "the watcher is closed" ])
;;

let mtime env path =
  Eio.Fiber.check ();
  try Some (Eio.Path.stat ~follow:true Eio.Path.(Eio.Stdenv.fs env / path)).mtime with
  | Eio.Io _ -> None
;;

let current t =
  check_domain t;
  Eio.Mutex.use_ro t.mutex (fun () -> t.installed.config)
;;

let status t =
  check_domain t;
  Eio.Mutex.use_ro t.mutex (fun () -> t.state, t.last_error)
;;

let check_admission t =
  check_domain t;
  Eio.Mutex.use_ro t.mutex (fun () ->
    match denied t t.state with
    | None -> Ok ()
    | Some failure -> Error failure)
;;

let cancel_pending pending =
  pending.cancel_requested <- true;
  Option.iter pending.context ~f:(fun context -> Eio.Cancel.cancel context Exit)
;;

let close t =
  check_domain t;
  let active =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match t.state with
      | State.Closed -> None
      | Available | Busy | Uncertain ->
        t.state <- Closed;
        t.active)
  in
  Option.iter active ~f:cancel_pending;
  (* Exactly one token is sufficient even if the polling fiber has not started. *)
  if Eio.Stream.length t.stop = 0 then Eio.Stream.add t.stop ()
;;

let close_and_wait t =
  check_domain t;
  Eio.Cancel.protect (fun () ->
    close t;
    let active = Eio.Mutex.use_ro t.mutex (fun () -> t.active) in
    Option.iter active ~f:(fun pending -> Eio.Promise.await pending.completed))
;;

let create ~sw ~env ~path ~initial ~hooks =
  let activity = Agent_session.Runtime_activity.create ~sw in
  let observed = mtime env path in
  let t =
    { domain_id = (Stdlib.Domain.self () :> int)
    ; env
    ; path
    ; hooks
    ; activity
    ; mutex = Eio.Mutex.create ()
    ; installed = { config = initial; mtime = observed }
    ; state = State.Available
    ; active = None
    ; last_error = None
    ; stop = Eio.Stream.create 1
    }
  in
  Eio.Switch.on_release sw (fun () -> close t);
  t
;;

let load_validated t =
  Eio.Fiber.check ();
  let result =
    let open Result.Let_syntax in
    let%bind raw = Config_parser.load ~env:t.env ~path:t.path in
    Config_validator.validate ~env:t.env raw
  in
  (* The existing configuration diagnostic boundary translates IO failures.
     Check actual cancellation again before admitting any prepared service. *)
  Eio.Fiber.check ();
  Result.map_error result ~f:(fun diagnostics -> Failure.Rejected diagnostics)
;;

let discard t pending =
  let certainty =
    match pending.discarded with
    | Some certainty -> certainty
    | None ->
      let certainty =
        Eio.Cancel.protect (fun () ->
          try t.hooks.discard_prepared () with
          | _ -> Certainty.Uncertain)
      in
      pending.discarded <- Some certainty;
      certainty
  in
  if pending.acknowledged then Certainty.Uncertain else certainty
;;

let reject t pending failure =
  let certainty = discard t pending in
  let failure =
    match failure, certainty with
    | Failure.Uncertain _, (Certainty.Rejected | Uncertain) -> failure
    | Rejected _, Rejected -> failure
    | Rejected diagnostics, Uncertain -> Failure.Uncertain diagnostics
  in
  pending.next_state
  <- (match failure with
      | Failure.Rejected _ -> Available
      | Uncertain _ -> Uncertain);
  t.last_error <- Some (Failure.diagnostics failure);
  Error failure
;;

let postcommit t operation diff =
  match t.hooks.audit operation diff with
  | () -> Eio.Cancel.protect t.hooks.retire
  | exception exn ->
    let trace = Stdlib.Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () ->
      try t.hooks.retire () with
      | _ -> ());
    Exn.raise_with_original_backtrace exn trace
;;

let unchanged_diff =
  let ids : Config_diff.ids = { added = []; removed = []; changed = [] } in
  Config_diff.
    { server_changed = false
    ; workspaces = ids
    ; prompts = ids
    ; permission_profiles = ids
    ; manifest_grants = ids
    }
;;

let candidate t previous = function
  | Operation.Reload ->
    let open Result.Let_syntax in
    let observed = mtime t.env t.path in
    let%map config = load_validated t in
    ( { config; mtime = observed }
    , Config_diff.between ~previous:previous.config ~current:config )
  | Update _ -> Ok (previous, unchanged_diff)
;;

let validate_result t operation diff result =
  let matches =
    match operation, result with
    | Operation.Reload, Prepared_result.Reloaded returned ->
      Config_diff.equal diff returned
    | Update request, Dependencies_updated returned ->
      Agent_protocol.Prompt_dependency_selection.Update_result.matches_request
        returned
        request
    | Reload, Dependencies_updated _ | Update _, Reloaded _ -> false
  in
  if matches
  then Ok ()
  else
    Error
      (Failure.Rejected
         [ diagnostic
             t
             "config.operation_result_mismatch"
             "prepared publication result does not match the admitted operation"
         ])
;;

let poll checkpoint =
  Eio.Fiber.check ();
  checkpoint ();
  Eio.Fiber.check ()
;;

let perform t pending previous operation ~checkpoint =
  let open Result.Let_syntax in
  let%bind next, diff = candidate t previous operation in
  poll checkpoint;
  let%bind prepared = t.hooks.prepare operation diff next.config ~checkpoint in
  let%bind () = validate_result t operation diff prepared in
  (* Everything installed after durable acknowledgment is already allocated.
     Same-domain confinement and a nonyielding commit permit one protected
     pointer installation without invoking any callback under the mutex. *)
  let result = Ok prepared in
  poll checkpoint;
  let%bind () = t.hooks.publish operation diff next.config in
  pending.acknowledged <- true;
  Eio.Fiber.check ();
  Eio.Cancel.protect (fun () ->
    t.hooks.commit operation diff next.config;
    t.installed <- next;
    pending.committed <- true;
    t.last_error <- None);
  postcommit t operation diff;
  result
;;

let finish t pending =
  check_domain t;
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      t.active <- None;
      match t.state with
      | State.Closed -> ()
      | Available | Busy | Uncertain -> t.state <- pending.next_state);
    Eio.Promise.resolve pending.resolve_completed ())
;;

let record_exception t pending =
  if pending.committed
  then
    t.last_error
    <- Some
         [ diagnostic
             t
             "config.postcommit_failure"
             "configuration installed; audit, retirement or delivery failed"
         ]
  else (
    let certainty = discard t pending in
    pending.next_state
    <- (match certainty with
        | Certainty.Rejected -> Available
        | Uncertain -> Uncertain);
    t.last_error
    <- Some [ diagnostic t "config.reload_exception" "reload raised before installation" ])
;;

let run_worker t pending previous operation ~checkpoint =
  pending.started <- true;
  Eio.Cancel.sub (fun context ->
    pending.context <- Some context;
    Fun.protect
      ~finally:(fun () -> pending.context <- None)
      (fun () ->
         match
           if pending.cancel_requested then Eio.Cancel.cancel context Exit;
           Eio.Fiber.check ();
           match perform t pending previous operation ~checkpoint with
           | Ok _ as result -> result
           | Error failure -> reject t pending failure
         with
         | result -> result
         | exception exn ->
           let trace = Stdlib.Printexc.get_raw_backtrace () in
           (* Cleanup belongs to the activity lifetime, so switch closure joins it
             before persistence owners borrowed by these hooks are released. *)
           record_exception t pending;
           Exn.raise_with_original_backtrace exn trace))
;;

let operate t operation ~checkpoint =
  check_domain t;
  Eio.Fiber.check ();
  let completed, resolve_completed = Eio.Promise.create () in
  let pending =
    { completed
    ; resolve_completed
    ; started = false
    ; context = None
    ; cancel_requested = false
    ; acknowledged = false
    ; committed = false
    ; discarded = None
    ; next_state = State.Available
    }
  in
  let admission =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match denied t t.state with
      | Some failure -> Error failure
      | None ->
        let previous = t.installed in
        t.state <- Busy;
        t.active <- Some pending;
        Ok previous)
  in
  match admission with
  | Error failure -> Error failure
  | Ok previous ->
    Fun.protect
      ~finally:(fun () -> finish t pending)
      (fun () ->
         match
           let result =
             Agent_session.Runtime_activity.run t.activity (fun () ->
               run_worker t pending previous operation ~checkpoint)
           in
           (* A resolved promise can deliver after cancellation without checking
              its caller. The worker has already joined all hook cleanup. *)
           Eio.Fiber.check ();
           result
         with
         | result -> result
         | exception exn ->
           let trace = Stdlib.Printexc.get_raw_backtrace () in
           (* A skipped activity admitted no hooks. A caller can also lose result
              delivery after worker completion; do not run borrowed cleanup again. *)
           if pending.started
           then
             if pending.committed
             then
               t.last_error
               <- Some
                    [ diagnostic
                        t
                        "config.postcommit_failure"
                        "configuration installed; result delivery failed"
                    ];
           Exn.raise_with_original_backtrace exn trace)
;;

let reload t =
  match operate t Operation.Reload ~checkpoint:Eio.Fiber.check with
  | Error _ as error -> error
  | Ok (Prepared_result.Reloaded diff) -> Ok diff
  | Ok (Dependencies_updated _) ->
    invalid_arg "Config_watcher: validated reload result changed kind"
;;

let update_dependencies t request ~checkpoint =
  match operate t (Operation.Update request) ~checkpoint with
  | Error _ as error -> error
  | Ok (Prepared_result.Dependencies_updated result) -> Ok result
  | Ok (Reloaded _) -> invalid_arg "Config_watcher: validated update result changed kind"
;;

let changed t =
  check_domain t;
  let previous = Eio.Mutex.use_ro t.mutex (fun () -> t.installed.mtime) in
  not (Option.equal Float.equal previous (mtime t.env t.path))
;;

let rec poll clock every t =
  match fst (status t) with
  | State.Closed | Uncertain -> ()
  | Available | Busy ->
    (match
       Eio.Fiber.first
         (fun () ->
            Eio.Time.sleep clock every;
            `Poll)
         (fun () ->
            Eio.Stream.take t.stop;
            `Stop)
     with
     | `Stop -> ()
     | `Poll ->
       (match fst (status t) with
        | State.Available ->
          if changed t then ignore (reload t : (Config_diff.t, Failure.t) Result.t)
        | Busy | Uncertain | Closed -> ());
       poll clock every t)
;;

let run ~sw ~clock ~every t =
  check_domain t;
  if not (Float.is_finite every && Float.(every > 0.))
  then invalid_arg "Config_watcher.run: every must be positive and finite";
  Eio.Fiber.fork ~sw (fun () -> poll clock every t)
;;
