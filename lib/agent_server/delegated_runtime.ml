open! Core
module P = Agent_protocol
module B = Agent_session.Runtime_builder

exception Cleanup_failed of P.Error.t

let interrupted message = P.Error.create Interrupted ~message ~retryable:false ()

module Outcome = struct
  type 'a t =
    | Returned of ('a, P.Error.t) result
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  let capture f =
    match f () with
    | result -> Returned result
    | exception exn -> Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  ;;

  let resume = function
    | Returned result -> result
    | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  ;;
end

module Observation = struct
  module CO = Agent_session.Close_observation
  module D = Agent_session.Cleanup_diagnostic
  module Failure = Agent_session.Callback_cleanup.Failure

  let return value = CO.create (Returned (Ok value)) ~diagnostics:[] ()

  let failures = function
    | CO.Returned (Ok _) -> []
    | Returned (Error error) -> [ Failure.Protocol error ]
    | Raised (exn, trace) -> [ Failure.Raised (exn, trace) ]
  ;;

  let create exit ~failures =
    let diagnostic =
      match failures with
      | [] -> None
      | _ :: _ -> Some (D.create failures)
    in
    CO.create exit ~diagnostics:(Option.to_list diagnostic) ?primary:diagnostic ()
  ;;

  let raw f =
    let exit =
      match f () with
      | result -> CO.Returned result
      | exception exn -> CO.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
    in
    create exit ~failures:(failures exit)
  ;;

  (* Preserve the literal original boundary classification before any caller
     projects it. Known observations forward; only an actual escape or missing
     negative provenance owns this boundary's control carrier. *)
  let capture_with_control f =
    let observed, control =
      match f () with
      | observed -> observed, None
      | exception exn ->
        let trace = Stdlib.Printexc.get_raw_backtrace () in
        let observed =
          create (CO.Raised (exn, trace)) ~failures:[ Failure.Raised (exn, trace) ]
        in
        observed, Some observed
    in
    match failures (CO.exit observed), CO.primary observed with
    | [], _ -> observed, control
    | _ :: _, Some primary when not (List.is_empty (D.failures primary)) ->
      observed, control
    | actual, (None | Some _) ->
      let diagnostic = D.create actual in
      let observed =
        CO.create
          (CO.exit observed)
          ~diagnostics:(CO.diagnostics observed @ [ diagnostic ])
          ~primary:diagnostic
          ()
      in
      observed, Some observed
  ;;

  let capture f = fst (capture_with_control f)

  let close runtime =
    match B.close_reported runtime with
    | observed -> observed
    | exception exn ->
      let trace = Stdlib.Printexc.get_raw_backtrace () in
      let diagnostic = D.create [ Failure.Raised (exn, trace) ] in
      CO.create (Raised (exn, trace)) ~diagnostics:[ diagnostic ] ~primary:diagnostic ()
  ;;

  let protect body finally =
    let result =
      Outcome.capture (fun () ->
        Exn.protect ~f:(fun () -> CO.resume body) ~finally:(fun () -> CO.resume finally))
    in
    let primary =
      match CO.exit body, CO.exit finally with
      | Raised _, _ -> CO.primary body
      | Returned _, Raised _ -> CO.primary finally
      | Returned _, Returned () -> CO.primary body
    in
    CO.create
      (match result with
       | Outcome.Returned result -> CO.Returned result
       | Raised (exn, trace) -> Raised (exn, trace))
      ~diagnostics:(CO.diagnostics body @ CO.diagnostics finally)
      ?primary
      ()
  ;;
end

module Cleanup = struct
  module CO = Agent_session.Close_observation

  module Local = struct
    type t =
      { observation : (unit, P.Error.t) result CO.t
      ; switch_started : bool
      ; switch : (unit, P.Error.t) result Eio.Switch.Outcome.t option
      ; created : B.t option
      ; runtime_close : unit CO.t option
      ; revocation : (unit, P.Error.t) result CO.t option
      }

    type status =
      | Joined
      | Failed
      | Incomplete
    [@@deriving equal, sexp_of]

    let release t = Option.map t.switch ~f:Eio.Switch.Outcome.release
    let observation t = t.observation
    let switch_outcome t = t.switch
    let runtime_close t = t.runtime_close
    let revocation t = t.revocation
    let diagnostics t = CO.diagnostics t.observation

    let status t =
      let release =
        match t.switch_started, release t with
        | false, None -> Joined
        | _, Some release ->
          (match
             ( Eio.Switch.Release_report.status release
             , Eio.Switch.Release_report.failures release )
           with
           | Complete, [] -> Joined
           | (Not_entered | Incomplete | Complete), _ :: _ -> Failed
           | (Not_entered | Incomplete), [] -> Incomplete)
        | true, None -> Incomplete
      in
      let close =
        match t.created, t.runtime_close with
        | None, None -> Joined
        | Some _, Some observed ->
          (match CO.exit observed with
           | Returned () -> Joined
           | Raised _ -> Failed)
        | None, Some _ | Some _, None -> Incomplete
      in
      let revoked =
        match t.revocation with
        | None -> Joined
        | Some observed ->
          (match CO.exit observed with
           | Returned (Ok ()) -> Joined
           | Returned (Error _) | Raised _ -> Failed)
      in
      if List.exists [ release; close; revoked ] ~f:(equal_status Failed)
      then Failed
      else if List.exists [ release; close; revoked ] ~f:(equal_status Incomplete)
      then Incomplete
      else Joined
    ;;
  end

  module Adoption = struct
    type t =
      | Original_running
      | Not_created
      | Closed_runtime of
          { runtime : B.t
          ; observation : unit CO.t
          }
  end

  module Normalization = struct
    type t =
      { original : (unit, P.Error.t) result CO.t
      ; selected : Agent_session.Cleanup_diagnostic.t
      }

    let original t = t.original
    let selected t = t.selected
  end

  module Original = struct
    type t =
      { local : Local.t
      ; terminal : (unit, P.Error.t) result CO.t
      ; outer_control : (unit, P.Error.t) result CO.t option
      ; normalization : Normalization.t option
      }

    let local t = t.local
    let terminal t = t.terminal
    let outer_control t = t.outer_control
    let normalization t = t.normalization
  end

  type t =
    { domain : int
    ; mutable started : bool
    ; mutable local : Local.t option
    ; mutable terminal : (unit, P.Error.t) result CO.t option
    ; mutable original : Original.t option
    ; mutable join : (unit -> Original.t CO.t) option
    ; mutable adoption : Adoption.t
    ; mutable direct_ancestry : Ancestry_cleanup.t option
    }

  let create () =
    { domain = (Stdlib.Domain.self () :> int)
    ; started = false
    ; local = None
    ; terminal = None
    ; original = None
    ; join = None
    ; adoption = Original_running
    ; direct_ancestry = None
    }
  ;;

  let check_domain t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then invalid_arg "delegated cleanup belongs to another Eio domain"
  ;;

  let start t =
    check_domain t;
    if t.started then invalid_arg "delegated cleanup original driver already started";
    t.started <- true
  ;;

  let local t =
    check_domain t;
    t.local
  ;;

  let terminal t =
    check_domain t;
    t.terminal
  ;;

  let adoption t =
    check_domain t;
    t.adoption
  ;;

  let install_join t join =
    check_domain t;
    if Option.is_some t.join then invalid_arg "delegated original join installed twice";
    t.join <- Some join
  ;;

  let join_original t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then Error (P.Error.invalid_request "delegated cleanup belongs to another Eio domain")
    else (
      match t.started, t.join with
      | true, Some join -> Ok (join ())
      | false, _ | true, None ->
        Error
          (P.Error.create
             Conflict
             ~message:"delegated original driver has not started"
             ~retryable:false
             ()))
  ;;

  let retain_direct_ancestry t ancestry =
    check_domain t;
    match t.direct_ancestry with
    | None -> t.direct_ancestry <- Some ancestry
    | Some _ -> invalid_arg "delegated direct ancestry already captured"
  ;;

  let record_local t local =
    check_domain t;
    if Option.is_some t.local then invalid_arg "delegated local outcome already recorded";
    t.local <- Some local
  ;;

  let finish t ~outer_control ~normalization observation =
    check_domain t;
    if Option.is_some t.terminal then invalid_arg "delegated terminal already recorded";
    let adoption =
      match t.local with
      | Some { Local.created = None; runtime_close = None; _ } -> Adoption.Not_created
      | Some { created = Some runtime; runtime_close = Some observation; _ } ->
        Closed_runtime { runtime; observation }
      | None | Some _ ->
        (* Terminal control is not an original close receipt. Retain the exact
           staged runtime through Local and refuse adoption/fallback. *)
        Original_running
    in
    let local =
      match t.local with
      | Some local -> local
      | None -> invalid_arg "delegated original has no actual local outcome"
    in
    t.original
    <- Some { Original.local; terminal = observation; outer_control; normalization };
    t.adoption <- adoption;
    t.terminal <- Some observation
  ;;
end

let cancelled_preparation () =
  Error (interrupted "delegation.runtime_cancelled: parent stopped during preparation")
;;

let normalize_cancellation ~published =
  if published then Ok () else cancelled_preparation ()
;;

let resume_switch outcome ~published =
  let release = Eio.Switch.Outcome.release outcome in
  let release_failure () =
    match Eio.Switch.Release_report.failures release with
    | (exn, backtrace) :: _ -> Exn.raise_with_original_backtrace exn backtrace
    | [] ->
      Error
        (P.Error.create
           Conflict
           ~message:"delegation.runtime_cleanup: child switch release did not finish"
           ~retryable:false
           ())
  in
  match Eio.Switch.Outcome.result outcome with
  | Error ((Eio.Cancel.Cancelled _ as exn), backtrace) ->
    (match
       ( Eio.Switch.Release_report.status release
       , Eio.Switch.Release_report.failures release )
     with
     | Complete, [] -> normalize_cancellation ~published
     | (Not_entered | Incomplete | Complete), _ :: _ -> release_failure ()
     | (Not_entered | Incomplete), [] -> Exn.raise_with_original_backtrace exn backtrace)
  | Error (exn, backtrace) ->
    (* A worker may induce callback cancellation. Preserve the exact Switch
       aggregate, never replace it with a saved callback Cancelled. *)
    Exn.raise_with_original_backtrace exn backtrace
  | Ok (Error _ as failure) -> failure
  | Ok (Ok ()) ->
    (match
       ( Eio.Switch.Release_report.status release
       , Eio.Switch.Release_report.failures release )
     with
     | Complete, [] -> Ok ()
     | (Not_entered | Incomplete | Complete), _ -> release_failure ())
;;

let observe_switch outcome ~published =
  let module CO = Agent_session.Close_observation in
  let module F = Agent_session.Callback_cleanup.Failure in
  let selected = Outcome.capture (fun () -> resume_switch outcome ~published) in
  let exit =
    match selected with
    | Outcome.Returned result -> CO.Returned result
    | Raised (exn, trace) -> CO.Raised (exn, trace)
  in
  let release = Eio.Switch.Outcome.release outcome in
  let release_failures () =
    List.map (Eio.Switch.Release_report.failures release) ~f:(fun (exn, trace) ->
      F.Raised (exn, trace))
  in
  let failures =
    match Eio.Switch.Outcome.result outcome with
    | Error (Eio.Cancel.Cancelled _, _) ->
      (match
         ( Eio.Switch.Release_report.status release
         , Eio.Switch.Release_report.failures release )
       with
       | Complete, [] -> Observation.failures exit
       | (Not_entered | Incomplete | Complete), _ :: _ -> release_failures ()
       | (Not_entered | Incomplete), [] -> Observation.failures exit)
    | Error (exn, trace) -> F.Raised (exn, trace) :: release_failures ()
    | Ok (Error error) -> F.Protocol error :: release_failures ()
    | Ok (Ok ()) ->
      (match
         ( Eio.Switch.Release_report.status release
         , Eio.Switch.Release_report.failures release )
       with
       | Complete, [] -> []
       | (Not_entered | Incomplete | Complete), _ :: _ -> release_failures ()
       | (Not_entered | Incomplete), [] -> Observation.failures exit)
  in
  Observation.create exit ~failures
;;

let prepare_with_lease ~cleanup ~with_parent ~sw ~parent ~on_revoked ~build =
  let module CO = Agent_session.Close_observation in
  Cleanup.start cleanup;
  let ready, ready_u = Eio.Promise.create () in
  let finished, finished_u = Eio.Promise.create () in
  let stop, stop_u = Eio.Promise.create () in
  let requested = Atomic.make false in
  let published = ref false in
  let terminal = ref None in
  let signal_stop () =
    match Atomic.exchange requested true with
    | true -> ()
    | false -> Eio.Promise.resolve stop_u ()
  in
  let join_original () =
    match
      Eio.Cancel.protect (fun () ->
        signal_stop ();
        let _, original_holder = Eio.Promise.await finished in
        match original_holder.Cleanup.original with
        | Some original -> original
        | None -> invalid_arg "joined delegated driver has no original outcome")
    with
    | original ->
      CO.create
        (Returned original)
        ~diagnostics:(CO.diagnostics (Cleanup.Original.terminal original))
        ()
    | exception exn ->
      let trace = Stdlib.Printexc.get_raw_backtrace () in
      let diagnostic =
        Agent_session.Cleanup_diagnostic.create
          [ Agent_session.Callback_cleanup.Failure.Raised (exn, trace) ]
      in
      let prior =
        Option.value_map (Cleanup.terminal cleanup) ~default:[] ~f:CO.diagnostics
      in
      CO.create
        (Raised (exn, trace))
        ~diagnostics:(prior @ [ diagnostic ])
        ~primary:diagnostic
        ()
  in
  Cleanup.install_join cleanup join_original;
  let close_reported () =
    let joined = join_original () in
    match CO.exit joined with
    | Raised (exn, trace) ->
      CO.create
        (Raised (exn, trace))
        ~diagnostics:(CO.diagnostics joined)
        ?primary:(CO.primary joined)
        ()
    | Returned original ->
      let completed = Cleanup.Original.terminal original in
      let exit =
        match CO.exit completed with
        | Returned (Ok ()) -> CO.Returned ()
        | Raised (exn, trace) -> Raised (exn, trace)
        | Returned (Error error) ->
          (* The legacy wrapper is a projection of the known terminal cause. *)
          (match raise (Cleanup_failed error) with
           | () -> assert false
           | exception exn -> CO.Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
      in
      CO.create
        exit
        ~diagnostics:(CO.diagnostics completed)
        ?primary:(CO.primary completed)
        ()
  in
  let no_entry () =
    { Cleanup.Local.observation = Observation.return ()
    ; switch_started = false
    ; switch = None
    ; created = None
    ; runtime_close = None
    ; revocation = None
    }
  in
  let finish ~local ~outer_control ~normalization observation =
    if Option.is_none (Cleanup.local cleanup) then Cleanup.record_local cleanup local;
    Cleanup.finish cleanup ~outer_control ~normalization observation;
    terminal := Some observation;
    Eio.Promise.resolve finished_u (observation, cleanup)
  in
  let forked =
    Outcome.capture (fun () ->
      (* [Fiber.fork] silently declines an already-cancelled switch. This
         synchronous public check has no yield before fork's own domain/On
         checks, so a legitimate caller either forks or records real no-entry. *)
      Eio.Switch.check sw;
      Eio.Fiber.fork ~sw (fun () ->
        let local = ref None in
        let callback_entered = ref false in
        let created = ref None in
        let switch_started = ref false in
        let switch = ref None in
        let switch_observation = ref None in
        let runtime_close = ref None in
        let revocation = ref None in
        let outer, outer_control =
          Observation.capture_with_control (fun () ->
            with_parent parent (fun parent_runtime ->
              callback_entered := true;
              let body =
                Observation.capture (fun () ->
                  let switched =
                    try
                      Ok
                        (Eio.Cancel.sub (fun context ->
                           switch_started := true;
                           let outcome =
                             Eio.Switch.run_reported (fun runtime_sw ->
                               Eio.Fiber.fork_daemon ~sw:runtime_sw (fun () ->
                                 Eio.Promise.await stop;
                                 Eio.Cancel.cancel context Exit;
                                 `Stop_daemon);
                               match build ~sw:runtime_sw parent_runtime with
                               | Error error -> Error error
                               | Ok (runtime : B.t) ->
                                 created := Some runtime;
                                 let check_open () =
                                   match !terminal with
                                   | Some completed ->
                                     (match CO.exit completed with
                                      | Returned (Error error) -> Error error
                                      | Returned (Ok ()) | Raised _ ->
                                        Error
                                          (interrupted
                                             "delegation.runtime_closed: inherited \
                                              runtime scope has ended"))
                                   | None -> Ok ()
                                 in
                                 let check_execution () =
                                   let open Result.Let_syntax in
                                   let%bind () = check_open () in
                                   match runtime.check_execution with
                                   | None -> Ok ()
                                   | Some check -> check ()
                                 in
                                 let check_ancestry =
                                   Option.map runtime.check_ancestry ~f:(fun check () ->
                                     let open Result.Let_syntax in
                                     let%bind () = check_open () in
                                     let%bind ancestry = check () in
                                     let%map () = check_open () in
                                     ancestry)
                                 in
                                 published := true;
                                 Eio.Promise.resolve
                                   ready_u
                                   (Outcome.Returned
                                      (Ok
                                         { runtime with
                                           close = B.Close.reported close_reported
                                         ; check_execution = Some check_execution
                                         ; check_ancestry
                                         }));
                                 Eio.Promise.await stop;
                                 Ok ())
                           in
                           switch := Some outcome;
                           let observed = observe_switch outcome ~published:!published in
                           switch_observation := Some observed;
                           observed))
                    with
                    | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
                  in
                  let selected =
                    match switched with
                    | Ok observed -> observed
                    | Error (Eio.Cancel.Cancelled _, _) ->
                      (match !switch_observation with
                       | Some observed -> observed
                       | None ->
                         Observation.raw (fun () ->
                           normalize_cancellation ~published:!published))
                    | Error (exn, trace) ->
                      Observation.create
                        (Raised (exn, trace))
                        ~failures:
                          [ Agent_session.Callback_cleanup.Failure.Raised (exn, trace) ]
                  in
                  match !published, Atomic.get requested, CO.exit selected with
                  | true, false, Returned (Ok ()) ->
                    let actual =
                      Eio.Cancel.protect (fun () -> Observation.raw on_revoked)
                    in
                    revocation := Some actual;
                    CO.create
                      (CO.exit actual)
                      ~diagnostics:(CO.diagnostics selected @ CO.diagnostics actual)
                      ?primary:(CO.primary actual)
                      ()
                  | _ -> selected)
              in
              (* Actual close runs once AFTER the body outcome, with the original
                 protected-finalizer ordering. Exn.protect below resumes DATA. *)
              let closed =
                Eio.Cancel.protect (fun () ->
                  match !created with
                  | None -> CO.create (Returned ()) ~diagnostics:[] ()
                  | Some runtime ->
                    let actual = Observation.close runtime in
                    runtime_close := Some actual;
                    actual)
              in
              let observed = Observation.protect body closed in
              let completed =
                { Cleanup.Local.observation = observed
                ; switch_started = !switch_started
                ; switch = !switch
                ; created = !created
                ; runtime_close = !runtime_close
                ; revocation = !revocation
                }
              in
              local := Some completed;
              Cleanup.record_local cleanup completed;
              observed))
        in
        let observed, outer_control, normalization =
          match CO.exit outer with
          | Raised (Eio.Cancel.Cancelled _, _) ->
            let normalization =
              match CO.primary outer with
              | Some selected -> Some { Cleanup.Normalization.original = outer; selected }
              | None ->
                (* [capture_with_control] installs a meaningful primary for
                   every actual negative before this branch. *)
                invalid_arg "delegated outer cancellation has no selected origin"
            in
            let selected =
              match !local with
              | Some completed -> completed.Cleanup.Local.observation
              | None -> Observation.raw cancelled_preparation
            in
            ( CO.create
                (CO.exit selected)
                ~diagnostics:(CO.diagnostics outer @ CO.diagnostics selected)
                ?primary:(CO.primary selected)
                ()
            , None
            , normalization )
          | Returned _ | Raised _ -> outer, outer_control, None
        in
        let completed_local =
          match !local with
          | Some completed -> completed
          | None ->
            if not !callback_entered
            then no_entry ()
            else
              { Cleanup.Local.observation = outer
              ; switch_started = !switch_started
              ; switch = !switch
              ; created = !created
              ; runtime_close = !runtime_close
              ; revocation = !revocation
              }
        in
        finish ~local:completed_local ~outer_control ~normalization observed;
        if not !published
        then
          Eio.Promise.resolve
            ready_u
            (match CO.exit observed with
             | Returned (Error error) -> Outcome.Returned (Error error)
             | Raised (exn, trace) -> Outcome.Raised (exn, trace)
             | Returned (Ok ()) -> Outcome.Returned (cancelled_preparation ())));
      Ok ())
  in
  (match forked with
   | Outcome.Returned (Ok ()) -> ()
   | Returned (Error _) -> invalid_arg "fork returned a protocol result"
   | Raised (exn, trace) ->
     let observed =
       Observation.create
         (CO.Raised (exn, trace))
         ~failures:[ Agent_session.Callback_cleanup.Failure.Raised (exn, trace) ]
     in
     (* Actual fork refusal precedes any driver/runtime entry. Terminalize this
        real no-entry invocation so an early Cell descriptor remains truthful. *)
     finish
       ~local:(no_entry ())
       ~outer_control:(Some observed)
       ~normalization:None
       observed;
     Exn.raise_with_original_backtrace exn trace);
  match Eio.Promise.await ready |> Outcome.resume with
  | result -> result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () ->
      signal_stop ();
      ignore (Eio.Promise.await finished : (unit, P.Error.t) result CO.t * Cleanup.t));
    Exn.raise_with_original_backtrace exn backtrace
;;

let prepare_resources_reported ~cleanup ~with_resources =
  prepare_with_lease ~cleanup ~with_parent:(fun () f -> with_resources f) ~parent:()
;;

let with_direct_parent cleanup owner ~with_owner f =
  let ancestry = Ancestry_cleanup.create () in
  Cleanup.retain_direct_ancestry cleanup ancestry;
  Ancestry_cleanup.begin_original ancestry;
  let captured = ref None in
  let callback = ref None in
  let body, escaped =
    Observation.capture_with_control (fun () ->
      let outcome =
        with_owner
          owner
          ~capture:(fun recovery ->
            captured := Some (Ancestry_cleanup.capture_lease ancestry ~owner recovery))
          (fun runtime ->
             let observed = Observation.capture (fun () -> f runtime) in
             callback := Some observed;
             observed)
      in
      Ancestry_cleanup.record_lease
        ancestry
        (Ancestry_cleanup.finish_capture
           ancestry
           ~owner
           ~captured:!captured
           ~actual:(Runtime_owner.Lease_outcome.cleanup outcome))
        ~callback:!callback
        ~outcome;
      Runtime_owner.Lease_outcome.observation outcome)
  in
  Option.iter escaped ~f:(Ancestry_cleanup.record_control ancestry ~role:Unknown);
  let sealed = Ancestry_cleanup.seal_observed ancestry in
  let finalizer =
    Agent_session.Close_observation.create
      (match Agent_session.Close_observation.exit sealed with
       | Returned (Ok ()) -> Returned ()
       | Raised (exn, trace) -> Raised (exn, trace)
       | Returned (Error _) -> invalid_arg "ancestry unit seal returned a protocol result")
      ~diagnostics:(Agent_session.Close_observation.diagnostics sealed)
      ?primary:(Agent_session.Close_observation.primary sealed)
      ()
  in
  Observation.protect body finalizer
;;

let prepare ~sw ~parent ~on_revoked ~build =
  let cleanup = Cleanup.create () in
  prepare_with_lease
    ~cleanup
    ~with_parent:(fun owner f ->
      with_direct_parent
        cleanup
        owner
        ~with_owner:Runtime_owner.with_background_runtime_observed
        f)
    ~sw
    ~parent
    ~on_revoked
    ~build
;;

let prepare_independent ~sw ~parent ~on_revoked ~build =
  let cleanup = Cleanup.create () in
  prepare_with_lease
    ~cleanup
    ~with_parent:(fun owner f ->
      with_direct_parent
        cleanup
        owner
        ~with_owner:Runtime_owner.with_delegation_resources_observed
        f)
    ~sw
    ~parent
    ~on_revoked
    ~build
;;

let prepare_resources ~with_resources ~sw ~on_revoked ~build =
  let cleanup = Cleanup.create () in
  prepare_resources_reported
    ~cleanup
    ~with_resources:(fun f ->
      (* Legacy arbitrary CPS has no selected-origin contract. Its actual raw
         invocation is opaque; the observed port avoids this boundary. *)
      Observation.raw (fun () ->
        with_resources (fun resources ->
          Agent_session.Close_observation.resume (f resources))))
    ~sw
    ~on_revoked
    ~build
;;
