open! Core
module CO = Agent_session.Close_observation
module D = Agent_session.Cleanup_diagnostic

module Failure = struct
  type t =
    | Protocol of Agent_protocol.Error.t
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  let capture f =
    try Result.map_error (f ()) ~f:(fun error -> Protocol error) with
    | exn -> Error (Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
  ;;

  let resume = function
    | Ok value -> Ok value
    | Error (Protocol error) -> Error error
    | Error (Raised (exn, backtrace)) -> Exn.raise_with_original_backtrace exn backtrace
  ;;

  let diagnostic = function
    | Protocol error -> D.create [ Agent_session.Callback_cleanup.Failure.Protocol error ]
    | Raised (exn, trace) -> D.create [ Raised (exn, trace) ]
  ;;
end

let failure_of_exit = function
  | CO.Returned (Ok _) -> None
  | Returned (Error error) -> Some (Failure.Protocol error)
  | Raised (exn, trace) -> Some (Failure.Raised (exn, trace))
;;

let capture f =
  let exit =
    match f () with
    | result -> CO.Returned result
    | exception exn -> CO.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
  in
  match failure_of_exit exit with
  | None -> CO.create exit ~diagnostics:[] ()
  | Some failure ->
    let diagnostic = Failure.diagnostic failure in
    CO.create exit ~diagnostics:[ diagnostic ] ~primary:diagnostic ()
;;

let capture_callback f =
  let observation =
    match f () with
    | observation -> observation
    | exception exn ->
      let trace = Stdlib.Printexc.get_raw_backtrace () in
      let diagnostic = Failure.diagnostic (Raised (exn, trace)) in
      CO.create
        (CO.Raised (exn, trace))
        ~diagnostics:[ diagnostic ]
        ~primary:diagnostic
        ()
  in
  match failure_of_exit (CO.exit observation), CO.primary observation with
  | None, _ -> observation
  | Some _, Some primary when not (List.is_empty (D.failures primary)) -> observation
  | Some failure, (None | Some _) ->
    let diagnostic = Failure.diagnostic failure in
    CO.create
      (CO.exit observation)
      ~diagnostics:(CO.diagnostics observation @ [ diagnostic ])
      ~primary:diagnostic
      ()
;;

let is_negative observation = Option.is_some (failure_of_exit (CO.exit observation))

module Report = struct
  type status =
    | Never_entered
    | Joined
    | Failed
    | Incomplete
  [@@deriving compare, equal, sexp_of]

  type t =
    { status : status
    ; failures : Failure.t list
    ; diagnostics : D.t list
    ; release_report : Eio.Switch.Release_report.t option
    ; primary : D.t option
    }

  let status t = t.status
  let failures t = t.failures
  let diagnostics t = t.diagnostics
  let release_report t = t.release_report

  let observation report =
    let observed exit primary =
      Ok (CO.create exit ~diagnostics:report.diagnostics ?primary ())
    in
    match failures report with
    | Failure.Protocol error :: _ -> observed (CO.Returned (Error error)) report.primary
    | Raised (exn, trace) :: _ -> observed (CO.Raised (exn, trace)) report.primary
    | [] ->
      (match status report with
       | Joined | Never_entered -> observed (CO.Returned (Ok ())) None
       | Failed | Incomplete ->
         (* Missing evidence is a lifecycle refusal, not a new failed producer. *)
         Error
           (Agent_protocol.Error.create
              Conflict
              ~message:"Independent switch release did not finish"
              ~retryable:true
              ()))
  ;;
end

module Outcome = struct
  type 'a t =
    { result : ('a, Failure.t) result
    ; cleanup : Report.t
    ; switch_outcome : ('a, Agent_protocol.Error.t) result Eio.Switch.Outcome.t
    ; observation : ('a, Agent_protocol.Error.t) result CO.t
    }

  let result t = t.result
  let cleanup t = t.cleanup
  let switch_outcome t = t.switch_outcome
  let observation t = t.observation
end

module Observation = struct
  type t =
    { ordinal : int
    ; failure : Failure.t
    ; diagnostic : D.t
    }
end

module Graph = struct
  type kind =
    | Seal
    | Finish

  type leg =
    | Pending
    | Succeeded
    | Failed of Observation.t

  type t =
    { tracking : Graph_tracking.t
    ; mutable seal : leg
    ; mutable finish : leg
    }

  let run t ~kind ~observe =
    let result =
      Failure.capture (fun () ->
        match kind with
        | Seal -> Graph_tracking.seal t.tracking
        | Finish -> Graph_tracking.finish t.tracking)
    in
    let leg, observation =
      match result with
      | Ok () -> Succeeded, CO.create (CO.Returned (Ok ())) ~diagnostics:[] ()
      | Error failure ->
        let observed = observe failure in
        let exit =
          match failure with
          | Failure.Protocol error -> CO.Returned (Error error)
          | Raised (exn, trace) -> CO.Raised (exn, trace)
        in
        ( Failed observed
        , CO.create
            exit
            ~diagnostics:[ observed.Observation.diagnostic ]
            ~primary:observed.diagnostic
            () )
    in
    (match kind with
     | Seal -> t.seal <- leg
     | Finish -> t.finish <- leg);
    observation
  ;;

  let current t = function
    | Seal -> t.seal
    | Finish -> t.finish
  ;;

  let failures ~kind t =
    match current t kind with
    | Pending | Succeeded -> []
    | Failed failure -> [ failure ]
  ;;

  let retry t ~kind ~observe =
    match current t kind with
    | Pending | Succeeded -> ()
    | Failed _ ->
      ignore (run t ~kind ~observe : (unit, Agent_protocol.Error.t) result CO.t)
  ;;

  let retryable t =
    match t.seal, t.finish with
    | Failed _, _ | _, Failed _ -> true
    | (Pending | Succeeded), (Pending | Succeeded) -> false
  ;;
end

module Release = struct
  type t =
    { report : Eio.Switch.Release_report.t
    ; failures : Observation.t list
    }
end

type registration =
  | Accepting_graphs
  | Registration_closed

type phase =
  | Fresh
  | Running of registration
  | Retrying of Release.t * Report.t
  | Unentered of Report.t
  | Finished of Release.t * Report.t

type t =
  { domain : int
  ; mutable graphs : Graph.t list
  ; mutable next_ordinal : int
  ; mutable phase : phase
  ; mutable original_report : Report.t option
  }

let create () =
  { domain = (Stdlib.Domain.self () :> int)
  ; graphs = []
  ; next_ordinal = 0
  ; phase = Fresh
  ; original_report = None
  }
;;

let check_domain t =
  if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "Independent cleanup belongs to another Eio domain"
;;

let observe ?diagnostic t failure =
  let ordinal = t.next_ordinal in
  t.next_ordinal <- ordinal + 1;
  let diagnostic =
    Option.value_or_thunk diagnostic ~default:(fun () -> Failure.diagnostic failure)
  in
  { Observation.ordinal; failure; diagnostic }
;;

let cleanup_all t ~kind =
  Eio.Cancel.protect (fun () ->
    List.fold
      t.graphs
      ~init:(CO.create (CO.Returned (Ok ())) ~diagnostics:[] ())
      ~f:(fun first graph ->
        let current = Graph.run graph ~kind ~observe:(observe t) in
        let selected = if is_negative first then first else current in
        CO.create
          (CO.exit selected)
          ~diagnostics:(CO.diagnostics first @ CO.diagnostics current)
          ?primary:(CO.primary selected)
          ()))
;;

let snapshot t (release : Release.t) =
  let observed =
    List.concat_map t.graphs ~f:(Graph.failures ~kind:Seal)
    @ release.failures
    @ List.concat_map t.graphs ~f:(Graph.failures ~kind:Finish)
    |> List.sort ~compare:(fun first second ->
      Int.compare first.Observation.ordinal second.Observation.ordinal)
  in
  let failures = List.map observed ~f:(fun observed -> observed.Observation.failure) in
  let status =
    match Eio.Switch.Release_report.status release.report with
    | Not_entered -> Report.Never_entered
    | Incomplete -> Report.Incomplete
    | Complete -> if List.is_empty failures then Report.Joined else Failed
  in
  { Report.status
  ; failures
  ; diagnostics =
      List.fold observed ~init:[] ~f:(fun diagnostics observed ->
        let diagnostic = observed.Observation.diagnostic in
        if List.exists diagnostics ~f:(D.equal diagnostic)
        then diagnostics
        else diagnostics @ [ diagnostic ])
  ; release_report = Some release.report
  ; primary =
      Option.map (List.hd observed) ~f:(fun observed -> observed.Observation.diagnostic)
  }
;;

let retain_primary callback cleanup =
  let exit, primary =
    match CO.exit callback with
    | (Raised _ | Returned (Error _)) as exit -> exit, CO.primary callback
    | Returned (Ok value) ->
      (match CO.exit cleanup with
       | Raised (exn, trace) -> CO.Raised (exn, trace), CO.primary cleanup
       | Returned (Error error) -> Returned (Error error), CO.primary cleanup
       | Returned (Ok ()) -> Returned (Ok value), None)
  in
  CO.create
    exit
    ~diagnostics:(CO.diagnostics callback @ CO.diagnostics cleanup)
    ?primary
    ()
;;

let run_internal t ~f =
  check_domain t;
  (match t.phase with
   | Fresh -> t.phase <- Running Accepting_graphs
   | Running _ | Retrying _ | Unentered _ | Finished _ ->
     invalid_arg "Independent cleanup bracket already entered");
  let observed = ref None in
  let switched =
    Eio.Switch.run_reported (fun sw ->
      let register_tracking tracking =
        check_domain t;
        match t.phase with
        | Running Accepting_graphs ->
          if
            List.exists t.graphs ~f:(fun graph ->
              phys_equal graph.Graph.tracking tracking)
          then invalid_arg "Independent graph is already registered";
          t.graphs <- { Graph.tracking; seal = Pending; finish = Pending } :: t.graphs
        | Fresh | Running Registration_closed | Retrying _ | Unentered _ | Finished _ ->
          invalid_arg "Independent graph registration is closed"
      in
      let callback = capture_callback (fun () -> f ~sw ~register_tracking) in
      t.phase <- Running Registration_closed;
      observed := Some callback;
      let selected = retain_primary callback (cleanup_all t ~kind:Seal) in
      observed := Some selected;
      CO.resume selected)
  in
  let release_report = Eio.Switch.Outcome.release switched in
  let hooks = Eio.Switch.Release_report.failures release_report in
  let saved_failure =
    Option.bind !observed ~f:(fun observation ->
      Option.map
        (failure_of_exit (CO.exit observation))
        ~f:(fun failure -> failure, CO.primary observation))
  in
  let saved_negative = Option.is_some saved_failure in
  (* One actual Switch stage owns one group. With a saved callback/seal primary,
     only its independent release failures need a Switch group. Otherwise the
     actual aggregate leads that SAME group's opaque supporting parts. Retain
     every actual release failure too: Eio may omit a Cancelled-valued hook from
     its aggregate. These are not deduplicated unique leaves. *)
  let switch_diagnostic =
    match Eio.Switch.Outcome.result switched, saved_negative, hooks with
    | Error (exn, trace), false, _ ->
      Some
        (D.create
           (Agent_session.Callback_cleanup.Failure.Raised (exn, trace)
            :: List.map hooks ~f:(fun (exn, trace) ->
              Agent_session.Callback_cleanup.Failure.Raised (exn, trace))))
    | _, _, (_ :: _ as failures) ->
      Some
        (D.create
           (List.map failures ~f:(fun (exn, trace) ->
              Agent_session.Callback_cleanup.Failure.Raised (exn, trace))))
    | _, _, [] -> None
  in
  let release =
    { Release.report = release_report
    ; failures =
        List.map hooks ~f:(fun (exn, trace) ->
          observe ?diagnostic:switch_diagnostic t (Failure.Raised (exn, trace)))
    }
  in
  let finished = cleanup_all t ~kind:Finish in
  let result, primary =
    match Eio.Switch.Outcome.result switched with
    | Ok (Error error) ->
      Error (Failure.Protocol error), Option.bind !observed ~f:CO.primary
    | Ok (Ok value) ->
      (match CO.exit finished with
       | Returned (Ok ()) -> Ok value, None
       | Returned (Error error) -> Error (Failure.Protocol error), CO.primary finished
       | Raised (exn, trace) -> Error (Failure.Raised (exn, trace)), CO.primary finished)
    | Error (exn, trace) ->
      (match saved_failure with
       | Some (failure, primary) -> Error failure, primary
       | None -> Error (Failure.Raised (exn, trace)), switch_diagnostic)
  in
  let cleanup = snapshot t release in
  let exit =
    match result with
    | Ok value -> CO.Returned (Ok value)
    | Error (Failure.Protocol error) -> Returned (Error error)
    | Error (Raised (exn, trace)) -> Raised (exn, trace)
  in
  let observation =
    CO.create
      exit
      ~diagnostics:
        (Option.value_map !observed ~default:[] ~f:CO.diagnostics
         @ Option.to_list switch_diagnostic
         @ cleanup.diagnostics)
      ?primary
      ()
  in
  t.original_report <- Some cleanup;
  t.phase <- Finished (release, cleanup);
  { Outcome.result; cleanup; observation; switch_outcome = switched }
;;

let run t ~f =
  run_internal t ~f:(fun ~sw ~register_tracking ->
    capture (fun () -> f ~sw ~register_tracking))
;;

let run_observed = run_internal

let report t =
  check_domain t;
  match t.phase with
  | Fresh | Running _ -> None
  | Unentered report | Retrying (_, report) | Finished (_, report) -> Some report
;;

let original_report t =
  check_domain t;
  t.original_report
;;

let finish_without_entry t =
  check_domain t;
  match t.phase with
  | Fresh ->
    let report =
      { Report.status = Never_entered
      ; failures = []
      ; diagnostics = []
      ; release_report = None
      ; primary = None
      }
    in
    t.original_report <- Some report;
    t.phase <- Unentered report
  | Unentered _ | Finished _ -> ()
  | Running _ | Retrying _ -> invalid_arg "Independent cleanup is still running"
;;

let refusal message = Agent_protocol.Error.create Conflict ~message ~retryable:true ()

type retry_status =
  | Original_running
  | Busy
  | Retryable
  | Joined
  | Fixed
[@@deriving equal, sexp_of]

let retry_status t =
  check_domain t;
  match t.phase with
  | Fresh | Running _ -> Original_running
  | Retrying _ -> Busy
  | Unentered _ -> Joined
  | Finished (release, report) ->
    (match Eio.Switch.Release_report.status release.report with
     | Complete when List.exists t.graphs ~f:Graph.retryable -> Retryable
     | _ ->
       (match Report.status report with
        | Never_entered | Joined -> Joined
        | Failed | Incomplete -> Fixed))
;;

let retry_observed t =
  check_domain t;
  match t.phase with
  | Fresh | Running _ -> Error (refusal "Independent cleanup bracket has not exited")
  | Retrying _ -> Error (refusal "Independent factual retry is already running")
  | Unentered report -> Report.observation report
  | Finished (release, report) ->
    (match Eio.Switch.Release_report.status release.report with
     | Not_entered | Incomplete -> Report.observation report
     | Complete ->
       t.phase <- Retrying (release, report);
       Exn.protect
         ~finally:(fun () ->
           match t.phase with
           | Retrying _ -> t.phase <- Finished (release, snapshot t release)
           | Fresh | Running _ | Unentered _ | Finished _ -> ())
         ~f:(fun () ->
           Eio.Cancel.protect (fun () ->
             List.iter t.graphs ~f:(fun graph ->
               Graph.retry graph ~kind:Seal ~observe:(observe t));
             List.iter t.graphs ~f:(fun graph ->
               Graph.retry graph ~kind:Finish ~observe:(observe t));
             let report = snapshot t release in
             t.phase <- Finished (release, report);
             Report.observation report)))
;;

let retry t =
  match retry_observed t with
  | Error _ as refusal -> refusal
  | Ok observation -> CO.resume observation
;;
