open! Core

let unavailable code message =
  Agent_protocol.Error.create code ~message ~retryable:false ()
;;

module Failure = Constructor_inference_graph.Failure
module B = Agent_session.Runtime_builder
module CO = Agent_session.Close_observation
module O = Native_cleanup_observation

module Parent_cleanup_repair = struct
  module Native = Runtime_owner.Native_borrow_outcome
  module Release = Runtime_owner.Native_release
  module R = Runtime_owner.Cleanup_recovery

  type t =
    { owner : Runtime_owner.t
    ; domain : int
    ; cleanup : Native.Cleanup.t
    }

  module Current = struct
    type t =
      | Pin of unit O.t
      | Outer of unit O.t

    let observation = function
      | Pin observed | Outer observed -> observed
    ;;
  end

  let create cleanup ~owner = { owner; domain = (Stdlib.Domain.self () :> int); cleanup }

  let observation t ~current =
    let original = Native.Cleanup.observation t.cleanup in
    match current with
    | None -> original
    | Some current ->
      let selected = Current.observation current in
      CO.create
        (CO.exit selected)
        ~diagnostics:(CO.diagnostics original @ CO.diagnostics selected)
        ?primary:(if O.is_negative selected then CO.primary selected else None)
        ()
  ;;

  let retry t ~pin ~current =
    let open Result.Let_syntax in
    let refuse message = Error (unavailable Conflict message) in
    let%bind () =
      if Int.equal t.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else refuse "native parent repair belongs to another Eio domain"
    in
    match current with
    | Some current when not (O.is_negative (Current.observation current)) ->
      Ok (Some current)
    | None | Some _ ->
      (match pin with
       | Some release ->
         (match Native.Cleanup.pin_only_failure t.cleanup, current with
          | Some actual, (None | Some (Current.Pin _)) when phys_equal actual release ->
            Ok
              (Some
                 (Current.Pin
                    (O.capture_observed (fun () -> Release.close_observed release))))
          | (None | Some _), (None | Some (Pin _ | Outer _)) ->
            refuse "native parent repair changed its actual failed pin")
       | None ->
         (match current with
          | Some (Current.Pin _) ->
            refuse "native parent repair lost its actual failed pin"
          | None | Some (Outer _) ->
            let%bind actual = Native.Cleanup.outer_recovery t.cleanup ~owner:t.owner in
            (match actual with
             | None -> Ok current
             | Some (recovery, after) ->
               let finish snapshot =
                 let%bind observed = R.Snapshot.observation snapshot in
                 match R.Snapshot.progress snapshot with
                 | Joined ->
                   if O.is_negative observed
                   then refuse "joined native outer cleanup is negative"
                   else (
                     match Native.Cleanup.pin t.cleanup with
                     | Not_acquired -> refuse "native outer repair has no actual pin"
                     | Acquired { release; _ } ->
                       let%bind status = Release.status release in
                       (match status with
                        | Held -> refuse "native outer repair pin is still held"
                        | Released ->
                          (* This sticky release cannot repeat physical work. Its
                             existing port owns the sealed report before pruning
                             the exact detached provider row. Keep actual failure. *)
                          let handoff =
                            O.capture_observed (fun () -> Release.close_observed release)
                          in
                          let selected = O.combine observed handoff in
                          if O.is_negative handoff
                          then Ok (Some (Current.Outer selected))
                          else (
                            (* The actual Released port can yield on the Owner
                               lock. A concurrent ancestor reservation must not
                               turn skipped handoff into cached positive DATA. *)
                            let%bind actual =
                              Native.Cleanup.outer_recovery t.cleanup ~owner:t.owner
                            in
                            match actual with
                            | Some (same, latest)
                              when phys_equal same recovery && phys_equal latest snapshot
                              -> Ok (Some (Current.Outer selected))
                            | None | Some _ ->
                              refuse "native outer cleanup changed during handoff")))
                 | Retryable | Blocked | Terminal ->
                   if O.is_negative observed
                   then Ok (Some (Current.Outer observed))
                   else refuse "native outer cleanup custody is unfinished"
               in
               (match R.Snapshot.progress after with
                | Joined | Blocked | Terminal -> finish after
                | Retryable ->
                  (match R.retry recovery ~owner:t.owner ~after with
                   | Error refusal -> Error refusal
                   | Ok snapshot ->
                     (* Public snapshot intentionally exposes old DATA during a
                       reservation. Re-capture actual Complete/sealed state at
                       this publication boundary; no yield before comparison. *)
                     let%bind actual =
                       Native.Cleanup.outer_recovery t.cleanup ~owner:t.owner
                     in
                     (match actual with
                      | Some (same, latest)
                        when phys_equal same recovery && phys_equal latest snapshot ->
                        finish latest
                      | None | Some _ ->
                        refuse "native outer cleanup changed after its actual retry")
                   | exception exn ->
                     let trace = Stdlib.Printexc.get_raw_backtrace () in
                     Ok
                       (Some
                          (Current.Outer
                             (O.capture_observed (fun () ->
                                CO.create (Raised (exn, trace)) ~diagnostics:[] ())))))))))
  ;;
end

module Child = struct
  type evidence =
    { work : (unit, Failure.t) result
    ; release : Eio.Switch.Release_report.t option
    ; requested : bool
    }

  let result completion =
    match completion.work with
    | Error (Failure.Raised (Eio.Cancel.Cancelled Exit, _)) when completion.requested ->
      (match completion.release with
       | Some report ->
         (match
            ( Eio.Switch.Release_report.status report
            , Eio.Switch.Release_report.failures report )
          with
          | Complete, [] -> Ok ()
          | (Not_entered | Incomplete | Complete), (exn, backtrace) :: _ ->
            Error (Failure.Raised (exn, backtrace))
          | (Not_entered | Incomplete), [] -> completion.work)
       | None -> completion.work)
    | result -> result
  ;;

  let diagnostic_failures completion selected =
    let module F = Agent_session.Callback_cleanup.Failure in
    let selected_parts =
      match selected with
      | Ok () -> []
      | Error (Failure.Protocol error) -> [ F.Protocol error ]
      | Error (Failure.Raised (exn, trace)) -> [ F.Raised (exn, trace) ]
    in
    let release_parts =
      Option.value_map completion.release ~default:[] ~f:(fun report ->
        List.map (Eio.Switch.Release_report.failures report) ~f:(fun (exn, trace) ->
          F.Raised (exn, trace)))
    in
    match completion.work, release_parts with
    | Error (Failure.Raised (Eio.Cancel.Cancelled Exit, _)), _ :: _
      when completion.requested ->
      (* The existing result branch selects the first actual release failure.
         Present those actual parts once, rather than prefixing that part again. *)
      release_parts
    | (Ok () | Error _), _ -> selected_parts @ release_parts
  ;;

  type completion =
    { evidence : evidence
    ; observation : unit O.t
    }

  type t =
    { sw : Eio.Switch.t
    ; construction : B.Prepared_native.Construction.t
    ; stop : unit -> unit
    ; joined : completion Eio.Promise.t
    }

  let create ~sw ~capture =
    let ready, ready_u = Eio.Promise.create () in
    let joined, joined_u = Eio.Promise.create () in
    let requested = ref false in
    (* [Fiber.fork] silently skips a cancelled Switch. This synchronous public
       check has no yield before fork's own domain/On checks. *)
    Eio.Switch.check sw;
    Eio.Fiber.fork ~sw (fun () ->
      let captured =
        Failure.capture (fun () ->
          Eio.Cancel.sub (fun cancel ->
            Ok
              (Eio.Switch.run_reported (fun child_sw ->
                 let stopped, stopped_u = Eio.Promise.create () in
                 let child =
                   { sw = child_sw
                   ; construction = B.Prepared_native.Construction.create ~sw:child_sw
                   ; stop =
                       (fun () ->
                         if not !requested
                         then (
                           requested := true;
                           Eio.Promise.resolve stopped_u ();
                           Eio.Cancel.cancel cancel Exit))
                   ; joined
                   }
                 in
                 (* Actual partial custody precedes publication of the switch. *)
                 capture child;
                 Eio.Promise.resolve ready_u (Ok child);
                 Eio.Promise.await stopped;
                 Ok ()))))
      in
      let work, release =
        match captured with
        | Error failure -> Error failure, None
        | Ok outcome ->
          let work =
            match Eio.Switch.Outcome.result outcome with
            | Ok result ->
              Result.map_error result ~f:(fun error -> Failure.Protocol error)
            | Error (exn, backtrace) -> Error (Failure.Raised (exn, backtrace))
          in
          work, Some (Eio.Switch.Outcome.release outcome)
      in
      let evidence = { work; release; requested = !requested } in
      let result = result evidence in
      (* Keep one actual Switch carrier, including supporting release parts
         which Eio's aggregate may omit. Raw work and release evidence remain
         separate; requested healthy cancellation keeps its existing result. *)
      let exit =
        match result with
        | Ok () -> CO.Returned (Ok ())
        | Error (Failure.Protocol error) -> Returned (Error error)
        | Error (Failure.Raised (exn, trace)) -> Raised (exn, trace)
      in
      let diagnostic =
        match diagnostic_failures evidence result with
        | [] -> None
        | failures -> Some (Agent_session.Cleanup_diagnostic.create failures)
      in
      let observation =
        CO.create
          exit
          ~diagnostics:(Option.to_list diagnostic)
          ?primary:(if Result.is_error result then diagnostic else None)
          ()
      in
      Eio.Promise.resolve joined_u { evidence; observation };
      if Option.is_none (Eio.Promise.peek ready)
      then
        Eio.Promise.resolve
          ready_u
          (match result with
           | Error failure -> Error failure
           | Ok () ->
             Error
               (Failure.Protocol
                  (unavailable
                     Interrupted
                     "native child switch exited before preparation"))));
    Eio.Promise.await ready |> Failure.resume
  ;;

  let close_observed t =
    O.capture_observed (fun () ->
      Eio.Cancel.protect (fun () ->
        t.stop ();
        (Eio.Promise.await t.joined).observation))
  ;;

  let switch t = t.sw
  let construction t = t.construction
end

type parent =
  { domain : int
  ; mutable release : Runtime_owner.Native_release.t option
  ; mutable cleanup :
      (Runtime_owner.Native_borrow_outcome.Cleanup.t * Parent_cleanup_repair.t) option
  ; mutable current : Parent_cleanup_repair.Current.t option
  ; mutable failure_reported : bool
  }

type independent =
  { cleanup : Independent_resources.Cleanup.t
  ; mutable failure_reported : bool
  }

type t =
  { scope : Runtime_native_scope.t
  ; mutable child : Child.t option
  ; mutable tracking : Graph_tracking.t option
  ; mutable parents : parent list
  ; mutable independent : independent list
  ; mutable started : bool
  ; mutable local_attempts : int
  ; mutable local_result : unit O.t option
  }

let create ~sw =
  { scope = Runtime_native_scope.create ~sw
  ; child = None
  ; tracking = None
  ; parents = []
  ; independent = []
  ; started = false
  ; local_attempts = 0
  ; local_result = None
  }
;;

let scope t = t.scope
let started t = t.started <- true
let capture_child t child = t.child <- Some child
let capture_tracking t tracking = t.tracking <- Some tracking

let independent t =
  let cleanup = Independent_resources.Cleanup.create () in
  t.independent <- { cleanup; failure_reported = false } :: t.independent;
  cleanup
;;

let independent_snapshot holder =
  let module C = Independent_resources.Cleanup in
  let open Result.Let_syntax in
  let%bind snapshot = C.snapshot holder.cleanup in
  Result.of_option
    snapshot
    ~error:(unavailable Conflict "Independent ancestry original scope is unfinished")
;;

let independent_observation snapshot =
  let module C = Independent_resources.Cleanup in
  let selected =
    match C.Snapshot.cleanup snapshot, C.Snapshot.cleanup_controls snapshot with
    | Error refusal, Error _ -> O.capture (fun () -> Error refusal)
    | Ok cleanup, Error refusal -> O.combine (O.capture (fun () -> Error refusal)) cleanup
    | Error refusal, Ok controls ->
      let fixed = List.fold controls ~init:(O.return ()) ~f:O.combine in
      O.combine (O.capture (fun () -> Error refusal)) fixed
    | Ok cleanup, Ok controls ->
      let fixed = List.fold controls ~init:(O.return ()) ~f:O.combine in
      let observed = O.combine fixed cleanup in
      if O.is_negative observed || C.Snapshot.custody_joined snapshot
      then observed
      else
        O.combine
          observed
          (O.capture (fun () ->
             Error (unavailable Conflict "Independent ancestry custody is unfinished")))
  in
  (* Original encounter order is independent of current structural selection.
     These are aliases of the actual graph/ancestor/control producers, never
     copies of their failure payloads. *)
  CO.create
    (CO.exit selected)
    ~diagnostics:
      (Cleanup_original_view.diagnostics (C.Snapshot.original_view snapshot)
       @ CO.diagnostics selected)
    ?primary:(CO.primary selected)
    ()
;;

let close_independent holder =
  let module C = Independent_resources.Cleanup in
  match independent_snapshot holder with
  | Error refusal -> O.capture (fun () -> Error refusal)
  | Ok snapshot ->
    if not holder.failure_reported
    then (
      (* As with the exact native pin, surface the original negative before any
         later explicit factual repair. No retry runs inside this first read. *)
      let original = independent_observation snapshot in
      if O.is_negative original then holder.failure_reported <- true;
      original)
    else if C.Snapshot.custody_joined snapshot
    then independent_observation snapshot
    else (
      match C.retry_observed holder.cleanup with
      | Not_started refusal ->
        O.combine (O.capture (fun () -> Error refusal)) (independent_observation snapshot)
      | Completed current -> independent_observation current
      | Stopped { refusal; current } ->
        (* A later refusal cannot discard already completed graph/lease repair. *)
        O.combine (O.capture (fun () -> Error refusal)) (independent_observation current))
;;

let parent t =
  let parent =
    { domain = (Stdlib.Domain.self () :> int)
    ; release = None
    ; cleanup = None
    ; current = None
    ; failure_reported = false
    }
  in
  t.parents <- parent :: t.parents;
  parent
;;

module Parent = struct
  type t = parent

  let check_domain t =
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then invalid_arg "native parent cleanup belongs to another Eio domain"
  ;;

  let capture_release t release =
    check_domain t;
    match t.release with
    | None -> t.release <- Some release
    | Some _ -> invalid_arg "native parent release already captured"
  ;;

  let record t ~owner outcome =
    check_domain t;
    if Option.is_some t.cleanup
    then invalid_arg "native parent cleanup report already recorded";
    let cleanup = Runtime_owner.Native_borrow_outcome.cleanup outcome in
    (match t.release, Runtime_owner.Native_borrow_outcome.Cleanup.pin cleanup with
     | None, Not_acquired -> ()
     | Some captured, Acquired { release; _ } when phys_equal captured release -> ()
     | (Some _ | None), (Not_acquired | Acquired _) ->
       invalid_arg "native parent report changed its actual captured release");
    t.cleanup <- Some (cleanup, Parent_cleanup_repair.create cleanup ~owner)
  ;;
end

let common_observation t =
  O.capture_observed (fun () ->
    match t.child with
    | None -> O.return ()
    | Some child ->
      let common = B.Prepared_native.Construction.close_reported child.construction in
      CO.create
        (match CO.exit common with
         | Returned () -> Returned (Ok ())
         | Raised (exn, trace) -> Raised (exn, trace))
        ~diagnostics:(CO.diagnostics common)
        ?primary:(CO.primary common)
        ())
;;

let close_local_observed t =
  t.local_attempts <- t.local_attempts + 1;
  let result =
    O.capture_observed (fun () ->
      (* A failed common leg retains the live switch/ancestor extent for the
         NEXT explicit close. Never consume its finalizers before that retry. *)
      let common = common_observation t in
      if O.is_negative common
      then common
      else (
        let child =
          O.capture_observed (fun () ->
            Option.value_map t.child ~default:(O.return ()) ~f:Child.close_observed)
        in
        let sealed =
          O.capture (fun () ->
            Option.value_map t.tracking ~default:(Ok ()) ~f:Graph_tracking.seal)
        in
        let factual =
          O.capture (fun () ->
            Option.value_map t.tracking ~default:(Ok ()) ~f:Graph_tracking.finish)
        in
        O.combine common (O.combine child (O.combine sealed factual))))
  in
  t.local_result <- Some result;
  result
;;

let close_local t = close_local_observed t |> CO.resume

let close_parent (parent : parent) =
  match parent.cleanup with
  | None ->
    O.capture (fun () ->
      Error (unavailable Conflict "native parent cleanup report is not available"))
  | Some (cleanup, repair) ->
    let module Native = Runtime_owner.Native_borrow_outcome in
    let original = Native.Cleanup.observation cleanup in
    if not (O.is_negative original)
    then original
    else if not parent.failure_reported
    then (
      parent.failure_reported <- true;
      original)
    else (
      let pin = Native.Cleanup.pin_only_failure cleanup in
      (match Parent_cleanup_repair.retry repair ~pin ~current:parent.current with
       | Error _ -> ()
       | Ok current -> parent.current <- current);
      Parent_cleanup_repair.observation repair ~current:parent.current)
;;

let with_prior_diagnostics prior selected =
  CO.create
    (CO.exit selected)
    ~diagnostics:(CO.diagnostics prior @ CO.diagnostics selected)
    ?primary:(CO.primary selected)
    ()
;;

let close_observed t =
  O.capture_observed (fun () ->
    Eio.Cancel.protect (fun () ->
      (* Cell invokes this sole composite only after ALL native/Product refs
         join. A retained driver is never waited on after a failed close. *)
      let before_attempt = t.local_attempts in
      let scoped =
        O.capture_observed (fun () -> Runtime_native_scope.close_observed t.scope)
      in
      let exited =
        O.capture (fun () ->
          if t.started then Runtime_native_scope.driver_has_exited t.scope else Ok false)
      in
      match CO.exit exited with
      | Raised _ | Returned (Error _) ->
        let selected =
          CO.create
            (match CO.exit exited with
             | Raised (exn, trace) -> Raised (exn, trace)
             | Returned (Error error) -> Returned (Error error)
             | Returned (Ok _) -> assert false)
            ~diagnostics:(CO.diagnostics exited)
            ?primary:(CO.primary exited)
            ()
        in
        with_prior_diagnostics scoped selected
      | Returned (Ok false) ->
        if t.started
        then scoped
        else (
          (* Before Scope.prepare starts, captured partial legs still belong to
             this actual Cell. There is no driver using them. *)
          let local = O.capture_observed (fun () -> close_local_observed t) in
          O.combine scoped local)
      | Returned (Ok true) ->
        (* No wait remains: actual exit was observed. A current local failure is
           returned once, never retried within this call. *)
        let local =
          if Int.equal before_attempt t.local_attempts
          then O.capture_observed (fun () -> close_local_observed t)
          else Option.value_exn t.local_result
        in
        if O.is_negative local
        then with_prior_diagnostics scoped local
        else (
          let parents =
            List.fold (List.rev t.parents) ~init:(O.return ()) ~f:(fun first parent ->
              let actual = O.capture_observed (fun () -> close_parent parent) in
              O.combine first actual)
          in
          let independent =
            List.fold (List.rev t.independent) ~init:(O.return ()) ~f:(fun first holder ->
              let actual = O.capture_observed (fun () -> close_independent holder) in
              O.combine first actual)
          in
          with_prior_diagnostics scoped (O.combine local (O.combine parents independent)))))
;;

let close t = close_observed t |> CO.resume
