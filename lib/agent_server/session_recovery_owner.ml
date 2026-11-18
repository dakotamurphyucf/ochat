open! Core
module Report = Disposal_report_data

type t =
  { store : Agent_store.Session_store.t
  ; handle : Agent_store.Session_store.Handle.t
  ; job_capacity : Job_capacity.t
  ; mutable capacity : Session_capacity.t option
  ; mutable writer : Agent_store.Commit_writer.t option
  ; mutable actor : Agent_session.Session_actor.t option
  ; mutable runtime : Runtime_owner.t option
  ; mutable retention : Session_retention.t option
  ; entry_close_mutex : Eio.Mutex.t
  ; close_mutex : Eio.Mutex.t
  ; mutable closing : bool
  ; mutable retention_joined : bool
  ; mutable jobs_closed : bool
  ; mutable actor_closed : bool
  ; mutable capacity_released : bool
  ; mutable writer_closed : bool
  ; mutable handle_closed : bool
  ; mutable checkpoint_failure : Agent_protocol.Error.t option
  ; mutable prepared_report : Report.t option
  ; mutable closed_report : Report.t option
  ; mutable handle_error : Report.Diagnostic.t option
  }

let create ~store ~handle ~job_capacity ~capacity =
  if not (Agent_store.Session_store.owns_handle store handle)
  then
    Error
      (Agent_protocol.Error.create
         Persistence_error
         ~message:"recovery resources belong to a different store"
         ~retryable:false
         ())
  else
    Ok
      { store
      ; handle
      ; job_capacity
      ; capacity
      ; writer = None
      ; actor = None
      ; runtime = None
      ; retention = None
      ; entry_close_mutex = Eio.Mutex.create ()
      ; close_mutex = Eio.Mutex.create ()
      ; closing = false
      ; retention_joined = false
      ; jobs_closed = false
      ; actor_closed = false
      ; capacity_released = false
      ; writer_closed = false
      ; handle_closed = false
      ; checkpoint_failure = None
      ; prepared_report = None
      ; closed_report = None
      ; handle_error = None
      }
;;

let session_id t = Agent_store.Session_store.Handle.session_id t.handle
let handle t = t.handle
let owns_actor t actor = Option.exists t.actor ~f:(phys_equal actor)
let owns_runtime t runtime = Option.exists t.runtime ~f:(phys_equal runtime)
let is_closing t = t.closing
let checkpoint_failure t = t.checkpoint_failure
let record_checkpoint_failure t failure = t.checkpoint_failure <- Some failure

let adopt_capacity_exn t capacity =
  if t.closing || Option.is_some t.capacity || Option.is_some t.writer
  then failwith "recovery capacity ownership is out of order";
  t.capacity <- Some capacity
;;

let adopt_writer_exn t writer =
  if t.closing || Option.is_some t.writer
  then failwith "recovery writer ownership has already transferred";
  t.writer <- Some writer
;;

let adopt_actor_exn t actor =
  if t.closing || Option.is_none t.writer || Option.is_some t.actor
  then failwith "recovery actor ownership is out of order";
  t.actor <- Some actor
;;

let adopt_retention_exn t retention =
  if t.closing || Option.is_none t.actor || Option.is_some t.retention
  then failwith "recovery retention ownership is out of order";
  t.retention <- Some retention
;;

let adopt_runtime_exn t runtime =
  if t.closing || Option.is_none t.actor || Option.is_some t.runtime
  then failwith "recovery runtime ownership is out of order";
  t.runtime <- Some runtime
;;

(* A report records actual stages, independently of legacy delivery. Only a
   positive actual stage is cached; a new failed retry owns a fresh diagnostic. *)
let failed_report ?prior failure =
  let diagnostic = Report.Diagnostic.create [ failure ] in
  let diagnostics =
    Option.value_map prior ~default:[] ~f:Report.diagnostics @ [ diagnostic ]
  in
  Report.create ~primary:diagnostic ~disposition:Retained diagnostics, diagnostic
;;

let prepare_close_reported_unlocked t =
  match t.prepared_report with
  | Some report -> report
  | None ->
    t.closing <- true;
    let retention =
      try
        if not t.retention_joined
        then (
          Option.iter t.retention ~f:Session_retention.close_and_wait;
          t.retention_joined <- true);
        Ok ()
      with
      | exn -> Error (Report.Failure.Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
    in
    (match retention with
     | Error failure -> fst (failed_report failure)
     | Ok () ->
       let runtime =
         try
           match t.runtime with
           | None -> Report.create ~disposition:Disposed []
           | Some runtime -> Runtime_owner.close_and_wait_reported runtime
         with
         | exn -> fst (failed_report (Raised (exn, Stdlib.Printexc.get_raw_backtrace ())))
       in
       (match Report.disposition runtime with
        | Retained -> runtime
        | Disposed ->
          t.prepared_report <- Some runtime;
          runtime))
;;

let prepare_close_reported t =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_ro t.close_mutex (fun () -> prepare_close_reported_unlocked t))
;;

let close_reported_unlocked t =
  match t.closed_report with
  | Some report -> report
  | None ->
    let prepared = prepare_close_reported_unlocked t in
    (match Report.disposition prepared with
     | Retained -> prepared
     | Disposed ->
       let closed =
         try
           if not t.jobs_closed
           then (
             Job_capacity.close_session t.job_capacity ~session_id:(session_id t);
             t.jobs_closed <- true);
           if not t.actor_closed
           then (
             Option.iter t.actor ~f:Agent_session.Session_actor.shutdown;
             t.actor_closed <- true);
           if not t.capacity_released
           then (
             Option.iter t.capacity ~f:Session_capacity.release;
             t.capacity_released <- true);
           if not t.writer_closed
           then (
             Option.iter t.writer ~f:Agent_store.Commit_writer.close;
             t.writer_closed <- true);
           if t.handle_closed
           then Ok ()
           else (
             (* This is the only actual legacy Result-valued release stage.
                Its carrier records origin even if other payloads coincide. *)
             t.handle_error <- None;
             match Agent_store.Session_store.close_session t.store t.handle with
             | Ok () ->
               t.handle_closed <- true;
               Ok ()
             | Error failure ->
               Error
                 (Report.Failure.Protocol
                    (Agent_store.Store_error.to_protocol_error failure)))
         with
         | exn ->
           Error (Report.Failure.Raised (exn, Stdlib.Printexc.get_raw_backtrace ()))
       in
       (match closed with
        | Ok () ->
          let report = Report.combine ~disposition:Disposed [ prepared ] in
          t.closed_report <- Some report;
          report
        | Error failure ->
          let report, diagnostic = failed_report ~prior:prepared failure in
          (match failure with
           | Protocol _ -> t.handle_error <- Some diagnostic
           | Raised _ -> ());
          report))
;;

let close_reported t =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_ro t.close_mutex (fun () -> close_reported_unlocked t))
;;

let retained_error () =
  Agent_protocol.Error.create
    Conflict
    ~message:"session recovery resources remain retained"
    ~retryable:true
    ()
;;

let raise_failure = function
  | Report.Failure.Protocol failure -> raise (Runtime_owner.Cleanup_failed failure)
  | Raised (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let prepare_close t =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_ro t.close_mutex (fun () ->
      let report = prepare_close_reported_unlocked t in
      match Report.take_legacy_failure_exn report with
      | Some failure -> raise_failure failure
      | None ->
        (match Report.disposition report with
         | Disposed -> ()
         | Retained -> raise (Runtime_owner.Cleanup_failed (retained_error ())))))
;;

let close ?capture_report t =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_ro t.close_mutex (fun () ->
      let report = close_reported_unlocked t in
      (* The trusted observer is invocation-local and runs before delivery under
         the SAME coordinator, so a later retry cannot replace report origin. *)
      Option.iter capture_report ~f:(fun capture -> capture report);
      (* Consume the exact origin while this actual close attempt remains under
         its stage coordinator; a later retry cannot replace the Handle carrier. *)
      match Report.take_legacy_diagnostic_exn report with
      | Some (diagnostic, Report.Failure.Protocol failure)
        when Option.exists t.handle_error ~f:(Report.Diagnostic.equal diagnostic) ->
        Error failure
      | Some (_, failure) -> raise_failure failure
      | None ->
        (match Report.disposition report with
         | Disposed -> Ok ()
         | Retained -> raise (Runtime_owner.Cleanup_failed (retained_error ())))))
;;

let with_entry_close t f =
  Eio.Cancel.protect (fun () -> Eio.Mutex.use_ro t.entry_close_mutex f)
;;
