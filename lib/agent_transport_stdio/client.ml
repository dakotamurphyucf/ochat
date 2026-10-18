open! Core
module P = Agent_protocol

let invalid message = P.Error.invalid_request message

module Limits = struct
  type t =
    { max_line_length : int
    ; notification_capacity : int
    ; max_stderr_bytes : int
    ; shutdown_grace_seconds : float
    }

  let create
        ~max_line_length
        ~notification_capacity
        ~max_stderr_bytes
        ~shutdown_grace_seconds
    =
    if
      max_line_length <= 0
      || max_line_length > 16 * 1024 * 1024
      || notification_capacity <= 0
      || notification_capacity > 65_536
      || max_stderr_bytes <= 0
      || max_stderr_bytes > 1024 * 1024
      || (not (Float.is_finite shutdown_grace_seconds))
      || Float.(shutdown_grace_seconds <= 0.)
      || Float.(shutdown_grace_seconds > 30.)
    then Error (invalid "invalid stdio child client limits")
    else
      Ok
        { max_line_length
        ; notification_capacity
        ; max_stderr_bytes
        ; shutdown_grace_seconds
        }
  ;;
end

type completion = (Eio.Process.exit_status, exn * Stdlib.Printexc.raw_backtrace) result

type phase =
  | Open
  | Closing of
      { completion : completion Eio.Promise.t
      ; resolver : completion Eio.Promise.u
      }
  | Closed of completion

type t =
  { domain_id : int
  ; sw : Eio.Switch.t
  ; connection : Agent_transport_ndjson.Client.t
  ; child : Eio_unix.Process.ty Eio.Process.t
  ; stderr_source : [ Eio.Flow.source_ty | `Close ] Eio.Resource.t
  ; close_flows : unit -> unit
  ; stderr_buffer : Bytes.t
  ; mutable stderr_next : int
  ; mutable stderr_length : int
  ; stderr_mutex : Eio.Mutex.t
  ; stderr_finished : unit Eio.Promise.t
  ; stderr_failure : (exn * Stdlib.Printexc.raw_backtrace) option ref
  ; mutable stderr_shutdown_started : bool
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; limits : Limits.t
  ; phase_mutex : Eio.Mutex.t
  ; mutable phase : phase
  }

let check_domain t =
  if Int.equal t.domain_id (Stdlib.Domain.self () :> int)
  then Ok ()
  else Error (invalid "stdio child client belongs to another Eio domain")
;;

let connection t =
  match check_domain t with
  | Ok () -> Agent_transport_ndjson.Client.connection t.connection
  | Error _ -> invalid_arg "stdio child client belongs to another Eio domain"
;;

let stderr_tail t =
  match check_domain t with
  | Error _ -> invalid_arg "stdio child client belongs to another Eio domain"
  | Ok () ->
    Eio.Mutex.use_rw ~protect:true t.stderr_mutex (fun () ->
      let capacity = Bytes.length t.stderr_buffer in
      let start = if t.stderr_length < capacity then 0 else t.stderr_next in
      let first = Int.min t.stderr_length (capacity - start) in
      let prefix = Bytes.sub t.stderr_buffer ~pos:start ~len:first |> Bytes.to_string in
      if first = t.stderr_length
      then prefix
      else
        prefix
        ^ (Bytes.sub t.stderr_buffer ~pos:0 ~len:(t.stderr_length - first)
           |> Bytes.to_string))
;;

let error_of_exception exn =
  P.Error.create
    Interrupted
    ~message:("stdio child cleanup failed: " ^ Exn.to_string exn)
    ~retryable:true
    ()
;;

let value_of_completion = function
  | Ok status -> Ok status
  | Error (exn, backtrace) ->
    (match exn with
     | Eio.Cancel.Cancelled _ -> Exn.raise_with_original_backtrace exn backtrace
     | _ -> Error (error_of_exception exn))
;;

let capture f =
  match f () with
  | value -> Ok value
  | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
;;

let raise_completion = function
  | Ok value -> value
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let await_exit t =
  let wait () = Ok (Eio.Process.await t.child) in
  let grace = t.limits.shutdown_grace_seconds in
  match Eio.Time.with_timeout t.clock grace wait with
  | Ok status -> status
  | Error `Timeout ->
    Eio.Process.signal t.child Stdlib.Sys.sigterm;
    (match Eio.Time.with_timeout t.clock grace wait with
     | Ok status -> status
     | Error `Timeout ->
       Eio.Process.signal t.child Stdlib.Sys.sigkill;
       Eio.Time.with_timeout_exn t.clock grace (fun () -> Eio.Process.await t.child))
;;

let cleanup t =
  let connection_close =
    capture (fun () ->
      Agent_client.Connection.close
        (Agent_transport_ndjson.Client.connection t.connection))
  in
  Eio.Fiber.check ();
  let child_status = capture (fun () -> await_exit t) in
  Eio.Fiber.check ();
  (* A descendant may retain a copy of the child's stdout/stderr write end.
     Close our readers after exact-child settlement instead of awaiting that
     descendant's EOF. Attempt every close and join even if another failed. *)
  t.stderr_shutdown_started <- true;
  let protocol_close = capture t.close_flows in
  Eio.Fiber.check ();
  let stderr_close = capture (fun () -> Eio.Flow.close t.stderr_source) in
  Eio.Fiber.check ();
  let protocol_join =
    capture (fun () -> Agent_transport_ndjson.Client.close_and_wait t.connection)
  in
  Eio.Fiber.check ();
  let stderr_join = capture (fun () -> Eio.Promise.await t.stderr_finished) in
  Eio.Fiber.check ();
  List.iter
    [ connection_close; protocol_close; stderr_close; protocol_join; stderr_join ]
    ~f:(fun outcome -> ignore (raise_completion outcome : unit));
  Option.iter !(t.stderr_failure) ~f:(fun (exn, backtrace) ->
    Exn.raise_with_original_backtrace exn backtrace);
  raise_completion child_status
;;

let settle_completion t resolver outcome =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true t.phase_mutex (fun () ->
      t.phase <- Closed outcome;
      Eio.Promise.resolve resolver outcome))
;;

let start_close_worker t resolver =
  (* The one cleanup worker belongs to the child switch, not the caller's
     cancellation scope. A locally cancelled caller stops waiting while
     cleanup and every other waiter continue to share this completion. *)
  let started =
    capture (fun () ->
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        let outcome =
          capture (fun () ->
            let status = cleanup t in
            Eio.Fiber.check ();
            status)
        in
        match outcome with
        | Error (Eio.Cancel.Cancelled _, _) when Eio.Fiber.is_cancelled () ->
          (* Eio's switch-owned process release hook reaps after the switch
             exits. The outer bracket settles the promise then. *)
          ()
        | Ok _ | Error _ -> settle_completion t resolver outcome))
  in
  match started with
  | Ok () -> ()
  | Error _ when Option.is_some (Eio.Switch.get_error t.sw) ->
    (* A cancelled or released owner cannot start another worker. The
       post-switch settlement reads its release-hook process status. *)
    ()
  | Error error -> settle_completion t resolver (Error error)
;;

let close_and_wait t =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  let step =
    Eio.Cancel.protect (fun () ->
      let step =
        Eio.Mutex.use_rw ~protect:true t.phase_mutex (fun () ->
          match t.phase with
          | Closed outcome -> `Completed outcome
          | Closing { completion; resolver = _ } -> `Follow completion
          | Open ->
            let completion, resolver = Eio.Promise.create () in
            t.phase <- Closing { completion; resolver };
            `Lead (completion, resolver))
      in
      (match step with
       | `Lead (_, resolver) -> start_close_worker t resolver
       | `Completed _ | `Follow _ -> ());
      step)
  in
  match step with
  | `Completed outcome -> value_of_completion outcome
  | `Follow completion | `Lead (completion, _) ->
    Eio.Promise.await completion |> value_of_completion
;;

let settle_after_switch t =
  Eio.Cancel.protect (fun () ->
    (* The switch has joined all reader fibers and Eio's process release hook
       has reaped the exact child. Reader completion promises need not have
       been resolved if a fork was cancelled before its body began. *)
    t.stderr_shutdown_started <- true;
    ignore
      (capture (fun () ->
         Agent_client.Connection.close
           (Agent_transport_ndjson.Client.connection t.connection)));
    ignore (capture t.close_flows);
    ignore (capture (fun () -> Eio.Flow.close t.stderr_source));
    let outcome = capture (fun () -> Eio.Process.await t.child) in
    Eio.Mutex.use_rw ~protect:true t.phase_mutex (fun () ->
      match t.phase with
      | Open -> t.phase <- Closed outcome
      | Closing { completion = _; resolver } ->
        t.phase <- Closed outcome;
        Eio.Promise.resolve resolver outcome
      | Closed _ -> ()))
;;

let append_stderr t chunk =
  Eio.Mutex.use_rw ~protect:true t.stderr_mutex (fun () ->
    let capacity = Bytes.length t.stderr_buffer in
    let count = String.length chunk in
    if count >= capacity
    then (
      Stdlib.Bytes.blit_string chunk (count - capacity) t.stderr_buffer 0 capacity;
      t.stderr_next <- 0;
      t.stderr_length <- capacity)
    else (
      let first = Int.min count (capacity - t.stderr_next) in
      Stdlib.Bytes.blit_string chunk 0 t.stderr_buffer t.stderr_next first;
      Stdlib.Bytes.blit_string chunk first t.stderr_buffer 0 (count - first);
      t.stderr_next <- (t.stderr_next + count) % capacity;
      t.stderr_length <- Int.min capacity (t.stderr_length + count)))
;;

let drain_stderr t =
  let scratch = Cstruct.create 4_096 in
  let rec loop () =
    match Eio.Flow.single_read t.stderr_source scratch with
    | count ->
      append_stderr t (Cstruct.to_string (Cstruct.sub scratch 0 count));
      loop ()
    | exception End_of_file -> ()
  in
  loop ()
;;

let with_spawned ~env ~argv ~(limits : Limits.t) ~f =
  match argv with
  | [] -> Error (invalid "stdio child executable is required")
  | _ ->
    let owned = ref None in
    let switched =
      capture (fun () ->
        Eio.Switch.run (fun sw ->
          let manager = Eio.Stdenv.process_mgr env in
          let stdin_source, stdin_sink = Eio.Process.pipe ~sw manager in
          let stdout_source, stdout_sink = Eio.Process.pipe ~sw manager in
          let stderr_source, stderr_sink = Eio.Process.pipe ~sw manager in
          let child =
            Eio.Process.spawn
              ~sw
              manager
              ~stdin:stdin_source
              ~stdout:stdout_sink
              ~stderr:stderr_sink
              argv
          in
          Eio.Flow.close stdin_source;
          Eio.Flow.close stdout_sink;
          Eio.Flow.close stderr_sink;
          let flows_closed = ref false in
          let close_flows () =
            if not !flows_closed
            then (
              flows_closed := true;
              Eio.Cancel.protect (fun () ->
                let stdin_close = capture (fun () -> Eio.Flow.close stdin_sink) in
                let stdout_close = capture (fun () -> Eio.Flow.close stdout_source) in
                ignore (raise_completion stdin_close : unit);
                ignore (raise_completion stdout_close : unit)))
          in
          let open Result.Let_syntax in
          let%bind connection =
            Agent_transport_ndjson.Client.create
              ~sw
              ~transport_name:"stdio"
              ~input:stdout_source
              ~output:stdin_sink
              ~close_flows
              ~max_line_length:limits.max_line_length
              ~notification_capacity:limits.notification_capacity
          in
          let stderr_finished, stderr_finished_u = Eio.Promise.create () in
          let t =
            { domain_id = (Stdlib.Domain.self () :> int)
            ; sw
            ; connection
            ; child
            ; stderr_source
            ; close_flows
            ; stderr_buffer = Bytes.create limits.max_stderr_bytes
            ; stderr_next = 0
            ; stderr_length = 0
            ; stderr_mutex = Eio.Mutex.create ()
            ; stderr_finished
            ; stderr_failure = ref None
            ; stderr_shutdown_started = false
            ; clock = Eio.Stdenv.clock env
            ; limits
            ; phase_mutex = Eio.Mutex.create ()
            ; phase = Open
            }
          in
          owned := Some t;
          Eio.Fiber.fork ~sw (fun () ->
            Exn.protect
              ~f:(fun () ->
                match drain_stderr t with
                | () -> ()
                | exception Eio.Cancel.Cancelled _ -> ()
                | exception exn ->
                  if not t.stderr_shutdown_started
                  then t.stderr_failure := Some (exn, Stdlib.Printexc.get_raw_backtrace ()))
              ~finally:(fun () -> Eio.Promise.resolve stderr_finished_u ()));
          let callback_outcome =
            match f t with
            | result -> `Result result
            | exception exn -> `Exception (exn, Stdlib.Printexc.get_raw_backtrace ())
          in
          (match callback_outcome with
           | `Exception ((Eio.Cancel.Cancelled _ as exn), backtrace) ->
             (* Eio's process reaper is owned by [sw] and may itself have been
            cancelled. Its release hook kills and reaps the exact child only
            as [sw] exits, so do not await the child from inside this switch. *)
             t.stderr_shutdown_started <- true;
             Exn.raise_with_original_backtrace exn backtrace
           | `Exception _ | `Result _ -> ());
          let cleanup_outcome = capture (fun () -> close_and_wait t) in
          match callback_outcome with
          | `Exception (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
          | `Result (Error error) ->
            (match cleanup_outcome with
             | Error ((Eio.Cancel.Cancelled _ as exn), backtrace) ->
               Exn.raise_with_original_backtrace exn backtrace
             | Ok _ | Error _ -> Error error)
          | `Result (Ok value) ->
            (match raise_completion cleanup_outcome with
             | Ok _ -> Ok value
             | Error _ as failure -> failure)))
    in
    Option.iter !owned ~f:(fun t -> ignore (capture (fun () -> settle_after_switch t)));
    (match switched with
     | Ok result -> result
     | Error ((Eio.Cancel.Cancelled _ as exn), backtrace) ->
       (* The nested switch has now joined its process release hook. Preserve
          the callback's original cancellation and backtrace. *)
       Exn.raise_with_original_backtrace exn backtrace
     | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace)
;;
