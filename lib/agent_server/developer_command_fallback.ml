open! Core
module P = Agent_protocol
module Store = Agent_store.Developer_run_store
module Receipt = Store.Command.Receipt

type t =
  { check_cancelled : Receipt.t
  ; check_failed : Receipt.t
  ; check_interrupted : Receipt.t
  ; fixture_cancelled : Receipt.t
  ; fixture_interrupted : Receipt.t
  ; live_cancelled : Receipt.t
  ; live_failed : Receipt.t
  ; live_interrupted : Receipt.t
  ; task_cancelled : Receipt.t
  ; task_failed : Receipt.t
  ; task_interrupted : Receipt.t
  }

let limited () =
  P.Error.create
    P.Error.Resource_limit
    ~message:"developer command limit reached"
    ~retryable:false
    ()
;;

let create limits =
  let open Result.Let_syntax in
  let document = Store.Limits.document limits in
  let commands = Store.Limits.commands limits in
  let terminal_cap = Agent_store.Developer_command_limits.max_terminal_bytes commands in
  let aggregate_cap = Agent_store.Developer_command_limits.max_bytes commands in
  let admit value =
    let%bind receipt =
      Receipt.create value ~limits:document ~poll:(fun () -> Eio.Fiber.yield ())
      |> Result.map_error ~f:(fun _ -> limited ())
    in
    let bytes = Receipt.encoded_bytes receipt in
    if
      bytes <= terminal_cap
      && bytes <= aggregate_cap
      && bytes <= Document_schema.Limits.max_bytes document
    then Ok receipt
    else Error (limited ())
  in
  let%bind check_cancelled = admit (Receipt.Check Receipt.Check.Cancelled) in
  let%bind check_failed =
    admit
      (Receipt.Check
         (Receipt.Check.Rejected
            { Receipt.Diagnostic.code = "developer.command.failed"
            ; message = "developer check failed before producing a result"
            ; location = None
            }))
  in
  let%bind check_interrupted = admit (Receipt.Check Receipt.Check.Interrupted) in
  let%bind fixture_cancelled = admit (Receipt.Fixture Receipt.Fixture.Cancelled) in
  let%bind fixture_interrupted = admit (Receipt.Fixture Receipt.Fixture.Interrupted) in
  let%bind live_cancelled = admit (Receipt.Live Receipt.Live.Cancelled) in
  let%bind live_failed =
    admit
      (Receipt.Live
         (Receipt.Live.Failed
            (P.Error.create
               P.Error.Invalid_state
               ~message:"developer command failed before producing a result"
               ~retryable:false
               ())))
  in
  let%bind live_interrupted = admit (Receipt.Live Receipt.Live.Interrupted) in
  let%bind task_cancelled = admit (Receipt.Task Receipt.Task.Cancelled) in
  let%bind task_failed =
    admit
      (Receipt.Task
         (Receipt.Task.Failed
            { error =
                P.Error.create
                  P.Error.Invalid_state
                  ~message:"developer REPL Task failed before producing a result"
                  ~retryable:false
                  ()
            ; diagnostic = None
            }))
  in
  let%map task_interrupted = admit (Receipt.Task Receipt.Task.Interrupted) in
  { check_cancelled
  ; check_failed
  ; check_interrupted
  ; fixture_cancelled
  ; fixture_interrupted
  ; live_cancelled
  ; live_failed
  ; live_interrupted
  ; task_cancelled
  ; task_failed
  ; task_interrupted
  }
;;

let interrupted t = function
  | Receipt.Kind.Check -> t.check_interrupted
  | Fixture_run -> t.fixture_interrupted
  | Live_run -> t.live_interrupted
  | Repl_task_run -> t.task_interrupted
;;

let choose t kind ~cancelled ~typed_failure =
  match kind with
  | Receipt.Kind.Check ->
    if cancelled
    then t.check_cancelled
    else if typed_failure
    then t.check_failed
    else t.check_interrupted
  | Fixture_run -> if cancelled then t.fixture_cancelled else t.fixture_interrupted
  | Live_run ->
    if cancelled
    then t.live_cancelled
    else if typed_failure
    then t.live_failed
    else t.live_interrupted
  | Repl_task_run ->
    if cancelled
    then t.task_cancelled
    else if typed_failure
    then t.task_failed
    else t.task_interrupted
;;
