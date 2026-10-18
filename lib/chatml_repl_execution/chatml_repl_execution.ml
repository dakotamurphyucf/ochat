open Core
module Typing = Chatml_repl_typing
module Modules = Chatml_linked_runtime.Repl_modules
module Execution = Chatml_execution
module Surface = Chatml.Chatml_builtin_surface
module Snapshot = Chatml_typechecker.Type_snapshot
module Lang = Chatml.Chatml_lang
module Eval = Chatml.Chatml_eval
module Builtins = Chatml.Chatml_builtin_spec
module Runtime = Chatml_host_runtime

module Limits = struct
  type t =
    { max_preview_bytes : int
    ; max_preview_nodes : int
    ; max_preview_depth : int
    ; max_diagnostic_bytes : int
    ; max_retained_tasks : int
    }

  let create
        ~max_preview_bytes
        ~max_preview_nodes
        ~max_preview_depth
        ~max_diagnostic_bytes
        ~max_retained_tasks
    =
    if max_preview_bytes <= 0 || max_preview_bytes > 65_536
    then Or_error.error_string "REPL preview bytes must be in 1..65536"
    else if max_preview_nodes <= 0 || max_preview_nodes > 4_096
    then Or_error.error_string "REPL preview nodes must be in 1..4096"
    else if max_preview_depth <= 0 || max_preview_depth > 128
    then Or_error.error_string "REPL preview depth must be in 1..128"
    else if max_diagnostic_bytes <= 0 || max_diagnostic_bytes > 16_384
    then Or_error.error_string "REPL diagnostic bytes must be in 1..16384"
    else if max_retained_tasks <= 0 || max_retained_tasks > 256
    then Or_error.error_string "REPL retained tasks must be in 1..256"
    else
      Ok
        { max_preview_bytes
        ; max_preview_nodes
        ; max_preview_depth
        ; max_diagnostic_bytes
        ; max_retained_tasks
        }
  ;;
end

module Error = struct
  type t =
    | Closed
    | Faulted_owner
    | Busy
    | Foreign_task_handle
    | Stale_task_handle
    | Typing of Typing.Error.t
    | Modules of Modules.Error.t
    | Lowering of Chatml_diagnostic.t
    | Budget of Execution.error
  [@@deriving sexp_of]
end

module Task_handle = struct
  type t = { owner : unit ref }

  let equal lhs rhs = phys_equal lhs rhs
end

module Diagnostic = struct
  type t =
    { message : string
    ; site : (Chatml_source.Location.t * int) option
    }

  let message t = t.message
  let location t = Option.map t.site ~f:fst
  let source_bytes t = Option.map t.site ~f:snd
end

module Receipt = struct
  type failure_kind =
    | Initializer
    | Runtime
    | Budget
  [@@deriving equal, sexp_of]

  type outcome =
    | Succeeded
    | Failed of failure_kind
  [@@deriving equal, sexp_of]

  type task_retention =
    | Not_a_task
    | Retained of Task_handle.t
    | Limit_reached

  type t =
    { outcome : outcome
    ; frozen_types : Typing.Frozen_types.t
    ; value_preview : string option
    ; diagnostic : Diagnostic.t option
    ; task_retention : task_retention
    }

  let outcome t = t.outcome
  let frozen_types t = t.frozen_types
  let expression_type t = Typing.Frozen_types.expression_type t.frozen_types
  let value_preview t = t.value_preview
  let diagnostic t = t.diagnostic
  let task_retention t = t.task_retention
end

module Task_result = struct
  type failure_kind =
    | Runtime
    | Budget
  [@@deriving equal, sexp_of]

  type outcome =
    | Succeeded
    | Failed of failure_kind
  [@@deriving equal, sexp_of]

  type t =
    { outcome : outcome
    ; value_preview : string option
    ; diagnostic : Diagnostic.t option
    }

  let outcome t = t.outcome
  let value_preview t = t.value_preview
  let diagnostic t = t.diagnostic
end

type retained_task =
  { handle : Task_handle.t
  ; task : Lang.Task.t
  ; source : Chatml_source.t
  }

type t =
  { typing : Typing.t
  ; modules : Modules.t
  ; runner : Execution.runner
  ; surface : Surface.surface
  ; policy : Execution.policy
  ; limits : Limits.t
  ; owner : unit ref
  ; mutable environment : Lang.env
  ; mutable tasks : retained_task list
  ; mutable busy : bool
  ; mutable faulted : bool
  ; mutable closed : bool
  }

let utf8_prefix text max_bytes =
  let output = Buffer.create (Int.min max_bytes (String.length text)) in
  let rec loop offset =
    if offset < String.length text && Buffer.length output < max_bytes
    then (
      let decoded = Stdlib.String.get_utf_8_uchar text offset in
      let width = Stdlib.Uchar.utf_decode_length decoded in
      let chunk =
        if Stdlib.Uchar.utf_decode_is_valid decoded
        then String.sub text ~pos:offset ~len:width
        else "?"
      in
      if String.length chunk <= max_bytes - Buffer.length output
      then (
        Buffer.add_string output chunk;
        loop (offset + width)))
  in
  loop 0;
  Buffer.contents output
;;

let preview (limits : Limits.t) value =
  Builtins.value_to_debug_string
    ~max_bytes:limits.Limits.max_preview_bytes
    ~max_nodes:limits.max_preview_nodes
    ~max_depth:limits.max_preview_depth
    value
  |> fun rendered -> utf8_prefix rendered limits.max_preview_bytes
;;

let bounded_text (limits : Limits.t) text = utf8_prefix text limits.max_diagnostic_bytes

let diagnostic_of_text limits message =
  { Diagnostic.message = bounded_text limits message; site = None }
;;

let diagnostic_of_owned limits (owned : Chatml_diagnostic.t) =
  { Diagnostic.message = bounded_text limits owned.message
  ; site =
      Option.map owned.site ~f:(fun site ->
        ( Chatml_diagnostic.Site.location site
        , String.length (Chatml_source.contents (Chatml_diagnostic.Site.source site)) ))
  }
;;

let modules_diagnostic limits = function
  | Modules.Error.Runtime { diagnostic; _ }
  | Faulted { diagnostic; _ }
  | Runtime_surface diagnostic
  | Invalid_linkage diagnostic
  | Lowering diagnostic -> diagnostic_of_owned limits diagnostic
  | Closed -> diagnostic_of_text limits "Configured-module runtime is closed"
  | Busy -> diagnostic_of_text limits "Configured-module runtime is busy"
  | Prepared_mismatch -> diagnostic_of_text limits "Configured-module input changed"
  | Typing _ -> diagnostic_of_text limits "REPL typing reservation failed"
  | Foreign_admission ->
    diagnostic_of_text limits "Configured-module admission is foreign"
  | Consumed_admission ->
    diagnostic_of_text limits "Configured-module admission was consumed"
  | Finalized_admission ->
    diagnostic_of_text limits "Configured-module admission was finalized"
;;

let create ~typing ~surface ~env ~policy ~limits ~checkpoint =
  let runner = Execution.create_runner ~env ~policy () in
  let control = Execution.runner_control runner in
  match Modules.create ~typing ~surface ~control ~checkpoint with
  | Error error -> Error (Error.Modules error)
  | Ok modules ->
    (match
       try
         Execution.run_scoped runner (fun () ->
           Chatml_builtin_modules.create_controlled_env_with_surface ~control surface)
       with
       | exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         (try ignore (Modules.close modules : (unit, Modules.Error.t) result) with
          | _ -> ());
         Exn.raise_with_original_backtrace exn backtrace
     with
     | Error error ->
       ignore (Modules.close modules : (unit, Modules.Error.t) result);
       Error (Error.Budget error)
     | Ok environment ->
       Ok
         { typing
         ; modules
         ; runner
         ; surface
         ; policy
         ; limits
         ; owner = ref ()
         ; environment
         ; tasks = []
         ; busy = false
         ; faulted = false
         ; closed = false
         })
;;

type successful_statement =
  { staged : Lang.env option
  ; value_preview : string option
  ; task : Lang.Task.t option
  }

type attempted_statement =
  | Success of successful_statement
  | Failure of Receipt.failure_kind * Diagnostic.t

let run_statement t candidate resolved admission =
  let source = Typing.Candidate.source candidate in
  Execution.run_scoped t.runner (fun () ->
    match Modules.initialize t.modules admission with
    | Error error -> Failure (Initializer, modules_diagnostic t.limits error)
    | Ok configured_modules ->
      (match resolved with
       | None -> Success { staged = None; value_preview = None; task = None }
       | Some statement ->
         (match
            Eval.eval_captured_statement
              t.environment
              ~source
              ~configured_modules
              statement
          with
          | Error error -> Failure (Runtime, diagnostic_of_owned t.limits error)
          | Ok staged ->
            let value = Eval.Staged_statement.expression_value staged in
            let value_preview = Option.map value ~f:(preview t.limits) in
            let task =
              match value with
              | Some (Lang.VTask task) -> Some task
              | Some
                  ( VInt _
                  | VBool _
                  | VFloat _
                  | VString _
                  | VVariant _
                  | VRecord _
                  | VArray _
                  | VTuple _
                  | VRef _
                  | VClosure _
                  | VModule _
                  | VUnit
                  | VBuiltin _ )
              | None -> None
            in
            Success
              { staged = Some (Eval.Staged_statement.environment staged)
              ; value_preview
              ; task
              })))
;;

let current_task_handle t handle =
  (not t.closed)
  && (not t.faulted)
  && phys_equal handle.Task_handle.owner t.owner
  && List.exists t.tasks ~f:(fun retained -> Task_handle.equal retained.handle handle)
;;

let discard_if_current t candidate =
  match Typing.validate_candidate t.typing candidate with
  | Error _ -> ()
  | Ok () ->
    ignore (Typing.discard_candidate t.typing candidate : (unit, Typing.Error.t) result)
;;

let execute_next t candidate ~checkpoint =
  if t.closed
  then Error Error.Closed
  else if t.faulted
  then Error Error.Faulted_owner
  else if t.busy
  then Error Error.Busy
  else (
    t.busy <- true;
    Exn.protect
      ~finally:(fun () -> t.busy <- false)
      ~f:(fun () ->
        let open Result.Let_syntax in
        let%bind () =
          Typing.validate_candidate t.typing candidate
          |> Result.map_error ~f:(fun error -> Error.Typing error)
        in
        let resolved =
          try
            Chatml_resolver.resolve_repl_statement
              (Typing.Candidate.checked candidate)
              (Typing.Candidate.parsed candidate)
              ~statement_index:(Typing.Candidate.statement_index candidate)
              ~checkpoint
          with
          | exn ->
            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
            discard_if_current t candidate;
            Exn.raise_with_original_backtrace exn backtrace
        in
        match resolved with
        | Error error ->
          discard_if_current t candidate;
          Error (Error.Lowering error)
        | Ok resolved ->
          let%bind admission =
            Modules.reserve t.modules candidate ~checkpoint
            |> Result.map_error ~f:(fun error -> Error.Modules error)
          in
          let reservation = Modules.Admission.typing_reservation admission in
          let attempted =
            match run_statement t candidate resolved admission with
            | Ok attempted -> attempted
            | Error error -> Failure (Budget, diagnostic_of_text t.limits error.message)
            | exception exn ->
              let backtrace = Stdlib.Printexc.get_raw_backtrace () in
              (match
                 Typing.finalize_execution
                   reservation
                   ~outcome:Typing.Published.Constrained_before_runtime_failure
               with
               | Ok _ -> ()
               | Error _ -> t.faulted <- true);
              Exn.raise_with_original_backtrace exn backtrace
          in
          let prepared_task =
            match attempted with
            | Failure _ | Success { task = None; _ } -> Receipt.Not_a_task, t.tasks
            | Success { task = Some _; _ }
              when List.length t.tasks >= t.limits.max_retained_tasks ->
              Receipt.Limit_reached, t.tasks
            | Success { task = Some task; _ } ->
              let handle = { Task_handle.owner = t.owner } in
              ( Retained handle
              , { handle; task; source = Typing.Candidate.source candidate } :: t.tasks )
          in
          let outcome =
            match attempted with
            | Success _ -> Typing.Published.After_success
            | Failure _ -> Typing.Published.Constrained_before_runtime_failure
          in
          let%bind published =
            Typing.finalize_execution reservation ~outcome
            |> Result.map_error ~f:(fun error ->
              t.faulted <- true;
              Error.Typing error)
          in
          let frozen_types = Typing.Published.frozen_types published in
          (match attempted with
           | Failure (kind, diagnostic) ->
             Ok
               { Receipt.outcome = Failed kind
               ; frozen_types
               ; value_preview = None
               ; diagnostic = Some diagnostic
               ; task_retention = Not_a_task
               }
           | Success { staged; value_preview; task = _ } ->
             Option.iter staged ~f:(fun environment -> t.environment <- environment);
             let task_retention, tasks = prepared_task in
             t.tasks <- tasks;
             Ok
               { Receipt.outcome = Succeeded
               ; frozen_types
               ; value_preview
               ; diagnostic = None
               ; task_retention
               })))
;;

let interpreter_limits epoch_policy action_policy =
  let of_policy = function
    | Execution.Unrestricted -> None
    | Bounded limits -> Some Runtime.{ fuel = limits.fuel; max_tasks = limits.max_tasks }
  in
  match of_policy epoch_policy, of_policy action_policy with
  | None, None -> None
  | Some limits, None | None, Some limits -> Some limits
  | Some epoch, Some action ->
    Some
      Runtime.
        { fuel = Int.min epoch.fuel action.fuel
        ; max_tasks = Int.min epoch.max_tasks action.max_tasks
        }
;;

let drive_retained
      t
      handle
      ~operations
      ?scoped_async_operation
      ?prepare_result
      ~action_policy
      ()
  =
  if t.closed
  then Error Error.Closed
  else if t.faulted
  then Error Error.Faulted_owner
  else if t.busy
  then Error Error.Busy
  else if not (phys_equal handle.Task_handle.owner t.owner)
  then Error Error.Foreign_task_handle
  else (
    match
      List.find t.tasks ~f:(fun retained -> Task_handle.equal retained.handle handle)
    with
    | None -> Error Error.Stale_task_handle
    | Some retained ->
      t.busy <- true;
      Exn.protect
        ~finally:(fun () -> t.busy <- false)
        ~f:(fun () ->
          let projected = ref None in
          let prepare_result =
            Option.map prepare_result ~f:(fun prepare ~value ~local_effects ->
              projected := Some (preview t.limits value);
              prepare ~value ~local_effects)
          in
          let config : Runtime.runtime_config = { surface = t.surface; operations } in
          let executed =
            Execution.run_scoped_with_host_budget
              t.runner
              ~policy:action_policy
              (fun () ->
                 Runtime.run_retained_task_detailed
                   ~control:(Execution.runner_control t.runner)
                   ?scoped_async_operation
                   ?limits:(interpreter_limits t.policy action_policy)
                   ?prepare_result
                   config
                   ~environment:t.environment
                   ~task:retained.task
                   ~source:retained.source
                   ~check_now:(fun () -> Execution.check_runner_now t.runner)
                   ()
                 |> Result.map ~f:(fun value ->
                   match !projected with
                   | Some preview -> preview
                   | None -> preview t.limits value))
          in
          match executed with
          | Error error ->
            Ok
              { Task_result.outcome = Failed Budget
              ; value_preview = None
              ; diagnostic = Some (diagnostic_of_text t.limits error.message)
              }
          | Ok (Error diagnostic) ->
            Ok
              { Task_result.outcome = Failed Runtime
              ; value_preview = None
              ; diagnostic = Some (diagnostic_of_owned t.limits diagnostic)
              }
          | Ok (Ok value_preview) ->
            Ok
              { Task_result.outcome = Succeeded
              ; value_preview = Some value_preview
              ; diagnostic = None
              }))
;;

let close t =
  if t.busy
  then Error Error.Busy
  else if t.closed
  then Ok ()
  else (
    match Typing.close t.typing with
    | Error error -> Error (Error.Typing error)
    | Ok () ->
      (match Modules.close t.modules with
       | Error error ->
         t.faulted <- true;
         Error (Error.Modules error)
       | Ok () ->
         t.closed <- true;
         t.tasks <- [];
         Ok ()))
;;
