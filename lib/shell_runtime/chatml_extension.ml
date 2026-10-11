open! Core
module D = Chatmd_shell_spec.Diagnostic
module L = Chatml.Chatml_lang
module R = Chatml_host_runtime
module S = Chatmd_shell_spec.Chatmd_script_spec
module Surface = Chatml.Chatml_builtin_surface

type action =
  | Match of bool * string
  | Review_approve
  | Review_approve_for of string
  | Review_deny of string
  | Review_rewrite of string list
  | Review_defer
  | Intercept_continue
  | Intercept_rewrite of string list
  | Intercept_respond of string * string
  | Intercept_reject of string
  | Result_keep
  | Result_replace of string * string
  | Result_reject_disclosure of string
  | Effect_add of string
  | Effect_replace of string list
  | Audit_keep
  | Audit_drop_field of string
  | Audit_replace of string
[@@deriving sexp, compare, equal]

type limits =
  { wall_time_seconds : float
  ; fuel : int
  ; max_tasks : int
  ; max_actions : int
  ; max_value_bytes : int
  ; max_string_bytes : int
  ; max_array_items : int
  ; max_depth : int
  }

type program =
  | Legacy of R.compiled_script
  | Prepared of Chatml_linked_program.t

type compiled =
  { declaration : S.t
  ; id : string
  ; kind : S.kind
  ; source : Chatmd_shell_spec.Source_ref.t
  ; source_text : string
  ; source_sha256 : string
  ; program : program
  ; alternatives : Chatml_linked_program.t String.Map.t
  ; surface : Surface.surface
  ; entrypoint : string
  ; limits : limits
  }

type instance =
  { compiled : compiled
  ; lifecycle : Chatmd_shell_spec.Shell_spec.lifecycle
  ; config : R.runtime_config
  ; execution : Chatml_execution.runner option
  ; mutable session : R.session option
  ; mutable snapshot_handler : (Session.Snapshot.t -> (unit, string) result) option
  ; mutex : Eio.Mutex.t
  ; env : Eio_unix.Stdenv.base
  ; limits : limits
  }

let default_limits =
  let limits = S.default_limits in
  let codec = Chatml_codec.of_script_limits limits in
  { wall_time_seconds = Chatmd_shell_spec.Duration.to_seconds limits.wall_time
  ; fuel = limits.fuel
  ; max_tasks = limits.max_tasks
  ; max_actions = 1
  ; max_value_bytes = codec.max_value_bytes
  ; max_string_bytes = codec.max_string_bytes
  ; max_array_items = codec.max_array_items
  ; max_depth = codec.max_depth
  }
;;

let limits_of_script limits =
  let codec = Chatml_codec.of_script_limits limits in
  { wall_time_seconds = Chatmd_shell_spec.Duration.to_seconds limits.S.wall_time
  ; fuel = limits.fuel
  ; max_tasks = limits.max_tasks
  ; max_actions = 1
  ; max_value_bytes = codec.max_value_bytes
  ; max_string_bytes = codec.max_string_bytes
  ; max_array_items = codec.max_array_items
  ; max_depth = codec.max_depth
  }
;;

let error ?source code message = D.error ?source ~code message

let surface = function
  | S.Shell_matcher -> Surface.shell_matcher_surface
  | Shell_reviewer -> Surface.shell_reviewer_surface
  | Shell_before_interceptor -> Surface.shell_before_interceptor_surface
  | Shell_after_interceptor -> Surface.shell_after_interceptor_surface
  | Shell_effect_analyzer -> Surface.shell_effect_surface
  | Shell_audit_filter -> Surface.shell_audit_surface
  | Moderator -> Surface.moderator_surface
;;

let entrypoint = function
  | S.Shell_matcher -> "match_command"
  | Shell_reviewer -> "review"
  | Shell_before_interceptor -> "before"
  | Shell_after_interceptor -> "after"
  | Shell_effect_analyzer -> "analyze"
  | Shell_audit_filter -> "filter"
  | Moderator -> "on_event"
;;

let default_entrypoint = entrypoint

let wrapper entrypoint =
  sprintf
    "\n\
     let __ochat_shell_initial_state = initial_state\n\
     let __ochat_shell_on_event = fun ctx st ev -> %s(ev, st)\n"
    entrypoint
;;

let compile ~script =
  match script.S.project_selection with
  | Some _ ->
    Error
      [ error
          ~source:script.source_ref
          "shell.chatml_missing_program"
          "Configured shell script requires captured project preparation"
      ]
  | None ->
    let surface = surface script.S.kind in
    let source_text = S.source_text script in
    let entrypoint = entrypoint script.kind in
    let source = source_text ^ wrapper entrypoint in
    (match R.compile_script ~surface ~source () with
     | Ok program ->
       Ok
         { declaration = script
         ; id = script.id
         ; kind = script.kind
         ; source = script.source_ref
         ; source_text
         ; source_sha256 = script.source_sha256
         ; program = Legacy program
         ; alternatives = String.Map.empty
         ; surface
         ; entrypoint
         ; limits = limits_of_script script.limits
         }
     | Error message ->
       Error [ error ~source:script.source_ref "shell.chatml_compile" message ])
;;

let with_entrypoint compiled entrypoint =
  match compiled.program with
  | Prepared _ ->
    if String.equal compiled.entrypoint entrypoint
    then Ok compiled
    else (
      match Map.find compiled.alternatives entrypoint with
      | Some program -> Ok { compiled with program = Prepared program; entrypoint }
      | None ->
        Error
          (error
             ~source:compiled.source
             "shell.chatml_prepared_entrypoint"
             "The selected entrypoint requires an already compiled captured program"))
  | Legacy _ ->
    let source = compiled.source_text ^ wrapper entrypoint in
    (match R.compile_script ~surface:compiled.surface ~source () with
     | Ok program -> Ok { compiled with program = Legacy program; entrypoint }
     | Error message ->
       Error (error ~source:compiled.source "shell.chatml_compile" message))
;;

let with_prepared_entrypoints compiled ~alternatives ~checkpoint =
  let invalid message =
    Error (error ~source:compiled.source "shell.chatml_entrypoint_set" message)
  in
  checkpoint ();
  match compiled.program, compiled.kind with
  | Prepared program, S.Shell_matcher
    when String.equal compiled.entrypoint (default_entrypoint compiled.kind)
         && Map.is_empty compiled.alternatives ->
    let open Result.Let_syntax in
    let%map alternatives =
      List.fold_result alternatives ~init:String.Map.empty ~f:(fun found alternative ->
        checkpoint ();
        if not (S.equal compiled.declaration alternative.declaration)
        then invalid "Matcher alternative belongs to a different declaration"
        else if
          String.equal alternative.entrypoint compiled.entrypoint
          || Map.mem found alternative.entrypoint
        then invalid "Duplicate matcher entrypoint"
        else if not (Map.is_empty alternative.alternatives)
        then invalid "Matcher alternatives cannot contain another entrypoint set"
        else (
          match alternative.program with
          | Legacy _ -> invalid "Matcher alternative must be a prepared program"
          | Prepared program ->
            Ok (Map.set found ~key:alternative.entrypoint ~data:program)))
    in
    { compiled with
      alternatives = Map.set alternatives ~key:compiled.entrypoint ~data:program
    }
  | Prepared _, _ | Legacy _, _ ->
    invalid "Entrypoint alternatives require an unattached default prepared matcher"
;;

let compiler_contract kind ~entrypoint ~checkpoint =
  checkpoint ();
  if String.is_empty entrypoint || String.length entrypoint > 1024
  then
    Error
      (error "shell.chatml_entrypoint" "Shell entrypoint must contain 1 to 1024 bytes")
  else
    let module Spec = Chatml.Chatml_builtin_spec in
    let open Result.Let_syntax in
    let%map event =
      match kind with
      | S.Shell_matcher
      | Shell_reviewer
      | Shell_before_interceptor
      | Shell_effect_analyzer -> Ok Spec.shell_context_ty
      | Shell_after_interceptor -> Ok Spec.shell_result_ty
      | Shell_audit_filter -> Ok Spec.shell_audit_ty
      | Moderator ->
        Error
          (error
             "shell.chatml_kind"
             "Moderator is a distinct three-argument session role")
    in
    let projected =
      Chatml.Chatml_static_surface.of_runtime_surface (surface kind) ~checkpoint
    in
    let state = Spec.TVar "state" in
    Chatml.Chatml_compiler_contract.for_static_surface
      projected
      ~required_bindings:
        [ "initial_state", state
        ; entrypoint, Spec.TFun ([ event; state ], Spec.TCon ("task", [ state ]))
        ]
;;

let compile_prepared input ~script ~entrypoint ~limits ~max_dependency_depth ~env () =
  let open Result.Let_syntax in
  let source_text = S.source_text script in
  let%bind () =
    Chatml_compilation.validate_limits limits
    |> Result.map_error ~f:(fun failure ->
      [ error ~source:script.S.source_ref failure.code failure.message ])
  in
  let%bind digest =
    Chatml_source.Digest.of_hex script.S.source_sha256
    |> Result.map_error ~f:(fun _ ->
      [ error
          ~source:script.source_ref
          "shell.chatml_digest"
          "Invalid declared script digest"
      ])
  in
  let%bind contract =
    compiler_contract script.kind ~entrypoint ~checkpoint:Eio.Fiber.yield
    |> Result.map_error ~f:List.return
  in
  let%map program =
    Chatml_compilation.compile_prepared_with_contract
      input
      ~contract
      ~expected_entry:{ contents = source_text; digest }
      ~limits
      ~max_dependency_depth
      ~env
      ()
    |> Result.map_error ~f:(fun failure ->
      [ error ~source:script.source_ref failure.code failure.message ])
  in
  { declaration = script
  ; id = script.id
  ; kind = script.kind
  ; source = script.source_ref
  ; source_text
  ; source_sha256 = script.source_sha256
  ; program = Prepared program
  ; alternatives = String.Map.empty
  ; surface = surface script.kind
  ; entrypoint
  ; limits = limits_of_script script.limits
  }
;;

let expect_string limits name = function
  | L.VString value when String.length value <= limits.max_string_bytes -> Ok value
  | VString _ -> Error (name ^ " exceeds the string limit")
  | _ -> Error (name ^ " must be a string")
;;

let expect_strings limits name = function
  | L.VArray values when Array.length values <= limits.max_array_items ->
    Array.to_list values |> List.map ~f:(expect_string limits name) |> Result.all
  | VArray _ -> Error (name ^ " exceeds the array limit")
  | _ -> Error (name ^ " must be an array of strings")
;;

let nullary action name = function
  | [] -> Ok action
  | _ -> Error (name ^ " accepts no arguments")
;;

let unary limits constructor name = function
  | [ value ] -> Result.map (expect_string limits name value) ~f:constructor
  | _ -> Error (name ^ " accepts one string")
;;

let unary_strings limits constructor name = function
  | [ value ] -> Result.map (expect_strings limits name value) ~f:constructor
  | _ -> Error (name ^ " accepts one string array")
;;

let binary limits constructor name = function
  | [ left; right ] ->
    Result.bind (expect_string limits name left) ~f:(fun left ->
      Result.map (expect_string limits name right) ~f:(fun right ->
        constructor left right))
  | _ -> Error (name ^ " accepts two strings")
;;

let decode limits (eff : L.eff) =
  match eff.op with
  | "Match.yes" -> unary limits (fun reason -> Match (true, reason)) eff.op eff.args
  | "Match.no" -> unary limits (fun reason -> Match (false, reason)) eff.op eff.args
  | "Review.approve" -> nullary Review_approve eff.op eff.args
  | "Review.approve_for" ->
    unary limits (fun value -> Review_approve_for value) eff.op eff.args
  | "Review.deny" -> unary limits (fun value -> Review_deny value) eff.op eff.args
  | "Review.rewrite" ->
    unary_strings limits (fun value -> Review_rewrite value) eff.op eff.args
  | "Review.defer" -> nullary Review_defer eff.op eff.args
  | "Intercept.continue" -> nullary Intercept_continue eff.op eff.args
  | "Intercept.rewrite" ->
    unary_strings limits (fun value -> Intercept_rewrite value) eff.op eff.args
  | "Intercept.respond" ->
    binary limits (fun a b -> Intercept_respond (a, b)) eff.op eff.args
  | "Intercept.reject" ->
    unary limits (fun value -> Intercept_reject value) eff.op eff.args
  | "Result.keep" -> nullary Result_keep eff.op eff.args
  | "Result.replace" -> binary limits (fun a b -> Result_replace (a, b)) eff.op eff.args
  | "Result.reject_disclosure" ->
    unary limits (fun value -> Result_reject_disclosure value) eff.op eff.args
  | "Effect.add" -> unary limits (fun value -> Effect_add value) eff.op eff.args
  | "Effect.read_path" ->
    unary limits (fun value -> Effect_add ("read:" ^ value)) eff.op eff.args
  | "Effect.write_path" ->
    unary limits (fun value -> Effect_add ("write:" ^ value)) eff.op eff.args
  | "Effect.network" -> nullary (Effect_add "network") eff.op eff.args
  | "Effect.child_processes" -> nullary (Effect_add "child_processes") eff.op eff.args
  | "Effect.arbitrary_code" -> nullary (Effect_add "arbitrary_code") eff.op eff.args
  | "Effect.privilege_change" -> nullary (Effect_add "privilege_change") eff.op eff.args
  | "Effect.replace" ->
    unary_strings limits (fun value -> Effect_replace value) eff.op eff.args
  | "Audit.keep" -> nullary Audit_keep eff.op eff.args
  | "Audit.drop_field" ->
    unary limits (fun value -> Audit_drop_field value) eff.op eff.args
  | "Audit.replace" -> unary limits (fun value -> Audit_replace value) eff.op eff.args
  | name -> Error ("unexpected shell ChatML operation: " ^ name)
;;

let operation name =
  R.
    { name
    ; kind = Local_transactional
    ; perform = (fun _ _ -> Ok L.VUnit)
    ; phase_check = allow_all_phases
    }
;;

let operation_names =
  [ "Match.yes"
  ; "Match.no"
  ; "Review.approve"
  ; "Review.approve_for"
  ; "Review.deny"
  ; "Review.rewrite"
  ; "Review.defer"
  ; "Intercept.continue"
  ; "Intercept.rewrite"
  ; "Intercept.respond"
  ; "Intercept.reject"
  ; "Result.keep"
  ; "Result.replace"
  ; "Result.reject_disclosure"
  ; "Effect.add"
  ; "Effect.read_path"
  ; "Effect.write_path"
  ; "Effect.network"
  ; "Effect.child_processes"
  ; "Effect.arbitrary_code"
  ; "Effect.privilege_change"
  ; "Effect.replace"
  ; "Audit.keep"
  ; "Audit.drop_field"
  ; "Audit.replace"
  ]
;;

let run_controlled execution f =
  match execution with
  | None -> f ()
  | Some execution ->
    Chatml_execution.run_scoped execution f
    |> Result.map_error ~f:(fun failure -> failure.Chatml_execution.message)
    |> Result.join
;;

let instantiate_session (compiled : compiled) (config : R.runtime_config) execution =
  match compiled.program with
  | Legacy program ->
    R.instantiate_session
      config
      program
      ~entrypoints:
        { initial_state_name = "__ochat_shell_initial_state"
        ; on_event_name = "__ochat_shell_on_event"
        }
  | Prepared program ->
    let control = Option.map execution ~f:Chatml_execution.runner_control in
    R.instantiate_prepared_event_state_session_detailed
      config
      program
      ~entrypoints:
        { initial_state_name = "initial_state"; on_event_name = compiled.entrypoint }
      ?control
      ()
    |> Result.map_error ~f:(fun diagnostic ->
      Chatml_diagnostic.render
        diagnostic
        ~label:"Runtime error"
        ~checkpoint:Eio.Fiber.yield)
;;

let instantiate ~env ?limits ~lifecycle (compiled : compiled) =
  let limits = Option.value limits ~default:compiled.limits in
  let config =
    R.{ surface = compiled.surface; operations = List.map operation_names ~f:operation }
  in
  let execution =
    match compiled.program with
    | Legacy _ -> None
    | Prepared _ ->
      Some
        (Chatml_execution.create_runner
           ~env
           ~policy:
             (Bounded
                { Chatml_execution.default_limits with
                  fuel = limits.fuel
                ; max_tasks = limits.max_tasks
                ; wall_seconds = limits.wall_time_seconds
                ; max_value_bytes = limits.max_value_bytes
                ; max_array_items = limits.max_array_items
                ; max_depth = limits.max_depth
                })
           ())
  in
  let session =
    match lifecycle with
    | Chatmd_shell_spec.Shell_spec.Invocation -> Ok None
    | Session | Runtime ->
      run_controlled execution (fun () -> instantiate_session compiled config execution)
      |> Result.map ~f:Option.some
  in
  Result.map_error session ~f:(error ~source:compiled.source "shell.chatml_instantiate")
  |> Result.map ~f:(fun session ->
    { compiled
    ; lifecycle
    ; config
    ; execution
    ; session
    ; snapshot_handler = None
    ; mutex = Eio.Mutex.create ()
    ; env
    ; limits
    })
;;

let session instance =
  match instance.lifecycle, instance.session with
  | Chatmd_shell_spec.Shell_spec.Invocation, _ ->
    instantiate_session instance.compiled instance.config instance.execution
  | (Session | Runtime), Some session -> Ok session
  | (Session | Runtime), None -> Error "stateful ChatML extension has no session"
;;

let snapshot_values ~state ~queued_events ~halted =
  let module Snapshot = Session.Snapshot in
  let open Result.Let_syntax in
  let%bind state = Snapshot.of_value state in
  let%map queued_events = List.map queued_events ~f:Snapshot.of_value |> Result.all in
  Snapshot.Record
    [ "ochat_shell_extension_snapshot", Int 1
    ; "state", state
    ; "queued_events", Array queued_events
    ; "halted", Bool halted
    ]
;;

exception Persistence_exception of exn * Backtrace.t

let call_unlocked instance ~context ~event =
  let codec_limits =
    Chatml_codec.
      { max_value_bytes = instance.limits.max_value_bytes
      ; max_string_bytes = instance.limits.max_string_bytes
      ; max_array_items = instance.limits.max_array_items
      ; max_depth = instance.limits.max_depth
      }
  in
  let runtime_limits =
    R.{ fuel = instance.limits.fuel; max_tasks = instance.limits.max_tasks }
  in
  let validate value =
    Chatml_codec.validate codec_limits value
    |> Result.map_error ~f:(fun diagnostic ->
      diagnostic.Chatmd_shell_spec.Diagnostic.message)
  in
  Result.bind (validate context) ~f:(fun () ->
    Result.bind (validate event) ~f:(fun () ->
      Result.bind (session instance) ~f:(fun session ->
        let action = ref None in
        let prepare_commit ~local_effects =
          if not (Int.equal (List.length local_effects) instance.limits.max_actions)
          then Error "shell ChatML hook must produce exactly one terminal action"
          else
            Result.map
              (decode instance.limits (List.hd_exn local_effects))
              ~f:(fun value -> fun () -> action := Some value)
        in
        let validate_state value =
          Chatml_codec.validate codec_limits value
          |> Result.map_error ~f:(fun diagnostic ->
            diagnostic.Chatmd_shell_spec.Diagnostic.message)
        in
        let persistent_handler =
          match instance.lifecycle with
          | Invocation -> None
          | Session | Runtime -> instance.snapshot_handler
        in
        let copy_state =
          match instance.compiled.program, persistent_handler with
          | Legacy _, None -> None
          | Prepared _, _ | Legacy _, Some _ ->
            Some
              (fun value ->
                Result.bind (validate_state value) ~f:(fun () ->
                  Session.Snapshot.of_value value
                  |> Result.bind ~f:Session.Snapshot.to_value))
        in
        let prepare_transaction =
          Option.map persistent_handler ~f:(fun persist (proposal : R.transaction) ->
            let open Result.Let_syntax in
            let%bind () =
              List.map proposal.queued_events ~f:validate_state
              |> Result.all
              |> Result.map ~f:ignore
            in
            let%bind snapshot =
              snapshot_values
                ~state:proposal.new_state
                ~queued_events:proposal.queued_events
                ~halted:proposal.halted
            in
            (* Every conversion/budget/cancellation check precedes durable write.
               Pending asynchronous cancellation cannot split successful persistence
               from the runtime's immediately following nonyielding installation. *)
            Eio.Fiber.check ();
            Option.iter instance.execution ~f:(fun runner ->
              (Chatml_execution.runner_control runner).diagnostic_checkpoint ());
            Eio.Cancel.protect (fun () ->
              let%map () =
                try persist snapshot with
                | exn ->
                  let backtrace = Backtrace.Exn.most_recent () in
                  raise (Persistence_exception (exn, backtrace))
              in
              ignore))
        in
        Result.bind
          (R.handle_event
             ~prepare_commit
             ?prepare_transaction
             ?copy_state
             ~validate_state
             ~limits:runtime_limits
             session
             ~context
             ~event)
          ~f:(fun () ->
            Result.of_option
              !action
              ~error:"shell ChatML hook committed without an action"))))
;;

let snapshot_session ?control session =
  let state = R.current_state session in
  let queued_events = R.queued_events session in
  Option.iter control ~f:(fun control ->
    control.L.check_value state;
    List.iter queued_events ~f:control.check_value);
  let result = snapshot_values ~state ~queued_events ~halted:(R.is_halted session) in
  Option.iter control ~f:(fun control -> control.L.diagnostic_checkpoint ());
  result
;;

let snapshot_unlocked instance =
  match instance.lifecycle, instance.session with
  | Chatmd_shell_spec.Shell_spec.Invocation, _ ->
    Error "invocation-scoped ChatML extensions are not persistent"
  | (Session | Runtime), Some session ->
    let control = Option.map instance.execution ~f:Chatml_execution.runner_control in
    snapshot_session ?control session
  | (Session | Runtime), None -> Error "stateful ChatML extension has no session"
;;

let snapshot_fields = function
  | Session.Snapshot.Record fields -> String.Map.of_alist fields
  | _ -> `Duplicate_key ""
;;

let decode_snapshot snapshot =
  let module Snapshot = Session.Snapshot in
  match snapshot_fields snapshot with
  | `Ok fields when Map.mem fields "ochat_shell_extension_snapshot" ->
    (match
       Map.find fields "state", Map.find fields "queued_events", Map.find fields "halted"
     with
     | Some state, Some (Array queued_events), Some (Bool halted) ->
       Result.bind (Snapshot.to_value state) ~f:(fun state ->
         queued_events
         |> List.map ~f:Snapshot.to_value
         |> Result.all
         |> Result.map ~f:(fun queued_events -> state, queued_events, halted))
     | _ -> Error "invalid stateful ChatML extension snapshot")
  | `Ok _ | `Duplicate_key _ ->
    Result.map (Snapshot.to_value snapshot) ~f:(fun state -> state, [], false)
;;

let restore_unlocked instance snapshot =
  match instance.lifecycle, instance.session with
  | Chatmd_shell_spec.Shell_spec.Invocation, _ ->
    Error "invocation-scoped ChatML extensions cannot be restored"
  | (Session | Runtime), Some session ->
    Result.bind (decode_snapshot snapshot) ~f:(fun (state, queued_events, halted) ->
      R.restore session ~state ~queued_events ~halted)
  | (Session | Runtime), None -> Error "stateful ChatML extension has no session"
;;

let call instance ~context ~event =
  let run () =
    try
      Eio.Time.with_timeout_exn
        (Eio.Stdenv.clock instance.env)
        instance.limits.wall_time_seconds
        (fun () -> call_unlocked instance ~context ~event)
    with
    | Eio.Time.Timeout -> Error "shell ChatML hook timed out"
  in
  let protect =
    match instance.compiled.program with
    | Legacy _ -> true
    | Prepared _ -> false
  in
  let outcome =
    Eio.Mutex.use_rw ~protect instance.mutex (fun () ->
      (* Host rollback and defensive state copying make these two exceptional
         paths consistent. Release the lock normally before propagating them;
         unexpected infrastructure failures still poison it. *)
      try Ok (run_controlled instance.execution run) with
      | Persistence_exception (exn, backtrace) -> Error (exn, backtrace)
      | Eio.Cancel.Cancelled _ as exn -> Error (exn, Backtrace.Exn.most_recent ()))
  in
  (match outcome with
   | Ok result -> result
   | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace)
  |> Result.map_error ~f:(error ~source:instance.compiled.source "shell.chatml_runtime")
;;

let source_revision (compiled : compiled) =
  match compiled.program with
  | Legacy _ -> compiled.source_sha256
  | Prepared program ->
    Chatml_linked_program.prepared program
    |> Chatml_prepared_program.fingerprint
    |> Chatml_source.Digest.to_hex
;;

let matches_script compiled script =
  S.equal compiled.declaration script
  && String.equal compiled.entrypoint (entrypoint script.S.kind)
;;

let id compiled = compiled.id
let kind compiled = compiled.kind
let source_sha256 (compiled : compiled) = compiled.source_sha256
let entrypoint (compiled : compiled) = compiled.entrypoint
let limits (compiled : compiled) = compiled.limits
let instance_id instance = instance.compiled.id
let instance_kind instance = instance.compiled.kind
let instance_source_sha256 instance = instance.compiled.source_sha256
let instance_source_revision instance = source_revision instance.compiled

let snapshot instance =
  Eio.Mutex.use_rw ~protect:true instance.mutex (fun () ->
    run_controlled instance.execution (fun () -> snapshot_unlocked instance))
  |> Result.map_error ~f:(error ~source:instance.compiled.source "shell.chatml_snapshot")
;;

let restore instance snapshot =
  Eio.Mutex.use_rw ~protect:true instance.mutex (fun () ->
    run_controlled instance.execution (fun () -> restore_unlocked instance snapshot))
  |> Result.map_error ~f:(error ~source:instance.compiled.source "shell.chatml_restore")
;;

let set_snapshot_handler instance handler = instance.snapshot_handler <- Some handler
