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

type compiled =
  { id : string
  ; kind : S.kind
  ; source : Chatmd_shell_spec.Source_ref.t
  ; source_text : string
  ; source_sha256 : string
  ; program : R.compiled_script
  ; surface : Surface.surface
  ; entrypoint : string
  ; limits : limits
  }

type instance =
  { compiled : compiled
  ; lifecycle : Chatmd_shell_spec.Shell_spec.lifecycle
  ; config : R.runtime_config
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

let wrapper entrypoint =
  sprintf
    "\n\
     let __ochat_shell_initial_state = initial_state\n\
     let __ochat_shell_on_event = fun ctx st ev -> %s(ev, st)\n"
    entrypoint
;;

let compile ~script =
  let surface = surface script.S.kind in
  let source_text = S.source_text script in
  let entrypoint = entrypoint script.kind in
  let source = source_text ^ wrapper entrypoint in
  match R.compile_script ~surface ~source () with
  | Ok program ->
    Ok
      { id = script.id
      ; kind = script.kind
      ; source = script.source_ref
      ; source_text
      ; source_sha256 = script.source_sha256
      ; program
      ; surface
      ; entrypoint
      ; limits = limits_of_script script.limits
      }
  | Error message ->
    Error [ error ~source:script.source_ref "shell.chatml_compile" message ]
;;

let with_entrypoint compiled entrypoint =
  let source = compiled.source_text ^ wrapper entrypoint in
  match R.compile_script ~surface:compiled.surface ~source () with
  | Ok program -> Ok { compiled with program; entrypoint }
  | Error message -> Error (error ~source:compiled.source "shell.chatml_compile" message)
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

let instantiate_session (compiled : compiled) (config : R.runtime_config) =
  R.instantiate_session
    config
    compiled.program
    ~entrypoints:
      { initial_state_name = "__ochat_shell_initial_state"
      ; on_event_name = "__ochat_shell_on_event"
      }
;;

let instantiate ~env ?limits ~lifecycle (compiled : compiled) =
  let limits = Option.value limits ~default:compiled.limits in
  let config =
    R.{ surface = compiled.surface; operations = List.map operation_names ~f:operation }
  in
  let session =
    match lifecycle with
    | Chatmd_shell_spec.Shell_spec.Invocation -> Ok None
    | Session | Runtime -> Result.map (instantiate_session compiled config) ~f:Option.some
  in
  Result.map_error session ~f:(error ~source:compiled.source "shell.chatml_instantiate")
  |> Result.map ~f:(fun session ->
    { compiled
    ; lifecycle
    ; config
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
    instantiate_session instance.compiled instance.config
  | (Session | Runtime), Some session -> Ok session
  | (Session | Runtime), None -> Error "stateful ChatML extension has no session"
;;

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
        Result.bind
          (R.handle_event
             ~prepare_commit
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

let snapshot_session session =
  let module Snapshot = Session.Snapshot in
  Result.bind
    (Snapshot.of_value (R.current_state session))
    ~f:(fun state ->
      R.queued_events session
      |> List.map ~f:Snapshot.of_value
      |> Result.all
      |> Result.map ~f:(fun queued_events ->
        Snapshot.Record
          [ "ochat_shell_extension_snapshot", Int 1
          ; "state", state
          ; "queued_events", Array queued_events
          ; "halted", Bool (R.is_halted session)
          ]))
;;

let snapshot_unlocked instance =
  match instance.lifecycle, instance.session with
  | Chatmd_shell_spec.Shell_spec.Invocation, _ ->
    Error "invocation-scoped ChatML extensions are not persistent"
  | (Session | Runtime), Some session -> snapshot_session session
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

let persist_or_rollback instance before =
  match instance.snapshot_handler with
  | None -> Ok ()
  | Some persist ->
    Result.bind (snapshot_unlocked instance) ~f:(fun snapshot ->
      match persist snapshot with
      | Ok () -> Ok ()
      | Error message ->
        Result.bind (restore_unlocked instance before) ~f:(fun () -> Error message))
;;

let call instance ~context ~event =
  let run () =
    try
      Eio.Time.with_timeout_exn
        (Eio.Stdenv.clock instance.env)
        instance.limits.wall_time_seconds
        (fun () ->
           let before =
             Option.bind instance.snapshot_handler ~f:(fun _ ->
               Result.ok (snapshot_unlocked instance))
           in
           Result.bind (call_unlocked instance ~context ~event) ~f:(fun action ->
             match before with
             | None -> Ok action
             | Some before ->
               Result.map (persist_or_rollback instance before) ~f:(fun () -> action)))
    with
    | Eio.Time.Timeout -> Error "shell ChatML hook timed out"
  in
  Eio.Mutex.use_rw ~protect:true instance.mutex run
  |> Result.map_error ~f:(error ~source:instance.compiled.source "shell.chatml_runtime")
;;

let id compiled = compiled.id
let kind compiled = compiled.kind
let source_sha256 (compiled : compiled) = compiled.source_sha256
let entrypoint (compiled : compiled) = compiled.entrypoint
let limits (compiled : compiled) = compiled.limits
let instance_id instance = instance.compiled.id
let instance_kind instance = instance.compiled.kind
let instance_source_sha256 instance = instance.compiled.source_sha256

let snapshot instance =
  Eio.Mutex.use_rw ~protect:true instance.mutex (fun () -> snapshot_unlocked instance)
  |> Result.map_error ~f:(error ~source:instance.compiled.source "shell.chatml_snapshot")
;;

let restore instance snapshot =
  Eio.Mutex.use_rw ~protect:true instance.mutex (fun () ->
    restore_unlocked instance snapshot)
  |> Result.map_error ~f:(error ~source:instance.compiled.source "shell.chatml_restore")
;;

let set_snapshot_handler instance handler = instance.snapshot_handler <- Some handler
