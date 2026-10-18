open! Core
module P = Agent_protocol
module Capture_host = Authoring_context_service.Developer
module Original_identity = Developer_command_original.Identity
module Source = Developer_command_source
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract
module Typing = Chatml_repl_typing
module Evaluator = Chatml_repl_execution
module Projection = Developer_repl_projection
module Scope = Chatml_configured_lookup.Repl_scope
module Parse = Chatml.Chatml_parse
module Captured = Chatml_source

let invalid message = P.Error.invalid_request message
let stale message = P.Error.create P.Error.Lease_stale ~message ~retryable:false ()
let busy message = P.Error.create P.Error.Conflict ~message ~retryable:true ()
let denied message = P.Error.create P.Error.Permission_denied ~message ~retryable:false ()
let limit message = P.Error.create P.Error.Resource_limit ~message ~retryable:false ()

exception Cleanup_invariant of exn * exn

let reraise_after_cleanup original backtrace ~cleanup =
  match cleanup () with
  | () -> Stdlib.Printexc.raise_with_backtrace original backtrace
  | exception cleanup_failure ->
    Stdlib.Printexc.raise_with_backtrace
      (Cleanup_invariant (original, cleanup_failure))
      backtrace
;;

let check_method ~original ~current =
  if
    (not (Operator_authorization.is_current original))
    || not (Operator_authorization.is_current current)
  then Error (denied "developer REPL actor is no longer current")
  else (
    let first = Operator_authorization.principal original in
    let latest = Operator_authorization.principal current in
    if
      (not (P.Id.Principal.equal first.id latest.id))
      || not (String.equal first.authentication_kind latest.authentication_kind)
    then Error (denied "developer REPL actor identity changed")
    else if
      P.Principal.has_scope first P.Scope.Developer_repl
      && P.Principal.has_scope latest P.Scope.Developer_repl
    then Ok ()
    else Error (denied "missing scope developer.repl"))
;;

module Original = struct
  type t =
    { route : Developer_command_original.Route.t
    ; target : Contract.Target.t
    ; prompt : P.Id.Prompt_definition.t
    ; workspace : P.Id.Workspace_definition.t
    ; requested_execution : Chatml_execution.limits option
    ; requested_max_output_bytes : int option
    ; fingerprint : Captured.Digest.t
    }

  let create
        ~route
        ~target
        ~prompt
        ~workspace
        ~requested_execution
        ~requested_max_output_bytes
        ~checkpoint
    =
    if
      (not (Original_identity.valid_route route))
      || (not
            (match target with
             | Contract.Target.One_off_v1 | Tool_v1 | Developer_tool_v1 -> true
             | Moderator_v1 | Delegated_moderator_v1 -> false))
      || (not (Option.for_all requested_execution ~f:Chatml_execution.valid_limits))
      || not (Option.for_all requested_max_output_bytes ~f:(fun value -> value >= 256))
    then Error (invalid "invalid original developer REPL request")
    else (
      let option f = function
        | None -> `Null
        | Some value -> f value
      in
      let fingerprint =
        `Object
          [ "version", `Number "1"
          ; "action", `String "developer_repl.create"
          ; "route", Original_identity.route_json route
          ; "target", `String (Original_identity.target_name target)
          ; "prompt", `String (P.Id.Prompt_definition.to_string prompt)
          ; "workspace", `String (P.Id.Workspace_definition.to_string workspace)
          ; ( "requested_execution"
            , option Original_identity.execution_json requested_execution )
          ; ( "requested_max_output_bytes"
            , option
                (fun value -> `Number (Int.to_string value))
                requested_max_output_bytes )
          ]
        |> Original_identity.digest_json ~checkpoint
      in
      Ok
        { route
        ; target
        ; prompt
        ; workspace
        ; requested_execution
        ; requested_max_output_bytes
        ; fingerprint
        })
  ;;

  let fingerprint t = t.fingerprint
  let route t = t.route
end

module Instance = struct
  type t = { token : unit ref }

  let equal left right = phys_equal left.token right.token
end

module Task_reference = struct
  type t =
    { encoded : string
    ; run_id : P.Id.Developer_run.t
    }

  let version = "developer_repl_task_v1"
  let max_bytes = 384
  let to_string t = t.encoded
  let run_id t = t.run_id
  let equal left right = String.equal left.encoded right.encoded

  let canonical_nonnegative text =
    String.length text > 0
    && (String.length text = 1 || not (Char.equal text.[0] '0'))
    && String.for_all text ~f:Char.is_digit
    && Option.value_map (Int.of_string_opt text) ~default:false ~f:(fun value ->
      value >= 0)
  ;;

  let of_string encoded =
    let invalid () = Error (invalid "invalid developer REPL Task reference") in
    if String.length encoded > max_bytes
    then invalid ()
    else (
      match String.split encoded ~on:'.' with
      | [ wire_version; run; instance_nonce; epoch; ordinal; source ]
        when String.equal wire_version version
             && canonical_nonnegative epoch
             && canonical_nonnegative ordinal
             && not (String.equal ordinal "0") ->
        (match
           ( P.Id.Developer_run.of_string run
           , P.Id.Transaction.of_string instance_nonce
           , Captured.Digest.of_hex source )
         with
         | Ok run_id, Ok _, Ok _ -> Ok { encoded; run_id }
         | _ -> invalid ())
      | _ -> invalid ())
  ;;

  let issue ~run_id ~instance_nonce ~epoch ~ordinal ~source =
    let encoded =
      String.concat
        ~sep:"."
        [ version
        ; P.Id.Developer_run.to_string run_id
        ; P.Id.Transaction.to_string instance_nonce
        ; Int.to_string epoch
        ; Int.to_string ordinal
        ; Captured.Digest.to_hex source
        ]
    in
    { encoded; run_id }
  ;;
end

module Epoch = struct
  type t =
    { token : unit ref
    ; serial : int
    }

  let equal left right = phys_equal left.token right.token
end

module Revision = struct
  type t =
    { token : unit ref
    ; serial : int
    }

  let equal left right = phys_equal left.token right.token
end

module Limits = struct
  type t =
    { max_instances : int
    ; max_keys_per_instance : int
    ; max_submissions_per_epoch : int
    ; max_source_bytes : int
    ; max_receipt_bytes : int
    ; max_total_receipt_bytes : int
    ; max_total_type_detail_bytes : int
    ; max_task_origins : int
    ; max_reference_page_bytes : int
    }

  let create
        ~max_instances
        ~max_keys_per_instance
        ~max_submissions_per_epoch
        ~max_source_bytes
        ~max_receipt_bytes
        ~max_total_receipt_bytes
        ~max_total_type_detail_bytes
        ~max_task_origins
        ~max_reference_page_bytes
    =
    let values =
      [ max_instances
      ; max_keys_per_instance
      ; max_submissions_per_epoch
      ; max_source_bytes
      ; max_receipt_bytes
      ; max_total_receipt_bytes
      ; max_total_type_detail_bytes
      ; max_task_origins
      ; max_reference_page_bytes
      ]
    in
    if
      List.for_all values ~f:(fun value -> value > 0 && value <= 1_048_576)
      && max_keys_per_instance >= 2
      && max_receipt_bytes >= 256
      && max_receipt_bytes <= max_total_receipt_bytes
      && max_task_origins <= max_submissions_per_epoch
      && max_reference_page_bytes >= Task_reference.max_bytes + 128
      && max_total_receipt_bytes
         >= (max_task_origins * (Task_reference.max_bytes + 64)) + max_receipt_bytes
    then
      Ok
        { max_instances
        ; max_keys_per_instance
        ; max_submissions_per_epoch
        ; max_source_bytes
        ; max_receipt_bytes
        ; max_total_receipt_bytes
        ; max_total_type_detail_bytes
        ; max_task_origins
        ; max_reference_page_bytes
        }
    else Or_error.error_string "invalid developer REPL limits"
  ;;
end

module Configuration = struct
  type t =
    { scope_limits : Scope.Limits.t
    ; root_limits : Chatml_dependency_session.Roots.Limits.t
    ; typing_limits : Typing.Limits.t
    ; evaluator_limits : Evaluator.Limits.t
    ; execution_policy : Chatml_execution.policy
    ; type_display_limits : Chatml_type_display.Limits.t
    ; limits : Limits.t
    }

  let create
        ~scope_limits
        ~root_limits
        ~typing_limits
        ~evaluator_limits
        ~execution_policy
        ~type_display_limits
        ~(limits : Limits.t)
    =
    if limits.max_task_origins < Evaluator.Limits.max_retained_tasks evaluator_limits
    then Or_error.error_string "REPL Task-origin cap is below evaluator retention cap"
    else
      Ok
        { scope_limits
        ; root_limits
        ; typing_limits
        ; evaluator_limits
        ; execution_policy
        ; type_display_limits
        ; limits
        }
  ;;
end

module View = struct
  type t =
    { instance : Instance.t
    ; epoch : Epoch.t
    ; revision : Revision.t
    ; run_id : P.Id.Developer_run.t
    ; capture_fingerprint : Captured.Digest.t
    }

  let instance t = t.instance
  let epoch t = t.epoch
  let revision t = t.revision
  let run_id t = t.run_id
  let capture_fingerprint t = t.capture_fingerprint
end

module Submission = struct
  module Statement_summary = Projection.Statement

  module Diagnostic = struct
    type t =
      { message : string
      ; location : Captured.Location.t option
      }

    let message t = t.message
    let location t = t.location
  end

  type status =
    | Need_more of Parse.Submission.Need_more.reason * Captured.Location.t option
    | Rejected of Diagnostic.t
    | Finished
    | Stopped of Diagnostic.t option

  type details =
    | Available
    | Output_limited

  type t =
    { status : status
    ; details : details
    ; completed_statements : int
    ; statement_count : int
    ; revision : Revision.t
    ; summaries : Statement_summary.t list
    ; presentation_bytes : int
    ; statements : Evaluator.Receipt.t list
    ; task_handles : Evaluator.Task_handle.t list
    ; task_reference_count : int
    ; task_references : Task_reference.t list
    }

  let status t = t.status
  let details t = t.details
  let completed_statements t = t.completed_statements
  let statement_count t = t.statement_count
  let revision t = t.revision
  let summaries t = t.summaries
  let statements t = t.statements
  let task_handles t = t.task_handles
  let task_reference_count t = t.task_reference_count
  let task_references t = t.task_references
end

module Type_selector = Projection.Selector

module Type_inspection = struct
  type t =
    { revision : Revision.t
    ; binding_name : string option
    ; rendered : Chatml_type_display.t
    }

  let revision t = t.revision
  let binding_name t = t.binding_name
  let rendered t = t.rendered
end

module Task_reference_page = struct
  type t =
    { items : Task_reference.t list
    ; next : Task_reference.t option
    ; total : int
    }

  let items t = t.items
  let next t = t.next
  let total t = t.total
end

open Submission

type task_unlaunched_disposition =
  | No_durable_row
  | Terminal_row

type task_phase =
  | Registered
  | Launching
  | Entered
  | Body_exited
  | Unlaunched of task_unlaunched_disposition
  | Settled
  | Uncertain

type task_origin =
  { source : Captured.t
  ; owner : instance_state
  ; handle : Evaluator.Task_handle.t
  ; reference : Task_reference.t
  ; issuing_key : P.Idempotency_key.t
  }

and key_entry =
  { key : P.Idempotency_key.t
  ; epoch : Epoch.t
  ; expected_revision : Revision.t
  ; fingerprint : Captured.Digest.t
  ; mutable outcome : (Submission.t, P.Error.t) result option
  ; mutable expired : bool
  ; mutable charged_bytes : int
  ; mutable reference_bytes : int
  ; mutable type_details : Evaluator.Receipt.t list option
  ; mutable type_detail_bytes : int
  ; mutable trace_entry : Developer_trace_retention.Entry.t option
  }

and active =
  { key : P.Idempotency_key.t
  ; kind : active_kind
  ; finished : unit Eio.Promise.t
  ; mutable cancel : Eio.Cancel.t option
  ; mutable cancel_requested : bool
  ; mutable private_failure : (exn * Stdlib.Printexc.raw_backtrace) option
  }

and active_kind =
  | Statement
  | Task of task_state

and task_state =
  { owner : instance_state
  ; actor : Operator_authorization.t
  ; epoch : Epoch.t
  ; domain : int
  ; checkpoint : unit -> unit
  ; mutable phase : task_phase
  ; mutable callback_cancel : Eio.Cancel.t option
  }

and instance_state =
  { id : Instance.t
  ; instance_nonce : P.Id.Transaction.t
  ; host_state : owner_state ref
  ; run_id : P.Id.Developer_run.t
  ; original : Original.t
  ; original_actor : Operator_authorization.t
  ; limits : Limits.t
  ; selection : Capture_host.Source_selection.t
  ; mutable typing : Typing.t
  ; mutable evaluator : Evaluator.t
  ; gate : Eio.Mutex.t
  ; mutable epoch : Epoch.t
  ; mutable revision : Revision.t
  ; mutable submission_count : int
  ; mutable pending_source : string
  ; mutable keys : key_entry list
  ; mutable task_keys : P.Idempotency_key.t list
  ; mutable reset_keys : (P.Idempotency_key.t * Epoch.t * View.t) list
  ; mutable close_keys : P.Idempotency_key.t list
  ; mutable task_origins : task_origin list
  ; mutable task_ordinal : int
  ; mutable retained_bytes : int
  ; mutable retained_type_detail_bytes : int
  ; mutable active : active option
  ; mutable closed : bool
  ; mutable borrowed : int
  ; mutable control : control option
  ; mutable trace_provenance : Developer_trace_retention.Provenance.t option
  }

and control_kind =
  | Reset_action
  | Close_action

and control_outcome =
  | Reset_done of (View.t, P.Error.t) result
  | Close_done of (unit, P.Error.t) result
  | Control_raised of exn * Stdlib.Printexc.raw_backtrace

and control =
  { kind : control_kind
  ; key : P.Idempotency_key.t
  ; custody : unit ref
  ; finished : unit Eio.Promise.t
  ; finish : unit Eio.Promise.u
  ; mutable outcome : control_outcome option
  ; mutable retry_cleanup : bool
  }

and owner_state =
  | Owner_open
  | Owner_closing
  | Owner_closed

type task_cleanup =
  { authorize_retirement :
      actor:Operator_authorization.t -> instance:Instance.t -> (unit, P.Error.t) result
  ; retire_instance :
      actor:Operator_authorization.t -> Instance.t -> (unit, P.Error.t) result
  ; retire_all : unit -> (unit, P.Error.t) result
  }

type t =
  { sw : Eio.Switch.t
  ; capture_host : Capture_host.t
  ; env : Eio_unix.Stdenv.base
  ; clock : Eio.Time.Mono.ty Eio.Time.Mono.t
  ; configuration : Configuration.t
  ; checkpoint : unit -> unit
  ; domain : int
  ; gate : Eio.Mutex.t
  ; mutable instances : instance_state list
  ; mutable create_keys :
      (P.Id.Developer_run.t * P.Idempotency_key.t * Captured.Digest.t * View.t) list
  ; owner_state : owner_state ref
  ; mutable task_cleanup : task_cleanup option
  ; traces : Developer_trace_retention.t option
  }

module Task_origin = struct
  type t = task_origin

  let source t = t.source
  let reference t = t.reference
  let trace_provenance t = t.owner.trace_provenance

  let check_current t ~actor ~checkpoint =
    let open Result.Let_syntax in
    if
      t.owner.closed
      || (match !(t.owner.host_state) with
          | Owner_open -> false
          | Owner_closing | Owner_closed -> true)
      || not (Evaluator.current_task_handle t.owner.evaluator t.handle)
    then Error (stale "developer REPL task handle expired")
    else (
      let%bind () = check_method ~original:t.owner.original_actor ~current:actor in
      let%bind checked =
        Capture_host.Source_selection.Shared.check_current
          t.owner.selection
          ~actor
          ~checkpoint
      in
      let%bind () =
        Capture_host.Source_selection.Shared.check_now t.owner.selection ~checked ~actor
      in
      if
        t.owner.closed
        || (match !(t.owner.host_state) with
            | Owner_open -> false
            | Owner_closing | Owner_closed -> true)
        || not (Evaluator.current_task_handle t.owner.evaluator t.handle)
      then Error (stale "developer REPL task handle expired")
      else check_method ~original:t.owner.original_actor ~current:actor)
  ;;
end

let on_domain t = Int.equal t.domain (Stdlib.Domain.self () :> int)

let check_host t =
  if not (on_domain t)
  then Error (stale "developer REPL owner used from another domain")
  else if
    match !(t.owner_state) with
    | Owner_open -> false
    | Owner_closing | Owner_closed -> true
  then
    Error
      (P.Error.create
         P.Error.Invalid_state
         ~message:"developer REPL closed"
         ~retryable:false
         ())
  else Ok ()
;;

let find_instance t id =
  List.find t.instances ~f:(fun state -> Instance.equal state.id id)
;;

let check_instance t (state : instance_state) ~actor =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  if state.closed
  then Error (stale "developer REPL instance closed")
  else check_method ~original:state.original_actor ~current:actor
;;

let selected_target selected =
  match Contract.kind (Capture_host.Selected_contract.contract selected) with
  | Contract.Kind.Standard target -> Some target
  | Native -> None
;;

let narrowed_execution policy requested =
  match policy, requested with
  | Chatml_execution.Unrestricted, None -> Chatml_execution.Unrestricted
  | Unrestricted, Some requested -> Bounded requested
  | Bounded host, None -> Bounded host
  | Bounded host, Some requested ->
    Bounded
      { Chatml_execution.fuel = Int.min host.fuel requested.fuel
      ; max_tasks = Int.min host.max_tasks requested.max_tasks
      ; wall_seconds = Float.min host.wall_seconds requested.wall_seconds
      ; max_value_bytes = Int.min host.max_value_bytes requested.max_value_bytes
      ; max_array_items = Int.min host.max_array_items requested.max_array_items
      ; max_depth = Int.min host.max_depth requested.max_depth
      ; allocation_bytes = Int.min host.allocation_bytes requested.allocation_bytes
      ; max_calls = Int.min host.max_calls requested.max_calls
      ; max_invocation_depth =
          Int.min host.max_invocation_depth requested.max_invocation_depth
      }
;;

let install ~sw ~capture_host ~traces ~env ~clock ~configuration ~checkpoint =
  { sw
  ; capture_host
  ; env
  ; clock
  ; configuration
  ; checkpoint
  ; domain = (Stdlib.Domain.self () :> int)
  ; gate = Eio.Mutex.create ()
  ; instances = []
  ; create_keys = []
  ; owner_state = ref Owner_open
  ; task_cleanup = None
  ; traces
  }
;;

let next_revision state =
  let revision =
    { Revision.token = ref (); serial = state.revision.Revision.serial + 1 }
  in
  state.revision <- revision;
  revision
;;

let view state =
  { View.instance = state.id
  ; epoch = state.epoch
  ; revision = state.revision
  ; run_id = state.run_id
  ; capture_fingerprint =
      Capture_host.Source_selection.repl_root_fingerprint state.selection
  }
;;

let with_gate gate f =
  Eio.Mutex.lock gate;
  Exn.protect ~f ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Mutex.unlock gate))
;;

let check_state_open state =
  if state.closed
  then Error (stale "developer REPL instance closed")
  else (
    match !(state.host_state) with
    | Owner_open -> Ok ()
    | Owner_closing | Owner_closed -> Error (stale "developer REPL owner is closing"))
;;

let check_selection state ~actor ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = check_state_open state in
  let%bind () = check_method ~original:state.original_actor ~current:actor in
  let%bind checked =
    Capture_host.Source_selection.Shared.check_current state.selection ~actor ~checkpoint
  in
  let%bind () = check_state_open state in
  let%bind () = check_method ~original:state.original_actor ~current:actor in
  Capture_host.Source_selection.Shared.check_now state.selection ~checked ~actor
;;

let surface_of_target = function
  | Contract.Target.One_off_v1 -> Chatml.Chatml_extension_surface.one_off_v1
  | Tool_v1 -> Chatml.Chatml_extension_surface.tool_v1
  | Developer_tool_v1 -> Chatml.Chatml_extension_surface.developer_tool_v1
  | Moderator_v1 | Delegated_moderator_v1 -> assert false
;;

let make_epoch_resources t ~selection ~(original : Original.t) =
  let open Result.Let_syntax in
  let prepared = Capture_host.Source_selection.prepared selection in
  let selected = Capture_host.Source_selection.selected_contract selection in
  let%bind () =
    match selected_target selected with
    | Some target when Contract.Target.equal target original.target -> Ok ()
    | Some _ | None -> Error (stale "developer REPL selected ABI changed")
  in
  let anchor = Prepared.entry prepared in
  let%bind scope =
    Scope.create
      prepared
      ~anchor_owner:(Prepared.Module_id.of_implementation anchor)
      ~anchor_implementation:anchor
      ~limits:t.configuration.scope_limits
      ~checkpoint:t.checkpoint
    |> Result.map_error ~f:(fun _ -> invalid "invalid developer REPL source scope")
  in
  let%bind typing =
    Typing.create
      ~prepared
      ~scope
      ~contract:(Capture_host.Selected_contract.contract selected)
      ~root_limits:t.configuration.root_limits
      ~limits:t.configuration.typing_limits
      ~checkpoint:t.checkpoint
    |> Result.map_error ~f:(fun _ -> invalid "developer REPL typing setup rejected")
  in
  let surface = surface_of_target original.target in
  let%map evaluator =
    Evaluator.create
      ~typing
      ~surface
      ~env:t.env
      ~policy:
        (narrowed_execution t.configuration.execution_policy original.requested_execution)
      ~limits:t.configuration.evaluator_limits
      ~checkpoint:t.checkpoint
    |> Result.map_error ~f:(fun _ -> invalid "developer REPL evaluator setup rejected")
  in
  typing, evaluator
;;

let discard_unpublished_evaluator evaluator =
  Eio.Cancel.protect (fun () ->
    match Evaluator.close evaluator with
    | Ok () -> ()
    | Error _ -> failwith "unpublished developer REPL evaluator could not close")
;;

let initial_trace_provenance t ~selection ~original ~actor =
  Option.bind t.traces ~f:(fun traces ->
    Developer_trace_retention.Provenance.of_prepared
      ~action:Repl_statement
      ~original_actor:actor
      ~route:(Original.route original)
      ~capture_fingerprint:None
      ~selection_fingerprint:
        (Capture_host.Source_selection.repl_root_fingerprint selection)
      ~prepared:(Capture_host.Source_selection.prepared selection)
      ~limits:(Developer_trace_retention.limits traces)
    |> Result.ok)
;;

let create_state t ~run_id ~original ~fresh_source ~actor =
  let open Result.Let_syntax in
  let%bind selection =
    Source.prepare_selection
      t.capture_host
      ~route:original.Original.route
      ~fresh:fresh_source
      ~actor
      ~clock:t.clock
      ~checkpoint:t.checkpoint
  in
  let%bind typing, evaluator = make_epoch_resources t ~selection ~original in
  let admission =
    try
      let%bind checked =
        Capture_host.Source_selection.Shared.check_current
          selection
          ~actor
          ~checkpoint:t.checkpoint
      in
      let%bind () = check_method ~original:actor ~current:actor in
      let%bind () =
        Capture_host.Source_selection.Shared.check_now selection ~checked ~actor
      in
      check_host t
    with
    | exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      reraise_after_cleanup exn backtrace ~cleanup:(fun () ->
        discard_unpublished_evaluator evaluator)
  in
  let%bind () =
    match admission with
    | Ok () -> Ok ()
    | Error _ as error ->
      discard_unpublished_evaluator evaluator;
      error
  in
  let id = { Instance.token = ref () } in
  let epoch = { Epoch.token = ref (); serial = 0 } in
  let revision = { Revision.token = ref (); serial = 0 } in
  let trace_provenance = initial_trace_provenance t ~selection ~original ~actor in
  Ok
    { id
    ; instance_nonce = P.Id.Transaction.create ()
    ; host_state = t.owner_state
    ; run_id
    ; original
    ; original_actor = actor
    ; limits = t.configuration.limits
    ; selection
    ; typing
    ; evaluator
    ; gate = Eio.Mutex.create ()
    ; epoch
    ; revision
    ; submission_count = 0
    ; pending_source = ""
    ; keys = []
    ; task_keys = []
    ; reset_keys = []
    ; close_keys = []
    ; task_origins = []
    ; task_ordinal = 0
    ; retained_bytes = 0
    ; retained_type_detail_bytes = 0
    ; active = None
    ; closed = false
    ; borrowed = 0
    ; control = None
    ; trace_provenance
    }
;;

let create t ~actor ~run_id ~key ~original ~fresh_source =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  let%bind () = check_method ~original:actor ~current:actor in
  with_gate t.gate (fun () ->
    let prior =
      List.find t.create_keys ~f:(fun (run, prior_key, _, _) ->
        P.Id.Developer_run.equal run run_id && P.Idempotency_key.equal prior_key key)
    in
    match prior with
    | Some (_, _, prior_fingerprint, prior_view) ->
      if Captured.Digest.equal prior_fingerprint (Original.fingerprint original)
      then (
        let%bind state =
          find_instance t (View.instance prior_view)
          |> Result.of_option ~error:(stale "developer REPL instance expired")
        in
        let%bind () = check_instance t state ~actor in
        let%map () = check_selection state ~actor ~checkpoint:t.checkpoint in
        prior_view)
      else Error (busy "developer REPL create key changed payload")
    | None ->
      if List.length t.instances >= t.configuration.limits.max_instances
      then Error (limit "developer REPL instance limit reached")
      else (
        match fresh_source with
        | None -> Error (invalid "fresh developer REPL source required")
        | Some fresh_source ->
          let%bind state = create_state t ~run_id ~original ~fresh_source ~actor in
          let created = view state in
          t.instances <- state :: t.instances;
          t.create_keys
          <- (run_id, key, Original.fingerprint original, created) :: t.create_keys;
          Ok created))
;;

let bounded_text max_bytes text =
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

let diagnostic message location =
  { Submission.Diagnostic.message = bounded_text 1024 message; location }
;;

let diagnostic_of_owned (owned : Chatml_diagnostic.t) =
  diagnostic owned.message (Option.map owned.site ~f:Chatml_diagnostic.Site.location)
;;

let rejected_diagnostic rejection =
  match Parse.Submission.Rejection.reason rejection with
  | Parse.Submission.Rejection.Syntax owned -> diagnostic_of_owned owned
  | Source_byte_limit -> diagnostic "REPL source byte limit reached" None
  | Token_limit -> diagnostic "REPL token limit reached" None
  | Work_limit -> diagnostic "REPL parse work limit reached" None
  | Sites _ -> diagnostic "REPL syntax site limit reached" None
;;

let typing_diagnostic = function
  | Typing.Error.Type_diagnostic owned -> diagnostic_of_owned owned
  | _ -> diagnostic "REPL statement typecheck rejected" None
;;

let evaluator_diagnostic = function
  | Evaluator.Error.Lowering owned -> diagnostic_of_owned owned
  | _ -> diagnostic "REPL statement execution failed" None
;;

let action_fingerprint state ~action ~epoch ~expected_revision ~source ~checkpoint =
  Original_identity.digest_json
    (`Object
        [ "version", `Number "1"
        ; "action", `String action
        ; "run_id", `String (P.Id.Developer_run.to_string state.run_id)
        ; ( "original"
          , `String (Captured.Digest.to_hex (Original.fingerprint state.original)) )
        ; "epoch", `Number (Int.to_string epoch.Epoch.serial)
        ; "revision", `Number (Int.to_string expected_revision.Revision.serial)
        ; "source", `String source
        ])
    ~checkpoint
;;

let make_source (state : instance_state) text ~checkpoint =
  let open Result.Let_syntax in
  let path =
    "repl/epoch-"
    ^ Int.to_string state.epoch.Epoch.serial
    ^ "/submission-"
    ^ Int.to_string (state.submission_count + 1)
    ^ ".chatml"
  in
  let%bind id =
    Captured.Id.create
      ~package_namespace:("developer-repl-" ^ P.Id.Developer_run.to_string state.run_id)
      ~relative_path:path
    |> Result.map_error ~f:(fun _ -> invalid "invalid REPL snippet identity")
  in
  Captured.create
    id
    ~contents:text
    ~expected_digest:None
    ~max_bytes:(Some state.limits.max_source_bytes)
    ~checkpoint
  |> Result.map_error ~f:(fun _ -> limit "developer REPL source limit reached")
;;

exception Admission_revoked of P.Error.t
exception Cancelled_statement
exception Reference_limit

let raise_if_error = function
  | Ok () -> ()
  | Error error -> raise (Admission_revoked error)
;;

let guarded_checkpoint t state ~actor ~checked () =
  t.checkpoint ();
  raise_if_error (check_host t);
  raise_if_error (check_method ~original:state.original_actor ~current:actor);
  raise_if_error
    (Capture_host.Source_selection.Shared.check_now state.selection ~checked ~actor)
;;

let status_bytes = function
  | Need_more (_, location) ->
    64 + Option.value_map location ~default:0 ~f:Projection.location_charge
  | Finished | Stopped None -> 64
  | Rejected diagnostic | Stopped (Some diagnostic) ->
    64
    + String.length (Submission.Diagnostic.message diagnostic)
    + Option.value_map
        (Submission.Diagnostic.location diagnostic)
        ~default:0
        ~f:Projection.location_charge
;;

let compact_bytes submission =
  status_bytes submission.Submission.status + submission.presentation_bytes
;;

let type_detail_bytes receipts =
  List.sum
    (module Int)
    receipts
    ~f:(fun receipt ->
      let frozen = Evaluator.Receipt.frozen_types receipt in
      let snapshot = Typing.Frozen_types.snapshot frozen in
      192
      + (Chatml_typechecker.Type_snapshot.node_count snapshot * 256)
      + (Chatml_typechecker.Type_snapshot.edge_count snapshot * 48)
      + (Chatml_typechecker.Type_snapshot.label_bytes snapshot * 2)
      + List.sum
          (module Int)
          (Typing.Frozen_types.bindings frozen)
          ~f:(fun (name, _) -> 64 + String.length name)
      + Option.value_map
          (Evaluator.Receipt.value_preview receipt)
          ~default:0
          ~f:String.length
      + Option.value_map
          (Evaluator.Receipt.diagnostic receipt)
          ~default:0
          ~f:(fun diagnostic ->
            String.length (Evaluator.Diagnostic.message diagnostic)
            + Option.value_map
                (Evaluator.Diagnostic.location diagnostic)
                ~default:0
                ~f:Projection.location_charge))
;;

let reference_charge reference = 64 + String.length (Task_reference.to_string reference)

let presentation_ceiling state =
  Option.value_map
    state.original.Original.requested_max_output_bytes
    ~default:state.limits.max_receipt_bytes
    ~f:(Int.min state.limits.max_receipt_bytes)
;;

let first_reference_page state references =
  let rec loop remaining acc = function
    | [] -> List.rev acc
    | reference :: rest ->
      let charged = reference_charge reference in
      if charged > remaining
      then List.rev acc
      else loop (remaining - charged) (reference :: acc) rest
  in
  loop (state.limits.max_reference_page_bytes - 64) [] references
;;

let bound_status ~ceiling status =
  if status_bytes status <= ceiling
  then status, false
  else (
    match status with
    | Rejected _ ->
      Rejected (diagnostic "REPL result diagnostic exceeded output limit" None), true
    | Stopped _ -> Stopped None, true
    | Need_more (reason, Some _) -> Need_more (reason, None), true
    | Need_more (_, None) | Finished -> status, true)
;;

let%test_unit "minimal receipt preserves incomplete input with a bounded location" =
  let long_run =
    match P.Id.Developer_run.of_string ("dvr_" ^ String.make 92 'a') with
    | Ok id -> id
    | Error _ -> failwith "maximum-length developer run ID was rejected"
  in
  let source_id =
    Captured.Id.create
      ~package_namespace:("developer-repl-" ^ P.Id.Developer_run.to_string long_run)
      ~relative_path:"snippet-1.chatml"
    |> Or_error.ok_exn
  in
  let source =
    Captured.create
      source_id
      ~contents:"let x ="
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:(fun () -> ())
    |> Or_error.ok_exn
  in
  let location =
    Captured.locate source ~start_byte:7 ~end_byte:7 ~checkpoint:(fun () -> ())
    |> Or_error.ok_exn
  in
  let original = Submission.Need_more (Parser_eof, Some location) in
  assert (status_bytes original > 256);
  (match bound_status ~ceiling:256 original with
   | Need_more (Parser_eof, None), true -> ()
   | _ -> failwith "bounded incomplete input lost its truthful limited status");
  assert (status_bytes (fst (bound_status ~ceiling:256 original)) <= 256);
  match bound_status ~ceiling:512 original with
  | Need_more (Parser_eof, Some retained), false ->
    assert (Captured.Id.equal (Captured.Location.source retained) source_id)
  | _ -> failwith "location was removed despite fitting the receipt"
;;

let compact_submission
      t
      state
      ~status
      ~revision
      ~completed_statements
      ~statements
      ~handles
      ~references
  =
  let ceiling = presentation_ceiling state in
  let status, status_limited = bound_status ~ceiling status in
  let presentation =
    try
      Projection.present
        ~display_limits:t.configuration.type_display_limits
        ~max_bytes:(Int.max 0 (ceiling - status_bytes status))
        statements
        ~checkpoint:t.checkpoint
    with
    | exn ->
      (match exn with
       | Cancelled_statement | Eio.Cancel.Cancelled Cancelled_statement -> ()
       | _ ->
         Option.iter state.active ~f:(fun active ->
           active.private_failure <- Some (exn, Stdlib.Printexc.get_raw_backtrace ())));
      Error Projection.Error.Invalid_selection
  in
  let summaries, presentation_limited, presentation_bytes =
    match presentation with
    | Ok presentation ->
      ( Projection.Presentation.statements presentation
      , not (Projection.Presentation.complete presentation)
      , Projection.Presentation.charged_bytes presentation )
    | Error _ -> [], true, 0
  in
  { Submission.status
  ; details =
      (if status_limited || presentation_limited then Output_limited else Available)
  ; completed_statements
  ; statement_count = List.length statements
  ; revision
  ; summaries
  ; presentation_bytes
  ; statements
  ; task_handles = handles
  ; task_reference_count = List.length references
  ; task_references = first_reference_page state references
  }
;;

let evict_old_compact state =
  let rec evict = function
    | [] -> ()
    | entry :: rest when state.retained_bytes > state.limits.max_total_receipt_bytes ->
      if entry.charged_bytes > 0
      then (
        state.retained_bytes <- state.retained_bytes - entry.charged_bytes;
        state.retained_type_detail_bytes
        <- state.retained_type_detail_bytes - entry.type_detail_bytes;
        entry.charged_bytes <- 0;
        entry.type_detail_bytes <- 0;
        entry.type_details <- None;
        entry.expired <- true;
        entry.outcome <- None);
      evict rest
    | _ -> ()
  in
  evict (List.rev state.keys)
;;

let evict_old_type_details state =
  let rec evict = function
    | [] -> ()
    | entry :: rest
      when state.retained_type_detail_bytes > state.limits.max_total_type_detail_bytes ->
      state.retained_type_detail_bytes
      <- state.retained_type_detail_bytes - entry.type_detail_bytes;
      entry.type_detail_bytes <- 0;
      entry.type_details <- None;
      evict rest
    | _ -> ()
  in
  evict (List.rev state.keys)
;;

let remember_outcome state entry outcome =
  match outcome with
  | Error _ ->
    entry.outcome <- Some outcome;
    evict_old_compact state
  | Ok submission ->
    let receipts = submission.statements in
    let detail_bytes = type_detail_bytes receipts in
    entry.type_details <- Some receipts;
    entry.type_detail_bytes <- detail_bytes;
    state.retained_type_detail_bytes <- state.retained_type_detail_bytes + detail_bytes;
    entry.outcome <- Some (Ok { submission with statements = [] });
    entry.charged_bytes <- compact_bytes submission;
    state.retained_bytes <- state.retained_bytes + entry.charged_bytes;
    evict_old_type_details state;
    evict_old_compact state
;;

let discard_if_current state candidate =
  Eio.Cancel.protect (fun () ->
    match Typing.validate_candidate state.typing candidate with
    | Error _ -> ()
    | Ok () ->
      (match Typing.discard_candidate state.typing candidate with
       | Ok () -> ()
       | Error _ -> failwith "current developer REPL candidate could not discard"))
;;

let task_source_digest source ~checkpoint =
  let id = Captured.id source in
  [%sexp
    "developer-repl-task-source.v1"
  , (Captured.Id.package_namespace id : string)
  , (Captured.Id.relative_path id : string)
  , (Captured.Digest.to_hex (Captured.digest source) : string)]
  |> Sexp.to_string_mach
  |> Captured.Digest.of_contents ~checkpoint
;;

let execute_ready t state ~actor ~key ready ~checked =
  let guard = guarded_checkpoint t state ~actor ~checked in
  let rec loop remaining receipts origins =
    if remaining = 0
    then Finished, List.rev receipts, List.rev origins
    else (
      try
        guard ();
        match Typing.prepare_next state.typing ready ~checkpoint:guard with
        | Error error ->
          Stopped (Some (typing_diagnostic error)), List.rev receipts, List.rev origins
        | Ok candidate ->
          let candidate_reference =
            try
              guard ();
              let source = Typing.Candidate.source candidate in
              let reference =
                Task_reference.issue
                  ~run_id:state.run_id
                  ~instance_nonce:state.instance_nonce
                  ~epoch:state.epoch.Epoch.serial
                  ~ordinal:(state.task_ordinal + 1)
                  ~source:(task_source_digest source ~checkpoint:guard)
              in
              if
                String.length (Task_reference.to_string reference)
                > Task_reference.max_bytes
              then raise Reference_limit;
              reference
            with
            | exn ->
              let backtrace = Stdlib.Printexc.get_raw_backtrace () in
              reraise_after_cleanup exn backtrace ~cleanup:(fun () ->
                discard_if_current state candidate)
          in
          (match Evaluator.execute_next state.evaluator candidate ~checkpoint:guard with
           | Error error ->
             discard_if_current state candidate;
             ( Stopped (Some (evaluator_diagnostic error))
             , List.rev receipts
             , List.rev origins )
           | Ok receipt ->
             (match Evaluator.Receipt.outcome receipt with
              | Failed _ ->
                let diagnostic =
                  Option.map (Evaluator.Receipt.diagnostic receipt) ~f:(fun failure ->
                    diagnostic
                      (Evaluator.Diagnostic.message failure)
                      (Evaluator.Diagnostic.location failure))
                in
                Stopped diagnostic, List.rev (receipt :: receipts), List.rev origins
              | Succeeded ->
                let origins =
                  match Evaluator.Receipt.task_retention receipt with
                  | Evaluator.Receipt.Retained handle ->
                    let source = Typing.Candidate.source candidate in
                    state.task_ordinal <- state.task_ordinal + 1;
                    { source
                    ; owner = state
                    ; handle
                    ; reference = candidate_reference
                    ; issuing_key = key
                    }
                    :: origins
                  | Not_a_task | Limit_reached -> origins
                in
                loop (remaining - 1) (receipt :: receipts) origins))
      with
      | exn ->
        let message, unexpected =
          match exn with
          | Cancelled_statement | Eio.Cancel.Cancelled Cancelled_statement ->
            "REPL statement cancelled", false
          | Admission_revoked _ -> "REPL source or actor admission expired", false
          | Reference_limit -> "REPL Task reference limit reached", false
          | _ -> "REPL statement interrupted", true
        in
        if unexpected
        then
          Option.iter state.active ~f:(fun active ->
            active.private_failure <- Some (exn, Stdlib.Printexc.get_raw_backtrace ()));
        Stopped (Some (diagnostic message None)), List.rev receipts, List.rev origins)
  in
  loop (Typing.Submission.statement_count ready) [] []
;;

let begin_statement_trace t state entry captured =
  match t.traces, state.trace_provenance with
  | None, _ | _, None -> None
  | Some traces, Some provenance ->
    (match
       Developer_trace_retention.Provenance.add_captured_source
         provenance
         captured
         ~limits:(Developer_trace_retention.limits traces)
     with
     | Error _ ->
       state.trace_provenance <- None;
       None
     | Ok provenance ->
       state.trace_provenance <- Some provenance;
       let selection_fingerprint =
         Capture_host.Source_selection.repl_root_fingerprint state.selection
       in
       (match
          Developer_trace_retention.Association.repl_statement
            ~run_id:state.run_id
            ~instance_nonce:state.instance_nonce
            ~epoch_serial:entry.epoch.serial
            ~key:entry.key
            ~selection_fingerprint
        with
        | Error _ -> None
        | Ok association ->
          let trace_entry =
            Developer_trace_retention.begin_submitted
              traces
              ~association
              ~provenance
              ~submission_fingerprint:entry.fingerprint
              ~source:captured
              ~checkpoint:Eio.Fiber.check
          in
          entry.trace_entry <- trace_entry;
          trace_entry))
;;

let within_statement_trace t entry f =
  match t.traces with
  | None -> f ()
  | Some traces -> Developer_trace_retention.within traces entry.trace_entry f
;;

let process_submission t state ~actor ~entry source =
  let open Result.Let_syntax in
  let previous_bytes = String.length state.pending_source in
  if String.length source > state.limits.max_source_bytes - previous_bytes
  then Error (limit "developer REPL source limit reached")
  else (
    let text = state.pending_source ^ source in
    let%bind checked =
      Capture_host.Source_selection.Shared.check_current
        state.selection
        ~actor
        ~checkpoint:t.checkpoint
    in
    let guard = guarded_checkpoint t state ~actor ~checked in
    guard ();
    let%bind captured = make_source state text ~checkpoint:guard in
    state.submission_count <- state.submission_count + 1;
    let parsed = Typing.admit state.typing captured ~checkpoint:guard in
    let%bind parsed =
      Result.map_error parsed ~f:(fun _ -> invalid "developer REPL parse rejected")
    in
    let revision = next_revision state in
    match parsed with
    | Typing.Submission.Need_more need_more ->
      state.pending_source <- text;
      Ok
        (compact_submission
           t
           state
           ~status:
             (Need_more
                ( Parse.Submission.Need_more.reason need_more
                , Some (Parse.Submission.Need_more.at need_more) ))
           ~revision
           ~completed_statements:0
           ~statements:[]
           ~handles:[]
           ~references:[])
    | Typing.Submission.Rejected rejection ->
      state.pending_source <- "";
      ignore
        (begin_statement_trace t state entry captured
         : Developer_trace_retention.Entry.t option);
      Ok
        (compact_submission
           t
           state
           ~status:(Rejected (rejected_diagnostic rejection))
           ~revision
           ~completed_statements:0
           ~statements:[]
           ~handles:[]
           ~references:[])
    | Typing.Submission.Ready ready ->
      state.pending_source <- "";
      ignore
        (begin_statement_trace t state entry captured
         : Developer_trace_retention.Entry.t option);
      let status, statements, origins =
        within_statement_trace t entry (fun () ->
          execute_ready t state ~actor ~key:entry.key ready ~checked)
      in
      let completed_statements =
        List.count statements ~f:(fun receipt ->
          match Evaluator.Receipt.outcome receipt with
          | Succeeded -> true
          | Failed _ -> false)
      in
      let handles = List.map origins ~f:(fun origin -> origin.handle) in
      let references = List.map origins ~f:(fun origin -> origin.reference) in
      (* Configuration admits every handle the evaluator can retain. This
         nonyielding publication precedes optional presentation work. *)
      state.task_origins <- origins @ state.task_origins;
      entry.reference_bytes <- List.sum (module Int) references ~f:reference_charge;
      state.retained_bytes <- state.retained_bytes + entry.reference_bytes;
      Ok
        (compact_submission
           t
           state
           ~status
           ~revision
           ~completed_statements
           ~statements
           ~handles
           ~references))
;;

let key_result entry =
  if entry.expired
  then Error (stale "developer REPL result detail expired")
  else (
    match entry.outcome with
    | Some (Ok submission) ->
      Ok
        (match entry.type_details with
         | Some statements -> { submission with statements }
         | None -> { submission with details = Output_limited })
    | Some (Error error) -> Error error
    | None -> Error (busy "developer REPL submission is still active"))
;;

let request_cancel active =
  active.cancel_requested <- true;
  Option.iter active.cancel ~f:(fun cancel ->
    Eio.Cancel.cancel cancel Cancelled_statement);
  match active.kind with
  | Statement -> ()
  | Task task ->
    Option.iter task.callback_cancel ~f:(fun cancel ->
      Eio.Cancel.cancel cancel Cancelled_statement)
;;

let launch_submission t state entry ~actor ~source =
  let finished, finish = Eio.Promise.create () in
  let active =
    { key = entry.key
    ; kind = Statement
    ; finished
    ; cancel = None
    ; cancel_requested = false
    ; private_failure = None
    }
  in
  state.active <- Some active;
  let worker () =
    let outcome =
      try
        Eio.Cancel.sub (fun cancel ->
          active.cancel <- Some cancel;
          if active.cancel_requested then Eio.Cancel.cancel cancel Cancelled_statement;
          with_gate state.gate (fun () -> process_submission t state ~actor ~entry source))
      with
      | exn ->
        (match exn with
         | Cancelled_statement | Eio.Cancel.Cancelled Cancelled_statement ->
           Ok
             { Submission.status =
                 Stopped (Some (diagnostic "REPL statement cancelled" None))
             ; details = Available
             ; completed_statements = 0
             ; statement_count = 0
             ; revision = state.revision
             ; summaries = []
             ; presentation_bytes = 0
             ; statements = []
             ; task_handles = []
             ; task_reference_count = 0
             ; task_references = []
             }
         | Admission_revoked error -> Error error
         | _ ->
           active.private_failure <- Some (exn, Stdlib.Printexc.get_raw_backtrace ());
           Error
             (P.Error.create
                P.Error.Invalid_state
                ~message:"developer REPL callback failed"
                ~retryable:false
                ()))
    in
    Eio.Cancel.protect (fun () ->
      active.cancel <- None;
      state.active <- None;
      remember_outcome state entry outcome;
      (try
         Option.iter t.traces ~f:(fun traces ->
           Option.iter entry.trace_entry ~f:(fun trace_entry ->
             let disposition =
               if active.cancel_requested
               then Chatml_trace.Terminal.Disposition.Cancelled
               else (
                 match outcome with
                 | Ok { Submission.status = Finished; _ } -> Succeeded
                 | Ok { status = Rejected _; _ } -> Failed Parse
                 | Ok { status = Stopped _; _ } -> Failed Runtime
                 | Ok { status = Need_more _; _ } | Error _ -> Interrupted)
             in
             match Chatml_trace.Terminal.create ~receipt_version:1 ~disposition with
             | Error _ -> ()
             | Ok terminal -> Developer_trace_retention.finish traces trace_entry terminal))
       with
       | _ -> ());
      Eio.Promise.resolve finish ())
  in
  match Eio.Fiber.fork ~sw:t.sw worker with
  | () -> active
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    active.private_failure <- Some (exn, backtrace);
    state.active <- None;
    remember_outcome
      state
      entry
      (Error
         (P.Error.create
            P.Error.Invalid_state
            ~message:"developer REPL callback could not start"
            ~retryable:false
            ()));
    Eio.Promise.resolve finish ();
    Stdlib.Printexc.raise_with_backtrace exn backtrace
;;

let submit t ~actor ~instance ~epoch ~expected_revision ~key ~source =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  let%bind state =
    find_instance t instance
    |> Result.of_option ~error:(stale "developer REPL instance not found")
  in
  let%bind () = check_instance t state ~actor in
  if String.length source > state.limits.max_source_bytes
  then Error (limit "developer REPL source limit reached")
  else (
    let fingerprint =
      action_fingerprint
        state
        ~action:"submit"
        ~epoch
        ~expected_revision
        ~source
        ~checkpoint:t.checkpoint
    in
    let replay entry =
      let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
      if
        (not (Epoch.equal epoch entry.epoch))
        || (not (Revision.equal expected_revision entry.expected_revision))
        || not (Captured.Digest.equal fingerprint entry.fingerprint)
      then Error (busy "developer REPL key changed payload")
      else (
        (match state.active with
         | Some active when P.Idempotency_key.equal active.key key ->
           Eio.Promise.await active.finished
         | Some _ | None -> ());
        let%bind () = check_instance t state ~actor in
        let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
        key_result entry)
    in
    match
      List.find state.keys ~f:(fun entry -> P.Idempotency_key.equal entry.key key)
    with
    | Some entry -> replay entry
    | None ->
      if not (Epoch.equal state.epoch epoch)
      then Error (stale "developer REPL epoch changed")
      else if not (Revision.equal state.revision expected_revision)
      then Error (busy "developer REPL revision changed")
      else if
        Option.is_some state.active || state.borrowed > 0 || Option.is_some state.control
      then Error (busy "developer REPL statement is active")
      else if
        List.length state.keys
        + List.length state.task_keys
        + List.length state.reset_keys
        + List.length state.close_keys
        >= state.limits.max_keys_per_instance - 1
      then Error (limit "developer REPL key ledger full")
      else if
        List.exists state.reset_keys ~f:(fun (prior, _, _) ->
          P.Idempotency_key.equal prior key)
        || List.exists state.task_keys ~f:(P.Idempotency_key.equal key)
        || List.exists state.close_keys ~f:(P.Idempotency_key.equal key)
      then Error (busy "developer REPL key used for another action")
      else if state.submission_count >= state.limits.max_submissions_per_epoch
      then Error (limit "developer REPL submission limit reached")
      else (
        let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
        let%bind () = check_host t in
        match
          List.find state.keys ~f:(fun entry -> P.Idempotency_key.equal entry.key key)
        with
        | Some entry -> replay entry
        | None
          when (not (Epoch.equal state.epoch epoch))
               || (not (Revision.equal state.revision expected_revision))
               || Option.is_some state.active
               || state.borrowed > 0
               || Option.is_some state.control
               || state.closed
               || List.length state.keys
                  + List.length state.task_keys
                  + List.length state.reset_keys
                  + List.length state.close_keys
                  >= state.limits.max_keys_per_instance - 1 ->
          Error (busy "developer REPL admission changed")
        | None
          when List.exists state.reset_keys ~f:(fun (prior, _, _) ->
                 P.Idempotency_key.equal prior key)
               || List.exists state.task_keys ~f:(P.Idempotency_key.equal key)
               || List.exists state.close_keys ~f:(P.Idempotency_key.equal key) ->
          Error (busy "developer REPL key used for another action")
        | None when state.submission_count >= state.limits.max_submissions_per_epoch ->
          Error (limit "developer REPL submission limit reached")
        | None ->
          Eio.Switch.check t.sw;
          let entry =
            { key
            ; epoch
            ; expected_revision
            ; fingerprint
            ; outcome = None
            ; expired = false
            ; charged_bytes = 0
            ; reference_bytes = 0
            ; type_details = None
            ; type_detail_bytes = 0
            ; trace_entry = None
            }
          in
          state.keys <- entry :: state.keys;
          let active = launch_submission t state entry ~actor ~source in
          Eio.Promise.await active.finished;
          let%bind () = check_instance t state ~actor in
          let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
          (match active.private_failure with
           | Some (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
           | None -> key_result entry)))
;;

let observe t ~actor ~instance ~key =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  let%bind state =
    find_instance t instance
    |> Result.of_option ~error:(stale "developer REPL instance not found")
  in
  let%bind () = check_instance t state ~actor in
  let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
  match List.find state.keys ~f:(fun entry -> P.Idempotency_key.equal entry.key key) with
  | None -> Ok None
  | Some entry ->
    (match state.active with
     | Some active when P.Idempotency_key.equal active.key key ->
       Error (busy "developer REPL submission is still active")
     | Some _ | None -> Result.map (key_result entry) ~f:Option.some)
;;

let historical_trace t ~actor ~instance ~epoch ~key =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  let%bind state =
    find_instance t instance
    |> Result.of_option ~error:(stale "developer REPL instance not found")
  in
  let original = Operator_authorization.principal state.original_actor in
  let current = Operator_authorization.principal actor in
  if
    (not (Operator_authorization.is_current actor))
    || (not (P.Id.Principal.equal original.id current.id))
    || (not (String.equal original.authentication_kind current.authentication_kind))
    || not (P.Principal.has_scope current P.Scope.Developer_repl)
  then Error (denied "developer REPL historical read denied")
  else (
    match
      List.find state.keys ~f:(fun entry ->
        Epoch.equal entry.epoch epoch && P.Idempotency_key.equal entry.key key)
    with
    | None -> Error (stale "developer REPL historical key not found")
    | Some entry -> Ok entry.trace_entry)
;;

let inspect_type t ~actor ~instance ~epoch ~key ~statement_index ~selector ~max_bytes =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  let%bind state =
    find_instance t instance
    |> Result.of_option ~error:(stale "developer REPL instance not found")
  in
  let%bind () = check_instance t state ~actor in
  let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
  with_gate state.gate (fun () ->
    if (not (Epoch.equal state.epoch epoch)) || Option.is_some state.control
    then Error (stale "developer REPL epoch is not current")
    else (
      let%bind entry =
        List.find state.keys ~f:(fun entry ->
          Epoch.equal entry.epoch epoch && P.Idempotency_key.equal entry.key key)
        |> Result.of_option ~error:(stale "developer REPL submission key not found")
      in
      let%bind submission, receipts =
        match entry.outcome, entry.type_details with
        | Some (Ok submission), Some receipts when not entry.expired ->
          Ok (submission, receipts)
        | Some (Error _), _ | Some (Ok _), None | None, _ | Some (Ok _), Some _ ->
          Error (stale "developer REPL frozen type detail expired")
      in
      let%bind receipt =
        List.nth receipts statement_index
        |> Result.of_option ~error:(invalid "invalid REPL statement index")
      in
      let%bind inspected =
        Projection.inspect
          ~display_limits:t.configuration.type_display_limits
          ~max_bytes
          receipt
          ~selector
          ~checkpoint:t.checkpoint
        |> Result.map_error ~f:(function
          | Projection.Error.Invalid_max_bytes | Invalid_selection ->
            invalid "invalid REPL type selection or byte limit"
          | Output_limit -> limit "REPL type inspection output limit reached"
          | Foreign_snapshot ->
            P.Error.create
              P.Error.Invalid_state
              ~message:"developer REPL frozen type owner changed"
              ~retryable:false
              ())
      in
      let%bind () = check_instance t state ~actor in
      let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
      if
        (not (Epoch.equal state.epoch epoch))
        || Option.is_some state.control
        || entry.expired
        || Option.is_none entry.type_details
      then Error (stale "developer REPL frozen type detail expired")
      else
        Ok
          { Type_inspection.revision = submission.revision
          ; binding_name = Projection.Inspected.binding_name inspected
          ; rendered = Projection.Inspected.rendered inspected
          }))
;;

let task_references_page t ~actor ~instance ~epoch ~key ~after ~limit:requested_limit =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  let%bind state =
    find_instance t instance
    |> Result.of_option ~error:(stale "developer REPL instance not found")
  in
  let%bind () = check_instance t state ~actor in
  let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
  with_gate state.gate (fun () ->
    if (not (Epoch.equal state.epoch epoch)) || Option.is_some state.control
    then Error (stale "developer REPL epoch is not current")
    else if requested_limit <= 0 || requested_limit > state.limits.max_task_origins
    then Error (invalid "invalid REPL Task reference page size")
    else (
      let%bind _entry =
        List.find state.keys ~f:(fun entry ->
          Epoch.equal entry.epoch epoch && P.Idempotency_key.equal entry.key key)
        |> Result.of_option ~error:(stale "developer REPL submission key not found")
      in
      match state.active with
      | Some active when P.Idempotency_key.equal active.key key ->
        Error (busy "developer REPL submission is still active")
      | Some _ | None ->
        let references =
          List.filter_map (List.rev state.task_origins) ~f:(fun origin ->
            if P.Idempotency_key.equal origin.issuing_key key
            then Some origin.reference
            else None)
        in
        let%bind remaining =
          match after with
          | None -> Ok references
          | Some cursor ->
            (match
               List.findi references ~f:(fun _ reference ->
                 Task_reference.equal reference cursor)
             with
             | None -> Error (invalid "foreign REPL Task reference cursor")
             | Some (index, _) -> Ok (List.drop references (index + 1)))
        in
        let rec page remaining_bytes remaining_items acc = function
          | [] -> List.rev acc, false
          | _ when remaining_items = 0 -> List.rev acc, true
          | reference :: rest ->
            let charge = reference_charge reference in
            if charge > remaining_bytes
            then List.rev acc, true
            else
              page
                (remaining_bytes - charge)
                (remaining_items - 1)
                (reference :: acc)
                rest
        in
        let items, more =
          page (state.limits.max_reference_page_bytes - 64) requested_limit [] remaining
        in
        let%bind () = check_instance t state ~actor in
        let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
        if (not (Epoch.equal state.epoch epoch)) || Option.is_some state.control
        then Error (stale "developer REPL epoch is not current")
        else
          Ok
            { Task_reference_page.items
            ; next = (if more then List.last items else None)
            ; total = List.length references
            }))
;;

let cancel_and_wait t ~actor ~instance ~key =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  let%bind state =
    find_instance t instance
    |> Result.of_option ~error:(stale "developer REPL instance not found")
  in
  let%bind () = check_instance t state ~actor in
  let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
  let%bind entry =
    List.find state.keys ~f:(fun entry -> P.Idempotency_key.equal entry.key key)
    |> Result.of_option ~error:(stale "developer REPL submission key not found")
  in
  (match state.active with
   | Some active when P.Idempotency_key.equal active.key key ->
     Eio.Cancel.protect (fun () ->
       request_cancel active;
       Eio.Promise.await active.finished)
   | Some _ | None -> ());
  let%bind () = check_instance t state ~actor in
  let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
  key_result entry
;;

type borrowed =
  { state : instance_state
  ; actor : Operator_authorization.t
  ; epoch : Epoch.t
  ; checked : Capture_host.Source_selection.Shared.checked
  ; mutable live : bool
  }

let check_now borrowed =
  let open Result.Let_syntax in
  if
    (not borrowed.live)
    || borrowed.state.closed
    || (match !(borrowed.state.host_state) with
        | Owner_open -> false
        | Owner_closing | Owner_closed -> true)
    || (not (Epoch.equal borrowed.state.epoch borrowed.epoch))
    || Option.is_some borrowed.state.control
  then Error (stale "developer REPL borrow expired")
  else (
    let%bind () =
      check_method ~original:borrowed.state.original_actor ~current:borrowed.actor
    in
    Capture_host.Source_selection.Shared.check_now
      borrowed.state.selection
      ~checked:borrowed.checked
      ~actor:borrowed.actor)
;;

let evaluator borrowed = borrowed.state.evaluator
let execution_surface borrowed = surface_of_target borrowed.state.original.target
let source_selection borrowed = borrowed.state.selection
let prompt borrowed = borrowed.state.original.prompt
let workspace borrowed = borrowed.state.original.workspace
let original_actor borrowed = borrowed.state.original_actor
let current_actor borrowed = borrowed.actor
let run_id borrowed = borrowed.state.run_id

let task_origin borrowed handle =
  let open Result.Let_syntax in
  let%bind () = check_now borrowed in
  if not (Evaluator.current_task_handle borrowed.state.evaluator handle)
  then Error (stale "developer REPL Task handle expired")
  else
    List.find borrowed.state.task_origins ~f:(fun origin ->
      Evaluator.Task_handle.equal origin.handle handle)
    |> Result.of_option ~error:(stale "developer REPL Task source unavailable")
;;

let with_current t ~actor ~instance ~epoch ~f =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  let%bind state =
    find_instance t instance
    |> Result.of_option ~error:(stale "developer REPL instance not found")
  in
  let%bind () = check_instance t state ~actor in
  with_gate state.gate (fun () ->
    if not (Epoch.equal state.epoch epoch)
    then Error (stale "developer REPL epoch changed")
    else if Option.is_some state.active || Option.is_some state.control
    then Error (busy "developer REPL statement is active")
    else (
      let%bind checked =
        Capture_host.Source_selection.Shared.check_current
          state.selection
          ~actor
          ~checkpoint:t.checkpoint
      in
      let%bind () = check_host t in
      if
        (not (Epoch.equal state.epoch epoch))
        || Option.is_some state.active
        || Option.is_some state.control
        || state.closed
      then Error (busy "developer REPL borrow admission changed")
      else (
        let borrowed = { state; actor; epoch; checked; live = true } in
        let%bind () = check_now borrowed in
        state.borrowed <- state.borrowed + 1;
        Exn.protect
          ~f:(fun () -> f borrowed)
          ~finally:(fun () ->
            borrowed.live <- false;
            state.borrowed <- state.borrowed - 1))))
;;

let key_count state =
  List.length state.keys
  + List.length state.task_keys
  + List.length state.reset_keys
  + List.length state.close_keys
;;

module Task_action = struct
  type nonrec active = active

  type unlaunched_disposition = task_unlaunched_disposition =
    | No_durable_row
    | Terminal_row

  let owner_key : active Eio.Fiber.key = Eio.Fiber.create_key ()
  let callback_key : active Eio.Fiber.key = Eio.Fiber.create_key ()

  let task active =
    match active.kind with
    | Task task -> Some task
    | Statement -> None
  ;;

  let owns_active active (task : task_state) =
    Option.value_map task.owner.active ~default:false ~f:(phys_equal active)
  ;;

  let owns_owner_fiber active =
    Option.value_map (Eio.Fiber.get owner_key) ~default:false ~f:(phys_equal active)
  ;;

  let owns_callback_fiber active =
    Option.value_map (Eio.Fiber.get callback_key) ~default:false ~f:(phys_equal active)
  ;;

  let task_now active =
    match task active with
    | None -> Error (stale "developer REPL Task action is foreign")
    | Some task ->
      if
        (not (owns_active active task))
        || (not (Int.equal task.domain (Stdlib.Domain.self () :> int)))
        || task.owner.closed
        || (not (Epoch.equal task.owner.epoch task.epoch))
        || not (Operator_authorization.is_current task.actor)
      then Error (stale "developer REPL Task action expired")
      else check_method ~original:task.owner.original_actor ~current:task.actor
  ;;

  let mark_launching active =
    let open Result.Let_syntax in
    let%bind () = task_now active in
    match task active with
    | Some task when owns_owner_fiber active ->
      (match task.phase with
       | Registered ->
         task.phase <- Launching;
         Ok ()
       | Launching | Entered | Body_exited | Unlaunched _ | Settled | Uncertain ->
         Error (stale "developer REPL Task launch marker changed"))
    | None | Some _ -> Error (stale "developer REPL Task launch marker changed")
  ;;

  let finish_unlaunched active ~disposition =
    match task active with
    | Some task
      when owns_active active task
           && owns_owner_fiber active
           &&
           match task.phase with
           | Registered | Launching -> true
           | Entered | Body_exited | Unlaunched _ | Settled | Uncertain -> false ->
      task.phase <- Unlaunched disposition;
      Ok ()
    | None | Some _ -> Error (stale "developer REPL Task cleanup marker changed")
  ;;

  let mark_settled active =
    match task active with
    | Some task
      when owns_active active task
           && owns_owner_fiber active
           &&
           match task.phase with
           | Launching | Body_exited -> true
           | Registered | Entered | Unlaunched _ | Settled | Uncertain -> false ->
      task.phase <- Settled;
      Ok ()
    | None | Some _ -> Error (stale "developer REPL Task terminal marker changed")
  ;;

  let preflight active ~actor ~reference =
    let open Result.Let_syntax in
    let%bind () = task_now active in
    let%bind task =
      task active
      |> Result.of_option ~error:(stale "developer REPL Task action is foreign")
    in
    if
      (not (owns_owner_fiber active && phys_equal actor task.actor))
      || Option.is_some task.owner.control
    then Error (stale "developer REPL Task action actor changed")
    else if
      not (P.Id.Developer_run.equal (Task_reference.run_id reference) task.owner.run_id)
    then Error (stale "developer REPL Task reference belongs to another run")
    else (
      let%bind () = check_selection task.owner ~actor ~checkpoint:task.checkpoint in
      let%bind () = task_now active in
      match
        List.find task.owner.task_origins ~f:(fun origin ->
          Task_reference.equal origin.reference reference
          && Evaluator.current_task_handle task.owner.evaluator origin.handle)
      with
      | Some _ -> Ok task.owner.selection
      | None -> Error (stale "developer REPL Task reference expired"))
  ;;

  let enter active ~sw ~f =
    let open Result.Let_syntax in
    let%bind () = task_now active in
    let%bind task =
      task active
      |> Result.of_option ~error:(stale "developer REPL Task action is foreign")
    in
    if
      (match task.phase with
       | Launching -> false
       | Registered | Entered | Body_exited | Unlaunched _ | Settled | Uncertain -> true)
      || Option.is_some task.owner.control
      || active.cancel_requested
    then Error (stale "developer REPL Task callback entry expired")
    else (
      Eio.Switch.check sw;
      task.phase <- Entered;
      Exn.protect
        ~finally:(fun () ->
          task.callback_cancel <- None;
          task.phase <- Body_exited)
        ~f:(fun () ->
          Eio.Cancel.sub (fun cancel ->
            task.callback_cancel <- Some cancel;
            if active.cancel_requested then Eio.Cancel.cancel cancel Cancelled_statement;
            Eio.Fiber.with_binding callback_key active f)))
  ;;

  let with_current active ~actor ~reference ~f =
    let open Result.Let_syntax in
    let%bind () = task_now active in
    let%bind task =
      task active
      |> Result.of_option ~error:(stale "developer REPL Task action is foreign")
    in
    if
      (not (owns_callback_fiber active))
      || (not (phys_equal actor task.actor))
      ||
      match task.phase with
      | Entered -> false
      | Registered | Launching | Body_exited | Unlaunched _ | Settled | Uncertain -> true
    then Error (stale "developer REPL Task callback scope expired")
    else
      with_gate task.owner.gate (fun () ->
        let%bind () = task_now active in
        if Option.is_some task.owner.control || active.cancel_requested
        then Error (stale "developer REPL Task control was reserved")
        else (
          let%bind checked =
            Capture_host.Source_selection.Shared.check_current
              task.owner.selection
              ~actor
              ~checkpoint:task.checkpoint
          in
          let%bind () = task_now active in
          let%bind () =
            Capture_host.Source_selection.Shared.check_now
              task.owner.selection
              ~checked
              ~actor
          in
          let%bind origin =
            List.find task.owner.task_origins ~f:(fun origin ->
              Task_reference.equal origin.reference reference
              && Evaluator.current_task_handle task.owner.evaluator origin.handle)
            |> Result.of_option ~error:(stale "developer REPL Task reference expired")
          in
          let borrowed =
            { state = task.owner; actor; epoch = task.epoch; checked; live = true }
          in
          let%bind () = check_now borrowed in
          task.owner.borrowed <- task.owner.borrowed + 1;
          Exn.protect
            ~f:(fun () -> f borrowed origin.handle origin)
            ~finally:(fun () ->
              borrowed.live <- false;
              task.owner.borrowed <- task.owner.borrowed - 1)))
  ;;

  let start
        (type a)
        t
        ~actor
        ~instance
        ~epoch
        ~key
        ~(f :
           active
           -> publish_initial:((a, P.Error.t) result -> unit)
           -> (unit, P.Error.t) result)
    : (a, P.Error.t) result
    =
    let open Result.Let_syntax in
    let%bind () = check_host t in
    let%bind () =
      match t.task_cleanup with
      | Some _ -> Ok ()
      | None -> Error (invalid "developer REPL Task cleanup is not installed")
    in
    let%bind state =
      find_instance t instance
      |> Result.of_option ~error:(stale "developer REPL instance not found")
    in
    let%bind () = check_instance t state ~actor in
    let%bind active =
      with_gate state.gate (fun () ->
        if
          state.closed
          || (not (Epoch.equal state.epoch epoch))
          || Option.is_some state.active
          || Option.is_some state.control
          || state.borrowed > 0
        then Error (busy "developer REPL Task admission is occupied")
        else if key_count state >= state.limits.max_keys_per_instance - 1
        then Error (limit "developer REPL key ledger full")
        else if
          List.exists state.keys ~f:(fun entry -> P.Idempotency_key.equal entry.key key)
          || List.exists state.task_keys ~f:(P.Idempotency_key.equal key)
          || List.exists state.reset_keys ~f:(fun (prior, _, _) ->
            P.Idempotency_key.equal prior key)
          || List.exists state.close_keys ~f:(P.Idempotency_key.equal key)
        then Error (busy "developer REPL key used for another action")
        else (
          Eio.Switch.check t.sw;
          let finished, finish = Eio.Promise.create () in
          let task =
            { owner = state
            ; actor
            ; epoch
            ; domain = t.domain
            ; checkpoint = t.checkpoint
            ; phase = Registered
            ; callback_cancel = None
            }
          in
          let active =
            { key
            ; kind = Task task
            ; finished
            ; cancel = None
            ; cancel_requested = false
            ; private_failure = None
            }
          in
          state.active <- Some active;
          state.task_keys <- key :: state.task_keys;
          Ok (active, finish)))
    in
    let active, finish = active in
    let initial, resolve_initial = Eio.Promise.create () in
    let published = ref false in
    let publish_initial result =
      if not !published
      then (
        published := true;
        Eio.Promise.resolve resolve_initial (`Returned result))
    in
    let worker () =
      let outcome =
        try
          `Returned
            (Eio.Cancel.sub (fun cancel ->
               active.cancel <- Some cancel;
               if active.cancel_requested
               then Eio.Cancel.cancel cancel Cancelled_statement;
               Eio.Fiber.with_binding owner_key active (fun () ->
                 f active ~publish_initial)))
        with
        | exn -> `Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
      in
      Eio.Cancel.protect (fun () ->
        active.cancel <- None;
        (match outcome with
         | `Raised failure -> active.private_failure <- Some failure
         | `Returned _ -> ());
        if not !published
        then (
          published := true;
          Eio.Promise.resolve
            resolve_initial
            (match outcome with
             | `Returned (Error error) -> `Returned (Error error)
             | `Returned (Ok ()) ->
               `Returned
                 (Error (invalid "developer REPL Task launch did not publish a result"))
             | `Raised failure -> `Raised failure));
        (match task active with
         | Some task ->
           (match task.phase with
            | Unlaunched No_durable_row ->
              state.task_keys
              <- List.filter state.task_keys ~f:(fun prior ->
                   not (P.Idempotency_key.equal prior key));
              state.active <- None
            | Unlaunched Terminal_row | Settled -> state.active <- None
            | Registered | Launching | Entered | Body_exited | Uncertain ->
              task.phase <- Uncertain)
         | None -> ());
        Eio.Promise.resolve finish ())
    in
    match Eio.Fiber.fork ~sw:t.sw worker with
    | () ->
      (match Eio.Promise.await initial with
       | `Returned result -> result
       | `Raised (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace)
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      state.active <- None;
      state.task_keys
      <- List.filter state.task_keys ~f:(fun prior ->
           not (P.Idempotency_key.equal prior key));
      Stdlib.Printexc.raise_with_backtrace exn backtrace
  ;;
end

type owner = t

let instance_run_id t instance =
  if not (on_domain t)
  then Error (stale "developer REPL owner used from another domain")
  else
    find_instance t instance
    |> Result.of_option ~error:(stale "developer REPL instance not found")
    |> Result.map ~f:(fun state -> state.run_id)
;;

let instance_paired_to t instance ~selection ~original_actor =
  on_domain t
  && Option.value_map (find_instance t instance) ~default:false ~f:(fun state ->
    phys_equal state.selection selection && phys_equal state.original_actor original_actor)
;;

let instance_uses_selection t instance selection =
  on_domain t
  && Option.value_map (find_instance t instance) ~default:false ~f:(fun state ->
    phys_equal state.selection selection)
;;

module Task_cleanup = struct
  type t = task_cleanup
  type custody = unit ref

  let current_custody owner instance =
    let open Result.Let_syntax in
    let%bind state =
      find_instance owner instance
      |> Result.of_option ~error:(stale "developer REPL instance not found")
    in
    if not (on_domain owner)
    then Error (stale "developer REPL owner used from another domain")
    else (
      match state.control with
      | Some control
        when Option.value_map state.active ~default:true ~f:(fun active ->
               Eio.Promise.is_resolved active.finished) -> Ok control.custody
      | Some _ | None -> Error (stale "developer REPL retirement control changed"))
  ;;

  let same_custody = phys_equal

  let check_retiring_current t ~actor ~instance =
    let open Result.Let_syntax in
    let%bind state =
      find_instance t instance
      |> Result.of_option ~error:(stale "developer REPL instance not found")
    in
    let expected = state.control in
    let check_local () =
      let%bind () = check_instance t state ~actor in
      if
        Option.value_map expected ~default:false ~f:(fun before ->
          Option.value_map state.control ~default:false ~f:(phys_equal before))
        && Option.value_map state.active ~default:true ~f:(fun active ->
          Eio.Promise.is_resolved active.finished)
      then Ok ()
      else Error (stale "developer REPL retirement control changed")
    in
    let%bind () = check_local () in
    let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
    check_local ()
  ;;

  let create ~authorize_retirement ~retire_instance ~retire_all =
    { authorize_retirement; retire_instance; retire_all }
  ;;

  let install_once owner cleanup =
    match owner.task_cleanup with
    | None ->
      owner.task_cleanup <- Some cleanup;
      Ok ()
    | Some _ -> Error (invalid "developer REPL Task cleanup already installed")
  ;;
end

let authorize_retirement t ~actor ~instance =
  match t.task_cleanup with
  | None -> Ok ()
  | Some cleanup -> cleanup.authorize_retirement ~actor ~instance
;;

let retire_instance t ~actor ~instance =
  match t.task_cleanup with
  | None -> Ok ()
  | Some cleanup -> cleanup.retire_instance ~actor instance
;;

let retire_all t =
  match t.task_cleanup with
  | None -> Ok ()
  | Some cleanup -> cleanup.retire_all ()
;;

let clear_settled_task state =
  match state.active with
  | Some ({ kind = Task _; _ } as active) when Eio.Promise.is_resolved active.finished ->
    state.active <- None
  | Some { kind = Statement; _ } | Some { kind = Task _; _ } | None -> ()
;;

type control_admission =
  | Control_owner of control
  | Control_follower of control

let begin_control state ~kind ~key =
  let create custody =
    let finished, finish = Eio.Promise.create () in
    let control =
      { kind; key; custody; finished; finish; outcome = None; retry_cleanup = false }
    in
    state.control <- Some control;
    Control_owner control
  in
  match state.control with
  | None -> Ok (create (ref ()))
  | Some current ->
    let same_kind =
      match current.kind, kind with
      | Reset_action, Reset_action | Close_action, Close_action -> true
      | Reset_action, Close_action | Close_action, Reset_action -> false
    in
    if not (same_kind && P.Idempotency_key.equal current.key key)
    then Error (busy "developer REPL control action is active")
    else if current.retry_cleanup && Option.is_some current.outcome
    then Ok (create current.custody)
    else Ok (Control_follower current)
;;

let finish_control state (control : control) ~outcome ~retry_cleanup =
  control.outcome <- Some outcome;
  control.retry_cleanup <- retry_cleanup;
  Eio.Promise.resolve control.finish ();
  if not retry_cleanup
  then (
    match state.control with
    | Some current when phys_equal current control -> state.control <- None
    | Some _ | None -> ())
;;

let reset_key_result state ~key ~expected_epoch =
  match
    List.find state.reset_keys ~f:(fun (prior, _, _) -> P.Idempotency_key.equal prior key)
  with
  | Some (_, prior_epoch, previous) ->
    if Epoch.equal prior_epoch expected_epoch
    then Ok (Some previous)
    else Error (busy "developer REPL reset key changed epoch")
  | None ->
    if
      List.exists state.keys ~f:(fun entry -> P.Idempotency_key.equal entry.key key)
      || List.exists state.task_keys ~f:(P.Idempotency_key.equal key)
      || List.exists state.close_keys ~f:(P.Idempotency_key.equal key)
    then Error (busy "developer REPL key used for another action")
    else Ok None
;;

let close_key_result state ~key =
  if List.exists state.close_keys ~f:(P.Idempotency_key.equal key)
  then Ok true
  else if
    List.exists state.keys ~f:(fun entry -> P.Idempotency_key.equal entry.key key)
    || List.exists state.task_keys ~f:(P.Idempotency_key.equal key)
    || List.exists state.reset_keys ~f:(fun (prior, _, _) ->
      P.Idempotency_key.equal prior key)
  then Error (busy "developer REPL key used for another action")
  else Ok false
;;

let join_active state =
  match state.active with
  | None -> ()
  | Some active ->
    Eio.Cancel.protect (fun () ->
      request_cancel active;
      Eio.Promise.await active.finished)
;;

let reset_and_wait t ~actor ~instance ~expected_epoch ~key =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  let%bind state =
    find_instance t instance
    |> Result.of_option ~error:(stale "developer REPL instance not found")
  in
  let%bind () = check_instance t state ~actor in
  let%bind prior = reset_key_result state ~key ~expected_epoch in
  match prior with
  | Some previous ->
    let%map () = check_selection state ~actor ~checkpoint:t.checkpoint in
    previous
  | None ->
    let%bind () = authorize_retirement t ~actor ~instance in
    if not (Epoch.equal state.epoch expected_epoch)
    then Error (stale "developer REPL epoch changed")
    else if key_count state >= state.limits.max_keys_per_instance - 1
    then Error (limit "developer REPL key ledger full")
    else (
      let%bind control = begin_control state ~kind:Reset_action ~key in
      match control with
      | Control_follower control ->
        Eio.Promise.await control.finished;
        let%bind () = check_instance t state ~actor in
        let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
        (match control.outcome with
         | Some (Reset_done result) -> result
         | Some (Control_raised (exn, backtrace)) ->
           Stdlib.Printexc.raise_with_backtrace exn backtrace
         | Some (Close_done _) | None -> assert false)
      | Control_owner control ->
        let old_close_started = ref false in
        let run () =
          Eio.Cancel.protect (fun () ->
            join_active state;
            old_close_started := true;
            let%bind () = retire_instance t ~actor ~instance in
            clear_settled_task state;
            with_gate state.gate (fun () ->
              let%bind prior = reset_key_result state ~key ~expected_epoch in
              match prior with
              | Some previous ->
                let%map () = check_selection state ~actor ~checkpoint:t.checkpoint in
                previous
              | None ->
                let%bind () = check_instance t state ~actor in
                let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
                if not (Epoch.equal state.epoch expected_epoch)
                then Error (stale "developer REPL epoch changed")
                else if state.borrowed > 0 || Option.is_some state.active
                then Error (busy "developer REPL Task or statement is active")
                else if key_count state >= state.limits.max_keys_per_instance - 1
                then Error (limit "developer REPL key ledger full")
                else (
                  let%bind new_typing, new_evaluator =
                    make_epoch_resources
                      t
                      ~selection:state.selection
                      ~original:state.original
                  in
                  let admission =
                    try
                      let%bind () = check_instance t state ~actor in
                      check_selection state ~actor ~checkpoint:t.checkpoint
                    with
                    | exn ->
                      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                      reraise_after_cleanup exn backtrace ~cleanup:(fun () ->
                        discard_unpublished_evaluator new_evaluator)
                  in
                  match admission with
                  | Error _ as error ->
                    discard_unpublished_evaluator new_evaluator;
                    error
                  | Ok () ->
                    old_close_started := true;
                    let trace_provenance =
                      initial_trace_provenance
                        t
                        ~selection:state.selection
                        ~original:state.original
                        ~actor:state.original_actor
                    in
                    (match Evaluator.close state.evaluator with
                     | exception exn ->
                       let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                       reraise_after_cleanup exn backtrace ~cleanup:(fun () ->
                         discard_unpublished_evaluator new_evaluator)
                     | Error _ ->
                       discard_unpublished_evaluator new_evaluator;
                       Error (busy "developer REPL evaluator busy")
                     | Ok () ->
                       state.typing <- new_typing;
                       state.evaluator <- new_evaluator;
                       state.epoch
                       <- { Epoch.token = ref (); serial = state.epoch.Epoch.serial + 1 };
                       state.revision <- { Revision.token = ref (); serial = 0 };
                       state.submission_count <- 0;
                       state.pending_source <- "";
                       state.trace_provenance <- trace_provenance;
                       List.iter state.keys ~f:(fun entry ->
                         state.retained_bytes
                         <- state.retained_bytes - entry.reference_bytes;
                         state.retained_type_detail_bytes
                         <- state.retained_type_detail_bytes - entry.type_detail_bytes;
                         entry.reference_bytes <- 0;
                         entry.type_detail_bytes <- 0;
                         entry.type_details <- None;
                         entry.outcome
                         <- (match entry.outcome with
                             | Some (Ok submission) when not entry.expired ->
                               Some
                                 (Ok
                                    { submission with
                                      details = Output_limited
                                    ; statements = []
                                    ; task_handles = []
                                    ; task_references = []
                                    })
                             | outcome -> outcome));
                       state.task_origins <- [];
                       state.task_ordinal <- 0;
                       let result = view state in
                       state.reset_keys
                       <- (key, expected_epoch, result) :: state.reset_keys;
                       Ok result))))
        in
        (match run () with
         | result ->
           finish_control
             state
             control
             ~outcome:(Reset_done result)
             ~retry_cleanup:(!old_close_started && Result.is_error result);
           result
         | exception exn ->
           let backtrace = Stdlib.Printexc.get_raw_backtrace () in
           finish_control
             state
             control
             ~outcome:(Control_raised (exn, backtrace))
             ~retry_cleanup:!old_close_started;
           Stdlib.Printexc.raise_with_backtrace exn backtrace))
;;

let close_and_wait t ~actor ~instance ~key =
  let open Result.Let_syntax in
  let%bind () = check_host t in
  let%bind state =
    find_instance t instance
    |> Result.of_option ~error:(stale "developer REPL instance not found")
  in
  let%bind () = check_method ~original:state.original_actor ~current:actor in
  let%bind prior = close_key_result state ~key in
  if prior
  then Ok ()
  else if state.closed
  then Error (stale "developer REPL instance closed")
  else if key_count state >= state.limits.max_keys_per_instance
  then Error (limit "developer REPL key ledger full")
  else (
    let%bind () = authorize_retirement t ~actor ~instance in
    let%bind control = begin_control state ~kind:Close_action ~key in
    match control with
    | Control_follower control ->
      Eio.Promise.await control.finished;
      let%bind () = check_method ~original:state.original_actor ~current:actor in
      (match control.outcome with
       | Some (Close_done result) -> result
       | Some (Control_raised (exn, backtrace)) ->
         Stdlib.Printexc.raise_with_backtrace exn backtrace
       | Some (Reset_done _) | None -> assert false)
    | Control_owner control ->
      let evaluator_close_started = ref false in
      let run () =
        Eio.Cancel.protect (fun () ->
          let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
          join_active state;
          evaluator_close_started := true;
          let%bind () = retire_instance t ~actor ~instance in
          clear_settled_task state;
          with_gate state.gate (fun () ->
            let%bind prior = close_key_result state ~key in
            if prior
            then Ok ()
            else (
              let%bind () = check_instance t state ~actor in
              let%bind () = check_selection state ~actor ~checkpoint:t.checkpoint in
              if state.borrowed > 0 || Option.is_some state.active
              then Error (busy "developer REPL Task or statement is active")
              else if key_count state >= state.limits.max_keys_per_instance
              then Error (limit "developer REPL key ledger full")
              else (
                evaluator_close_started := true;
                let%bind () =
                  Evaluator.close state.evaluator
                  |> Result.map_error ~f:(fun _ -> busy "developer REPL evaluator busy")
                in
                state.closed <- true;
                state.pending_source <- "";
                state.trace_provenance <- None;
                state.task_origins <- [];
                state.task_ordinal <- 0;
                state.close_keys <- key :: state.close_keys;
                Ok ()))))
      in
      (match run () with
       | result ->
         finish_control
           state
           control
           ~outcome:(Close_done result)
           ~retry_cleanup:(!evaluator_close_started && Result.is_error result);
         result
       | exception exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         finish_control
           state
           control
           ~outcome:(Control_raised (exn, backtrace))
           ~retry_cleanup:!evaluator_close_started;
         Stdlib.Printexc.raise_with_backtrace exn backtrace))
;;

let shutdown_and_wait t =
  if not (on_domain t)
  then Error (stale "developer REPL owner used from another domain")
  else if
    match !(t.owner_state) with
    | Owner_closed -> true
    | Owner_open | Owner_closing -> false
  then Ok ()
  else (
    t.owner_state := Owner_closing;
    Eio.Cancel.protect (fun () ->
      List.iter t.instances ~f:join_active;
      let open Result.Let_syntax in
      let%bind () = retire_all t in
      List.iter t.instances ~f:clear_settled_task;
      let%bind () =
        List.fold_result t.instances ~init:() ~f:(fun () state ->
          with_gate state.gate (fun () ->
            if state.closed
            then Ok ()
            else if state.borrowed > 0
            then Error (busy "developer REPL Task borrow is active")
            else
              Evaluator.close state.evaluator
              |> Result.map_error ~f:(fun _ -> busy "developer REPL evaluator busy")
              |> Result.map ~f:(fun () -> state.closed <- true)))
      in
      t.owner_state := Owner_closed;
      Ok ()))
;;
