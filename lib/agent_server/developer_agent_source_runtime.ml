open! Core
module P = Agent_protocol
module O = Developer_root_owner
module C = Chat_response.Tool_capability
module R = Chat_response.Agent_runtime
module N = Agent_session.Native_tool_invocation
module Claimed = Agent_session.Captured_agent_runner.Claimed_developer

module Selected_target = struct
  type mode =
    | Literal_only
    | Bound_runner

  type runner =
    target:Claimed.Selected.Target.t
    -> startup:Chat_response.Startup_compilation.t
    -> compilation_limits:Chatml_compilation.limits
    -> programs:Chatml_prompt_programs.Restored.t option
    -> ctx:Eio_unix.Stdenv.base Chat_response.Ctx.t
    -> input:Prompt.Chat_markdown.content_item list
    -> observer:Chat_response.Driver.agent_observer option
    -> source:string
    -> (string, P.Error.t) result
end

module Limits = struct
  type t =
    { max_agents : int
    ; max_work_steps : int
    }

  let create ~max_agents ~max_work_steps =
    if max_agents <= 0 || max_agents > 1024 || max_work_steps <= 0
    then Or_error.error_string "deferred Agent limits must be positive and bounded"
    else Ok { max_agents; max_work_steps }
  ;;

  let max_agents t = t.max_agents
  let max_work_steps t = t.max_work_steps
end

type bound =
  { root : O.run
  ; models : Developer_model_job_runner.t
  ; agent_runtime : R.t
  ; run_selected_target : Selected_target.runner option
  }

type state =
  | Unbound
  | Bound of bound
  | Revoked

type t =
  { domain_id : int
  ; deferred : Agent_session.Captured_agent_runner.Deferred_developer.t
  ; source_binding : Agent_session.Captured_agent_runner.Source_binding.t
  ; limits : Limits.t
  ; selected_target : Selected_target.mode
  ; state : state ref
  }

let invalid message = Error (P.Error.invalid_request message)
let current_domain domain_id = Int.equal domain_id (Stdlib.Domain.self () :> int)

let with_context state ~implementation ~invocation:_ f =
  let fail error = failwith error.P.Error.message in
  let result =
    match !state with
    | Unbound -> invalid "deferred Agent Root has not been bound"
    | Revoked -> invalid "deferred Agent Root has been revoked"
    | Bound bound ->
      let open Result.Let_syntax in
      let%bind borrowed = N.borrow_selected () in
      Claimed.with_selected ~borrowed ~implementation (fun selected ->
        let%bind () = Claimed.Selected.check selected in
        let%map value =
          Developer_model_job_runner.with_saved_agent_context
            bound.models
            ~selected
            ~f:(fun ctx ->
              (match !state with
               | Bound current when phys_equal current bound -> ()
               | Unbound | Bound _ | Revoked ->
                 fail (P.Error.invalid_request "deferred Agent Root changed"));
              match Claimed.Selected.check selected with
              | Ok () -> f ctx
              | Error error -> fail error)
        in
        match !state with
        | Bound current when phys_equal current bound -> value
        | Unbound | Bound _ | Revoked ->
          fail (P.Error.invalid_request "deferred Agent Root changed"))
  in
  match result with
  | Ok value -> value
  | Error error -> fail error
;;

let with_selected_target
      state
      ~target
      ~startup
      ~compilation_limits
      ~programs
      ~ctx
      ~input
      ~observer
      ~source
  =
  match !state with
  | Unbound -> invalid "deferred Agent child Root has not been bound"
  | Revoked -> invalid "deferred Agent child Root has been revoked"
  | Bound { run_selected_target = None; _ } ->
    invalid "deferred Agent selected child executor is unavailable"
  | Bound { run_selected_target = Some run; _ } ->
    run ~target ~startup ~compilation_limits ~programs ~ctx ~input ~observer ~source
;;

let prepare
      ~selected_target
      ~source
      ~script_preparation
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~env
      ~revision
      ~sw
      ~(limits : Limits.t)
      ~checkpoint
  =
  let state = ref Unbound in
  let run_selected_target =
    match selected_target with
    | Selected_target.Literal_only -> None
    | Bound_runner -> Some (with_selected_target state)
  in
  Agent_session.Captured_agent_runner.Deferred_developer.prepare
    ?run_selected_target
    source
    ~script_preparation
    ~manifest_authorizer
    ~approval_provider
    ~response_dir
    ~env
    ~revision
    ~sw
    ~max_agents:limits.max_agents
    ~max_work_steps:limits.max_work_steps
    ~checkpoint
    ~with_context:(with_context state)
  |> Result.map ~f:(fun deferred ->
    { domain_id = (Stdlib.Domain.self () :> int)
    ; deferred
    ; source_binding = source
    ; limits
    ; selected_target
    ; state
    })
;;

let registrations t =
  Agent_session.Captured_agent_runner.Deferred_developer.registrations t.deferred
;;

let bind t ~root ~models ~agent_runtime ~run_selected_target ~checkpoint =
  let open Result.Let_syntax in
  if not (current_domain t.domain_id)
  then invalid "deferred Agent belongs to another Eio domain"
  else (
    match !(t.state) with
    | Bound _ | Revoked -> invalid "deferred Agent Root is already bound or revoked"
    | Unbound ->
      if
        not
          (match t.selected_target, run_selected_target with
           | Selected_target.Literal_only, None | Bound_runner, Some _ -> true
           | Literal_only, Some _ | Bound_runner, None -> false)
      then invalid "deferred Agent selected child mode changed before bind"
      else if not (Developer_model_job_runner.issued_by_root models root)
      then invalid "deferred Agent provider belongs to another Root"
      else (
        let selected = O.native_selected root in
        let%bind () =
          R.admit_native_selection
            agent_runtime
            ~selected
            ~max_bindings:t.limits.max_work_steps
            ~checkpoint
        in
        let references = C.references selected in
        let%bind () =
          List.fold_result (registrations t) ~init:() ~f:(fun () registration ->
            let%bind () =
              R.admit_exact_saved_agent_registration
                agent_runtime
                ~registration
                ~max_registrations:t.limits.max_work_steps
                ~checkpoint
            in
            let%bind matches =
              List.fold_result references ~init:0 ~f:(fun matches reference ->
                checkpoint ();
                let%bind binding =
                  C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
                  |> Result.map_error ~f:(fun error ->
                    P.Error.invalid_request error.C.message)
                in
                Ok
                  (if
                     Option.value_map
                       (C.native_implementation binding)
                       ~default:false
                       ~f:(phys_equal registration.implementation)
                   then matches + 1
                   else matches))
            in
            if Int.equal matches 1
            then Ok ()
            else invalid "deferred Agent registration is not uniquely selected")
        in
        match !(t.state) with
        | Unbound ->
          t.state := Bound { root; models; agent_runtime; run_selected_target };
          Ok ()
        | Bound _ | Revoked -> invalid "deferred Agent Root changed during admission"))
;;

let bound_to t ~root ~agent_runtime =
  if not (current_domain t.domain_id)
  then invalid "deferred Agent belongs to another Eio domain"
  else (
    match !(t.state) with
    | Bound bound
      when phys_equal bound.root root && phys_equal bound.agent_runtime agent_runtime ->
      Ok ()
    | Unbound | Revoked | Bound _ ->
      invalid "deferred Agent source is not bound to this Root/runtime")
;;

let prepare_claimed
      t
      ~root
      ~agent_runtime
      ~source
      ~script_preparation
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~env
      ~sw
      ~checkpoint
  =
  let open Result.Let_syntax in
  let check () =
    let%bind () = bound_to t ~root ~agent_runtime in
    let%bind () =
      if
        Agent_session.Captured_agent_runner.Source_binding.Claimed_source.issued_by
          source
          t.source_binding
      then Ok ()
      else invalid "claimed Agent source belongs to another bound source runtime"
    in
    Agent_session.Captured_agent_runner.Source_binding.Claimed_source.check_now source
  in
  let%bind () = check () in
  let%bind () =
    match t.selected_target, !(t.state) with
    | Selected_target.Bound_runner, Bound { run_selected_target = Some _; _ } -> Ok ()
    | Literal_only, (Unbound | Bound _ | Revoked)
    | Bound_runner, (Unbound | Bound _ | Revoked) ->
      invalid "claimed Agent selected child runner is unavailable"
  in
  let%bind claimed =
    Agent_session.Captured_agent_runner.Deferred_developer.prepare_claimed
      source
      ~script_preparation
      ~manifest_authorizer
      ~approval_provider
      ~response_dir
      ~env
      ~sw
      ~max_agents:t.limits.max_agents
      ~max_work_steps:t.limits.max_work_steps
      ~checkpoint
      ~with_context:(with_context t.state)
      ~run_selected_target:(with_selected_target t.state)
  in
  let%map () = check () in
  claimed
;;

let revoke t =
  if not (current_domain t.domain_id)
  then invalid_arg "deferred Agent belongs to another Eio domain"
  else t.state := Revoked
;;
