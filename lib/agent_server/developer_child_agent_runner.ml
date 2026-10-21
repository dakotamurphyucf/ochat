open! Core
module P = Agent_protocol
module H = P.Host_invocation
module O = Developer_root_owner
module N = Developer_native_executor
module Live = Developer_live_resources
module Saved = Agent_session.Captured_agent_runner.Claimed_developer.Selected
module Content_target = Agent_session.Captured_agent_runner.Selected_content_target
module Model_target = Agent_session.Captured_agent_runner.Selected_model_target
module Tool_target = Agent_session.Captured_agent_runner.Selected_tool_target
module Native_borrow = Agent_session.Native_tool_invocation
module Revision = Agent_session.Prompt_revision
module C = Chat_response.Tool_capability
module M = Chat_response.Managed_tool_registry
module Driver = Chat_response.Driver
module Model_executor = Chat_response.Model_executor
module Model_drain = Chat_response.Model_completion_drain
module Model_request = P.Model_spawn_request
module CM = Prompt.Chat_markdown
module Prepared = Driver.Prepared_inputs
module Content = Developer_child_content_projection
module Content_source = Agent_session.Captured_agent_runner.Source_binding.Content_source
module Output = Openai.Responses.Tool_output.Output

exception Tool_denied of P.Error.t
exception Selected_stop of unit

let ( let* ) result f = Result.bind result ~f
let invalid message = P.Error.invalid_request message

let input_of_tool (descriptor : Openai.Completions.tool) raw =
  match descriptor.type_ with
  | "custom" -> Ok (`String raw)
  | "function" ->
    Result.try_with (fun () -> Jsonaf.of_string raw)
    |> Result.map_error ~f:(fun _ -> invalid "child tool arguments are not JSON")
  | _ -> Error (invalid "selected child has an unsupported tool descriptor")
;;

let response_of_outcome = function
  | (P.Invocation.Complete _ | Pending _ | Fail _ | Cancelled _) as outcome ->
    Output.Text (P.Invocation.outcome_to_json outcome |> Jsonaf.to_string)
;;

let invocation ~root ~parent ~managed ~selected ~binding ~input =
  let reference = C.reference binding in
  let parent = H.context parent in
  let* implementation_revision =
    match C.implementation binding with
    | Native _ -> Ok reference.implementation_revision
    | Managed _ ->
      M.execution_revision managed binding
      |> Result.map_error ~f:(fun error -> invalid error.message)
  in
  H.create_developer
    { H.id = P.Id.Invocation.create ()
    ; owner = P.Execution_owner.Developer (O.owner root)
    ; origin = Delegated_agent
    ; parent_invocation = Some parent.id
    ; parent_job = parent.parent_job
    ; tool_name = reference.name
    ; implementation_revision
    ; capability_fingerprint = C.fingerprint selected
    ; input
    ; created_at = O.native_now root
    ; deadline = parent.deadline
    }
;;

let tool_table ~root ~prepared ~child ~authorize ~prepare_output =
  let* selected = N.Child.current_capabilities child in
  let* descriptors =
    List.fold_result (C.references selected) ~init:[] ~f:(fun descriptors reference ->
      let* binding =
        C.resolve selected ~id:reference.id ~fingerprint:reference.fingerprint
        |> Result.map_error ~f:(fun error -> invalid error.message)
      in
      Ok (C.descriptor binding :: descriptors))
  in
  let descriptors = List.rev descriptors in
  let table = Hashtbl.create (module String) in
  List.iter (C.references selected) ~f:(fun reference ->
    let runner ~invocation:native_invocation raw =
      let checked =
        let* current = N.Child.current_capabilities child in
        let* binding =
          C.resolve current ~id:reference.id ~fingerprint:reference.fingerprint
          |> Result.map_error ~f:(fun error -> invalid error.message)
        in
        let* input = input_of_tool (C.descriptor binding) raw in
        let* borrowed = Native_borrow.borrow_selected () in
        let* invocation =
          invocation
            ~root
            ~parent:(Native_borrow.selected_borrowed_invocation borrowed)
            ~managed:(Live.Child_prepared.managed_definition prepared)
            ~selected:current
            ~binding
            ~input
        in
        let* terminal =
          N.Child.run
            child
            ~reference
            ~invocation
            ~native_invocation
            ~authorize
            ~prepare_output
        in
        let* outcome =
          H.outcome terminal
          |> Result.of_option
               ~error:(invalid "selected child has no terminal native outcome")
        in
        Ok (response_of_outcome outcome)
      in
      match checked with
      | Ok response -> response
      | Error error -> raise (Tool_denied error)
    in
    Hashtbl.add_exn table ~key:reference.name ~data:runner);
  Ok (Chat_response.Tool.convert_tools descriptors, table)
;;

type child_context =
  { prepared : Live.Child_prepared.t
  ; child : N.Child.t
  ; budget : Content.Budget.t
  ; target : Content_target.parent
  }

let child_key : child_context Eio.Fiber.key = Eio.Fiber.create_key ()

type execute_context =
  { child : child_context
  ; ctx : Eio_unix.Stdenv.base Chat_response.Ctx.t
  }

let execute_context_key : execute_context Eio.Fiber.key = Eio.Fiber.create_key ()

type selected_moderator_run =
  { prepared : Live.Child_prepared.t
  ; runtime_policy : Chat_response.Runtime_semantics.policy
  ; ctx : Eio_unix.Stdenv.base Chat_response.Ctx.t
  ; executor : Model_executor.t
  ; session_id : string
  ; materialized : Prepared.Materialized.t
  ; tools : Openai.Responses.Request.Tool.t list
  ; tool_tbl : (string, Ochat_function.runner) Hashtbl.t
  ; history_compaction : bool
  ; observer : Driver.agent_observer option
  ; source : string
  ; parent_call_id : string
  }

let run_selected_moderator run ~stop_reason ~stop_result =
  let stop result =
    stop_result := Some result;
    raise stop_reason
  in
  let { prepared
      ; runtime_policy
      ; ctx
      ; executor
      ; session_id
      ; materialized
      ; tools
      ; tool_tbl
      ; history_compaction
      ; observer
      ; source
      ; parent_call_id
      }
    =
    run
  in
  let allocator = Prepared.Materialized.allocator materialized in
  let history = Prepared.Materialized.initial_history materialized in
  let committed_ids = Hash_set.create (module History_entry.Id) in
  let committed_rev = ref [] in
  let on_committed_entry entry =
    let id = History_entry.id entry in
    if not (Hash_set.mem committed_ids id)
    then (
      Hash_set.add committed_ids id;
      committed_rev := entry :: !committed_rev)
  in
  let committed_text () = Driver.assistant_text (List.rev !committed_rev) in
  let runtime = Live.Child_prepared.runtime prepared in
  let recipe = Model_executor.recipe_agent_prompt_v1 executor ~session_id () in
  let capabilities =
    Driver.capabilities_with_model_recipe
      ~recipe
      Chat_response.Moderation.Capabilities.default
  in
  let on_process_run =
    Chat_response.Agent_runtime.moderator_process_handler runtime
    |> Option.map ~f:(fun handler session ~command ~args ->
      let refresh () =
        Live.Child_prepared.refresh_current_borrow prepared
        |> Result.map_error ~f:(fun error -> error.P.Error.message)
        |> Result.map ~f:(fun admission () ->
          Live.Child_prepared.Admission.check_now admission
          |> Result.map_error ~f:(fun error -> error.P.Error.message))
      in
      Shell_runtime.Call_context.with_narrowed_check ~refresh (fun () ->
        handler session ~command ~args)
      |> Result.join)
  in
  let started =
    Driver.create_selected_moderator_entries
      ~env:(Chat_response.Ctx.env ctx)
      ~session_id
      ~artifact:runtime.legacy_moderator
      ~allocator
      ~history
      ~available_tools:tools
      ~capabilities
      ~runtime_policy
      ?on_process_run
      ()
  in
  let moderator =
    match started with
    | Error message -> stop (Error (invalid message))
    | Ok Driver.Selected_moderator_start.No_moderator -> None
    | Ok (Ready moderator) ->
      Model_executor.register_session executor ~session_id ~manager:moderator.manager;
      Some moderator
    | Ok Halted | Ok (End_session_requested _) -> stop (Ok "")
    | Ok Paused -> stop (Error (invalid "selected moderator is paused"))
  in
  let on_runtime_request = function
    | Chat_response.Moderation.Runtime_request.End_session _ ->
      stop (Ok (committed_text ()))
    | Request_turn | Request_compaction ->
      stop (Error (invalid "selected moderator requested a mutable runtime action"))
  in
  let all =
    Driver.run_stream_entries
      ~ctx
      ~allocator
      ~history
      ~tools
      ~tool_tbl
      ~moderator
      ~history_compaction
      ~response_dir:(Live.Child_prepared.response_dir prepared)
      ~on_runtime_request
      ~on_committed_entry
      ?observer
      ~source
      ~parent_call_id
      ()
  in
  match moderator with
  | None -> all
  | Some moderator ->
    let drained =
      Model_drain.run_entries
        ~executor
        ~moderator
        ~now_ms:(fun () ->
          Eio.Time.now (Eio.Stdenv.clock (Chat_response.Ctx.env ctx)) *. 1000.
          |> Int.of_float)
        ~history:all
        ~available_tools:tools
        ~on_runtime_request:(function
          | Chat_response.Moderation.Runtime_request.Request_turn | Request_compaction ->
            stop (Error (invalid "selected moderator requested a mutable runtime action"))
          | End_session _ -> ())
        ()
    in
    (match drained with
     | Error message -> stop (Error (invalid message))
     | Ok drained ->
       (match drained.stop with
        | Quiescent -> all
        | Halted | End_session_requested _ ->
          stop (Ok (Prepared.Materialized.completion_text materialized all))
        | Paused -> stop (Error (invalid "selected moderator is paused"))
        | Waiting_for_ui _ ->
          stop (Error (invalid "selected moderator is waiting for UI input"))
        | Event_limit_reached ->
          stop (Error (invalid "selected moderator event limit reached"))))
;;

let rec run_child
          ~root
          ~runtime_policy
          ~authorize_for_runtime
          ~prepare_output
          ~prepared
          ~child
          ~budget
          ~target
          ~caller_revision
          ~startup
          ~ctx
          ~input
          ~observer
          ~source
          ~history_compaction
          ~selected_session_id
  =
  let authorize = authorize_for_runtime (Live.Child_prepared.runtime prepared) in
  let revision = Live.Child_prepared.revision prepared in
  let context = { prepared; child; budget; target } in
  Eio.Fiber.with_binding child_key context (fun () ->
    let* parsed =
      Revision.parser_context revision
      |> Result.of_option ~error:(invalid "selected child has no retained parser context")
    in
    let check_current () =
      let* current = Live.Child_prepared.refresh prepared in
      Live.Child_prepared.Admission.check_now current
    in
    let check_inference_current () =
      let* current = Live.Child_prepared.refresh_current_borrow prepared in
      Live.Child_prepared.Admission.check_now current
    in
    let require_current () =
      match check_inference_current () with
      | Ok () -> ()
      | Error _ -> raise Chat_response.Ctx.Inference_admission_rejected
    in
    let guarded_ctx =
      let submission =
        Inference_runtime.Submission_guard.create ~refresh:(fun () ->
          let current =
            match Live.Child_prepared.refresh_current_borrow prepared with
            | Ok current -> current
            | Error _ -> raise Chat_response.Ctx.Inference_admission_rejected
          in
          fun () ->
            match Live.Child_prepared.Admission.check_now current with
            | Ok () -> ()
            | Error _ -> raise Chat_response.Ctx.Inference_admission_rejected)
      in
      let ctx = Chat_response.Ctx.with_submission_guard ctx submission in
      Chat_response.Ctx.with_inference_attempt_guard ctx ~before_attempt:(fun _ ->
        require_current ())
    in
    let prompt_dir =
      Eio.Path.(
        Revision.materialized_tree revision
        / Filename.dirname (Revision.root_relative_path revision))
    in
    let* tools, tool_tbl = tool_table ~root ~prepared ~child ~authorize ~prepare_output in
    let* borrowed = Native_borrow.borrow_selected () in
    let parent = H.context (Native_borrow.selected_borrowed_invocation borrowed) in
    let* source_mode =
      if Live.Child_prepared.needs_dynamic_source prepared
      then
        let* () =
          if Option.is_some (Live.Child_prepared.content_limits prepared)
          then Ok ()
          else Error (invalid "dynamic child content limits are not configured")
        in
        let* source = Live.Child_prepared.content_source prepared in
        Ok (Content.Selected source)
      else Ok Content.Captured_only
    in
    (* Content materialization can execute nested agents. Their runtime belongs to
       the invocation deadline, not the already completed compilation phase.
       Content projectors retain their own source, authority and aggregate limits. *)
    let checkpoint () =
      Eio.Fiber.check ();
      match parent.deadline with
      | Some deadline when P.Timestamp.compare deadline (O.native_now root) <= 0 ->
        raise (Tool_denied (invalid "developer invocation deadline has expired"))
      | Some _ | None -> ()
    in
    let remote = Live.Child_prepared.remote_content prepared in
    let* declaration =
      Content.create
        ~budget
        ~revision
        ~source:source_mode
        ~side:Content_source.Declaring
        ~remote
        ~check_current
        ~checkpoint
    in
    let* invocation =
      Content.create
        ~budget
        ~revision:caller_revision
        ~source:source_mode
        ~side:Content_source.Caller
        ~remote
        ~check_current
        ~checkpoint
    in
    let* select =
      match target with
      | Content_target.Tool _ -> Ok Prepared.select_prepared
      | Content_target.Model _ -> Ok Prepared.select_prepared_model
      | Content_target.Saved _ | Content_target.Content _ -> Ok Prepared.select_prepared
    in
    let selected_inputs =
      select
        ~ctx:guarded_ctx
        ~prompt_dir
        ~parsed
        ?document_projection:(Revision.document_projection revision)
        ~input
        ()
    in
    let run_selected selected_inputs =
      let stop_reason = Selected_stop () in
      let stop_result = ref None in
      let stop result =
        stop_result := Some result;
        raise stop_reason
      in
      let execute ~prepared_inputs ~model_executor =
        try
          let ctx = Prepared.context prepared_inputs in
          let config = Prepared.config prepared_inputs in
          let session_id =
            Option.value
              selected_session_id
              ~default:(Option.value config.id ~default:"nested-agent")
          in
          let agent side declaration =
            Live.Child_prepared.run_content_agent prepared ~side ~declaration ~ctx
          in
          let* materialized =
            Prepared.materialize_with_projectors
              prepared_inputs
              ~session_id
              ~source
              ~project_declaration:
                (Content.to_items declaration ~agent:(agent Content_source.Declaring))
              ~project_invocation:
                (Content.to_items invocation ~agent:(agent Content_source.Caller))
              ()
          in
          let history = Prepared.Materialized.initial_history materialized in
          let allocator = Prepared.Materialized.allocator materialized in
          let response_dir = Live.Child_prepared.response_dir prepared in
          let parent_call_id = P.Id.Invocation.to_string parent.id in
          let all =
            match model_executor with
            | None ->
              Driver.run_entries
                ~ctx
                ~allocator
                ~tools
                ~history_compaction
                ~response_dir
                ?observer
                ~source
                ~parent_call_id
                ~tool_tbl
                history
            | Some executor ->
              run_selected_moderator
                { prepared
                ; runtime_policy
                ; ctx
                ; executor
                ; session_id
                ; materialized
                ; tools
                ; tool_tbl
                ; history_compaction
                ; observer
                ; source
                ; parent_call_id
                }
                ~stop_reason
                ~stop_result
          in
          Ok (Prepared.Materialized.completion_text materialized all)
        with
        | Tool_denied error -> Error error
      in
      if
        List.exists (Revision.elements revision) ~f:(function
          | CM.Script _ -> true
          | _ -> false)
      then (
        let executor = ref None in
        let initial_config = Prepared.config selected_inputs in
        let session_id =
          Option.value
            selected_session_id
            ~default:(Option.value initial_config.id ~default:"nested-agent")
        in
        match
          Eio.Switch.run (fun sw ->
            let source_port =
              selected_model_source
                ~root
                ~runtime_policy
                ~authorize_for_runtime
                ~prepare_output
                ~prepared
                ~child
                ~budget
                ~startup
            in
            let selected =
              Model_executor.create_selected
                ~sw
                ~ctx:(Prepared.context selected_inputs)
                ~source:source_port
                ()
            in
            executor := Some selected;
            let recipe = Model_executor.recipe_agent_prompt_v1 selected ~session_id () in
            let prepared_inputs = Prepared.with_model_recipe selected_inputs recipe in
            match execute ~prepared_inputs ~model_executor:(Some selected) with
            | Ok value -> Ok value
            | Error error -> stop (Error error))
        with
        | result -> result
        | exception exn when phys_equal exn stop_reason ->
          let selected = Option.value_exn !executor in
          (match
             Model_executor.audit_joined_session
               selected
               ~session_id
               ~allowed_cancelled_reason:stop_reason
           with
           | Error message -> Error (invalid message)
           | Ok () -> Option.value_exn !stop_result))
      else execute ~prepared_inputs:selected_inputs ~model_executor:None
    in
    O.Model_inference.with_selected_child_target
      root
      ~target
      ~selected_context:(Prepared.context selected_inputs).inference_context
      ~f:(fun inference_context ->
        let selected_inputs =
          Prepared.with_inference_context selected_inputs ~inference_context
        in
        let execution = { child = context; ctx = Prepared.context selected_inputs } in
        Eio.Fiber.with_binding execute_context_key execution (fun () ->
          run_selected selected_inputs)))

and selected_model_source
      ~root
      ~runtime_policy
      ~authorize_for_runtime
      ~prepare_output
      ~prepared
      ~child
      ~budget
      ~startup
  =
  Model_executor.Selected_source.create ~with_selected:(fun request ~f ->
    if not (Model_request.is_local request)
    then Error "selected Model requires a local source member"
    else (
      let result =
        Live.Child_prepared.with_selected_model_member
          prepared
          ~budget
          ~side:Content_source.Declaring
          ~reference:(Model_request.prompt request)
          ~mode:(Model_request.source_mode request)
          ~checkpoint:(fun () -> Chat_response.Startup_compilation.checkpoint startup)
          ~f:(fun target ->
            let config =
              Chat_response.Config.of_elements
                (Revision.elements (Model_target.revision target))
            in
            let used = Atomic.make false in
            let run ~ctx ~input ~history_compaction ~session_id =
              if Atomic.exchange used true
              then Error "selected Model child run was already consumed"
              else
                Live.with_selected_model_child
                  ~parent:prepared
                  ~target
                  ~startup
                  ~input
                  ~f:(fun selected ->
                    N.Child.with_model_target
                      ~parent:child
                      ~target
                      ~prepared:selected
                      (fun child ->
                         let open Result.Let_syntax in
                         let%bind () =
                           Content.Budget.admit_revisions
                             budget
                             ~declaration:(Model_target.revision target)
                             ~caller:(Model_target.caller_revision target)
                         in
                         run_child
                           ~root
                           ~runtime_policy
                           ~authorize_for_runtime
                           ~prepare_output
                           ~prepared:selected
                           ~child
                           ~budget
                           ~target:(Content_target.Model target)
                           ~caller_revision:(Model_target.caller_revision target)
                           ~startup
                           ~ctx
                           ~input
                           ~observer:None
                           ~source:(Model_request.prompt request)
                           ~history_compaction
                           ~selected_session_id:(Some session_id)))
                |> Result.map_error ~f:(fun error -> error.message)
            in
            Ok (f ~config ~run))
      in
      match result with
      | Error error -> Error error.message
      | Ok value -> value))
;;

let run_selected
      ~live
      ~root
      ~native
      ~runtime_policy
      ~authorize_for_runtime
      ~prepare_output
      ~target
      ~startup
      ~compilation_limits
      ~programs
      ~ctx
      ~input
      ~observer
      ~source
  =
  Live.with_saved_child
    live
    ~root
    ~target
    ~startup
    ~compilation_limits
    ~programs
    ~input
    ~f:(fun prepared ->
      N.Child.with_target ~parent:native ~target ~prepared (fun child ->
        let* limits =
          Content.Limits.for_host (Live.Child_prepared.content_limits prepared)
          |> Result.map_error ~f:(fun _ ->
            invalid "selected child content limits are invalid")
        in
        let* budget =
          Content.Budget.create
            ~limits
            ~declaration:(Saved.Target.revision target)
            ~caller:(Some (Saved.Target.caller_revision target))
        in
        run_child
          ~root
          ~runtime_policy
          ~authorize_for_runtime
          ~prepare_output
          ~prepared
          ~child
          ~budget
          ~target:(Content_target.Saved target)
          ~caller_revision:(Saved.Target.caller_revision target)
          ~startup
          ~ctx
          ~input
          ~observer
          ~source
          ~history_compaction:false
          ~selected_session_id:None))
;;

let create ~live ~root ~native ~runtime_policy ~authorize_for_runtime ~prepare_output =
  if not (N.issued_by_root native root)
  then Error (invalid "saved child runner belongs to another physical Root")
  else
    Ok
      (run_selected
         ~live
         ~root
         ~native
         ~runtime_policy
         ~authorize_for_runtime
         ~prepare_output
       : Developer_agent_source_runtime.Selected_target.runner)
;;

let create_content
      ~live
      ~root
      ~native
      ~runtime_policy
      ~authorize_for_runtime
      ~prepare_output
  =
  if not (N.issued_by_root native root)
  then Error (invalid "content child runner belongs to another physical Root")
  else
    Ok
      (fun ~target ~startup ~compilation_limits ~programs ~ctx ~input ~source ->
        let* parent =
          Eio.Fiber.get child_key
          |> Result.of_option
               ~error:(invalid "nested Agent content has no selected child parent")
        in
        let* () =
          if Content_target.matches_parent target parent.target
          then Ok ()
          else Error (invalid "nested Agent content has a foreign child parent")
        in
        let* _ = N.Child.current_capabilities parent.child in
        Live.with_selected_content_child
          ~parent:parent.prepared
          ~target
          ~startup
          ~compilation_limits
          ~programs
          ~input
          ~f:(fun prepared ->
            N.Child.with_content_target
              ~parent:parent.child
              ~target
              ~prepared
              (fun child ->
                 let* () =
                   Content.Budget.admit_revisions
                     parent.budget
                     ~declaration:(Content_target.revision target)
                     ~caller:(Content_target.caller_revision target)
                 in
                 run_child
                   ~root
                   ~runtime_policy
                   ~authorize_for_runtime
                   ~prepare_output
                   ~prepared
                   ~child
                   ~budget:parent.budget
                   ~target:(Content_target.Content target)
                   ~caller_revision:(Content_target.caller_revision target)
                   ~startup
                   ~ctx
                   ~input
                   ~observer:None
                   ~source
                   ~history_compaction:false
                   ~selected_session_id:None)))
;;

let create_tool ~live ~root ~native ~runtime_policy ~authorize_for_runtime ~prepare_output
  =
  if not (N.issued_by_root native root)
  then Error (invalid "ordinary Agent Tool runner belongs to another physical Root")
  else (
    let borrow_child_context ~implementation ~invocation:_ f =
      let* parent =
        Eio.Fiber.get child_key
        |> Result.of_option
             ~error:(invalid "ordinary Agent Tool has no selected child parent")
      in
      let* execution =
        Eio.Fiber.get execute_context_key
        |> Result.of_option
             ~error:(invalid "ordinary Agent Tool has no active child execution")
      in
      let* () =
        if
          phys_equal execution.child parent
          && Live.Child_prepared.issued_by_live parent.prepared live
        then Ok ()
        else Error (invalid "ordinary Agent Tool has a foreign child execution")
      in
      let check_current () =
        let* borrowed = Native_borrow.borrow_selected () in
        let* () = Native_borrow.check_selected_source borrowed ~implementation in
        let* admission =
          Live.Child_prepared.refresh_descendant parent.prepared ~child:borrowed
        in
        let* () = Live.Child_prepared.Admission.check_now admission in
        let* inference = O.Model_inference.current_selected_context root in
        let* () =
          if phys_equal inference execution.ctx.inference_context
          then Ok ()
          else Error (invalid "ordinary Agent Tool has a foreign inference context")
        in
        Live.Child_prepared.check_tool_depth parent.prepared
      in
      let* () = check_current () in
      let value = f execution.ctx in
      let* () = check_current () in
      Ok value
    in
    let run_selected_tool
          ~target
          ~startup
          ~compilation_limits
          ~programs
          ~ctx
          ~input
          ~observer
          ~source
      =
      let* parent =
        Eio.Fiber.get child_key
        |> Result.of_option
             ~error:(invalid "ordinary Agent Tool has no selected child parent")
      in
      let* () =
        if Live.Child_prepared.issued_by_live parent.prepared live
        then Ok ()
        else Error (invalid "ordinary Agent Tool has a foreign Live package")
      in
      let* _ = N.Child.current_capabilities parent.child in
      let expected_parent =
        match parent.target with
        | Saved target -> Content_target.Saved target
        | Content target -> Content_target.Content target
        | Model target -> Content_target.Model target
        | Tool target -> Content_target.Tool target
      in
      let* () =
        if Tool_target.matches_parent target expected_parent
        then Ok ()
        else Error (invalid "ordinary Agent Tool has a foreign immediate parent")
      in
      let* () = Tool_target.check target in
      let* borrowed = Native_borrow.borrow_selected () in
      let* admission =
        Live.Child_prepared.refresh_descendant parent.prepared ~child:borrowed
      in
      let* () = Live.Child_prepared.Admission.check_now admission in
      Live.with_selected_tool_child
        ~parent:parent.prepared
        ~target
        ~startup
        ~compilation_limits
        ~programs
        ~input
        ~f:(fun prepared ->
          N.Child.with_tool_target ~parent:parent.child ~target ~prepared (fun child ->
            let* () =
              Content.Budget.admit_revisions
                parent.budget
                ~declaration:(Tool_target.revision target)
                ~caller:(Tool_target.caller_revision target)
            in
            run_child
              ~root
              ~runtime_policy
              ~authorize_for_runtime
              ~prepare_output
              ~prepared
              ~child
              ~budget:parent.budget
              ~target:(Content_target.Tool target)
              ~caller_revision:(Tool_target.caller_revision target)
              ~startup
              ~ctx
              ~input
              ~observer
              ~source
              ~history_compaction:false
              ~selected_session_id:None))
    in
    Ok (Live.Selected_tool_callbacks.create ~borrow_child_context ~run_selected_tool))
;;
