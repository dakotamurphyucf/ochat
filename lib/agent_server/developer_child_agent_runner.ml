open! Core
module P = Agent_protocol
module H = P.Host_invocation
module O = Developer_root_owner
module N = Developer_native_executor
module Live = Developer_live_resources
module Saved = Agent_session.Captured_agent_runner.Claimed_developer.Selected
module Content_target = Agent_session.Captured_agent_runner.Selected_content_target
module Native_borrow = Agent_session.Native_tool_invocation
module Revision = Agent_session.Prompt_revision
module C = Chat_response.Tool_capability
module M = Chat_response.Managed_tool_registry
module Driver = Chat_response.Driver
module Prepared = Driver.Prepared_inputs
module Content = Developer_child_content_projection
module Content_source = Agent_session.Captured_agent_runner.Source_binding.Content_source
module Output = Openai.Responses.Tool_output.Output

exception Tool_denied of P.Error.t

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

let run_child
      ~root
      ~authorize
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
  =
  let revision = Live.Child_prepared.revision prepared in
  let context = { prepared; child; budget; target } in
  Eio.Fiber.with_binding child_key context (fun () ->
    let* parsed =
      Revision.parser_context revision
      |> Result.of_option ~error:(invalid "selected child has no retained parser context")
    in
    let prompt_dir =
      Eio.Path.(
        Revision.materialized_tree revision
        / Filename.dirname (Revision.root_relative_path revision))
    in
    let prepared_inputs =
      Prepared.select_prepared
        ~ctx
        ~prompt_dir
        ~parsed
        ?document_projection:(Revision.document_projection revision)
        ~input
        ()
    in
    let ctx = Prepared.context prepared_inputs in
    let* tools, tool_tbl = tool_table ~root ~prepared ~child ~authorize ~prepare_output in
    let* borrowed = Native_borrow.borrow_selected () in
    let parent = H.context (Native_borrow.selected_borrowed_invocation borrowed) in
    let config = Prepared.config prepared_inputs in
    let session_id = Option.value config.id ~default:"nested-agent" in
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
    let check_current () =
      let* current = Live.Child_prepared.refresh prepared in
      Live.Child_prepared.Admission.check_now current
    in
    let checkpoint () = Chat_response.Startup_compilation.checkpoint startup in
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
    let result =
      try
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
        let all =
          Driver.run_entries
            ~ctx
            ~allocator:(Prepared.Materialized.allocator materialized)
            ~tools
            ~response_dir:(Live.Child_prepared.response_dir prepared)
            ?observer
            ~source
            ~parent_call_id:(P.Id.Invocation.to_string parent.id)
            ~tool_tbl
            history
        in
        Ok (Prepared.Materialized.completion_text materialized all)
      with
      | Tool_denied error -> Error error
    in
    result)
;;

let run_selected
      ~live
      ~root
      ~native
      ~authorize
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
          ~authorize
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
          ~source))
;;

let create ~live ~root ~native ~authorize ~prepare_output =
  if not (N.issued_by_root native root)
  then Error (invalid "saved child runner belongs to another physical Root")
  else
    Ok
      (run_selected ~live ~root ~native ~authorize ~prepare_output
       : Developer_agent_source_runtime.Selected_target.runner)
;;

let create_content ~live ~root ~native ~authorize ~prepare_output =
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
                   ~authorize
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
                   ~source)))
;;
