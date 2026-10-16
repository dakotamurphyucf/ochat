open! Core
module P = Agent_protocol
module H = P.Host_invocation
module O = Developer_root_owner
module N = Developer_native_executor
module Live = Developer_live_resources
module Saved = Agent_session.Captured_agent_runner.Claimed_developer.Selected
module Revision = Agent_session.Prompt_revision
module C = Chat_response.Tool_capability
module M = Chat_response.Managed_tool_registry
module Driver = Chat_response.Driver
module Prepared = Driver.Prepared_inputs
module Output = Openai.Responses.Tool_output.Output

exception Tool_denied of P.Error.t
exception Content_denied

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

let tool_table ~root ~target ~prepared ~child ~authorize ~prepare_output =
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
        let* invocation =
          invocation
            ~root
            ~parent:(Saved.invocation (Saved.Target.saved target))
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
        let revision = Saved.Target.revision target in
        let* parsed =
          Revision.parser_context revision
          |> Result.of_option
               ~error:(invalid "selected child has no retained parser context")
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
        let* tools, tool_tbl =
          tool_table ~root ~target ~prepared ~child ~authorize ~prepare_output
        in
        let parent = H.context (Saved.invocation (Saved.Target.saved target)) in
        let config = Prepared.config prepared_inputs in
        let session_id = Option.value config.id ~default:"nested-agent" in
        let result =
          try
            let materialized =
              Prepared.materialize
                prepared_inputs
                ~session_id
                ~source
                ~run_agent:(fun ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ ->
                  raise Content_denied)
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
          | Content_denied ->
            Error (invalid "selected developer child contains unsupported content")
        in
        result))
;;

let create ~live ~root ~native ~authorize ~prepare_output =
  if not (N.issued_by_root native root)
  then Error (invalid "saved child runner belongs to another physical Root")
  else
    Ok
      (run_selected ~live ~root ~native ~authorize ~prepare_output
       : Developer_agent_source_runtime.Selected_target.runner)
;;
