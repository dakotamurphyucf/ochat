open Core
module Owner = Prompt_source_owner
module Builder = Prompt_revision_builder
module CM = Prompt.Chat_markdown
module Plan = Chat_response.Script_contract_plan
module C = Chatml_prompt_programs
module P = Chatml_prepared_program
module S = Chatml_source
module Input = Chatmd_parser_input
module Prepare = Chatml_project_prepare

module Context = struct
  type t =
    { parsed : CM.Parsed.t
    ; mutable programs : Plan.Program.t list
    ; mutable projects : bool
    }
end

module Row = struct
  type t =
    { program : Plan.Program.t
    ; source : S.t option
    ; supplied : P.t option
    ; request : Prepare.Request.t option
    ; active : bool
    }
end

let bytes_equal left right ~checkpoint =
  let length = String.length left in
  let equal = ref (Int.equal length (String.length right)) in
  let offset = ref 0 in
  while !equal && !offset < length do
    checkpoint ();
    let stop = !offset + Int.min 4096 (length - !offset) in
    while !equal && !offset < stop do
      equal := Char.equal left.[!offset] right.[!offset];
      incr offset
    done
  done;
  checkpoint ();
  !equal
;;

let binding_equal left right ~checkpoint =
  phys_equal left right
  || (Int.equal (C.Binding.start_byte left) (C.Binding.start_byte right)
      && Int.equal (C.Binding.end_byte left) (C.Binding.end_byte right)
      && S.Digest.equal (C.Binding.script_digest left) (C.Binding.script_digest right)
      && bytes_equal (C.Binding.script_id left) (C.Binding.script_id right) ~checkpoint
      && String.equal
           (Input.identity_digest (C.Binding.parser_input left) ~checkpoint)
           (Input.identity_digest (C.Binding.parser_input right) ~checkpoint)
      && String.equal
           (Input.identity_digest (C.Binding.registration_input left) ~checkpoint)
           (Input.identity_digest (C.Binding.registration_input right) ~checkpoint))
;;

let capture
      scope
      ~capture
      ~root_plan
      ~role_plans
      ~operation
      ~provided
      ~additional_anchors
      ~git_store
      ~git_grants
      ~root_limits
      ~max_grants
      ~max_grant_bytes
      ~max_request_bytes
      ~max_path_components
      ~max_input_bytes
      ~max_work_steps
      ~policy
      ~limits
      ~custody_limits
      ~clock
      ~checkpoint
  =
  let module Abort = struct
    exception Stop of Builder.Diagnostic.t list
  end
  in
  let fail ?source code message =
    raise (Abort.Stop [ Builder.Diagnostic.{ code; message; source } ])
  in
  let owner_error error =
    match Owner.Error.capture_diagnostics error with
    | Some errors -> raise (Abort.Stop errors)
    | None ->
      fail "prompt.program_source_failed" "Original source admission is unavailable"
  in
  let remaining_work = ref max_work_steps in
  let poll () =
    Eio.Fiber.check ();
    (match Owner.Scope.check_capture scope capture with
     | Ok () -> ()
     | Error error -> owner_error error);
    if !remaining_work <= 0
    then
      fail "prompt.program_source_limit" "Script source helper work allowance exhausted";
    decr remaining_work;
    checkpoint ();
    Eio.Fiber.check ();
    match Owner.Scope.check scope with
    | Ok () -> ()
    | Error error -> owner_error error
  in
  let admit result =
    match result with
    | Ok value -> value
    | Error _ ->
      fail "prompt.program_source_invalid" "Invalid script source preparation input"
  in
  let remaining = ref max_input_bytes in
  let debit bytes =
    poll ();
    if bytes < 0 || bytes > !remaining
    then fail "prompt.program_source_limit" "Script source input allowance exhausted";
    remaining := !remaining - bytes
  in
  let value = Owner.Capture.value capture in
  let root = Builder.Capture.root value in
  let contexts = root :: List.map (Builder.Capture.agents value) ~f:snd in
  let root_input = CM.Parsed.root_input root in
  let raw_sources =
    (Input.path root_input, Input.authored root_input) :: Builder.Capture.sources value
  in
  let namespace =
    "chatmd."
    ^ Agent_store.Prompt_source_receipt.fingerprint (Owner.Capture.receipt capture)
  in
  let used_provided = ref [] in
  let supplied program =
    let matches =
      List.filter provided ~f:(fun (binding, graph) ->
        poll ();
        binding_equal binding (Plan.Program.binding program) ~checkpoint:poll
        && S.Digest.equal
             (P.compiler_contract graph)
             (Plan.Program.compiler_contract program))
    in
    match matches with
    | [] -> None
    | [ pair ] ->
      let _, graph = pair in
      used_provided := pair :: !used_provided;
      Some graph
    | _ :: _ :: _ ->
      fail "prompt.program_source_duplicate" "Duplicate supplied binding and contract"
  in
  let source program =
    let declaration = Plan.Program.declaration program in
    let input = Input.Range.input (CM.Declaration.range declaration) in
    let binding = Plan.Program.binding program in
    let relative_path =
      match CM.Declaration.script_path declaration with
      | None ->
        Printf.sprintf
          "fragments/%s/%d-%d.chatml"
          (Input.identity_digest input ~checkpoint:poll)
          (C.Binding.start_byte binding)
          (C.Binding.end_byte binding)
      | Some reference ->
        let route = admit (Owner.Capture.route capture input ~checkpoint:poll) in
        let member =
          admit
            (Source_loader.resolve_member
               (Owner.Capture.Route.member route)
               ~reference
               ~max_reference_bytes:1024
               ~checkpoint:poll)
        in
        let path = Source.Portable_path.to_string member in
        let raw =
          List.find_map raw_sources ~f:(fun (candidate, contents) ->
            poll ();
            if bytes_equal candidate path ~checkpoint:poll then Some contents else None)
        in
        (match raw with
         | Some contents
           when bytes_equal
                  contents
                  (CM.Declaration.script_body declaration)
                  ~checkpoint:poll -> ()
         | None | Some _ ->
           fail
             ~source:(Input.path input)
             "prompt.program_source_conflict"
             "External script differs from its original captured member");
        path
    in
    let id = admit (S.Id.create ~package_namespace:namespace ~relative_path) in
    let contents = CM.Declaration.script_body declaration in
    debit (String.length contents);
    admit
      (S.create
         id
         ~contents
         ~expected_digest:(Some (C.Binding.script_digest binding))
         ~max_bytes:None
         ~checkpoint:poll)
  in
  let check_graph program graph =
    let binding = Plan.Program.binding program in
    debit (P.Accounting.input_bytes (P.accounting graph));
    let entry_id = P.entry graph in
    let entry =
      List.find (P.sources graph) ~f:(fun source ->
        poll ();
        let id = S.id source in
        String.equal (S.Id.relative_path id) (S.Id.relative_path entry_id)
        && bytes_equal
             (S.Id.package_namespace id)
             (S.Id.package_namespace entry_id)
             ~checkpoint:poll)
      |> function
      | Some entry -> entry
      | None -> fail "prompt.program_source_invalid" "Provided graph lacks entry"
    in
    if
      (not (S.Digest.equal (S.digest entry) (C.Binding.script_digest binding)))
      || not
           (bytes_equal
              (S.contents entry)
              (CM.Declaration.script_body (Plan.Program.declaration program))
              ~checkpoint:poll)
    then
      fail
        "prompt.program_source_conflict"
        "Provided graph entry differs from declaration"
  in
  let implicit program source =
    let id = S.id source in
    let scope =
      admit (P.Scope.create id ~bindings:[] ~selected_at:None ~checkpoint:poll)
    in
    let contract = Plan.Program.compiler_contract program in
    let graph =
      admit
        (P.create
           ~max_input_bytes:(Some !remaining)
           ~compiler_contract:contract
           ~environment:(P.Environment.implicit ~required_contract:contract)
           ~provenance:Implicit
           ~entry:id
           ~sources:[ source ]
           ~definitions:[]
           ~scopes:[ scope ]
           ~checkpoint:poll
           ())
    in
    debit (P.Accounting.input_bytes (P.accounting graph));
    graph
  in
  try
    poll ();
    if max_input_bytes <= 0 || max_path_components <= 0 || max_request_bytes <= 0
    then fail "prompt.program_source_limit" "Script source ceilings must be positive";
    let grouped = ref [] in
    let program_count = ref 0 in
    if not (phys_equal (Plan.parsed root_plan) root)
    then fail "prompt.program_source_owner" "Current Root role plan is required";
    let add_plan plan =
      poll ();
      let parsed = Plan.parsed plan in
      if
        not
          (List.exists contexts ~f:(fun context ->
             poll ();
             phys_equal parsed context))
      then fail "prompt.program_source_owner" "Plan belongs to another parser capture";
      let context =
        match
          List.find !grouped ~f:(fun context ->
            poll ();
            phys_equal context.Context.parsed parsed)
        with
        | Some context -> context
        | None ->
          let context = Context.{ parsed; programs = []; projects = false } in
          grouped := context :: !grouped;
          context
      in
      context.projects <- context.projects || Plan.has_projects plan;
      List.iter (Plan.programs plan) ~f:(fun program ->
        poll ();
        let duplicate =
          List.exists context.programs ~f:(fun previous ->
            poll ();
            phys_equal
              (Plan.Program.declaration program)
              (Plan.Program.declaration previous)
            && S.Digest.equal
                 (Plan.Program.compiler_contract program)
                 (Plan.Program.compiler_contract previous))
        in
        if not duplicate
        then (
          if !program_count = Int.max_value
          then fail "prompt.program_source_limit" "Program count overflow";
          incr program_count;
          context.programs <- program :: context.programs))
    in
    add_plan root_plan;
    List.iter role_plans ~f:add_plan;
    let grouped = List.rev !grouped in
    ignore
      (List.fold provided ~init:0 ~f:(fun count _ ->
         poll ();
         if count >= !program_count
         then fail "prompt.program_source_invalid" "Surplus supplied program inventory";
         count + 1)
       : int);
    let admitted_contexts =
      List.map grouped ~f:(fun context ->
        poll ();
        let programs =
          List.map (List.rev context.Context.programs) ~f:(fun program ->
            poll ();
            program, supplied program)
        in
        let selected =
          context.projects
          || List.exists programs ~f:(fun (_, graph) -> Option.is_some graph)
        in
        context, if selected then programs else [])
    in
    let unique = ref 0 in
    List.iter admitted_contexts ~f:(fun (_, programs) ->
      List.iter programs ~f:(fun _ ->
        poll ();
        if !unique >= C.Limits.max_programs custody_limits
        then fail "prompt.program_source_limit" "Captured program ceiling exceeded";
        incr unique));
    let rows =
      List.concat_map admitted_contexts ~f:(fun (context, programs) ->
        List.map programs ~f:(fun (program, supplied) ->
          poll ();
          let declaration = Plan.Program.declaration program in
          let source, request =
            match CM.Declaration.project_selection declaration, supplied with
            | Some _, Some _ ->
              fail
                "prompt.program_source_invalid"
                "Provided graph cannot replace selected project"
            | None, Some graph ->
              check_graph program graph;
              None, None
            | selection, None ->
              let source = source program in
              let request =
                match selection with
                | None -> None
                | Some _ ->
                  let route =
                    admit
                      (Prompt_project_route.create
                         capture
                         ~declaration
                         ~max_locator_bytes:max_request_bytes
                         ~max_path_components
                         ~checkpoint:poll)
                  in
                  let entry =
                    admit
                      (Prompt_project_route.entry
                         route
                         ~source
                         ~additional_anchors
                         ~root_limits
                         ~checkpoint:poll)
                  in
                  let lock_id =
                    admit
                      (S.Id.create
                         ~package_namespace:namespace
                         ~relative_path:
                           (Printf.sprintf
                              "locks/%s-%d-%s.json"
                              (Input.identity_digest
                                 (C.Binding.registration_input
                                    (Plan.Program.binding program))
                                 ~checkpoint:poll)
                              (C.Binding.start_byte (Plan.Program.binding program))
                              (S.Digest.to_hex (Plan.Program.compiler_contract program))))
                  in
                  poll ();
                  let operation = operation program in
                  poll ();
                  Some
                    (admit
                       (Prepare.Request.create_with_entry
                          (Prompt_project_route.root route)
                          ~operation
                          ~selection:
                            (Library
                               { environment = Prompt_project_route.environment route })
                          ~entry
                          ~contract:(Plan.Program.contract program)
                          ~lock_id
                          ~max_request_bytes))
              in
              Some source, request
          in
          Row.
            { program
            ; source
            ; supplied
            ; request
            ; active =
                phys_equal context.Context.parsed root
                && List.exists (Plan.programs root_plan) ~f:(fun active ->
                  poll ();
                  phys_equal (Plan.Program.declaration active) declaration
                  && S.Digest.equal
                       (Plan.Program.compiler_contract active)
                       (Plan.Program.compiler_contract program))
            }))
    in
    List.iter provided ~f:(fun pair ->
      poll ();
      if not (List.exists !used_provided ~f:(phys_equal pair))
      then fail "prompt.program_source_invalid" "Unused supplied binding and contract");
    let requests =
      List.filter_map rows ~f:(fun row ->
        poll ();
        row.Row.request)
    in
    let selected =
      match requests with
      | [] -> []
      | _ :: _ ->
        (match
           Owner.Scope.prepare_sources
             scope
             ~capture
             ~requests
             ~additional_anchors
             ~git_store
             ~git_grants
             ~root_limits
             ~max_grants
             ~max_grant_bytes
             ~policy
             ~limits
             ~clock
             ~checkpoint:poll
         with
         | Error error -> owner_error error
         | Ok batch -> Prepare.Source_batch.preparations batch)
    in
    let selected = ref selected in
    let programs, active =
      List.fold rows ~init:([], []) ~f:(fun (programs, active) row ->
        poll ();
        let input =
          match row.Row.request, row.supplied, row.source with
          | Some _, None, Some _ ->
            (match !selected with
             | [] -> fail "prompt.program_source_invalid" "Incomplete source batch"
             | preparation :: rest ->
               selected := rest;
               C.Input.Selected (Prepare.Source_preparation.build preparation))
          | None, Some graph, None -> C.Input.Provided graph
          | None, None, Some source -> C.Input.Provided (implicit row.program source)
          | Some _, Some _, _
          | Some _, None, None
          | None, Some _, Some _
          | None, None, None ->
            fail "prompt.program_source_invalid" "Inconsistent source preparation row"
        in
        let binding = Plan.Program.binding row.program in
        let active =
          if row.active
          then (binding, Plan.Program.compiler_contract row.program) :: active
          else active
        in
        (binding, input) :: programs, active)
    in
    if not (List.is_empty !selected)
    then fail "prompt.program_source_invalid" "Surplus source batch";
    let authored =
      admit
        (Prompt_program_custody.authored
           ~root_file:(Input.path root_input)
           ~sources:raw_sources)
    in
    let result =
      C.capture
        authored
        ~parsed_contexts:contexts
        ~root:root_input
        ~programs:(List.rev programs)
        ~active_programs:(List.rev active)
        ~limits:custody_limits
        ~checkpoint:poll
      |> Result.map_error ~f:(fun _ ->
        [ Builder.Diagnostic.
            { code = "prompt.program_custody_failed"
            ; message = "Script programs exceed or conflict with captured custody"
            ; source = Some (Input.path root_input)
            }
        ])
    in
    poll ();
    result
  with
  | Abort.Stop errors -> Error errors
;;
