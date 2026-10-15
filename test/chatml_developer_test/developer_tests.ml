open Core
module C = Chatml_developer_capture
module T = Chatml_developer_test
module S = Chatml_source
module B = Chatml_project_build
module P = Chatml_prepared_program

let checkpoint () = ()

let json_bounds =
  C.Json_bounds.create ~max_bytes:4096 ~max_depth:32 ~max_nodes:256 |> Or_error.ok_exn
;;

let capture_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (C.Error.sexp_of_t error)
;;

let run_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (T.Error.sexp_of_t error)
;;

let build source =
  let fixture = Fixtures.make ~library:true () in
  let entry = Support.capture ~namespace:"root" "src/developer.chatml" source in
  let build = Fixtures.compose fixture ~entry:(B.Entry.Captured entry) |> Support.ok in
  build
;;

let capture ?(steps = []) ?(input = `String "hello") source =
  let build = build source in
  let graph = B.prepared build |> P.fingerprint in
  let origin =
    C.Origin.local_saved ~graph ~source_revision:"saved-1" |> Or_error.ok_exn
  in
  let fixtures =
    C.Fixture.create ~steps ~max_steps:8 ~max_bytes:16_384 ~checkpoint |> Or_error.ok_exn
  in
  match
    C.create
      ~build
      ~origin
      ~target:One_off_v1
      ~entrypoint:"main"
      ~input
      ~fixtures:(Some fixtures)
      ~json_bounds
      ~checkpoint
  with
  | Ok capture -> capture
  | Error error -> Error.raise_s (C.Error.sexp_of_t error)
;;

let step name =
  C.Fixture.Step.create
    ~tool_name:name
    ~input_schema:(Jsonaf.of_string "{}")
    ~output_schema:(Jsonaf.of_string "{}")
    ~expected_input:(`String "hello")
    ~response:(Return (`String "world"))
    ~json_bounds
    ~checkpoint
  |> Or_error.ok_exn
;;

let check env capture =
  T.Check.perform
    capture
    ~id:(T.Check.Id.create "check" |> Or_error.ok_exn)
    ~policy:(Bounded Chatml_compilation.default_limits)
    ~max_dependency_depth:16
    ~env
;;

let run env capture check id =
  match
    T.run
      capture
      ~check
      ~id:(T.Run_id.create id |> Or_error.ok_exn)
      ~limits:Chatml_execution.default_limits
      ~max_output_bytes:4096
      ~env
  with
  | Ok receipt -> receipt
  | Error error -> Error.raise_s (T.Error.sexp_of_t error)
;;

let pure = "let main = fun input -> Task.pure(input)"

let%test_unit
    "a captured standalone source keeps its host identity through check and fixture"
  =
  Eio_main.run (fun env ->
    let contract =
      Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint
      |> fun contract -> Chatml.Chatml_compiler_contract.fingerprint contract ~checkpoint
    in
    let id =
      S.Id.create ~package_namespace:"host.capture" ~relative_path:"nested/main.chatml"
      |> Or_error.ok_exn
    in
    let source =
      S.create id ~contents:pure ~expected_digest:None ~max_bytes:(Some 4096) ~checkpoint
      |> Or_error.ok_exn
    in
    let prepared =
      P.single_captured
        ~compiler_contract:contract
        ~source
        ~max_input_bytes:4096
        ~checkpoint
        ()
      |> function
      | Ok prepared -> prepared
      | Error error -> Error.raise_s (P.Error.sexp_of_t error)
    in
    assert (S.Id.equal (P.entry prepared) id);
    assert (S.Id.equal (S.id (P.find_source prepared id |> Option.value_exn)) id);
    let exact_bytes = P.Accounting.input_bytes (P.accounting prepared) in
    assert (
      Result.is_ok
        (P.single_captured
           ~compiler_contract:contract
           ~source
           ~max_input_bytes:exact_bytes
           ~checkpoint
           ()));
    (match
       P.single_captured
         ~compiler_contract:contract
         ~source
         ~max_input_bytes:(exact_bytes - 1)
         ~checkpoint
         ()
     with
     | Error { problem = Input_limit _; _ } -> ()
     | Error _ | Ok _ -> failwith "single source metadata escaped the input cap");
    let origin =
      C.Origin.local_saved
        ~graph:(P.fingerprint prepared)
        ~source_revision:(S.Digest.to_hex (S.digest source))
      |> Or_error.ok_exn
    in
    let fixtures =
      C.Fixture.create ~steps:[] ~max_steps:8 ~max_bytes:4096 ~checkpoint
      |> Or_error.ok_exn
    in
    let captured =
      C.create_prepared
        ~prepared
        ~origin
        ~target:One_off_v1
        ~entrypoint:"main"
        ~input:(`String "captured")
        ~fixtures:(Some fixtures)
        ~json_bounds
        ~checkpoint
      |> capture_ok
    in
    let checked = check env captured in
    (match T.Check.outcome checked with
     | Ready -> ()
     | Rejected _ -> failwith "standalone source did not check");
    match T.Receipt.outcome (run env captured checked "standalone") with
    | Succeeded (`String "captured") -> ()
    | Succeeded _ | Failed _ -> failwith "standalone fixture did not run")
;;

let%test_unit "an embedded source-only script keeps its parsed origin through fixture" =
  Eio_main.run (fun env ->
    let module DC = Agent_server.Authoring_source_host.Document_context in
    let module CM = Prompt.Chat_markdown in
    let module Input = Chatmd_parser_input in
    let module Context = Chatml_authoring_context in
    let id =
      S.Id.create ~package_namespace:"host.document" ~relative_path:"draft.chatmd"
      |> Or_error.ok_exn
    in
    let text =
      {|<script id="worker" language="chatml" kind="tool">let main = fun input -> Task.pure(input)</script>|}
    in
    let root =
      S.create id ~contents:text ~expected_digest:None ~max_bytes:(Some 4096) ~checkpoint
      |> Or_error.ok_exn
    in
    let parsed =
      DC.Parsed_capture.parse_root_only
        ~limits:Chatmd_source_bundle.default_limits
        ~source:root
        ~dir:(Eio.Stdenv.fs env)
        ~checkpoint
      |> function
      | Ok parsed -> parsed
      | Error error ->
        Error.raise_s (Agent_server.Authoring_source_host.Error.sexp_of_t error)
    in
    let generated = CM.Generated.parsed (DC.Parsed_capture.parsed parsed) in
    let declaration = List.hd_exn (CM.Parsed.declarations generated.parsed_root) in
    let script =
      DC.Parsed_capture.script_source
        parsed
        ~declaration
        ~max_script_bytes:4096
        ~max_locator_bytes:1024
        ~max_path_components:16
        ~checkpoint
      |> function
      | Ok script -> script
      | Error error ->
        Error.raise_s (Agent_server.Authoring_source_host.Error.sexp_of_t error)
    in
    assert (not (S.Id.equal (S.id root) (S.id script)));
    [%test_eq: string] (S.contents script) pure;
    let contract =
      Chatml.Chatml_compiler_contract.for_target One_off_v1 ~checkpoint
      |> fun contract -> Chatml.Chatml_compiler_contract.fingerprint contract ~checkpoint
    in
    let prepared =
      P.single_captured
        ~compiler_contract:contract
        ~source:script
        ~max_input_bytes:4096
        ~checkpoint
        ()
      |> function
      | Ok prepared -> prepared
      | Error error -> Error.raise_s (P.Error.sexp_of_t error)
    in
    let identity =
      Context.Identity.create
        ~principal:"test"
        ~host_incarnation:"host"
        ~connection_lease:"lease"
        ~nonce:"context"
      |> Or_error.ok_exn
    in
    let limits =
      Context.Limits.create
        ~max_contexts:1
        ~max_sources:1
        ~max_open_documents:1
        ~max_document_bytes:4096
        ~max_effective_input_bytes:8192
        ~max_context_source_bytes:8192
        ~max_total_source_bytes:8192
      |> Or_error.ok_exn
    in
    let context =
      Context.admit_document
        (Context.create ~limits)
        ~identity
        ~base:root
        ~requested_environment:None
        ~checkpoint
      |> function
      | Ok context -> context
      | Error error -> Error.raise_s (Context.Error.sexp_of_t error)
    in
    let generation =
      Context.find_document context ~identity
      |> Option.value_exn
      |> Context.Document.View.generation
    in
    let declaring_input = Input.Range.input (CM.Declaration.range declaration) in
    let declaring_source =
      DC.Parsed_capture.input_sources parsed
      |> List.find_map ~f:(fun (input, source) ->
        if phys_equal input declaring_input then Some source else None)
      |> Option.value_exn
    in
    let range = CM.Declaration.range declaration in
    let input_digest =
      Input.identity_digest declaring_input ~checkpoint
      |> S.Digest.of_hex
      |> Or_error.ok_exn
    in
    let document_origin
          ?(graph = P.fingerprint prepared)
          ?(script_source = script)
          ?(end_byte = Input.Range.end_byte range)
          ()
      =
      C.Origin.document_script
        ~graph
        ~context:identity
        ~generation
        ~root_source:root
        ~declaring_source
        ~declaring_input_digest:input_digest
        ~input_effective_bytes:(Input.effective_byte_length declaring_input)
        ~declaration_start_byte:(Input.Range.start_byte range)
        ~declaration_end_byte:end_byte
        ~script_id:(CM.Declaration.script_id declaration)
        ~script_source
        ~max_identity_bytes:16_384
        ~checkpoint
    in
    let origin = document_origin () |> Or_error.ok_exn in
    let fixtures =
      C.Fixture.create ~steps:[] ~max_steps:8 ~max_bytes:4096 ~checkpoint
      |> Or_error.ok_exn
    in
    let capture ?(prepared = prepared) origin =
      C.create_prepared
        ~prepared
        ~origin
        ~target:One_off_v1
        ~entrypoint:"main"
        ~input:(`String "embedded")
        ~fixtures:(Some fixtures)
        ~json_bounds
        ~checkpoint
    in
    let captured = capture origin |> capture_ok in
    let checked = check env captured in
    (match T.Check.outcome checked with
     | Ready -> ()
     | Rejected _ -> failwith "embedded script did not check");
    (match T.Receipt.outcome (run env captured checked "embedded") with
     | Succeeded (`String "embedded") -> ()
     | Succeeded _ | Failed _ -> failwith "embedded fixture did not run");
    let changed =
      S.create
        (S.id script)
        ~contents:"let main = fun input -> Task.fail(\"changed\")"
        ~expected_digest:None
        ~max_bytes:(Some 4096)
        ~checkpoint
      |> Or_error.ok_exn
    in
    (match capture (document_origin ~script_source:changed () |> Or_error.ok_exn) with
     | Error (Stale_source stale) when S.Id.equal stale (S.id script) -> ()
     | Error _ | Ok _ -> failwith "changed script body reused prepared source");
    let other_id =
      S.Id.create ~package_namespace:"host.document" ~relative_path:"other-entry.chatml"
      |> Or_error.ok_exn
    in
    let other =
      S.create
        other_id
        ~contents:pure
        ~expected_digest:None
        ~max_bytes:(Some 4096)
        ~checkpoint
      |> Or_error.ok_exn
    in
    let other_scope =
      P.Scope.create other_id ~bindings:[] ~selected_at:None ~checkpoint
      |> Or_error.ok_exn
    in
    let wrong_entry =
      P.create
        ~max_input_bytes:(Some 8192)
        ~compiler_contract:contract
        ~environment:(P.Environment.implicit ~required_contract:contract)
        ~provenance:Implicit
        ~entry:other_id
        ~sources:[ script; other ]
        ~definitions:[]
        ~scopes:(other_scope :: P.scopes prepared)
        ~checkpoint
        ()
      |> function
      | Ok prepared -> prepared
      | Error error -> Error.raise_s (P.Error.sexp_of_t error)
    in
    assert (Option.is_some (P.find_source wrong_entry (S.id script)));
    (match
       capture
         ~prepared:wrong_entry
         (document_origin ~graph:(P.fingerprint wrong_entry) () |> Or_error.ok_exn)
     with
     | Error Invalid_origin -> ()
     | Error _ | Ok _ -> failwith "embedded script was only a dependency, not the entry");
    assert (
      Result.is_error
        (document_origin ~end_byte:(Input.effective_byte_length declaring_input + 1) ()));
    let foreign =
      DC.Parsed_capture.parse_root_only
        ~limits:Chatmd_source_bundle.default_limits
        ~source:root
        ~dir:(Eio.Stdenv.fs env)
        ~checkpoint
      |> function
      | Ok foreign -> foreign
      | Error error ->
        Error.raise_s (Agent_server.Authoring_source_host.Error.sexp_of_t error)
    in
    let foreign_generated = CM.Generated.parsed (DC.Parsed_capture.parsed foreign) in
    let foreign_declaration =
      List.hd_exn (CM.Parsed.declarations foreign_generated.parsed_root)
    in
    match
      DC.Parsed_capture.script_source
        parsed
        ~declaration:foreign_declaration
        ~max_script_bytes:4096
        ~max_locator_bytes:1024
        ~max_path_components:16
        ~checkpoint
    with
    | Error Agent_server.Authoring_source_host.Error.Selected_script_unavailable -> ()
    | Error _ | Ok _ -> failwith "foreign parsed declaration entered source-only route")
;;

let tool =
  {|let main = fun input -> Task.bind(Tool.call("fixture", input), fun result ->
  match result with
  | `Ok(value) -> Task.pure(value)
  | `Error(message) -> Task.fail(message))|}
;;

let%expect_test "pure empty fixtures and exact ordered tool fixture both run" =
  Eio_main.run (fun env ->
    let pure_capture = capture pure in
    let pure_check = check env pure_capture in
    let pure_receipt = run env pure_capture pure_check "pure" in
    let tool_capture = capture ~steps:[ step "fixture" ] tool in
    let tool_check = check env tool_capture in
    let first = run env tool_capture tool_check "first" in
    let second = run env tool_capture tool_check "second" in
    let show receipt =
      let id = T.Receipt.id receipt in
      let matched = T.Receipt.matched_steps receipt in
      let outcome =
        match T.Receipt.outcome receipt with
        | Succeeded json -> Jsonaf.to_string json
        | Failed _ -> "failed"
      in
      print_s [%sexp (id : T.Run_id.t), (matched : int), (outcome : string)]
    in
    show pure_receipt;
    show first;
    show second);
  [%expect
    {|
    (pure 0 "\"hello\"")
    (first 1 "\"world\"")
    (second 1 "\"world\"")
    |}]
;;

let caught_wrong_tool =
  {|let main = fun input ->
  Task.bind(
    Task.catch(Tool.call("wrong", input), fun _ -> Task.pure(`Ok(input))),
    fun result -> match result with
      | `Ok(value) -> Task.pure(value)
      | `Error(message) -> Task.fail(message))|}
;;

let caught_job =
  {|let main = fun input ->
  Task.catch(Job.get("unknown"), fun _ -> Task.pure(input))|}
;;

let%test_unit "caught fixture mismatch and unsupported Job effect remain failed" =
  Eio_main.run (fun env ->
    let mismatch_capture = capture ~steps:[ step "fixture" ] caught_wrong_tool in
    let mismatch_check = check env mismatch_capture in
    (match T.Check.outcome mismatch_check with
     | Ready -> ()
     | Rejected diagnostic -> failwith (T.Diagnostic.message diagnostic));
    let mismatch = run env mismatch_capture mismatch_check "mismatch" in
    (match T.Receipt.outcome mismatch with
     | Failed (Fixture_mismatch { expected_step = 0; actual_tool }) ->
       [%test_eq: string] actual_tool "wrong"
     | _ -> failwith "caught fixture mismatch was admitted");
    let job_capture = capture caught_job in
    let job_check = check env job_capture in
    (match T.Check.outcome job_check with
     | Ready -> ()
     | Rejected diagnostic -> failwith (T.Diagnostic.message diagnostic));
    let job = run env job_capture job_check "job" in
    match T.Receipt.outcome job with
    | Failed (Unsupported_effect { operation }) -> [%test_eq: string] operation "Job.get"
    | _ -> failwith "caught Job effect was admitted")
;;

let%test_unit "missing required fixture and compile failure have no runtime events" =
  Eio_main.run (fun env ->
    let missing_capture = capture ~steps:[ step "fixture" ] pure in
    let missing =
      T.run
        missing_capture
        ~check:(check env missing_capture)
        ~id:(T.Run_id.create "missing" |> Or_error.ok_exn)
        ~limits:Chatml_execution.default_limits
        ~max_output_bytes:32
        ~env
      |> run_ok
    in
    (match T.Receipt.outcome missing with
     | Failed (Fixture_missing { next_step = 0 }) -> ()
     | _ -> failwith "missing fixture admitted");
    let invalid_capture = capture "let main = fun input -> unknown(input)" in
    let checked = check env invalid_capture in
    (match T.Check.outcome checked with
     | Ready -> failwith "invalid program checked"
     | Rejected diagnostic -> assert (Option.is_some (T.Diagnostic.location diagnostic)));
    let failed = run env invalid_capture checked "invalid" in
    (match T.Receipt.outcome failed with
     | Failed (Compile _) -> ()
     | _ -> failwith "compile failure did not terminate test");
    [%test_eq: int] (T.Receipt.matched_steps failed) 0;
    [%test_eq: int] (List.length (T.Receipt.logs failed)) 0)
;;

let%test_unit "fixture numeric matching follows finite ChatML Json.t round trips" =
  Eio_main.run (fun env ->
    let numeric_step expected_input =
      C.Fixture.Step.create
        ~tool_name:"fixture"
        ~input_schema:(Jsonaf.of_string "{}")
        ~output_schema:(Jsonaf.of_string "{}")
        ~expected_input
        ~response:(Return (`String "world"))
        ~json_bounds
        ~checkpoint
      |> Or_error.ok_exn
    in
    List.iter
      [ `Number "1"; `Number "1e0"; `Number "0.1" ]
      ~f:(fun input ->
        let fixture_capture = capture ~steps:[ numeric_step input ] ~input tool in
        let receipt = run env fixture_capture (check env fixture_capture) "numeric" in
        match T.Receipt.outcome receipt with
        | Succeeded (`String "world") -> ()
        | _ -> failwith "finite numeric fixture did not match round-tripped input");
    let build = build pure in
    let graph = B.prepared build |> P.fingerprint in
    let origin =
      C.Origin.local_saved ~graph ~source_revision:"saved" |> Or_error.ok_exn
    in
    List.iter [ "1e999"; "1e-999" ] ~f:(fun number ->
      match
        C.create
          ~build
          ~origin
          ~target:One_off_v1
          ~entrypoint:"main"
          ~input:(`Number number)
          ~fixtures:None
          ~json_bounds
          ~checkpoint
      with
      | Error Input_limit -> ()
      | _ -> failwith "out-of-range numeric input admitted");
    let exact_integer = `Number "9007199254740993" in
    let exact_schema = Jsonaf.of_string {|{"enum":[9007199254740993]}|} in
    let create_step ~input_schema ~output_schema ~expected_input ~response =
      C.Fixture.Step.create
        ~tool_name:"fixture"
        ~input_schema
        ~output_schema
        ~expected_input
        ~response
        ~json_bounds
        ~checkpoint
    in
    (match
       create_step
         ~input_schema:exact_schema
         ~output_schema:(Jsonaf.of_string "{}")
         ~expected_input:exact_integer
         ~response:(Return `Null)
     with
     | Error _ -> ()
     | Ok _ -> failwith "precision-changing fixture input admitted");
    match
      create_step
        ~input_schema:(Jsonaf.of_string "{}")
        ~output_schema:exact_schema
        ~expected_input:`Null
        ~response:(Return exact_integer)
    with
    | Error _ -> ()
    | Ok _ -> failwith "precision-changing fixture output admitted")
;;

let%test_unit "admission binds the exact check object and fixture mode" =
  Eio_main.run (fun env ->
    let first = capture pure in
    let second = capture pure in
    let checked = check env first in
    let id = T.Run_id.create "same" |> Or_error.ok_exn in
    (match
       T.run
         second
         ~check:checked
         ~id
         ~limits:Chatml_execution.default_limits
         ~max_output_bytes:4096
         ~env
     with
     | Error Mismatched_check -> ()
     | _ -> failwith "equal-looking reconstructed capture reused check");
    let build = build pure in
    let graph = B.prepared build |> P.fingerprint in
    let origin =
      C.Origin.local_saved ~graph ~source_revision:"saved" |> Or_error.ok_exn
    in
    let unchecked =
      C.create
        ~build
        ~origin
        ~target:One_off_v1
        ~entrypoint:"main"
        ~input:`Null
        ~fixtures:None
        ~json_bounds
        ~checkpoint
      |> capture_ok
    in
    let no_fixture = check env unchecked in
    match
      T.run
        unchecked
        ~check:no_fixture
        ~id
        ~limits:Chatml_execution.default_limits
        ~max_output_bytes:4096
        ~env
    with
    | Error No_fixtures -> ()
    | _ -> failwith "non-fixture capture entered fixture runner")
;;

let%test_unit "schema and source graph admission reject mismatches" =
  let invalid_schema = Jsonaf.of_string {|{"type":"integer"}|} in
  (match
     C.Fixture.Step.create
       ~tool_name:"fixture"
       ~input_schema:invalid_schema
       ~output_schema:(Jsonaf.of_string "{}")
       ~expected_input:(`String "hello")
       ~response:(Return `Null)
       ~json_bounds
       ~checkpoint
   with
   | Error _ -> ()
   | Ok _ -> failwith "invalid fixture input admitted");
  let build = build pure in
  let wrong = S.Digest.of_contents "wrong graph" ~checkpoint in
  let origin =
    C.Origin.local_saved ~graph:wrong ~source_revision:"saved" |> Or_error.ok_exn
  in
  match
    C.create
      ~build
      ~origin
      ~target:One_off_v1
      ~entrypoint:"main"
      ~input:`Null
      ~fixtures:None
      ~json_bounds
      ~checkpoint
  with
  | Error Invalid_origin -> ()
  | _ -> failwith "foreign graph provenance admitted"
;;

let log_program =
  {|let main = fun input -> Task.bind(Log.info("logged"), fun _ -> Task.pure(input))|}
;;

let%test_unit "aggregate output cap includes logs and terminal result" =
  Eio_main.run (fun env ->
    let capture = capture log_program in
    let checked = check env capture in
    let id = T.Run_id.create "log" |> Or_error.ok_exn in
    let receipt =
      T.run
        capture
        ~check:checked
        ~id
        ~limits:Chatml_execution.default_limits
        ~max_output_bytes:10
        ~env
      |> run_ok
    in
    match T.Receipt.outcome receipt with
    | Failed Output_limit -> ()
    | _ -> failwith "logs and result exceeded one output cap")
;;

let%test_unit "an empty fixture suite rejects a Tool.call" =
  Eio_main.run (fun env ->
    let capture = capture tool in
    let receipt = run env capture (check env capture) "empty" in
    match T.Receipt.outcome receipt with
    | Failed (Fixture_mismatch { expected_step = 0; actual_tool }) ->
      [%test_eq: string] actual_tool "fixture"
    | _ -> failwith "empty fixture suite allowed a call")
;;

let%test_unit "fixture output schemas and bounded JSON reject before capture" =
  let string_schema = Jsonaf.of_string {|{"type":"string"}|} in
  (match
     C.Fixture.Step.create
       ~tool_name:"fixture"
       ~input_schema:(Jsonaf.of_string "{}")
       ~output_schema:string_schema
       ~expected_input:(`String "hello")
       ~response:(Return (`Number "1"))
       ~json_bounds
       ~checkpoint
   with
   | Error _ -> ()
   | Ok _ -> failwith "invalid fixture output admitted");
  let bound =
    C.Json_bounds.create ~max_bytes:16 ~max_depth:2 ~max_nodes:2 |> Or_error.ok_exn
  in
  let build = build pure in
  let graph = B.prepared build |> P.fingerprint in
  let origin = C.Origin.local_saved ~graph ~source_revision:"saved" |> Or_error.ok_exn in
  (match
     C.create
       ~build
       ~origin
       ~target:One_off_v1
       ~entrypoint:"main"
       ~input:(`String "too long for the bound")
       ~fixtures:None
       ~json_bounds:bound
       ~checkpoint
   with
   | Error Input_limit -> ()
   | _ -> failwith "oversized developer input admitted");
  let marker = ref () in
  let exception Stop of unit ref in
  match
    C.Fixture.create
      ~steps:[ step "fixture" ]
      ~max_steps:2
      ~max_bytes:16_384
      ~checkpoint:(fun () -> raise (Stop marker))
  with
  | exception Stop actual -> assert (phys_equal marker actual)
  | _ -> failwith "fixture checkpoint exception was swallowed"
;;

let%test_unit "JSON admission preserves Exit and rejects a huge numeric lexeme first" =
  let bounded =
    C.Json_bounds.create ~max_bytes:16 ~max_depth:2 ~max_nodes:2 |> Or_error.ok_exn
  in
  (match
     C.Json_bounds.measure bounded (`String "x") ~checkpoint:(fun () -> raise Exit)
   with
   | exception Exit -> ()
   | _ -> failwith "caller Exit was swallowed as a JSON limit");
  let polls = ref 0 in
  (match
     C.Json_bounds.measure
       bounded
       (`Number (String.make 1_000_000 '1'))
       ~checkpoint:(fun () -> incr polls)
   with
   | Error _ -> ()
   | Ok _ -> failwith "oversized numeric lexeme admitted");
  [%test_eq: int] !polls 1
;;

let%test_unit "Tool_v1 requires a separate authenticated invocation contract" =
  let contract = Chatml.Chatml_compiler_contract.for_target Tool_v1 ~checkpoint in
  let fixture = Fixtures.make ~library:true ~contract () in
  let entry = Support.capture ~namespace:"root" "src/developer.chatml" pure in
  let build = Fixtures.compose fixture ~entry:(B.Entry.Captured entry) |> Support.ok in
  let graph = B.prepared build |> P.fingerprint in
  let origin = C.Origin.local_saved ~graph ~source_revision:"saved" |> Or_error.ok_exn in
  match
    C.create
      ~build
      ~origin
      ~target:Tool_v1
      ~entrypoint:"run"
      ~input:`Null
      ~fixtures:None
      ~json_bounds
      ~checkpoint
  with
  | Error Unsupported_target -> ()
  | _ -> failwith "Tool_v1 host context was fabricated"
;;

let inert_initializer =
  {|let initialized = fail("must not execute during check")
let main = fun input -> Task.pure(input)|}
;;

let%test_unit "non-executing check leaves failing initializer untouched" =
  Eio_main.run (fun env ->
    let capture = capture inert_initializer in
    let checked = check env capture in
    (match T.Check.outcome checked with
     | Ready -> ()
     | Rejected diagnostic -> failwith (T.Diagnostic.message diagnostic));
    let receipt = run env capture checked "initializer" in
    match T.Receipt.outcome receipt with
    | Failed (Runtime _) -> ()
    | _ -> failwith "initializer failure did not occur at execution")
;;
