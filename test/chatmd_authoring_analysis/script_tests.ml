open Core
module A = Chatml_authoring_analysis
module D = Chatmd_authoring_analysis
module B = Chatml_project_build
module C = Chatml_authoring_context
module E = Chatml_authoring_effective
module P = Chatml_prepared_program
module S = Chatml_source
module CM = Prompt.Chat_markdown
module F = Chatml_authoring_fallback
module FD = Chatmd_authoring_fallback
module AP = Agent_protocol
module W = AP.Authoring.Assistance.Assistance

let ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (D.Error.sexp_of_t error)
;;

let context_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (C.Error.sexp_of_t error)
;;

let parse sources =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let root = List.Assoc.find_exn sources "root.chatmd" ~equal:String.equal in
    let loader = Source_loader.captured_filesystem ~root:dir ~sources in
    let limits =
      CM.Limits.create
        ~max_input_bytes:1_000_000
        ~max_parse_calls:1024
        ~max_tokens:100_000
        ~max_depth:128
      |> Or_error.ok_exn
    in
    CM.prepare_chat_inputs
      ~source:"root.chatmd"
      ~source_loader:loader
      ~dir
      ~preprocess:Fn.id
      ~limits
      ~checkpoint:ignore
      root)
;;

let policy ?(max_results = 2) () =
  let semantic =
    Chatml_semantics.Limits.create
      ~max_input_bytes:1_000_000
      ~recording:
        (Chatml_typechecker.Semantic.Limits.create
           ~max_facts:20_000
           ~max_edges:40_000
           ~max_label_bytes:400_000
           ~max_work_steps:400_000
         |> Or_error.ok_exn)
      ~type_graph:
        (Chatml_semantics.Frozen.Limits.create
           ~max_nodes:20_000
           ~max_edges:40_000
           ~max_label_bytes:400_000
         |> Or_error.ok_exn)
      ~max_published_facts:20_000
      ~max_published_edges:40_000
      ~max_published_label_bytes:400_000
      ~max_work_steps:400_000
    |> Or_error.ok_exn
  in
  A.Policy.create
    ~compilation:Chatml_compilation.default_limits
    ~semantic
    ~max_dependency_depth:16
    ~max_results
    ~max_projected_diagnostics:4
    ~max_projected_sites:12
    ~max_projection_sources:8
    ~max_position_index_entries:256
    ~max_projection_source_bytes:1_000_000
  |> Or_error.ok_exn
;;

let identity nonce =
  C.Identity.create
    ~principal:"analysis-principal"
    ~host_incarnation:"analysis-host"
    ~connection_lease:"analysis-lease"
    ~nonce
  |> Or_error.ok_exn
;;

let source contents =
  S.create
    (Support.id ~namespace:"document" "root.chatmd")
    ~contents
    ~expected_digest:None
    ~max_bytes:None
    ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let limits () =
  C.Limits.create
    ~max_contexts:2
    ~max_sources:32
    ~max_open_documents:2
    ~max_document_bytes:1_000_000
    ~max_effective_input_bytes:2_000_000
    ~max_context_source_bytes:2_000_000
    ~max_total_source_bytes:4_000_000
  |> Or_error.ok_exn
;;

let selected_build body =
  let fixture = Fixtures.make () in
  let base = Fixtures.compose fixture |> Support.ok in
  let sources =
    P.sources (B.prepared base)
    |> List.map ~f:(fun captured ->
      if S.Id.equal (S.id captured) (S.id fixture.main)
      then
        S.create
          (S.id captured)
          ~contents:body
          ~expected_digest:None
          ~max_bytes:None
          ~checkpoint:ignore
        |> Or_error.ok_exn
      else captured)
  in
  let selected =
    B.Effective.recompose
      base
      ~sources
      ~lock_id:(Support.id ~namespace:"root" "script-selected.lock.json")
      ~contract:fixture.contract
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  fixture, base, selected
;;

let ready inputs parsed script_id build =
  match
    D.Script.assess
      inputs
      ~parsed:(Some parsed)
      ~script_id:(Some script_id)
      ~selection:(D.Selection.Validated_build build)
      ~max_membership_work_steps:64
      ~checkpoint:ignore
    |> ok
  with
  | Ready script -> script
  | Unavailable unavailable -> Error.raise_s (D.Unavailable.sexp_of_t unavailable)
;;

let document_context contexts ~identity ~contents =
  C.admit_document
    contexts
    ~identity
    ~base:(source contents)
    ~requested_environment:None
    ~checkpoint:ignore
  |> context_ok
;;

let site source start_byte end_byte =
  let location =
    S.locate source ~start_byte ~end_byte ~checkpoint:ignore |> Or_error.ok_exn
  in
  Chatml_diagnostic.Site.create source ~span:(S.Location.span location) ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let wire_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (AP.Error.sexp_of_t error)
;;

let fallback_limits () =
  F.Limits.create
    ~lexical:
      (Language_highlighting.Highlight_snapshot.Limits.create
         ~max_source_bytes:100_000
         ~max_identity_bytes:2048
         ~max_spans:10_000
         ~max_issues:256
         ~max_work_units:100_000
         ~max_nesting:128
       |> Or_error.ok_exn)
    ~max_contract_identity_bytes:20_000_000
    ~max_contract_work_steps:100_000
    ~max_projection_work_steps:100_000
    ~max_output_bytes:200_000
  |> Or_error.ok_exn
;;

let fallback_adapter_limits () =
  FD.Limits.create
    ~max_source_bytes:1_000_000
    ~max_identity_bytes:2048
    ~max_work_steps:10_000
  |> Or_error.ok_exn
;;

let fallback_request source ~cursor_byte =
  let generator =
    AP.Id.Generator.create ~bytes:(fun length -> String.make length '\000')
  in
  let host = AP.Id.Server.create_with generator in
  let id = AP.Id.Authoring_context.create_with generator in
  let wire_source =
    AP.Authoring.Source.create
      ~id:(S.id source)
      ~digest:(S.digest source)
      ~byte_length:(S.byte_length source)
      ~editor_revision:None
    |> wire_ok
  in
  let fingerprint = S.Digest.of_contents "document" ~checkpoint:ignore in
  let context =
    AP.Authoring.Context.create
      ~id
      ~host
      ~origin:
        (AP.Authoring.Context.Supplied_document
           { source = wire_source; default_environment = None })
      ~generation:0
      ~basis:(AP.Authoring.Context.Document { source = wire_source; selection = None })
      ~basis_fingerprint:fingerprint
      ~editable_sources:[]
    |> wire_ok
    |> AP.Authoring.Context.Expected.of_view
  in
  W.Request.create ~context ~source:wire_source ~cursor_byte ~max_items:64 |> wire_ok
;;

let%test_unit "selected script uses real parser ownership and current document bytes" =
  let authored_body = "let main input = Task.pure(input)" in
  let root =
    sprintf {|<script id="s" language="chatml" kind="tool">%s</script>|} authored_body
  in
  let parsed = parse [ "root.chatmd", root ] in
  let fixture, _base, selected = selected_build authored_body in
  let id = identity "document" in
  let contexts =
    C.create ~limits:(limits ())
    |> C.admit_document
         ~identity:id
         ~base:(source root)
         ~requested_environment:None
         ~checkpoint:ignore
    |> context_ok
  in
  let inputs = C.document_inputs contexts ~identity:id ~checkpoint:ignore |> context_ok in
  let script = ready inputs parsed "s" selected in
  let stamp = D.Script.document_stamp script in
  assert (C.Identity.equal (D.Document_stamp.identity stamp) id);
  assert (String.equal (D.Document_stamp.script_id stamp) "s");
  assert (
    S.Id.equal
      (D.Document_stamp.selected_script_source stamp)
      (P.entry (B.prepared selected)));
  let selected_contract =
    F.Selected_contract.admit
      selected
      ~selected_contract:fixture.contract
      ~limits:(fallback_limits ())
      ~checkpoint:ignore
    |> function
    | Ok selected -> selected
    | Error error -> Error.raise_s (F.Error.sexp_of_t error)
  in
  let authored_input = CM.Parsed.root_input parsed in
  let authored_source = C.Document.Inputs.source inputs in
  let origin =
    match
      FD.Origin.admit
        script
        ~current:inputs
        ~authored_input
        ~authored_source
        ~selected:selected_contract
        ~limits:(fallback_adapter_limits ())
        ~checkpoint:ignore
    with
    | Ok (Ready origin) -> origin
    | Ok Unavailable | Error _ -> assert false
  in
  let body_start = String.substr_index_exn root ~pattern:authored_body in
  let request = fallback_request authored_source ~cursor_byte:(body_start + 2) in
  let fallback =
    F.produce
      origin
      ~request
      ~selected:(Some selected_contract)
      ~limits:(fallback_limits ())
      ~checkpoint:ignore
    |> function
    | Ok result -> result
    | Error error -> Error.raise_s (F.Error.sexp_of_t error)
  in
  let let_item =
    W.Result.items fallback
    |> List.find_exn ~f:(fun item -> String.equal (W.Suggestion.label item) "let")
  in
  let replacement = W.Suggestion.replace let_item in
  assert (Int.equal (W.Range.start_byte replacement) body_start);
  assert (Int.equal (W.Range.end_byte replacement) (body_start + 3));
  (match
     FD.Origin.admit
       script
       ~current:inputs
       ~authored_input
       ~authored_source:(source (root ^ " changed"))
       ~selected:selected_contract
       ~limits:(fallback_adapter_limits ())
       ~checkpoint:ignore
   with
   | Error FD.Error.Stale_document -> ()
   | Ok _ | Error _ -> assert false);
  let foreign_parser = parse [ "root.chatmd", root ] in
  (match
     FD.Origin.admit
       script
       ~current:inputs
       ~authored_input:(CM.Parsed.root_input foreign_parser)
       ~authored_source
       ~selected:selected_contract
       ~limits:(fallback_adapter_limits ())
       ~checkpoint:ignore
   with
   | Ok FD.Origin.Unavailable -> ()
   | Ok (Ready _) | Error _ -> assert false);
  Eio_main.run (fun env ->
    let policy = policy () in
    let candidate =
      D.Candidate.analyze
        script
        ~policy
        ~env
        ~target:Chatml_compilation.One_off_v1
        ~checkpoint:ignore
      |> ok
    in
    let exception Interrupted of unit ref in
    let token = ref () in
    (match
       D.Candidate.analyze
         script
         ~policy
         ~env
         ~target:Chatml_compilation.One_off_v1
         ~checkpoint:(fun () -> raise (Interrupted token))
     with
     | exception Interrupted actual -> assert (phys_equal token actual)
     | Ok _ | Error _ -> failwith "interrupted checker published a candidate");
    let cache =
      D.Document_cache.create ~policy
      |> fun cache ->
      D.Document_cache.publish
        cache
        ~current_inputs:inputs
        ~occupied_elsewhere:0
        candidate
        ~checkpoint:ignore
      |> ok
    in
    assert (Option.is_some (D.Document_cache.current cache ~contexts ~identity:id));
    let version =
      C.version contexts ~identity:id ~source:(S.id (source root)) |> Option.value_exn
    in
    let edited =
      C.replace
        contexts
        ~expected:version
        ~next_revision:(C.Editor_revision.create 1 |> Or_error.ok_exn)
        ~contents:(root ^ " ")
        ~checkpoint:ignore
      |> context_ok
    in
    let cache = D.Document_cache.transition cache ~contexts:edited in
    assert (Int.equal (D.Document_cache.retained_count cache) 0);
    assert (Option.is_none (D.Document_cache.current cache ~contexts:edited ~identity:id));
    let edited_inputs =
      C.document_inputs edited ~identity:id ~checkpoint:ignore |> context_ok
    in
    (match
       D.Document_cache.publish
         (D.Document_cache.create ~policy)
         ~current_inputs:edited_inputs
         ~occupied_elsewhere:0
         candidate
         ~checkpoint:ignore
     with
     | Error D.Error.Stale_document -> ()
     | Error error -> Error.raise_s (D.Error.sexp_of_t error)
     | Ok _ -> failwith "stale candidate published after edit");
    match
      D.Script.assess
        edited_inputs
        ~parsed:(Some parsed)
        ~script_id:(Some "s")
        ~selection:(D.Selection.Validated_build selected)
        ~max_membership_work_steps:64
        ~checkpoint:ignore
    with
    | Error D.Error.Stale_document -> ()
    | Error error -> Error.raise_s (D.Error.sexp_of_t error)
    | Ok _ -> failwith "stale parser admitted")
;;

let%test_unit "unavailable and invalid selections retain distinct outcomes" =
  let root = {|<script id="s" language="chatml" kind="tool">body</script>|} in
  let parsed = parse [ "root.chatmd", root ] in
  let id = identity "availability" in
  let contexts =
    C.create ~limits:(limits ())
    |> C.admit_document
         ~identity:id
         ~base:(source root)
         ~requested_environment:None
         ~checkpoint:ignore
    |> context_ok
  in
  let inputs = C.document_inputs contexts ~identity:id ~checkpoint:ignore |> context_ok in
  let assess parsed script_id selection =
    D.Script.assess
      inputs
      ~parsed
      ~script_id
      ~selection
      ~max_membership_work_steps:64
      ~checkpoint:ignore
  in
  let unavailable expected = function
    | Ok (D.Script.Unavailable actual) ->
      assert (
        Sexp.equal (D.Unavailable.sexp_of_t expected) (D.Unavailable.sexp_of_t actual))
    | Ok (Ready _) | Error _ -> failwith "wrong availability outcome"
  in
  unavailable
    D.Unavailable.No_selected_script
    (assess (Some parsed) None No_validated_build);
  unavailable
    D.Unavailable.Incomplete_or_malformed_document
    (assess None (Some "s") No_validated_build);
  unavailable
    D.Unavailable.No_validated_project
    (assess (Some parsed) (Some "s") No_validated_build);
  unavailable
    D.Unavailable.Dynamic_only_source
    (assess (Some parsed) (Some "s") Dynamic_only);
  (match assess (Some parsed) (Some "missing") No_validated_build with
   | Error D.Error.Foreign_declaration -> ()
   | Ok _ | Error _ -> failwith "foreign declaration admitted");
  (match
     D.Script.assess
       inputs
       ~parsed:(Some parsed)
       ~script_id:(Some "s")
       ~selection:No_validated_build
       ~max_membership_work_steps:1
       ~checkpoint:ignore
   with
   | Error D.Error.Membership_limit -> ()
   | Ok _ | Error _ -> failwith "membership budget bypassed");
  let _fixture, _base, mismatched = selected_build "different body" in
  (match assess (Some parsed) (Some "s") (Validated_build mismatched) with
   | Error D.Error.Script_body_mismatch -> ()
   | Ok _ | Error _ -> failwith "foreign selected body admitted");
  let selected_root =
    {|<script id="s" language="chatml" kind="tool" project="./p.json" environment="other">body</script>|}
  in
  let selected_parsed = parse [ "root.chatmd", selected_root ] in
  let selected_contexts =
    document_context
      (C.create ~limits:(limits ()))
      ~identity:(identity "selected-environment")
      ~contents:selected_root
  in
  let selected_inputs =
    C.document_inputs
      selected_contexts
      ~identity:(identity "selected-environment")
      ~checkpoint:ignore
    |> context_ok
  in
  let _fixture, _base, matching = selected_build "body" in
  match
    D.Script.assess
      selected_inputs
      ~parsed:(Some selected_parsed)
      ~script_id:(Some "s")
      ~selection:(Validated_build matching)
      ~max_membership_work_steps:64
      ~checkpoint:ignore
  with
  | Error D.Error.Selection_mismatch -> ()
  | Ok _ | Error _ -> failwith "wrong selected environment admitted"
;;

let%test_unit "imported, transformed and external script provenance remains distinct" =
  let cases =
    [ ( "imported"
      , [ "root.chatmd", {|<import src="part.chatmd" namespace="lib"/>|}
        ; ( "part.chatmd"
          , {|<script id="s" language="chatml" kind="tool">same same</script>|} )
        ]
      , "lib:s"
      , "same same"
      , "part.chatmd" )
    ; ( "external"
      , [ ( "root.chatmd"
          , {|<script id="s" language="chatml" kind="tool" src="code.chatml"/>|} )
        ; "code.chatml", "same same"
        ]
      , "s"
      , "same same"
      , "root.chatmd" )
    ]
  in
  List.iter cases ~f:(fun (kind, files, script_id, body, declaring_path) ->
    let parsed = parse files in
    let root = List.Assoc.find_exn files "root.chatmd" ~equal:String.equal in
    let _fixture, _base, build = selected_build body in
    let id = identity kind in
    let contexts =
      document_context (C.create ~limits:(limits ())) ~identity:id ~contents:root
    in
    let inputs =
      C.document_inputs contexts ~identity:id ~checkpoint:ignore |> context_ok
    in
    let script = ready inputs parsed script_id build in
    let stamp = D.Script.document_stamp script in
    let declaring_input =
      CM.Parsed.declarations parsed
      |> List.find_exn ~f:(fun declaration ->
        String.equal (CM.Declaration.script_id declaration) script_id)
      |> CM.Declaration.range
      |> Chatmd_parser_input.Range.input
    in
    [%test_eq: string] (Chatmd_parser_input.path declaring_input) declaring_path;
    [%test_eq: string]
      (D.Document_stamp.declaration_input_fingerprint stamp)
      (Chatmd_parser_input.identity_digest declaring_input ~checkpoint:ignore);
    [%test_eq: int]
      (D.Document_stamp.declaration_input_byte_length stamp)
      (Chatmd_parser_input.effective_byte_length declaring_input);
    Eio_main.run (fun env ->
      let policy = policy () in
      let candidate =
        D.Candidate.analyze
          script
          ~policy
          ~env
          ~target:Chatml_compilation.One_off_v1
          ~checkpoint:ignore
        |> ok
      in
      let entry =
        P.sources (B.prepared build)
        |> List.find_exn ~f:(fun item ->
          S.Id.equal (S.id item) (P.entry (B.prepared build)))
      in
      let native = Chatml_diagnostic.create ~site:(site entry 0 4) "selected" in
      let projected =
        A.Diagnostic_projection.project_native native ~policy ~checkpoint:ignore
        |> D.Projection.of_diagnostic candidate ~checkpoint:ignore
        |> ok
      in
      match D.Projection.primary projected |> Option.value_exn with
      | { authored = Some (CM.Declaration.Extraction.Exact_inline span); _ }
        when String.equal kind "imported" ->
        assert (phys_equal (CM.Declaration.Extraction.input span) declaring_input)
      | { authored = Some (CM.Declaration.Extraction.External_origin origin); _ }
        when String.equal kind "external" ->
        [%test_eq: string] origin.member "code.chatml"
      | _ -> failwith "script provenance was fabricated or lost"));
  let authored = {|<script id="s" language="chatml" kind="tool">x</script>|} in
  let parsed =
    Eio_main.run (fun env ->
      let dir = Eio.Stdenv.cwd env in
      let loader =
        Source_loader.captured_filesystem ~root:dir ~sources:[ "root.chatmd", authored ]
      in
      let limits =
        CM.Limits.create
          ~max_input_bytes:1_000_000
          ~max_parse_calls:1024
          ~max_tokens:100_000
          ~max_depth:128
        |> Or_error.ok_exn
      in
      CM.prepare_chat_inputs
        ~source:"root.chatmd"
        ~source_loader:loader
        ~dir
        ~preprocess:(fun _ -> {|<script id="s" language="chatml" kind="tool">y</script>|})
        ~limits
        ~checkpoint:ignore
        authored)
  in
  let _fixture, _base, build = selected_build "y" in
  let id = identity "effective" in
  let contexts =
    document_context (C.create ~limits:(limits ())) ~identity:id ~contents:authored
  in
  let inputs = C.document_inputs contexts ~identity:id ~checkpoint:ignore |> context_ok in
  let script = ready inputs parsed "s" build in
  Eio_main.run (fun env ->
    let policy = policy () in
    let candidate =
      D.Candidate.analyze
        script
        ~policy
        ~env
        ~target:Chatml_compilation.One_off_v1
        ~checkpoint:ignore
      |> ok
    in
    let entry =
      P.sources (B.prepared build)
      |> List.find_exn ~f:(fun item ->
        S.Id.equal (S.id item) (P.entry (B.prepared build)))
    in
    let native = Chatml_diagnostic.create ~site:(site entry 0 1) "selected" in
    let projected =
      A.Diagnostic_projection.project_native native ~policy ~checkpoint:ignore
      |> D.Projection.of_diagnostic candidate ~checkpoint:ignore
      |> ok
    in
    match D.Projection.primary projected |> Option.value_exn with
    | { authored = Some (CM.Declaration.Extraction.Approximate_declaration _); _ } -> ()
    | _ -> failwith "effective-only bytes claimed an exact authored site")
;;

let%test_unit "project and document result maps share one aggregate retention cap" =
  let body = "let main input = Task.pure(input)" in
  let root = sprintf {|<script id="s" language="chatml" kind="tool">%s</script>|} body in
  let parsed = parse [ "root.chatmd", root ] in
  let fixture, base, selected = selected_build body in
  let project_id = identity "project" in
  let document_id = identity "document-shared" in
  let contexts =
    C.create ~limits:(limits ())
    |> C.admit
         ~identity:project_id
         ~base
         ~editable_sources:[ S.id fixture.main ]
         ~checkpoint:ignore
    |> context_ok
  in
  let contexts = document_context contexts ~identity:document_id ~contents:root in
  let document_inputs =
    C.document_inputs contexts ~identity:document_id ~checkpoint:ignore |> context_ok
  in
  Eio_main.run (fun env ->
    let foreign_policy = policy ~max_results:1 () in
    let policy = policy ~max_results:1 () in
    let script = ready document_inputs parsed "s" selected in
    let document_candidate =
      D.Candidate.analyze
        script
        ~policy
        ~env
        ~target:Chatml_compilation.One_off_v1
        ~checkpoint:ignore
      |> ok
    in
    let project_inputs =
      C.inputs contexts ~identity:project_id ~checkpoint:ignore |> context_ok
    in
    let receipt =
      E.validate_overlay
        project_inputs
        ~lock_id:(Support.id ~namespace:"root" "project-analysis.lock.json")
        ~contract:fixture.contract
        ~limits:(Support.limits ())
        ~checkpoint:ignore
      |> Support.ok
    in
    let project_candidate =
      A.Project_candidate.analyze_validated
        project_inputs
        ~receipt
        ~policy
        ~env
        ~target:Chatml_compilation.One_off_v1
        ~checkpoint:ignore
      |> Result.map_error ~f:A.Error.sexp_of_t
      |> function
      | Ok candidate -> candidate
      | Error error -> Error.raise_s error
    in
    let empty_project () = A.Project_cache.create ~contexts ~policy in
    let empty_document () = D.Document_cache.create ~policy in
    let document_first =
      D.Document_cache.publish
        (empty_document ())
        ~current_inputs:document_inputs
        ~occupied_elsewhere:0
        document_candidate
        ~checkpoint:ignore
      |> ok
    in
    (match
       A.Project_cache.publish
         (empty_project ())
         project_candidate
         ~occupied_elsewhere:(D.Document_cache.retained_count document_first)
         ~checkpoint:ignore
     with
     | Error A.Error.Retention_limit -> ()
     | _ -> failwith "project bypassed document retention");
    let project_first =
      A.Project_cache.publish
        (empty_project ())
        project_candidate
        ~occupied_elsewhere:0
        ~checkpoint:ignore
      |> function
      | Ok cache -> cache
      | Error error -> Error.raise_s (A.Error.sexp_of_t error)
    in
    (match
       D.Document_cache.publish
         (empty_document ())
         ~current_inputs:document_inputs
         ~occupied_elsewhere:(A.Project_cache.retained_count project_first)
         document_candidate
         ~checkpoint:ignore
     with
     | Error D.Error.Retention_limit -> ()
     | _ -> failwith "document bypassed project retention");
    let replaced =
      D.Document_cache.publish
        document_first
        ~current_inputs:document_inputs
        ~occupied_elsewhere:0
        document_candidate
        ~checkpoint:ignore
      |> ok
    in
    assert (Int.equal (D.Document_cache.retained_count replaced) 1);
    (match
       D.Document_cache.publish
         (D.Document_cache.create ~policy:foreign_policy)
         ~current_inputs:document_inputs
         ~occupied_elsewhere:0
         document_candidate
         ~checkpoint:ignore
     with
     | Error D.Error.Policy_mismatch -> ()
     | _ -> failwith "foreign policy admitted");
    match
      D.Document_cache.publish
        (empty_document ())
        ~current_inputs:document_inputs
        ~occupied_elsewhere:(-1)
        document_candidate
        ~checkpoint:ignore
    with
    | Error D.Error.Invalid_occupancy -> ()
    | _ -> failwith "invalid occupancy admitted")
;;
