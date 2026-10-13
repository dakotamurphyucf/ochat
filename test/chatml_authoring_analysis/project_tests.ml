open Core
module A = Chatml_authoring_analysis
module B = Chatml_project_build
module C = Chatml_authoring_context
module E = Chatml_authoring_effective
module P = Chatml_prepared_program
module S = Chatml_source
module Sem = Chatml_semantics

let ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (A.Error.sexp_of_t error)
;;

let context_ok = function
  | Ok value -> value
  | Error error -> Error.raise_s (C.Error.sexp_of_t error)
;;

let semantic_limits () =
  Sem.Limits.create
    ~max_input_bytes:1_000_000
    ~recording:
      (Chatml_typechecker.Semantic.Limits.create
         ~max_facts:20_000
         ~max_edges:40_000
         ~max_label_bytes:400_000
         ~max_work_steps:400_000
       |> Or_error.ok_exn)
    ~type_graph:
      (Sem.Frozen.Limits.create
         ~max_nodes:20_000
         ~max_edges:40_000
         ~max_label_bytes:400_000
       |> Or_error.ok_exn)
    ~max_published_facts:20_000
    ~max_published_edges:40_000
    ~max_published_label_bytes:400_000
    ~max_work_steps:400_000
  |> Or_error.ok_exn
;;

let policy
      ?(max_results = 1)
      ?(max_projected_sites = 12)
      ?(max_projection_sources = 8)
      ?(max_position_index_entries = 256)
      ()
  =
  A.Policy.create
    ~compilation:Chatml_compilation.default_limits
    ~semantic:(semantic_limits ())
    ~max_dependency_depth:16
    ~max_results
    ~max_projected_diagnostics:4
    ~max_projected_sites
    ~max_projection_sources
    ~max_position_index_entries
    ~max_projection_source_bytes:1_000_000
  |> Or_error.ok_exn
;;

let recapture source contents =
  S.create
    (S.id source)
    ~contents
    ~expected_digest:None
    ~max_bytes:None
    ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let revise_build ?(lock_name = "analysis.lock.json") fixture base main_contents =
  let sources =
    P.sources (B.prepared base)
    |> List.map ~f:(fun source ->
      if S.Id.equal (S.id source) (S.id fixture.Fixtures.main)
      then recapture source main_contents
      else source)
  in
  B.Effective.recompose
    base
    ~sources
    ~lock_id:(Support.id ~namespace:"root" lock_name)
    ~contract:fixture.contract
    ~limits:(Support.limits ())
    ~checkpoint:ignore
  |> Support.ok
;;

let initial_context ?(max_contexts = 1) base fixture =
  let id =
    C.Identity.create
      ~principal:"analysis-principal"
      ~host_incarnation:"analysis-host"
      ~connection_lease:"analysis-lease"
      ~nonce:"analysis-context"
    |> Or_error.ok_exn
  in
  let accounting = P.accounting (B.prepared base) in
  let source_bytes = P.Accounting.source_bytes accounting in
  let input_bytes = P.Accounting.input_bytes accounting in
  let limits =
    C.Limits.create
      ~max_contexts
      ~max_sources:(max_contexts * P.Accounting.source_count accounting)
      ~max_open_documents:1
      ~max_document_bytes:1024
      ~max_effective_input_bytes:(input_bytes + 256)
      ~max_context_source_bytes:(source_bytes + 256)
      ~max_total_source_bytes:(max_contexts * (source_bytes + 256))
    |> Or_error.ok_exn
  in
  let contexts =
    C.create ~limits
    |> C.admit
         ~identity:id
         ~base
         ~editable_sources:[ S.id fixture.Fixtures.main ]
         ~checkpoint:ignore
    |> context_ok
  in
  id, contexts
;;

let validated contexts id fixture =
  let inputs = C.inputs contexts ~identity:id ~checkpoint:ignore |> context_ok in
  let receipt =
    E.validate_overlay
      inputs
      ~lock_id:(Support.id ~namespace:"root" "next-analysis.lock.json")
      ~contract:fixture.Fixtures.contract
      ~limits:(Support.limits ())
      ~checkpoint:ignore
    |> Support.ok
  in
  inputs, receipt
;;

let assert_native_compiler_agreement env build checked =
  let compiled =
    Chatml_compilation.compile_prepared
      (B.prepared build)
      ~limits:Chatml_compilation.default_limits
      ~max_dependency_depth:16
      ~env
      ~target:Chatml_compilation.One_off_v1
      ()
  in
  match A.Checked.status checked, compiled with
  | Complete _, Ok _ | Failed _, Error _ -> ()
  | Complete _, Error _ -> failwith "analyzer certified a graph rejected by compiler"
  | Failed _, Ok _ -> failwith "analyzer rejected a graph accepted by compiler"
;;

let%test_unit
    "validated project analysis agrees with native compiler and evicts stale results"
  =
  Eio_main.run (fun env ->
    let fixture = Fixtures.make () in
    let base = Fixtures.compose fixture |> Support.ok in
    let base = revise_build fixture base "let main input = Task.pure(input)" in
    let id, contexts = initial_context base fixture in
    let foreign_policy = policy () in
    let policy = policy () in
    let inputs, receipt = validated contexts id fixture in
    let candidate =
      A.Project_candidate.analyze_validated
        inputs
        ~receipt
        ~policy
        ~env
        ~target:Chatml_compilation.One_off_v1
        ~checkpoint:ignore
      |> ok
    in
    let checked = A.Project_payload.checked (A.Project_candidate.payload candidate) in
    assert_native_compiler_agreement env (E.Overlay.build receipt) checked;
    (match A.Checked.status checked with
     | Complete snapshot ->
       assert (
         S.Digest.equal
           (A.Checked.input_fingerprint checked)
           (Sem.Identity.input_fingerprint (Sem.Snapshot.identity snapshot)))
     | Failed _ -> failwith "valid native project did not complete");
    let cache = A.Project_cache.create ~contexts ~policy in
    (match
       A.Project_cache.publish
         (A.Project_cache.create ~contexts ~policy:foreign_policy)
         candidate
         ~occupied_elsewhere:0
         ~checkpoint:ignore
     with
     | Error A.Error.Policy_mismatch -> ()
     | Error error -> Error.raise_s (A.Error.sexp_of_t error)
     | Ok _ -> failwith "foreign policy candidate entered cache");
    let exception Interrupted in
    (match
       A.Project_candidate.analyze_validated
         inputs
         ~receipt
         ~policy
         ~env
         ~target:Chatml_compilation.One_off_v1
         ~checkpoint:(fun () -> raise Interrupted)
     with
     | exception Interrupted -> ()
     | Error error -> Error.raise_s (A.Error.sexp_of_t error)
     | Ok _ -> failwith "candidate ignored interrupted comparison");
    let cache =
      A.Project_cache.publish cache candidate ~occupied_elsewhere:0 ~checkpoint:ignore
      |> ok
    in
    assert (Option.is_some (A.Project_cache.current cache ~identity:id));
    let version =
      C.version (A.Project_cache.contexts cache) ~identity:id ~source:(S.id fixture.main)
      |> Option.value_exn
    in
    let edited =
      C.replace
        (A.Project_cache.contexts cache)
        ~expected:version
        ~next_revision:(C.Editor_revision.create 1 |> Or_error.ok_exn)
        ~contents:"let main input ="
        ~checkpoint:ignore
      |> context_ok
    in
    let cache = A.Project_cache.transition cache ~contexts:edited in
    assert (Option.is_none (A.Project_cache.current cache ~identity:id));
    let wrong_inputs = C.inputs edited ~identity:id ~checkpoint:ignore |> context_ok in
    (match
       A.Project_candidate.analyze_validated
         wrong_inputs
         ~receipt
         ~policy
         ~env
         ~target:Chatml_compilation.One_off_v1
         ~checkpoint:ignore
     with
     | Error A.Error.Receipt_mismatch -> ()
     | Error error -> Error.raise_s (A.Error.sexp_of_t error)
     | Ok _ -> failwith "old captured receipt analyzed against edited inputs");
    (match
       A.Project_cache.publish cache candidate ~occupied_elsewhere:0 ~checkpoint:ignore
     with
     | Error A.Error.Stale_generation -> ()
     | Error error -> Error.raise_s (A.Error.sexp_of_t error)
     | Ok _ -> failwith "stale worker published after edit");
    let next_inputs, next_receipt = validated edited id fixture in
    let next =
      A.Project_candidate.analyze_validated
        next_inputs
        ~receipt:next_receipt
        ~policy
        ~env
        ~target:Chatml_compilation.One_off_v1
        ~checkpoint:ignore
      |> ok
    in
    let next_checked = A.Project_payload.checked (A.Project_candidate.payload next) in
    assert_native_compiler_agreement env (E.Overlay.build next_receipt) next_checked;
    (match A.Checked.status next_checked with
     | Failed partial -> assert (not (List.is_empty (Sem.Partial.diagnostics partial)))
     | Complete _ -> failwith "malformed edit was certified");
    let ill_typed =
      revise_build
        ~lock_name:"ill-typed.lock.json"
        fixture
        base
        "let main input = missing"
    in
    let type_checked =
      A.Checked.analyze_build
        ill_typed
        ~policy
        ~env
        ~target:Chatml_compilation.One_off_v1
        ~checkpoint:ignore
      |> ok
    in
    assert_native_compiler_agreement env ill_typed type_checked;
    (match A.Checked.status type_checked with
     | Failed _ -> ()
     | Complete _ -> failwith "unknown value unexpectedly typechecked");
    let cache =
      A.Project_cache.publish cache next ~occupied_elsewhere:0 ~checkpoint:ignore |> ok
    in
    let replacement_id =
      C.Identity.create
        ~principal:"analysis-principal"
        ~host_incarnation:"analysis-host"
        ~connection_lease:"analysis-lease"
        ~nonce:"analysis-refreshed"
      |> Or_error.ok_exn
    in
    let refreshed =
      C.refresh
        (A.Project_cache.contexts cache)
        ~identity:id
        ~expected:(C.Inputs.generation next_inputs)
        ~new_identity:replacement_id
        ~base
        ~editable_sources:[ S.id fixture.main ]
        ~policy:C.Refresh.Discard_open_documents
        ~checkpoint:ignore
      |> context_ok
    in
    let cache = A.Project_cache.transition cache ~contexts:refreshed in
    assert (Option.is_none (A.Project_cache.current cache ~identity:id));
    assert (Option.is_none (A.Project_cache.current cache ~identity:replacement_id));
    let closed =
      C.close_context (A.Project_cache.contexts cache) ~identity:replacement_id
      |> context_ok
    in
    let cache = A.Project_cache.transition cache ~contexts:closed in
    assert (Option.is_none (A.Project_cache.current cache ~identity:id));
    assert (Option.is_none (A.Project_cache.current cache ~identity:replacement_id)))
;;

let%test_unit "one retained result is a real cross-context capacity bound" =
  Eio_main.run (fun env ->
    let fixture = Fixtures.make () in
    let base = Fixtures.compose fixture |> Support.ok in
    let base = revise_build fixture base "let main input = Task.pure(input)" in
    let first_id, one_context = initial_context ~max_contexts:2 base fixture in
    let second_id =
      C.Identity.create
        ~principal:"analysis-principal"
        ~host_incarnation:"analysis-host"
        ~connection_lease:"analysis-lease"
        ~nonce:"analysis-second"
      |> Or_error.ok_exn
    in
    let contexts =
      C.admit
        one_context
        ~identity:second_id
        ~base
        ~editable_sources:[ S.id fixture.main ]
        ~checkpoint:ignore
      |> context_ok
    in
    let policy = policy ~max_results:1 () in
    let candidate identity =
      let inputs, receipt = validated contexts identity fixture in
      A.Project_candidate.analyze_validated
        inputs
        ~receipt
        ~policy
        ~env
        ~target:Chatml_compilation.One_off_v1
        ~checkpoint:ignore
      |> ok
    in
    let first = candidate first_id in
    let second = candidate second_id in
    let cache = A.Project_cache.create ~contexts ~policy in
    let cache =
      A.Project_cache.publish cache first ~occupied_elsewhere:0 ~checkpoint:ignore |> ok
    in
    (match
       A.Project_cache.publish cache second ~occupied_elsewhere:0 ~checkpoint:ignore
     with
     | Error A.Error.Retention_limit -> ()
     | Error error -> Error.raise_s (A.Error.sexp_of_t error)
     | Ok _ -> failwith "second result exceeded retention cap");
    let remaining =
      C.close_context (A.Project_cache.contexts cache) ~identity:first_id |> context_ok
    in
    let cache = A.Project_cache.transition cache ~contexts:remaining in
    let cache =
      A.Project_cache.publish cache second ~occupied_elsewhere:0 ~checkpoint:ignore |> ok
    in
    assert (Option.is_some (A.Project_cache.current cache ~identity:second_id)))
;;

let site source start_byte end_byte =
  let location =
    S.locate source ~start_byte ~end_byte ~checkpoint:ignore |> Or_error.ok_exn
  in
  Chatml_diagnostic.Site.create source ~span:(S.Location.span location) ~checkpoint:ignore
  |> Or_error.ok_exn
;;

let%test_unit "diagnostic projection keeps primary and related exact owners" =
  let primary_source = Support.capture ~namespace:"diagnostic" "bad.chatml" "a\255b" in
  let related_source =
    Support.capture ~namespace:"diagnostic" "context.chatml" "x\r\ny"
  in
  let primary = site primary_source 1 2 in
  let related = site related_source 2 2 in
  let native =
    Chatml_diagnostic.create
      ~site:primary
      ~related:[ Chatml_diagnostic.Related.create related ~message:"related" ]
      "bad source"
  in
  let result =
    A.Diagnostic_projection.project_native native ~policy:(policy ()) ~checkpoint:ignore
  in
  let projected = A.Diagnostic_projection.primary result |> Option.value_exn in
  assert (
    S.Id.equal
      (S.id primary_source)
      (S.id (Chatml_diagnostic.Site.source (A.Site_projection.native projected))));
  (match A.Site_projection.byte_range projected with
   | Ok _ -> ()
   | Error _ -> failwith "malformed UTF-8 removed exact byte range");
  (match A.Site_projection.utf16_range projected with
   | Error
       (A.Site_projection.Error.Position (Chatml_editor_position.Error.Invalid_utf8 _)) ->
     ()
   | _ -> failwith "malformed UTF-8 was silently projected as UTF-16");
  let _, related_projection = List.hd_exn (A.Diagnostic_projection.related result) in
  assert (
    S.Id.equal
      (S.id related_source)
      (S.id (Chatml_diagnostic.Site.source (A.Site_projection.native related_projection))));
  (match A.Site_projection.byte_range related_projection with
   | Error (A.Site_projection.Error.Position (Chatml_editor_position.Error.Inside_crlf _))
     -> ()
   | _ -> failwith "CRLF midpoint was silently converted");
  let site_limited =
    A.Diagnostic_projection.project_native
      native
      ~policy:(policy ~max_projected_sites:1 ())
      ~checkpoint:ignore
  in
  assert (A.Diagnostic_projection.sites_truncated site_limited);
  assert (List.is_empty (A.Diagnostic_projection.related site_limited));
  assert (Int.equal (List.length (A.Diagnostic_projection.native site_limited).related) 1);
  let many_related =
    Chatml_diagnostic.create
      ~site:primary
      ~related:
        (List.init 8 ~f:(fun index ->
           Chatml_diagnostic.Related.create related ~message:(Int.to_string index)))
      "bad source"
  in
  assert (Int.equal (List.length many_related.related) 8);
  let projection_checkpoints native =
    let calls = ref 0 in
    let projected =
      A.Diagnostic_projection.project_native
        native
        ~policy:(policy ~max_projected_sites:1 ())
        ~checkpoint:(fun () -> incr calls)
    in
    assert (A.Diagnostic_projection.sites_truncated projected);
    !calls
  in
  [%test_eq: int] (projection_checkpoints native) (projection_checkpoints many_related);
  let source_limited =
    A.Diagnostic_projection.project_native
      native
      ~policy:(policy ~max_projection_sources:1 ())
      ~checkpoint:ignore
  in
  let _, related = List.hd_exn (A.Diagnostic_projection.related source_limited) in
  (match A.Site_projection.byte_range related with
   | Error A.Site_projection.Error.Projection_limit -> ()
   | _ -> failwith "a second source escaped aggregate index count");
  let entries_limited =
    A.Diagnostic_projection.project_native
      native
      ~policy:(policy ~max_position_index_entries:1 ())
      ~checkpoint:ignore
  in
  let _, related = List.hd_exn (A.Diagnostic_projection.related entries_limited) in
  match A.Site_projection.byte_range related with
  | Error A.Site_projection.Error.Projection_limit -> ()
  | _ -> failwith "a second source escaped aggregate index-entry count"
;;
