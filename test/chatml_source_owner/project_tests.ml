open Core
open Support
module P = Chatml_project_prepare
module B = Chatml_project_build
module S = Chatml_source
module R = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

let path text = Source.Portable_path.create text |> Or_error.ok_exn

let root_limits () =
  Chatml_local_roots.Limits.create
    ~max_locator_bytes:4096
    ~max_locator_components:64
    ~max_anchors:8
    ~max_retained_roots:32
    ~max_work_steps:10_000
  |> Or_error.ok_exn
;;

let composition_limits () =
  B.Limits.create
    ~max_work_steps:1_000_000
    ~max_input_bytes:(Some 1_000_000)
    ~document:Document_schema.Limits.default
  |> Or_error.ok_exn
;;

let policy () =
  P.Policy.create
    ~roots:(root_limits ())
    ~local:
      (Chatml_local_capture.Limits.create
         ~max_directory_entries:100
         ~max_sources:100
         ~max_source_bytes:100_000
         ~max_total_source_bytes:1_000_000
         ~max_path_components:32
         ~max_work_steps:1_000_000
       |> Or_error.ok_exn)
    ~git_objects:
      (Chatml_git_catalog.Limits.create
         ~max_object_bytes:100_000
         ~max_total_object_bytes:1_000_000
         ~max_objects:100
         ~max_tree_entries:100
         ~max_sources:100
         ~max_source_bytes:100_000
         ~max_total_source_bytes:1_000_000
         ~max_path_components:64
         ~max_work_steps:1_000_000
         ~document:Document_schema.Limits.default
       |> Or_error.ok_exn)
    ~git_native:
      (Chatml_git_host.Limits.create
         ~metadata_file_bytes:100_000
         ~pack_file_bytes:1_000_000
         ~max_acquisition_payload_bytes:10_000_000
         ~max_request_bytes:100_000
         ~max_output_bytes:1_000_000
         ~max_work_steps:100_000
         ~wall_time:(Time_ns.Span.of_sec 60.)
         ~cpu_seconds_per_process:10
         ~memory_bytes_per_process:100_000_000
         ~open_files_per_process:64
       |> Or_error.ok_exn)
    ~composition:(composition_limits ())
;;

let limits ?(prepared = 1_000_000) ?(sources = 100) ?(bytes = 1_000_000) () =
  P.Limits.create
    ~max_packages:100
    ~max_dependency_visits:100
    ~max_source_count:sources
    ~max_source_bytes:bytes
    ~max_local_directory_entries:100
    ~max_git_objects:100
    ~max_git_object_bytes:1_000_000
    ~max_git_tree_entries:100
    ~max_native_request_bytes:100_000
    ~max_native_output_bytes:1_000_000
    ~max_acquisition_payload_reserved:10_000_000
    ~max_work_steps:10_000_000
    ~max_prepared_input_bytes:prepared
    ~wall_time:(Time_ns.Span.of_sec 60.)
  |> Or_error.ok_exn
;;

let%test_unit
    "project source preparation borrows the actual retained original root, without \
     initializers"
  =
  with_workspace (fun env parent root ->
    save root "root.chatmd" "<user>original root</user>";
    save
      root
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"example","modules":{"Main":{"implementation":"main.chatml"}},"entries":{"main":{"source":"main.chatml","target":"one_off_v1"}},"capabilities":["missing.tool"]}|};
    let original =
      "let poison = fail(\"initializer must remain inert\")\n\
       let main ctx input = Task.pure(())"
    in
    save root "main.chatml" original;
    let definition = definition root in
    Eio.Switch.run (fun sw ->
      let owner =
        Eio.Switch.run (fun original_sw ->
          let original = Eio.Path.open_dir ~sw:original_sw Eio.Path.(root / ".") in
          open_owner sw original definition)
      in
      let captured =
        Owner.with_scope owner ~f:(fun scope -> capture env scope definition) |> ok
      in
      let input =
        Owner.Capture.value captured
        |> Builder.Capture.root
        |> Prompt.Chat_markdown.Parsed.root_input
      in
      let route = Owner.Capture.route captured input ~checkpoint:ignore |> ok in
      let selected_root =
        Owner.Capture.Route.local_root
          route
          ~locator:"."
          ~project:(path "chatml.project.json")
          ~max_locator_bytes:4096
          ~checkpoint:ignore
        |> ok
      in
      let lock_id =
        S.Id.create ~package_namespace:"example" ~relative_path:"selected.json" |> ok
      in
      let request =
        P.Request.create
          selected_root
          ~operation:Initial_prepare
          ~selection:(Entry { name = "main"; environment = None })
          ~entry:Declared
          ~contract:(Contract.for_target One_off_v1 ~checkpoint:ignore)
          ~lock_id
          ~max_request_bytes:4096
        |> ok
      in
      Eio.Path.rename Eio.Path.(parent / "workspace") Eio.Path.(parent / "old-workspace");
      Eio.Path.mkdir ~perm:0o700 Eio.Path.(parent / "workspace");
      save Eio.Path.(parent / "workspace") "chatml.project.json" "invalid replacement";
      let prepare scope =
        Owner.Scope.prepare_sources
          scope
          ~capture:captured
          ~requests:[ request ]
          ~additional_anchors:[]
          ~git_store:None
          ~git_grants:[]
          ~root_limits:(root_limits ())
          ~max_grants:0
          ~max_grant_bytes:0
          ~policy:(policy ())
          ~limits:(limits ())
          ~clock:env#mono_clock
          ~checkpoint:ignore
      in
      let batch = Owner.with_scope owner ~f:prepare |> ok in
      let preparation = P.Source_batch.preparations batch |> List.hd_exn in
      assert (
        List.equal
          String.equal
          (P.Source_preparation.required_tools preparation)
          [ "missing.tool" ]);
      let sources = P.Source_preparation.build preparation |> B.prepared |> R.sources in
      let main =
        List.find_exn sources ~f:(fun source ->
          String.equal (S.Id.relative_path (S.id source)) "main.chatml")
      in
      assert (String.equal (S.contents main) original);
      let foreign = open_owner sw root definition in
      error (Owner.with_scope foreign ~f:prepare) Foreign_capture))
;;
