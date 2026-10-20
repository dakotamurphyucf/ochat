open! Core
module Build = Chatml_project_build
module Project_fixture = Chatml_project_build_tests.Authoring_bundle_tests
module Capture = Chatml_project_build_tests.Support
module Target = Chatml.Chatml_compiler_contract.Target

let create ~contents () =
  let contract =
    Chatml.Chatml_compiler_contract.for_target Target.One_off_v1 ~checkpoint:ignore
  in
  let declaration =
    Capture.capture
      ~namespace:"workflow-upload"
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"workflow-upload","source_dirs":["src"],"default_environment":"default","environments":{"default":{"modules":{"Util":"self.Util"}}},"entries":{"main":{"source":"src/main.chatml","target":"one_off_v1"}}}|}
  in
  let main = Capture.capture ~namespace:"workflow-upload" "src/main.chatml" contents in
  let util =
    Capture.capture
      ~namespace:"workflow-upload"
      "src/util.chatml"
      "let marker = \"é\"\nlet value = 8\n"
  in
  let catalog =
    Capture.catalog
      declaration
      ~request:(Entry { name = "main"; environment = None })
      ~members:[ main; util ]
  in
  let root =
    Build.Catalog_input.create
      catalog
      ~origin:Local
      ~acquisition_key:"workflow-upload"
      ~dependencies:[]
  in
  let limits = Capture.limits () in
  let gate, _ =
    Build.Selection_gate.create Initial_prepare ~limits ~checkpoint:ignore |> Capture.ok
  in
  let original =
    Build.compose
      root
      ~catalogs:[]
      ~entry:Declared
      ~selection_gate:gate
      ~contract
      ~lock_id:(Capture.id ~namespace:"workflow-upload" "original.lock.json")
      ~limits
      ~checkpoint:ignore
    |> Capture.ok
  in
  let sources = Capture.without_lock original in
  let project =
    Project_fixture.project
      original
      ~source_members:sources
      ~selection:(Project_fixture.selection (Entry "main"))
      ~entry:Declared
      ~mode:
        (Compose
           { selected_previous_lock = None
           ; generated_lock =
               Capture.id ~namespace:"workflow-upload" "installed.lock.json"
           })
  in
  Project_fixture.manifest project sources, sources
;;
