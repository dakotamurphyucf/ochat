open! Core
module B = Chatml_project_build
module S = Chatml_project_build_tests.Support
module C = Chatml.Chatml_compiler_contract
module H = Chatml_host_runtime
module L = Chatml.Chatml_lang

let library_namespace = "ochat.capabilities.lifecycle"
let namespace = "ochat.examples.lifecycle-composition"
let checkpoint () = ()

let library_members =
  [ ( "src/lifecycle.chatml"
    , [%blob "../../docs-src/examples/libraries/lifecycle/src/lifecycle.chatml"] )
  ; ( "src/lifecycle.chatmli"
    , [%blob "../../docs-src/examples/libraries/lifecycle/src/lifecycle.chatmli"] )
  ]
;;

let adapter_members =
  [ ( "src/lifecycle_run.chatml"
    , [%blob "../../docs-src/examples/libraries/lifecycle-run/src/lifecycle_run.chatml"] )
  ; ( "src/lifecycle_run.chatmli"
    , [%blob "../../docs-src/examples/libraries/lifecycle-run/src/lifecycle_run.chatmli"]
    )
  ]
;;

let components =
  [ ( "counter"
    , "Counter"
    , [%blob
        "../../docs-src/examples/learning/lifecycle-composition/components/counter/chatml.project.json"]
    , [ ( "src/counter.chatml"
        , [%blob
            "../../docs-src/examples/learning/lifecycle-composition/components/counter/src/counter.chatml"]
        )
      ; ( "src/counter.chatmli"
        , [%blob
            "../../docs-src/examples/learning/lifecycle-composition/components/counter/src/counter.chatmli"]
        )
      ] )
  ; ( "review"
    , "Review"
    , [%blob
        "../../docs-src/examples/learning/lifecycle-composition/components/review/chatml.project.json"]
    , [ ( "src/review.chatml"
        , [%blob
            "../../docs-src/examples/learning/lifecycle-composition/components/review/src/review.chatml"]
        )
      ; ( "src/review.chatmli"
        , [%blob
            "../../docs-src/examples/learning/lifecycle-composition/components/review/src/review.chatmli"]
        )
      ] )
  ]
;;

let example =
  [%blob "../../docs-src/examples/learning/lifecycle-composition/moderator.chatml"]
;;

let build_ok = function
  | Ok value -> value
  | Error error ->
    let message =
      match B.Error.problem error with
      | Missing_catalog id -> "missing catalog " ^ Chatml_source.Id.relative_path id
      | Unexpected_catalog id -> "unexpected catalog " ^ Chatml_source.Id.relative_path id
      | Namespace_conflict name -> "namespace conflict " ^ name
      | Invalid_repository_member id ->
        "invalid repository member " ^ Chatml_source.Id.relative_path id
      | Invalid_acquisition_key -> "invalid acquisition key"
      | Invalid_dependency name -> "invalid dependency " ^ name
      | Missing_module reference ->
        "missing module " ^ Chatml_project.Module_ref.name reference
      | Compiler_requirement -> "compiler requirement mismatch"
      | Invalid_entry_selection -> "invalid entry selection"
      | Invalid_lock_identity -> "invalid lock identity"
      | Lock_mismatch -> "lock mismatch"
      | Missing_capture id -> "missing capture " ^ Chatml_source.Id.relative_path id
      | Duplicate_capture id -> "duplicate capture " ^ Chatml_source.Id.relative_path id
      | Needs_update -> "needs explicit update"
      | Limit_exceeded -> "limit exceeded"
      | Accounting_overflow -> "accounting overflow"
      | Declaration error ->
        Sexp.to_string_hum
          (Document_schema.Error.sexp_of_t (Chatml_project.Error.details error))
      | Catalog error ->
        Sexp.to_string_hum
          (Chatml_catalog.Error.sexp_of_problem (Chatml_catalog.Error.problem error))
      | Prepared error -> error.message
    in
    failwith message
;;

let build ~target ~entry ~example_modules =
  let contract = C.for_target target ~checkpoint in
  let library =
    S.capture
      ~namespace:library_namespace
      "chatml.project.json"
      [%blob "../../docs-src/examples/libraries/lifecycle/chatml.project.json"]
  in
  let environment = "pure" in
  let library_catalog =
    S.catalog
      library
      ~request:(Library { environment = Some environment })
      ~members:
        (List.map library_members ~f:(fun (path, contents) ->
           S.capture ~namespace:library_namespace path contents))
  in
  let declaration =
    if example_modules
    then
      [%blob "../../docs-src/examples/learning/lifecycle-composition/chatml.project.json"]
    else
      sprintf
        {|{"schema_version":1,"namespace":%S,"dependencies":{"lifecycle":{"source":{"kind":"local","path":"../lifecycle"},"project":"chatml.project.json","environment":%S}},"environments":{"default":{"modules":{"Lifecycle":"lifecycle.Lifecycle"}}}}|}
        namespace
        environment
  in
  let root_declaration = S.capture ~namespace "chatml.project.json" declaration in
  let root =
    S.catalog root_declaration ~request:(Library { environment = None }) ~members:[]
  in
  let library_input =
    B.Catalog_input.create
      library_catalog
      ~origin:Local
      ~acquisition_key:"lifecycle"
      ~dependencies:[]
  in
  let dependency =
    Chatml_project.Lock.Dependency_selection.create
      ~name:"lifecycle"
      ~declaration:(Chatml_source.id library)
      ~checkpoint
    |> Or_error.ok_exn
  in
  let adapter_inputs =
    if not example_modules
    then []
    else (
      let adapter_namespace = "ochat.capabilities.lifecycle-run" in
      let declaration =
        S.capture
          ~namespace:adapter_namespace
          "chatml.project.json"
          [%blob "../../docs-src/examples/libraries/lifecycle-run/chatml.project.json"]
      in
      let catalog =
        S.catalog
          declaration
          ~request:(Library { environment = Some "moderator" })
          ~members:
            (List.map adapter_members ~f:(fun (path, contents) ->
               S.capture ~namespace:adapter_namespace path contents))
      in
      let selected =
        Chatml_project.Lock.Dependency_selection.create
          ~name:"lifecycle_run"
          ~declaration:(Chatml_source.id declaration)
          ~checkpoint
        |> Or_error.ok_exn
      in
      [ ( selected
        , B.Catalog_input.create
            catalog
            ~origin:Local
            ~acquisition_key:"lifecycle-run"
            ~dependencies:[ dependency ] )
      ])
  in
  let component_inputs =
    if not example_modules
    then []
    else
      List.map components ~f:(fun (name, _module_name, declaration, members) ->
        let component_namespace = namespace ^ "." ^ name in
        let declaration =
          S.capture ~namespace:component_namespace "chatml.project.json" declaration
        in
        let catalog =
          S.catalog
            declaration
            ~request:(Library { environment = None })
            ~members:
              (List.map members ~f:(fun (path, contents) ->
                 S.capture ~namespace:component_namespace path contents))
        in
        let selected =
          Chatml_project.Lock.Dependency_selection.create
            ~name
            ~declaration:(Chatml_source.id declaration)
            ~checkpoint
          |> Or_error.ok_exn
        in
        ( selected
        , B.Catalog_input.create
            catalog
            ~origin:Local
            ~acquisition_key:name
            ~dependencies:[ dependency ] ))
  in
  let root_input =
    B.Catalog_input.create
      root
      ~origin:Local
      ~acquisition_key:"example"
      ~dependencies:(dependency :: List.map (adapter_inputs @ component_inputs) ~f:fst)
  in
  let limits = S.limits () in
  let selection_gate, _ =
    B.Selection_gate.create Initial_prepare ~limits ~checkpoint |> build_ok
  in
  let build =
    B.compose
      root_input
      ~catalogs:(library_input :: List.map (adapter_inputs @ component_inputs) ~f:snd)
      ~entry:(Captured (S.capture ~namespace "entry.chatml" entry))
      ~selection_gate
      ~contract
      ~lock_id:(S.id ~namespace "selected.lock.json")
      ~limits
      ~checkpoint
    |> build_ok
  in
  let program =
    Chatml_linked_program.compile
      (B.prepared build)
      ~contract
      ~max_dependency_depth:16
      ~checkpoint
    |> Result.map_error ~f:(fun failure ->
      (Chatml_linked_program.Failure.diagnostic failure).message)
    |> Result.ok_or_failwith
  in
  build, program
;;

let runtime surface = H.default_runtime_config ~surface ()

let run program input =
  H.run_prepared_entrypoint_detailed
    (runtime Chatml.Chatml_extension_surface.one_off_v1)
    program
    ~entrypoint:"main"
    ~arguments:[ Chatml.Chatml_value_codec.jsonaf_to_value input ]
    ()
  |> Result.map_error ~f:(fun diagnostic -> diagnostic.Chatml_diagnostic.message)
  |> Result.ok_or_failwith
  |> Chatml.Chatml_value_codec.value_to_jsonaf_exn
;;
