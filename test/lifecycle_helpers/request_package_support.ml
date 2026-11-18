open! Core
module B = Chatml_project_build
module S = Chatml_project_build_tests.Support
module Contract = Chatml.Chatml_compiler_contract

let test_project ~namespace ~module_name =
  sprintf
    {|{"schema_version":1,"namespace":%S,"dependencies":{"requests":{"source":{"kind":"local","path":"../requests"},"project":"chatml.project.json","environment":"pure"}},"environments":{"default":{"modules":{"Requests":%S}}}}|}
    namespace
    ("requests." ^ module_name)
;;

let compile
      ~library_namespace
      ~library_source
      ~members
      ~namespace
      ~project
      ~entry_path
      ~target
      entry
  =
  let library =
    S.capture ~namespace:library_namespace "chatml.project.json" library_source
  in
  let catalog =
    S.catalog
      library
      ~request:(Library { environment = Some "pure" })
      ~members:
        (List.map members ~f:(fun (path, text) ->
           S.capture ~namespace:library_namespace path text))
  in
  let declaration = S.capture ~namespace "chatml.project.json" project in
  let root =
    S.catalog declaration ~request:(Library { environment = None }) ~members:[]
  in
  let dependency =
    Chatml_project.Lock.Dependency_selection.create
      ~name:"requests"
      ~declaration:(Chatml_source.id library)
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let root =
    B.Catalog_input.create
      root
      ~origin:Local
      ~acquisition_key:"example"
      ~dependencies:[ dependency ]
  in
  let library =
    B.Catalog_input.create
      catalog
      ~origin:Local
      ~acquisition_key:"requests"
      ~dependencies:[]
  in
  let contract = Contract.for_target target ~checkpoint:ignore in
  let limits = S.limits () in
  let gate, _ =
    B.Selection_gate.create Initial_prepare ~limits ~checkpoint:ignore |> S.ok
  in
  let built =
    B.compose
      root
      ~catalogs:[ library ]
      ~entry:(Captured (S.capture ~namespace entry_path entry))
      ~selection_gate:gate
      ~contract
      ~lock_id:(S.id ~namespace "selected.lock.json")
      ~limits
      ~checkpoint:ignore
    |> S.ok
  in
  Chatml_linked_program.compile
    (B.prepared built)
    ~contract
    ~max_dependency_depth:16
    ~checkpoint:ignore
;;

let ok result =
  Result.map_error result ~f:(fun failure ->
    let diagnostic = Chatml_linked_program.Failure.diagnostic failure in
    Chatml_diagnostic.render
      ~label:"checked request package"
      ~checkpoint:ignore
      diagnostic)
  |> Result.ok_or_failwith
;;
