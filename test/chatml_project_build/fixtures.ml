open Core
module S = Chatml_source
module P = Chatml_project
module C = Chatml_catalog
module B = Chatml_project_build
module R = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

type t =
  { contract : Contract.t
  ; root : B.Catalog_input.t
  ; catalogs : B.Catalog_input.t list
  ; declaration : S.t
  ; main : S.t
  ; helper : S.t
  ; util : S.t
  ; entry : S.t
  ; outside_entry : bool
  }

let contract t = t.contract
let root t = t.root
let catalogs t = t.catalogs
let declaration t = t.declaration
let main t = t.main
let helper t = t.helper
let util t = t.util
let lock_id = Support.id ~namespace:"root" "selected.lock.json"

let make
      ?contract
      ?(library = false)
      ?(outside_entry = false)
      ?(reverse_sources = false)
      ()
  =
  let contract =
    Option.value contract ~default:(Contract.for_target One_off_v1 ~checkpoint:ignore)
  in
  let entry_path = if outside_entry then "runner.chatml" else "src/main.chatml" in
  let declaration =
    Support.capture
      ~namespace:"root"
      "chatml.project.json"
      (sprintf
         {|{"schema_version":1,"namespace":"root","source_dirs":["src"],"modules":{"Alias":{"implementation":"src/main.chatml"}},"dependencies":{"lib":{"source":{"kind":"local","path":"../lib"},"project":"chatml.project.json"}},"environments":{"default":{"modules":{"Again":"lib.Util","Imported":"lib.Util","Local":"self.Main"},"requires":{"targets":["%s"]}},"empty":{"modules":{}}},"file_environments":{"src/helper.chatml":"empty"},"entries":{"main":{"source":"%s","target":"%s"}},"capabilities":["shared.read","root.write"]}|}
         (Contract.target_name contract)
         entry_path
         (Contract.target_name contract))
  in
  let main = Support.capture ~namespace:"root" "src/main.chatml" "let main = 0" in
  let helper = Support.capture ~namespace:"root" "src/helper.chatml" "let value = 2" in
  let entry =
    if outside_entry
    then Support.capture ~namespace:"root" entry_path "let main = 3"
    else main
  in
  let root_members =
    if outside_entry then [ main; helper; entry ] else [ main; helper ]
  in
  let order members = if reverse_sources then List.rev members else members in
  let root_catalog =
    Support.catalog
      declaration
      ~request:
        (if library
         then Library { environment = None }
         else Entry { name = "main"; environment = None })
      ~members:(order root_members)
  in
  let lib_declaration =
    Support.capture
      ~namespace:"lib"
      "chatml.project.json"
      {|{"schema_version":1,"namespace":"lib","source_dirs":["src"],"environments":{"own":{"modules":{"Local":"self.Util"}},"empty":{"modules":{}}},"default_environment":"own","file_environments":{"src/util.chatmli":"empty"},"capabilities":["shared.read","dep.read"]}|}
  in
  let util = Support.capture ~namespace:"lib" "src/util.chatml" "let value = 1" in
  let interface = Support.capture ~namespace:"lib" "src/util.chatmli" "val value : int" in
  let lib_catalog =
    Support.catalog
      lib_declaration
      ~request:(Library { environment = None })
      ~members:(order [ util; interface ])
  in
  let dependency =
    P.Lock.Dependency_selection.create
      ~name:"lib"
      ~declaration:(S.id lib_declaration)
      ~checkpoint:ignore
    |> Or_error.ok_exn
  in
  let input catalog dependencies key =
    B.Catalog_input.create catalog ~origin:Local ~acquisition_key:key ~dependencies
  in
  { contract
  ; root = input root_catalog [ dependency ] "root-attempt"
  ; catalogs = [ input lib_catalog [] "lib-attempt" ]
  ; declaration
  ; main
  ; helper
  ; util
  ; entry
  ; outside_entry
  }
;;

let compose
      ?(checkpoint = ignore)
      ?(limits = Support.limits ())
      ?(entry = B.Entry.Declared)
      ?catalogs
      t
  =
  let open Result.Let_syntax in
  let%bind gate, _ = B.Selection_gate.create Initial_prepare ~limits ~checkpoint in
  B.compose
    t.root
    ~catalogs:(Option.value catalogs ~default:t.catalogs)
    ~entry
    ~selection_gate:gate
    ~contract:t.contract
    ~lock_id
    ~limits
    ~checkpoint
;;

let scope_targets prepared id =
  let scope = R.find_scope prepared id |> Option.value_exn in
  List.map (R.Scope.bindings scope) ~f:(fun binding ->
    let id = R.Module_id.implementation binding.module_id in
    binding.name, S.Id.package_namespace id, S.Id.relative_path id)
;;

let assert_model t build =
  let prepared = B.prepared build in
  [%test_eq: int] (List.length (R.sources prepared)) (if t.outside_entry then 8 else 7);
  [%test_eq: int] (List.length (R.definitions prepared)) 3;
  assert (S.Id.equal (R.entry prepared) (S.id t.entry));
  [%test_eq: (string * string * string) list]
    (scope_targets prepared (S.id t.main))
    [ "Again", "lib", "src/util.chatml"
    ; "Imported", "lib", "src/util.chatml"
    ; "Local", "root", "src/main.chatml"
    ];
  [%test_eq: (string * string * string) list] (scope_targets prepared (S.id t.helper)) [];
  [%test_eq: (string * string * string) list]
    (scope_targets prepared (S.id t.util))
    [ "Local", "lib", "src/util.chatml" ];
  [%test_eq: (string * string * string) list]
    (scope_targets prepared (Support.id ~namespace:"lib" "src/util.chatmli"))
    [];
  [%test_eq: string list]
    (P.Lock.required_tools (B.lock build))
    [ "dep.read"; "root.write"; "shared.read" ];
  let definition =
    R.find_definition prepared (R.Module_id.of_implementation (S.id t.util))
    |> Option.value_exn
  in
  assert (
    Option.equal
      S.Id.equal
      (R.Definition.interface definition)
      (Some (Support.id ~namespace:"lib" "src/util.chatmli")));
  List.iter
    (P.Lock.catalogs (B.lock build))
    ~f:(fun catalog ->
      assert (
        not
          (List.exists (P.Lock.Catalog.sources catalog) ~f:(fun source ->
             S.Id.equal (P.Lock.Source_entry.id source) lock_id))));
  let accounting = R.accounting prepared in
  [%test_eq: int]
    (R.Accounting.source_count accounting)
    (List.length (R.sources prepared));
  [%test_eq: int]
    (R.Accounting.source_bytes accounting)
    (List.sum (module Int) (R.sources prepared) ~f:S.byte_length);
  [%test_eq: int]
    (R.Accounting.input_bytes accounting)
    (R.Accounting.source_bytes accounting + R.Accounting.metadata_bytes accounting);
  assert (B.Accounting.work_steps (B.accounting build) > 0)
;;
