open! Core
module P = Agent_protocol
module S = Agent_server
module Service = S.Authoring_context_service
module Host_policy = S.Authoring_installation.Host_policy

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "trace source fixture admission", (error : P.Error.t)]
;;

let admitted = function
  | Ok value -> value
  | Error _ -> failwith "trace source fixture lower limits are invalid"
;;

type t =
  { primary : string
  ; dependency : string
  ; dependency_allowed : bool ref
  ; project : Source.Portable_path.t
  ; authoring : Host_policy.t
  ; capture : Service.Developer.Target_policy.t
  }

let analysis_policy () =
  let recording =
    Chatml_typechecker.Semantic.Limits.create
      ~max_facts:20_000
      ~max_edges:40_000
      ~max_label_bytes:400_000
      ~max_work_steps:400_000
    |> admitted
  in
  let type_graph =
    Chatml_semantics.Frozen.Limits.create
      ~max_nodes:20_000
      ~max_edges:40_000
      ~max_label_bytes:400_000
    |> admitted
  in
  let semantic =
    Chatml_semantics.Limits.create
      ~max_input_bytes:(1024 * 1024)
      ~recording
      ~type_graph
      ~max_published_facts:20_000
      ~max_published_edges:40_000
      ~max_published_label_bytes:400_000
      ~max_work_steps:400_000
    |> admitted
  in
  Chatml_authoring_analysis.Policy.create
    ~compilation:Chatml_compilation.default_limits
    ~semantic
    ~max_dependency_depth:8
    ~max_results:8
    ~max_projected_diagnostics:32
    ~max_projected_sites:128
    ~max_projection_sources:16
    ~max_position_index_entries:1024
    ~max_projection_source_bytes:(1024 * 1024)
  |> admitted
;;

let create ~env ~fixture =
  let directory = Filename.dirname (Config_fixture.prompt_path fixture) in
  let primary = Filename.concat directory "primary" in
  let dependency = Filename.concat directory "dependency" in
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / primary);
  Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / dependency);
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / Filename.concat primary "dependent.project.json")
    {|{"schema_version":1,"namespace":"primary","dependencies":{"dep":{"source":{"kind":"local","path":"../dependency"},"project":"chatml.project.json"}},"default_environment":"default","environments":{"default":{"modules":{"Dep":"dep.Util"}}},"entries":{"main":{"source":"main.chatml","target":"one_off_v1"}}}|};
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / Filename.concat dependency "chatml.project.json")
    {|{"schema_version":1,"namespace":"dep","modules":{"Util":{"implementation":"util.chatml"}}}|};
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / Filename.concat dependency "util.chatml")
    "let value = 1\n";
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / Filename.concat primary "main.chatml")
    "let dependency_value = Dep.value\nlet main input = Task.pure(input)\n";
  let dependency_allowed = ref true in
  let project = Source.Portable_path.create "dependent.project.json" |> admitted in
  let selected =
    let contracts =
      S.Authoring_standard_contracts.create
        ~targets:[ One_off_v1 ]
        ~max_identity_bytes:(16 * 1024 * 1024)
        ~max_work_steps:1_000_000
        ~checkpoint:ignore
      |> admitted
    in
    S.Authoring_standard_contracts.find contracts One_off_v1
    |> Option.value_exn
    |> Service.Developer.Selected_contract.of_standard_entry
    |> ok
  in
  let requested_project root member =
    Int.equal (S.Authoring_source_host.Root.index root) 0
    && String.equal (Source.Portable_path.to_string member) "dependent.project.json"
  in
  let requested_entry = function
    | Chatml_project.Selection.Entry { name = "main"; environment = None } -> true
    | Entry _ | Library _ -> false
  in
  let capture =
    Service.Developer.Target_policy.create
      ~local_project:(fun actor ~root ~project:member ~selection ->
        if
          Operator_authorization.is_current actor
          && requested_project root member
          && requested_entry selection
        then Some selected
        else None)
      ~local_source:(fun _ ~root:_ ~member:_ -> None)
      ~authoring_project:(fun _ ~origin:_ ~context:_ ~current:_ -> None)
      ~document_project:
        (fun
          _
          ~origin:_
          ~context:_
          ~current:_
          ~parsed:_
          ~declaration:_
          ~build:_
          ~metadata:_ -> None)
      ~document_source:
        (fun
          _ ~origin:_ ~context:_ ~current:_ ~parsed:_ ~declaration:_ ~script:_ -> None)
  in
  let authoring =
    Host_policy.create
      ~core:true
      ~source:
        (Some
           (Host_policy.Source.create
              ~read:
                (S.Authoring_source_host.Read_policy.create
                   ~authorize:(fun actor ~root_index ->
                     Operator_authorization.is_current actor
                     && (Int.equal root_index 0
                         || (Int.equal root_index 1 && !dependency_allowed))))
              ~document:S.Authoring_source_host.Document_policy.deny_all
              ~limits:
                (S.Authoring_source_host.Limits.create
                   ~max_contexts:8
                   ~max_context_requests:32
                   ~max_manifest_entries:16
                   ~max_browse_entries:32
                   ~max_read_bytes:65536
                   ~max_context_read_bytes:262144
                 |> admitted)))
      ~upload:None
      ~supplied_document:None
      ~selection:
        (Service.Selection_policy.create
           ~project_contract:(fun actor ~root ~project:member ~selection ->
             if
               Operator_authorization.is_current actor
               && requested_project root member
               && P.Authoring.Context.Selection.equal
                    selection
                    (P.Authoring.Context.Selection.create
                       ~target:(Entry "main")
                       ~environment:None
                     |> ok)
             then Some (Service.Developer.Selected_contract.contract selected)
             else None)
           ~document_static:(fun _ ~requested_environment:_ -> None))
      ~context_limits:
        (Chatml_authoring_context.Limits.create
           ~max_contexts:8
           ~max_sources:32
           ~max_open_documents:4
           ~max_document_bytes:65536
           ~max_effective_input_bytes:262144
           ~max_context_source_bytes:262144
           ~max_total_source_bytes:(1024 * 1024)
         |> admitted)
      ~analysis_policy:(analysis_policy ())
      ~protocol_limits:
        (P.Authoring.Limits.create
           ~max_sources:32
           ~max_source_bytes:65536
           ~max_context_source_bytes:262144
           ~max_request_bytes:(1024 * 1024)
           ~max_result_bytes:(1024 * 1024)
           ~max_path_candidates:32
         |> ok)
      ~service_limits:
        (Service.Limits.create
           ~max_connections:16
           ~max_active_requests_per_connection:4
           ~max_roots_per_connection:8
           ~max_response_bytes:(1024 * 1024)
           ~max_provisional_results:8
         |> admitted)
      ~semantic:None
      ~assistance:None
      ~initialize_limits:S.Daemon.default_options.protocol_limits
    |> ok
  in
  { primary; dependency; dependency_allowed; project; authoring; capture }
;;

let authoring_policy t = t.authoring
let capture_policy t = t.capture
let revoke_dependency t = t.dependency_allowed := false
let restore_dependency t = t.dependency_allowed := true
let project t = t.project
let primary_root t = t.primary
let dependency_root t = t.dependency

let install_sources t ~env ~fixture =
  let original = Config_fixture.configuration fixture () in
  let anchor = "(unsafe_allow_unauthenticated_remote_http false)))" in
  if not (String.is_substring original ~substring:anchor)
  then failwith "trace source fixture lost server configuration anchor";
  let roots =
    sprintf
      "(project_sources ((external_roots (%s %s))))\n  "
      (Sexp.to_string_mach (Sexp.Atom t.primary))
      (Sexp.to_string_mach (Sexp.Atom t.dependency))
  in
  let configured =
    String.substr_replace_first original ~pattern:anchor ~with_:(roots ^ anchor)
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / Config_fixture.config_path fixture)
    configured
;;
