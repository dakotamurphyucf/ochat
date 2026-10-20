open! Core
module P = Agent_protocol
module S = Agent_server
module D = P.Developer_common
module W = P.Developer_wire
module R = P.Developer_repl_wire
module T = P.Developer_trace_wire
module B = P.Authoring_bundle.Bundle

let fail message = raise_s [%sexp "Developer workflow E2E failed", (message : string)]
let require condition message = if not condition then fail message

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "Developer workflow rejected", (error : P.Error.t)]
;;

let transport_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "Developer workflow transport failed", (error : string)]
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    port)
;;

type client =
  { send : P.Envelope.t -> P.Envelope.t
  ; close : unit -> unit
  ; mutable next_id : int
  }

let call_outcome client ~method_ ~params =
  let id = P.Envelope.Request_id.of_json (`Number (Int.to_string client.next_id)) |> ok in
  client.next_id <- client.next_id + 1;
  let request = P.Envelope.request ~id ~method_ ~params () in
  match client.send request with
  | P.Envelope.Response { id = received; outcome } ->
    require (Int.equal (P.Envelope.Request_id.compare id received) 0) "request ID changed";
    outcome
  | P.Envelope.Request _ | P.Envelope.Notification _ -> fail "missing method response"
;;

let call client ~method_ ~params = call_outcome client ~method_ ~params |> ok

let authoring client request =
  let method_ = P.Authoring_wire.Request.method_name request in
  let result = call client ~method_ ~params:(P.Authoring_wire.Request.params request) in
  P.Authoring_wire.Response.of_method_and_json ~method_ result |> ok
;;

let developer client ~limits request =
  let method_ = W.Request.method_name request in
  let result = call client ~method_ ~params:(W.Request.params request) in
  W.Response.of_method_and_json ~limits ~method_ result |> ok
;;

let repl client ~limits request =
  let method_ = R.Request.method_name request in
  let result = call client ~method_ ~params:(R.Request.params request) in
  R.Response.of_method_and_json ~limits ~method_ result |> ok
;;

let http_client ~sw env fixture =
  let driver =
    Support.Http_driver.create
      ~sw
      ~env
      ~port:(Support.Config_fixture.http_port fixture)
      ~token:(Some (Support.Config_fixture.public_token fixture))
    |> transport_ok
  in
  let closed = ref false in
  { next_id = 1
  ; send =
      (fun request ->
        let response =
          Support.Http_driver.rpc_raw
            driver
            (P.Envelope.to_json request |> Jsonaf.to_string)
          |> transport_ok
        in
        if not (Int.equal response.status 200)
        then
          raise_s
            [%sexp
              "HTTP method response failed"
            , (response.status : int)
            , (String.prefix response.body 128 : string)];
        let parsed =
          Jsonaf.parse response.body |> Or_error.ok_exn |> P.Envelope.of_json |> ok
        in
        parsed)
  ; close =
      (fun () ->
        if not !closed
        then (
          closed := true;
          Exn.protect
            ~f:(fun () ->
              let response =
                Support.Http_driver.close_connection driver |> transport_ok
              in
              require
                (Int.equal response.status 200)
                "HTTP logical connection did not close")
            ~finally:(fun () -> Support.Http_driver.shutdown driver)))
  }
;;

let initialize client =
  let implementation =
    P.Initialize.Implementation.create ~name:"developer-workflow-e2e" ~version:"1" |> ok
  in
  let request =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:
        [ P.Authoring.Features.core
        ; P.Authoring.Features.semantic
        ; P.Authoring.Features.bundle
        ; D.Features.run
        ; R.Features.repl
        ; T.Features.trace
        ]
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(2 * 1024 * 1024)
      ()
    |> ok
  in
  let response =
    call
      client
      ~method_:"protocol.initialize"
      ~params:(P.Initialize.Request.to_json request)
    |> P.Initialize.Response.of_json
    |> ok
  in
  List.iter
    [ P.Authoring.Features.core
    ; P.Authoring.Features.semantic
    ; P.Authoring.Features.bundle
    ; D.Features.run
    ; R.Features.repl
    ; T.Features.trace
    ]
    ~f:(fun feature ->
      require
        (List.mem response.enabled_features feature ~equal:String.equal)
        "required workflow feature was not installed");
  response
;;

let project_source =
  {|{"schema_version":1,"namespace":"workflow","source_dirs":["src"],"default_environment":"default","environments":{"default":{"modules":{"Util":"self.Util"}},"other":{"modules":{}}},"entries":{"main":{"source":"src/main.chatml","target":"one_off_v1"}}}|}
;;

let initial_source = "let main input = Task.pure(input)\n"
let imported_source = "let marker = \"é\"\nlet value = 8\n"

let saved_source =
  "let marker = \"é\"\n\
   let dependency_value = Util.value\n\
   let main input = Task.pure(input)\n"
;;

type catalog =
  { prompt : P.Id.Prompt_definition.t
  ; workspace : P.Id.Workspace_definition.t
  }

let configuration_fragment fixture ~project_root ~local =
  let principal = Support.Config_fixture.public_principal fixture in
  let authentication_kind = if local then "embedded.local" else "http.static_bearer" in
  let principal_text = P.Id.Principal.to_string principal in
  let root_atom = Sexp.to_string_mach (Sexp.Atom project_root) in
  let project_atom = Sexp.to_string_mach (Sexp.Atom "chatml.project.json") in
  let request =
    S.Authoring_project_access_configuration.Selection.Entry
      { name = "main"; environment = None }
  in
  let source =
    S.Developer_startup_configuration.Source.Authoring_project
      (Host_project { root = project_root; project = "chatml.project.json"; request })
  in
  let uploaded =
    S.Developer_startup_configuration.Source.Authoring_project
      (Uploaded_project { environment = "default" })
  in
  let document member =
    S.Developer_startup_configuration.Source.Document_project
      { origin = Host_document { root = project_root; member }
      ; script_id = "workflow-unicode"
      ; authored_project =
          Chatmd_shell_spec.Script_project.create
            ~project:"chatml.project.json"
            ~environment:None
          |> Or_error.ok_exn
      }
  in
  let principal_selector =
    S.Developer_startup_configuration.Principal_selector.
      { principal_id = principal; authentication_kind }
  in
  let developer_config =
    S.Developer_startup_configuration.
      { selections =
          List.map
            [ source
            ; uploaded
            ; document "unicode.chatmd"
            ; document "unicode-external.chatmd"
            ]
            ~f:(fun source ->
              Selection.
                { source; target = One_off_v1; principals = [ principal_selector ] })
      ; capacity = Capacity.standard_v1
      ; model = Disabled
      ; approvals = Disabled
      ; repl = Standard_v1
      ; portable = true
      ; traces = Trace_full
      }
  in
  S.Developer_startup_configuration.validate developer_config |> ok;
  let developer_sexp =
    S.Developer_startup_configuration.sexp_of_t developer_config |> Sexp.to_string_mach
  in
  let operations = [ "upload_write"; "publish"; "bundle_read"; "rebind" ] in
  let grants =
    List.map operations ~f:(fun operation ->
      sprintf
        "((principal_id %s) (authentication_kind http.static_bearer) (operation %s))"
        principal_text
        operation)
    |> String.concat ~sep:" "
  in
  let added =
    sprintf
      {|(project_sources ((external_roots (%s))))
  (authoring_project_access
   ((read_grants (((root %s) (principal_id %s)
                   (authentication_kind http.static_bearer))))
    (selections (((root %s) (project %s)
                  (request (entry ((name main)))) (target one_off_v1))
                 ((root %s) (project %s)
                  (request (library ())) (target one_off_v1))
                 ((root %s) (project %s)
                  (request (entry ((name main) (environment other))))
                  (target one_off_v1))))))
  (authoring_upload_access
   ((mode augment_core)
    (grants (%s))
    (selections (((principal_id %s)
                  (authentication_kind http.static_bearer)
                  (request (entry ((name main))))
                  (selected_environment default) (target one_off_v1))))))
  (developer %s)
  |}
      root_atom
      root_atom
      principal_text
      root_atom
      project_atom
      root_atom
      project_atom
      root_atom
      project_atom
      grants
      principal_text
      developer_sexp
  in
  if local
  then (
    let embedded =
      String.substr_replace_all
        added
        ~pattern:"http.static_bearer"
        ~with_:authentication_kind
      |> String.substr_replace_all ~pattern:principal_text ~with_:"Embedded_operator"
    in
    embedded
    ^ "(developer_scopes (developer.check developer.fixture.run developer.live.run \
       developer.repl))")
  else added
;;

let install_config env fixture ~project_root =
  let added = configuration_fragment fixture ~project_root ~local:false in
  let original = Support.Config_fixture.configuration fixture () in
  let anchor = "(unsafe_allow_unauthenticated_remote_http false)))" in
  require (String.is_substring original ~substring:anchor) "server config anchor changed";
  let configured =
    String.substr_replace_first original ~pattern:anchor ~with_:(added ^ anchor)
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / Support.Config_fixture.config_path fixture)
    configured
;;

let phase_open_project ?environment client =
  let source_root =
    match authoring client P.Authoring_wire.Request.Roots with
    | Roots roots -> (List.hd_exn roots.roots).id
    | _ -> fail "configured Project source root is unavailable"
  in
  let selection =
    P.Authoring.Context.Selection.create ~target:(Entry "main") ~environment |> ok
  in
  let opened =
    P.Authoring.Context.Open_request.create_host_project
      ~root:source_root
      ~dependency_roots:[]
      ~project_member:"chatml.project.json"
      ~selection
    |> ok
  in
  let context =
    match authoring client (P.Authoring_wire.Request.Context_open opened) with
    | Context_open context -> context
    | _ -> fail "Project Context_open returned another method"
  in
  let expected = P.Authoring.Context.Expected.of_view context in
  let analyze =
    P.Authoring.Analysis.Analyze.Request.create
      ~context:expected
      ~target:P.Authoring.Analysis.Target.project
    |> ok
  in
  (match authoring client (P.Authoring_wire.Request.Analyze analyze) with
   | Analyze _ -> ()
   | _ -> fail "Project Analyze returned another method");
  context
;;

let phase_check_project ?selector client context ~key_name =
  let capabilities =
    call client ~method_:"developer.capabilities" ~params:(`Object [])
    |> D.Capabilities.of_json
    |> ok
  in
  let limits = D.Capabilities.limits capabilities in
  let expected = P.Authoring.Context.Expected.of_view context in
  let route =
    match selector with
    | None -> D.Route.authoring_project ~limits:(D.Limits.request limits) ~expected |> ok
    | Some selector ->
      D.Route.document_script ~limits:(D.Limits.request limits) ~expected ~selector |> ok
  in
  let run_id = P.Id.Developer_run.create () in
  let key = P.Idempotency_key.of_string key_name |> ok in
  let submit =
    D.Submit.create
      ~limits
      ~run_id
      ~key
      ~kind:P.Developer_receipt.Kind.Check
      ~route
      ~target:D.Target.One_off_v1
      ~prompt:None
      ~workspace:None
      ~entrypoint:"main"
      ~input:`Null
      ~fixtures:None
      ~requested_execution:None
      ~requested_max_output_bytes:None
    |> ok
  in
  let submitted =
    match developer client ~limits (W.Request.Submit submit) with
    | W.Response.Submit view -> view
    | _ -> fail "Project Check Submit returned another method"
  in
  let lookup = D.Lookup.create ~run_id ~key in
  let terminal =
    match developer client ~limits (W.Request.Await_terminal lookup) with
    | W.Response.Await_terminal view -> view
    | _ -> fail "Project Check Await returned another method"
  in
  require
    (P.Developer_command_id.equal
       (D.View.command_id submitted)
       (D.View.command_id terminal))
    "Check changed its durable command identity";
  terminal, lookup
;;

let check_receipt view =
  match D.View.phase view with
  | Terminal { receipt; _ } ->
    (match P.Developer_receipt.value receipt with
     | Check outcome -> outcome
     | Fixture _ | Live _ -> fail "Check carried a different receipt kind")
  | Reserved | Waiting_permission | Execution_started -> fail "Check did not settle"
;;

let phase_deny_other_environment client ~catalog =
  let context = phase_open_project ~environment:"other" client in
  (match P.Authoring.Context.basis context with
   | Project { selection; _ } ->
     require
       (String.equal selection.environment "other")
       "Authoring selected another environment"
   | Document _ -> fail "Project selection became a document");
  let capabilities =
    call client ~method_:"developer.capabilities" ~params:(`Object [])
    |> D.Capabilities.of_json
    |> ok
  in
  let limits = D.Capabilities.limits capabilities in
  let route =
    D.Route.authoring_project
      ~limits:(D.Limits.request limits)
      ~expected:(P.Authoring.Context.Expected.of_view context)
    |> ok
  in
  let fixtures =
    D.Fixture.create
      ~limits:(D.Limits.request limits)
      ~max_steps:(D.Limits.max_fixture_steps limits)
      ~max_bytes:(D.Limits.max_fixture_bytes limits)
      ~steps:[]
    |> ok
  in
  List.iter [ P.Developer_receipt.Kind.Check; Fixture_run; Live_run ] ~f:(fun kind ->
    let suffix, selected_fixtures, prompt, workspace =
      match kind with
      | Check -> "check", None, None, None
      | Fixture_run -> "fixture", Some fixtures, None, None
      | Live_run -> "live", None, Some catalog.prompt, Some catalog.workspace
    in
    let request =
      D.Submit.create
        ~limits
        ~run_id:(P.Id.Developer_run.create ())
        ~key:(P.Idempotency_key.of_string ("workflow-other-" ^ suffix) |> ok)
        ~kind
        ~route
        ~target:One_off_v1
        ~prompt
        ~workspace
        ~entrypoint:"main"
        ~input:`Null
        ~fixtures:selected_fixtures
        ~requested_execution:None
        ~requested_max_output_bytes:None
      |> ok
    in
    let wire = W.Request.Submit request in
    match
      call_outcome
        client
        ~method_:(W.Request.method_name wire)
        ~params:(W.Request.params wire)
    with
    | Error { code = P.Error.Permission_denied; _ } -> ()
    | Error error ->
      raise_s [%sexp "ungranted environment returned another error", (error : P.Error.t)]
    | Ok _ -> fail "Authoring environment selection granted Developer execution")
;;

let phase_edit_and_save client context ~project_path =
  let original_expected = P.Authoring.Context.Expected.of_view context in
  let inventory_request =
    P.Authoring.Source_read.List_request.create
      ~context:original_expected
      ~package_namespace:None
      ~directory:None
    |> ok
  in
  let inventory =
    match authoring client (P.Authoring_wire.Request.Source_list inventory_request) with
    | Source_list inventory -> P.Authoring.Source_read.List_result.sources inventory
    | _ -> fail "Project Source_list returned another method"
  in
  let source =
    inventory
    |> List.find ~f:(fun source ->
      String.equal
        (Chatml_source.Id.relative_path (P.Authoring.Source.id source))
        "src/main.chatml")
    |> Option.value_or_thunk ~default:(fun () ->
      let members =
        inventory
        |> List.map ~f:(fun source ->
          Chatml_source.Id.relative_path (P.Authoring.Source.id source))
      in
      raise_s [%sexp "Project main source was not editable", (members : string list)])
  in
  let version_request =
    P.Authoring.Document.Version_request.create
      ~context:original_expected
      ~source_id:(P.Authoring.Source.id source)
    |> ok
  in
  let version =
    match
      authoring client (P.Authoring_wire.Request.Document_version version_request)
    with
    | Document_version version -> version
    | _ -> fail "Document_version returned another method"
  in
  let edited_source = "let marker = \"é\"\nlet main input = Task.pure(missing)\n" in
  let replace =
    P.Authoring.Document.Replace_request.create
      ~expected:version
      ~next_editor_revision:1
      ~contents:edited_source
    |> ok
  in
  let edited_result =
    match authoring client (P.Authoring_wire.Request.Document_replace replace) with
    | Document_replace result -> result
    | _ -> fail "Document_replace returned another method"
  in
  let edited = edited_result.P.Authoring.Document.Result.context in
  require
    Int.(P.Authoring.Context.generation edited > P.Authoring.Context.generation context)
    "editor replacement did not advance the context generation";
  let stale_analyze =
    P.Authoring.Analysis.Analyze.Request.create
      ~context:original_expected
      ~target:P.Authoring.Analysis.Target.project
    |> ok
  in
  let method_ = P.Authoring_wire.Request.method_name (Analyze stale_analyze) in
  (match
     call_outcome
       client
       ~method_
       ~params:(P.Authoring_wire.Request.params (Analyze stale_analyze))
   with
   | Error ({ code = P.Error.Conflict; data = `Object fields; _ } as error) ->
     (match List.Assoc.find fields ~equal:String.equal "authoring_reason" with
      | Some (`String "stale_context") -> ()
      | Some _ | None -> raise_s [%sexp "stale Expected lost reason", (error : P.Error.t)])
   | Error error ->
     raise_s [%sexp "stale Authoring Expected had another result", (error : P.Error.t)]
   | Ok _ -> fail "stale Authoring Expected was accepted");
  let bad_check, _ =
    phase_check_project client edited ~key_name:"workflow-edited-check"
  in
  (match check_receipt bad_check with
   | Rejected { location = Some location; _ } ->
     require
       (Chatml_source.Id.equal location.source (P.Authoring.Source.id source))
       "Check diagnostic lost the edited Project member";
     require
       (Chatml_source.Digest.equal
          location.digest
          (Chatml_source.Digest.of_contents edited_source ~checkpoint:Fn.id))
       "Check diagnostic lost the edited digest";
     require
       (Int.equal
          location.start_byte
          (String.substr_index_exn edited_source ~pattern:"missing")
        && Int.equal
             location.end_byte
             (String.substr_index_exn edited_source ~pattern:"missing"
              + String.length "missing")
        && Int.equal location.source_bytes (String.length edited_source))
       "Check diagnostic used character offsets instead of bytes"
   | Rejected { location = None; _ } -> fail "Check lost edited source location"
   | Ready | Interrupted | Cancelled -> fail "invalid edited source passed Check");
  let incomplete_source = "let marker = \"é\"\nlet main input = Task.pure(inpu" in
  let incomplete_replace =
    P.Authoring.Document.Replace_request.create
      ~expected:edited_result.P.Authoring.Document.Result.version
      ~next_editor_revision:2
      ~contents:incomplete_source
    |> ok
  in
  let incomplete_result =
    match
      authoring client (P.Authoring_wire.Request.Document_replace incomplete_replace)
    with
    | Document_replace result -> result
    | _ -> fail "incomplete Project Document_replace returned another method"
  in
  let incomplete = incomplete_result.P.Authoring.Document.Result.context in
  let incomplete_member =
    P.Authoring.Document.Version.source
      incomplete_result.P.Authoring.Document.Result.version
  in
  let prefix_request =
    P.Authoring.Analysis.Prefix.Request.create
      ~context:(P.Authoring.Context.Expected.of_view incomplete)
      ~target:P.Authoring.Analysis.Target.project
      ~source:incomplete_member
      ~cursor_byte:(String.length incomplete_source)
    |> ok
  in
  (match authoring client (P.Authoring_wire.Request.Prefix prefix_request) with
   | Prefix (Provisional provisional) ->
     require
       (Chatml_source.Digest.equal
          (P.Authoring.Source.digest
             (P.Authoring.Analysis.Prefix.full_source provisional))
          (Chatml_source.Digest.of_contents incomplete_source ~checkpoint:Fn.id))
       "provisional query lost the incomplete editor revision"
   | Prefix (Unavailable _) -> fail "incomplete Project query was unavailable"
   | _ -> fail "incomplete Project Prefix returned another method");
  let source_path = Eio.Path.(project_path / "src/main.chatml") in
  require
    (String.equal (Eio.Path.load source_path) initial_source)
    "open editor overlay silently wrote the host file";
  Eio.Path.save ~create:(`Or_truncate 0o600) source_path saved_source;
  let refresh =
    P.Authoring.Context.Refresh_request.create
      ~expected:(P.Authoring.Context.Expected.of_view incomplete)
      ~policy:Discard_open_documents
  in
  let refreshed =
    match authoring client (P.Authoring_wire.Request.Context_refresh refresh) with
    | Context_refresh refreshed -> refreshed
    | _ -> fail "Context_refresh returned another method"
  in
  let saved_check, _ =
    phase_check_project client refreshed ~key_name:"workflow-saved-check"
  in
  (match check_receipt saved_check with
   | Ready -> ()
   | Rejected _ | Interrupted | Cancelled -> fail "saved Project revision failed Check");
  refreshed
;;

let phase_imported_diagnostic client context =
  let expected = P.Authoring.Context.Expected.of_view context in
  let inventory =
    P.Authoring.Source_read.List_request.create
      ~context:expected
      ~package_namespace:None
      ~directory:None
    |> ok
  in
  let sources =
    match authoring client (P.Authoring_wire.Request.Source_list inventory) with
    | Source_list inventory -> P.Authoring.Source_read.List_result.sources inventory
    | _ -> fail "imported Source_list returned another method"
  in
  let source =
    List.find_exn sources ~f:(fun source ->
      String.equal
        (Chatml_source.Id.relative_path (P.Authoring.Source.id source))
        "src/util.chatml")
  in
  let request =
    P.Authoring.Document.Version_request.create
      ~context:expected
      ~source_id:(P.Authoring.Source.id source)
    |> ok
  in
  let version =
    match authoring client (P.Authoring_wire.Request.Document_version request) with
    | Document_version version -> version
    | _ -> fail "imported Document_version returned another method"
  in
  let contents = "let marker = \"é\"\nlet value = missing\n" in
  let request =
    P.Authoring.Document.Replace_request.create
      ~expected:version
      ~next_editor_revision:1
      ~contents
    |> ok
  in
  let edited =
    match authoring client (P.Authoring_wire.Request.Document_replace request) with
    | Document_replace result -> result.P.Authoring.Document.Result.context
    | _ -> fail "imported Document_replace returned another method"
  in
  let require_site source_id digest start_byte end_byte =
    let start = String.substr_index_exn contents ~pattern:"missing" in
    require
      (Chatml_source.Id.equal source_id (P.Authoring.Source.id source)
       && Chatml_source.Digest.equal
            digest
            (Chatml_source.Digest.of_contents contents ~checkpoint:Fn.id)
       && Int.equal start_byte start
       && Int.equal end_byte (start + String.length "missing"))
      "imported Unicode error lost exact source/digest/byte range"
  in
  let analyze =
    P.Authoring.Analysis.Analyze.Request.create
      ~context:(P.Authoring.Context.Expected.of_view edited)
      ~target:P.Authoring.Analysis.Target.project
    |> ok
  in
  (match authoring client (P.Authoring_wire.Request.Analyze analyze) with
   | Analyze (Failed { diagnostics; _ }) ->
     require
       (List.exists diagnostics ~f:(fun diagnostic ->
          match P.Authoring.Analysis.Diagnostic.site diagnostic with
          | None -> false
          | Some site ->
            let native = P.Authoring.Analysis.Site.native site in
            let located_source = P.Authoring.Analysis.Byte_range.source native in
            require_site
              (P.Authoring.Source.id located_source)
              (P.Authoring.Source.digest located_source)
              (P.Authoring.Analysis.Byte_range.start_byte native)
              (P.Authoring.Analysis.Byte_range.end_byte native);
            true))
       "Authoring lost imported error site"
   | Analyze (Complete _ | Unavailable _) -> fail "invalid imported source passed Analyze"
   | _ -> fail "imported Analyze returned another method");
  let check, _ = phase_check_project client edited ~key_name:"workflow-imported-error" in
  (match check_receipt check with
   | Rejected { location = Some location; _ } ->
     require_site location.source location.digest location.start_byte location.end_byte
   | Rejected { location = None; _ } -> fail "Check lost imported error location"
   | Ready | Interrupted | Cancelled -> fail "invalid imported source passed Check");
  let refresh =
    P.Authoring.Context.Refresh_request.create
      ~expected:(P.Authoring.Context.Expected.of_view edited)
      ~policy:Discard_open_documents
  in
  match authoring client (P.Authoring_wire.Request.Context_refresh refresh) with
  | Context_refresh context -> context
  | _ -> fail "imported Context_refresh returned another method"
;;

let phase_document_diagnostic client ~project_path ~external_source =
  let body = "let marker = \"é\"\nlet main input = Task.pure(missing)\n" in
  let member = if external_source then "unicode-external.chatmd" else "unicode.chatmd" in
  let prefix =
    "<user>é before declaration</user>\n\
     <script id=\"workflow-unicode\" language=\"chatml\" kind=\"tool\" \
     project=\"chatml.project.json\""
    ^ if external_source then " src=\"unicode-external.chatml\">" else ">"
  in
  let document = prefix ^ (if external_source then "" else body) ^ "</script>" in
  if external_source
  then
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(project_path / "unicode-external.chatml")
      body;
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(project_path / member) document;
  let root =
    match authoring client P.Authoring_wire.Request.Roots with
    | Roots roots -> (List.hd_exn roots.roots).id
    | _ -> fail "embedded source root is unavailable"
  in
  let request =
    P.Authoring.Context.Open_request.create_host_document
      ~root
      ~member
      ~default_environment:None
    |> ok
  in
  let context =
    match authoring client (P.Authoring_wire.Request.Context_open request) with
    | Context_open context -> context
    | _ -> fail "embedded Context_open returned another method"
  in
  let selector = P.Authoring.Analysis.Target.selected_script "workflow-unicode" |> ok in
  let request =
    P.Authoring.Analysis.Analyze.Request.create
      ~context:(P.Authoring.Context.Expected.of_view context)
      ~target:selector
    |> ok
  in
  let source, authored =
    match authoring client (P.Authoring_wire.Request.Analyze request) with
    | Analyze (Failed { capture; diagnostics; _ }) ->
      let stamp =
        match P.Authoring.Analysis.Capture.target capture with
        | Script_input stamp -> stamp
        | Project_input -> fail "embedded analysis lost its selected script stamp"
      in
      let source = P.Authoring.Analysis.Target.selected_script_source stamp in
      let authored = P.Authoring.Analysis.Target.document_source stamp in
      require
        (Chatml_source.Digest.equal
           (P.Authoring.Source.digest source)
           (Chatml_source.Digest.of_contents body ~checkpoint:Fn.id)
         && Chatml_source.Digest.equal
              (P.Authoring.Source.digest authored)
              (Chatml_source.Digest.of_contents document ~checkpoint:Fn.id))
        "embedded analysis relabelled its native or authored revision";
      let start_byte = String.substr_index_exn body ~pattern:"missing" in
      require
        (List.exists diagnostics ~f:(fun diagnostic ->
           match P.Authoring.Analysis.Diagnostic.site diagnostic with
           | None -> false
           | Some site ->
             let range_matches range expected_source start =
               let actual = P.Authoring.Analysis.Byte_range.source range in
               Chatml_source.Id.equal
                 (P.Authoring.Source.id actual)
                 (P.Authoring.Source.id expected_source)
               && Chatml_source.Digest.equal
                    (P.Authoring.Source.digest actual)
                    (P.Authoring.Source.digest expected_source)
               && Int.equal (P.Authoring.Analysis.Byte_range.start_byte range) start
               && Int.equal
                    (P.Authoring.Analysis.Byte_range.end_byte range)
                    (start + String.length "missing")
             in
             range_matches (P.Authoring.Analysis.Site.native site) source start_byte
             &&
               (match external_source, P.Authoring.Analysis.Site.projection site with
               | false, Some (Authored_exact range) ->
                 range_matches range authored (String.length prefix + start_byte)
               | true, Some (External_origin location) ->
                 String.equal location.member "unicode-external.chatml"
                 && Chatml_source.Digest.equal
                      location.body_digest
                      (P.Authoring.Source.digest source)
                 && Int.equal location.start_byte start_byte
                 && Int.equal location.end_byte (start_byte + String.length "missing")
               | true, Some (Authored_exact _)
               | false, Some (External_origin _)
               | _, (None | Some (Approximate_declaration _)) -> false)))
        "embedded Unicode error lost exact native/authored byte projection";
      source, authored
    | Analyze ((Complete _ | Unavailable _) as result) ->
      raise_s
        [%sexp
          "embedded Analyze did not fail"
        , (P.Authoring.Analysis.Analyze.to_json result : Jsonaf.t)]
    | _ -> fail "embedded Analyze returned another method"
  in
  require
    (not
       (Chatml_source.Id.equal
          (P.Authoring.Source.id source)
          (P.Authoring.Source.id authored)))
    "embedded projection replaced its compiler-owned source identity";
  let check, _ =
    phase_check_project
      client
      context
      ~selector
      ~key_name:
        (if external_source then "workflow-external-error" else "workflow-embedded-error")
  in
  match check_receipt check with
  | Rejected { location = Some location; _ } ->
    let start_byte = String.substr_index_exn body ~pattern:"missing" in
    require
      (Chatml_source.Id.equal location.source (P.Authoring.Source.id source)
       && Chatml_source.Digest.equal location.digest (P.Authoring.Source.digest source)
       && Int.equal location.source_bytes (String.length body)
       && Int.equal location.start_byte start_byte
       && Int.equal location.end_byte (start_byte + String.length "missing"))
      "Developer Check lost the analyzed embedded Unicode native source site"
  | Rejected { location = None; _ } -> fail "Check lost embedded source location"
  | Ready | Interrupted | Cancelled -> fail "invalid embedded script passed Check"
;;

module Publication = struct
  type t =
    { receipt : B.Receipt.t
    ; context : P.Authoring.Context.t
    ; check : D.View.t
    ; lookup : D.Lookup.t
    ; commit : B.Commit.request
    }
end

let upload_revision client ~name ~contents =
  let manifest, sources = Support.Developer_workflow_project.create ~contents () in
  let begin_request =
    B.Begin.create_request
      ~idempotency_key:(P.Idempotency_key.of_string (name ^ "-begin") |> ok)
      ~manifest
      ()
  in
  let upload_id =
    match authoring client (P.Authoring_wire.Request.Bundle_begin begin_request) with
    | Bundle_begin response -> B.Begin.upload_id response
    | _ -> fail "Project Bundle_begin returned another method"
  in
  List.iter sources ~f:(fun source ->
    let chunk =
      B.Chunk.create_request
        ~upload_id
        ~source:(Chatml_source.id source)
        ~byte_offset:0
        ~bytes:(Chatml_source.contents source)
      |> ok
    in
    match authoring client (P.Authoring_wire.Request.Bundle_chunk chunk) with
    | Bundle_chunk _ -> ()
    | _ -> fail "Project Bundle_chunk returned another method");
  let commit =
    B.Commit.create_request
      ~upload_id
      ~idempotency_key:(P.Idempotency_key.of_string (name ^ "-commit") |> ok)
  in
  let receipt =
    match authoring client (P.Authoring_wire.Request.Bundle_commit commit) with
    | Bundle_commit response ->
      (match B.Commit.outcome response with
       | Published receipt -> receipt
       | Accepted_for_validation | Rejected _ -> fail "Project upload did not publish")
    | _ -> fail "Project Bundle_commit returned another method"
  in
  let opened =
    P.Authoring.Context.Open_request.create_uploaded_project (B.Receipt.bundle_id receipt)
  in
  let context =
    match authoring client (P.Authoring_wire.Request.Context_open opened) with
    | Context_open context -> context
    | _ -> fail "uploaded Project Context_open returned another method"
  in
  let check, lookup = phase_check_project client context ~key_name:(name ^ "-check") in
  (match check_receipt check with
   | Ready -> ()
   | Rejected _ | Interrupted | Cancelled ->
     raise_s
       [%sexp
         "uploaded Project failed Check"
       , (name : string)
       , (D.View.to_json check : Jsonaf.t)]);
  Publication.{ receipt; context; check; lookup; commit }
;;

let require_context_contents client context contents =
  let expected = P.Authoring.Context.Expected.of_view context in
  let request =
    P.Authoring.Source_read.List_request.create
      ~context:expected
      ~package_namespace:None
      ~directory:None
    |> ok
  in
  let source =
    match authoring client (P.Authoring_wire.Request.Source_list request) with
    | Source_list inventory ->
      P.Authoring.Source_read.List_result.sources inventory
      |> List.find_exn ~f:(fun source ->
        String.equal
          (Chatml_source.Id.relative_path (P.Authoring.Source.id source))
          "src/main.chatml")
    | _ -> fail "uploaded Source_list returned another method"
  in
  let request =
    P.Authoring.Source_read.Read_request.create
      ~context:expected
      ~source_id:(P.Authoring.Source.id source)
    |> ok
  in
  match authoring client (P.Authoring_wire.Request.Source_read request) with
  | Source_read read ->
    require
      (String.equal (P.Authoring.Source_read.Read_result.contents read) contents)
      "published context source bytes changed";
    require
      (Chatml_source.Digest.equal
         (P.Authoring.Source.digest source)
         (Chatml_source.Digest.of_contents contents ~checkpoint:Fn.id))
      "published context source digest changed"
  | _ -> fail "uploaded Source_read returned another method"
;;

let phase_upload_project client =
  let old_source = saved_source in
  let new_source = "let marker = \"新\"\nlet main input = Task.pure(input)\n" in
  let old = upload_revision client ~name:"workflow-upload-old" ~contents:old_source in
  let fresh = upload_revision client ~name:"workflow-upload-new" ~contents:new_source in
  require
    (not
       (P.Id.Authoring_bundle.equal
          (B.Receipt.bundle_id old.receipt)
          (B.Receipt.bundle_id fresh.receipt)))
    "changed published bytes reused a bundle identity";
  require
    (not
       (Chatml_source.Digest.equal
          (B.Receipt.manifest_fingerprint old.receipt)
          (B.Receipt.manifest_fingerprint fresh.receipt)))
    "changed published bytes reused a manifest fingerprint";
  require
    (not
       (Chatml_source.Digest.equal
          (D.View.capture_fingerprint old.check)
          (D.View.capture_fingerprint fresh.check)))
    "fresh uploaded Check reused the old capture";
  require_context_contents client old.context old_source;
  require_context_contents client fresh.context new_source;
  (match authoring client (P.Authoring_wire.Request.Bundle_commit old.commit) with
   | Bundle_commit response ->
     (match B.Commit.outcome response with
      | Published receipt ->
        require
          (B.Receipt.equal old.receipt receipt)
          "historical publication retry replaced its immutable receipt"
      | Accepted_for_validation | Rejected _ ->
        fail "published revision retry changed state")
   | _ -> fail "historical Bundle_commit returned another method");
  let limits =
    call client ~method_:"developer.capabilities" ~params:(`Object [])
    |> D.Capabilities.of_json
    |> ok
    |> D.Capabilities.limits
  in
  let observed =
    match developer client ~limits (W.Request.Observe old.lookup) with
    | W.Response.Observe (Some view) -> view
    | W.Response.Observe None -> fail "uploaded Check vanished"
    | _ -> fail "uploaded Check Observe returned another method"
  in
  require
    (P.Developer_command_id.equal
       (D.View.command_id old.check)
       (D.View.command_id observed)
     && Chatml_source.Digest.equal
          (D.View.request_fingerprint old.check)
          (D.View.request_fingerprint observed)
     && Chatml_source.Digest.equal
          (D.View.capture_fingerprint old.check)
          (D.View.capture_fingerprint observed))
    "historical Check changed identity after a new publication";
  match D.View.phase old.check, D.View.phase observed with
  | Terminal { receipt = original; _ }, Terminal { receipt; _ } ->
    require
      (P.Developer_receipt.equal original receipt)
      "historical Check changed its terminal receipt"
  | (Reserved | Waiting_permission | Execution_started | Terminal _), _ ->
    fail "historical Check lost terminal state"
;;

let trace_pages client locator =
  let capabilities =
    call client ~method_:(T.Request.method_name Capabilities) ~params:(`Object [])
    |> T.Capabilities.of_json
    |> ok
  in
  let limits = T.Capabilities.limits capabilities in
  let rec read after remaining expected_header events =
    require Int.(remaining > 0) "workflow trace exceeded its bounded page count";
    let request =
      T.Page_request.create
        ~limits
        ~locator
        ~after
        ~limit:(Int.min 64 (T.Limits.max_page_items limits))
      |> ok
    in
    let wire = T.Request.Page request in
    let response =
      call client ~method_:(T.Request.method_name wire) ~params:(T.Request.params wire)
      |> T.Response.of_method_and_json ~limits ~method_:(T.Request.method_name wire)
      |> ok
    in
    let page =
      match response with
      | T.Response.Page (Retained page) -> page
      | Page (Unavailable | Expired) -> fail "workflow execution trace was not retained"
      | Capabilities _ -> fail "workflow trace Page returned another method"
    in
    require
      (T.Locator.equal (T.Page.locator page) locator)
      "trace page changed its workflow locator";
    let header = T.Page.header page in
    Option.iter expected_header ~f:(fun expected ->
      require
        (String.equal (T.Header.execution_id expected) (T.Header.execution_id header))
        "trace execution changed between pages");
    let events = List.rev_append (T.Page.events page) events in
    match T.Page.next page with
    | Some cursor -> read (Some cursor) (remaining - 1) (Some header) events
    | None ->
      (match T.Page.state page with
       | Complete _ -> ()
       | Incomplete -> fail "settled workflow execution had an incomplete trace");
      header, List.rev events
  in
  read None 8 None []
;;

let phase_command_trace client view lookup =
  let header, events = trace_pages client (T.Locator.Command lookup) in
  require
    (Chatml_source.Digest.equal
       (T.Header.submission_fingerprint header)
       (D.View.request_fingerprint view))
    "trace submission fingerprint did not identify its command";
  let graph =
    match D.View.phase view with
    | Terminal { receipt; _ } ->
      (match P.Developer_receipt.value receipt with
       | Fixture (Completed receipt) -> receipt.graph_fingerprint
       | Fixture (Interrupted | Cancelled) | Check _ | Live _ ->
         fail "trace correlation lost its successful Fixture")
    | Reserved | Waiting_permission | Execution_started ->
      fail "Fixture trace preceded settlement"
  in
  require
    (Option.equal
       Chatml_source.Digest.equal
       (T.Header.graph_fingerprint header)
       (Some graph)
     && Chatml_source.Digest.equal (T.Header.selection_fingerprint header) graph)
    "trace graph/selection did not identify its captured Fixture";
  let digest = Chatml_source.Digest.of_contents imported_source ~checkpoint:Fn.id in
  let expected_source =
    T.Header.sources header
    |> List.find_exn ~f:(fun source ->
      String.equal
        (Chatml_source.Id.relative_path (T.Header.Source_ref.id source))
        "src/util.chatml")
  in
  require
    (Chatml_source.Digest.equal (T.Header.Source_ref.digest expected_source) digest
     && Int.equal
          (T.Header.Source_ref.byte_length expected_source)
          (String.length imported_source))
    "trace header lost its imported captured revision";
  let value_start = String.substr_index_exn imported_source ~pattern:"let value" in
  let value_end = String.length imported_source - 1 in
  let exact_site =
    List.exists events ~f:(fun event ->
      Option.value_map (T.Event.site event) ~default:false ~f:(fun site ->
        Chatml_source.Id.equal
          (T.Location.source site)
          (T.Header.Source_ref.id expected_source)
        && Chatml_source.Digest.equal (T.Location.digest site) digest
        && Int.equal (T.Location.start_byte site) value_start
        && Int.equal (T.Location.end_byte site) value_end))
  in
  require exact_site "trace lost authentic imported byte site after Unicode";
  T.Header.execution_id header
;;

let phase_run_project client context ~catalog =
  let capabilities =
    call client ~method_:"developer.capabilities" ~params:(`Object [])
    |> D.Capabilities.of_json
    |> ok
  in
  let limits = D.Capabilities.limits capabilities in
  let route =
    D.Route.authoring_project
      ~limits:(D.Limits.request limits)
      ~expected:(P.Authoring.Context.Expected.of_view context)
    |> ok
  in
  let fixtures =
    D.Fixture.create
      ~limits:(D.Limits.request limits)
      ~max_steps:(D.Limits.max_fixture_steps limits)
      ~max_bytes:(D.Limits.max_fixture_bytes limits)
      ~steps:[]
    |> ok
  in
  let run ~kind ~name ~fixtures =
    let run_id = P.Id.Developer_run.create () in
    let key = P.Idempotency_key.of_string name |> ok in
    let request =
      D.Submit.create
        ~limits
        ~run_id
        ~key
        ~kind
        ~route
        ~target:D.Target.One_off_v1
        ~prompt:
          (match kind with
           | Live_run -> Some catalog.prompt
           | Check | Fixture_run -> None)
        ~workspace:
          (match kind with
           | Live_run -> Some catalog.workspace
           | Check | Fixture_run -> None)
        ~entrypoint:"main"
        ~input:(`String "workflow")
        ~fixtures
        ~requested_execution:None
        ~requested_max_output_bytes:None
      |> ok
    in
    let submitted =
      match developer client ~limits (W.Request.Submit request) with
      | W.Response.Submit view -> view
      | _ -> fail "Project run Submit returned another method"
    in
    let lookup = D.Lookup.create ~run_id ~key in
    let terminal =
      match developer client ~limits (W.Request.Await_terminal lookup) with
      | W.Response.Await_terminal view -> view
      | _ -> fail "Project run Await returned another method"
    in
    require
      (P.Developer_command_id.equal
         (D.View.command_id submitted)
         (D.View.command_id terminal))
      "Project run changed durable command identity";
    match D.View.phase terminal with
    | Terminal { execution_started; receipt } ->
      let expected_started =
        match kind with
        | Check | Fixture_run -> false
        | Live_run -> true
      in
      require
        (Bool.equal execution_started expected_started)
        "Project run claimed the wrong Live execution state";
      P.Developer_receipt.value receipt, terminal, lookup
    | Reserved | Waiting_permission | Execution_started ->
      raise_s
        [%sexp
          "Project run did not execute to terminal", (D.View.to_json terminal : Jsonaf.t)]
  in
  let fixture, fixture_view, fixture_lookup =
    run
      ~kind:P.Developer_receipt.Kind.Fixture_run
      ~name:"workflow-project-fixture"
      ~fixtures:(Some fixtures)
  in
  (match fixture with
   | Fixture
       (Completed { outcome = Succeeded (`String "workflow"); matched_steps = 0; _ }) ->
     ()
   | _ -> fail "Project Fixture result did not match its authored One_off");
  let live, _, _ =
    run
      ~kind:P.Developer_receipt.Kind.Live_run
      ~name:"workflow-project-live"
      ~fixtures:None
  in
  (match live with
   | Live (Succeeded (`String "workflow")) -> ()
   | _ -> fail "Project Live result did not match its authored One_off");
  fixture_view, fixture_lookup
;;

let repl_limits client =
  call client ~method_:"developer.repl.capabilities" ~params:(`Object [])
  |> R.Capabilities.of_json
  |> ok
  |> R.Capabilities.limits
;;

let phase_repl client context ~catalog ~reconnect ~command_execution =
  let limits = repl_limits client in
  let route =
    D.Route.authoring_project
      ~limits:(R.Limits.request limits)
      ~expected:(P.Authoring.Context.Expected.of_view context)
    |> ok
  in
  let key text = P.Idempotency_key.of_string text |> ok in
  let created =
    let request =
      R.Create.create
        ~limits
        ~run_id:(P.Id.Developer_run.create ())
        ~key:(key "workflow-repl-create")
        ~route
        ~target:One_off_v1
        ~prompt:catalog.prompt
        ~workspace:catalog.workspace
        ~requested_execution:None
        ~requested_max_output_bytes:None
      |> ok
    in
    match repl client ~limits (R.Request.Create request) with
    | R.Response.Create view -> view
    | _ -> fail "Project REPL Create returned another method"
  in
  let instance = R.View.instance created in
  let epoch = R.View.epoch created in
  let submit client ~limits ~epoch ~revision ~name ~source =
    let request =
      R.Submit.create
        ~limits
        ~instance
        ~epoch
        ~expected_revision:revision
        ~key:(key name)
        ~source
      |> ok
    in
    match repl client ~limits (R.Request.Submit request) with
    | R.Response.Submit value -> value
    | _ -> fail "Project REPL Submit returned another method"
  in
  let first =
    submit
      client
      ~limits
      ~epoch
      ~revision:(R.View.revision created)
      ~name:"workflow-repl-good"
      ~source:"let kept = 8"
  in
  (match R.Submission.status first with
   | Finished -> ()
   | Need_more _ | Rejected _ | Stopped _ -> fail "Project REPL binding did not finish");
  let rejected =
    submit
      client
      ~limits
      ~epoch
      ~revision:(R.Submission.revision first)
      ~name:"workflow-repl-type-error"
      ~source:"let kept = kept + true"
  in
  (match R.Submission.status rejected with
   | Stopped (Some diagnostic) ->
     require
       (Option.is_some (R.Submission.Diagnostic.location diagnostic))
       "REPL type failure lost its owned diagnostic"
   | Need_more _ | Rejected _ | Finished | Stopped None ->
     fail "Project REPL did not report its typing failure");
  require
    (List.is_empty (R.Submission.summaries rejected))
    "REPL type failure executed a statement";
  require
    (Int.equal (R.Submission.completed_statements rejected) 0)
    "REPL typing failure executed a statement";
  (* The wire revision serializes accepted submission attempts; it is separate
     from the transactional typing state. The later Task proves [kept] remains8. *)
  let partial =
    submit
      client
      ~limits
      ~epoch
      ~revision:(R.Submission.revision rejected)
      ~name:"workflow-repl-runtime-error"
      ~source:"let partial = kept\nlet doomed = 1 / 0"
  in
  (match R.Submission.status partial with
   | Stopped _ -> ()
   | Need_more _ | Rejected _ | Finished ->
     fail "Project REPL did not stop at runtime failure");
  require
    (Int.equal (R.Submission.completed_statements partial) 1)
    "REPL runtime failure lost completed work";
  (match R.Submission.details partial, R.Submission.summaries partial with
   | Available, [ good; failed ] ->
     (match R.Submission.Summary.outcome good, R.Submission.Summary.outcome failed with
      | Succeeded, Failed Runtime -> ()
      | (Succeeded | Failed _), _ -> fail "REPL summaries hid the partial runtime failure")
   | Available, _ | Output_limited, _ ->
     fail "REPL lost bounded partial statement summaries");
  let task_source = "Task.pure(partial)" in
  let submitted =
    submit
      client
      ~limits
      ~epoch
      ~revision:(R.Submission.revision partial)
      ~name:"workflow-repl-submit"
      ~source:task_source
  in
  (match R.Submission.status submitted with
   | Finished -> ()
   | Need_more _ | Rejected _ | Stopped _ ->
     fail "retained partial binding was unavailable");
  let reference =
    match R.Submission.task_references submitted with
    | [ reference ] -> reference
    | _ -> fail "Project REPL did not retain its explicit Task"
  in
  let task_key = key "workflow-repl-task" in
  let task_lookup = D.Lookup.create ~run_id:(R.View.run_id created) ~key:task_key in
  (* This checks durable request-key absence, not the absence of arbitrary
     effects. Explicit execution semantics have their own portable REPL tests. *)
  (match repl client ~limits (R.Request.Task_observe task_lookup) with
   | R.Response.Task_observe None -> ()
   | Task_observe (Some _) -> fail "unsubmitted REPL Task key already had a receipt"
   | _ -> fail "Project REPL Task_observe returned another method");
  let request =
    R.Task_submit.create
      ~limits
      ~key:task_key
      ~reference
      ~requested_execution:None
      ~requested_max_output_bytes:None
      ~fresh_epoch:(Some (instance, epoch))
    |> ok
  in
  let started =
    match repl client ~limits (R.Request.Task_submit request) with
    | R.Response.Task_submit view -> view
    | _ -> fail "Project REPL Task_submit returned another method"
  in
  let terminal =
    match repl client ~limits (R.Request.Task_await_terminal task_lookup) with
    | R.Response.Task_await_terminal view -> view
    | _ -> fail "Project REPL Task_await returned another method"
  in
  require
    (P.Developer_command_id.equal
       (R.Task_view.command_id started)
       (R.Task_view.command_id terminal))
    "Project REPL Task changed durable identity";
  (match R.Task_view.phase terminal with
   | Terminal { execution_started = true; outcome = Succeeded (Some "8") } -> ()
   | _ -> fail "Project REPL explicit Task did not return retained partial value 8");
  let statement_locator =
    T.Locator.Repl_statement { instance; epoch; key = key "workflow-repl-submit" }
  in
  let statement_header, _ = trace_pages client statement_locator in
  let task_header, _ = trace_pages client (T.Locator.Repl_task task_lookup) in
  let task_digest = Chatml_source.Digest.of_contents task_source ~checkpoint:Fn.id in
  require
    (List.exists (T.Header.sources statement_header) ~f:(fun source ->
       Chatml_source.Digest.equal (T.Header.Source_ref.digest source) task_digest
       && Int.equal (T.Header.Source_ref.byte_length source) (String.length task_source)))
    "REPL statement trace lost its exact submitted source revision";
  require
    (Chatml_source.Digest.equal
       (T.Header.submission_fingerprint task_header)
       (R.Task_view.request_fingerprint terminal))
    "Task trace did not identify the explicit durable request";
  require
    ((not
        (String.equal
           (T.Header.execution_id statement_header)
           (T.Header.execution_id task_header)))
     && (not (String.equal command_execution (T.Header.execution_id task_header)))
     && not (String.equal command_execution (T.Header.execution_id statement_header)))
    "command, REPL statement and Task reused an execution identity";
  let reconnected, finish =
    match reconnect with
    | None -> client, Fn.id
    | Some reconnect ->
      client.close ();
      let next = reconnect () in
      next, next.close
  in
  Exn.protect ~finally:finish ~f:(fun () ->
    let limits = repl_limits reconnected in
    let observe name =
      match
        repl
          reconnected
          ~limits
          (R.Request.Observe (R.Lookup.create ~instance ~key:(key name)))
      with
      | R.Response.Observe (Some submission) -> submission
      | Observe None -> fail "REPL reconnect lost a historical submission"
      | _ -> fail "REPL Observe returned another method"
    in
    let observed = observe "workflow-repl-runtime-error" in
    let compact_identity submission =
      (* On-demand type graphs have their own retention pool. Their eviction
         changes [details] to Output_limited, never the compact outcome. *)
      match R.Submission.to_json submission with
      | `Object fields ->
        `Object
          (List.filter fields ~f:(fun (name, _) -> not (String.equal name "details")))
        |> Jsonaf.to_string
      | `Array _ | `True | `False | `Null | `Number _ | `String _ ->
        fail "REPL submission encoder lost its object representation"
    in
    require
      (String.equal (compact_identity partial) (compact_identity observed))
      "REPL reconnect changed its retained compact partial receipt";
    let header, _ = trace_pages reconnected statement_locator in
    require
      (String.equal
         (T.Header.execution_id header)
         (T.Header.execution_id statement_header))
      "trace reconnect changed statement execution identity";
    let reset =
      match
        repl
          reconnected
          ~limits
          (R.Request.Reset_and_wait
             (R.Current_epoch.create ~instance ~epoch ~key:(key "workflow-repl-reset")))
      with
      | R.Response.Reset_and_wait view -> view
      | _ -> fail "Project REPL Reset returned another method"
    in
    require
      ((not (R.Wire_id.Epoch.equal epoch (R.View.epoch reset)))
       && R.Wire_id.Instance.equal instance (R.View.instance reset)
       && Chatml_source.Digest.equal
            (R.View.capture_fingerprint created)
            (R.View.capture_fingerprint reset))
      "REPL Reset replaced its source capture or retained its old epoch";
    let old = observe "workflow-repl-submit" in
    require
      (R.Wire_id.Epoch.equal (R.Submission.epoch old) epoch
       && R.Wire_id.Revision.equal
            (R.Submission.revision old)
            (R.Submission.revision submitted))
      "REPL Reset rewrote a historical submission";
    let cleared =
      submit
        reconnected
        ~limits
        ~epoch:(R.View.epoch reset)
        ~revision:(R.View.revision reset)
        ~name:"workflow-repl-after-reset"
        ~source:"partial"
    in
    (match R.Submission.status cleared with
     | Stopped (Some _) ->
       require
         (Int.equal (R.Submission.completed_statements cleared) 0
          && List.is_empty (R.Submission.summaries cleared))
         "REPL Reset typing failure executed a statement"
     | Need_more _ | Rejected _ | Finished | Stopped None ->
       fail "REPL Reset retained prior interactive bindings");
    match
      repl
        reconnected
        ~limits
        (R.Request.Close_and_wait
           (R.Lookup.create ~instance ~key:(key "workflow-repl-close")))
    with
    | R.Response.Close_and_wait -> ()
    | _ -> fail "Project REPL Close returned another method")
;;

let qualify client ~project_path ~catalog ~reconnect =
  let context = phase_open_project client in
  let initial_check, _ =
    phase_check_project client context ~key_name:"workflow-first-check"
  in
  (match check_receipt initial_check with
   | Ready -> ()
   | Rejected _ | Interrupted | Cancelled -> fail "initial Project revision failed Check");
  let saved = phase_edit_and_save client context ~project_path in
  let saved = phase_imported_diagnostic client saved in
  let fixture_view, fixture_lookup = phase_run_project client saved ~catalog in
  let command_execution = phase_command_trace client fixture_view fixture_lookup in
  phase_deny_other_environment client ~catalog;
  List.iter [ false; true ] ~f:(fun external_source ->
    phase_document_diagnostic client ~project_path ~external_source);
  phase_upload_project client;
  phase_repl client saved ~catalog ~reconnect ~command_execution
;;

let stdio_client ~sw env fixture ~policy =
  let executable = Sys.getenv_exn "OCHAT_E2E_STDIO_EXE" in
  let executable =
    if Filename.is_absolute executable
    then executable
    else Filename.concat (Eio.Path.native_exn (Eio.Stdenv.cwd env)) executable
  in
  let environment =
    Support.Temporary_environment.child_environment
      (Support.Config_fixture.environment fixture)
      ~base:(Core_unix.environment ())
  in
  let process =
    Support.Stdio_process.spawn
      ~sw
      ~env
      ~environment
      ~max_output_bytes:(2 * 1024 * 1024)
      [ executable
      ; "--local"
      ; "--developer-only"
      ; "--developer-policy"
      ; policy
      ; "--prompt"
      ; Support.Config_fixture.prompt_path fixture
      ; "--workspace"
      ; Support.Config_fixture.physical_workspace fixture
      ; "--transient"
      ]
  in
  let wire = Support.Stdio_client.create ~process ~clock:(Eio.Stdenv.clock env) in
  let rec response remaining =
    if Int.equal remaining 0
    then fail "too many local stdio notifications before response";
    match Support.Stdio_client.next_envelope wire ~timeout_seconds:5. |> ok with
    | P.Envelope.Notification _ -> response (remaining - 1)
    | response -> response
  in
  let closed = ref false in
  { next_id = 1
  ; send =
      (fun request ->
        Support.Stdio_client.send_raw_line
          wire
          (P.Envelope.to_json request |> Jsonaf.to_string)
        |> ok;
        response 64)
  ; close =
      (fun () ->
        if not !closed
        then (
          closed := true;
          Support.Stdio_process.close_stdin process;
          match
            Eio.Time.with_timeout (Eio.Stdenv.clock env) 3. (fun () ->
              Ok (Support.Stdio_process.await process))
          with
          | Ok result ->
            require
              (Support.Process_manager.equal_exit result.exit (Exited 0)
               && (not result.stdout.truncated)
               && not result.stderr.truncated)
              "local stdio host failed to exit and drain cleanly"
          | Error `Timeout ->
            ignore
              (Support.Stdio_process.terminate
                 process
                 ~clock:(Eio.Stdenv.clock env)
                 ~grace_seconds:1.
               : Support.Process_manager.termination);
            fail "local stdio host required forced cleanup"))
  }
;;

let run env ~case =
  (match case with
   | None -> ()
   | Some case -> raise_s [%sexp "unknown case", (case : string)]);
  (* The same phase functions run against two independently owned hosts. Each
     transport retains the original 45 second workflow watchdog. *)
  List.iter [ false; true ] ~f:(fun local ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
      Support.Temporary_environment.with_
        ~scenario:
          (if local then "developer-workflow-local-stdio" else "developer-workflow-http")
        ~env
        (fun environment ->
           let fixture =
             Support.Config_fixture.create
               environment
               ~name:"developer-workflow"
               ~http_port:(reserve_port env)
           in
           Support.Config_fixture.grant_public_all_scopes fixture;
           let project_root =
             Filename.concat
               (Support.Temporary_environment.roots environment).root
               "workflow-project"
           in
           let project_path = Eio.Path.(Eio.Stdenv.fs env / project_root) in
           Eio.Path.mkdir ~perm:0o700 project_path;
           Eio.Path.mkdir ~perm:0o700 Eio.Path.(project_path / "src");
           List.iter
             [ "chatml.project.json", project_source
             ; "src/main.chatml", initial_source
             ; "src/util.chatml", imported_source
             ]
             ~f:(fun (member, contents) ->
               Eio.Path.save
                 ~create:(`Exclusive 0o600)
                 Eio.Path.(project_path / member)
                 contents);
           if local
           then (
             let policy =
               Support.Config_fixture.write_configuration
                 fixture
                 ~name:"local-workflow-policy.sexp"
                 (configuration_fragment fixture ~project_root ~local:true)
             in
             Eio.Switch.run (fun sw ->
               let client = stdio_client ~sw env fixture ~policy in
               Exn.protect ~finally:client.close ~f:(fun () ->
                 let initialized = initialize client in
                 require
                   (String.equal
                      initialized.principal.authentication_kind
                      "embedded.local")
                   "local stdio did not bind the embedded operator";
                 let catalog =
                   { prompt = S.Catalog_identity.prompt_definition "embedded.prompt"
                   ; workspace =
                       S.Catalog_identity.workspace_definition "embedded.workspace"
                   }
                 in
                 qualify client ~project_path ~catalog ~reconnect:None)))
           else (
             install_config env fixture ~project_root;
             let options =
               Support.Daemon_host.with_offline_inference S.Daemon.default_options
             in
             Support.Daemon_host.with_ env fixture ~options (fun sw _daemon ->
               let client = http_client ~sw env fixture in
               Exn.protect ~finally:client.close ~f:(fun () ->
                 let initialized = initialize client in
                 let reconnect () =
                   let next = http_client ~sw env fixture in
                   let resumed = initialize next in
                   require
                     (P.Id.Server.equal initialized.server_id resumed.server_id
                      && P.Id.Principal.equal
                           initialized.principal.id
                           resumed.principal.id
                      && String.equal
                           resumed.principal.authentication_kind
                           "http.static_bearer")
                     "HTTP reconnect changed authenticated host or principal";
                   next
                 in
                 let catalog =
                   { prompt = S.Catalog_identity.prompt_definition "smoke"
                   ; workspace = S.Catalog_identity.workspace_definition "physical"
                   }
                 in
                 qualify client ~project_path ~catalog ~reconnect:(Some reconnect)))))));
  Eio.Flow.copy_string
    "Developer authored workflow passed: HTTP and local stdio\n"
    (Eio.Stdenv.stdout env)
;;
