open! Core
module P = Agent_protocol
module A = P.Authoring
module W = A.Wire

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let base_contents = "raw\255\000\r\nbytes"

let base_id =
  Chatml_source.Id.create
    ~package_namespace:"\255source namespace"
    ~relative_path:"unsaved/example.chatmd"
  |> Or_error.ok_exn
;;

let source id contents =
  A.Source.create
    ~id
    ~digest:(Chatml_source.Digest.of_contents contents ~checkpoint:Fn.id)
    ~byte_length:(String.length contents)
    ~editor_revision:None
  |> ok
;;

let base = source base_id base_contents
let generator = P.Id.Generator.create ~bytes:(fun length -> String.make length '\000')
let host = P.Id.Server.create_with generator
let context_id = P.Id.Authoring_context.create_with generator
let fingerprint = Chatml_source.Digest.of_contents "basis" ~checkpoint:Fn.id

let context =
  A.Context.create
    ~id:context_id
    ~host
    ~origin:(A.Context.Supplied_document { source = base; default_environment = None })
    ~generation:0
    ~basis:(A.Context.Document { source = base; selection = None })
    ~basis_fingerprint:fingerprint
    ~editable_sources:[]
  |> ok
;;

let expected = A.Context.Expected.of_view context

let%expect_test "authoring source custody and strict wire fields" =
  let encoded = A.Source.to_json base in
  let decoded = A.Source.of_json encoded |> ok in
  print_s [%sexp (A.Source.equal decoded base : bool)];
  let open_request =
    A.Context.Open_request.create_supplied_document
      ~member:"unsaved/example.chatmd"
      ~default_environment:None
      ~contents:base_contents
    |> ok
  in
  let request = W.Request.Context_open open_request in
  let method_name = W.Request.method_name request in
  let decoded_request =
    P.Command.of_method_and_params ~method_:method_name ~params:(W.Request.params request)
    |> ok
  in
  print_s
    [%sexp (String.equal (P.Command.method_name decoded_request) method_name : bool)];
  let null_optional =
    match W.Request.params request with
    | `Object fields -> `Object (("default_environment", `Null) :: fields)
    | _ -> assert false
  in
  print_s
    [%sexp
      (Result.is_error
         (W.Request.of_method_and_params ~method_:method_name ~params:null_optional)
       : bool)];
  let unknown =
    match W.Request.params request with
    | `Object fields -> `Object (("unapproved", `True) :: fields)
    | _ -> assert false
  in
  print_s
    [%sexp
      (Result.is_error
         (W.Request.of_method_and_params ~method_:method_name ~params:unknown)
       : bool)];
  let long_key = String.make 512 'x' in
  let error =
    W.Request.of_method_and_params
      ~method_:method_name
      ~params:(`Object [ long_key, `Null; long_key, `Null ])
    |> Result.error
    |> Option.value_exn
  in
  print_s [%sexp (String.length error.message < 128 : bool)];
  [%expect
    {|
    true
    true
    true
    true
    true
    |}]
;;

let%expect_test "document context and read receipt preserve exact source" =
  let read =
    A.Source_read.Read_result.create
      ~context:expected
      ~source:base
      ~contents:base_contents
    |> ok
  in
  let decoded =
    A.Source_read.Read_result.of_json (A.Source_read.Read_result.to_json read) |> ok
  in
  print_s
    [%sexp
      (String.equal (A.Source_read.Read_result.contents decoded) base_contents : bool)];
  print_s
    [%sexp
      (Result.is_error
         (A.Source_read.Read_result.create
            ~context:expected
            ~source:base
            ~contents:"different")
       : bool)];
  let foreign =
    source
      (Chatml_source.Id.create
         ~package_namespace:"foreign"
         ~relative_path:"unsaved/example.chatmd"
       |> Or_error.ok_exn)
      base_contents
  in
  print_s
    [%sexp
      (Result.is_error
         (A.Context.create
            ~id:context_id
            ~host
            ~origin:
              (A.Context.Supplied_document { source = base; default_environment = None })
            ~generation:0
            ~basis:(A.Context.Document { source = base; selection = None })
            ~basis_fingerprint:fingerprint
            ~editable_sources:[ foreign ])
       : bool)];
  [%expect
    {|
    true
    true
    true
    |}]
;;

let%expect_test "closed document CAS retains the last editor revision" =
  let version =
    A.Document.Version.create
      ~context:expected
      ~source:base
      ~last_editor_revision:(Some 4)
      ~overlay_open:false
    |> ok
  in
  print_s [%sexp (A.Document.Version.last_editor_revision version : int option)];
  print_s
    [%sexp
      (Result.is_error
         (A.Document.Replace_request.create
            ~expected:version
            ~next_editor_revision:4
            ~contents:"new")
       : bool)];
  print_s
    [%sexp
      (Result.is_ok
         (A.Document.Replace_request.create
            ~expected:version
            ~next_editor_revision:5
            ~contents:"new")
       : bool)];
  [%expect
    {|
    (4)
    true
    true
    |}]
;;

let%expect_test "constructor and decoder bounds agree for source IDs and roots" =
  List.iter [ 1; 1024; 1025 ] ~f:(fun length ->
    let id =
      Chatml_source.Id.create
        ~package_namespace:(String.make length '\255')
        ~relative_path:"a.chatmd"
      |> Or_error.ok_exn
    in
    print_s
      [%sexp
        (Result.is_ok
           (A.Source.create
              ~id
              ~digest:(Chatml_source.Digest.of_contents "x" ~checkpoint:Fn.id)
              ~byte_length:1
              ~editor_revision:None)
         : bool)]);
  let roots count =
    List.init count ~f:(fun index ->
      P.Id.Authoring_root.of_string ("aroot_" ^ Int.to_string index) |> ok)
  in
  let root = P.Id.Authoring_root.of_string "aroot_main" |> ok in
  List.iter [ 64; 65 ] ~f:(fun count ->
    print_s
      [%sexp
        (Result.is_ok
           (A.Context.Open_request.create_host_project
              ~root
              ~dependency_roots:(roots count)
              ~project_member:"main.chatml"
              ~selection:
                (A.Context.Selection.create
                   ~target:A.Context.Selection.Library
                   ~environment:None
                 |> ok))
         : bool)]);
  [%expect
    {|
    true
    true
    false
    true
    false
    |}]
;;

let%expect_test "aggregate escaped wire bytes are rejected before decoding" =
  let oversized = `Object [ "kind", `String (String.make (6 * 1024 * 1024) '\n') ] in
  let error =
    W.Request.of_method_and_params ~method_:"authoring.context.open" ~params:oversized
    |> Result.error
    |> Option.value_exn
  in
  print_s
    [%sexp (String.equal error.message "authoring JSON exceeds structural limits" : bool)];
  [%expect {| true |}]
;;

let%expect_test "client binds source, document transitions and initialization" =
  Eio_main.run (fun _ ->
    let version = P.Version.create ~major:1 ~minor:0 |> ok in
    let implementation =
      P.Initialize.Implementation.create ~name:"test" ~version:"1" |> ok
    in
    let principal =
      P.Principal.create
        ~id:(P.Id.Principal.of_string "pri_authoring_test" |> ok)
        ~authentication_kind:"test"
        ~scopes:(Set.empty (module P.Scope))
        ~attributes:[]
      |> ok
    in
    let initialization =
      P.Initialize.Response.create
        ~protocol_name:"ochat.agent"
        ~selected_version:version
        ~implementation
        ~server_id:host
        ~enabled_features:
          [ A.Features.core
          ; A.Features.semantic
          ; A.Features.lexical
          ; A.Features.assistance
          ]
        ~extensions:None
        ~principal
        ~limits:
          { max_request_bytes = 16 * 1024 * 1024
          ; max_event_bytes = 1024
          ; max_page_size = 100
          ; max_attachments_per_connection = 1
          }
        ~event_retention:
          { minimum_age_ms = 0; maximum_events = 1; oldest_replayable_sequence = None }
        ~timing:
          { heartbeat_interval_ms = 1000
          ; owner_lease_duration_ms = 10000
          ; owner_renew_after_ms = 5000
          ; disconnect_grace_default_ms = 0
          }
        ~server_time:(P.Timestamp.of_string "2026-10-13T00:00:00Z" |> ok)
      |> ok
    in
    let current_initialization = ref initialization in
    let request =
      P.Initialize.Request.create
        ~implementation
        ~protocol_min:version
        ~protocol_max:version
        ~features:
          [ A.Features.core
          ; A.Features.semantic
          ; A.Features.lexical
          ; A.Features.assistance
          ]
        ~event_encodings:[ Json ]
        ~max_inbound_event_bytes:1024
        ()
      |> ok
    in
    let limits =
      A.Limits.create
        ~max_sources:100
        ~max_source_bytes:1024
        ~max_context_source_bytes:4096
        ~max_request_bytes:16384
        ~max_result_bytes:16384
        ~max_path_candidates:100
      |> ok
    in
    let capabilities =
      A.Capabilities.create
        ~methods:
          [ "authoring.capabilities"
          ; "authoring.context.open"
          ; "authoring.context.refresh"
          ; "authoring.source.read"
          ; "authoring.document.version"
          ; "authoring.document.replace"
          ; "authoring.document.close"
          ; "authoring.analysis"
          ; "authoring.prefix"
          ; "authoring.query"
          ; "authoring.lexical"
          ; "authoring.assistance"
          ]
        ~encodings:[ A.Encoding.Utf8_bytes ]
        ~limits
      |> ok
    in
    let advertised_capabilities = ref capabilities in
    let foreign =
      source
        (Chatml_source.Id.create
           ~package_namespace:"foreign"
           ~relative_path:"unsaved/example.chatmd"
         |> Or_error.ok_exn)
        base_contents
    in
    let foreign_view =
      let different_source = source (A.Source.id foreign) "different" in
      A.Context.create
        ~id:context_id
        ~host
        ~origin:
          (A.Context.Supplied_document
             { source = different_source; default_environment = None })
        ~generation:0
        ~basis:(A.Context.Document { source = different_source; selection = None })
        ~basis_fingerprint:fingerprint
        ~editable_sources:[]
      |> ok
    in
    let open_view = ref foreign_view in
    let refresh_view = ref context in
    let version_reply = ref None in
    let replace_reply = ref None in
    let close_reply = ref None in
    let analysis_reply = ref None in
    let prefix_reply = ref None in
    let query_reply = ref None in
    let lexical_reply = ref None in
    let assistance_reply = ref None in
    let cancel_read = ref None in
    let return response =
      P.Public.Result.Non_history
        (P.Public.Result.Non_history.of_internal (P.Method_result.Authoring response)
         |> ok)
      |> Result.return
    in
    let transport =
      Agent_client.Transport.create
        ~request:(function
          | P.Command.Protocol_initialize _ ->
            P.Public.Result.Non_history
              (P.Public.Result.Non_history.of_internal
                 (P.Method_result.Protocol_initialize !current_initialization)
               |> ok)
            |> Result.return
          | P.Command.Authoring W.Request.Capabilities ->
            return (W.Response.Capabilities !advertised_capabilities)
          | P.Command.Authoring (W.Request.Context_open _) ->
            return (W.Response.Context_open !open_view)
          | P.Command.Authoring (W.Request.Context_refresh _) ->
            return (W.Response.Context_refresh !refresh_view)
          | P.Command.Authoring (W.Request.Document_version _) ->
            return (W.Response.Document_version (Option.value_exn !version_reply))
          | P.Command.Authoring (W.Request.Document_replace _) ->
            return (W.Response.Document_replace (Option.value_exn !replace_reply))
          | P.Command.Authoring (W.Request.Document_close _) ->
            return (W.Response.Document_close (Option.value_exn !close_reply))
          | P.Command.Authoring (W.Request.Analyze _) ->
            return (W.Response.Analyze (Option.value_exn !analysis_reply))
          | P.Command.Authoring (W.Request.Prefix _) ->
            return (W.Response.Prefix (Option.value_exn !prefix_reply))
          | P.Command.Authoring (W.Request.Query _) ->
            return (W.Response.Query (Option.value_exn !query_reply))
          | P.Command.Authoring (W.Request.Lexical _) ->
            return (W.Response.Lexical (Option.value_exn !lexical_reply))
          | P.Command.Authoring (W.Request.Assistance _) ->
            return (W.Response.Assistance (Option.value_exn !assistance_reply))
          | P.Command.Authoring (W.Request.Source_read _) ->
            Option.iter !cancel_read ~f:(fun (context, marker) ->
              Eio.Cancel.cancel context marker;
              Eio.Fiber.check ());
            return
              (W.Response.Source_read
                 (A.Source_read.Read_result.create
                    ~context:expected
                    ~source:foreign
                    ~contents:base_contents
                  |> ok))
          | _ -> Error (P.Error.invalid_request "unexpected mock request"))
        ~next_notification:(fun () -> None)
        ~close:Fn.id
    in
    let connection = Agent_client.Connection.create transport in
    ignore
      (Agent_client.Connection.request connection (P.Command.Protocol_initialize request)
       |> ok);
    let client = Agent_client.Authoring.create connection |> ok in
    let open_request =
      A.Context.Open_request.create_supplied_document
        ~member:"unsaved/example.chatmd"
        ~default_environment:None
        ~contents:base_contents
      |> ok
    in
    print_s
      [%sexp
        (Result.is_error (Agent_client.Authoring.open_context client open_request) : bool)];
    open_view := context;
    let opened = Agent_client.Authoring.open_context client open_request |> ok in
    let expected = A.Context.Expected.of_view opened in
    let module S = A.Analysis in
    let analyze_request =
      S.Analyze.Request.create ~context:expected ~target:S.Target.project |> ok
    in
    analysis_reply
    := Some
         (S.Analyze.unavailable
            ~context:expected
            ~target:S.Target.project
            ~reason:S.Analyze.Document_basis
          |> ok);
    assert (Result.is_ok (Agent_client.Authoring.analyze client analyze_request));
    analysis_reply
    := Some
         (S.Analyze.unavailable
            ~context:expected
            ~target:(S.Target.selected_script "other" |> ok)
            ~reason:S.Analyze.No_selected_script
          |> ok);
    assert (Result.is_error (Agent_client.Authoring.analyze client analyze_request));
    let prefix_request =
      S.Prefix.Request.create
        ~context:expected
        ~target:S.Target.project
        ~source:base
        ~cursor_byte:3
      |> ok
    in
    prefix_reply
    := Some
         (S.Prefix.unavailable
            ~context:expected
            ~target:S.Target.project
            ~source:base
            ~cursor_byte:3
            ~reason:S.Prefix.No_complete_statement
          |> ok);
    assert (Result.is_ok (Agent_client.Authoring.analyze_prefix client prefix_request));
    prefix_reply
    := Some
         (S.Prefix.unavailable
            ~context:expected
            ~target:S.Target.project
            ~source:base
            ~cursor_byte:2
            ~reason:S.Prefix.No_complete_statement
          |> ok);
    assert (Result.is_error (Agent_client.Authoring.analyze_prefix client prefix_request));
    let stamp =
      S.Target.script_stamp
        ~document_source:base
        ~parser_root_fingerprint:fingerprint
        ~script_id:"selected"
        ~declaration_input_fingerprint:fingerprint
        ~declaration_input_byte_length:100
        ~declaration_start_byte:0
        ~declaration_end_byte:4
        ~selected_script_source:base
      |> ok
    in
    let capture =
      S.Capture.create
        ~context:expected
        ~target:(S.Target.Script_input stamp)
        ~input_fingerprint:fingerprint
        ~environment:"default"
        ~required_contract:fingerprint
        ~selected_contract:fingerprint
      |> ok
    in
    let query_request =
      S.Query.Request.create
        ~evidence:(S.Query.Syntax_only capture)
        ~source:base
        ~byte_offset:1
        ~kind:S.Query.Syntax_at
        ~max_items:1
      |> ok
    in
    query_reply
    := Some
         (S.Query.create
            ~request:query_request
            ~payload:(S.Query.Syntax_result (S.Query.Unavailable S.Query.No_syntax))
          |> ok);
    assert (Result.is_ok (Agent_client.Authoring.query_semantics client query_request));
    let changed_query_request =
      S.Query.Request.create
        ~evidence:(S.Query.Syntax_only capture)
        ~source:base
        ~byte_offset:2
        ~kind:S.Query.Syntax_at
        ~max_items:1
      |> ok
    in
    query_reply
    := Some
         (S.Query.create
            ~request:changed_query_request
            ~payload:(S.Query.Syntax_result (S.Query.Unavailable S.Query.No_syntax))
          |> ok);
    assert (Result.is_error (Agent_client.Authoring.query_semantics client query_request));
    let module H = A.Assistance in
    let lexical_request =
      H.Lexical.Request.create ~context:expected ~source:base ~view:Chatmd_authored |> ok
    in
    let lexical_owner =
      H.Lexical.Owner.chatmd
        ~captured_source:base
        ~input_identity:fingerprint
        ~view:Authored
        ~view_sha256:(A.Source.digest base)
        ~view_byte_length:(A.Source.byte_length base)
      |> ok
    in
    lexical_reply
    := Some
         (H.Lexical.Result.create
            ~request:lexical_request
            ~owner:lexical_owner
            ~spans:[]
            ~issues:[]
            ~status:Reached_eof
          |> ok);
    assert (Result.is_ok (Agent_client.Authoring.lexical client lexical_request));
    let changed_lexical_request =
      H.Lexical.Request.create ~context:expected ~source:base ~view:Chatmd_effective |> ok
    in
    let changed_lexical_owner =
      H.Lexical.Owner.chatmd
        ~captured_source:base
        ~input_identity:fingerprint
        ~view:Effective
        ~view_sha256:fingerprint
        ~view_byte_length:(A.Source.byte_length base)
      |> ok
    in
    lexical_reply
    := Some
         (H.Lexical.Result.create
            ~request:changed_lexical_request
            ~owner:changed_lexical_owner
            ~spans:[]
            ~issues:[]
            ~status:Reached_eof
          |> ok);
    assert (Result.is_error (Agent_client.Authoring.lexical client lexical_request));
    let assistance_request =
      H.Assistance.Request.create
        ~context:expected
        ~source:base
        ~cursor_byte:1
        ~max_items:1
      |> ok
    in
    assistance_reply
    := Some
         (H.Assistance.Result.create
            ~request:assistance_request
            ~items:[]
            ~status:(Unavailable No_local_context)
          |> ok);
    assert (Result.is_ok (Agent_client.Authoring.assist client assistance_request));
    let changed_assistance_request =
      H.Assistance.Request.create
        ~context:expected
        ~source:base
        ~cursor_byte:2
        ~max_items:1
      |> ok
    in
    assistance_reply
    := Some
         (H.Assistance.Result.create
            ~request:changed_assistance_request
            ~items:[]
            ~status:(Unavailable No_local_context)
          |> ok);
    assert (Result.is_error (Agent_client.Authoring.assist client assistance_request));
    let version_request =
      A.Document.Version_request.create ~context:expected ~source_id:base_id |> ok
    in
    version_reply
    := Some
         (A.Document.Version.create
            ~context:expected
            ~source:foreign
            ~last_editor_revision:None
            ~overlay_open:false
          |> ok);
    print_s
      [%sexp
        (Result.is_error (Agent_client.Authoring.document_version client version_request)
         : bool)];
    let read_request =
      A.Source_read.Read_request.create ~context:expected ~source_id:base_id |> ok
    in
    print_s
      [%sexp
        (Result.is_error (Agent_client.Authoring.read_source client read_request) : bool)];
    let root = P.Id.Authoring_root.of_string "aroot_host" |> ok in
    let host_context_id = P.Id.Authoring_context.of_string "actx_host" |> ok in
    let host_source_id =
      Chatml_source.Id.create
        ~package_namespace:"host"
        ~relative_path:"docs/example.chatmd"
      |> Or_error.ok_exn
    in
    let captured contents = source host_source_id contents in
    let first_source = captured "first" in
    let refreshed_source = captured "refreshed" in
    let host_view generation source ~base ~editable =
      A.Context.create
        ~id:host_context_id
        ~host
        ~origin:
          (A.Context.Host_document { root; source = base; default_environment = None })
        ~generation
        ~basis:(A.Context.Document { source; selection = None })
        ~basis_fingerprint:fingerprint
        ~editable_sources:editable
      |> ok
    in
    let first_view = host_view 0 first_source ~base:first_source ~editable:[] in
    let refreshed_view =
      host_view 1 refreshed_source ~base:refreshed_source ~editable:[]
    in
    open_view := first_view;
    refresh_view := refreshed_view;
    let host_request =
      A.Context.Open_request.create_host_document
        ~root
        ~member:"docs/example.chatmd"
        ~default_environment:None
      |> ok
    in
    let opened_host = Agent_client.Authoring.open_context client host_request |> ok in
    let refreshed =
      Agent_client.Authoring.refresh_context
        client
        (A.Context.Refresh_request.create
           ~expected:(A.Context.Expected.of_view opened_host)
           ~policy:Reject_open_documents)
      |> ok
    in
    print_s
      [%sexp
        (Agent_client.Authoring.is_current_context
           client
           (A.Context.Expected.of_view refreshed)
         : bool)];
    let version0 =
      A.Document.Version.create
        ~context:(A.Context.Expected.of_view refreshed)
        ~source:refreshed_source
        ~last_editor_revision:None
        ~overlay_open:false
      |> ok
    in
    version_reply := Some version0;
    let version_request =
      A.Document.Version_request.create
        ~context:(A.Context.Expected.of_view refreshed)
        ~source_id:host_source_id
      |> ok
    in
    ignore (Agent_client.Authoring.document_version client version_request |> ok);
    let overlay_source contents revision =
      A.Source.create
        ~id:host_source_id
        ~digest:(Chatml_source.Digest.of_contents contents ~checkpoint:Fn.id)
        ~byte_length:(String.length contents)
        ~editor_revision:(Some revision)
      |> ok
    in
    let overlay = overlay_source "new" 1 in
    let edited_view = host_view 2 overlay ~base:refreshed_source ~editable:[ overlay ] in
    let version1 =
      A.Document.Version.create
        ~context:(A.Context.Expected.of_view edited_view)
        ~source:overlay
        ~last_editor_revision:(Some 1)
        ~overlay_open:true
      |> ok
    in
    replace_reply
    := Some (A.Document.Result.create ~context:edited_view ~version:version1 |> ok);
    ignore
      (Agent_client.Authoring.replace_document
         client
         (A.Document.Replace_request.create
            ~expected:version0
            ~next_editor_revision:1
            ~contents:"new"
          |> ok)
       |> ok);
    let closed_view = host_view 3 refreshed_source ~base:refreshed_source ~editable:[] in
    let closed_version =
      A.Document.Version.create
        ~context:(A.Context.Expected.of_view closed_view)
        ~source:refreshed_source
        ~last_editor_revision:(Some 1)
        ~overlay_open:false
      |> ok
    in
    close_reply
    := Some (A.Document.Result.create ~context:closed_view ~version:closed_version |> ok);
    let closed =
      Agent_client.Authoring.close_document
        client
        (A.Document.Close_request.create ~expected:version1)
      |> ok
    in
    print_s [%sexp (A.Document.Version.last_editor_revision closed.version : int option)];
    let wrong = overlay_source "wrong" 2 in
    let wrong_view = host_view 4 wrong ~base:refreshed_source ~editable:[ wrong ] in
    let wrong_version =
      A.Document.Version.create
        ~context:(A.Context.Expected.of_view wrong_view)
        ~source:wrong
        ~last_editor_revision:(Some 2)
        ~overlay_open:true
      |> ok
    in
    replace_reply
    := Some (A.Document.Result.create ~context:wrong_view ~version:wrong_version |> ok);
    print_s
      [%sexp
        (Result.is_error
           (Agent_client.Authoring.replace_document
              client
              (A.Document.Replace_request.create
                 ~expected:closed_version
                 ~next_editor_revision:2
                 ~contents:"submitted"
               |> ok))
         : bool)];
    print_s
      [%sexp
        (Agent_client.Authoring.is_current_context
           client
           (A.Context.Expected.of_view closed_view)
         : bool)];
    let cancel_view = host_view 0 first_source ~base:first_source ~editable:[] in
    let cancel_view =
      A.Context.create
        ~id:(P.Id.Authoring_context.of_string "actx_cancel" |> ok)
        ~host
        ~origin:(A.Context.origin cancel_view)
        ~generation:0
        ~basis:(A.Context.basis cancel_view)
        ~basis_fingerprint:fingerprint
        ~editable_sources:[]
      |> ok
    in
    open_view := cancel_view;
    let opened_cancel = Agent_client.Authoring.open_context client host_request |> ok in
    let cancel_expected = A.Context.Expected.of_view opened_cancel in
    let marker = Failure "authoring read cancelled" in
    (match
       Eio.Cancel.sub (fun context ->
         cancel_read := Some (context, marker);
         ignore
           (Agent_client.Authoring.read_source
              client
              (A.Source_read.Read_request.create
                 ~context:cancel_expected
                 ~source_id:host_source_id
               |> ok)
            : (A.Source_read.Read_result.t, P.Error.t) result))
     with
     | () -> assert false
     | exception Eio.Cancel.Cancelled reason -> assert (phys_equal reason marker));
    cancel_read := None;
    print_s
      [%sexp (Agent_client.Authoring.is_current_context client cancel_expected : bool)];
    current_initialization
    := { initialization with enabled_features = [ A.Features.core ] };
    let without_semantic = { request with features = [ A.Features.core ] } in
    ignore
      (Agent_client.Connection.request
         connection
         (P.Command.Protocol_initialize without_semantic)
       |> ok);
    open_view := context;
    let core_client = Agent_client.Authoring.create connection |> ok in
    let core_context =
      Agent_client.Authoring.open_context core_client open_request |> ok
    in
    let core_request =
      A.Analysis.Analyze.Request.create
        ~context:(A.Context.Expected.of_view core_context)
        ~target:A.Analysis.Target.project
      |> ok
    in
    assert (Result.is_error (Agent_client.Authoring.analyze core_client core_request));
    let core_expected = A.Context.Expected.of_view core_context in
    let core_lexical =
      H.Lexical.Request.create ~context:core_expected ~source:base ~view:Chatml |> ok
    in
    let core_assistance =
      H.Assistance.Request.create
        ~context:core_expected
        ~source:base
        ~cursor_byte:1
        ~max_items:1
      |> ok
    in
    assert (Result.is_error (Agent_client.Authoring.lexical core_client core_lexical));
    assert (Result.is_error (Agent_client.Authoring.assist core_client core_assistance));
    current_initialization := initialization;
    advertised_capabilities
    := A.Capabilities.create
         ~methods:[ "authoring.capabilities"; "authoring.context.open" ]
         ~encodings:[ A.Encoding.Utf8_bytes ]
         ~limits
       |> ok;
    ignore
      (Agent_client.Connection.request connection (P.Command.Protocol_initialize request)
       |> ok);
    let no_method_client = Agent_client.Authoring.create connection |> ok in
    let no_method_context =
      Agent_client.Authoring.open_context no_method_client open_request |> ok
    in
    let no_method_request =
      A.Analysis.Analyze.Request.create
        ~context:(A.Context.Expected.of_view no_method_context)
        ~target:A.Analysis.Target.project
      |> ok
    in
    assert (
      Result.is_error (Agent_client.Authoring.analyze no_method_client no_method_request));
    print_s [%sexp (Agent_client.Authoring.is_current_context client expected : bool)];
    print_s
      [%sexp
        (Result.is_error (Agent_client.Authoring.read_source client read_request) : bool)];
    Agent_client.Connection.close connection;
    print_s [%sexp (Agent_client.Authoring.is_current_context client expected : bool)]);
  [%expect
    {|
    true
    true
    true
    true
    (1)
    true
    false
    false
    false
    true
    false
    |}]
;;
