open! Core
module P = Agent_protocol
module S = Agent_server
module D = P.Developer_common
module W = P.Developer_wire
module R = P.Developer_repl_wire
module T = P.Developer_trace_wire

let fail message = raise_s [%sexp "developer transport E2E failed", (message : string)]
let require condition message = if not condition then fail message

let ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "developer transport protocol error", (error : P.Error.t)]
;;

let transport_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "developer transport failure", (error : string)]
;;

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Support.Port_reservation.create ~sw ~env in
    let port = Support.Port_reservation.port reservation in
    Support.Port_reservation.release reservation;
    port)
;;

let wire_limits () =
  let document ~bytes ~depth ~fields ~nodes =
    match
      Document_schema.Limits.create
        ~max_bytes:bytes
        ~max_depth:depth
        ~max_fields:fields
        ~max_nodes:nodes
    with
    | Ok value -> value
    | Error _ -> fail "invalid Developer wire fixture limits"
  in
  D.Limits.create
    ~request:(document ~bytes:(1024 * 1024) ~depth:16 ~fields:1024 ~nodes:2048)
    ~receipt:(document ~bytes:((1024 * 1024) + 362) ~depth:17 ~fields:1025 ~nodes:2049)
    ~response:(document ~bytes:(2 * 1024 * 1024) ~depth:24 ~fields:2048 ~nodes:4096)
    ~approval_page:(document ~bytes:(1024 * 1024) ~depth:16 ~fields:1024 ~nodes:2048)
    ~max_output_bytes:65536
    ~max_fixture_steps:4
    ~max_fixture_bytes:4096
    ~max_approval_display_bytes:4096
    ~max_approval_decision_reason_bytes:1024
    ~max_approval_page:8
    ~max_event_bytes:4096
  |> ok
;;

let configured_wire_limits () =
  let document ~bytes ~depth ~fields ~nodes =
    match
      Document_schema.Limits.create
        ~max_bytes:bytes
        ~max_depth:depth
        ~max_fields:fields
        ~max_nodes:nodes
    with
    | Ok value -> value
    | Error _ -> fail "invalid configured Developer wire fixture limits"
  in
  D.Limits.create
    ~request:(document ~bytes:(1024 * 1024) ~depth:24 ~fields:2048 ~nodes:4096)
    ~receipt:(document ~bytes:65898 ~depth:17 ~fields:1025 ~nodes:2049)
    ~response:(document ~bytes:69994 ~depth:24 ~fields:1057 ~nodes:2081)
    ~approval_page:(document ~bytes:14080 ~depth:8 ~fields:32 ~nodes:48)
    ~max_output_bytes:65536
    ~max_fixture_steps:64
    ~max_fixture_bytes:65536
    ~max_approval_display_bytes:256
    ~max_approval_decision_reason_bytes:256
    ~max_approval_page:1
    ~max_event_bytes:4096
  |> ok
;;

let configured_repl_decoder_limits () =
  let document ~bytes ~depth ~fields ~nodes =
    match
      Document_schema.Limits.create
        ~max_bytes:bytes
        ~max_depth:depth
        ~max_fields:fields
        ~max_nodes:nodes
    with
    | Ok value -> value
    | Error _ -> fail "invalid configured REPL decoder limits"
  in
  R.Limits.create
    ~request:(document ~bytes:512000 ~depth:24 ~fields:2048 ~nodes:4096)
    ~result:(document ~bytes:512000 ~depth:20 ~fields:2048 ~nodes:4096)
    ~page:(document ~bytes:8192 ~depth:8 ~fields:128 ~nodes:128)
    ~response:(document ~bytes:1048576 ~depth:24 ~fields:4096 ~nodes:8192)
    ~max_source_bytes:65536
    ~max_reference_bytes:384
    ~max_output_bytes:32768
    ~max_page_items:8
    ~max_event_bytes:4096
    ~max_registry_entries:2056
  |> ok
;;

let trace_wire_limits () =
  let bound ~bytes ~depth ~fields ~nodes =
    match
      Document_schema.Limits.create
        ~max_bytes:bytes
        ~max_depth:depth
        ~max_fields:fields
        ~max_nodes:nodes
    with
    | Ok limits -> limits
    | Error _ -> fail "invalid trace fixture limits"
  in
  T.Limits.create
    ~request:(bound ~bytes:4_096 ~depth:12 ~fields:128 ~nodes:256)
    ~page:(bound ~bytes:80_000 ~depth:12 ~fields:256 ~nodes:256)
    ~response:(bound ~bytes:96_000 ~depth:16 ~fields:300 ~nodes:300)
    ~max_page_items:16
    ~max_cursor_bytes:2_048
  |> ok
;;

let trace_configuration () =
  let recorder =
    Chatml_trace.Recorder.Limits.create
      ~max_events:256
      ~max_normal_bytes:65_536
      ~max_terminal_bytes:1_024
    |> Or_error.ok_exn
  in
  let limits =
    S.Daemon.Developer_configuration.Traces.Limits.create
      ~max_traces:16
      ~max_retained_bytes:(2 * 1024 * 1024)
      ~max_source_refs_per_trace:8
      ~max_provenance_bytes_per_trace:4_096
      ~max_expired_tombstones:16
      ~max_age_seconds:60.
      ~max_header_bytes:4_096
      ~recorder
    |> ok
  in
  S.Daemon.Developer_configuration.Traces.create
    ~mode:Chatml_trace.Recorder.Mode.Full
    ~limits
;;

type raw_client =
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
    require (P.Envelope.Request_id.compare id received = 0) "request ID changed";
    outcome
  | P.Envelope.Request _ | P.Envelope.Notification _ ->
    fail "request did not receive a response"
;;

let call client ~method_ ~params = call_outcome client ~method_ ~params |> ok

let call_authoring client request =
  let method_ = P.Authoring_wire.Request.method_name request in
  let result = call client ~method_ ~params:(P.Authoring_wire.Request.params request) in
  P.Authoring_wire.Response.of_method_and_json ~method_ result |> ok
;;

let call_developer client ~limits request =
  let method_ = W.Request.method_name request in
  let result = call client ~method_ ~params:(W.Request.params request) in
  W.Response.of_method_and_json ~limits ~method_ result |> ok
;;

let call_repl client ~limits request =
  let method_ = R.Request.method_name request in
  let result = call client ~method_ ~params:(R.Request.params request) in
  R.Response.of_method_and_json ~limits ~method_ result |> ok
;;

let call_trace client ~limits request =
  let method_ = T.Request.method_name request in
  let result = call client ~method_ ~params:(T.Request.params request) in
  T.Response.of_method_and_json ~limits ~method_ result |> ok
;;

let initialize_response ?(repl = false) ?(trace = false) client =
  let implementation =
    P.Initialize.Implementation.create ~name:"developer-wire-e2e" ~version:"1" |> ok
  in
  let request =
    P.Initialize.Request.create
      ~implementation
      ~protocol_min:P.Version.current
      ~protocol_max:P.Version.current
      ~features:
        ([ P.Authoring.Features.core; P.Authoring.Features.semantic; D.Features.run ]
         @ (if repl then [ R.Features.repl; R.Features.events ] else [])
         @ if trace then [ T.Features.trace ] else [])
      ~event_encodings:[ Json ]
      ~max_inbound_event_bytes:(2 * 1024 * 1024)
      ()
    |> ok
  in
  call
    client
    ~method_:"protocol.initialize"
    ~params:(P.Initialize.Request.to_json request)
  |> P.Initialize.Response.of_json
  |> ok
;;

let initialize ?(trace = false) client =
  let response = initialize_response ~trace client in
  require
    (List.mem response.enabled_features D.Features.run ~equal:String.equal)
    "Developer run feature was not selected";
  if trace
  then
    require
      (List.mem response.enabled_features T.Features.trace ~equal:String.equal)
      "installed trace feature was not selected"
;;

let qualify_unconfigured_client client =
  Exn.protect
    ~f:(fun () ->
      let response = initialize_response client in
      require
        (not (List.mem response.enabled_features D.Features.run ~equal:String.equal))
        "unconfigured daemon selected Developer run";
      match
        call_outcome client ~method_:"developer.capabilities" ~params:(`Object [])
      with
      | Error { code = P.Error.Method_not_found; _ } -> ()
      | Error error ->
        raise_s
          [%sexp
            "unconfigured Developer method returned another error", (error : P.Error.t)]
      | Ok _ -> fail "unconfigured daemon exposed Developer capabilities")
    ~finally:client.close
;;

let qualify_trace_disabled client =
  Exn.protect
    ~f:(fun () ->
      let response = initialize_response ~trace:true client in
      require
        (not (List.mem response.enabled_features T.Features.trace ~equal:String.equal))
        "uninstalled trace feature was selected";
      match
        call_outcome
          client
          ~method_:(T.Request.method_name T.Request.Capabilities)
          ~params:(T.Request.params T.Request.Capabilities)
      with
      | Error { code = P.Error.Invalid_state; _ } -> ()
      | Error error ->
        raise_s [%sexp "uninstalled trace method had another error", (error : P.Error.t)]
      | Ok _ -> fail "uninstalled trace method returned capabilities")
    ~finally:client.close
;;

let open_and_analyze client =
  let source = {|let main input = Task.pure(input)|} in
  let opened =
    P.Authoring.Context.Open_request.create_supplied_document
      ~member:"draft.chatmd"
      ~default_environment:None
      ~contents:
        ("<script id=\"s\" language=\"chatml\" kind=\"tool\">" ^ source ^ "</script>")
    |> ok
  in
  let expected =
    match call_authoring client (P.Authoring_wire.Request.Context_open opened) with
    | P.Authoring_wire.Response.Context_open context ->
      P.Authoring.Context.Expected.of_view context
    | _ -> fail "Authoring Context_open returned another method"
  in
  let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
  let analyze =
    P.Authoring.Analysis.Analyze.Request.create ~context:expected ~target:selector |> ok
  in
  (match call_authoring client (P.Authoring_wire.Request.Analyze analyze) with
   | P.Authoring_wire.Response.Analyze _ -> ()
   | _ -> fail "Authoring Analyze returned another method");
  expected, selector
;;

let qualify_rejected_check client ~limits ~run_id =
  let source = "let main input = Task.pure(no_such_developer_identifier)" in
  let document =
    "<script id=\"s\" language=\"chatml\" kind=\"tool\">" ^ source ^ "</script>"
  in
  let opened =
    P.Authoring.Context.Open_request.create_supplied_document
      ~member:"draft.chatmd"
      ~default_environment:None
      ~contents:document
    |> ok
  in
  let expected =
    match call_authoring client (P.Authoring_wire.Request.Context_open opened) with
    | P.Authoring_wire.Response.Context_open context ->
      P.Authoring.Context.Expected.of_view context
    | _ -> fail "rejected Check Context_open returned another method"
  in
  let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
  let analyze =
    P.Authoring.Analysis.Analyze.Request.create ~context:expected ~target:selector |> ok
  in
  (match call_authoring client (P.Authoring_wire.Request.Analyze analyze) with
   | P.Authoring_wire.Response.Analyze _ -> ()
   | _ -> fail "rejected Check Analyze returned another method");
  let route =
    D.Route.document_script ~limits:(D.Limits.request limits) ~expected ~selector |> ok
  in
  let key = P.Idempotency_key.of_string "developer-wire-rejected-check" |> ok in
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
    match call_developer client ~limits (W.Request.Submit submit) with
    | W.Response.Submit value -> value
    | _ -> fail "rejected Check Submit returned another method"
  in
  let lookup = D.Lookup.create ~run_id ~key in
  let terminal =
    match call_developer client ~limits (W.Request.Await_terminal lookup) with
    | W.Response.Await_terminal value -> value
    | _ -> fail "rejected Check Await_terminal returned another method"
  in
  let observed =
    match call_developer client ~limits (W.Request.Observe lookup) with
    | W.Response.Observe (Some value) -> value
    | _ -> fail "rejected Check Observe did not return its receipt"
  in
  require
    (P.Developer_command_id.equal
       (D.View.command_id submitted)
       (D.View.command_id terminal)
     && P.Developer_command_id.equal
          (D.View.command_id terminal)
          (D.View.command_id observed))
    "rejected Check changed command identity";
  let rejected view =
    match D.View.phase view with
    | D.View.Terminal { execution_started = false; receipt } ->
      (match P.Developer_receipt.value receipt with
       | P.Developer_receipt.Check (P.Developer_receipt.Check.Rejected value) -> value
       | _ -> fail "rejected Check returned another receipt")
    | _ -> fail "rejected Check did not settle before execution"
  in
  let terminal_diagnostic = rejected terminal in
  let observed_diagnostic = rejected observed in
  require
    (String.equal terminal_diagnostic.code "chatml.invalid_handler")
    "rejected Check diagnostic code changed";
  let location = Option.value_exn terminal_diagnostic.location in
  require
    (Option.equal
       (fun (left : P.Developer_receipt.Diagnostic.Location.t)
         (right : P.Developer_receipt.Diagnostic.Location.t) ->
          Chatml_source.Id.equal left.source right.source
          && Chatml_source.Digest.equal left.digest right.digest
          && Int.equal left.source_bytes right.source_bytes
          && Int.equal left.start_byte right.start_byte
          && Int.equal left.end_byte right.end_byte)
       terminal_diagnostic.location
       observed_diagnostic.location)
    "rejected Check Await and Observe changed source attribution";
  require
    (let relative_path = Chatml_source.Id.relative_path location.source in
     String.is_prefix relative_path ~prefix:"fragments/"
     && String.is_suffix
          relative_path
          ~suffix:(Printf.sprintf "/0-%d.chatml" (String.length document))
     && Chatml_source.Digest.equal
          location.digest
          (Chatml_source.Digest.of_contents source ~checkpoint:ignore)
     && Int.equal location.source_bytes (String.length source)
     && Int.equal location.start_byte 27
     && Int.equal location.end_byte 55)
    "rejected Check diagnostic lost its captured fragment, digest or byte range"
;;

let qualify_trace client ~run_id ~key =
  let caps =
    match call_trace client ~limits:(trace_wire_limits ()) T.Request.Capabilities with
    | T.Response.Capabilities value -> value
    | _ -> fail "trace Capabilities returned another method"
  in
  let limits = T.Capabilities.limits caps in
  require
    (List.mem (T.Capabilities.methods caps) "developer.trace.page" ~equal:String.equal)
    "installed trace page method was not advertised";
  let locator = T.Locator.Command (D.Lookup.create ~run_id ~key) in
  let request = T.Page_request.create ~limits ~locator ~after:None ~limit:16 |> ok in
  let page =
    match call_trace client ~limits (T.Request.Page request) with
    | T.Response.Page (T.Availability.Retained page) -> page
    | _ -> fail "terminal Check trace was not retained"
  in
  require (not (List.is_empty (T.Page.events page))) "terminal Check trace lost events";
  (match T.Page.state page with
   | T.Page.Complete terminal ->
     require
       (match T.Terminal.disposition terminal with
        | Succeeded -> true
        | Failed _ | Cancelled | Denied | Interrupted -> false)
       "terminal Check trace changed disposition"
   | Incomplete -> fail "terminal Check trace remains incomplete");
  let foreign =
    T.Cursor.create
      ~limits
      ~locator
      ~execution_id:(T.Header.execution_id (T.Page.header page))
      ~submission_fingerprint:
        (Chatml_source.Digest.of_contents "foreign trace" ~checkpoint:ignore)
      ~selection_fingerprint:(T.Header.selection_fingerprint (T.Page.header page))
      ~after_sequence:1L
    |> ok
  in
  let foreign_request =
    T.Page_request.create ~limits ~locator ~after:(Some foreign) ~limit:16 |> ok
  in
  (match
     call_outcome
       client
       ~method_:(T.Request.method_name (T.Request.Page foreign_request))
       ~params:(T.Request.params (T.Request.Page foreign_request))
   with
   | Error { code = P.Error.Lease_stale; _ } -> ()
   | Error error ->
     raise_s [%sexp "foreign trace cursor had another error", (error : P.Error.t)]
   | Ok _ -> fail "foreign trace cursor was accepted");
  let capabilities = T.Request.Capabilities in
  let request_with_id count =
    let id = P.Envelope.Request_id.of_json (`String (String.make count '"')) |> ok in
    P.Envelope.request
      ~id
      ~method_:(T.Request.method_name capabilities)
      ~params:(T.Request.params capabilities)
      ()
    |> client.send
  in
  (match request_with_id 127 with
   | P.Envelope.Response { outcome = Ok _; _ } -> ()
   | _ -> fail "exact 256-byte escaped trace request ID did not fit");
  match request_with_id 128 with
  | P.Envelope.Response { outcome = Error { code = P.Error.Resource_limit; _ }; _ } -> ()
  | _ -> fail "oversized escaped trace request ID was accepted"
;;

let qualify_unselected_trace client ~run_id ~key =
  Exn.protect
    ~f:(fun () ->
      initialize ~trace:true client;
      let limits = trace_wire_limits () in
      let locator = T.Locator.Command (D.Lookup.create ~run_id ~key) in
      let request = T.Page_request.create ~limits ~locator ~after:None ~limit:1 |> ok in
      match
        call_outcome
          client
          ~method_:(T.Request.method_name (T.Request.Page request))
          ~params:(T.Request.params (T.Request.Page request))
      with
      | Error { code = P.Error.Lease_stale; _ } -> ()
      | Error error ->
        raise_s [%sexp "unselected trace read had another error", (error : P.Error.t)]
      | Ok _ -> fail "unselected principal disclosed a retained trace")
    ~finally:client.close
;;

let qualify_client
      ?expected_wire_limits
      ?(trace = false)
      ?(after_check = fun _ _ -> ())
      client
  =
  Exn.protect
    ~f:(fun () ->
      initialize ~trace client;
      let caps =
        match call_developer client ~limits:(wire_limits ()) W.Request.Capabilities with
        | W.Response.Capabilities caps -> caps
        | _ -> fail "Developer capabilities returned another method"
      in
      require (D.Capabilities.supports caps D.Method.Submit) "Submit is not installed";
      require (D.Capabilities.supports caps D.Method.Observe) "Observe is not installed";
      require
        (D.Capabilities.supports caps D.Method.Await_terminal)
        "Await_terminal is not installed";
      let limits = D.Capabilities.limits caps in
      let expected_wire_limits =
        Option.value expected_wire_limits ~default:(wire_limits ())
      in
      require
        (String.equal
           (D.Limits.to_json limits |> Jsonaf.to_string)
           (D.Limits.to_json expected_wire_limits |> Jsonaf.to_string))
        "advertised Developer limits differ from installed policy";
      let expected, selector = open_and_analyze client in
      let route =
        D.Route.document_script ~limits:(D.Limits.request limits) ~expected ~selector
        |> ok
      in
      let run_id = P.Id.Developer_run.create () in
      let key = P.Idempotency_key.of_string "developer-wire-check" |> ok in
      let input = `Object [ "transport", `String "raw" ] in
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
          ~input
          ~fixtures:None
          ~requested_execution:None
          ~requested_max_output_bytes:None
        |> ok
      in
      let submitted =
        match call_developer client ~limits (W.Request.Submit submit) with
        | W.Response.Submit view -> view
        | _ -> fail "Developer Submit returned another method"
      in
      let lookup = D.Lookup.create ~run_id ~key in
      let terminal =
        match call_developer client ~limits (W.Request.Await_terminal lookup) with
        | W.Response.Await_terminal view -> view
        | _ -> fail "Developer Await_terminal returned another method"
      in
      let observed =
        match call_developer client ~limits (W.Request.Observe lookup) with
        | W.Response.Observe (Some view) -> view
        | W.Response.Observe None -> fail "submitted Developer Check was not observable"
        | _ -> fail "Developer Observe returned another method"
      in
      let retried =
        match call_developer client ~limits (W.Request.Submit submit) with
        | W.Response.Submit view -> view
        | _ -> fail "same-key Developer retry returned another method"
      in
      require
        (P.Developer_command_id.equal
           (D.View.command_id retried)
           (D.View.command_id observed))
        "same-key retry created another command";
      require
        (Chatml_source.Digest.equal
           (D.View.capture_fingerprint retried)
           (D.View.capture_fingerprint observed))
        "same-key retry changed the captured source";
      require
        (P.Developer_command_id.equal
           (D.View.command_id submitted)
           (D.View.command_id observed))
        "Submit and Observe identified different commands";
      require
        (P.Developer_command_id.equal
           (D.View.command_id terminal)
           (D.View.command_id observed))
        "Await and Observe identified different commands";
      require
        (P.Id.Developer_run.equal (D.View.owner observed).run_id run_id)
        "observed Developer owner belongs to another run";
      require
        (P.Idempotency_key.equal (D.View.key observed) key)
        "observed Developer key changed";
      require
        (Chatml_source.Digest.equal
           (D.View.request_fingerprint submitted)
           (D.View.request_fingerprint observed))
        "Submit and Observe request fingerprints differ";
      require
        (Chatml_source.Digest.equal
           (D.View.capture_fingerprint submitted)
           (D.View.capture_fingerprint observed))
        "Submit and Observe capture fingerprints differ";
      (match D.View.phase observed with
       | D.View.Terminal { execution_started = false; receipt } ->
         (match P.Developer_receipt.value receipt with
          | P.Developer_receipt.Check P.Developer_receipt.Check.Ready -> ()
          | _ -> fail "Developer Check did not produce Ready")
       | _ -> fail "Developer Check was not terminal");
      qualify_rejected_check client ~limits ~run_id;
      if trace then qualify_trace client ~run_id ~key;
      after_check run_id key)
    ~finally:client.close
;;

let qualify_configured_repl client =
  Exn.protect
    ~f:(fun () ->
      let initialized = initialize_response ~repl:true client in
      require
        (List.mem initialized.enabled_features R.Features.repl ~equal:String.equal)
        "configured REPL feature was not selected";
      let caps =
        match
          call_repl
            client
            ~limits:(configured_repl_decoder_limits ())
            R.Request.Capabilities
        with
        | R.Response.Capabilities caps -> caps
        | _ -> fail "configured REPL capabilities returned another method"
      in
      let limits = R.Capabilities.limits caps in
      require
        (R.Capabilities.supports caps (R.Request.method_name R.Request.Capabilities))
        "configured REPL capabilities did not advertise REPL";
      require
        (Int.equal (R.Limits.max_output_bytes limits) 32768)
        "configured REPL wire output ceiling changed";
      let expected, selector = open_and_analyze client in
      let route =
        D.Route.document_script ~limits:(R.Limits.request limits) ~expected ~selector
        |> ok
      in
      let key value = P.Idempotency_key.of_string value |> ok in
      let created =
        let request =
          R.Create.create
            ~limits
            ~run_id:(P.Id.Developer_run.create ())
            ~key:(key "configured-wire-repl-create")
            ~route
            ~target:One_off_v1
            ~prompt:(S.Catalog_identity.prompt_definition "smoke")
            ~workspace:(S.Catalog_identity.workspace_definition "physical")
            ~requested_execution:None
            ~requested_max_output_bytes:None
          |> ok
        in
        match call_repl client ~limits (R.Request.Create request) with
        | R.Response.Create value -> value
        | _ -> fail "configured REPL Create returned another method"
      in
      let submitted =
        let request =
          R.Submit.create
            ~limits
            ~instance:(R.View.instance created)
            ~epoch:(R.View.epoch created)
            ~expected_revision:(R.View.revision created)
            ~key:(key "configured-wire-repl-submit")
            ~source:"Task.pure(8)"
          |> ok
        in
        match call_repl client ~limits (R.Request.Submit request) with
        | R.Response.Submit value -> value
        | _ -> fail "configured REPL Submit returned another method"
      in
      let reference =
        match R.Submission.task_references submitted with
        | [ reference ] -> reference
        | _ -> fail "4 KiB REPL receipt did not expose its Task reference"
      in
      let lookup =
        R.Lookup.create
          ~instance:(R.View.instance created)
          ~key:(key "configured-wire-repl-submit")
      in
      let observed =
        match call_repl client ~limits (R.Request.Observe lookup) with
        | R.Response.Observe (Some value) -> value
        | _ -> fail "configured REPL submission was not observable"
      in
      require
        (R.Wire_id.Revision.equal
           (R.Submission.revision submitted)
           (R.Submission.revision observed))
        "configured REPL Observe changed revision";
      let current =
        R.Current_epoch.create
          ~instance:(R.View.instance created)
          ~epoch:(R.View.epoch created)
          ~key:(key "configured-wire-repl-submit")
      in
      let page =
        let request =
          R.Reference_page_request.create ~limits ~current ~after:None ~limit:1 |> ok
        in
        match call_repl client ~limits (R.Request.Task_references_page request) with
        | R.Response.Task_references_page value -> value
        | _ -> fail "configured REPL reference page returned another method"
      in
      require
        (List.equal String.equal (R.Reference_page.items page) [ reference ])
        "configured REPL reference page lost its Task";
      let inspected =
        let request =
          R.Inspect.create
            ~limits
            ~current
            ~statement_index:0
            ~selector:R.Type_selector.Expression
            ~max_bytes:256
          |> ok
        in
        match call_repl client ~limits (R.Request.Inspect_type request) with
        | R.Response.Inspect_type value -> value
        | _ -> fail "configured REPL Inspect_type returned another method"
      in
      require
        (not
           (String.is_empty
              (R.Submission.Rendered_type.text (R.Type_inspection.rendered inspected))))
        "configured REPL type inspection was empty";
      let task_key = key "configured-wire-repl-task" in
      let task_lookup = D.Lookup.create ~run_id:(R.View.run_id created) ~key:task_key in
      (match call_repl client ~limits (R.Request.Task_observe task_lookup) with
       | R.Response.Task_observe None -> ()
       | _ -> fail "unsubmitted configured REPL Task key was unexpectedly present");
      let task_request =
        R.Task_submit.create
          ~limits
          ~key:task_key
          ~reference
          ~requested_execution:None
          ~requested_max_output_bytes:None
          ~fresh_epoch:(Some (R.View.instance created, R.View.epoch created))
        |> ok
      in
      let started =
        match call_repl client ~limits (R.Request.Task_submit task_request) with
        | R.Response.Task_submit value -> value
        | _ -> fail "configured REPL Task_submit returned another method"
      in
      let terminal =
        match call_repl client ~limits (R.Request.Task_await_terminal task_lookup) with
        | R.Response.Task_await_terminal value -> value
        | _ -> fail "configured REPL Task_await_terminal returned another method"
      in
      require
        (P.Developer_command_id.equal
           (R.Task_view.command_id started)
           (R.Task_view.command_id terminal))
        "configured REPL Task identity changed";
      (match R.Task_view.phase terminal with
       | Terminal { execution_started = true; outcome = Succeeded (Some preview) } ->
         require
           (String.equal preview "8")
           "configured REPL Task returned an unexpected result"
       | Terminal { outcome = Failed { error; diagnostic = _ }; _ }
       | Terminal { outcome = Limited { error; diagnostic = _ }; _ } ->
         raise_s [%sexp "configured REPL Task failed", (error : P.Error.t)]
       | _ -> fail "configured REPL Task was not successful");
      (match call_repl client ~limits (R.Request.Task_observe task_lookup) with
       | R.Response.Task_observe (Some value)
         when P.Developer_command_id.equal
                (R.Task_view.command_id terminal)
                (R.Task_view.command_id value) -> ()
       | _ -> fail "configured REPL Task was not observable");
      (* A native-shaped Task carries the recursive JSON type graph. Its
         compact reference must survive the configured 4 KiB receipt ceiling
         without increasing that ceiling to fit frozen type details. *)
      let native_submission =
        R.Submit.create
          ~limits
          ~instance:(R.View.instance created)
          ~epoch:(R.View.epoch created)
          ~expected_revision:(R.Submission.revision submitted)
          ~key:(key "configured-wire-repl-native-shape")
          ~source:
            {|Tool.call("read_file", `Object([{key="file"; value=`String("sample.txt")}]))|}
        |> ok
      in
      (match call_repl client ~limits (R.Request.Submit native_submission) with
       | R.Response.Submit value ->
         require
           (Int.equal (List.length (R.Submission.task_references value)) 1)
           "4 KiB REPL receipt lost the native Task reference"
       | _ -> fail "configured native-shaped Submit returned another method");
      (match
         call_repl
           client
           ~limits
           (R.Request.Reset_and_wait
              (R.Current_epoch.create
                 ~instance:(R.View.instance created)
                 ~epoch:(R.View.epoch created)
                 ~key:(key "configured-wire-repl-reset")))
       with
       | R.Response.Reset_and_wait next ->
         require
           (not (R.Wire_id.Epoch.equal (R.View.epoch created) (R.View.epoch next)))
           "configured REPL Reset did not replace epoch"
       | _ -> fail "configured REPL Reset returned another method");
      match
        call_repl
          client
          ~limits
          (R.Request.Close_and_wait
             (R.Lookup.create
                ~instance:(R.View.instance created)
                ~key:(key "configured-wire-repl-close")))
      with
      | R.Response.Close_and_wait -> ()
      | _ -> fail "configured REPL Close returned another method")
    ~finally:client.close
;;

let qualify_typed_repl_cancel connection =
  Exn.protect
    ~finally:(fun () -> Agent_client.Connection.close connection)
    ~f:(fun () ->
      let implementation =
        P.Initialize.Implementation.create ~name:"repl-control-e2e" ~version:"1" |> ok
      in
      let request =
        P.Initialize.Request.create
          ~implementation
          ~protocol_min:P.Version.current
          ~protocol_max:P.Version.current
          ~features:
            [ P.Authoring.Features.core
            ; P.Authoring.Features.semantic
            ; D.Features.run
            ; R.Features.repl
            ; R.Features.events
            ]
          ~event_encodings:[ Json ]
          ~max_inbound_event_bytes:(2 * 1024 * 1024)
          ()
        |> ok
      in
      (match
         Agent_client.Connection.request_without_history
           connection
           (Protocol_initialize request)
         |> ok
       with
       | Protocol_initialize initialized ->
         require
           (List.mem initialized.enabled_features R.Features.repl ~equal:String.equal)
           "typed control connection did not select REPL"
       | _ -> fail "typed control Initialize changed method");
      let repl = Agent_client.Developer_repl_connection.create connection |> ok in
      let limits =
        Agent_client.Developer_repl_connection.capabilities repl |> R.Capabilities.limits
      in
      let opened =
        P.Authoring.Context.Open_request.create_supplied_document
          ~member:"draft.chatmd"
          ~default_environment:None
          ~contents:
            {|<script id="s" language="chatml" kind="tool">let main input = Task.pure(input)</script>|}
        |> ok
      in
      let expected =
        match
          Agent_client.Connection.request_without_history
            connection
            (Authoring (Context_open opened))
          |> ok
        with
        | Authoring (Context_open context) -> P.Authoring.Context.Expected.of_view context
        | _ -> fail "typed control context open changed method"
      in
      let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
      let analyze =
        P.Authoring.Analysis.Analyze.Request.create ~context:expected ~target:selector
        |> ok
      in
      (match
         Agent_client.Connection.request_without_history
           connection
           (Authoring (Analyze analyze))
         |> ok
       with
       | Authoring (Analyze _) -> ()
       | _ -> fail "typed control analyze changed method");
      let route =
        D.Route.document_script ~limits:(R.Limits.request limits) ~expected ~selector
        |> ok
      in
      let key name = P.Idempotency_key.of_string name |> ok in
      let created =
        R.Create.create
          ~limits
          ~run_id:(P.Id.Developer_run.create ())
          ~key:(key "typed-control-create")
          ~route
          ~target:One_off_v1
          ~prompt:(S.Catalog_identity.prompt_definition "smoke")
          ~workspace:(S.Catalog_identity.workspace_definition "physical")
          ~requested_execution:None
          ~requested_max_output_bytes:None
        |> ok
        |> Agent_client.Developer_repl_connection.create_instance repl
        |> ok
      in
      let held_key = key "typed-control-held-submit" in
      let held_lookup =
        R.Lookup.create ~instance:(R.View.instance created) ~key:held_key
      in
      let held_request =
        R.Submit.create
          ~limits
          ~instance:(R.View.instance created)
          ~epoch:(R.View.epoch created)
          ~expected_revision:(R.View.revision created)
          ~key:held_key
          ~source:"let rec loop x = loop(x)\nlet never = loop(0)"
        |> ok
      in
      let submitted = ref None in
      let cancelled = ref None in
      Eio.Fiber.both
        (fun () ->
           submitted
           := Some (Agent_client.Developer_repl_connection.submit repl held_request))
        (fun () ->
           let rec cancel_exact () =
             match
               Agent_client.Developer_repl_connection.cancel_submission repl held_lookup
             with
             | Error { code = P.Error.Lease_stale; _ } ->
               Eio.Fiber.yield ();
               cancel_exact ()
             | result -> cancelled := Some result
           in
           cancel_exact ());
      let submitted = Option.value_exn !submitted |> ok in
      let cancelled = Option.value_exn !cancelled |> ok in
      require
        (R.Wire_id.Revision.equal
           (R.Submission.revision submitted)
           (R.Submission.revision cancelled))
        "typed control returned another submission identity";
      match R.Submission.status submitted with
      | Stopped (Some diagnostic)
        when String.is_substring
               (R.Submission.Diagnostic.message diagnostic)
               ~substring:"cancelled" -> ()
      | _ -> fail "typed control did not cancel held Submit")
;;

let qualify_unselected_principal client =
  Exn.protect
    ~f:(fun () ->
      initialize client;
      let limits =
        match
          call_developer client ~limits:(configured_wire_limits ()) W.Request.Capabilities
        with
        | W.Response.Capabilities caps -> D.Capabilities.limits caps
        | _ -> fail "unselected principal could not read installed capabilities"
      in
      let expected, selector = open_and_analyze client in
      let route =
        D.Route.document_script ~limits:(D.Limits.request limits) ~expected ~selector
        |> ok
      in
      let submit =
        D.Submit.create
          ~limits
          ~run_id:(P.Id.Developer_run.create ())
          ~key:(P.Idempotency_key.of_string "unselected-principal" |> ok)
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
      match
        call_outcome
          client
          ~method_:(W.Request.method_name (W.Request.Submit submit))
          ~params:(W.Request.params (W.Request.Submit submit))
      with
      | Error { code = P.Error.Permission_denied; _ } -> ()
      | Error error ->
        raise_s [%sexp "unselected principal had another denial", (error : P.Error.t)]
      | Ok _ -> fail "unselected principal admitted a Developer command")
    ~finally:client.close
;;

let qualify_repl_capacity daemon fixture =
  let module Repl = S.Daemon.Developer.Repl in
  let principal =
    P.Principal.create
      ~id:(Support.Config_fixture.public_principal fixture)
      ~authentication_kind:"http.static_bearer"
      ~scopes:Support.Config_fixture.all_scopes
      ~attributes:[]
    |> ok
  in
  let actor = Operator_authorization.trusted_local principal in
  let connection =
    S.Connection_context.create_authenticated
      ~actor
      ~connection_id:P.Id.Transaction.(create () |> to_string)
      ~transport:In_memory
      ~publish_notification:(fun _ -> ())
      ~max_attachments:8
  in
  Exn.protect
    ~f:(fun () ->
      let dispatch command =
        S.Dispatcher.dispatch_command
          (S.Daemon.dispatcher daemon)
          ~context:connection
          command
        |> ok
      in
      let implementation =
        P.Initialize.Implementation.create ~name:"configured-repl" ~version:"1" |> ok
      in
      let initialize =
        P.Initialize.Request.create
          ~implementation
          ~protocol_min:P.Version.current
          ~protocol_max:P.Version.current
          ~features:
            [ P.Authoring.Features.core; P.Authoring.Features.semantic; D.Features.run ]
          ~event_encodings:[ Json ]
          ~max_inbound_event_bytes:(2 * 1024 * 1024)
          ()
        |> ok
      in
      (match dispatch (P.Command.Protocol_initialize initialize) with
       | P.Public.Result.Non_history result ->
         (match P.Public.Result.Non_history.value result with
          | P.Method_result.Protocol_initialize _ -> ()
          | _ -> fail "in-process REPL connection did not initialize")
       | _ -> fail "in-process REPL connection did not initialize");
      let opened =
        P.Authoring.Context.Open_request.create_supplied_document
          ~member:"repl.chatmd"
          ~default_environment:None
          ~contents:
            "<script id=\"s\" language=\"chatml\" kind=\"tool\">let main input = \
             Task.pure(input)</script>"
        |> ok
      in
      let expected =
        match
          dispatch (P.Command.Authoring (P.Authoring_wire.Request.Context_open opened))
        with
        | P.Public.Result.Non_history result ->
          (match P.Public.Result.Non_history.value result with
           | P.Method_result.Authoring (P.Authoring_wire.Response.Context_open context) ->
             P.Authoring.Context.Expected.of_view context
           | _ -> fail "in-process REPL source did not open")
        | _ -> fail "in-process REPL source did not open"
      in
      let selector = P.Authoring.Analysis.Target.selected_script "s" |> ok in
      let analyze =
        P.Authoring.Analysis.Analyze.Request.create ~context:expected ~target:selector
        |> ok
      in
      (match
         dispatch (P.Command.Authoring (P.Authoring_wire.Request.Analyze analyze))
       with
       | P.Public.Result.Non_history result ->
         (match P.Public.Result.Non_history.value result with
          | P.Method_result.Authoring (P.Authoring_wire.Response.Analyze _) -> ()
          | _ -> fail "in-process REPL source analysis did not complete")
       | _ -> fail "in-process REPL source analysis did not complete");
      let route =
        S.Daemon.Developer.Request.Route.Document_script { expected; selector }
      in
      let original =
        Repl.Original.create
          ~route
          ~target:Chatml.Chatml_compiler_contract.Target.One_off_v1
          ~prompt:(S.Catalog_identity.prompt_definition "smoke")
          ~workspace:(S.Catalog_identity.workspace_definition "physical")
          ~requested_execution:None
          ~requested_max_output_bytes:None
          ~checkpoint:ignore
        |> ok
      in
      let view =
        Repl.create
          daemon
          ~actor
          ~run_id:(P.Id.Developer_run.create ())
          ~key:(P.Idempotency_key.of_string "configured-repl-create" |> ok)
          ~original
          ~fresh_source:
            (Some (S.Daemon.Developer.Request.Fresh_source.Document_script { connection }))
        |> ok
      in
      let rec submit_many revision index =
        if index = 10
        then ()
        else (
          let key =
            P.Idempotency_key.of_string ("configured-repl-" ^ Int.to_string index) |> ok
          in
          let result =
            Repl.submit
              daemon
              ~actor
              ~instance:(Repl.View.instance view)
              ~epoch:(Repl.View.epoch view)
              ~expected_revision:revision
              ~key
              ~source:(sprintf "let value_%d = %d" index index)
            |> ok
          in
          (match Repl.Submission.status result with
           | Finished -> ()
           | Need_more _ | Rejected _ | Stopped _ ->
             fail "configured REPL did not retain a submitted statement");
          submit_many (Repl.Submission.revision result) (index + 1))
      in
      submit_many (Repl.View.revision view) 0)
    ~finally:(fun () -> S.Daemon.close_connection daemon connection)
;;

let qualify_own_control_without_approval_scope daemon fixture run_id key =
  let principal =
    P.Principal.create
      ~id:(Support.Config_fixture.public_principal fixture)
      ~authentication_kind:"http.static_bearer"
      ~scopes:(P.Scope.Set.of_list [ P.Scope.Developer_check ])
      ~attributes:[]
    |> ok
  in
  let actor = Operator_authorization.trusted_local principal in
  let view = S.Daemon.Developer.cancel_and_wait daemon ~actor ~run_id ~key |> ok in
  require
    (P.Id.Developer_run.equal (S.Daemon.Developer.View.owner view).run_id run_id)
    "own Check control returned a foreign run"
;;

let http_client ?token ~sw env fixture =
  let token = Option.value token ~default:(Support.Config_fixture.public_token fixture) in
  let driver =
    Support.Http_driver.create
      ~sw
      ~env
      ~port:(Support.Config_fixture.http_port fixture)
      ~token:(Some token)
    |> transport_ok
  in
  { next_id = 1
  ; send =
      (fun request ->
        let response =
          Support.Http_driver.rpc_raw
            driver
            (P.Envelope.to_json request |> Jsonaf.to_string)
          |> transport_ok
        in
        require (Int.equal response.status 200) "HTTP Developer RPC failed";
        Jsonaf.parse response.body |> Or_error.ok_exn |> P.Envelope.of_json |> ok)
  ; close = (fun () -> Support.Http_driver.shutdown driver)
  }
;;

let rec read_unix_response raw clock remaining =
  if remaining = 0 then fail "too many Unix notifications before response";
  match Support.Unix_driver.read_envelope raw ~clock ~timeout_seconds:5. with
  | Support.Unix_driver.Envelope (P.Envelope.Notification _) ->
    read_unix_response raw clock (remaining - 1)
  | Support.Unix_driver.Envelope response -> response
  | Support.Unix_driver.End_of_file
  | Support.Unix_driver.Timeout
  | Support.Unix_driver.Invalid_response _ -> fail "Unix Developer response unavailable"
;;

let unix_client ~sw env fixture =
  let raw =
    Support.Unix_driver.connect_raw
      ~sw
      ~env
      ~socket_path:(Support.Config_fixture.unix_socket fixture)
      ~max_response_bytes:(2 * 1024 * 1024)
  in
  { next_id = 1
  ; send =
      (fun request ->
        Support.Unix_driver.send_line raw (P.Envelope.to_json request |> Jsonaf.to_string);
        read_unix_response raw (Eio.Stdenv.clock env) 4)
  ; close = (fun () -> Support.Unix_driver.close_raw raw)
  }
;;

module Native_hold = struct
  type t =
    { entered : unit Eio.Promise.t
    ; signal_entered : unit Eio.Promise.u
    ; release : unit Eio.Promise.t
    ; signal_release : unit Eio.Promise.u
    ; mutable finished : bool
    ; mutable returned : bool
    }

  let create () =
    let entered, signal_entered = Eio.Promise.create () in
    let release, signal_release = Eio.Promise.create () in
    { entered
    ; signal_entered
    ; release
    ; signal_release
    ; finished = false
    ; returned = false
    }
  ;;

  let await t =
    require (not (Eio.Promise.is_resolved t.entered)) "native hold entered twice";
    Eio.Promise.resolve t.signal_entered ();
    Exn.protect
      ~finally:(fun () -> t.finished <- true)
      ~f:(fun () ->
        Eio.Promise.await t.release;
        t.returned <- true)
  ;;

  let require_cancelled t =
    require
      (t.finished && not t.returned)
      "Task cancellation did not join the held native call"
  ;;

  (* Failure cleanup only. Successful cases must cancel the real Task while its
     authorization is still pending, without completing this promise. *)
  let release t =
    if (not t.finished) && not (Eio.Promise.is_resolved t.release)
    then Eio.Promise.resolve t.signal_release ()
  ;;
end

let qualify_unix_task_saturation ~sw ~daemon ~current_hold ~holds env fixture =
  let raw =
    Support.Unix_driver.connect_raw
      ~sw
      ~env
      ~socket_path:(Support.Config_fixture.unix_socket fixture)
      ~max_response_bytes:(2 * 1024 * 1024)
  in
  let observer = unix_client ~sw env fixture in
  Exn.protect
    ~finally:(fun () ->
      List.iter holds ~f:Native_hold.release;
      observer.close ();
      Support.Unix_driver.close_raw raw)
    ~f:(fun () ->
      let clock = Eio.Stdenv.clock env in
      let client =
        { next_id = 1
        ; send =
            (fun request ->
              Support.Unix_driver.send_line
                raw
                (P.Envelope.to_json request |> Jsonaf.to_string);
              read_unix_response raw clock 4)
        ; close = Fn.id
        }
      in
      let initialized = initialize_response ~repl:true client in
      require
        (List.mem initialized.enabled_features R.Features.repl ~equal:String.equal)
        "saturation connection did not select REPL";
      ignore (initialize_response ~repl:true observer : P.Initialize.Response.t);
      let limits =
        match
          call_repl
            client
            ~limits:(Support.Developer_fixture.repl_wire_limits ())
            R.Request.Capabilities
        with
        | R.Response.Capabilities caps -> R.Capabilities.limits caps
        | _ -> fail "saturation capabilities changed method"
      in
      let expected, selector = open_and_analyze client in
      let route =
        D.Route.document_script ~limits:(R.Limits.request limits) ~expected ~selector
        |> ok
      in
      let key name = P.Idempotency_key.of_string name |> ok in
      let created =
        let request =
          R.Create.create
            ~limits
            ~run_id:(P.Id.Developer_run.create ())
            ~key:(key "saturation-create")
            ~route
            ~target:One_off_v1
            ~prompt:(S.Catalog_identity.prompt_definition "smoke")
            ~workspace:(S.Catalog_identity.workspace_definition "physical")
            ~requested_execution:None
            ~requested_max_output_bytes:None
          |> ok
        in
        match call_repl client ~limits (R.Request.Create request) with
        | R.Response.Create view -> view
        | _ -> fail "saturation Create changed method"
      in
      let submission_key = key "saturation-task-expression" in
      let request =
        R.Submit.create
          ~limits
          ~instance:(R.View.instance created)
          ~epoch:(R.View.epoch created)
          ~expected_revision:(R.View.revision created)
          ~key:submission_key
          ~source:
            {|Tool.call("read_file", `Object([{key="file"; value=`String("sample.txt")}]))|}
        |> ok
      in
      let envelope id request =
        P.Envelope.request
          ~id
          ~method_:(R.Request.method_name request)
          ~params:(R.Request.params request)
          ()
      in
      let id number =
        P.Envelope.Request_id.of_json (`Number (Int.to_string number)) |> ok
      in
      let schedule request =
        S.Dispatcher.dispatch_schedule
          (S.Daemon.dispatcher daemon)
          (envelope (id 90) request)
      in
      let submission_lookup =
        R.Lookup.create ~instance:(R.View.instance created) ~key:submission_key
      in
      (* The real transport proof below uses a Task held at a supported native
         boundary. Keep the production Submit/Cancel classification contract
         explicit, alongside the configured actual statement-cancellation smoke. *)
      (match
         ( schedule (R.Request.Submit request)
         , schedule (R.Request.Cancel_submission submission_lookup) )
       with
       | S.Dispatcher.Repl_wait, S.Dispatcher.Repl_cancel -> ()
       | _ -> fail "Submit and Cancel_submission lost their transport scheduling classes");
      let submitted =
        match call_repl client ~limits (R.Request.Submit request) with
        | R.Response.Submit value -> value
        | _ -> fail "saturation Task expression changed method"
      in
      let reference =
        match R.Submission.task_references submitted with
        | [ reference ] -> reference
        | _ -> fail "saturation Submit did not retain one native Task"
      in
      let require_running lookup started hold =
        (match call_repl observer ~limits (R.Request.Task_observe lookup) with
         | R.Response.Task_observe (Some observed) ->
           require
             (P.Developer_command_id.equal
                (R.Task_view.command_id started)
                (R.Task_view.command_id observed))
             "held Task observation changed command identity";
           (match R.Task_view.phase observed with
            | Execution_started -> ()
            | _ ->
              raise_s
                [%sexp
                  "held Task was not executing", (R.Task_view.to_json observed : Jsonaf.t)])
         | _ -> fail "held Task was not observable");
        require
          (not hold.Native_hold.finished)
          "held Task ended before explicit cancellation"
      in
      let start_task hold name =
        current_hold := Some hold;
        let task_key = key name in
        let lookup = D.Lookup.create ~run_id:(R.View.run_id created) ~key:task_key in
        let request =
          R.Task_submit.create
            ~limits
            ~key:task_key
            ~reference
            ~requested_execution:None
            ~requested_max_output_bytes:None
            ~fresh_epoch:(Some (R.View.instance created, R.View.epoch created))
          |> ok
        in
        let started =
          match call_repl client ~limits (R.Request.Task_submit request) with
          | R.Response.Task_submit value -> value
          | _ -> fail "saturation Task_submit changed method"
        in
        Eio.Time.with_timeout_exn clock 5. (fun () ->
          Eio.Promise.await hold.Native_hold.entered);
        require_running lookup started hold;
        lookup, started
      in
      let send_unread number request =
        let id = id number in
        Support.Unix_driver.send_line
          raw
          (envelope id request |> P.Envelope.to_json |> Jsonaf.to_string);
        id
      in
      let require_cancelled started terminal hold =
        require
          (P.Developer_command_id.equal
             (R.Task_view.command_id started)
             (R.Task_view.command_id terminal))
          "saturated control changed Task command identity";
        (match R.Task_view.phase terminal with
         | Terminal { execution_started = true; outcome = Cancelled } -> ()
         | _ ->
           raise_s
             [%sexp
               "saturated Task did not finish cancelled"
             , (R.Task_view.to_json terminal : Jsonaf.t)]);
        Native_hold.require_cancelled hold
      in
      let first, second =
        match holds with
        | [ first; second ] -> first, second
        | _ -> fail "invalid saturation hold count"
      in
      let lookup, started = start_task first "saturation-held-task" in
      (match
         ( schedule (R.Request.Task_await_terminal lookup)
         , schedule (R.Request.Task_cancel_and_wait lookup) )
       with
       | S.Dispatcher.Repl_wait, S.Dispatcher.Repl_cancel -> ()
       | _ -> fail "Task wait and cancellation lost their transport scheduling classes");
      let await_id = send_unread 100 (R.Request.Task_await_terminal lookup) in
      let ping_id = id 101 in
      Support.Unix_driver.send_line
        raw
        (P.Envelope.request ~id:ping_id ~method_:"protocol.ping" ~params:(`Object []) ()
         |> P.Envelope.to_json
         |> Jsonaf.to_string);
      let cancel_id = send_unread 102 (R.Request.Task_cancel_and_wait lookup) in
      let responses : P.Envelope.response list ref = ref [] in
      let rec collect remaining =
        if remaining > 0
        then (
          match Support.Unix_driver.read_envelope raw ~clock ~timeout_seconds:5. with
          | Support.Unix_driver.Envelope (P.Envelope.Response response) ->
            responses := response :: !responses;
            collect (remaining - 1)
          | Support.Unix_driver.Envelope (P.Envelope.Notification _) -> collect remaining
          | ( Support.Unix_driver.Envelope (P.Envelope.Request _)
            | Support.Unix_driver.End_of_file
            | Support.Unix_driver.Timeout
            | Support.Unix_driver.Invalid_response _ ) as observed ->
            raise_s
              [%sexp
                "saturation response was unavailable"
              , (observed : Support.Unix_driver.raw_read)
              , (List.map !responses ~f:(fun response ->
                   P.Envelope.to_json (P.Envelope.Response response))
                 : Jsonaf.t list)])
      in
      collect 3;
      let outcome id =
        List.find_exn !responses ~f:(fun (response : P.Envelope.response) ->
          P.Envelope.Request_id.compare response.id id = 0)
        |> fun (response : P.Envelope.response) -> response.outcome
      in
      (match outcome ping_id with
       | Error { code = P.Error.Resource_limit; _ } -> ()
       | _ -> fail "busy ordinary request was not rejected with Resource_limit");
      let decode id request =
        outcome id
        |> ok
        |> R.Response.of_method_and_json ~limits ~method_:(R.Request.method_name request)
        |> ok
      in
      let awaited =
        match decode await_id (R.Request.Task_await_terminal lookup) with
        | R.Response.Task_await_terminal value -> value
        | _ -> fail "held Task await changed method"
      in
      let cancelled =
        match decode cancel_id (R.Request.Task_cancel_and_wait lookup) with
        | R.Response.Task_cancel_and_wait value -> value
        | _ -> fail "held Task cancellation changed method"
      in
      require_cancelled started awaited first;
      require_cancelled started cancelled first;
      let eof_lookup, eof_started = start_task second "saturation-held-eof-task" in
      let eof_id = send_unread 103 (R.Request.Task_await_terminal eof_lookup) in
      (* The held Task cannot finish between the busy-lane probe and input EOF.
         Disconnect cancels the transport waiter, not the daemon-owned Task. *)
      let eof_ping_id = id 104 in
      Support.Unix_driver.send_line
        raw
        (P.Envelope.request
           ~id:eof_ping_id
           ~method_:"protocol.ping"
           ~params:(`Object [])
           ()
         |> P.Envelope.to_json
         |> Jsonaf.to_string);
      (match read_unix_response raw clock 4 with
       | P.Envelope.Response { id; outcome = Error { code = P.Error.Resource_limit; _ } }
         when P.Envelope.Request_id.compare id eof_ping_id = 0 -> ()
       | _ -> fail "EOF Task waiter did not occupy the ordinary lane");
      Support.Unix_driver.shutdown_write raw;
      Eio.Time.with_timeout_exn clock 5. (fun () ->
        let rec await_eof () =
          match Support.Unix_driver.read_envelope raw ~clock ~timeout_seconds:5. with
          | Support.Unix_driver.End_of_file -> ()
          | Support.Unix_driver.Envelope (P.Envelope.Response response)
            when P.Envelope.Request_id.compare response.id eof_id = 0 ->
            fail "held Task waiter published a response after input EOF"
          | Support.Unix_driver.Envelope
              (P.Envelope.Response _ | P.Envelope.Notification _) -> await_eof ()
          | Support.Unix_driver.Envelope (P.Envelope.Request _)
          | Support.Unix_driver.Timeout | Support.Unix_driver.Invalid_response _ ->
            fail "held Task waiter EOF did not close cleanly"
        in
        await_eof ());
      require_running eof_lookup eof_started second;
      match call_repl observer ~limits (R.Request.Task_cancel_and_wait eof_lookup) with
      | R.Response.Task_cancel_and_wait terminal ->
        require_cancelled eof_started terminal second
      | _ -> fail "EOF Task cleanup changed method")
;;

let absolute env path =
  if Filename.is_absolute path
  then path
  else Filename.concat (Eio.Path.native_exn (Eio.Stdenv.cwd env)) path
;;

let stdio_executable env =
  let default =
    Filename.concat
      (Eio.Path.native_exn (Eio.Stdenv.cwd env))
      "_build/default/bin/ochat_agent_stdio.exe"
  in
  let path = Sys.getenv "OCHAT_E2E_STDIO_EXE" |> Option.value ~default |> absolute env in
  require
    (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / path))
    "stdio relay is unavailable";
  path
;;

let rec read_stdio_response client remaining =
  if remaining = 0 then fail "too many stdio notifications before response";
  match Support.Stdio_client.next_envelope client ~timeout_seconds:5. |> ok with
  | P.Envelope.Notification _ -> read_stdio_response client (remaining - 1)
  | response -> response
;;

let stdio_client ~sw env fixture =
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
      [ stdio_executable env
      ; "--connect"
      ; "unix://" ^ Support.Config_fixture.unix_socket fixture
      ]
  in
  let client = Support.Stdio_client.create ~process ~clock:(Eio.Stdenv.clock env) in
  { next_id = 1
  ; send =
      (fun request ->
        Support.Stdio_client.send_raw_line
          client
          (P.Envelope.to_json request |> Jsonaf.to_string)
        |> ok;
        read_stdio_response client 4)
  ; close =
      (fun () ->
        Support.Stdio_process.close_stdin process;
        match
          Eio.Time.with_timeout (Eio.Stdenv.clock env) 3. (fun () ->
            Ok (Support.Stdio_process.await process))
        with
        | Ok _ -> ()
        | Error `Timeout ->
          ignore
            (Support.Stdio_process.terminate
               process
               ~clock:(Eio.Stdenv.clock env)
               ~grace_seconds:1.
             : Support.Process_manager.termination))
  }
;;

let start_socket_listener ~sw env fixture daemon =
  let path = Support.Config_fixture.unix_socket fixture in
  Agent_transport_socket.Server.prepare_path ~env ~socket_path:path |> ok;
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Eio.Switch.run (fun listener_sw ->
      Agent_transport_socket.Server.run
        ~sw:listener_sw
        ~net:(Eio.Stdenv.net env)
        ~socket_path:path
        ~backlog:16
        ~dispatcher:(S.Daemon.dispatcher daemon)
        ~close_connection:(S.Daemon.close_connection daemon)
        ~authenticate:(fun flow _ ->
          Agent_transport_socket.Peer_credentials.authenticate_same_user_actor
            ~scopes:Support.Config_fixture.all_scopes
            flow)
        ~max_line_length:(16 * 1024 * 1024)
        ~outgoing_capacity:32
        ~max_attachments:
          S.Daemon.default_options.protocol_limits.max_attachments_per_connection
        ~on_error:raise
        ~on_protocol_error:(fun error ->
          raise_s [%sexp "Developer socket protocol error", (error : P.Error.t)]));
    `Stop_daemon);
  let rec await_socket attempts =
    if attempts = 0 then fail "Developer Unix listener did not become ready";
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
    | `Socket -> ()
    | _ ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
      await_socket (attempts - 1)
  in
  await_socket 250
;;

let qualify_embedded_unix_saturation env =
  Support.Temporary_environment.with_
    ~scenario:"developer-repl-saturation"
    ~env
    (fun environment ->
       let fixture =
         Support.Config_fixture.create
           environment
           ~name:"developer-repl-saturation"
           ~http_port:(reserve_port env)
       in
       Support.Config_fixture.grant_public_all_scopes fixture;
       Eio.Path.save
         ~create:(`Or_truncate 0o600)
         (Support.Temporary_environment.path
            environment
            (Support.Config_fixture.prompt_path fixture))
         "<developer>Native Task saturation fixture.</developer>\n\
          <tool name=\"read_file\"/>";
       Eio.Path.save
         ~create:(`Exclusive 0o600)
         (Support.Temporary_environment.path
            environment
            (Filename.concat
               (Support.Config_fixture.physical_workspace fixture)
               "sample.txt"))
         "saturation fixture";
       let current_hold = ref None in
       let holds = [ Native_hold.create (); Native_hold.create () ] in
       let developer =
         Support.Developer_fixture.developer_configuration
           ~authorize_native:(fun invocation binding ->
             require
               (String.equal
                  (Chat_response.Tool_capability.reference binding).name
                  "read_file")
               "unexpected saturation native binding";
             require
               (P.Invocation.equal_origin
                  (P.Host_invocation.context invocation).origin
                  P.Invocation.Script)
               "saturation native origin changed";
             Native_hold.await (Option.value_exn !current_hold);
             Ok ())
           ()
       in
       let module Config = S.Daemon.Developer_configuration in
       let portable =
         Config.Portable.create
           ~wire_limits:(wire_limits ())
           ~max_connections:4
           ~max_watches_per_connection:4
           ~max_pending_hints_per_connection:8
         |> ok
         |> fun portable ->
         Config.Portable.with_repl_wire
           portable
           (Support.Developer_fixture.repl_wire_limits ())
         |> ok
       in
       let developer =
         Config.with_portable_wire developer portable
         |> fun developer ->
         Config.with_repl developer (Support.Developer_fixture.repl_configuration ())
       in
       let options =
         { (Support.Daemon_host.with_offline_inference S.Daemon.default_options) with
           authoring = Some (Support.Developer_fixture.semantic_authoring_policy env)
         ; developer = Some developer
         }
       in
       Support.Daemon_host.with_ env fixture ~options (fun sw daemon ->
         start_socket_listener ~sw env fixture daemon;
         qualify_unix_task_saturation ~sw ~daemon ~current_hold ~holds env fixture))
;;

let qualify_terminal_remote_profile env fixture =
  let connect expected_server =
    let profile =
      Agent_transport_client.Connection_profile.create
        ~home:None
        ~name:"configured Developer REPL Unix"
        ~endpoint:("unix://" ^ Support.Config_fixture.unix_socket fixture)
        ~expected_server
        ~daemon_credential_file:None
      |> ok
    in
    Chatml_repl_terminal.Client_adapter.with_connection
      ~env
      ~target:(Remote profile)
      ~notification_capacity:16
      ~f:(fun session ->
        let limits =
          Agent_client.Developer_repl_connection.capabilities
            (Chatml_repl_terminal.Client_adapter.client session)
          |> P.Developer_repl_wire.Capabilities.limits
        in
        if P.Developer_repl_wire.Limits.max_source_bytes limits <= 0
        then fail "explicit-feature remote terminal profile did not select REPL";
        let initialized =
          Agent_client.Connection.initialization
            (Chatml_repl_terminal.Client_adapter.connection session)
          |> Option.value_exn
        in
        Ok initialized.server_id)
    |> ok
  in
  let actual_server = connect None in
  let pinned_server = connect (Some actual_server) in
  if not (P.Id.Server.equal actual_server pinned_server)
  then fail "pinned terminal profile changed physical Developer host"
;;

let configured_developer fixture =
  let http_principal =
    Support.Config_fixture.public_principal fixture |> P.Id.Principal.to_string
  in
  let unix_principal =
    Digestif.SHA256.(
      digest_string
        ("unix-peer:v1:"
         ^ Int.to_string Agent_transport_socket.Peer_credentials.(effective_uid ()))
      |> to_hex)
    |> fun digest -> "pri_" ^ digest
  in
  sprintf
    {|((selections
       (((source (Document_source (origin Supplied_document) (script_id s)))
         (target One_off_v1)
         (principals
          (((principal_id %s) (authentication_kind http.static_bearer))
           ((principal_id %s) (authentication_kind unix.peer)))))))
      (capacity
       ((max_runs 128)
        (max_active_commands 4)
        (max_active_agent_calls 8)
        (max_agent_sources 6)
        (max_native_bindings 32)
        (max_retained_tasks 8)
        (max_watches_per_connection 4)
        (max_pending_approvals 4)
        (max_repl_instances 8)
        (max_repl_keys_per_instance 128)
        (max_repl_submissions_per_epoch 64)
        (max_repl_source_bytes 65536)))
      (model Disabled)
      (approvals Disabled)
      (repl Standard_v1)
      (portable true))|}
    http_principal
    unix_principal
;;

let install_configured_developer env fixture =
  let original = Support.Config_fixture.configuration fixture () in
  let anchor = "(unsafe_allow_unauthenticated_remote_http false)))" in
  require (String.is_substring original ~substring:anchor) "missing server config anchor";
  let configured =
    String.substr_replace_first
      original
      ~pattern:anchor
      ~with_:("(developer " ^ configured_developer fixture ^ ")\n  " ^ anchor)
  in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / Support.Config_fixture.config_path fixture)
    configured
;;

let reject_uninstalled_source_and_reopen env environment fixture options f =
  let raw =
    match
      S.Config_parser.load ~env ~path:(Support.Config_fixture.config_path fixture)
    with
    | Ok raw -> raw
    | Error _ -> fail "valid configured fixture did not parse"
  in
  let config =
    match S.Config_validator.validate ~env raw with
    | Ok config -> config
    | Error _ -> fail "valid configured fixture did not validate"
  in
  let developer = Option.value_exn config.server.developer in
  let selected = List.hd_exn developer.selections in
  let selected =
    { selected with
      source =
        S.Developer_startup_configuration.Source.Document_source
          { origin =
              S.Developer_startup_configuration.Source.Host_document
                { root = "uninstalled-root"; member = "draft.chatmd" }
          ; script_id = "s"
          }
    }
  in
  let invalid =
    { config with
      server =
        { config.server with
          developer = Some { developer with selections = [ selected ] }
        }
    }
  in
  let tool_dir = Eio.Path.native_exn (Eio.Stdenv.cwd env) in
  let home = (Support.Temporary_environment.roots environment).home in
  Eio.Switch.run (fun sw ->
    match
      S.Daemon.start
        ~sw
        ~env
        ~config:invalid
        ~tool_dir
        ~home
        ~process_start_identity:None
        ~options
        ()
    with
    | Error { code = P.Error.Invalid_request; _ } -> ()
    | Error error ->
      raise_s [%sexp "invalid startup had another error", (error : P.Error.t)]
    | Ok daemon ->
      ignore (S.Daemon.shutdown daemon : (unit, P.Error.t) result);
      fail "uninstalled Developer source was accepted");
  Support.Daemon_host.with_ env fixture ~options f
;;

let run_configured env =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
    Support.Temporary_environment.with_
      ~scenario:"developer-disabled-startup"
      ~env
      (fun environment ->
         let fixture =
           Support.Config_fixture.create
             environment
             ~name:"developer-disabled-startup"
             ~http_port:(reserve_port env)
         in
         Support.Config_fixture.grant_public_all_scopes fixture;
         let options =
           Support.Daemon_host.with_offline_inference S.Daemon.default_options
         in
         Support.Daemon_host.with_ env fixture ~options (fun sw _daemon ->
           qualify_unconfigured_client (http_client ~sw env fixture)));
    Support.Temporary_environment.with_
      ~scenario:"developer-configured-startup"
      ~env
      (fun environment ->
         let fixture =
           Support.Config_fixture.create
             environment
             ~name:"developer-configured-startup"
             ~http_port:(reserve_port env)
         in
         Support.Config_fixture.grant_public_all_scopes fixture;
         install_configured_developer env fixture;
         let options =
           let options =
             Support.Daemon_host.with_offline_inference S.Daemon.default_options
           in
           { options with
             authoring = Some (Support.Developer_fixture.semantic_authoring_policy env)
           ; inference_policy =
               { options.inference_policy with
                 capture_developer_default_target =
                   (fun () ->
                     failwith "disabled Developer model attempted default target capture")
               }
           }
         in
         reject_uninstalled_source_and_reopen
           env
           environment
           fixture
           options
           (fun sw daemon ->
              qualify_repl_capacity daemon fixture;
              qualify_trace_disabled (http_client ~sw env fixture);
              start_socket_listener ~sw env fixture daemon;
              qualify_client
                ~expected_wire_limits:(configured_wire_limits ())
                ~after_check:(qualify_own_control_without_approval_scope daemon fixture)
                (http_client ~sw env fixture);
              qualify_configured_repl (http_client ~sw env fixture);
              qualify_typed_repl_cancel
                (Agent_transport_http.Client.connect
                   ~sw
                   ~env
                   ~uri:
                     (Uri.of_string
                        (sprintf
                           "http://127.0.0.1:%d"
                           (Support.Config_fixture.http_port fixture)))
                   ~bearer_token:(Some (Support.Config_fixture.public_token fixture))
                   ~notification_capacity:64
                 |> ok);
              qualify_terminal_remote_profile env fixture;
              qualify_unselected_principal
                (http_client
                   ~token:(Support.Config_fixture.admin_token fixture)
                   ~sw
                   env
                   fixture);
              qualify_client
                ~expected_wire_limits:(configured_wire_limits ())
                (unix_client ~sw env fixture);
              qualify_configured_repl (unix_client ~sw env fixture);
              qualify_typed_repl_cancel
                (Agent_transport_socket.Client.connect
                   ~sw
                   ~net:(Eio.Stdenv.net env)
                   ~socket_path:(Support.Config_fixture.unix_socket fixture)
                   ~max_line_length:(16 * 1024 * 1024)
                   ~notification_capacity:64)));
    qualify_embedded_unix_saturation env);
  Eio.Flow.copy_string
    "configured Developer HTTP and Unix passed\n"
    (Eio.Stdenv.stdout env)
;;

let run_embedded env =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 45. (fun () ->
    Support.Temporary_environment.with_
      ~scenario:"developer-wire"
      ~env
      (fun environment ->
         let fixture =
           Support.Config_fixture.create
             environment
             ~name:"developer-wire"
             ~http_port:(reserve_port env)
         in
         Support.Config_fixture.grant_public_all_scopes fixture;
         let developer = Support.Developer_fixture.developer_configuration () in
         let portable =
           S.Daemon.Developer_configuration.Portable.create
             ~wire_limits:(wire_limits ())
             ~max_connections:4
             ~max_watches_per_connection:4
             ~max_pending_hints_per_connection:8
           |> ok
           |> fun portable ->
           S.Daemon.Developer_configuration.Portable.with_trace_wire
             portable
             (trace_wire_limits ())
           |> ok
         in
         let developer =
           S.Daemon.Developer_configuration.with_traces developer (trace_configuration ())
         in
         let options =
           { (Support.Daemon_host.with_offline_inference S.Daemon.default_options) with
             authoring = Some (Support.Developer_fixture.semantic_authoring_policy env)
           ; developer =
               Some
                 (S.Daemon.Developer_configuration.with_portable_wire developer portable)
           }
         in
         Support.Daemon_host.with_ env fixture ~options (fun sw daemon ->
           start_socket_listener ~sw env fixture daemon;
           qualify_client
             ~trace:true
             ~after_check:(fun run_id key ->
               qualify_unselected_trace
                 (http_client
                    ~token:(Support.Config_fixture.admin_token fixture)
                    ~sw
                    env
                    fixture)
                 ~run_id
                 ~key)
             (http_client ~sw env fixture);
           qualify_client ~trace:true (unix_client ~sw env fixture);
           qualify_client (stdio_client ~sw env fixture))));
  Eio.Flow.copy_string "developer wire transports passed\n" (Eio.Stdenv.stdout env)
;;

let run env ~case =
  match case with
  | None -> run_embedded env
  | Some "configured" -> run_configured env
  | Some case -> raise_s [%sexp "unknown Developer wire E2E case", (case : string)]
;;
