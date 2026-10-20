open! Core
module P = Agent_protocol
module W = P.Developer_trace_wire
module Native = Chatml_trace
module Retention = Developer_trace_retention

let ( let* ) result f = Result.bind result ~f
let error code message = P.Error.create code ~message ~retryable:false ()
let stale () = error P.Error.Lease_stale "trace page cursor does not match this recording"

let limited () =
  error P.Error.Resource_limit "one trace event cannot fit the installed page limit"
;;

let projection_error _source =
  error
    P.Error.Resource_limit
    "retained trace data cannot be represented by the installed wire contract"
;;

let project_location site =
  let span = Chatml_source.Location.span site in
  W.Location.create
    ~source:(Chatml_source.Location.source site)
    ~digest:(Chatml_source.Location.source_digest site)
    ~start_byte:span.left.offset
    ~end_byte:span.right.offset
    ~start_line:span.left.line
    ~start_column:span.left.column
    ~end_line:span.right.line
    ~end_column:span.right.column
  |> Result.map_error ~f:projection_error
;;

let project_phase = function
  | Native.Event.Phase.Preparation -> W.Event.Preparation
  | Compilation -> Compilation
  | Dependency_initialization -> Dependency_initialization
  | Entry_initialization -> Entry_initialization
  | Evaluation -> Evaluation
  | Task -> Task
  | Host_wait -> Host_wait
;;

let project_operation = function
  | Native.Event.Operation.Tool_call -> W.Event.Tool_call
  | Tool_spawn -> Tool_spawn
  | Job_start -> Job_start
  | Model_call -> Model_call
  | Model_spawn -> Model_spawn
  | Process_run -> Process_run
  | Schedule -> Schedule
  | Job_read -> Job_read
  | Local_transaction -> Local_transaction
  | Other -> Other
;;

let project_failure = function
  | Native.Event.Failure.Parse -> W.Event.Parse
  | Typecheck -> Typecheck
  | Link -> Link
  | Runtime -> Runtime
  | Host_operation -> Host_operation
  | Permission -> Permission
  | Resource_limit -> Resource_limit
  | Other -> Other
;;

let project_kind = function
  | Native.Event.Kind.Phase_started phase -> W.Event.Phase_started (project_phase phase)
  | Phase_finished phase -> Phase_finished (project_phase phase)
  | Statement_entered -> Statement_entered
  | Call_entered -> Call_entered
  | Effect_attempted operation -> Effect_attempted (project_operation operation)
  | Effect_host_returned operation -> Effect_host_returned (project_operation operation)
  | Effect_host_failed operation -> Effect_host_failed (project_operation operation)
  | Host_wait_entered -> Host_wait_entered
  | Host_wait_resumed -> Host_wait_resumed
  | Job_admitted -> Job_admitted
  | Job_terminal -> Job_terminal
  | Recovered_failure failure -> Recovered_failure (project_failure failure)
  | Unrecovered_failure failure -> Unrecovered_failure (project_failure failure)
;;

let project_event event =
  let* site =
    match Native.Event.site event with
    | None -> Ok None
    | Some site -> Result.map (project_location site) ~f:Option.some
  in
  W.Event.create
    ~sequence:(Native.Event.sequence event)
    ~kind:(project_kind (Native.Event.kind event))
    ~site
    ~correlation:
      (Option.map
         (Native.Event.correlation event)
         ~f:Native.Event.Correlation.to_opaque_string)
  |> Result.map_error ~f:projection_error
;;

let project_disposition = function
  | Native.Terminal.Disposition.Succeeded -> W.Terminal.Succeeded
  | Failed failure -> Failed (project_failure failure)
  | Cancelled -> Cancelled
  | Denied -> Denied
  | Interrupted -> Interrupted
;;

let project_terminal terminal =
  W.Terminal.create
    ~receipt_version:(Native.Terminal.receipt_version terminal)
    ~disposition:(project_disposition (Native.Terminal.disposition terminal))
  |> Result.map_error ~f:projection_error
;;

let project_header header =
  let sources =
    Native.Header.source_inventory header
    |> List.map ~f:(fun source ->
      W.Header.Source_ref.create
        ~id:(Native.Header.Source_ref.id source)
        ~digest:(Native.Header.Source_ref.digest source)
        ~byte_length:(Native.Header.Source_ref.byte_length source)
      |> Result.map_error ~f:projection_error)
    |> Result.all
  in
  let* sources = sources in
  let admission =
    match Native.Header.admission header with
    | Native.Header.Admission.Submitted_capture -> W.Header.Submitted_capture
    | Prepared_graph -> Prepared_graph
  in
  W.Header.create
    ~execution_id:
      (Native.Header.execution_id header |> Native.Execution_id.to_opaque_string)
    ~admission
    ~submission_fingerprint:(Native.Header.submission_fingerprint header)
    ~selection_fingerprint:(Native.Header.selection_fingerprint header)
    ~graph_fingerprint:(Native.Header.graph_fingerprint header)
    ~contract_fingerprint:(Native.Header.contract_fingerprint header)
    ~environment_fingerprint:(Native.Header.environment_fingerprint header)
    ~sources
  |> Result.map_error ~f:projection_error
;;

let cursor_matches_snapshot cursor ~locator ~header ~snapshot =
  W.Locator.equal (W.Cursor.locator cursor) locator
  && String.equal (W.Cursor.execution_id cursor) (W.Header.execution_id header)
  && Chatml_source.Digest.equal
       (W.Cursor.submission_fingerprint cursor)
       (W.Header.submission_fingerprint header)
  && Chatml_source.Digest.equal
       (W.Cursor.selection_fingerprint cursor)
       (W.Header.selection_fingerprint header)
  && Int64.(
       W.Cursor.after_sequence cursor <= Native.Recorder.Snapshot.last_sequence snapshot)
  && List.exists (Native.Recorder.Snapshot.prefix snapshot) ~f:(fun event ->
    Int64.equal (Native.Event.sequence event) (W.Cursor.after_sequence cursor))
;;

let project_retained ~limits ~request snapshot =
  let locator = W.Page_request.locator request in
  let after = W.Page_request.after request in
  let* header = project_header (Native.Recorder.Snapshot.header snapshot) in
  let cursor_valid =
    Option.for_all after ~f:(fun cursor ->
      cursor_matches_snapshot cursor ~locator ~header ~snapshot)
  in
  if not cursor_valid
  then Error (stale ())
  else (
    let after_sequence = Option.value_map after ~default:0L ~f:W.Cursor.after_sequence in
    let remaining =
      Native.Recorder.Snapshot.prefix snapshot
      |> List.filter ~f:(fun event ->
        Int64.(Native.Event.sequence event > after_sequence))
    in
    let max_count = Int.min (List.length remaining) (W.Page_request.limit request) in
    let rec project_prefix reversed = function
      | [] -> Ok (List.rev reversed)
      | native :: rest ->
        (match project_event native with
         | Ok event -> project_prefix (event :: reversed) rest
         | Error error when List.is_empty reversed -> Error error
         | Error _ -> Ok (List.rev reversed))
    in
    let* events = project_prefix [] (List.take remaining max_count) in
    let max_count = List.length events in
    let last_sequence = Native.Recorder.Snapshot.last_sequence snapshot in
    let omitted_after_prefix =
      Option.map (Native.Recorder.Snapshot.omitted_after_prefix snapshot) ~f:(fun drop ->
        W.Page.
          { at_least = Native.Recorder.Drop_count.at_least drop
          ; overflowed = Native.Recorder.Drop_count.overflowed drop
          })
    in
    let* last_retained_site_context =
      match Native.Recorder.Snapshot.last_retained_site_context snapshot with
      | None -> Ok None
      | Some site -> Result.map (project_location site) ~f:Option.some
    in
    let* state =
      match Native.Recorder.Snapshot.state snapshot with
      | Incomplete -> Ok W.Page.Incomplete
      | Complete terminal ->
        Result.map (project_terminal terminal) ~f:(fun value -> W.Page.Complete value)
    in
    let make_page count =
      let selected = List.take events count in
      let* resume =
        match List.last selected with
        | None -> Ok after
        | Some event ->
          W.Cursor.create
            ~limits
            ~locator
            ~execution_id:(W.Header.execution_id header)
            ~submission_fingerprint:(W.Header.submission_fingerprint header)
            ~selection_fingerprint:(W.Header.selection_fingerprint header)
            ~after_sequence:(W.Event.sequence event)
          |> Result.map ~f:Option.some
      in
      let next = if count < List.length remaining then resume else None in
      let* page =
        W.Page.create
          ~limits
          ~locator
          ~after
          ~header
          ~events:selected
          ~last_sequence
          ~omitted_after_prefix
          ~last_retained_site_context
          ~omitted_site_count:(Native.Recorder.Snapshot.omitted_site_count snapshot)
          ~state
          ~resume
          ~next
      in
      let* _ = W.Response.to_json (Page (Retained page)) ~limits in
      Ok page
    in
    if max_count = 0
    then
      if List.is_empty remaining
      then Result.map (make_page 0) ~f:(fun page -> W.Availability.Retained page)
      else Error (limited ())
    else (
      match make_page max_count with
      | Ok page -> Ok (W.Availability.Retained page)
      | Error { code = P.Error.Resource_limit; _ } ->
        let rec search low high best =
          if low > high
          then (
            match best with
            | Some page -> Ok (W.Availability.Retained page)
            | None -> Error (limited ()))
          else (
            let middle = low + ((high - low) / 2) in
            match make_page middle with
            | Ok page -> search (middle + 1) high (Some page)
            | Error { code = P.Error.Resource_limit; _ } -> search low (middle - 1) best
            | Error error -> Error error)
        in
        search 1 (max_count - 1) None
      | Error error -> Error error))
;;

let project ~limits ~request ~availability =
  match availability with
  | (Retention.Availability.Unavailable | Expired)
    when Option.is_some (W.Page_request.after request) -> Error (stale ())
  | Unavailable ->
    let availability = W.Availability.Unavailable in
    let* _ = W.Response.to_json (Page availability) ~limits in
    Ok availability
  | Expired ->
    let availability = W.Availability.Expired in
    let* _ = W.Response.to_json (Page availability) ~limits in
    Ok availability
  | Retained snapshot -> project_retained ~limits ~request snapshot
;;

let%expect_test
    "retained trace pages preserve historical sites, progress and bounded resume"
  =
  let must = function
    | Ok value -> value
    | Error error -> raise_s [%sexp (error : P.Error.t)]
  in
  let must_fixture = function
    | Ok value -> value
    | Error _ -> failwith "invalid trace pager fixture bound"
  in
  let source ?(namespace = "pager") name contents =
    let id =
      Chatml_source.Id.create ~package_namespace:namespace ~relative_path:name
      |> Or_error.ok_exn
    in
    Chatml_source.create
      id
      ~contents
      ~expected_digest:None
      ~max_bytes:None
      ~checkpoint:Fn.id
    |> must_fixture
  in
  let current = source "current.chatml" "Task.pure(1)" in
  let older = source ~namespace:"pager-\255\000" "older.chatml" "Task.pure(0)" in
  let older_location =
    Chatml_source.locate older ~start_byte:0 ~end_byte:4 ~checkpoint:Fn.id |> must_fixture
  in
  let older_site =
    Chatml_diagnostic.Site.create
      older
      ~span:(Chatml_source.Location.span older_location)
      ~checkpoint:Fn.id
    |> Or_error.ok_exn
  in
  let header =
    Native.Header.of_submitted_capture
      ~execution_id:
        (Native.Execution_id.of_opaque_string "pager-execution" |> Or_error.ok_exn)
      ~submission_fingerprint:(Chatml_source.digest current)
      ~sources:[ current ]
      ~selection_fingerprint:(Chatml_source.digest current)
      ~max_inventory_entries:1
      ~max_header_bytes:4_096
      ~checkpoint:Fn.id
    |> Or_error.ok_exn
  in
  let recorder =
    Native.Recorder.create
      header
      ~mode:Native.Recorder.Mode.Full
      ~limits:
        (Native.Recorder.Limits.create
           ~max_events:8
           ~max_normal_bytes:16_384
           ~max_terminal_bytes:128
         |> Or_error.ok_exn)
  in
  Eio_main.run (fun _ ->
    Native.Scope.within (Some recorder) (fun () ->
      Native.Scope.observe_site
        ~kind:(Native.Event.Kind.Effect_attempted Native.Event.Operation.Tool_call)
        ~site:(Some older_site)
        ~correlation:
          (Some
             (Native.Event.Correlation.of_host_token "pager-correlation"
              |> Or_error.ok_exn));
      Native.Scope.observe_unlocated
        ~kind:(Native.Event.Kind.Recovered_failure Native.Event.Failure.Permission)
        ~correlation:None));
  let snapshot = Native.Recorder.snapshot recorder in
  let json_limits max_bytes =
    Document_schema.Limits.create
      ~max_bytes
      ~max_depth:32
      ~max_fields:1_024
      ~max_nodes:2_048
    |> must_fixture
  in
  let request_bound = json_limits 16_384 in
  let page_bound = json_limits 65_536 in
  let response_bound = json_limits 65_536 in
  let limits =
    W.Limits.create
      ~request:request_bound
      ~page:page_bound
      ~response:response_bound
      ~max_page_items:1
      ~max_cursor_bytes:1_024
    |> must
  in
  let generator = P.Id.Generator.create ~bytes:(fun length -> String.make length 'a') in
  let locator =
    W.Locator.Command
      (P.Developer_common.Lookup.create
         ~run_id:(P.Id.Developer_run.create_with generator)
         ~key:(P.Idempotency_key.of_string "pager-key" |> must))
  in
  let request after = W.Page_request.create ~limits ~locator ~after ~limit:1 |> must in
  let retained = function
    | W.Availability.Retained page -> page
    | Unavailable | Expired -> failwith "authenticated recording disappeared"
  in
  let first =
    project ~limits ~request:(request None) ~availability:(Retained snapshot)
    |> must
    |> retained
  in
  let first_event = List.hd_exn (W.Page.events first) in
  let first_site = W.Event.site first_event |> Option.value_exn in
  if
    not
      (Chatml_source.Id.equal (W.Location.source first_site) (Chatml_source.id older)
       && Chatml_source.Digest.equal
            (W.Location.digest first_site)
            (Chatml_source.digest older)
       && Int.equal (W.Location.start_byte first_site) 0
       && Int.equal (W.Location.end_byte first_site) 4
       && Option.value_map
            (W.Event.correlation first_event)
            ~default:false
            ~f:(String.equal "pager-correlation"))
  then failwith "historical closure site was lost";
  (match W.Event.kind first_event with
   | W.Event.Effect_attempted W.Event.Tool_call -> ()
   | _ -> failwith "native effect kind was not preserved");
  if
    List.exists
      (W.Header.sources (W.Page.header first))
      ~f:(fun source ->
        Chatml_source.Id.equal (W.Header.Source_ref.id source) (Chatml_source.id older))
  then failwith "older closure source was invented in current Header";
  let after_first = W.Page.next first |> Option.value_exn in
  let second =
    project
      ~limits
      ~request:(request (Some after_first))
      ~availability:(Retained snapshot)
    |> must
    |> retained
  in
  if
    not
      (Int64.equal (W.Event.sequence (List.hd_exn (W.Page.events second))) 2L
       && Option.is_none (W.Page.next second))
  then failwith "second page did not advance exactly once";
  let after_second = W.Page.resume second |> Option.value_exn in
  let empty =
    project
      ~limits
      ~request:(request (Some after_second))
      ~availability:(Retained snapshot)
    |> must
    |> retained
  in
  if
    not
      (List.is_empty (W.Page.events empty)
       && Option.is_none (W.Page.next empty)
       && Option.value_map (W.Page.resume empty) ~default:false ~f:(fun cursor ->
         Int64.equal (W.Cursor.after_sequence cursor) 2L))
  then failwith "empty Incomplete poll rewound or repeated a next cursor";
  let foreign =
    W.Cursor.create
      ~limits
      ~locator
      ~execution_id:"another-execution"
      ~submission_fingerprint:(Chatml_source.digest current)
      ~selection_fingerprint:(Chatml_source.digest current)
      ~after_sequence:1L
    |> must
  in
  (match
     project ~limits ~request:(request (Some foreign)) ~availability:(Retained snapshot)
   with
   | Error { code = P.Error.Lease_stale; _ } -> ()
   | Ok _ | Error _ -> failwith "foreign cursor was admitted");
  let first_json =
    W.Response.to_json (W.Response.Page (W.Availability.Retained first)) ~limits |> must
  in
  let first_bytes =
    Document_schema.Json.validate_and_measure ~limits:response_bound first_json
    |> must_fixture
  in
  let tight_limits =
    W.Limits.create
      ~request:request_bound
      ~page:page_bound
      ~response:(json_limits (first_bytes - 1))
      ~max_page_items:1
      ~max_cursor_bytes:1_024
    |> must
  in
  (match
     project
       ~limits:tight_limits
       ~request:(request None)
       ~availability:(Retained snapshot)
   with
   | Error { code = P.Error.Resource_limit; _ } -> ()
   | Ok _ | Error _ -> failwith "one unrepresentable event repeated an empty page");
  let three =
    Native.Recorder.create
      header
      ~mode:Native.Recorder.Mode.Full
      ~limits:
        (Native.Recorder.Limits.create
           ~max_events:8
           ~max_normal_bytes:16_384
           ~max_terminal_bytes:128
         |> Or_error.ok_exn)
  in
  Eio_main.run (fun _ ->
    Native.Scope.within (Some three) (fun () ->
      List.iter
        [ Native.Event.Kind.Phase_started Native.Event.Phase.Evaluation
        ; Statement_entered
        ; Phase_finished Evaluation
        ]
        ~f:(fun kind -> Native.Scope.observe_unlocated ~kind ~correlation:None)));
  let three_snapshot = Native.Recorder.snapshot three in
  let two_limits response =
    W.Limits.create
      ~request:request_bound
      ~page:page_bound
      ~response
      ~max_page_items:2
      ~max_cursor_bytes:1_024
    |> must
  in
  let wide_two = two_limits response_bound in
  let one_request =
    W.Page_request.create ~limits:wide_two ~locator ~after:None ~limit:1 |> must
  in
  let one_page =
    project ~limits:wide_two ~request:one_request ~availability:(Retained three_snapshot)
    |> must
    |> retained
  in
  let one_bytes =
    W.Response.to_json
      (W.Response.Page (W.Availability.Retained one_page))
      ~limits:wide_two
    |> must
    |> Document_schema.Json.validate_and_measure ~limits:response_bound
    |> must_fixture
  in
  let bounded_two = two_limits (json_limits one_bytes) in
  let two_request =
    W.Page_request.create ~limits:bounded_two ~locator ~after:None ~limit:2 |> must
  in
  let contracted =
    project
      ~limits:bounded_two
      ~request:two_request
      ~availability:(Retained three_snapshot)
    |> must
    |> retained
  in
  if
    not
      (Int.equal (List.length (W.Page.events contracted)) 1
       && Option.is_some (W.Page.next contracted))
  then failwith "bounded page did not choose an advancing shorter prefix";
  let historical_site source =
    let location =
      Chatml_source.locate source ~start_byte:0 ~end_byte:4 ~checkpoint:Fn.id
      |> must_fixture
    in
    Chatml_diagnostic.Site.create
      source
      ~span:(Chatml_source.Location.span location)
      ~checkpoint:Fn.id
    |> Or_error.ok_exn
  in
  let near_max =
    source
      ~namespace:(String.make 1_024 '\255')
      (String.make 1_017 '"' ^ ".chatml")
      "Task.pure(0)"
  in
  let overlong =
    source ~namespace:(String.make 1_025 '\255') "old.chatml" "Task.pure(0)"
  in
  let historical =
    Native.Recorder.create
      header
      ~mode:Native.Recorder.Mode.Full
      ~limits:
        (Native.Recorder.Limits.create
           ~max_events:8
           ~max_normal_bytes:16_384
           ~max_terminal_bytes:128
         |> Or_error.ok_exn)
  in
  Eio_main.run (fun _ ->
    Native.Scope.within (Some historical) (fun () ->
      List.iter
        [ historical_site near_max; historical_site overlong; older_site ]
        ~f:(fun site ->
          Native.Scope.observe_site
            ~kind:(Native.Event.Kind.Effect_attempted Native.Event.Operation.Tool_call)
            ~site:(Some site)
            ~correlation:None)));
  let historical_snapshot = Native.Recorder.snapshot historical in
  let before_incompatible =
    project
      ~limits:wide_two
      ~request:
        (W.Page_request.create ~limits:wide_two ~locator ~after:None ~limit:2 |> must)
      ~availability:(Retained historical_snapshot)
    |> must
    |> retained
  in
  if
    not
      (Int.equal (List.length (W.Page.events before_incompatible)) 1
       && Option.is_some (W.Page.next before_incompatible)
       && Chatml_source.Id.equal
            (W.Event.site (List.hd_exn (W.Page.events before_incompatible))
             |> Option.value_exn
             |> W.Location.source)
            (Chatml_source.id near_max))
  then failwith "representable historical prefix did not advance";
  (match
     project
       ~limits:wide_two
       ~request:
         (W.Page_request.create
            ~limits:wide_two
            ~locator
            ~after:(W.Page.next before_incompatible)
            ~limit:2
          |> must)
       ~availability:(Retained historical_snapshot)
   with
   | Error { code = P.Error.Resource_limit; _ } -> ()
   | Ok _ | Error _ -> failwith "unrepresentable native site was not bounded");
  let terminal =
    Native.Terminal.create
      ~receipt_version:1
      ~disposition:(Failed Native.Event.Failure.Permission)
    |> Or_error.ok_exn
  in
  Native.Recorder.finish recorder terminal |> Or_error.ok_exn;
  let complete =
    project
      ~limits
      ~request:(request (Some after_second))
      ~availability:(Retained (Native.Recorder.snapshot recorder))
    |> must
    |> retained
  in
  (match W.Page.state complete with
   | W.Page.Complete terminal ->
     (match W.Terminal.disposition terminal with
      | W.Terminal.Failed W.Event.Permission -> ()
      | Succeeded | Failed _ | Cancelled | Denied | Interrupted ->
        failwith "terminal disposition was not projected")
   | Incomplete -> failwith "terminal recording stayed incomplete");
  print_endline "historical site, cursor progress, bounded prefix and terminal: pass";
  [%expect {| historical site, cursor progress, bounded prefix and terminal: pass |}]
;;
