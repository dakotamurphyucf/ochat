open! Core
open Agent_protocol.Error
module P = Agent_protocol
module W = P.Authoring.Wire
module Service = Authoring_context_service
module Host_policy = Authoring_host_policy

type t =
  { policy : Host_policy.t
  ; service : Service.t
  ; semantic : Service.Semantic.t option
  ; assistance : Service.Assistance.t option
  ; source : Authoring_source_host.t option
  ; upload : Authoring_upload_host.t option
  ; mutable closed : bool
  }

type constructing =
  { mutable source : Authoring_source_host.t option
  ; mutable upload_store : Agent_store.Authoring_upload_store.t option
  ; mutable upload : Authoring_upload_host.t option
  ; mutable service : Service.t option
  ; mutable transferred : bool
  }

let error code message ~retryable = P.Error.create code ~message ~retryable ()

let uninstalled () =
  error Method_not_found "authoring method is not installed" ~retryable:false
;;

let upload_store_error = function
  | Agent_store.Authoring_upload_store.Error.Limit_exceeded ->
    error Resource_limit "authoring upload storage limit" ~retryable:false
  | Busy -> error Conflict "authoring upload storage is busy" ~retryable:true
  | Closed ->
    error Server_shutting_down "authoring upload storage is closed" ~retryable:true
  | Storage failure -> Agent_store.Store_error.to_protocol_error failure
  | Invalid_manifest
  | Idempotency_conflict
  | Invalid_offset
  | Incomplete
  | Sealed
  | Expired
  | Missing_or_foreign ->
    error Persistence_error "authoring upload storage could not open" ~retryable:false
;;

let close_children (t : t) =
  let open Result.Let_syntax in
  t.closed <- true;
  let%bind () =
    Service.close_and_wait t.service |> Result.map_error ~f:Service.Error.to_protocol
  in
  let%bind () =
    match t.upload with
    | None -> Ok ()
    | Some upload ->
      Authoring_upload_host.close_and_wait upload
      |> Result.map_error ~f:(fun failure ->
        Service.Error.to_protocol (Service.Error.Upload_failure failure))
  in
  Option.iter t.source ~f:Authoring_source_host.close_and_wait;
  Ok ()
;;

let close_untransferred (partial : constructing) =
  let open Result.Let_syntax in
  let%bind () =
    match partial.service with
    | None -> Ok ()
    | Some service ->
      Service.close_and_wait service
      |> Result.map_error ~f:Service.Error.to_protocol
      |> Result.map ~f:(fun () -> partial.service <- None)
  in
  let%bind () =
    match partial.upload with
    | Some host ->
      Authoring_upload_host.close_and_wait host
      |> Result.map_error ~f:(fun failure ->
        Service.Error.to_protocol (Service.Error.Upload_failure failure))
      |> Result.map ~f:(fun () ->
        partial.upload <- None;
        partial.upload_store <- None)
    | None ->
      (match partial.upload_store with
       | None -> Ok ()
       | Some store ->
         Agent_store.Authoring_upload_store.close_and_wait store
         |> Result.map_error ~f:upload_store_error
         |> Result.map ~f:(fun () -> partial.upload_store <- None))
  in
  Option.iter partial.source ~f:Authoring_source_host.close_and_wait;
  partial.source <- None;
  Ok ()
;;

let create ~env ~sw ~clock ~project_sources ~store ~policy ~initialize_limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind () = Host_policy.validate_initialize policy initialize_limits in
  let%bind () =
    match policy.configured_projects with
    | None -> Ok ()
    | Some bound ->
      if Authoring_configured_project_policy.issued_by bound project_sources
      then Ok ()
      else Error (P.Error.invalid_request "authoring source owner mismatch")
  in
  Eio.Switch.check sw;
  let partial =
    { source = None
    ; upload_store = None
    ; upload = None
    ; service = None
    ; transferred = false
    }
  in
  ignore
    (Eio.Switch.on_release_cancellable sw (fun () ->
       if not partial.transferred
       then (
         match close_untransferred partial with
         | Ok () -> ()
         | Error failure ->
           raise_s [%sexp "authoring partial-owner cleanup failed", (failure : P.Error.t)]))
     : Eio.Switch.hook);
  let construct () =
    partial.source
    <- Option.map policy.source ~f:(fun source ->
         Authoring_source_host.create
           project_sources
           ~sw
           ~read_policy:source.read
           ~document_policy:source.document
           ~limits:source.limits);
    let%bind upload_store =
      match policy.upload with
      | None -> Ok None
      | Some upload ->
        Agent_store.Session_store.open_authoring_upload_store
          store
          ~sw
          ~limits:upload.limits
          ~checkpoint
        |> Result.map ~f:Option.some
        |> Result.map_error ~f:upload_store_error
    in
    partial.upload_store <- upload_store;
    partial.upload
    <- Option.map2 policy.upload upload_store ~f:(fun policy store ->
         Authoring_upload_host.create
           ?publication:policy.publication
           ?rebind:policy.rebind
           store
           ~sw
           ~clock:(Eio.Stdenv.clock env)
           ~upload_policy:policy.grant);
    let service =
      Service.create
        ~sw
        ~clock
        ~host:(Agent_store.Session_store.server_id store)
        ~sources:partial.source
        ~upload:partial.upload
        ~supplied_document:policy.supplied_document
        ~selection_policy:policy.selection
        ~context_limits:policy.context_limits
        ~analysis_policy:policy.analysis_policy
        ~protocol_limits:policy.protocol_limits
        ~limits:policy.service_limits
    in
    partial.service <- Some service;
    let semantic =
      Option.map policy.semantic ~f:(fun selected ->
        Service.Semantic.create
          ~owner:service
          ~env
          ~document_dir:selected.document_dir
          ~script_selection:selected.script_selection
          ~project_metadata:selected.project_metadata
          ~project_build_limits:selected.project_build_limits
          ~limits:selected.limits
          ~prefix_limits:selected.prefix_limits
          ~query_limits:selected.query_limits)
    in
    let%map assistance =
      match policy.assistance with
      | None -> Ok None
      | Some selected ->
        Service.Assistance.create
          ~owner:service
          ~script_selection:
            (Option.bind policy.semantic ~f:(fun semantic -> semantic.script_selection))
          ~project_metadata:
            (Option.bind policy.semantic ~f:(fun semantic -> semantic.project_metadata))
          ~selected_tools:selected.selected_tools
          ~project_build_limits:selected.project_build_limits
          ~limits:selected.limits
        |> Result.map ~f:Option.some
        |> Result.map_error ~f:Service.Error.to_protocol
    in
    { policy
    ; service
    ; semantic
    ; assistance
    ; source = partial.source
    ; upload = partial.upload
    ; closed = false
    }
  in
  match construct () with
  | Ok installed ->
    partial.transferred <- true;
    Ok installed
  | Error failure ->
    (try ignore (close_untransferred partial : (unit, P.Error.t) result) with
     | _ -> ());
    Error failure
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try ignore (close_untransferred partial : (unit, P.Error.t) result) with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let available_features (t : t) =
  List.concat
    [ (if t.policy.core then [ P.Authoring.Features.core ] else [])
    ; (if Option.is_some t.upload then [ P.Authoring.Features.bundle ] else [])
    ; (if Option.is_some t.semantic then [ P.Authoring.Features.semantic ] else [])
    ; (match t.policy.assistance with
       | None -> []
       | Some assistance ->
         (if assistance.lexical then [ P.Authoring.Features.lexical ] else [])
         @ if assistance.assistance then [ P.Authoring.Features.assistance ] else [])
    ]
;;

let selected negotiation feature =
  Connection_context.Authoring_negotiation.selected negotiation feature
;;

let installed_method_groups (t : t) =
  let open P.Authoring.Method in
  [ ( P.Authoring.Features.core
    , if t.policy.core
      then
        [ Context_open
        ; Context_refresh
        ; Context_close
        ; Source_list
        ; Source_read
        ; Document_version
        ; Document_replace
        ; Document_close
        ]
        @ if Option.is_some t.source then [ Roots; Host_list; Host_read ] else []
      else [] )
  ; ( P.Authoring.Features.bundle
    , if Option.is_some t.upload
      then [ Bundle_begin; Bundle_chunk; Bundle_commit; Bundle_status ]
      else [] )
  ; ( P.Authoring.Features.semantic
    , if Option.is_some t.semantic then [ Analyze; Prefix; Query ] else [] )
  ; ( P.Authoring.Features.lexical
    , if Option.exists t.policy.assistance ~f:(fun policy -> policy.lexical)
      then [ Lexical ]
      else [] )
  ; ( P.Authoring.Features.assistance
    , if Option.exists t.policy.assistance ~f:(fun policy -> policy.assistance)
      then [ Assistance ]
      else [] )
  ]
;;

let methods (t : t) ~include_group =
  P.Authoring.Method.Capabilities
  :: (installed_method_groups t
      |> List.concat_map ~f:(fun (feature, names) ->
        if include_group feature then names else []))
;;

let capabilities (t : t) negotiation =
  let methods = methods t ~include_group:(selected negotiation) in
  let core = selected negotiation P.Authoring.Features.core in
  (* Installed method names and protocol limits were admitted at construction;
     a failure here is an internal invariant, never a client request error. *)
  match
    P.Authoring.Capabilities.create
      ~methods:(List.map methods ~f:P.Authoring.Method.to_string)
      ~encodings:(if core then [ P.Authoring.Encoding.Utf8_bytes; Utf16_units ] else [])
      ~limits:t.policy.protocol_limits
  with
  | Ok capability -> capability
  | Error failure ->
    raise_s [%sexp "invalid installed authoring methods", (failure : P.Error.t)]
;;

let handle (t : t) ~actor ~connection request =
  let open Result.Let_syntax in
  if t.closed
  then Error (error Server_shutting_down "authoring is closed" ~retryable:true)
  else if
    not
      (List.mem
         (List.map
            (methods t ~include_group:(fun _ -> true))
            ~f:P.Authoring.Method.to_string)
         (W.Request.method_name request)
         ~equal:String.equal)
  then Error (uninstalled ())
  else (
    let result =
      match request with
      | W.Request.Capabilities ->
        Service.handle_capabilities t.service ~actor ~connection ~make:(capabilities t)
        |> Result.map_error ~f:Service.Error.to_protocol
      | Roots
      | Host_list _
      | Host_read _
      | Context_open _
      | Context_refresh _
      | Context_close _
      | Source_list _
      | Source_read _
      | Document_version _
      | Document_replace _
      | Document_close _ ->
        if t.policy.core
        then
          Service.handle_core t.service ~actor ~connection request
          |> Result.map_error ~f:Service.Error.to_protocol
        else Error (uninstalled ())
      | Bundle_begin _ | Bundle_chunk _ | Bundle_commit _ | Bundle_status _ ->
        if Option.is_some t.upload
        then
          Service.handle_bundle
            ?script_selection:
              (Option.bind t.policy.semantic ~f:(fun semantic ->
                 semantic.script_selection))
            t.service
            ~actor
            ~connection
            request
          |> Result.map_error ~f:Service.Error.to_protocol
        else Error (uninstalled ())
      | Analyze request ->
        (match t.semantic with
         | None -> Error (uninstalled ())
         | Some semantic ->
           Service.Semantic.analyze semantic ~actor ~connection request
           |> Result.map ~f:(fun value -> W.Response.Analyze value)
           |> Result.map_error ~f:Service.Error.to_protocol)
      | Prefix request ->
        (match t.semantic with
         | None -> Error (uninstalled ())
         | Some semantic ->
           Service.Semantic.prefix semantic ~actor ~connection request
           |> Result.map ~f:(fun value -> W.Response.Prefix value)
           |> Result.map_error ~f:Service.Error.to_protocol)
      | Query request ->
        (match t.semantic with
         | None -> Error (uninstalled ())
         | Some semantic ->
           Service.Semantic.query semantic ~actor ~connection request
           |> Result.map ~f:(fun value -> W.Response.Query value)
           |> Result.map_error ~f:Service.Error.to_protocol)
      | Lexical request ->
        (match t.assistance, t.policy.assistance with
         | Some assistance, Some policy when policy.lexical ->
           Service.Assistance.handle_lexical assistance ~actor ~connection request
           |> Result.map_error ~f:Service.Error.to_protocol
         | _ -> Error (uninstalled ()))
      | Assistance request ->
        (match t.assistance, t.policy.assistance with
         | Some assistance, Some policy when policy.assistance ->
           Service.Assistance.handle_assistance assistance ~actor ~connection request
           |> Result.map_error ~f:Service.Error.to_protocol
         | _ -> Error (uninstalled ()))
    in
    result)
;;

let publish_initialize_and_wait (t : t) ~connection ~response =
  if t.closed
  then Error (error Server_shutting_down "authoring is closed" ~retryable:true)
  else
    Service.publish_initialize_and_wait t.service ~connection ~response
    |> Result.map_error ~f:Service.Error.to_protocol
;;

let developer_capture (t : t) ~policy ~limits =
  if t.closed
  then Error (error Server_shutting_down "authoring is closed" ~retryable:true)
  else Ok (Service.Developer.create ~owner:t.service ~policy ~limits)
;;

let developer_roots (t : t) ~actor ~checkpoint =
  if t.closed
  then Error (error Server_shutting_down "authoring is closed" ~retryable:true)
  else (
    match t.source with
    | None -> Error (uninstalled ())
    | Some source ->
      Authoring_source_host.roots source ~actor ~checkpoint
      |> Result.map_error ~f:(fun failure ->
        Authoring_context_owner.source_host_error failure
        |> Authoring_context_service_core.Error.to_protocol))
;;

let close_connection_and_wait (t : t) connection =
  Service.close_connection_and_wait t.service connection
  |> Result.map_error ~f:Service.Error.to_protocol
;;

let close_and_wait = close_children
