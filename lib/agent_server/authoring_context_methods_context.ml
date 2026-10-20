open! Core
open Authoring_context_service_core
open Authoring_context_activity
open Authoring_context_owner
open Authoring_context_methods_common

let commit_context t entry ~ledger ~next ~context response =
  let open Result.Let_syntax in
  let%map () = check_snapshot t entry ledger in
  transition t next;
  entry.contexts <- context :: entry.contexts;
  response
;;

type opening =
  | Open_project of
      { owner : context_owner
      ; origin : P.Authoring.Context.origin
      ; build : Build.t
      ; editable_sources : Captured.Id.t list
      }
  | Open_document of
      { owner : context_owner
      ; origin : P.Authoring.Context.origin
      ; source : Captured.t
      ; requested_environment : string option
      }

let captured_metadata source =
  P.Authoring.Source.create
    ~id:(Captured.id source)
    ~digest:(Captured.digest source)
    ~byte_length:(Captured.byte_length source)
    ~editor_revision:None
  |> protocol_error
;;

let prepare_open t entry ~actor ~id ~checkpoint request =
  let open Result.Let_syntax in
  match (request : P.Authoring.Context.Open_request.t) with
  | Host_project
      { root = root_id
      ; dependency_roots = dependency_root_ids
      ; project_member
      ; selection
      } ->
    let%bind source_host = t.sources |> Result.of_option ~error:Error.Uninstalled in
    let%bind root, dependency_roots =
      with_gate t (fun () ->
        let%bind root = root_for entry root_id in
        let%map dependency_roots =
          List.map dependency_root_ids ~f:(root_for entry) |> Result.all
        in
        root, dependency_roots)
    in
    let%bind project = portable_member project_member in
    let%bind contract =
      t.selection_policy.project_contract actor ~root ~project ~selection
      |> Result.of_option ~error:Error.Selection_unavailable
    in
    let%bind owner =
      Sources.Context.open_root
        source_host
        ~actor
        ~root
        ~dependency_roots
        ~project
        ~selection:(project_selection selection)
        ~contract
      |> Result.map_error ~f:source_host_error
    in
    let keep = ref false in
    Exn.protect
      ~finally:(fun () -> if not !keep then Sources.Context.close_and_wait owner)
      ~f:(fun () ->
        let%map capture =
          Sources.Context.capture owner ~actor ~clock:t.clock ~checkpoint
          |> Result.map_error ~f:source_host_error
        in
        keep := true;
        Open_project
          { owner = Host_project owner
          ; origin =
              Host_project
                { root = root_id
                ; dependency_roots = dependency_root_ids
                ; project_member
                ; selection
                }
          ; build = Sources.Capture.build capture
          ; editable_sources = editable_local_sources (Sources.Capture.build capture)
          })
  | Uploaded_project bundle_id ->
    let%bind upload = t.upload |> Result.of_option ~error:Error.Uninstalled in
    let%bind capture =
      Upload.capture_for_context upload ~actor ~bundle_id ~checkpoint
      |> Result.map_error ~f:upload_host_error
    in
    let rebound = Upload.Context_capture.rebound capture in
    (match Upload.Rebound.basis rebound with
     | Upload.Rebound.Project validated ->
       Ok
         (Open_project
            { owner = Uploaded_project capture
            ; origin = Uploaded_project bundle_id
            ; build = Chatml_authoring_bundle_project.Validated.build validated
            ; editable_sources = []
            })
     | Chatmd_document _ -> Error Error.Selection_unavailable)
  | Host_document { root; member; default_environment } ->
    let%bind source_host = t.sources |> Result.of_option ~error:Error.Uninstalled in
    let%bind selected_root = with_gate t (fun () -> root_for entry root) in
    let%bind portable = portable_member member in
    let%bind owner, capture =
      Sources.Document_context.open_host
        source_host
        ~actor
        ~root:selected_root
        ~member:portable
        ~checkpoint
      |> Result.map_error ~f:source_host_error
    in
    let keep = ref false in
    Exn.protect
      ~finally:(fun () -> if not !keep then Sources.Document_context.close_and_wait owner)
      ~f:(fun () ->
        let source = Sources.Document_capture.source capture in
        let%map metadata = captured_metadata source in
        keep := true;
        Open_document
          { owner = Host_document owner
          ; origin = Host_document { root; source = metadata; default_environment }
          ; source
          ; requested_environment = default_environment
          })
  | Supplied_document { member; default_environment; contents } ->
    let%bind policy = t.supplied_document |> Result.of_option ~error:Error.Uninstalled in
    if not (Supplied_document_policy.read_admitted policy actor)
    then Error Error.Unauthorized
    else (
      let%bind _ = portable_member member in
      let%bind () =
        if String.length contents > t.protocol_limits.max_source_bytes
        then Error Error.Resource_limit
        else Ok ()
      in
      let%bind source_id =
        Captured.Id.create
          ~package_namespace:
            ("@ochat.authoring.document." ^ P.Id.Authoring_context.to_string id)
          ~relative_path:member
        |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
      in
      let%bind source =
        Captured.create
          source_id
          ~contents
          ~expected_digest:None
          ~max_bytes:(Some t.protocol_limits.max_source_bytes)
          ~checkpoint
        |> source_error
      in
      let%map metadata = captured_metadata source in
      Open_document
        { owner = Supplied_document actor
        ; origin = Supplied_document { source = metadata; default_environment }
        ; source
        ; requested_environment = default_environment
        })
;;

let handle_context_open t ~actor ~connection request ~checkpoint =
  let pending = ref None in
  let opened_owner = ref None in
  with_request
    t
    ~actor
    ~connection
    ~checkpoint
    ~abort:(fun () -> Option.iter !opened_owner ~f:close_owner)
    ~final_check:(fun () ->
      match !pending with
      | None -> Error Error.Stale_context
      | Some (_, _, next, context) ->
        check_context_selection_now ~ledger:next t context ~actor)
    ~commit:(fun response ->
      match !pending with
      | None -> Error Error.Stale_context
      | Some (entry, ledger, next, context) ->
        commit_context t entry ~ledger ~next ~context response)
    (fun entry ~checkpoint ->
       let open Result.Let_syntax in
       let id = P.Id.Authoring_context.create () in
       let%bind identity = identity_for t entry id in
       let%bind opening = prepare_open t entry ~actor ~id ~checkpoint request in
       let owner =
         match opening with
         | Open_project { owner; _ } | Open_document { owner; _ } -> owner
       in
       opened_owner := Some owner;
       let ledger =
         with_gate t (fun () -> Project.Project_cache.contexts t.project_cache)
       in
       let%bind next, origin =
         match opening with
         | Open_project { origin; build; editable_sources; _ } ->
           Ledger.admit ledger ~identity ~base:build ~editable_sources ~checkpoint
           |> Result.map_error ~f:ledger_error
           |> Result.map ~f:(fun next -> next, origin)
         | Open_document { origin; source; requested_environment; _ } ->
           let%bind next =
             Ledger.admit_document
               ledger
               ~identity
               ~base:source
               ~requested_environment
               ~checkpoint
             |> Result.map_error ~f:ledger_error
           in
           let%map next =
             match t.selection_policy.document_static actor ~requested_environment with
             | None -> Ok next
             | Some build ->
               (match Ledger.find_document next ~identity with
                | None -> Error Error.Stale_context
                | Some document ->
                  Ledger.attach_document_static
                    next
                    ~identity
                    ~expected:(Ledger.Document.View.generation document)
                    ~build
                    ~checkpoint
                  |> Result.map_error ~f:ledger_error)
           in
           next, origin
       in
       let%bind view = view_of_context t next ~id ~identity ~origin ~checkpoint in
       let context = { id; identity; original_actor = actor; origin; owner; view } in
       let%bind () = check_context_owner ~ledger:next t context ~actor ~checkpoint in
       let%map response = checked_response t (W.Response.Context_open view) in
       pending := Some (entry, ledger, next, context);
       response)
;;

let document_version ledger context source_id ~checkpoint =
  let open Result.Let_syntax in
  let%bind sources = selected_sources ledger context.identity ~checkpoint in
  let%bind source = captured_source sources source_id in
  let%bind version =
    Ledger.version ledger ~identity:context.identity ~source:source_id
    |> Result.of_option ~error:Error.Source_unavailable
  in
  let%bind metadata =
    Authoring_context_projection.source_metadata ledger context.identity source
    |> Result.map_error ~f:(fun () -> Error.Source_unavailable)
  in
  P.Authoring.Document.Version.create
    ~context:(P.Authoring.Context.Expected.of_view context.view)
    ~source:metadata
    ~last_editor_revision:
      (Option.map
         (Ledger.Version.editor_revision version)
         ~f:Ledger.Editor_revision.to_int)
    ~overlay_open:(Ledger.Version.overlay_open version)
  |> protocol_error
;;

let equal_document_version left right =
  P.Authoring.Context.Expected.equal
    (P.Authoring.Document.Version.context left)
    (P.Authoring.Document.Version.context right)
  && P.Authoring.Source.equal
       (P.Authoring.Document.Version.source left)
       (P.Authoring.Document.Version.source right)
  && Option.equal
       Int.equal
       (P.Authoring.Document.Version.last_editor_revision left)
       (P.Authoring.Document.Version.last_editor_revision right)
  && Bool.equal
       (P.Authoring.Document.Version.overlay_open left)
       (P.Authoring.Document.Version.overlay_open right)
;;

let lookup_expected t entry expected =
  with_gate t (fun () ->
    match find_context entry expected.P.Authoring.Context.Expected.context_id with
    | None -> Error Error.Stale_context
    | Some context ->
      if
        P.Authoring.Context.Expected.equal
          expected
          (P.Authoring.Context.Expected.of_view context.view)
      then Ok (context, Project.Project_cache.contexts t.project_cache)
      else Error Error.Stale_context)
;;

let commit_updated t entry ~ledger ~next ~context ~expected ~view response =
  let open Result.Let_syntax in
  let%bind () = check_snapshot t entry ledger in
  let%bind () =
    match find_context entry context.id with
    | Some actual
      when phys_equal actual context
           && P.Authoring.Context.Expected.equal
                expected
                (P.Authoring.Context.Expected.of_view actual.view) ->
      transition t next;
      context.view <- view;
      Ok ()
    | Some _ | None -> Error Error.Stale_context
  in
  Ok response
;;

let handle_document_version
      t
      ~actor
      ~connection
      (request : P.Authoring.Document.Version_request.t)
      ~checkpoint
  =
  with_current
    t
    ~actor
    ~connection
    ~expected:request.context
    ~checkpoint
    ~f:(fun current ~checkpoint ->
      let open Result.Let_syntax in
      let%bind version =
        document_version current.owner current.context request.source_id ~checkpoint
      in
      checked_response t (W.Response.Document_version version))
;;

let handle_document_replace
      t
      ~actor
      ~connection
      (request : P.Authoring.Document.Replace_request.t)
      ~checkpoint
  =
  let pending = ref None in
  with_request
    t
    ~actor
    ~connection
    ~checkpoint
    ~final_check:(fun () ->
      match !pending with
      | None -> Error Error.Stale_context
      | Some (_, _, next, context, _, _) ->
        check_context_selection_now ~ledger:next t context ~actor)
    ~commit:(fun response ->
      match !pending with
      | None -> Error Error.Stale_context
      | Some (entry, ledger, next, context, expected, view) ->
        commit_updated t entry ~ledger ~next ~context ~expected ~view response)
    (fun entry ~checkpoint ->
       let open Result.Let_syntax in
       let wire_version = request.expected in
       let expected = P.Authoring.Document.Version.context wire_version in
       let%bind context, ledger = lookup_expected t entry expected in
       let%bind () = check_context_owner t context ~actor ~checkpoint in
       let source_id =
         P.Authoring.Source.id (P.Authoring.Document.Version.source wire_version)
       in
       let%bind actual = document_version ledger context source_id ~checkpoint in
       let%bind () =
         if equal_document_version actual wire_version
         then Ok ()
         else Error Error.Stale_source
       in
       let%bind native =
         Ledger.version ledger ~identity:context.identity ~source:source_id
         |> Result.of_option ~error:Error.Stale_source
       in
       let%bind next_revision =
         Ledger.Editor_revision.create request.next_editor_revision
         |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
       in
       let%bind next =
         Ledger.replace
           ledger
           ~expected:native
           ~next_revision
           ~contents:request.contents
           ~checkpoint
         |> Result.map_error ~f:ledger_error
       in
       let%bind view =
         view_of_context
           t
           next
           ~id:context.id
           ~identity:context.identity
           ~origin:context.origin
           ~checkpoint
       in
       let current = { context with view } in
       let%bind version = document_version next current source_id ~checkpoint in
       let%bind result =
         P.Authoring.Document.Result.create ~context:view ~version |> protocol_error
       in
       let%bind () = check_context_owner ~ledger:next t context ~actor ~checkpoint in
       let%map response = checked_response t (W.Response.Document_replace result) in
       pending := Some (entry, ledger, next, context, expected, view);
       response)
;;

let handle_document_close
      t
      ~actor
      ~connection
      (request : P.Authoring.Document.Close_request.t)
      ~checkpoint
  =
  let pending = ref None in
  with_request
    t
    ~actor
    ~connection
    ~checkpoint
    ~final_check:(fun () ->
      match !pending with
      | None -> Error Error.Stale_context
      | Some (_, _, next, context, _, _) ->
        check_context_selection_now ~ledger:next t context ~actor)
    ~commit:(fun response ->
      match !pending with
      | None -> Error Error.Stale_context
      | Some (entry, ledger, next, context, expected, view) ->
        commit_updated t entry ~ledger ~next ~context ~expected ~view response)
    (fun entry ~checkpoint ->
       let open Result.Let_syntax in
       let wire_version = request.expected in
       let expected = P.Authoring.Document.Version.context wire_version in
       let%bind context, ledger = lookup_expected t entry expected in
       let%bind () = check_context_owner t context ~actor ~checkpoint in
       let source_id =
         P.Authoring.Source.id (P.Authoring.Document.Version.source wire_version)
       in
       let%bind actual = document_version ledger context source_id ~checkpoint in
       let%bind () =
         if equal_document_version actual wire_version
         then Ok ()
         else Error Error.Stale_source
       in
       let%bind native =
         Ledger.version ledger ~identity:context.identity ~source:source_id
         |> Result.of_option ~error:Error.Stale_source
       in
       let%bind next =
         Ledger.close_document ledger ~expected:native |> Result.map_error ~f:ledger_error
       in
       let%bind view =
         view_of_context
           t
           next
           ~id:context.id
           ~identity:context.identity
           ~origin:context.origin
           ~checkpoint
       in
       let current = { context with view } in
       let%bind version = document_version next current source_id ~checkpoint in
       let%bind result =
         P.Authoring.Document.Result.create ~context:view ~version |> protocol_error
       in
       let%bind () = check_context_owner ~ledger:next t context ~actor ~checkpoint in
       let%map response = checked_response t (W.Response.Document_close result) in
       pending := Some (entry, ledger, next, context, expected, view);
       response)
;;

let refresh_policy = function
  | P.Authoring.Context.Refresh_request.Reject_open_documents ->
    Ledger.Refresh.Reject_open_documents
  | Discard_open_documents -> Ledger.Refresh.Discard_open_documents
;;

let handle_context_refresh
      t
      ~actor
      ~connection
      (request : P.Authoring.Context.Refresh_request.t)
      ~checkpoint
  =
  let pending = ref None in
  with_request
    t
    ~actor
    ~connection
    ~checkpoint
    ~final_check:(fun () ->
      match !pending with
      | None -> Error Error.Stale_context
      | Some (_, _, next, _, _, replacement) ->
        check_context_selection_now ~ledger:next t replacement ~actor)
    ~commit:(fun response ->
      let open Result.Let_syntax in
      match !pending with
      | None -> Error Error.Stale_context
      | Some (entry, ledger, next, context, expected, replacement) ->
        let%bind () = check_snapshot t entry ledger in
        (match find_context entry context.id with
         | Some actual
           when phys_equal actual context
                && P.Authoring.Context.Expected.equal
                     expected
                     (P.Authoring.Context.Expected.of_view actual.view) ->
           transition t next;
           entry.contexts
           <- replacement
              :: List.filter entry.contexts ~f:(fun item -> not (phys_equal item context));
           Ok response
         | Some _ | None -> Error Error.Stale_context))
    (fun entry ~checkpoint ->
       let open Result.Let_syntax in
       let expected = request.expected in
       let%bind context, ledger = lookup_expected t entry expected in
       let%bind () = check_context_owner t context ~actor ~checkpoint in
       let id = P.Id.Authoring_context.create () in
       let%bind identity = identity_for t entry id in
       let policy = refresh_policy request.policy in
       let%bind next, origin =
         match context.owner with
         | Host_project owner ->
           let%bind capture =
             Sources.Context.capture owner ~actor ~clock:t.clock ~checkpoint
             |> Result.map_error ~f:source_host_error
           in
           let build = Sources.Capture.build capture in
           let%bind old =
             Ledger.find ledger ~identity:context.identity
             |> Result.of_option ~error:Error.Stale_context
           in
           let%map next =
             Ledger.refresh
               ledger
               ~identity:context.identity
               ~expected:(Ledger.View.generation old)
               ~new_identity:identity
               ~base:build
               ~editable_sources:(editable_local_sources build)
               ~policy
               ~checkpoint
             |> Result.map_error ~f:ledger_error
           in
           next, context.origin
         | Host_document owner ->
           let%bind capture =
             Sources.Document_context.refresh owner ~actor ~checkpoint
             |> Result.map_error ~f:source_host_error
           in
           let source = Sources.Document_capture.source capture in
           let%bind old =
             Ledger.find_document ledger ~identity:context.identity
             |> Result.of_option ~error:Error.Stale_context
           in
           let%bind next =
             Ledger.refresh_document
               ledger
               ~identity:context.identity
               ~expected:(Ledger.Document.View.generation old)
               ~new_identity:identity
               ~base:source
               ~policy
               ~checkpoint
             |> Result.map_error ~f:ledger_error
           in
           let requested_environment =
             match context.origin with
             | Host_document { default_environment; _ } -> default_environment
             | Host_project _ | Uploaded_project _ | Supplied_document _ -> None
           in
           let%bind next =
             match t.selection_policy.document_static actor ~requested_environment with
             | None ->
               (match P.Authoring.Context.basis context.view with
                | Document { selection = Some _; _ } -> Error Error.Selection_unavailable
                | Document { selection = None; _ } -> Ok next
                | Project _ -> Error Error.Stale_context)
             | Some build ->
               (match Ledger.find_document next ~identity with
                | None -> Error Error.Stale_context
                | Some document ->
                  Ledger.attach_document_static
                    next
                    ~identity
                    ~expected:(Ledger.Document.View.generation document)
                    ~build
                    ~checkpoint
                  |> Result.map_error ~f:ledger_error)
           in
           let%map source = captured_metadata source in
           let origin =
             match context.origin with
             | Host_document { root; default_environment; _ } ->
               P.Authoring.Context.Host_document { root; source; default_environment }
             | Host_project _ | Uploaded_project _ | Supplied_document _ -> context.origin
           in
           next, origin
         | Uploaded_project _ | Supplied_document _ -> Error Error.Source_unavailable
       in
       let%bind view = view_of_context t next ~id ~identity ~origin ~checkpoint in
       let replacement = { context with id; identity; origin; view } in
       let%bind () = check_context_owner ~ledger:next t replacement ~actor ~checkpoint in
       let%map response = checked_response t (W.Response.Context_refresh view) in
       pending := Some (entry, ledger, next, context, expected, replacement);
       response)
;;

let handle_context_close
      t
      ~actor
      ~connection
      (request : P.Authoring.Context.Close_request.t)
      ~checkpoint
  =
  let pending = ref None in
  with_request
    t
    ~actor
    ~connection
    ~checkpoint
    ~final_check:(fun () ->
      match !pending with
      | None -> Error Error.Stale_context
      | Some (_, _, _, context, _) -> check_context_selection_now t context ~actor)
    ~commit:(fun response ->
      let open Result.Let_syntax in
      match !pending with
      | None -> Error Error.Stale_context
      | Some (entry, ledger, next, context, expected) ->
        let%bind () = check_snapshot t entry ledger in
        (match find_context entry context.id with
         | Some actual
           when phys_equal actual context
                && P.Authoring.Context.Expected.equal
                     expected
                     (P.Authoring.Context.Expected.of_view actual.view) ->
           transition t next;
           entry.contexts
           <- List.filter entry.contexts ~f:(fun item -> not (phys_equal item context));
           entry.retired_owners <- context.owner :: entry.retired_owners;
           Ok response
         | Some _ | None -> Error Error.Stale_context))
    ~after_commit:(fun () ->
      match !pending with
      | None -> ()
      | Some (entry, _, _, context, _) ->
        close_owner context.owner;
        with_gate t (fun () ->
          entry.retired_owners
          <- List.filter entry.retired_owners ~f:(fun owner ->
               not (phys_equal owner context.owner))))
    (fun entry ~checkpoint ->
       let open Result.Let_syntax in
       let expected = request.expected in
       let%bind context, ledger = lookup_expected t entry expected in
       let%bind () = check_context_owner t context ~actor ~checkpoint in
       let%bind next =
         Ledger.close_context ledger ~identity:context.identity
         |> Result.map_error ~f:ledger_error
       in
       let%bind result =
         P.Authoring.Context.Close_result.create
           ~id:context.id
           ~host:t.host
           ~generation:(P.Authoring.Context.generation context.view)
         |> protocol_error
       in
       let%bind response = checked_response t (W.Response.Context_close result) in
       let%map () = check_context_owner t context ~actor ~checkpoint in
       pending := Some (entry, ledger, next, context, expected);
       response)
;;
