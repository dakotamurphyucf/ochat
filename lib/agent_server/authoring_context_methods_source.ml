open! Core
open Authoring_context_service_core
open Authoring_context_activity
open Authoring_context_owner
open Authoring_context_methods_common

let handle_roots t ~actor ~connection ~checkpoint =
  let issued = ref [] in
  let pending = ref None in
  with_request
    t
    ~actor
    ~connection
    ~checkpoint
    ~final_check:(fun () ->
      match t.sources with
      | None -> Error Error.Uninstalled
      | Some source_host ->
        List.fold_result !issued ~init:() ~f:(fun () root ->
          Sources.check_root_current source_host ~actor root
          |> Result.map_error ~f:source_host_error))
    ~commit:(fun response ->
      match !pending with
      | None -> Error Error.Stale_context
      | Some (entry, existing, selected) ->
        if phys_equal entry.roots existing
        then (
          entry.roots <- selected;
          Ok response)
        else Error Error.Busy)
    (fun entry ~checkpoint ->
       let open Result.Let_syntax in
       let%bind source_host = t.sources |> Result.of_option ~error:Error.Uninstalled in
       let%bind roots =
         Sources.roots source_host ~actor ~checkpoint
         |> Result.map_error ~f:source_host_error
       in
       let%bind () =
         if List.length roots > t.limits.max_roots_per_connection
         then Error Error.Resource_limit
         else Ok ()
       in
       let existing = with_gate t (fun () -> entry.roots) in
       let selected =
         List.map roots ~f:(fun root ->
           match
             List.find existing ~f:(fun (_, known) ->
               Int.equal (Sources.Root.index root) (Sources.Root.index known))
           with
           | Some (id, _) -> id, root
           | None -> P.Id.Authoring_root.create (), root)
       in
       let%bind roots =
         List.map selected ~f:(fun (id, root) ->
           P.Authoring.Source_read.Root.create
             ~id
             ~display_name:("Root " ^ Int.to_string (Sources.Root.index root + 1))
           |> protocol_error)
         |> Result.all
       in
       let%bind roots =
         P.Authoring.Source_read.Roots_result.create ~roots |> protocol_error
       in
       let%map response = checked_response t (W.Response.Roots roots) in
       issued := List.map selected ~f:snd;
       pending := Some (entry, existing, selected);
       response)
;;

let handle_host_list
      t
      ~actor
      ~connection
      (request : P.Authoring.Source_read.Host_list_request.t)
      ~checkpoint
  =
  let admitted_root = ref None in
  with_request
    t
    ~actor
    ~connection
    ~checkpoint
    ~final_check:(fun () ->
      match t.sources, !admitted_root with
      | Some source_host, Some root ->
        Sources.check_root_current source_host ~actor root
        |> Result.map_error ~f:source_host_error
      | None, _ -> Error Error.Uninstalled
      | Some _, None -> Error Error.Source_unavailable)
    ~commit:(fun response ->
      match find_connection t connection, !admitted_root with
      | Some entry, Some admitted ->
        (match root_for entry request.root with
         | Ok current when phys_equal current admitted -> Ok response
         | Ok _ | Error _ -> Error Error.Source_unavailable)
      | Some _, None | None, _ -> Error Error.Source_unavailable)
    (fun entry ~checkpoint ->
       let open Result.Let_syntax in
       let%bind source_host = t.sources |> Result.of_option ~error:Error.Uninstalled in
       let%bind root = with_gate t (fun () -> root_for entry request.root) in
       let%bind directory =
         Option.value_map request.directory ~default:(Ok None) ~f:(fun value ->
           portable_member value |> Result.map ~f:Option.some)
       in
       let%bind entries =
         Sources.list_root_directory source_host ~actor ~root ~directory ~checkpoint
         |> Result.map_error ~f:source_host_error
       in
       let%bind () =
         if List.length entries > t.protocol_limits.max_path_candidates
         then Error Error.Resource_limit
         else Ok ()
       in
       let members =
         List.map entries ~f:(fun entry ->
           Source.Portable_path.to_string (Sources.Entry.member entry))
       in
       let%bind result =
         P.Authoring.Source_read.Host_list_result.create ~root:request.root ~members
         |> protocol_error
       in
       let%map response = checked_response t (W.Response.Host_list result) in
       admitted_root := Some root;
       response)
;;

let handle_host_read
      t
      ~actor
      ~connection
      (request : P.Authoring.Source_read.Host_read_request.t)
      ~checkpoint
  =
  let admitted_root = ref None in
  with_request
    t
    ~actor
    ~connection
    ~checkpoint
    ~final_check:(fun () ->
      match t.sources, !admitted_root with
      | Some source_host, Some root ->
        Sources.check_root_current source_host ~actor root
        |> Result.map_error ~f:source_host_error
      | None, _ -> Error Error.Uninstalled
      | Some _, None -> Error Error.Source_unavailable)
    ~commit:(fun response ->
      match find_connection t connection, !admitted_root with
      | Some entry, Some admitted ->
        (match root_for entry request.root with
         | Ok current when phys_equal current admitted -> Ok response
         | Ok _ | Error _ -> Error Error.Source_unavailable)
      | Some _, None | None, _ -> Error Error.Source_unavailable)
    (fun entry ~checkpoint ->
       let open Result.Let_syntax in
       let%bind source_host = t.sources |> Result.of_option ~error:Error.Uninstalled in
       let%bind root = with_gate t (fun () -> root_for entry request.root) in
       let%bind member = portable_member request.member in
       let%bind capture =
         Sources.read_root_member source_host ~actor ~root ~member ~checkpoint
         |> Result.map_error ~f:source_host_error
       in
       let source = Sources.Document_capture.source capture in
       let%bind () =
         if Captured.byte_length source > t.protocol_limits.max_source_bytes
         then Error Error.Resource_limit
         else Ok ()
       in
       let%bind source =
         P.Authoring.Source.create
           ~id:(Captured.id source)
           ~digest:(Captured.digest source)
           ~byte_length:(Captured.byte_length source)
           ~editor_revision:None
         |> protocol_error
       in
       let%bind result =
         P.Authoring.Source_read.Host_read_result.create
           ~root:request.root
           ~member:request.member
           ~source
           ~contents:(Captured.contents (Sources.Document_capture.source capture))
         |> protocol_error
       in
       let%map response = checked_response t (W.Response.Host_read result) in
       admitted_root := Some root;
       response)
;;

let handle_source_list
      t
      ~actor
      ~connection
      (request : P.Authoring.Source_read.List_request.t)
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
      let identity = current.Current.context.identity in
      let%bind sources = selected_sources current.owner identity ~checkpoint in
      let directory = Option.map request.directory ~f:(fun value -> value ^ "/") in
      let matched =
        List.filter sources ~f:(fun source ->
          checkpoint ();
          let id = Captured.id source in
          let namespace_matches =
            Option.value_map request.package_namespace ~default:true ~f:(fun name ->
              String.equal name (Captured.Id.package_namespace id))
          in
          namespace_matches
          && Option.value_map directory ~default:true ~f:(fun directory ->
            String.is_prefix (Captured.Id.relative_path id) ~prefix:directory))
        |> List.sort ~compare:(fun a b ->
          Captured.Id.compare (Captured.id a) (Captured.id b))
      in
      let retained, omitted =
        if List.length matched <= t.protocol_limits.max_sources
        then matched, 0
        else
          ( List.take matched t.protocol_limits.max_sources
          , List.length matched - t.protocol_limits.max_sources )
      in
      let%bind sources =
        List.map retained ~f:(fun source ->
          Authoring_context_projection.source_metadata current.owner identity source
          |> Result.map_error ~f:(fun () -> Error.Source_unavailable))
        |> Result.all
      in
      let%bind result =
        P.Authoring.Source_read.List_result.create
          ~context:request.context
          ~sources
          ~omitted
        |> protocol_error
      in
      checked_response t (W.Response.Source_list result))
;;

let handle_source_read
      t
      ~actor
      ~connection
      (request : P.Authoring.Source_read.Read_request.t)
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
      let identity = current.Current.context.identity in
      let%bind sources = selected_sources current.owner identity ~checkpoint in
      let%bind source = captured_source sources request.source_id in
      let%bind () =
        if Captured.byte_length source > t.protocol_limits.max_source_bytes
        then Error Error.Resource_limit
        else Ok ()
      in
      let%bind metadata =
        Authoring_context_projection.source_metadata current.owner identity source
        |> Result.map_error ~f:(fun () -> Error.Source_unavailable)
      in
      let%bind result =
        P.Authoring.Source_read.Read_result.create
          ~context:request.context
          ~source:metadata
          ~contents:(Captured.contents source)
        |> protocol_error
      in
      checked_response t (W.Response.Source_read result))
;;
