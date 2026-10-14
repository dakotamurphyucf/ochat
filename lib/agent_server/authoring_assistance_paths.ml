open! Core
module Sources = Authoring_source_host
module Context = Agent_protocol.Authoring.Context
module Path_candidate = Chatmd_authoring_assistance.Path_candidate
module Cursor = Language_highlighting.Highlight_snapshot.Cursor
module Source_owner = Language_highlighting.Highlight_snapshot.Source_owner
module Error = Authoring_context_service_core.Error

let path_prefix cursor =
  match Cursor.kind cursor with
  | Cursor.Kind.Attribute_value { attribute; value_state; _ }
    when (String.equal attribute "src" || String.equal attribute "path")
         && not (Cursor.Value_state.open_entity value_state) ->
    Some (Cursor.Raw_slice.contents (Cursor.raw_prefix cursor))
  | Attribute_value _ | Tag_name _ | Attribute_name _ | Text | Script_body | Opaque ->
    None
;;

let host_document
      ~owner
      ~view
      ~source
      ~cursor
      ~actor
      ~max_paths
      ~max_projection_work_steps
      ~checkpoint
  =
  let open Result.Let_syntax in
  if
    max_paths <= 0
    || max_paths > Agent_protocol.Authoring_assistance.Limits.max_suggestions
    || max_projection_work_steps <= 0
    || max_projection_work_steps > Agent_protocol.Authoring.Limits.maximum_wire_integer
  then Error Error.Resource_limit
  else (
    match
      Sources.Document_context.origin owner, Context.origin view, path_prefix cursor
    with
    | ( Sources.Document_context.Origin.Host_member
      , Context.Host_document { root; source = base; _ }
      , Some prefix ) ->
      let source_id = Chatml_source.id source in
      let owner_digest_ok =
        match Cursor.owner cursor with
        | Source_owner.Chatmd { view = Authored; view_sha256; _ } ->
          String.equal
            view_sha256
            (Chatml_source.Digest.to_hex (Chatml_source.digest source))
        | Chatmd { view = Effective; _ } | Chatml _ -> false
      in
      if
        (not owner_digest_ok)
        || (not
              (Chatml_source.Id.equal
                 source_id
                 (Sources.Document_context.source_id owner)))
        || not
             (Chatml_source.Id.equal source_id (Agent_protocol.Authoring.Source.id base))
      then Error Error.Stale_source
      else (
        let remaining = ref max_projection_work_steps in
        let charge amount =
          if amount < 0 || amount > !remaining
          then Error Error.Resource_limit
          else (
            remaining := !remaining - amount;
            checkpoint ();
            Ok ())
        in
        let%bind () = charge 2050 in
        if String.length prefix >= 1024
        then Error Error.Resource_limit
        else if String.contains prefix '&'
        then Ok []
        else (
          let%bind base_member =
            Source.Portable_path.create (Chatml_source.Id.relative_path source_id)
            |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
          in
          match
            Source_loader.resolve_member
              base_member
              ~reference:(prefix ^ "x")
              ~max_reference_bytes:1024
              ~checkpoint
          with
          | Error _ -> Ok []
          | Ok probe ->
            let directory =
              let parent = Filename.dirname (Source.Portable_path.to_string probe) in
              if String.equal parent "."
              then Ok None
              else
                Source.Portable_path.create parent
                |> Result.map ~f:Option.some
                |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
            in
            let%bind directory = directory in
            let%bind entries =
              Sources.Document_context.list_directory owner ~actor ~directory ~checkpoint
              |> Result.map_error ~f:Authoring_context_owner.source_host_error
            in
            let dir_prefix =
              match String.rsplit2 prefix ~on:'/' with
              | None -> ""
              | Some (parent, _) -> parent ^ "/"
            in
            let rec project count acc = function
              | [] -> Ok (List.rev acc)
              | entry :: tail ->
                let%bind () = charge 2050 in
                if count >= max_paths
                then Error Error.Resource_limit
                else (
                  let member = Sources.Entry.member entry in
                  let name = Filename.basename (Source.Portable_path.to_string member) in
                  let%bind () = charge (String.length name + String.length dir_prefix) in
                  let edit = dir_prefix ^ name in
                  let%bind edit_spelling =
                    Path_candidate.Edit_spelling.create edit
                    |> Result.map_error ~f:(fun _ -> Error.Resource_limit)
                  in
                  let%bind resolved =
                    Source_loader.resolve_member
                      base_member
                      ~reference:edit
                      ~max_reference_bytes:1024
                      ~checkpoint
                    |> Result.map_error ~f:(fun _ -> Error.Invalid_request)
                  in
                  if not (Source.Portable_path.equal resolved member)
                  then Error Error.Invalid_request
                  else (
                    let candidate =
                      Path_candidate.create ~origin:(Admitted_root root) ~edit_spelling
                    in
                    project (count + 1) (candidate :: acc) tail))
            in
            project 0 [] entries))
    | Sources.Document_context.Origin.Supplied, _, _
    | Host_member, (Context.Host_project _ | Uploaded_project _ | Supplied_document _), _
    | Host_member, Host_document _, None -> Ok [])
;;
