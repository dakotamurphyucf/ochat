open Core
module Bundle = Agent_protocol.Authoring_bundle.Bundle
module Captured = Chatml_source
module Stage = Chatml_authoring_bundle_project.Attempt.Stage

type t =
  { root_file : string
  ; by_path : Captured.Id.t Map.M(String).t
  ; by_id : (Source.Portable_path.t * Captured.t) Map.M(Captured.Id).t
  ; bundle_sources : (string * string) list
  }

let root_file t = t.root_file
let bundle_sources t = t.bundle_sources
let source_id t path = Map.find t.by_path (Source.Portable_path.to_string path)

let create document ~stage =
  let open Result.Let_syntax in
  let%bind by_path, by_id, bundle_sources =
    List.fold_result
      (Bundle.Manifest.Chatmd_document.document_members document)
      ~init:(Map.empty (module String), Map.empty (module Captured.Id), [])
      ~f:(fun (by_path, by_id, sources) member ->
        Stage.checkpoint stage;
        let id = Bundle.Manifest.Document_member.source member in
        let path = Bundle.Manifest.Document_member.member member in
        let file = Source.Portable_path.to_string path in
        match Stage.source stage id with
        | None -> Error ()
        | Some source ->
          if Map.mem by_path file || Map.mem by_id id
          then Error ()
          else
            Ok
              ( Map.set by_path ~key:file ~data:id
              , Map.set by_id ~key:id ~data:(path, source)
              , (file, Captured.contents source) :: sources ))
  in
  let root = Bundle.Manifest.Chatmd_document.root_source document in
  match Map.find by_id root with
  | None -> Error ()
  | Some (path, _) ->
    Ok
      { root_file = Source.Portable_path.to_string path
      ; by_path
      ; by_id
      ; bundle_sources = List.rev bundle_sources
      }
;;

let source_at t path ~stage =
  Stage.checkpoint stage;
  Option.bind (source_id t path) ~f:(fun id -> Option.map (Map.find t.by_id id) ~f:snd)
;;

let input_source_id t input ~stage =
  Stage.checkpoint stage;
  match Map.find t.by_path (Chatmd_parser_input.path input) with
  | None -> None
  | Some id ->
    (match Map.find t.by_id id with
     | None -> None
     | Some (_, source) ->
       if
         Chatmd_parser_input.authored_matches
           input
           (Captured.contents source)
           ~checkpoint:(fun () -> Stage.checkpoint stage)
         && Chatmd_parser_input.effective_matches
              input
              (Captured.contents source)
              ~checkpoint:(fun () -> Stage.checkpoint stage)
       then Some id
       else None)
;;

let covers_reads_or_roots t ~read_members ~selected_roots ~stage =
  let covered =
    List.fold
      read_members
      ~init:(Map.empty (module Captured.Id))
      ~f:(fun covered path ->
        Stage.checkpoint stage;
        match Map.find t.by_path path with
        | None -> covered
        | Some id -> Map.set covered ~key:id ~data:())
  in
  let covered =
    List.fold selected_roots ~init:covered ~f:(fun covered id ->
      Stage.checkpoint stage;
      Map.set covered ~key:id ~data:())
  in
  Map.length covered = Map.length t.by_id
  && List.for_all read_members ~f:(fun path ->
    Stage.checkpoint stage;
    Map.mem t.by_path path)
;;
