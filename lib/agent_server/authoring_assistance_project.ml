open! Core
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Schema = Document_schema
module Error = Authoring_context_service_core.Error

let selected_project build ~max_project_bytes ~checkpoint =
  if max_project_bytes <= 0
  then Error Error.Resource_limit
  else (
    checkpoint ();
    let prepared = Build.prepared build in
    match Prepared.provenance prepared with
    | Implicit -> Ok None
    | Project { declaration; _ } ->
      (match Prepared.find_source prepared declaration with
       | None -> Error Error.Selection_unavailable
       | Some source ->
         let default = Schema.Limits.default in
         let max_bytes = Int.min max_project_bytes (Schema.Limits.max_bytes default) in
         if Chatml_source.byte_length source > max_bytes
         then Error Error.Resource_limit
         else (
           match
             Schema.Limits.create
               ~max_bytes
               ~max_depth:(Schema.Limits.max_depth default)
               ~max_fields:(Schema.Limits.max_fields default)
               ~max_nodes:(Schema.Limits.max_nodes default)
           with
           | Error _ -> Error Error.Resource_limit
           | Ok limits ->
             checkpoint ();
             (match Chatml_project.decode source ~limits ~checkpoint with
              | Ok project -> Ok (Some project)
              | Error _ -> Error Error.Selection_unavailable))))
;;
