open Core
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Analysis = Chatml_authoring_analysis
module Markdown = Prompt.Chat_markdown
module Admission = Authoring_script_admission
module Cache = Authoring_script_cache
module Error = Admission.Error

module Projection = struct
  type site =
    { native : Analysis.Site_projection.t
    ; authored : Markdown.Declaration.Extraction.projection option
    }

  type t =
    { native : Diagnostic.t
    ; primary : site option
    ; related : (Diagnostic.Related.t * site) list
    ; sites_truncated : bool
    }

  let native t = t.native
  let primary t = t.primary
  let related t = t.related
  let sites_truncated t = t.sites_truncated

  let of_diagnostic candidate projected ~checkpoint =
    let script = candidate.Cache.Candidate.script in
    let selected_source = script.Admission.Script.entry_source in
    let project_site native =
      checkpoint ();
      let site = Analysis.Site_projection.native native in
      let source = Diagnostic.Site.source site in
      if
        Captured.Id.equal (Captured.id source) (Captured.id selected_source)
        && Captured.Digest.equal
             (Captured.digest source)
             (Captured.digest selected_source)
      then
        Markdown.Declaration.Extraction.project_raw
          script.extraction
          (Diagnostic.Site.location site)
          ~checkpoint
        |> Result.map_error ~f:(fun error -> Error.Projection error)
        |> Result.map ~f:(fun authored -> { native; authored = Some authored })
      else Ok { native; authored = None }
    in
    let open Result.Let_syntax in
    let%bind primary =
      match Analysis.Diagnostic_projection.primary projected with
      | None -> Ok None
      | Some site -> project_site site |> Result.map ~f:Option.some
    in
    let%map related =
      List.fold_result
        (Analysis.Diagnostic_projection.related projected)
        ~init:[]
        ~f:(fun reversed (related, site) ->
          let%map projected_site = project_site site in
          (related, projected_site) :: reversed)
      |> Result.map ~f:List.rev
    in
    { native = Analysis.Diagnostic_projection.native projected
    ; primary
    ; related
    ; sites_truncated = Analysis.Diagnostic_projection.sites_truncated projected
    }
  ;;
end
