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

  let equal_string left right ~checkpoint =
    let length = String.length left in
    if not (Int.equal length (String.length right))
    then false
    else (
      let offset = ref 0 in
      let same = ref true in
      while !same && !offset < length do
        checkpoint ();
        same := Char.equal left.[!offset] right.[!offset];
        Int.incr offset
      done;
      !same)
  ;;

  let equal_id left right ~checkpoint =
    equal_string
      (Captured.Id.package_namespace left)
      (Captured.Id.package_namespace right)
      ~checkpoint
    && equal_string
         (Captured.Id.relative_path left)
         (Captured.Id.relative_path right)
         ~checkpoint
  ;;

  let of_location script location ~checkpoint =
    checkpoint ();
    let selected_source = Admission.Script.selected_source script in
    if
      not
        (equal_id
           (Captured.Location.source location)
           (Captured.id selected_source)
           ~checkpoint)
    then Ok None
    else if
      not
        (Captured.Digest.equal
           (Captured.Location.source_digest location)
           (Captured.digest selected_source))
    then Error Error.Script_body_mismatch
    else
      Markdown.Declaration.Extraction.project_raw script.extraction location ~checkpoint
      |> Result.map_error ~f:(fun error -> Error.Projection error)
      |> Result.map ~f:Option.some
  ;;

  let of_diagnostic candidate projected ~checkpoint =
    let script = candidate.Cache.Candidate.script in
    let project_site native =
      let site = Analysis.Site_projection.native native in
      of_location script (Diagnostic.Site.location site) ~checkpoint
      |> Result.map ~f:(fun authored -> { native; authored })
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
