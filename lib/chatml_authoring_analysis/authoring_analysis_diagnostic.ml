open Core
module Captured = Chatml_source
module Diagnostic = Chatml_diagnostic
module Position = Chatml_editor_position
module Policy = Authoring_analysis_checked.Policy
module Checked = Authoring_analysis_checked.Checked

module Site_projection = struct
  module Error = struct
    type t =
      | Position of Position.Error.t
      | Projection_limit
    [@@deriving sexp_of]
  end

  type t =
    { native : Diagnostic.Site.t
    ; byte_range : (Position.Range.t, Error.t) Result.t
    ; utf16_range : (Position.Range.t, Error.t) Result.t
    }

  let native t = t.native
  let byte_range t = t.byte_range
  let utf16_range t = t.utf16_range
end

module Diagnostic_projection = struct
  module Index_key = struct
    module T = struct
      type t =
        { id : Captured.Id.t
        ; digest_hex : string
        }
      [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)

    let of_source source =
      { id = Captured.id source
      ; digest_hex = Captured.Digest.to_hex (Captured.digest source)
      }
    ;;
  end

  type t =
    { native : Diagnostic.t
    ; primary : Site_projection.t option
    ; related : (Diagnostic.Related.t * Site_projection.t) list
    ; sites_truncated : bool
    }

  type page =
    { items : t list
    ; truncated : bool
    }

  let native t = t.native
  let primary t = t.primary
  let related t = t.related
  let sites_truncated t = t.sites_truncated

  let project_list diagnostics ~policy ~checkpoint =
    let indexes = ref (Map.empty (module Index_key)) in
    let remaining_source_bytes = ref policy.Policy.max_projection_source_bytes in
    let remaining_index_entries = ref policy.Policy.max_position_index_entries in
    let remaining_sites = ref policy.Policy.max_projected_sites in
    let find_index source =
      let key = Index_key.of_source source in
      match Map.find !indexes key with
      | Some index -> index
      | None ->
        checkpoint ();
        let bytes = Captured.byte_length source in
        let index =
          if Map.length !indexes >= policy.max_projection_sources
          then Error Site_projection.Error.Projection_limit
          else if bytes > !remaining_source_bytes || !remaining_index_entries <= 0
          then Error Site_projection.Error.Projection_limit
          else (
            remaining_source_bytes := !remaining_source_bytes - bytes;
            match
              Position.create
                source
                ~max_index_entries:!remaining_index_entries
                ~checkpoint
            with
            | Ok index ->
              remaining_index_entries
              := !remaining_index_entries - Position.index_entry_count index;
              Ok index
            | Error Position.Error.Index_limit_exceeded ->
              Error Site_projection.Error.Projection_limit
            | Error error -> Error (Site_projection.Error.Position error))
        in
        if Map.length !indexes < policy.max_projection_sources
        then indexes := Map.set !indexes ~key ~data:index;
        index
    in
    let project_site native =
      let index = find_index (Diagnostic.Site.source native) in
      let convert encoding =
        Result.bind index ~f:(fun index ->
          Position.range_of_location
            index
            ~encoding
            (Diagnostic.Site.location native)
            ~checkpoint
          |> Result.map_error ~f:(fun error -> Site_projection.Error.Position error))
      in
      { Site_projection.native
      ; byte_range = convert Position.Encoding.Utf8_bytes
      ; utf16_range = convert Position.Encoding.Utf16_units
      }
    in
    let project_diagnostic (native : Diagnostic.t) =
      checkpoint ();
      let sites_truncated = ref false in
      let admit_site site =
        checkpoint ();
        if !remaining_sites <= 0
        then (
          sites_truncated := true;
          None)
        else (
          decr remaining_sites;
          Some (project_site site))
      in
      let primary = Option.bind native.site ~f:admit_site in
      let rec project_related reversed = function
        | [] -> List.rev reversed
        | _ :: _ when !remaining_sites <= 0 ->
          sites_truncated := true;
          List.rev reversed
        | related :: tail ->
          (match admit_site (Diagnostic.Related.site related) with
           | Some site -> project_related ((related, site) :: reversed) tail
           | None ->
             sites_truncated := true;
             List.rev reversed)
      in
      let related = project_related [] native.related in
      { native; primary; related; sites_truncated = !sites_truncated }
    in
    let rec take count reversed = function
      | [] -> { items = List.rev reversed; truncated = false }
      | _ :: _ when count >= policy.max_projected_diagnostics ->
        { items = List.rev reversed; truncated = true }
      | diagnostic :: tail ->
        take (count + 1) (project_diagnostic diagnostic :: reversed) tail
    in
    take 0 [] diagnostics
  ;;

  let project checked ~policy ~checkpoint =
    project_list (Checked.diagnostics checked) ~policy ~checkpoint
  ;;

  let project_native native ~policy ~checkpoint =
    let projected = project_list [ native ] ~policy ~checkpoint in
    List.hd_exn projected.items
  ;;
end
