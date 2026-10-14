open! Core
module CM = Prompt.Chat_markdown
module Input = Chatmd_parser_input
module Captured = Chatml_source
module Path = Source.Portable_path

module Error = struct
  type t =
    | Foreign_input
    | Unavailable_route
    | Limit
  [@@deriving sexp_of]
end

let count_components path ~checkpoint =
  let components = ref 1 in
  String.iteri path ~f:(fun index character ->
    if index mod 256 = 0 then checkpoint ();
    if Char.equal character '/' then incr components);
  !components
;;

let admit
      ~parsed
      ~input_sources
      ~declaration
      ~package_namespace
      ~max_locator_bytes
      ~max_path_components
      ~checkpoint
  =
  let open Result.Let_syntax in
  if
    max_locator_bytes <= 0
    || max_locator_bytes > 1024
    || max_path_components <= 0
    || max_path_components > 1024
  then Error Error.Limit
  else (
    let generated = CM.Generated.parsed parsed in
    let contains unit =
      checkpoint ();
      List.exists (CM.Parsed.declarations unit) ~f:(fun candidate ->
        checkpoint ();
        phys_equal candidate declaration)
    in
    let belongs =
      contains generated.parsed_root
      || List.exists generated.parsed_agents ~f:(fun (_, unit) ->
        checkpoint ();
        contains unit)
    in
    if not belongs
    then Error Error.Foreign_input
    else (
      let input = Input.Range.input (CM.Declaration.range declaration) in
      let source =
        List.find_map input_sources ~f:(fun (candidate, source) ->
          checkpoint ();
          if phys_equal candidate input then Some source else None)
      in
      match source with
      | None -> Error Error.Foreign_input
      | Some source ->
        let%bind expected_id =
          Captured.Id.create ~package_namespace ~relative_path:(Input.path input)
          |> Result.map_error ~f:(fun _ -> Error.Foreign_input)
        in
        if
          (not (Captured.Id.equal (Captured.id source) expected_id))
          || not (Input.authored_matches input (Captured.contents source) ~checkpoint)
        then Error Error.Foreign_input
        else (
          let%bind selection =
            CM.Declaration.project_selection declaration
            |> Result.of_option ~error:Error.Unavailable_route
          in
          let spelling = selection.Chatmd_shell_spec.Script_project.project in
          if String.length spelling > max_locator_bytes
          then Error Error.Limit
          else if count_components spelling ~checkpoint > max_path_components
          then Error Error.Limit
          else (
            let%bind base =
              Path.create (Input.path input)
              |> Result.map_error ~f:(fun _ -> Error.Foreign_input)
            in
            let%bind member =
              Source_loader.resolve_member
                base
                ~reference:spelling
                ~max_reference_bytes:max_locator_bytes
                ~checkpoint
              |> Result.map_error ~f:(fun _ -> Error.Unavailable_route)
            in
            if count_components (Path.to_string member) ~checkpoint > max_path_components
            then Error Error.Limit
            else Ok (input, member, selection)))))
;;

let identity ~source ~input ~declaration ~selection ~max_identity_bytes ~checkpoint =
  let range = CM.Declaration.range declaration in
  let id = Captured.id source in
  let values =
    [ Captured.Id.package_namespace id
    ; Captured.Id.relative_path id
    ; Captured.Digest.to_hex (Captured.digest source)
    ; Int.to_string (Captured.byte_length source)
    ; Input.identity_digest input ~checkpoint
    ; Int.to_string (Input.Range.start_byte range)
    ; Int.to_string (Input.Range.end_byte range)
    ; CM.Declaration.script_id declaration
    ; selection.Chatmd_shell_spec.Script_project.project
    ; Option.value selection.environment ~default:""
    ]
  in
  let remaining = ref max_identity_bytes in
  if
    max_identity_bytes <= 0
    || max_identity_bytes > 1024 * 1024
    || List.exists values ~f:(fun value ->
      checkpoint ();
      let length = String.length value in
      if length > !remaining - 16
      then true
      else (
        remaining := !remaining - length - 16;
        false))
  then Error Error.Limit
  else (
    let feed context value =
      checkpoint ();
      let context =
        Digestif.SHA256.feed_string context (Int.to_string (String.length value))
      in
      let context = Digestif.SHA256.feed_string context ":" in
      Digestif.SHA256.feed_string context value
    in
    let context =
      List.fold
        values
        ~init:
          (Digestif.SHA256.feed_string Digestif.SHA256.empty "ochat-project-capture-v1")
        ~f:feed
    in
    Digestif.SHA256.(get context |> to_hex)
    |> Captured.Digest.of_hex
    |> Result.map_error ~f:(fun _ -> Error.Limit))
;;

let matches_project_contents ~source ~build ~max_compare_bytes ~checkpoint =
  if max_compare_bytes <= 0 || max_compare_bytes > 64 * 1024 * 1024
  then Error Error.Limit
  else
    let module Build = Chatml_project_build in
    let module Prepared = Chatml_prepared_program in
    let module Lock = Chatml_project.Lock in
    let prepared = Build.prepared build in
    let root_id = Lock.root (Build.lock build) in
    match Prepared.provenance prepared with
    | Implicit -> Ok false
    | Project { declaration; _ } when not (Captured.Id.equal declaration root_id) ->
      Ok false
    | Project _ ->
      (* Prepared.find_source also scans this list, but does not poll. Keep the
         lookup within the caller's cooperative comparison budget. *)
      (match
         List.find (Prepared.sources prepared) ~f:(fun candidate ->
           checkpoint ();
           Captured.Id.equal (Captured.id candidate) root_id)
       with
       | None -> Ok false
       | Some root ->
         let length = Captured.byte_length source in
         if length > max_compare_bytes || Captured.byte_length root > max_compare_bytes
         then Error Error.Limit
         else if
           (not (Int.equal length (Captured.byte_length root)))
           || not (Captured.Digest.equal (Captured.digest source) (Captured.digest root))
         then Ok false
         else (
           let left = Captured.contents source in
           let right = Captured.contents root in
           let equal = ref true in
           let offset = ref 0 in
           while !equal && !offset < length do
             checkpoint ();
             let stop = !offset + Int.min 4096 (length - !offset) in
             while !equal && !offset < stop do
               equal := Char.equal left.[!offset] right.[!offset];
               incr offset
             done
           done;
           Ok !equal))
;;
