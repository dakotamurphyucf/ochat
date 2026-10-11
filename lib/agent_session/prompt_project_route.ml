open Core
module CM = Prompt.Chat_markdown
module Owner = Prompt_source_owner
module Input = Chatmd_parser_input
module Captured = Chatml_source
module Path = Source.Portable_path

type t =
  { declaration : CM.Declaration.t
  ; original_prefix : string
  ; root : Chatml_project_prepare.Root.t
  ; environment : string option
  ; source_member : Path.t option
  ; root_member : Path.t option
  ; raw_source : string option
  }

let root t = t.root
let environment t = t.environment
let source_member t = t.source_member

let bytes_equal left right ~checkpoint =
  let length = String.length left in
  let equal = ref (Int.equal length (String.length right)) in
  let offset = ref 0 in
  while !equal && !offset < length do
    checkpoint ();
    let stop = !offset + Int.min 4096 (length - !offset) in
    while !equal && !offset < stop do
      equal := Char.equal left.[!offset] right.[!offset];
      incr offset
    done
  done;
  checkpoint ();
  !equal
;;

let create capture ~declaration ~max_locator_bytes ~max_path_components ~checkpoint =
  checkpoint ();
  let value = Owner.Capture.value capture in
  let contexts =
    Prompt_revision_builder.Capture.root value
    :: List.map (Prompt_revision_builder.Capture.agents value) ~f:snd
  in
  let owned =
    List.exists contexts ~f:(fun parsed ->
      checkpoint ();
      List.exists (CM.Parsed.declarations parsed) ~f:(fun candidate ->
        checkpoint ();
        phys_equal candidate declaration))
  in
  let open Or_error.Let_syntax in
  let%bind () =
    if not owned
    then Or_error.error_string "Project declaration belongs to another capture"
    else if max_locator_bytes < 0 || max_path_components <= 0
    then Or_error.error_string "Invalid project route limits"
    else Ok ()
  in
  let%bind selection =
    match CM.Declaration.project_selection declaration with
    | None -> Or_error.error_string "Script has no project selection"
    | Some selection -> Ok selection
  in
  let spelling = selection.Chatmd_shell_spec.Script_project.project in
  let%bind () =
    if String.length spelling > max_locator_bytes
    then Or_error.error_string "Project locator exceeds byte allowance"
    else Ok ()
  in
  let component_count = ref 1 in
  String.iteri spelling ~f:(fun i character ->
    if i mod 256 = 0 then checkpoint ();
    if Char.equal character '/' then incr component_count);
  let%bind () =
    if !component_count > max_path_components
    then Or_error.error_string "Project locator exceeds component allowance"
    else Ok ()
  in
  let input = Input.Range.input (CM.Declaration.range declaration) in
  let%bind route = Owner.Capture.route capture input ~checkpoint in
  let base = Owner.Capture.Route.member route in
  let%bind project = Path.create (Filename.basename spelling) in
  let%bind root =
    Owner.Capture.Route.local_root
      route
      ~locator:(Filename.dirname spelling)
      ~project
      ~max_locator_bytes
      ~checkpoint
  in
  let%bind source_member, raw_source =
    match CM.Declaration.script_path declaration with
    | None -> Ok (None, None)
    | Some reference ->
      let%bind member =
        Source_loader.resolve_member base ~reference ~max_reference_bytes:1024 ~checkpoint
      in
      let sources =
        let root_input =
          CM.Parsed.root_input (Prompt_revision_builder.Capture.root value)
        in
        (Input.path root_input, Input.authored root_input)
        :: Prompt_revision_builder.Capture.sources value
      in
      let%bind raw =
        List.find_map sources ~f:(fun (path, contents) ->
          checkpoint ();
          if String.equal path (Path.to_string member) then Some contents else None)
        |> Option.value_map
             ~default:(Or_error.error_string "External script missing from capture")
             ~f:Result.return
      in
      let%bind () =
        if bytes_equal raw (CM.Declaration.script_body declaration) ~checkpoint
        then Ok ()
        else
          Or_error.error_string
            "External script body differs from captured original member"
      in
      Ok (Some member, Some raw)
  in
  let root_member =
    match source_member with
    | None -> None
    | Some member ->
      (* A failed relative resolution grants no same-anchor affinity. Absolute or
         escaping projects still go through explicit host-root admission later. *)
      (match
         Source_loader.resolve_member
           base
           ~reference:spelling
           ~max_reference_bytes:1024
           ~checkpoint
       with
       | Error _ -> None
       | Ok manifest_member ->
         let project_directory = Filename.dirname (Path.to_string manifest_member) in
         let parts path =
           if String.equal path "." then [] else String.split path ~on:'/'
         in
         let rec remove_prefix prefix member =
           checkpoint ();
           match prefix, member with
           | [], remaining -> Some remaining
           | first :: rest, actual :: remaining when String.equal first actual ->
             remove_prefix rest remaining
           | _ :: _, [] | _ :: _, _ :: _ -> None
         in
         Option.bind
           (remove_prefix (parts project_directory) (parts (Path.to_string member)))
           ~f:(fun remaining ->
             Path.create (String.concat ~sep:"/" remaining) |> Result.ok))
  in
  checkpoint ();
  Ok
    { declaration
    ; original_prefix =
        Filename.dirname
          (Agent_store.Prompt_source_receipt.root_file (Owner.Capture.receipt capture))
    ; root
    ; environment = selection.environment
    ; source_member
    ; root_member
    ; raw_source
    }
;;

let entry t ~source ~additional_anchors ~root_limits ~checkpoint =
  let expected = CM.Declaration.script_body t.declaration in
  if not (bytes_equal expected (Captured.contents source) ~checkpoint)
  then Or_error.error_string "Supplied entry body differs from parser declaration"
  else
    let open Or_error.Let_syntax in
    let%bind () =
      match t.raw_source with
      | None -> Ok ()
      | Some raw ->
        if bytes_equal raw (Captured.contents source) ~checkpoint
        then Ok ()
        else Or_error.error_string "Supplied file body differs from original capture"
    in
    checkpoint ();
    match t.source_member with
    | None -> Ok (Chatml_project_prepare.Request.Entry.Captured_fragment source)
    | Some _ ->
      let%bind overridden =
        match t.root_member, t.root with
        | None, _ -> Ok false
        | Some _, Chatml_project_prepare.Root.Local { locator; project = _ } ->
          List.fold_result
            additional_anchors
            ~init:(0, false)
            ~f:(fun (count, overridden) anchor ->
              checkpoint ();
              if count >= Chatml_local_roots.Limits.anchors root_limits - 1
              then Or_error.error_string "Additional project anchors exceed allowance"
              else
                Chatml_local_roots.Anchor.may_override
                  anchor
                  ~original_prefix:t.original_prefix
                  ~locator
                  ~limits:root_limits
                  ~checkpoint
                |> Result.map_error ~f:(fun _ ->
                  Error.of_string "Invalid project anchor route")
                |> Result.map ~f:(fun candidate -> count + 1, overridden || candidate))
          |> Result.map ~f:snd
        | Some _, Git _ -> Or_error.error_string "Original capture cannot own a Git route"
      in
      Ok
        (Chatml_project_prepare.Request.Entry.Captured_file
           { source; root_member = (if overridden then None else t.root_member) })
;;
