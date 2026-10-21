open! Core
module Rows = Authoring_document_selection_rows
module Sources = Authoring_source_host
module Markdown = Prompt.Chat_markdown
module Captured = Chatml_source
module Input = Chatmd_parser_input
module Route = Authoring_document_script_route
module Path = Source.Portable_path
module Selection = Authoring_script_selection
module Entry = Chatml_project_prepare.Request.Entry

module Error = struct
  type t =
    | Foreign_owner_or_imports
    | Changed_route_or_environment
    | Ambiguous_declaration
    | No_validated_build
end

type t =
  { declaration : Markdown.Declaration.t
  ; selected : Selection.Selected.t
  ; parsed : Markdown.Parsed.t
  ; host_source : Sources.Document_context.Script_capture.t option
  ; original_parsed : Markdown.Parsed.t
  ; original_capture : Sources.Document_context.Parsed_capture.t
  ; original_declaration : Markdown.Declaration.t
  ; original_metadata : Sources.Document_context.Project_capture.t option
  ; updated_capture : Sources.Document_context.Parsed_capture.t
  }

let declaration t = t.declaration
let selected t = t.selected
let parsed t = t.parsed
let host_source t = t.host_source
let original_parsed t = t.original_parsed
let original_capture t = t.original_capture
let original_declaration t = t.original_declaration
let original_metadata t = t.original_metadata
let updated_capture t = t.updated_capture

let same_bytes left right ~checkpoint =
  let length = String.length left in
  if length <> String.length right
  then false
  else (
    let offset = ref 0 in
    let same = ref true in
    while !same && !offset < length do
      checkpoint ();
      let stop = !offset + Int.min 4096 (length - !offset) in
      while !same && !offset < stop do
        same := Char.equal left.[!offset] right.[!offset];
        incr offset
      done
    done;
    !same)
;;

let same_source left right ~checkpoint =
  checkpoint ();
  Captured.Id.equal (Captured.id left) (Captured.id right)
  && Captured.Digest.equal (Captured.digest left) (Captured.digest right)
  && Int.equal (Captured.byte_length left) (Captured.byte_length right)
  && same_bytes (Captured.contents left) (Captured.contents right) ~checkpoint
;;

let same_imports previous updated ~checkpoint =
  let old_bundle = Sources.Document_context.Parsed_capture.bundle previous in
  let new_bundle = Sources.Document_context.Parsed_capture.bundle updated in
  let old_root = Chatmd_source_bundle.root_file old_bundle in
  let new_root = Chatmd_source_bundle.root_file new_bundle in
  let imports bundle root =
    Chatmd_source_bundle.sources bundle
    |> List.filter ~f:(fun (member, _) ->
      checkpoint ();
      not (String.equal member root))
  in
  let rec compare old_imports new_imports =
    match old_imports, new_imports with
    | [], [] -> true
    | (old_member, old_bytes) :: old_tail, (new_member, new_bytes) :: new_tail ->
      checkpoint ();
      String.equal old_member new_member
      && same_bytes old_bytes new_bytes ~checkpoint
      && compare old_tail new_tail
    | [], _ :: _ | _ :: _, [] -> false
  in
  String.equal old_root new_root
  && compare (imports old_bundle old_root) (imports new_bundle new_root)
;;

let same_declaring_owner old_source new_source ~old_root ~current_source ~checkpoint =
  if Captured.Id.equal (Captured.id old_source) (Captured.id old_root)
  then
    Captured.Id.equal (Captured.id new_source) (Captured.id current_source)
    && same_source new_source current_source ~checkpoint
  else same_source old_source new_source ~checkpoint
;;

let source_for_declaration capture declaration ~checkpoint =
  let input = Input.Range.input (Markdown.Declaration.range declaration) in
  let rec find found = function
    | [] -> found
    | (candidate, source) :: tail ->
      checkpoint ();
      if phys_equal candidate input
      then (
        match found with
        | `Missing -> find (`Found source) tail
        | `Found _ | `Duplicate -> `Duplicate)
      else find found tail
  in
  find `Missing (Sources.Document_context.Parsed_capture.input_sources capture)
;;

let same_source_ref owner updated =
  let old = Markdown.Declaration.source_ref owner in
  let fresh = Markdown.Declaration.source_ref updated in
  String.equal old.file fresh.file
  && String.equal old.source_dir fresh.source_dir
  && String.equal old.prompt_dir fresh.prompt_dir
  && Option.equal String.equal old.namespace fresh.namespace
;;

let same_route
      old
      new_
      ~old_capture
      ~updated
      ~metadata
      ~max_script_bytes
      ~max_locator_bytes
      ~max_path_components
      ~checkpoint
  =
  let old_root = Sources.Document_context.Parsed_capture.source old_capture in
  let new_root = Sources.Document_context.Parsed_capture.source updated in
  let old_receipt = Sources.Document_context.Parsed_capture.parsed old_capture in
  let new_receipt = Sources.Document_context.Parsed_capture.parsed updated in
  let old_sources = Sources.Document_context.Parsed_capture.input_sources old_capture in
  let new_sources = Sources.Document_context.Parsed_capture.input_sources updated in
  let old_bundle = Sources.Document_context.Parsed_capture.bundle old_capture in
  let new_bundle = Sources.Document_context.Parsed_capture.bundle updated in
  let old_namespace = Captured.Id.package_namespace (Captured.id old_root) in
  let new_namespace = Captured.Id.package_namespace (Captured.id new_root) in
  let selected_project declaration receipt sources namespace =
    match Markdown.Declaration.project_selection declaration with
    | None -> Ok None
    | Some _ ->
      Authoring_project_declaration.admit
        ~parsed:receipt
        ~input_sources:sources
        ~declaration
        ~package_namespace:namespace
        ~max_locator_bytes
        ~max_path_components
        ~checkpoint
      |> Result.map ~f:(fun (_, member, selection) -> Some (member, selection))
  in
  let old_project = selected_project old old_receipt old_sources old_namespace in
  let new_project = selected_project new_ new_receipt new_sources new_namespace in
  match old_project, new_project with
  | Ok old_project, Ok new_project ->
    let same_project =
      Option.equal
        (fun (old_member, old_selection) (new_member, new_selection) ->
           Path.equal old_member new_member
           && Chatmd_shell_spec.Script_project.equal old_selection new_selection)
        old_project
        new_project
    in
    if not same_project
    then false
    else (
      match metadata with
      | None -> true
      | Some metadata ->
        let member = Sources.Document_context.Project_capture.member metadata in
        let route receipt sources bundle declaration root_source =
          Route.admit
            ~parsed:receipt
            ~input_sources:sources
            ~bundle
            ~declaration
            ~project_member:member
            ~root_source
            ~max_script_bytes
            ~max_locator_bytes
            ~max_path_components
            ~checkpoint
        in
        (match
           ( route old_receipt old_sources old_bundle old old_root
           , route new_receipt new_sources new_bundle new_ new_root )
         with
         | Ok before, Ok after ->
           Option.equal String.equal (Route.environment before) (Route.environment after)
           && Path.equal (Route.project_member before) (Route.project_member after)
           &&
             (match Route.entry before, Route.entry after with
             | Entry.Captured_fragment _, Entry.Captured_fragment _ -> true
             | Entry.Captured_file old_file, Entry.Captured_file new_file ->
               Option.equal Path.equal old_file.root_member new_file.root_member
               && same_source old_file.source new_file.source ~checkpoint
             | Entry.Declared, Entry.Declared -> true
             | Entry.Captured_fragment _, (Entry.Captured_file _ | Entry.Declared)
             | Entry.Captured_file _, (Entry.Captured_fragment _ | Entry.Declared)
             | Entry.Declared, (Entry.Captured_fragment _ | Entry.Captured_file _) ->
               false)
         | Error _, _ | _, Error _ -> false))
  | Error _, _ | _, Error _ -> false
;;

let prove
      ~old_capture
      ~rows
      ~old
      ~updated
      ~current_source
      ~script_id
      ~max_script_bytes
      ~max_locator_bytes
      ~max_path_components
      ~checkpoint
  =
  let old_declaration = Rows.Row.declaration old in
  let original_parsed = Rows.Row.parsed old in
  let original_metadata = Rows.Row.metadata old in
  let physical_member =
    match
      Rows.find rows ~capture:old_capture ~declaration:old_declaration ~checkpoint
    with
    | Ok (Some member) -> phys_equal member old
    | Ok None | Error _ -> false
  in
  let old_source = Sources.Document_context.Parsed_capture.source old_capture in
  if
    (not physical_member)
    || (not (same_imports old_capture updated ~checkpoint))
    || (not (Captured.Id.equal (Captured.id old_source) (Captured.id current_source)))
    || not
         (same_source
            current_source
            (Sources.Document_context.Parsed_capture.source updated)
            ~checkpoint)
  then Error Error.Foreign_owner_or_imports
  else (
    let parsed =
      (Markdown.Generated.parsed (Sources.Document_context.Parsed_capture.parsed updated))
        .parsed_root
    in
    let matches =
      Markdown.Parsed.declarations parsed
      |> List.filter ~f:(fun declaration ->
        checkpoint ();
        String.equal (Markdown.Declaration.script_id declaration) script_id)
    in
    match matches with
    | [ declaration ] ->
      (match source_for_declaration updated declaration ~checkpoint with
       | `Missing | `Duplicate -> Error Error.Foreign_owner_or_imports
       | `Found declaring_source ->
         let old_declaring_source = Rows.Row.declaring_source old in
         if
           (not (String.equal (Markdown.Declaration.script_id old_declaration) script_id))
           || (not
                 (Markdown.Declaration.Surface.equal
                    (Markdown.Declaration.surface old_declaration)
                    (Markdown.Declaration.surface declaration)))
           || (not (same_source_ref old_declaration declaration))
           || (not
                 (Option.equal
                    String.equal
                    (Markdown.Declaration.script_path old_declaration)
                    (Markdown.Declaration.script_path declaration)))
           || (not
                 (Option.equal
                    Chatmd_shell_spec.Script_project.equal
                    (Markdown.Declaration.project_selection old_declaration)
                    (Markdown.Declaration.project_selection declaration)))
           || (not
                 (same_declaring_owner
                    old_declaring_source
                    declaring_source
                    ~old_root:old_source
                    ~current_source
                    ~checkpoint))
           || (Option.is_some (Markdown.Declaration.script_path declaration)
               && not
                    (String.equal
                       (Markdown.Declaration.script_body old_declaration)
                       (Markdown.Declaration.script_body declaration)))
           || not
                (same_route
                   old_declaration
                   declaration
                   ~old_capture
                   ~updated
                   ~metadata:original_metadata
                   ~max_script_bytes
                   ~max_locator_bytes
                   ~max_path_components
                   ~checkpoint)
         then Error Error.Changed_route_or_environment
         else (
           match Rows.Row.status old with
           | No_validated_build _ -> Error Error.No_validated_build
           | Independently_selected selected ->
             Ok
               { declaration
               ; selected
               ; parsed
               ; host_source = None
               ; original_parsed
               ; original_capture = old_capture
               ; original_declaration = old_declaration
               ; original_metadata
               ; updated_capture = updated
               }
           | Host_prepared { selected; source } ->
             Ok
               { declaration
               ; selected
               ; parsed
               ; host_source = Some source
               ; original_parsed
               ; original_capture = old_capture
               ; original_declaration = old_declaration
               ; original_metadata
               ; updated_capture = updated
               }))
    | [] | _ :: _ :: _ -> Error Error.Ambiguous_declaration)
;;
