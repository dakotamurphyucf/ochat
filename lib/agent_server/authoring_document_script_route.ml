open! Core
module CM = Prompt.Chat_markdown
module Input = Chatmd_parser_input
module Captured = Chatml_source
module Path = Source.Portable_path
module Request = Chatml_project_prepare.Request

module Error = struct
  type t =
    | Foreign_declaration
    | Invalid_project_route
    | Missing_external_source
    | Conflicting_external_source
    | Limit
  [@@deriving sexp_of]
end

type t =
  { source : Captured.t
  ; entry : Request.Entry.t
  ; environment : string option
  ; project_member : Path.t
  }

let source t = t.source
let entry t = t.entry
let environment t = t.environment
let project_member t = t.project_member

let equal_bytes left right ~checkpoint =
  let length = String.length left in
  if length <> String.length right
  then false
  else (
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
    !equal)
;;

let count_components spelling ~checkpoint =
  let count = ref 1 in
  String.iteri spelling ~f:(fun index character ->
    if index mod 256 = 0 then checkpoint ();
    if Char.equal character '/' then incr count);
  !count
;;

let project_relative project_member member ~checkpoint =
  let directory = Filename.dirname (Path.to_string project_member) in
  let components path = if String.equal path "." then [] else String.split path ~on:'/' in
  let rec drop directory member =
    checkpoint ();
    match directory, member with
    | [], remaining ->
      if List.is_empty remaining
      then None
      else Path.create (String.concat ~sep:"/" remaining) |> Result.ok
    | head :: rest, actual :: remaining when String.equal head actual ->
      drop rest remaining
    | _ -> None
  in
  drop (components directory) (components (Path.to_string member))
;;

let admit
      ~parsed
      ~input_sources
      ~bundle
      ~declaration
      ~project_member
      ~root_source
      ~max_script_bytes
      ~max_locator_bytes
      ~max_path_components
      ~checkpoint
  =
  let open Result.Let_syntax in
  if
    max_script_bytes <= 0
    || max_script_bytes > Chatmd_source_bundle.default_limits.max_source_bytes
    || max_locator_bytes <= 0
    || max_locator_bytes > 1024
    || max_path_components <= 0
    || max_path_components > 1024
  then Error Error.Limit
  else (
    checkpoint ();
    let body = CM.Declaration.script_body declaration in
    if String.length body > max_script_bytes
    then Error Error.Limit
    else (
      let namespace = Captured.Id.package_namespace (Captured.id root_source) in
      let%bind input, actual_project, selection =
        Authoring_project_declaration.admit
          ~parsed
          ~input_sources
          ~declaration
          ~package_namespace:namespace
          ~max_locator_bytes
          ~max_path_components
          ~checkpoint
        |> Result.map_error ~f:(function
          | Foreign_input -> Error.Foreign_declaration
          | Unavailable_route -> Error.Invalid_project_route
          | Limit -> Error.Limit)
      in
      if not (Path.equal actual_project project_member)
      then Error Error.Invalid_project_route
      else (
        let%bind relative_path, external_member =
          match CM.Declaration.script_path declaration with
          | None ->
            let range = CM.Declaration.range declaration in
            Ok
              ( Printf.sprintf
                  "fragments/%s/%d-%d.chatml"
                  (Input.identity_digest input ~checkpoint)
                  (Input.Range.start_byte range)
                  (Input.Range.end_byte range)
              , None )
          | Some reference ->
            if
              String.length reference > max_locator_bytes
              || count_components reference ~checkpoint > max_path_components
            then Error Error.Limit
            else (
              let%bind base =
                Path.create (Input.path input)
                |> Result.map_error ~f:(fun _ -> Error.Foreign_declaration)
              in
              let%map member =
                Source_loader.resolve_member
                  base
                  ~reference
                  ~max_reference_bytes:max_locator_bytes
                  ~checkpoint
                |> Result.map_error ~f:(fun _ -> Error.Invalid_project_route)
              in
              Path.to_string member, Some member)
        in
        let%bind () =
          if count_components relative_path ~checkpoint > max_path_components
          then Error Error.Limit
          else Ok ()
        in
        let%bind () =
          match external_member with
          | None ->
            if
              List.exists (Chatmd_source_bundle.sources bundle) ~f:(fun (member, _) ->
                checkpoint ();
                String.equal member relative_path)
            then Error Error.Conflicting_external_source
            else Ok ()
          | Some _ ->
            let matches =
              List.filter (Chatmd_source_bundle.sources bundle) ~f:(fun (member, _) ->
                checkpoint ();
                String.equal member relative_path)
            in
            (match matches with
             | [] -> Error Error.Missing_external_source
             | [ (_, bytes) ] ->
               if equal_bytes bytes body ~checkpoint
               then Ok ()
               else Error Error.Conflicting_external_source
             | _ -> Error Error.Conflicting_external_source)
        in
        let%bind id =
          Captured.Id.create
            ~package_namespace:
              (if Option.is_some external_member
               then namespace
               else
                 namespace
                 ^ ".fragment."
                 ^ Captured.Digest.to_hex (Captured.digest root_source))
            ~relative_path
          |> Result.map_error ~f:(fun _ -> Error.Invalid_project_route)
        in
        let%bind () =
          if
            Option.is_none external_member
            && (Captured.Id.equal id (Captured.id root_source)
                || List.exists input_sources ~f:(fun (_, source) ->
                  checkpoint ();
                  Captured.Id.equal id (Captured.id source)))
          then Error Error.Invalid_project_route
          else Ok ()
        in
        let%map source =
          Captured.create
            id
            ~contents:body
            ~expected_digest:None
            ~max_bytes:(Some max_script_bytes)
            ~checkpoint
          |> Result.map_error ~f:(fun _ -> Error.Limit)
        in
        let entry =
          match external_member with
          | None -> Request.Entry.Captured_fragment source
          | Some member ->
            Request.Entry.Captured_file
              { source; root_member = project_relative project_member member ~checkpoint }
        in
        { source; entry; environment = selection.environment; project_member })))
;;
