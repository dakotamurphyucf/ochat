open! Core
module C = Developer_codec

let ( let* ) result f = Result.bind result ~f
let get fields name decode = C.required fields name |> Result.bind ~f:decode

let option fields name decode =
  let* value = C.optional fields name in
  Option.value_map value ~default:(Ok None) ~f:(fun value ->
    Result.map (decode value) ~f:Option.some)
;;

let path json =
  let* value = C.string ~max_bytes:1024 json in
  Source.Portable_path.create value
  |> Result.map_error ~f:(fun _ ->
    Protocol_error.invalid_request "invalid portable developer path")
;;

module Route = struct
  type t =
    | Local_project of
        { root_index : int
        ; dependency_root_indices : int list
        ; project : Source.Portable_path.t
        ; selection : Chatml_project.Selection.request
        }
    | Local_source of
        { root_index : int
        ; member : Source.Portable_path.t
        }
    | Authoring_project of { expected : Authoring.Context.Expected.t }
    | Document_script of
        { expected : Authoring.Context.Expected.t
        ; selector : Authoring.Analysis.Target.selector
        }

  let selection_json = function
    | Chatml_project.Selection.Entry { name; environment } ->
      `Object
        ([ "kind", `String "entry"; "name", `String name ]
         @ C.optional_field "environment" environment (fun x -> `String x))
    | Library { environment } ->
      `Object
        ([ "kind", `String "library" ]
         @ C.optional_field "environment" environment (fun x -> `String x))
  ;;

  let to_json = function
    | Local_project { root_index; dependency_root_indices; project; selection } ->
      `Object
        [ "kind", `String "local_project"
        ; "root_index", C.integer_json root_index
        ; ( "dependency_root_indices"
          , `Array (List.map dependency_root_indices ~f:C.integer_json) )
        ; "project", `String (Source.Portable_path.to_string project)
        ; "selection", selection_json selection
        ]
    | Local_source { root_index; member } ->
      `Object
        [ "kind", `String "local_source"
        ; "root_index", C.integer_json root_index
        ; "member", `String (Source.Portable_path.to_string member)
        ]
    | Authoring_project { expected } ->
      `Object
        [ "kind", `String "authoring_project"
        ; "expected", Authoring.Context.Expected.to_json expected
        ]
    | Document_script { expected; selector } ->
      `Object
        [ "kind", `String "document_script"
        ; "expected", Authoring.Context.Expected.to_json expected
        ; "selector", Authoring.Analysis.Target.selector_to_json selector
        ]
  ;;

  let bound ~limits value =
    let* () = C.validate ~limits (to_json value) in
    Ok value
  ;;

  let check_root root_index =
    if root_index >= 0 && root_index < 32
    then Ok ()
    else C.invalid "invalid developer root index"
  ;;

  let check_selection = function
    | Chatml_project.Selection.Entry { name; environment } ->
      (not (String.is_empty name))
      && String.length name <= 1024
      && Option.for_all environment ~f:(fun x -> String.length x <= 1024)
    | Library { environment } ->
      Option.for_all environment ~f:(fun x -> String.length x <= 1024)
  ;;

  let local_project ~limits ~root_index ~dependency_root_indices ~project ~selection =
    let* () = check_root root_index in
    if
      List.length dependency_root_indices >= 32
      || List.contains_dup dependency_root_indices ~compare:Int.compare
      || List.exists dependency_root_indices ~f:(fun x ->
        x = root_index || x < 0 || x >= 32)
      || not (check_selection selection)
    then C.invalid "invalid developer project route"
    else
      bound
        ~limits
        (Local_project { root_index; dependency_root_indices; project; selection })
  ;;

  let local_source ~limits ~root_index ~member =
    let* () = check_root root_index in
    bound ~limits (Local_source { root_index; member })
  ;;

  let authoring_project ~limits ~expected = bound ~limits (Authoring_project { expected })

  let document_script ~limits ~expected ~selector =
    bound ~limits (Document_script { expected; selector })
  ;;

  let selection_of_json json =
    let* fields = C.fields ~allowed:[ "kind"; "name"; "environment" ] json in
    let* kind = get fields "kind" (C.string ~max_bytes:16) in
    let* environment = option fields "environment" (C.string ~max_bytes:1024) in
    match kind with
    | "entry" ->
      let* name = get fields "name" (C.string ~max_bytes:1024) in
      let* () =
        C.fields ~allowed:[ "kind"; "name"; "environment" ] json |> Result.map ~f:ignore
      in
      Ok (Chatml_project.Selection.Entry { name; environment })
    | "library" ->
      let* () =
        C.fields ~allowed:[ "kind"; "environment" ] json |> Result.map ~f:ignore
      in
      Ok (Chatml_project.Selection.Library { environment })
    | _ -> C.invalid "unknown developer selection"
  ;;

  let of_json ~limits json =
    let* () = C.validate ~limits json in
    let* fields =
      C.fields
        ~allowed:
          [ "kind"
          ; "root_index"
          ; "dependency_root_indices"
          ; "project"
          ; "selection"
          ; "member"
          ; "expected"
          ; "selector"
          ]
        json
    in
    let* kind = get fields "kind" (C.string ~max_bytes:24) in
    let exact allowed = C.fields ~allowed json |> Result.map ~f:ignore in
    match kind with
    | "local_project" ->
      let* () =
        exact [ "kind"; "root_index"; "dependency_root_indices"; "project"; "selection" ]
      in
      let* root_index = get fields "root_index" C.nonnegative in
      let* dependency_root_indices =
        get fields "dependency_root_indices" (C.array ~max_items:32 C.nonnegative)
      in
      let* project = get fields "project" path in
      let* selection = get fields "selection" selection_of_json in
      local_project ~limits ~root_index ~dependency_root_indices ~project ~selection
    | "local_source" ->
      let* () = exact [ "kind"; "root_index"; "member" ] in
      let* root_index = get fields "root_index" C.nonnegative in
      let* member = get fields "member" path in
      local_source ~limits ~root_index ~member
    | "authoring_project" ->
      let* () = exact [ "kind"; "expected" ] in
      let* expected = get fields "expected" Authoring.Context.Expected.of_json in
      authoring_project ~limits ~expected
    | "document_script" ->
      let* () = exact [ "kind"; "expected"; "selector" ] in
      let* expected = get fields "expected" Authoring.Context.Expected.of_json in
      let* selector = get fields "selector" Authoring.Analysis.Target.selector_of_json in
      document_script ~limits ~expected ~selector
    | _ -> C.invalid "unknown developer route"
  ;;
end
