open! Core
module A = Authoring_common
module C = Authoring_codec
module Context = Authoring_context.Context
module Captured = Chatml_source
module Project = Chatml_project
module Path = Source.Portable_path

let ( let* ) value f = Result.bind value ~f
let obj fields = `Object fields
let str value = `String value
let invalid = C.invalid

let bounded_count ~max values =
  let rec loop left = function
    | [] -> true
    | _ :: rest when left > 0 -> loop (left - 1) rest
    | _ :: _ -> false
  in
  loop max values
;;

let fields json allowed =
  match json with
  | `Object entries
    when List.exists entries ~f:(fun (name, _) ->
           not (List.mem allowed name ~equal:String.equal)) ->
    invalid "unknown bundle basis field"
  | _ -> Json_codec.fields json
;;

let required json name decode =
  let* value = Json_codec.required json name in
  decode value
;;

let optional json name decode =
  match Json_codec.optional json name with
  | None -> Ok None
  | Some `Null -> invalid "null bundle basis field"
  | Some value -> Result.map (decode value) ~f:Option.some
;;

let path_json path = str (Path.to_string path)

let path_of_json json =
  let* value = C.portable_member json in
  Path.create value
  |> Result.map_error ~f:(fun _ -> Protocol_error.invalid_request "invalid bundle member")
;;

let immutable_source source =
  let* () = C.validate_source_id (A.Source.id source) in
  if Option.is_some (A.Source.editor_revision source)
  then invalid "immutable bundle source has editor revision"
  else if A.Source.byte_length source > 32 * 1024 * 1024
  then invalid "immutable bundle source is too large"
  else Ok ()
;;

let origin_equal left right =
  match left, right with
  | Project.Lock.Catalog.Local, Project.Lock.Catalog.Local -> true
  | Project.Lock.Catalog.Git left, Project.Lock.Catalog.Git right ->
    String.equal left.repository right.repository
    && Project.Git_pin.equal left.pin right.pin
    && Option.equal Path.equal left.package_root right.package_root
  | Project.Lock.Catalog.Local, Project.Lock.Catalog.Git _
  | Project.Lock.Catalog.Git _, Project.Lock.Catalog.Local -> false
;;

let origin_to_json = function
  | Project.Lock.Catalog.Local -> obj [ "kind", str "local" ]
  | Project.Lock.Catalog.Git { repository; pin; package_root } ->
    obj
      ([ "kind", str "git"
       ; "repository", str repository
       ; ( "object_format"
         , str
             (match Project.Git_pin.object_format pin with
              | Project.Git_pin.Sha1 -> "sha1"
              | Project.Git_pin.Sha256 -> "sha256") )
       ; "commit", str (Project.Git_pin.commit pin)
       ]
       @ C.optional_field "package_root" package_root path_json)
;;

let origin_of_json json =
  let* generic =
    fields json [ "kind"; "repository"; "object_format"; "commit"; "package_root" ]
  in
  let* kind = required generic "kind" (C.string ~max_bytes:8) in
  match kind with
  | "local" ->
    let* _ = fields json [ "kind" ] in
    Ok Project.Lock.Catalog.Local
  | "git" ->
    let* repository = required generic "repository" (C.utf8_text ~max_bytes:1024) in
    let* format = required generic "object_format" (C.string ~max_bytes:8) in
    let* format =
      match format with
      | "sha1" -> Ok Project.Git_pin.Sha1
      | "sha256" -> Ok Project.Git_pin.Sha256
      | _ -> invalid "invalid bundle Git object format"
    in
    let* commit = required generic "commit" (C.string ~max_bytes:64) in
    let* pin =
      Project.Git_pin.create ~object_format:format ~commit
      |> Result.map_error ~f:(fun _ ->
        Protocol_error.invalid_request "invalid bundle Git pin")
    in
    let* package_root = optional generic "package_root" path_of_json in
    Ok (Project.Lock.Catalog.Git { repository; pin; package_root })
  | _ -> invalid "invalid bundle origin kind"
;;
