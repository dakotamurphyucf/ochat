open Core
module M = Chatmd_shell_spec.Authoring_metadata
module S = Chatmd_shell_spec.Tool_schema
module C = Authoring_corpus

type t =
  { source_file : string
  ; contents : string
  }
[@@deriving compare, equal, sexp]

let string = `Object [ "type", `String "string" ]
let array items = `Object [ "type", `String "array"; "items", items ]
let strings = array string

let object_ properties =
  `Object
    [ "type", `String "object"
    ; "properties", `Object properties
    ; "required", `Array (List.map properties ~f:(fun (key, _) -> `String key))
    ; "additionalProperties", `False
    ]
;;

let version = `Object [ "const", `Number "1" ]

let tasks =
  M.[ One_off_script; Standalone_tool; Moderator_tool; Child_agent; Background_workflow ]
;;

let helpers = M.[ Reference; Validation ]
let enum names = `Object [ "enum", `Array (List.map names ~f:(fun name -> `String name)) ]

let schema =
  let help =
    object_
      [ "version", version
      ; "package", string
      ; "tasks", array (enum (List.map tasks ~f:M.task_id))
      ; "topics", strings
      ; "required_helpers", array (enum (List.map helpers ~f:M.helper_name))
      ]
  in
  let topic =
    object_
      [ "id", string
      ; "title", string
      ; "prerequisites", strings
      ; "surfaces", strings
      ; "source_name", string
      ; "text", string
      ]
  in
  object_
    [ "version", version
    ; "packages", array (object_ [ "help", help; "topics", array topic ])
    ]
  |> S.compile
  |> Result.map_error ~f:(fun errors ->
    Sexp.to_string_hum [%sexp (errors : S.diagnostic list)])
  |> Result.ok_or_failwith
;;

let diagnostics errors =
  List.map errors ~f:(fun error ->
    String.concat ~sep:"." error.S.path ^ ": " ^ error.message)
  |> String.concat ~sep:"; "
;;

let decode contents =
  let open Result.Let_syntax in
  let%bind json = S.parse_json contents |> Result.map_error ~f:diagnostics in
  let%map () = S.validate schema json |> Result.map_error ~f:diagnostics in
  let member name json = Jsonaf.member_exn name json in
  let text name json = member name json |> Jsonaf.string_exn in
  let texts name json =
    member name json |> Jsonaf.list_exn |> List.map ~f:Jsonaf.string_exn
  in
  member "packages" json
  |> Jsonaf.list_exn
  |> List.map ~f:(fun package ->
    { C.help =
        (let help = member "help" package in
         { M.version = 1
         ; package = text "package" help
         ; tasks =
             texts "tasks" help
             |> List.map ~f:(fun id ->
               List.find_exn tasks ~f:(fun task -> String.equal id (M.task_id task)))
         ; topics = texts "topics" help
         ; required_helpers =
             texts "required_helpers" help
             |> List.map ~f:(fun id ->
               List.find_exn helpers ~f:(fun helper ->
                 String.equal id (M.helper_name helper)))
         })
    ; topics =
        member "topics" package
        |> Jsonaf.list_exn
        |> List.map ~f:(fun topic ->
          { C.id = text "id" topic
          ; title = text "title" topic
          ; prerequisites = texts "prerequisites" topic
          ; surfaces = texts "surfaces" topic
          ; source_name = text "source_name" topic
          ; text = text "text" topic
          })
    })
;;

let of_string ~source_file contents =
  Result.map (decode contents) ~f:(fun _ -> { source_file; contents })
;;

let load ~env ~path =
  match Filename.is_absolute path with
  | false -> Error "authoring package file path must be absolute"
  | true ->
    (match
       Eio.Path.with_open_in
         Eio.Path.(Eio.Stdenv.fs env / path)
         (fun flow ->
            Eio.Buf_read.parse_exn
              ~max_size:((1024 * 1024) + 1)
              Eio.Buf_read.take_all
              flow)
     with
     | contents -> of_string ~source_file:path contents
     | exception (Eio.Cancel.Cancelled _ as error) -> raise error
     | exception error ->
       Error ("cannot read authoring package file: " ^ Exn.to_string error))
;;

let packages files =
  let open Result.Let_syntax in
  let%bind () =
    match
      List.length files <= 128
      && List.sum (module Int) files ~f:(fun file -> String.length file.contents)
         <= 4 * 1024 * 1024
    with
    | true -> Ok ()
    | false -> Error "authoring package files exceed the 128-file / 4 MiB aggregate bound"
  in
  let%bind packages =
    List.map files ~f:(fun file -> decode file.contents)
    |> Result.all
    |> Result.map ~f:List.concat
  in
  let%bind sources = Authoring_sources.installed () in
  let%bind corpus = C.runtime_foundation ~sources in
  let%map (_ : C.t) = C.extend_authored corpus packages in
  packages
;;

let load_many ~env ~paths =
  let open Result.Let_syntax in
  let%bind () =
    match List.length paths <= 128 with
    | false -> Error "at most 128 authoring package files may be configured"
    | true ->
      (match List.find_a_dup paths ~compare:String.compare with
       | Some _ -> Error "duplicate authoring package file path"
       | None ->
         (match List.for_all paths ~f:Filename.is_absolute with
          | true -> Ok ()
          | false -> Error "authoring package file paths must be absolute"))
  in
  match paths with
  | [] -> Ok []
  | _ ->
    let%bind _, reversed =
      List.fold_result paths ~init:(0, []) ~f:(fun (bytes, files) path ->
        let%bind file =
          load ~env ~path |> Result.map_error ~f:(fun message -> path ^ ": " ^ message)
        in
        let bytes = bytes + String.length file.contents in
        match bytes <= 4 * 1024 * 1024 with
        | true -> Ok (bytes, file :: files)
        | false -> Error "authoring package files exceed the 4 MiB aggregate bound")
    in
    let files = List.rev reversed in
    let%map _ = packages files in
    files
;;
