open Core
open Chatml_project_declaration

module Reader = struct
  type t =
    { origin : Declared_at.t
    ; checkpoint : unit -> unit
    }

  let origin t field_path = Declared_at.with_field_path t.origin ~field_path

  let fail t path reason =
    Error
      (Error.create
         (Declared_at.source t.origin)
         ~details:(Document_schema.Error.Invalid_field { path; reason })
         ~field_path:path)
  ;;

  let object_fields t path ~allowed = function
    | `Object fields ->
      let open Result.Let_syntax in
      let%map () =
        List.fold_result fields ~init:() ~f:(fun () (name, _) ->
          t.checkpoint ();
          if List.mem allowed name ~equal:String.equal
          then Ok ()
          else fail t (path @ [ name ]) "unknown field")
      in
      fields
    | `Null | `True | `False | `String _ | `Number _ | `Array _ ->
      fail t path "expected an object"
  ;;

  let required t path fields name ~decode =
    let path = path @ [ name ] in
    match Document_schema.Json.field (`Object fields) ~name with
    | Absent -> fail t path "required field is absent"
    | Null -> fail t path "null is not allowed"
    | Value value -> decode t path value
  ;;

  let optional t path fields name ~decode =
    let path = path @ [ name ] in
    match Document_schema.Json.field (`Object fields) ~name with
    | Absent -> Ok None
    | Null -> fail t path "null is not allowed"
    | Value value -> Result.map (decode t path value) ~f:Option.some
  ;;

  let nonempty_string t path = function
    | `String string when not (String.is_empty string) -> Ok string
    | `String _ -> fail t path "expected a nonempty string"
    | `Null | `True | `False | `Number _ | `Array _ | `Object _ ->
      fail t path "expected a string"
  ;;

  let portable_path t path value =
    let open Result.Let_syntax in
    let%bind string = nonempty_string t path value in
    match Source.Portable_path.create string with
    | Ok path -> Ok path
    | Error _ -> fail t path "invalid portable relative member path"
  ;;

  let module_name t path value =
    let open Result.Let_syntax in
    let%bind name = nonempty_string t path value in
    if Module_ref.is_valid_name name ~checkpoint:t.checkpoint
    then Ok name
    else fail t path "expected a configured UIDENT module name"
  ;;

  let dependency_name t path value =
    let open Result.Let_syntax in
    let%bind name = nonempty_string t path value in
    if Dependency.is_valid_name name ~checkpoint:t.checkpoint
    then Ok name
    else fail t path "expected a dependency alias identifier"
  ;;

  let locator t path value =
    let open Result.Let_syntax in
    let%bind string = nonempty_string t path value in
    if not (Dependency.is_valid_locator string ~checkpoint:t.checkpoint)
    then fail t path "locator contains NUL or a line break"
    else Ok string
  ;;

  let array t path ~decode = function
    | `Array values ->
      let open Result.Let_syntax in
      let%map _, values =
        List.fold_result values ~init:(0, []) ~f:(fun (index, values) value ->
          t.checkpoint ();
          let%map value = decode t (path @ [ Int.to_string index ]) value in
          index + 1, value :: values)
      in
      List.rev values
    | `Null | `True | `False | `String _ | `Number _ | `Object _ ->
      fail t path "expected an array"
  ;;

  let unique_array t path ~decode ~key value =
    let open Result.Let_syntax in
    let%bind values = array t path ~decode value in
    let%map _ =
      List.fold_result values ~init:String.Set.empty ~f:(fun seen value ->
        t.checkpoint ();
        let key = key value in
        if Set.mem seen key
        then fail t path "duplicate array member"
        else Ok (Set.add seen key))
    in
    values
  ;;

  let named t path ~key ~decode = function
    | `Object fields ->
      List.fold_result fields ~init:String.Map.empty ~f:(fun map (name, value) ->
        t.checkpoint ();
        let open Result.Let_syntax in
        let path = path @ [ name ] in
        let%bind name = key t path (`String name) in
        let%map value = decode t path ~name value in
        Map.set map ~key:name ~data:value)
    | `Null | `True | `False | `String _ | `Number _ | `Array _ ->
      fail t path "expected a named object"
  ;;
end

module Decode = struct
  let source_dir reader path value =
    let open Result.Let_syntax in
    let%bind string = Reader.nonempty_string reader path value in
    let%map directory =
      if String.equal string "."
      then Ok Source_dir.Root
      else
        Result.map (Reader.portable_path reader path value) ~f:(fun path ->
          Source_dir.Relative path)
    in
    { Source_dir.directory; declared_at = Reader.origin reader path }
  ;;

  let source_dir_key source_dir =
    match Source_dir.directory source_dir with
    | Root -> "."
    | Relative path -> Source.Portable_path.to_string path
  ;;

  let module_override reader path ~name value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields reader path ~allowed:[ "implementation"; "interface" ] value
    in
    let%bind implementation =
      Reader.required reader path fields "implementation" ~decode:Reader.portable_path
    in
    let%map interface =
      Reader.optional reader path fields "interface" ~decode:Reader.portable_path
    in
    { Module_override.name
    ; implementation
    ; interface_selection =
        (match interface with
         | None -> Companion
         | Some path -> Explicit path)
    ; declared_at = Reader.origin reader path
    }
  ;;

  let dependency_source reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields
        reader
        path
        ~allowed:[ "kind"; "path"; "repository"; "object_format"; "commit" ]
        value
    in
    let%bind kind =
      Reader.required reader path fields "kind" ~decode:Reader.nonempty_string
    in
    match kind with
    | "local" ->
      let%bind _ = Reader.object_fields reader path ~allowed:[ "kind"; "path" ] value in
      let%map directory =
        Reader.required reader path fields "path" ~decode:Reader.locator
      in
      Dependency.Local { directory }
    | "git" ->
      let%bind _ =
        Reader.object_fields
          reader
          path
          ~allowed:[ "kind"; "repository"; "object_format"; "commit" ]
          value
      in
      let%bind repository =
        Reader.required reader path fields "repository" ~decode:Reader.locator
      in
      let%bind object_format =
        Reader.required reader path fields "object_format" ~decode:Reader.nonempty_string
      in
      let%bind object_format =
        match object_format with
        | "sha1" -> Ok Git_pin.Sha1
        | "sha256" -> Ok Git_pin.Sha256
        | _ ->
          Reader.fail reader (path @ [ "object_format" ]) "unsupported Git object format"
      in
      let%bind commit =
        Reader.required reader path fields "commit" ~decode:Reader.nonempty_string
      in
      let%map pin =
        match Git_pin.create ~object_format ~commit with
        | Ok pin -> Ok pin
        | Error _ -> Reader.fail reader (path @ [ "commit" ]) "invalid full commit pin"
      in
      Dependency.Git { repository; pin }
    | _ -> Reader.fail reader (path @ [ "kind" ]) "unsupported source kind"
  ;;

  let dependency reader path ~name value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields
        reader
        path
        ~allowed:[ "source"; "project"; "environment"; "namespace" ]
        value
    in
    let%bind source =
      Reader.required reader path fields "source" ~decode:dependency_source
    in
    let%bind project =
      Reader.required reader path fields "project" ~decode:Reader.portable_path
    in
    let%bind environment =
      Reader.optional reader path fields "environment" ~decode:Reader.nonempty_string
    in
    let%map namespace_override =
      Reader.optional reader path fields "namespace" ~decode:Reader.nonempty_string
    in
    { Dependency.name
    ; source
    ; project
    ; environment
    ; namespace_override
    ; declared_at = Reader.origin reader path
    }
  ;;

  let module_ref dependencies reader path value =
    let open Result.Let_syntax in
    let%bind string = Reader.nonempty_string reader path value in
    match String.lsplit2 string ~on:'.' with
    | Some (catalog, name) when not (String.contains name '.') ->
      let%bind name = Reader.module_name reader path (`String name) in
      let%map catalog =
        if String.equal catalog "self"
        then Ok Module_ref.Self
        else if Set.mem dependencies catalog
        then Ok (Module_ref.Dependency catalog)
        else Reader.fail reader path "unknown dependency catalog"
      in
      { Module_ref.catalog; name }
    | _ -> Reader.fail reader path "expected catalog.Module reference"
  ;;

  let compiler_requirement reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields
        reader
        path
        ~allowed:[ "compiler"; "targets"; "contract_sha256" ]
        value
    in
    let%bind compiler_family =
      Reader.optional reader path fields "compiler" ~decode:Reader.nonempty_string
    in
    let%bind allowed_targets =
      Reader.optional
        reader
        path
        fields
        "targets"
        ~decode:(Reader.unique_array ~decode:Reader.nonempty_string ~key:Fn.id)
    in
    let%map exact_contract =
      Reader.optional
        reader
        path
        fields
        "contract_sha256"
        ~decode:(fun reader path value ->
          let%bind string = Reader.nonempty_string reader path value in
          match Chatml_source.Digest.of_hex string with
          | Ok digest -> Ok digest
          | Error _ -> Reader.fail reader path "invalid SHA-256 contract digest")
    in
    { Compiler_requirement.compiler_family
    ; allowed_targets
    ; exact_contract
    ; declared_at = Some (Reader.origin reader path)
    }
  ;;

  let environment dependencies reader path ~name value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields reader path ~allowed:[ "modules"; "requires" ] value
    in
    let%bind bindings =
      Reader.optional reader path fields "modules" ~decode:(fun reader path value ->
        let%map bindings =
          Reader.named
            reader
            path
            ~key:Reader.module_name
            value
            ~decode:(fun reader path ~name value ->
              let%map module_ref = module_ref dependencies reader path value in
              { Scope_spec.name; module_ref; declared_at = Reader.origin reader path })
        in
        Map.data bindings)
    in
    let%map requirement =
      Reader.optional reader path fields "requires" ~decode:compiler_requirement
    in
    { Scope_spec.name
    ; bindings
    ; compiler_requirement =
        Option.value requirement ~default:Compiler_requirement.unconstrained
    ; declared_at = Some (Reader.origin reader path)
    }
  ;;

  let entry reader path ~name value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields
        reader
        path
        ~allowed:[ "source"; "target"; "environment" ]
        value
    in
    let%bind source =
      Reader.required reader path fields "source" ~decode:Reader.portable_path
    in
    let%bind target_name =
      Reader.required reader path fields "target" ~decode:Reader.nonempty_string
    in
    let%map environment =
      Reader.optional reader path fields "environment" ~decode:Reader.nonempty_string
    in
    ( { Entry.name; source; target_name; declared_at = Reader.origin reader path }
    , environment )
  ;;

  let version reader path value =
    match value with
    | `Number number ->
      let is_integer =
        String.for_all number ~f:(function
          | '-' | '0' .. '9' -> true
          | _ -> false)
      in
      (match if is_integer then Int.of_string_opt number else None with
       | Some 1 -> Ok ()
       | Some version ->
         Error
           (Error.create
              (Declared_at.source reader.Reader.origin)
              ~details:
                (Document_schema.Error.Unsupported_version
                   { kind = "chatml.project"; version; target = 1 })
              ~field_path:path)
       | None -> Reader.fail reader path "expected an integer schema version")
    | `Null | `True | `False | `String _ | `Array _ | `Object _ ->
      Reader.fail reader path "expected an integer schema version"
  ;;

  let validate_environment reader environments path name =
    reader.Reader.checkpoint ();
    if Map.mem environments name
    then Ok ()
    else Reader.fail reader path "unknown project environment"
  ;;

  type catalog_members =
    { source_dirs : Source_dir.t list
    ; exclusions : (Source.Portable_path.t * Declared_at.t) list
    ; module_overrides : Module_override.t list
    }

  let catalog_members reader fields =
    let open Result.Let_syntax in
    let path = [] in
    let%bind source_dirs =
      Reader.optional
        reader
        path
        fields
        "source_dirs"
        ~decode:(Reader.unique_array ~decode:source_dir ~key:source_dir_key)
    in
    let%bind exclusions =
      Reader.optional
        reader
        path
        fields
        "exclude"
        ~decode:
          (Reader.unique_array
             ~decode:(fun reader path value ->
               let%map member = Reader.portable_path reader path value in
               member, Reader.origin reader path)
             ~key:(fun (member, _) -> Source.Portable_path.to_string member))
    in
    let%bind modules =
      Reader.optional
        reader
        path
        fields
        "modules"
        ~decode:(Reader.named ~key:Reader.module_name ~decode:module_override)
    in
    Ok
      { source_dirs = Option.value source_dirs ~default:[]
      ; exclusions = Option.value exclusions ~default:[]
      ; module_overrides = Map.data (Option.value modules ~default:String.Map.empty)
      }
  ;;

  let environments reader fields dependency_names =
    let open Result.Let_syntax in
    let path = [] in
    let%bind environments =
      Reader.optional
        reader
        path
        fields
        "environments"
        ~decode:
          (Reader.named
             ~key:Reader.nonempty_string
             ~decode:(environment dependency_names))
    in
    let environments =
      Option.value
        environments
        ~default:(String.Map.singleton "default" Scope_spec.implicit_default)
    in
    Ok environments
  ;;

  let default_environment reader fields environments =
    let open Result.Let_syntax in
    let path = [] in
    let%bind default_environment =
      Reader.optional
        reader
        path
        fields
        "default_environment"
        ~decode:Reader.nonempty_string
    in
    let%bind () =
      match default_environment with
      | None -> Ok ()
      | Some name ->
        validate_environment reader environments [ "default_environment" ] name
    in
    Ok
      (match default_environment with
       | Some _ -> default_environment
       | None -> if Map.mem environments "default" then Some "default" else None)
  ;;

  let entries reader fields environments =
    let open Result.Let_syntax in
    let path = [] in
    let%bind entries =
      Reader.optional
        reader
        path
        fields
        "entries"
        ~decode:(Reader.named ~key:Reader.nonempty_string ~decode:entry)
    in
    let entries = Option.value entries ~default:String.Map.empty in
    let%bind () =
      Map.fold entries ~init:(Ok ()) ~f:(fun ~key:name ~data:(_, environment) result ->
        reader.Reader.checkpoint ();
        let%bind () = result in
        match environment with
        | None -> Ok ()
        | Some environment ->
          validate_environment
            reader
            environments
            [ "entries"; name; "environment" ]
            environment)
    in
    Ok entries
  ;;

  let file_environments reader fields environments =
    let open Result.Let_syntax in
    let path = [] in
    let%bind file_names =
      Reader.optional
        reader
        path
        fields
        "file_environments"
        ~decode:
          (Reader.named
             ~key:(fun reader path value ->
               Result.map
                 (Reader.portable_path reader path value)
                 ~f:Source.Portable_path.to_string)
             ~decode:(fun reader path ~name:_ value ->
               Reader.nonempty_string reader path value))
    in
    let%bind file_environments =
      Map.fold
        (Option.value file_names ~default:String.Map.empty)
        ~init:(Ok String.Map.empty)
        ~f:(fun ~key:member ~data:name result ->
          reader.Reader.checkpoint ();
          let%bind map = result in
          let%bind () =
            validate_environment reader environments [ "file_environments"; member ] name
          in
          match Map.find environments name with
          | Some environment -> Ok (Map.set map ~key:member ~data:environment)
          | None ->
            Reader.fail
              reader
              [ "file_environments"; member ]
              "unknown project environment")
    in
    Ok file_environments
  ;;

  let declaration reader value =
    let open Result.Let_syntax in
    let path = [] in
    let%bind fields =
      Reader.object_fields
        reader
        path
        ~allowed:
          [ "schema_version"
          ; "namespace"
          ; "source_dirs"
          ; "exclude"
          ; "modules"
          ; "entries"
          ; "dependencies"
          ; "environments"
          ; "default_environment"
          ; "file_environments"
          ; "capabilities"
          ]
        value
    in
    let%bind () = Reader.required reader path fields "schema_version" ~decode:version in
    let%bind namespace =
      Reader.required reader path fields "namespace" ~decode:Reader.nonempty_string
    in
    let%bind members = catalog_members reader fields in
    let%bind dependencies =
      Reader.optional
        reader
        path
        fields
        "dependencies"
        ~decode:(Reader.named ~key:Reader.dependency_name ~decode:dependency)
    in
    let dependencies = Option.value dependencies ~default:String.Map.empty in
    let dependency_names = Map.key_set dependencies in
    let%bind environments = environments reader fields dependency_names in
    let%bind default_environment = default_environment reader fields environments in
    let%bind entries = entries reader fields environments in
    let%bind file_environments = file_environments reader fields environments in
    let%map tools =
      Reader.optional
        reader
        path
        fields
        "capabilities"
        ~decode:
          (Reader.unique_array
             ~decode:(fun reader path value ->
               let%map name = Reader.nonempty_string reader path value in
               name, Reader.origin reader path)
             ~key:fst)
    in
    { source = Declared_at.source reader.Reader.origin
    ; declared_at = reader.origin
    ; namespace
    ; source_dirs = members.source_dirs
    ; exclusions = members.exclusions
    ; module_overrides = members.module_overrides
    ; dependencies = Map.data dependencies
    ; required_tools = Option.value tools ~default:[]
    ; environments
    ; default_environment
    ; file_environments
    ; entries
    }
  ;;
end

let schema_error source details =
  let field_path =
    match details with
    | Document_schema.Error.Duplicate_key path | Invalid_field { path; reason = _ } ->
      path
    | Invalid_configuration _
    | Limit_exceeded _
    | Malformed _
    | Unsupported_beta_format
    | Unsupported_format _
    | Unsupported_kind _
    | Unsupported_version _
    | Missing_conversion _
    | Wrong_kind _
    | Wrong_version _
    | Required_semantics_unknown _
    | Extension_conflict _ -> []
  in
  Error.create source ~details ~field_path
;;

let decode source ~limits ~checkpoint =
  checkpoint ();
  let json = Document_schema.Json.decode ~limits (Chatml_source.contents source) in
  checkpoint ();
  let open Result.Let_syntax in
  let%bind json = Result.map_error json ~f:(schema_error source) in
  let origin = Declared_at.create source ~field_path:[] ~checkpoint in
  Decode.declaration { Reader.origin; checkpoint } json
;;
