open Core
module Project = Chatml_project_declaration
module Receipt = Chatml_project_lock_receipt
module Reader = Chatml_project_decode.Reader
module Error = Project.Error
module Id = Chatml_source.Id

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

module Decode = struct
  let primitive reader path result reason =
    match result with
    | Ok value -> Ok value
    | Error _ -> Reader.fail reader path reason
  ;;

  let id reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields reader path ~allowed:[ "namespace"; "path" ] value
    in
    let%bind package_namespace =
      Reader.required reader path fields "namespace" ~decode:Reader.nonempty_string
    in
    let%bind relative_path =
      Reader.required reader path fields "path" ~decode:Reader.nonempty_string
    in
    primitive
      reader
      path
      (Id.create ~package_namespace ~relative_path)
      "invalid source identity"
  ;;

  let digest reader path value =
    let open Result.Let_syntax in
    let%bind hex = Reader.nonempty_string reader path value in
    primitive
      reader
      path
      (Chatml_source.Digest.of_hex hex)
      "expected a lowercase SHA-256 digest"
  ;;

  let integer reader path = function
    | `Number number ->
      let integer =
        String.for_all number ~f:(function
          | '-' | '0' .. '9' -> true
          | _ -> false)
      in
      (match if integer then Int.of_string_opt number else None with
       | Some integer -> Ok integer
       | None -> Reader.fail reader path "expected an integer")
    | `Null | `True | `False | `String _ | `Array _ | `Object _ ->
      Reader.fail reader path "expected an integer"
  ;;

  let version reader path value =
    let open Result.Let_syntax in
    let%bind version = integer reader path value in
    if Int.equal version 1
    then Ok ()
    else
      Error
        (Error.create
           (Project.Declared_at.source reader.Reader.origin)
           ~details:
             (Document_schema.Error.Unsupported_version
                { kind = "chatml.project.lock"; version; target = 1 })
           ~field_path:path)
  ;;

  let source reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields reader path ~allowed:[ "id"; "sha256"; "byte_length" ] value
    in
    let%bind id = Reader.required reader path fields "id" ~decode:id in
    let%bind digest = Reader.required reader path fields "sha256" ~decode:digest in
    let%bind byte_length =
      Reader.required reader path fields "byte_length" ~decode:integer
    in
    primitive
      reader
      path
      (Receipt.Source_entry.create id ~digest ~byte_length)
      "expected a nonnegative source byte length"
  ;;

  let entry reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields reader path ~allowed:[ "source"; "selection" ] value
    in
    let%bind source = Reader.required reader path fields "source" ~decode:source in
    let selection reader path value =
      let%bind fields =
        Reader.object_fields reader path ~allowed:[ "kind"; "name" ] value
      in
      let%bind kind =
        Reader.required reader path fields "kind" ~decode:Reader.nonempty_string
      in
      match kind with
      | "declared" ->
        let%map name =
          Reader.required reader path fields "name" ~decode:Reader.nonempty_string
        in
        Receipt.Entry.Declared name
      | "captured" ->
        let%map _ = Reader.object_fields reader path ~allowed:[ "kind" ] value in
        Receipt.Entry.Captured
      | _ -> Reader.fail reader path "expected declared or captured entry selection"
    in
    let%bind selection =
      Reader.required reader path fields "selection" ~decode:selection
    in
    primitive
      reader
      path
      (Receipt.Entry.create source ~selection)
      "invalid entry selection"
  ;;

  let dependency reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields reader path ~allowed:[ "name"; "declaration" ] value
    in
    let%bind name =
      Reader.required reader path fields "name" ~decode:Reader.nonempty_string
    in
    let%bind declaration = Reader.required reader path fields "declaration" ~decode:id in
    primitive
      reader
      path
      (Receipt.Dependency_selection.create
         ~name
         ~declaration
         ~checkpoint:reader.checkpoint)
      "invalid selected dependency name"
  ;;

  let module_selection reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields reader path ~allowed:[ "implementation"; "interface" ] value
    in
    let%bind implementation =
      Reader.required reader path fields "implementation" ~decode:id
    in
    let%bind interface = Reader.optional reader path fields "interface" ~decode:id in
    primitive
      reader
      path
      (Receipt.Module_selection.create ~implementation ~interface)
      "implementation and interface IDs must differ"
  ;;

  let binding reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields reader path ~allowed:[ "name"; "module" ] value
    in
    let%bind name =
      Reader.required reader path fields "name" ~decode:Reader.module_name
    in
    let%map implementation = Reader.required reader path fields "module" ~decode:id in
    { Receipt.File_scope.name
    ; module_id = Chatml_prepared_program.Module_id.of_implementation implementation
    }
  ;;

  let scope reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields
        reader
        path
        ~allowed:[ "source"; "environment"; "bindings" ]
        value
    in
    let%bind source = Reader.required reader path fields "source" ~decode:id in
    let%bind environment =
      Reader.required reader path fields "environment" ~decode:Reader.nonempty_string
    in
    let%bind bindings =
      Reader.required reader path fields "bindings" ~decode:(Reader.array ~decode:binding)
    in
    primitive
      reader
      path
      (Receipt.File_scope.create
         source
         ~environment
         ~bindings
         ~checkpoint:reader.checkpoint)
      "invalid or duplicate scope bindings"
  ;;

  let origin reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields
        reader
        path
        ~allowed:
          [ "kind"; "path"; "repository"; "object_format"; "commit"; "package_root" ]
        value
    in
    let%bind kind =
      Reader.required reader path fields "kind" ~decode:Reader.nonempty_string
    in
    match kind with
    | "local" ->
      let%map _ = Reader.object_fields reader path ~allowed:[ "kind" ] value in
      Receipt.Catalog.Local
    | "git" ->
      let%bind package_root =
        let path = path @ [ "package_root" ] in
        match Document_schema.Json.field (`Object fields) ~name:"package_root" with
        | Absent -> Reader.fail reader path "required field is absent"
        | Null -> Ok None
        | Value value ->
          let%bind value = Reader.nonempty_string reader path value in
          primitive
            reader
            path
            (Source.Portable_path.create value)
            "invalid portable repository package root"
          |> Result.map ~f:Option.some
      in
      (* Reuse the authored source-kind/pin decoder without admitting this receipt-
         only field into user dependency syntax. *)
      let value =
        `Object
          (List.filter fields ~f:(fun (name, _) -> not (String.equal name "package_root")))
      in
      let%bind source =
        Chatml_project_decode.Decode.dependency_source reader path value
      in
      (match source with
       | Git { repository; pin } ->
         Ok (Receipt.Catalog.Git { repository; pin; package_root })
       | Local _ -> Reader.fail reader path "expected a Git catalog origin")
    | _ -> Reader.fail reader path "expected local or git catalog origin"
  ;;

  let catalog reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields
        reader
        path
        ~allowed:
          [ "namespace"
          ; "origin"
          ; "declaration"
          ; "selected_environment"
          ; "dependencies"
          ; "sources"
          ; "modules"
          ; "scopes"
          ]
        value
    in
    let%bind namespace =
      Reader.required reader path fields "namespace" ~decode:Reader.nonempty_string
    in
    let%bind origin = Reader.required reader path fields "origin" ~decode:origin in
    let%bind declaration = Reader.required reader path fields "declaration" ~decode:id in
    let%bind selected_environment =
      Reader.required
        reader
        path
        fields
        "selected_environment"
        ~decode:Reader.nonempty_string
    in
    let%bind dependencies =
      Reader.required
        reader
        path
        fields
        "dependencies"
        ~decode:(Reader.array ~decode:dependency)
    in
    let%bind sources =
      Reader.required reader path fields "sources" ~decode:(Reader.array ~decode:source)
    in
    let%bind modules =
      Reader.required
        reader
        path
        fields
        "modules"
        ~decode:(Reader.array ~decode:module_selection)
    in
    let%bind scopes =
      Reader.required reader path fields "scopes" ~decode:(Reader.array ~decode:scope)
    in
    Receipt.Catalog.create
      ~declared_at:(Reader.origin reader path)
      ~namespace
      ~origin
      ~declaration
      ~selected_environment
      ~dependencies
      ~sources
      ~modules
      ~scopes
      ~checkpoint:reader.checkpoint
  ;;

  let compiler reader path value =
    let open Result.Let_syntax in
    let%bind fields =
      Reader.object_fields
        reader
        path
        ~allowed:[ "family"; "target"; "contract_sha256" ]
        value
    in
    let%bind family =
      Reader.required reader path fields "family" ~decode:Reader.nonempty_string
    in
    let%bind target =
      Reader.required reader path fields "target" ~decode:Reader.nonempty_string
    in
    let%map contract =
      Reader.required reader path fields "contract_sha256" ~decode:digest
    in
    family, target, contract
  ;;

  let reject_own_source reader entry catalogs =
    let own = Chatml_source.id (Project.Declared_at.source reader.Reader.origin) in
    let open Result.Let_syntax in
    let%bind () =
      if Id.equal own (Receipt.Source_entry.id (Receipt.Entry.source entry))
      then Reader.fail reader [ "entry" ] "lock entry includes its own source ID"
      else Ok ()
    in
    List.fold_result catalogs ~init:() ~f:(fun () catalog ->
      reader.checkpoint ();
      List.fold_result (Receipt.Catalog.sources catalog) ~init:() ~f:(fun () source ->
        reader.checkpoint ();
        if Id.equal own (Receipt.Source_entry.id source)
        then Reader.fail reader [ "catalogs" ] "lock inventory includes its own source ID"
        else Ok ()))
  ;;

  let receipt reader value =
    let open Result.Let_syntax in
    let path = [] in
    let%bind fields =
      Reader.object_fields
        reader
        path
        ~allowed:
          [ "schema_version"
          ; "root"
          ; "entry"
          ; "environment"
          ; "compiler"
          ; "required_tools"
          ; "catalogs"
          ]
        value
    in
    let%bind () = Reader.required reader path fields "schema_version" ~decode:version in
    let%bind root = Reader.required reader path fields "root" ~decode:id in
    let%bind entry = Reader.required reader path fields "entry" ~decode:entry in
    let%bind environment =
      Reader.required reader path fields "environment" ~decode:Reader.nonempty_string
    in
    let%bind compiler_family, target_name, compiler_contract =
      Reader.required reader path fields "compiler" ~decode:compiler
    in
    let%bind required_tools =
      Reader.required
        reader
        path
        fields
        "required_tools"
        ~decode:(Reader.array ~decode:Reader.nonempty_string)
    in
    let%bind catalogs =
      Reader.required reader path fields "catalogs" ~decode:(Reader.array ~decode:catalog)
    in
    let%bind () = reject_own_source reader entry catalogs in
    Receipt.create
      ~declared_at:reader.origin
      ~root
      ~entry
      ~environment
      ~compiler_family
      ~target_name
      ~compiler_contract
      ~required_tools
      ~catalogs
      ~checkpoint:reader.checkpoint
      ()
  ;;
end

let decode source ~limits ~checkpoint =
  checkpoint ();
  let json = Document_schema.Json.decode ~limits (Chatml_source.contents source) in
  checkpoint ();
  let open Result.Let_syntax in
  let%bind json = Result.map_error json ~f:(schema_error source) in
  let origin = Project.Declared_at.create source ~field_path:[] ~checkpoint in
  Decode.receipt { Reader.origin; checkpoint } json
;;

module Encode = struct
  (** Invocation-local lower-bound accounting limits JSON construction before the
      existing exact escaped-size validation. Mutation never escapes this call. *)
  type t =
    { origin : Project.Declared_at.t
    ; checkpoint : unit -> unit
    ; mutable remaining_bytes : int
    }

  let charge t bytes =
    t.checkpoint ();
    if bytes > t.remaining_bytes
    then
      Error
        (Error.create
           (Project.Declared_at.source t.origin)
           ~details:
             (Document_schema.Error.Limit_exceeded "lock output exceeds byte limit")
           ~field_path:(Project.Declared_at.field_path t.origin))
    else (
      t.remaining_bytes <- t.remaining_bytes - bytes;
      Ok ())
  ;;

  let string t string =
    let open Result.Let_syntax in
    let%bind () = charge t (String.length string) in
    let%map () = charge t 2 in
    `String string
  ;;

  let integer t integer =
    let lexeme = Int.to_string integer in
    Result.map (charge t (String.length lexeme)) ~f:(fun () -> `Number lexeme)
  ;;

  let object_fields t fields =
    let open Result.Let_syntax in
    let%bind () = charge t 2 in
    let%map _ =
      List.fold_result fields ~init:true ~f:(fun first (key, _) ->
        let%bind () = charge t (String.length key) in
        let%map () = charge t (if first then 3 else 4) in
        false)
    in
    `Object fields
  ;;

  let array t values ~encode =
    let open Result.Let_syntax in
    let%bind () = charge t 2 in
    let%map _, values =
      List.fold_result values ~init:(true, []) ~f:(fun (first, encoded) value ->
        t.checkpoint ();
        let%bind () = if first then Ok () else charge t 1 in
        let%map value = encode t value in
        false, value :: encoded)
    in
    `Array (List.rev values)
  ;;

  let id t id =
    let open Result.Let_syntax in
    let%bind namespace = string t (Id.package_namespace id) in
    let%bind path = string t (Id.relative_path id) in
    object_fields t [ "namespace", namespace; "path", path ]
  ;;

  let digest t digest = string t (Chatml_source.Digest.to_hex digest)

  let source t source =
    let open Result.Let_syntax in
    let%bind id = id t (Receipt.Source_entry.id source) in
    let%bind sha256 = digest t (Receipt.Source_entry.digest source) in
    let%bind byte_length = integer t (Receipt.Source_entry.byte_length source) in
    object_fields t [ "id", id; "sha256", sha256; "byte_length", byte_length ]
  ;;

  let entry t entry =
    let open Result.Let_syntax in
    let%bind source = source t (Receipt.Entry.source entry) in
    let%bind selection =
      match Receipt.Entry.selection entry with
      | Declared name ->
        let%bind kind = string t "declared" in
        let%bind name = string t name in
        object_fields t [ "kind", kind; "name", name ]
      | Captured ->
        let%bind kind = string t "captured" in
        object_fields t [ "kind", kind ]
    in
    object_fields t [ "source", source; "selection", selection ]
  ;;

  let dependency t dependency =
    let open Result.Let_syntax in
    let%bind name = string t (Receipt.Dependency_selection.name dependency) in
    let%bind declaration = id t (Receipt.Dependency_selection.declaration dependency) in
    object_fields t [ "name", name; "declaration", declaration ]
  ;;

  let module_selection t selection =
    let open Result.Let_syntax in
    let%bind implementation = id t (Receipt.Module_selection.implementation selection) in
    let%bind fields =
      match Receipt.Module_selection.interface selection with
      | None -> Ok [ "implementation", implementation ]
      | Some interface ->
        let%map interface = id t interface in
        [ "implementation", implementation; "interface", interface ]
    in
    object_fields t fields
  ;;

  let binding t (binding : Receipt.File_scope.binding) =
    let open Result.Let_syntax in
    let%bind name = string t binding.name in
    let%bind module_ =
      id t (Chatml_prepared_program.Module_id.implementation binding.module_id)
    in
    object_fields t [ "name", name; "module", module_ ]
  ;;

  let scope t scope =
    let open Result.Let_syntax in
    let%bind source = id t (Receipt.File_scope.source scope) in
    let%bind environment = string t (Receipt.File_scope.environment scope) in
    let%bind bindings = array t (Receipt.File_scope.bindings scope) ~encode:binding in
    object_fields t [ "source", source; "environment", environment; "bindings", bindings ]
  ;;

  let origin t = function
    | Receipt.Catalog.Local ->
      let open Result.Let_syntax in
      let%bind kind = string t "local" in
      object_fields t [ "kind", kind ]
    | Git { repository; pin; package_root } ->
      let open Result.Let_syntax in
      let%bind kind = string t "git" in
      let%bind repository = string t repository in
      let%bind object_format =
        string
          t
          (match Project.Git_pin.object_format pin with
           | Sha1 -> "sha1"
           | Sha256 -> "sha256")
      in
      let%bind commit = string t (Project.Git_pin.commit pin) in
      let%bind package_root =
        match package_root with
        | None -> Result.map (charge t 4) ~f:(fun () -> `Null)
        | Some root -> string t (Source.Portable_path.to_string root)
      in
      object_fields
        t
        [ "kind", kind
        ; "repository", repository
        ; "object_format", object_format
        ; "commit", commit
        ; "package_root", package_root
        ]
  ;;

  let catalog t catalog =
    let open Result.Let_syntax in
    let%bind namespace = string t (Receipt.Catalog.namespace catalog) in
    let%bind origin = origin t (Receipt.Catalog.origin catalog) in
    let%bind declaration = id t (Receipt.Catalog.declaration catalog) in
    let%bind selected_environment =
      string t (Receipt.Catalog.selected_environment catalog)
    in
    let%bind dependencies =
      array t (Receipt.Catalog.dependencies catalog) ~encode:dependency
    in
    let%bind sources = array t (Receipt.Catalog.sources catalog) ~encode:source in
    let%bind modules =
      array t (Receipt.Catalog.modules catalog) ~encode:module_selection
    in
    let%bind scopes = array t (Receipt.Catalog.scopes catalog) ~encode:scope in
    object_fields
      t
      [ "namespace", namespace
      ; "origin", origin
      ; "declaration", declaration
      ; "selected_environment", selected_environment
      ; "dependencies", dependencies
      ; "sources", sources
      ; "modules", modules
      ; "scopes", scopes
      ]
  ;;

  let receipt t receipt =
    let open Result.Let_syntax in
    let%bind schema_version = integer t 1 in
    let%bind root = id t (Receipt.root receipt) in
    let%bind entry = entry t (Receipt.entry_selection receipt) in
    let%bind environment = string t (Receipt.environment receipt) in
    let%bind family = string t (Receipt.compiler_family receipt) in
    let%bind target = string t (Receipt.target_name receipt) in
    let%bind contract_sha256 = digest t (Receipt.compiler_contract receipt) in
    let%bind compiler =
      object_fields
        t
        [ "family", family; "target", target; "contract_sha256", contract_sha256 ]
    in
    let%bind required_tools = array t (Receipt.required_tools receipt) ~encode:string in
    let%bind catalogs = array t (Receipt.catalogs receipt) ~encode:catalog in
    object_fields
      t
      [ "schema_version", schema_version
      ; "root", root
      ; "entry", entry
      ; "environment", environment
      ; "compiler", compiler
      ; "required_tools", required_tools
      ; "catalogs", catalogs
      ]
  ;;
end

let to_string receipt ~limits ~checkpoint =
  checkpoint ();
  let origin = Receipt.declared_at receipt in
  let context =
    { Encode.origin
    ; checkpoint
    ; remaining_bytes = Document_schema.Limits.max_bytes limits
    }
  in
  let open Result.Let_syntax in
  let%bind json = Encode.receipt context receipt in
  checkpoint ();
  let measured = Document_schema.Json.validate_and_measure ~limits json in
  checkpoint ();
  let%bind _ =
    Result.map_error measured ~f:(fun details ->
      Error.create
        (Project.Declared_at.source origin)
        ~details
        ~field_path:(Project.Declared_at.field_path origin))
  in
  let rendered = Jsonaf.to_string json in
  checkpoint ();
  Ok rendered
;;
