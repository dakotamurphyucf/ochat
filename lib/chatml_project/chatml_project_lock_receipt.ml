open Core
module Error = Chatml_project_declaration.Error
module Id = Chatml_source.Id
module Module_id = Chatml_prepared_program.Module_id

let owned_error declared_at field reason =
  let path = Chatml_project_declaration.Declared_at.field_path declared_at @ field in
  Error
    (Error.create
       (Chatml_project_declaration.Declared_at.source declared_at)
       ~details:(Document_schema.Error.Invalid_field { path; reason })
       ~field_path:path)
;;

let nonempty string = not (String.is_empty string)

let canonical_unique values ~compare ~checkpoint =
  let values =
    List.sort values ~compare:(fun left right ->
      checkpoint ();
      compare left right)
  in
  let rec validate previous = function
    | [] -> Ok values
    | value :: rest ->
      checkpoint ();
      (match previous with
       | Some previous when Int.equal (compare previous value) 0 ->
         Or_error.error_string "duplicate receipt record key"
       | None | Some _ -> validate (Some value) rest)
  in
  validate None values
;;

module Source_entry = struct
  type t =
    { id : Id.t
    ; digest : Chatml_source.Digest.t
    ; byte_length : int
    }

  let create id ~digest ~byte_length =
    if byte_length < 0
    then Or_error.error_string "expected a nonnegative source byte length"
    else Ok { id; digest; byte_length }
  ;;

  let id t = t.id
  let digest t = t.digest
  let byte_length t = t.byte_length
end

module Entry = struct
  type selection =
    | Declared of string
    | Captured

  type t =
    { source : Source_entry.t
    ; selection : selection
    }

  let create source ~selection =
    match selection with
    | Declared name when String.is_empty name ->
      Or_error.error_string "expected a nonempty declared entry name"
    | Declared _ | Captured -> Ok { source; selection }
  ;;

  let source t = t.source
  let selection t = t.selection
end

module Module_selection = struct
  type t =
    { implementation : Id.t
    ; interface : Id.t option
    }

  let create ~implementation ~interface =
    match interface with
    | Some interface when Id.equal implementation interface ->
      Or_error.error_string "implementation and interface IDs must differ"
    | None | Some _ -> Ok { implementation; interface }
  ;;

  let implementation t = t.implementation
  let interface t = t.interface
end

module File_scope = struct
  type binding =
    { name : string
    ; module_id : Module_id.t
    }

  type t =
    { source : Id.t
    ; environment : string
    ; bindings : binding list
    }

  let create source ~environment ~bindings ~checkpoint =
    checkpoint ();
    let open Result.Let_syntax in
    let%bind () =
      if nonempty environment
      then Ok ()
      else Or_error.error_string "expected a nonempty environment name"
    in
    let%bind () =
      List.fold_result bindings ~init:() ~f:(fun () binding ->
        checkpoint ();
        if Chatml_project_declaration.Module_ref.is_valid_name binding.name ~checkpoint
        then Ok ()
        else Or_error.error_string "expected a configured UIDENT binding name")
    in
    let%map bindings =
      canonical_unique bindings ~checkpoint ~compare:(fun left right ->
        String.compare left.name right.name)
    in
    { source; environment; bindings }
  ;;

  let source t = t.source
  let environment t = t.environment
  let bindings t = t.bindings
end

module Dependency_selection = struct
  type t =
    { name : string
    ; declaration : Id.t
    }

  let create ~name ~declaration ~checkpoint =
    if Chatml_project_declaration.Dependency.is_valid_name name ~checkpoint
    then Ok { name; declaration }
    else Or_error.error_string "expected a dependency alias identifier other than self"
  ;;

  let name t = t.name
  let declaration t = t.declaration
end

module Catalog = struct
  type origin =
    | Local
    | Git of
        { repository : string
        ; pin : Chatml_project_declaration.Git_pin.t
        ; package_root : Source.Portable_path.t option
        }

  type t =
    { namespace : string
    ; origin : origin
    ; declaration : Id.t
    ; selected_environment : string
    ; dependencies : Dependency_selection.t list
    ; sources : Source_entry.t list
    ; modules : Module_selection.t list
    ; scopes : File_scope.t list
    }

  let validate_origin origin ~checkpoint =
    match origin with
    | Local ->
      checkpoint ();
      true
    | Git { repository; pin = _; package_root = _ } ->
      Chatml_project_declaration.Dependency.is_valid_locator repository ~checkpoint
  ;;

  let create
        ~declared_at
        ~namespace
        ~origin
        ~declaration
        ~selected_environment
        ~dependencies
        ~sources
        ~modules
        ~scopes
        ~checkpoint
    =
    checkpoint ();
    let fail field reason = owned_error declared_at field reason in
    let owns id = String.equal namespace (Id.package_namespace id) in
    let open Result.Let_syntax in
    let%bind () =
      if nonempty namespace
      then Ok ()
      else fail [ "namespace" ] "expected a nonempty namespace"
    in
    let%bind () =
      if nonempty selected_environment
      then Ok ()
      else fail [ "selected_environment" ] "expected a nonempty environment name"
    in
    let%bind () =
      if validate_origin origin ~checkpoint
      then Ok ()
      else fail [ "origin" ] "invalid dependency locator"
    in
    let%bind () =
      if owns declaration
      then Ok ()
      else fail [ "declaration" ] "declaration ID has another namespace"
    in
    let%bind () =
      List.fold_result sources ~init:() ~f:(fun () source ->
        checkpoint ();
        if owns (Source_entry.id source)
        then Ok ()
        else fail [ "sources" ] "source ID has another namespace")
    in
    let%bind () =
      List.fold_result modules ~init:() ~f:(fun () selection ->
        checkpoint ();
        if
          owns (Module_selection.implementation selection)
          && Option.for_all (Module_selection.interface selection) ~f:owns
        then Ok ()
        else fail [ "modules" ] "module owner ID has another namespace")
    in
    let%bind () =
      List.fold_result scopes ~init:() ~f:(fun () scope ->
        checkpoint ();
        if owns (File_scope.source scope)
        then Ok ()
        else fail [ "scopes" ] "scope owner ID has another namespace")
    in
    let canonical values field compare =
      match canonical_unique values ~compare ~checkpoint with
      | Ok values -> Ok values
      | Error _ -> fail [ field ] "duplicate receipt record key"
    in
    let%bind dependencies =
      canonical dependencies "dependencies" (fun a b ->
        String.compare (Dependency_selection.name a) (Dependency_selection.name b))
    in
    let%bind sources =
      canonical sources "sources" (fun a b ->
        Id.compare (Source_entry.id a) (Source_entry.id b))
    in
    let%bind modules =
      canonical modules "modules" (fun a b ->
        Id.compare (Module_selection.implementation a) (Module_selection.implementation b))
    in
    let%map scopes =
      canonical scopes "scopes" (fun a b ->
        Id.compare (File_scope.source a) (File_scope.source b))
    in
    { namespace
    ; origin
    ; declaration
    ; selected_environment
    ; dependencies
    ; sources
    ; modules
    ; scopes
    }
  ;;

  let namespace t = t.namespace
  let origin t = t.origin
  let declaration t = t.declaration
  let dependencies t = t.dependencies
  let selected_environment t = t.selected_environment
  let sources t = t.sources
  let modules t = t.modules
  let scopes t = t.scopes
end

type t =
  { declared_at : Chatml_project_declaration.Declared_at.t
  ; root : Id.t
  ; entry : Entry.t
  ; environment : string
  ; compiler_family : string
  ; target_name : string
  ; compiler_contract : Chatml_source.Digest.t
  ; required_tools : string list
  ; catalogs : Catalog.t list
  }

let create
      ~declared_at
      ~root
      ~entry
      ~environment
      ~compiler_family
      ~target_name
      ~compiler_contract
      ~required_tools
      ~catalogs
      ~checkpoint
      ()
  =
  checkpoint ();
  let fail field reason = owned_error declared_at field reason in
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result
      [ "environment", environment; "compiler", compiler_family; "compiler", target_name ]
      ~init:()
      ~f:(fun () (field, value) ->
        checkpoint ();
        if nonempty value then Ok () else fail [ field ] "expected a nonempty name")
  in
  let%bind () =
    List.fold_result required_tools ~init:() ~f:(fun () name ->
      checkpoint ();
      if nonempty name
      then Ok ()
      else fail [ "required_tools" ] "expected a nonempty tool name")
  in
  let%bind required_tools =
    match canonical_unique required_tools ~compare:String.compare ~checkpoint with
    | Ok tools -> Ok tools
    | Error _ -> fail [ "required_tools" ] "duplicate tool name"
  in
  let%map catalogs =
    match
      canonical_unique catalogs ~checkpoint ~compare:(fun a b ->
        String.compare (Catalog.namespace a) (Catalog.namespace b))
    with
    | Ok catalogs -> Ok catalogs
    | Error _ -> fail [ "catalogs" ] "duplicate catalog namespace"
  in
  { declared_at
  ; root
  ; entry
  ; environment
  ; compiler_family
  ; target_name
  ; compiler_contract
  ; required_tools
  ; catalogs
  }
;;

let declared_at t = t.declared_at
let root t = t.root
let entry t = Source_entry.id (Entry.source t.entry)
let entry_selection t = t.entry
let environment t = t.environment
let compiler_family t = t.compiler_family
let target_name t = t.target_name
let compiler_contract t = t.compiler_contract
let required_tools t = t.required_tools
let catalogs t = t.catalogs
