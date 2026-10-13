open! Core
open! Authoring_bundle_basis_common
module A = Authoring_common
module Context = Authoring_context.Context
module Captured = Chatml_source
module Project = Chatml_project
module Path = Source.Portable_path

module Route = struct
  type t =
    { declaration : Captured.Id.t
    ; origin : Project.Lock.Catalog.origin
    ; dependencies : Project.Lock.Dependency_selection.t list
    }

  let declaration t = t.declaration
  let origin t = t.origin
  let dependencies t = t.dependencies

  let dependency_equal left right =
    String.equal
      (Project.Lock.Dependency_selection.name left)
      (Project.Lock.Dependency_selection.name right)
    && Captured.Id.equal
         (Project.Lock.Dependency_selection.declaration left)
         (Project.Lock.Dependency_selection.declaration right)
  ;;

  let equal left right =
    Captured.Id.equal left.declaration right.declaration
    && origin_equal left.origin right.origin
    && List.equal dependency_equal left.dependencies right.dependencies
  ;;

  let create ~declaration ~origin ~dependencies =
    let* () = C.validate_source_id declaration in
    if not (bounded_count ~max:1024 dependencies)
    then invalid "too many bundle route dependencies"
    else
      let* () =
        match origin with
        | Project.Lock.Catalog.Local -> Ok ()
        | Project.Lock.Catalog.Git { repository; package_root; pin = _ } ->
          if
            String.is_empty repository
            || String.length repository > 1024
            || not (Stdlib.String.is_valid_utf_8 repository)
          then invalid "invalid bundle Git repository"
          else
            Option.value_map package_root ~default:(Ok ()) ~f:(fun path ->
              if String.length (Path.to_string path) <= 1024
              then Ok ()
              else invalid "invalid bundle Git package root")
      in
      let* dependencies =
        List.fold_result dependencies ~init:String.Map.empty ~f:(fun seen dependency ->
          let name = Project.Lock.Dependency_selection.name dependency in
          let* () =
            C.validate_source_id
              (Project.Lock.Dependency_selection.declaration dependency)
          in
          if
            String.is_empty name
            || String.length name > 256
            || (not (Stdlib.String.is_valid_utf_8 name))
            || Map.mem seen name
          then invalid "invalid bundle route dependency"
          else Ok (Map.set seen ~key:name ~data:dependency))
      in
      Ok { declaration; origin; dependencies = Map.data dependencies }
  ;;

  let dependency_to_json dependency =
    obj
      [ "name", str (Project.Lock.Dependency_selection.name dependency)
      ; ( "declaration"
        , C.source_id_json (Project.Lock.Dependency_selection.declaration dependency) )
      ]
  ;;

  let dependency_of_json json =
    let* value = fields json [ "name"; "declaration" ] in
    let* name = required value "name" (C.utf8_text ~max_bytes:256) in
    let* declaration = required value "declaration" C.source_id in
    Project.Lock.Dependency_selection.create ~name ~declaration ~checkpoint:Fn.id
    |> Result.map_error ~f:(fun _ ->
      Protocol_error.invalid_request "invalid bundle dependency")
  ;;

  let to_json t =
    obj
      [ "declaration", C.source_id_json t.declaration
      ; "origin", origin_to_json t.origin
      ; "dependencies", C.array dependency_to_json t.dependencies
      ]
  ;;

  let of_json json =
    let* value = fields json [ "declaration"; "origin"; "dependencies" ] in
    let* declaration = required value "declaration" C.source_id in
    let* origin = required value "origin" origin_of_json in
    let* dependencies =
      required value "dependencies" (C.list ~max_items:1024 dependency_of_json)
    in
    create ~declaration ~origin ~dependencies
  ;;
end

module Project_basis = struct
  module Captured_entry = struct
    type delivery =
      | Uploaded_member
      | Derived_script
    [@@deriving equal, sexp_of]

    type t =
      { source : A.Source.t
      ; delivery : delivery
      }

    let source t = t.source
    let delivery t = t.delivery

    let create ~source ~delivery =
      let* () = immutable_source source in
      if
        equal_delivery delivery Derived_script
        && A.Source.byte_length source > 1024 * 1024
      then invalid "invalid derived script source length"
      else Ok { source; delivery }
    ;;

    let equal left right =
      A.Source.equal left.source right.source
      && equal_delivery left.delivery right.delivery
    ;;
  end

  type entry =
    | Declared
    | Captured of Captured_entry.t

  type mode =
    | Exact_lock of Captured.Id.t
    | Compose of
        { selected_previous_lock : A.Source.t option
        ; generated_lock : Captured.Id.t
        }

  type t =
    { root_source : Captured.Id.t
    ; selection : Context.Selection.t
    ; entry : entry
    ; source_members : Captured.Id.t list
    ; routes : Route.t list
    ; mode : mode
    }

  let root_source t = t.root_source
  let selection t = t.selection
  let entry t = t.entry
  let source_members t = t.source_members
  let routes t = t.routes
  let mode t = t.mode

  let equal_entry left right =
    match left, right with
    | Declared, Declared -> true
    | Captured left, Captured right -> Captured_entry.equal left right
    | Declared, Captured _ | Captured _, Declared -> false
  ;;

  let equal_mode left right =
    match left, right with
    | Exact_lock left, Exact_lock right -> Captured.Id.equal left right
    | Compose left, Compose right ->
      Option.equal A.Source.equal left.selected_previous_lock right.selected_previous_lock
      && Captured.Id.equal left.generated_lock right.generated_lock
    | Exact_lock _, Compose _ | Compose _, Exact_lock _ -> false
  ;;

  let equal left right =
    Captured.Id.equal left.root_source right.root_source
    && Context.Selection.equal left.selection right.selection
    && equal_entry left.entry right.entry
    && List.equal Captured.Id.equal left.source_members right.source_members
    && List.equal Route.equal left.routes right.routes
    && equal_mode left.mode right.mode
  ;;

  let create ~root_source ~selection ~entry ~source_members ~routes ~mode =
    let* () = C.validate_source_id root_source in
    if not (bounded_count ~max:1024 source_members && bounded_count ~max:1024 routes)
    then invalid "too many bundle project sources or routes"
    else
      let* () =
        match entry, selection.Context.Selection.target with
        | Declared, Context.Selection.Entry _ | Captured _, Context.Selection.Library ->
          Ok ()
        | Declared, Context.Selection.Library | Captured _, Context.Selection.Entry _ ->
          invalid "bundle entry selection mismatch"
      in
      let* () =
        match mode with
        | Exact_lock id -> C.validate_source_id id
        | Compose { selected_previous_lock; generated_lock } ->
          let* () = C.validate_source_id generated_lock in
          Option.value_map selected_previous_lock ~default:(Ok ()) ~f:immutable_source
      in
      let* source_members =
        List.fold_result
          source_members
          ~init:(Set.empty (module Captured.Id))
          ~f:(fun seen id ->
            let* () = C.validate_source_id id in
            if Set.mem seen id
            then invalid "duplicate bundle project member"
            else Ok (Set.add seen id))
        |> Result.map ~f:Set.to_list
      in
      let* routes =
        List.fold_result
          routes
          ~init:(Map.empty (module Captured.Id))
          ~f:(fun seen route ->
            if Map.mem seen (Route.declaration route)
            then invalid "duplicate bundle project route"
            else Ok (Map.set seen ~key:(Route.declaration route) ~data:route))
        |> Result.map ~f:Map.data
      in
      Ok { root_source; selection; entry; source_members; routes; mode }
  ;;

  let entry_to_json = function
    | Declared -> obj [ "kind", str "declared" ]
    | Captured entry ->
      obj
        [ "kind", str "captured"
        ; "source", A.Source.to_json (Captured_entry.source entry)
        ; ( "delivery"
          , str
              (match Captured_entry.delivery entry with
               | Uploaded_member -> "uploaded_member"
               | Derived_script -> "derived_script") )
        ]
  ;;

  let entry_of_json json =
    let* generic = fields json [ "kind"; "source"; "delivery" ] in
    let* kind = required generic "kind" (C.string ~max_bytes:16) in
    match kind with
    | "declared" ->
      let* _ = fields json [ "kind" ] in
      Ok Declared
    | "captured" ->
      let* source = required generic "source" A.Source.of_json in
      let* delivery = required generic "delivery" (C.string ~max_bytes:16) in
      let* delivery =
        match delivery with
        | "uploaded_member" -> Ok Captured_entry.Uploaded_member
        | "derived_script" -> Ok Derived_script
        | _ -> invalid "invalid bundle entry delivery"
      in
      let* entry = Captured_entry.create ~source ~delivery in
      Ok (Captured entry)
    | _ -> invalid "invalid bundle entry kind"
  ;;

  let mode_to_json = function
    | Exact_lock id ->
      obj [ "kind", str "exact_lock"; "selected_lock", C.source_id_json id ]
    | Compose { selected_previous_lock; generated_lock } ->
      obj
        ([ "kind", str "compose"; "generated_lock", C.source_id_json generated_lock ]
         @ C.optional_field
             "selected_previous_lock"
             selected_previous_lock
             A.Source.to_json)
  ;;

  let mode_of_json json =
    let* generic =
      fields json [ "kind"; "selected_lock"; "generated_lock"; "selected_previous_lock" ]
    in
    let* kind = required generic "kind" (C.string ~max_bytes:16) in
    match kind with
    | "exact_lock" ->
      let* _ = fields json [ "kind"; "selected_lock" ] in
      let* id = required generic "selected_lock" C.source_id in
      Ok (Exact_lock id)
    | "compose" ->
      let* _ = fields json [ "kind"; "generated_lock"; "selected_previous_lock" ] in
      let* generated_lock = required generic "generated_lock" C.source_id in
      let* selected_previous_lock =
        optional generic "selected_previous_lock" A.Source.of_json
      in
      Ok (Compose { selected_previous_lock; generated_lock })
    | _ -> invalid "invalid bundle project mode"
  ;;

  let to_json t =
    obj
      [ "root_source", C.source_id_json t.root_source
      ; "selection", Context.Selection.to_json t.selection
      ; "entry", entry_to_json t.entry
      ; "source_members", C.array C.source_id_json t.source_members
      ; "routes", C.array Route.to_json t.routes
      ; "mode", mode_to_json t.mode
      ]
  ;;

  let of_json json =
    let* value =
      fields
        json
        [ "root_source"; "selection"; "entry"; "source_members"; "routes"; "mode" ]
    in
    let* root_source = required value "root_source" C.source_id in
    let* selection = required value "selection" Context.Selection.of_json in
    let* entry = required value "entry" entry_of_json in
    let* source_members =
      required value "source_members" (C.list ~max_items:1024 C.source_id)
    in
    let* routes = required value "routes" (C.list ~max_items:1024 Route.of_json) in
    let* mode = required value "mode" mode_of_json in
    create ~root_source ~selection ~entry ~source_members ~routes ~mode
  ;;
end
