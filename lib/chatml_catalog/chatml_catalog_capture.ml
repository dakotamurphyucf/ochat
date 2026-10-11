open Core
module Project = Chatml_project
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Path = Source.Portable_path
module Plan = Chatml_catalog_plan
module Error = Chatml_catalog_error

module Binding = struct
  type t =
    { name : string
    ; module_id : Prepared.Module_id.t
    ; declared_at : Project.Declared_at.t
    }

  let name t = t.name
  let module_id t = t.module_id
  let declared_at t = t.declared_at
end

module Accounting = struct
  type t =
    { source_count : int
    ; source_bytes : int
    }

  let source_count t = t.source_count
  let source_bytes t = t.source_bytes
end

type t =
  { plan : Plan.t
  ; sources : Captured.t list
  ; by_path : Captured.t String.Map.t
  ; entry : Captured.Id.t option
  ; definitions : Prepared.Definition.t list
  ; bindings : Binding.t list
  ; accounting : Accounting.t
  }

let project t = Plan.project t.plan
let selection t = Plan.selection t.plan
let sources t = t.sources
let entry t = t.entry
let definitions t = t.definitions
let bindings t = t.bindings
let accounting t = t.accounting

let namespace plan =
  Captured.Id.package_namespace (Captured.id (Project.source (Plan.project plan)))
;;

let find_source t id =
  if String.equal (namespace t.plan) (Captured.Id.package_namespace id)
  then Map.find t.by_path (Captured.Id.relative_path id)
  else None
;;

let scope_for t ~source =
  match find_source t source with
  | None -> None
  | Some _ ->
    let path = Captured.Id.relative_path source in
    List.find (Plan.members t.plan) ~f:(fun member ->
      String.equal (Path.to_string (Plan.Member.path member)) path)
    |> Option.map ~f:(fun member ->
      Project.Selection.scope_for (selection t) ~source:(Plan.Member.path member))
;;

let expected plan ~checkpoint =
  let declaration = Project.source (Plan.project plan) in
  let initial =
    String.Map.singleton
      (Captured.Id.relative_path (Captured.id declaration))
      (Plan.origin plan)
  in
  List.fold (Plan.members plan) ~init:initial ~f:(fun map member ->
    checkpoint ();
    Map.set
      map
      ~key:(Path.to_string (Plan.Member.path member))
      ~data:(Plan.Member.declared_at member))
;;

let equal_revision left right ~checkpoint =
  Captured.Digest.equal (Captured.digest left) (Captured.digest right)
  && Captured.byte_length left = Captured.byte_length right
  && Plan.compare_string (Captured.contents left) (Captured.contents right) ~checkpoint
     = 0
;;

let validate_inventory plan supplied ~checkpoint =
  let open Result.Let_syntax in
  let expected = expected plan ~checkpoint in
  let effective_namespace = namespace plan in
  let%bind by_path =
    List.fold_result supplied ~init:String.Map.empty ~f:(fun map source ->
      checkpoint ();
      let id = Captured.id source in
      let path = Captured.Id.relative_path id in
      let context = Option.value (Map.find expected path) ~default:(Plan.origin plan) in
      if
        Plan.compare_string
          effective_namespace
          (Captured.Id.package_namespace id)
          ~checkpoint
        <> 0
      then Error (Error.create (Plan.origin plan) (Wrong_namespace id))
      else if not (Map.mem expected path)
      then Error (Error.create (Plan.origin plan) (Unexpected_capture id))
      else (
        match Map.find map path with
        | None -> Ok (Map.set map ~key:path ~data:source)
        | Some previous ->
          let problem =
            if equal_revision previous source ~checkpoint
            then Error.Duplicate_capture id
            else Conflicting_capture id
          in
          Error (Error.create context problem)))
  in
  let declaration = Project.source (Plan.project plan) in
  let declaration_path = Captured.Id.relative_path (Captured.id declaration) in
  let%bind () =
    match Map.find by_path declaration_path with
    | None ->
      Error
        (Error.create
           (Plan.origin plan)
           (Missing_capture (Path.create declaration_path |> Or_error.ok_exn)))
    | Some observed ->
      if equal_revision declaration observed ~checkpoint
      then Ok ()
      else
        Error
          (Error.create
             (Plan.origin plan)
             (Conflicting_capture (Captured.id declaration)))
  in
  let%map () =
    List.fold_result (Plan.members plan) ~init:() ~f:(fun () member ->
      checkpoint ();
      if Map.mem by_path (Path.to_string (Plan.Member.path member))
      then Ok ()
      else (
        match Plan.Member.requirement member with
        | Optional -> Ok ()
        | Required ->
          Error
            (Error.create
               (Plan.Member.declared_at member)
               (Missing_capture (Plan.Member.path member)))))
  in
  by_path
;;

let validate_paths plan by_path ~checkpoint =
  let paths =
    Map.fold by_path ~init:[] ~f:(fun ~key ~data:_ paths ->
      checkpoint ();
      (Path.create key |> Or_error.ok_exn) :: paths)
  in
  match Path.first_collision paths ~checkpoint with
  | None -> Ok ()
  | Some (first, second) ->
    let context path =
      List.find (Plan.members plan) ~f:(fun member ->
        Path.equal path (Plan.Member.path member))
      |> Option.value_map ~default:(Plan.origin plan) ~f:Plan.Member.declared_at
    in
    Error
      (Error.create
         (context first)
         ~related:(context second)
         (Portable_collision (first, second)))
;;

let build_associations plan by_path ~checkpoint =
  let open Result.Let_syntax in
  let source path = Map.find_exn by_path (Path.to_string path) in
  let interface = function
    | Plan.Implementation.Absent -> None
    | Optional path -> Option.map (Map.find by_path (Path.to_string path)) ~f:Captured.id
    | Required path -> Some (Captured.id (source path))
  in
  let equal_interface left right =
    Option.equal
      String.equal
      (Option.map left ~f:Captured.Id.relative_path)
      (Option.map right ~f:Captured.Id.relative_path)
  in
  let%map definitions, bindings =
    List.fold_result
      (Plan.implementations plan)
      ~init:(String.Map.empty, [])
      ~f:(fun (definitions, bindings) implementation ->
        checkpoint ();
        let path = Plan.Implementation.source implementation in
        let id = Captured.id (source path) in
        let interface = interface (Plan.Implementation.interface implementation) in
        let declared_at = Plan.Implementation.declared_at implementation in
        let%bind definitions =
          match Map.find definitions (Path.to_string path) with
          | Some (previous, previous_context) ->
            if equal_interface (Prepared.Definition.interface previous) interface
            then Ok definitions
            else (
              let member id =
                Path.create (Captured.Id.relative_path id) |> Or_error.ok_exn
              in
              Error
                (Error.create
                   previous_context
                   ~related:declared_at
                   (Conflicting_interfaces
                      { implementation = path
                      ; first =
                          Option.map (Prepared.Definition.interface previous) ~f:member
                      ; second = Option.map interface ~f:member
                      })))
          | None ->
            Prepared.Definition.create
              ~implementation:id
              ~interface
              ~declared_at:(Project.Declared_at.location declared_at)
            |> Result.map_error ~f:(fun _ ->
              Error.create declared_at (Invalid_association path))
            |> Result.map ~f:(fun definition ->
              Map.set
                definitions
                ~key:(Path.to_string path)
                ~data:(definition, declared_at))
        in
        let binding =
          { Binding.name = Plan.Implementation.name implementation
          ; module_id = Prepared.Module_id.of_implementation id
          ; declared_at
          }
        in
        Ok (definitions, binding :: bindings))
  in
  let definitions =
    Map.fold definitions ~init:[] ~f:(fun ~key:_ ~data:(definition, _) definitions ->
      checkpoint ();
      definition :: definitions)
    |> List.rev
  in
  definitions, List.rev bindings
;;

let measure plan by_path ~checkpoint =
  Map.fold
    by_path
    ~init:(Ok { Accounting.source_count = 0; source_bytes = 0 })
    ~f:(fun ~key:_ ~data:source pending ->
      let open Result.Let_syntax in
      let%bind accounting = pending in
      checkpoint ();
      let bytes = Captured.byte_length source in
      if
        accounting.Accounting.source_count = Int.max_value
        || bytes > Int.max_value - accounting.source_bytes
      then Error (Error.create (Plan.origin plan) Accounting_overflow)
      else
        Ok
          { Accounting.source_count = accounting.source_count + 1
          ; source_bytes = accounting.source_bytes + bytes
          })
;;

let create plan ~sources:supplied ~checkpoint =
  let open Result.Let_syntax in
  let%bind by_path = validate_inventory plan supplied ~checkpoint in
  let%bind () = validate_paths plan by_path ~checkpoint in
  let%bind definitions, bindings = build_associations plan by_path ~checkpoint in
  let%map accounting = measure plan by_path ~checkpoint in
  let sources =
    Map.fold by_path ~init:[] ~f:(fun ~key:_ ~data sources ->
      checkpoint ();
      data :: sources)
    |> List.rev
  in
  let entry =
    Option.map
      (Project.Selection.entry (Plan.selection plan))
      ~f:(fun entry ->
        Captured.id (Map.find_exn by_path (Path.to_string (Project.Entry.source entry))))
  in
  { plan; sources; by_path; entry; definitions; bindings; accounting }
;;
