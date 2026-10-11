open Core
module Project = Chatml_project
module Captured = Chatml_source
module Path = Source.Portable_path
module Catalog = Chatml_catalog
module Native = Chatml_local_source
module Budget = Chatml_local_capture_budget
module Error = Chatml_local_capture_error

let poll checkpoint () =
  Eio.Fiber.check ();
  checkpoint ()
;;

let budget context result = Result.map_error result ~f:(Error.of_budget context)

let catalog_error failure =
  Error.create
    (Catalog.Error.declared_at failure)
    ?related:(Catalog.Error.related failure)
    (Catalog failure)
;;

let filesystem context ?member failure =
  Error.create context (Filesystem { failure; member })
;;

let components path = 1 + String.count (Path.to_string path) ~f:(Char.equal '/')

let check_components context limits count =
  if count > Budget.Limits.path_components limits
  then
    Error
      (Error.create
         context
         (Limit_exceeded
            { limit = Path_components; maximum = Budget.Limits.path_components limits }))
  else Ok ()
;;

let directory_key directory =
  match Project.Source_dir.directory directory with
  | Root -> "."
  | Relative path -> Path.to_string path
;;

(* The decoded byte size bounds the local metadata visit allowance. Ordering
   separately charges all lookup-set construction and override sorting passes. *)
let declaration_work project counters context ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    budget
      context
      (Budget.reserve counters (Captured.byte_length (Project.source project)))
  in
  let%bind override_count, key_bytes =
    List.fold_result
      (Project.module_overrides project)
      ~init:(0, 1024)
      ~f:(fun (count, maximum) value ->
        checkpoint ();
        let%map count = budget context (Budget.sum count 1) in
        count, Int.max maximum (String.length (Project.Module_override.name value)))
  in
  let%bind lookup_count =
    budget context (Budget.sum override_count (List.length (Project.exclusions project)))
  in
  let%bind lookup_count =
    budget context (Budget.sum lookup_count (List.length (Project.source_dirs project)))
  in
  let%map () =
    budget
      context
      (Budget.reserve_ordering counters ~count:lookup_count ~key_bytes ~passes:6)
  in
  lookup_count, key_bytes
;;

let discover project owner discovery counters limits context ~lookup_count ~checkpoint =
  let open Result.Let_syntax in
  let directories = Project.source_dirs project in
  let%bind () =
    budget
      context
      (Budget.reserve_ordering
         counters
         ~count:(List.length directories)
         ~key_bytes:1024
         ~passes:2)
  in
  let directories =
    List.sort directories ~compare:(fun left right ->
      checkpoint ();
      String.compare (directory_key left) (directory_key right))
  in
  List.fold_result directories ~init:[] ~f:(fun observations directory ->
    let declared_at = Project.Source_dir.declared_at directory in
    let path, depth =
      match Project.Source_dir.directory directory with
      | Root -> None, 0
      | Relative path -> Some path, components path
    in
    let%bind () = check_components declared_at limits depth in
    let%bind () =
      budget
        declared_at
        (Budget.reserve_read counters ~byte_allowance:0 ~components:depth)
    in
    match
      Native.fold_directory
        owner
        ~directory:path
        ~max_entries:(Budget.remaining_entries counters)
        ~init:observations
        ~checkpoint
        ~f:(fun observations ~name ->
          let%bind () =
            budget declared_at (Budget.reserve_observation counters ~lookup_count)
          in
          Catalog.Plan.Discovery.observe discovery ~directory ~name
          |> Result.map_error ~f:catalog_error
          |> Result.map ~f:(function
            | None -> observations
            | Some observation -> observation :: observations))
    with
    | Error (`Callback failure) -> Error failure
    | Error (`Filesystem failure) ->
      if Native.Error.equal_code (Native.Error.code failure) Entry_limit
      then
        Error
          (Error.of_budget
             declared_at
             (Exceeded
                { limit = Directory_entries
                ; maximum = Budget.Limits.directory_entries limits
                }))
      else Error (filesystem declared_at ?member:path failure)
    | Ok (observations, count) ->
      let%map () = budget declared_at (Budget.add_entries counters count) in
      observations)
;;

let optional_missing failure =
  Native.Error.equal_operation (Native.Error.operation failure) Open_file
  && Native.Error.equal_code (Native.Error.code failure) Missing
;;

let read_member owner counters limits namespace member ~checkpoint =
  let open Result.Let_syntax in
  let path = Catalog.Plan.Member.path member in
  let context = Catalog.Plan.Member.declared_at member in
  let optional =
    Catalog.Plan.Member.equal_requirement
      (Catalog.Plan.Member.requirement member)
      Optional
  in
  let%bind () = check_components context limits (components path) in
  let%bind () =
    if optional then Ok () else budget context (Budget.check_source_slot counters)
  in
  (* If no source slot remains, an optional missing leaf is still admissible.
     A zero-byte probe cannot retain nonempty bytes and a present empty source
     fails the source-count check before constructing a capture. *)
  let allowance =
    if Budget.source_available counters then Budget.read_allowance counters else 0
  in
  let%bind () =
    budget
      context
      (Budget.reserve_read
         counters
         ~byte_allowance:allowance
         ~components:(components path))
  in
  match Native.read owner ~member:path ~max_bytes:allowance ~checkpoint with
  | Error failure when optional && optional_missing failure -> Ok None
  | Error failure ->
    if Native.Error.equal_code (Native.Error.code failure) Too_large
    then (
      let limit, maximum =
        if not (Budget.source_available counters)
        then Error.Source_count, Budget.Limits.sources limits
        else if allowance = Budget.Limits.source_bytes limits
        then Source_bytes, Budget.Limits.source_bytes limits
        else Total_source_bytes, Budget.Limits.total_source_bytes limits
      in
      Error (Error.create context (Limit_exceeded { limit; maximum })))
    else Error (filesystem context ~member:path failure)
  | Ok contents ->
    let bytes = String.length contents in
    let%bind () = budget context (Budget.add_source counters ~bytes) in
    let%map () = budget context (Budget.reserve_scan counters ~bytes) in
    (* Both identity parts were admitted: namespace comes from the supplied
       capture, and path is a Portable_path. No digest constraint/byte ceiling
       remains after the explicit acquisition checks above. *)
    let id =
      Captured.Id.create ~package_namespace:namespace ~relative_path:(Path.to_string path)
      |> Or_error.ok_exn
    in
    Some
      (Captured.create id ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
       |> Or_error.ok_exn)
;;

let acquire_members project owner plan counters limits ~checkpoint =
  let declaration = Project.source project in
  let namespace = Captured.Id.package_namespace (Captured.id declaration) in
  let declaration_path = Captured.Id.relative_path (Captured.id declaration) in
  List.fold_result
    (Catalog.Plan.members plan)
    ~init:(String.Map.singleton declaration_path declaration)
    ~f:(fun captured member ->
      checkpoint ();
      let path = Path.to_string (Catalog.Plan.Member.path member) in
      if Map.mem captured path
      then Ok captured
      else
        read_member owner counters limits namespace member ~checkpoint
        |> Result.map ~f:(function
          | None -> captured
          | Some source -> Map.set captured ~key:path ~data:source))
;;

let capture project ~root ~request ~limits ~checkpoint =
  let open Result.Let_syntax in
  let checkpoint = poll checkpoint in
  checkpoint ();
  let declaration = Project.source project in
  let counters = Budget.create limits in
  (* The decoded declaration already owns its validated whole-source location. *)
  let origin = Project.declared_at project in
  let%bind () =
    budget origin (Budget.add_source counters ~bytes:(Captured.byte_length declaration))
  in
  let%bind () =
    budget origin (Budget.reserve_scan counters ~bytes:(Captured.byte_length declaration))
  in
  let%bind lookup_count, key_bytes =
    declaration_work project counters origin ~checkpoint
  in
  let%bind discovery =
    Catalog.Plan.Discovery.create project ~request ~checkpoint
    |> Result.map_error ~f:catalog_error
  in
  let%bind () = budget origin (Budget.reserve counters 6) in
  match
    Native.with_root
      root
      ~max_path_components:(Budget.Limits.path_components limits)
      ~checkpoint
      ~f:(fun owner ->
        let%bind observations =
          discover
            project
            owner
            discovery
            counters
            limits
            origin
            ~lookup_count
            ~checkpoint
        in
        let%bind count =
          budget
            origin
            (Budget.sum
               (List.length observations)
               (List.length (Project.module_overrides project)))
        in
        let%bind count = budget origin (Budget.sum count 1) in
        let%bind () =
          budget origin (Budget.reserve_ordering counters ~count ~key_bytes ~passes:16)
        in
        let%bind plan =
          Catalog.Plan.create discovery ~observations ~checkpoint
          |> Result.map_error ~f:catalog_error
        in
        let%bind captured =
          acquire_members project owner plan counters limits ~checkpoint
        in
        let%bind capture_count =
          budget
            origin
            (Budget.sum (Map.length captured) (List.length (Catalog.Plan.members plan)))
        in
        let%bind capture_count = budget origin (Budget.sum capture_count count) in
        let key_bytes =
          Int.max
            key_bytes
            (String.length (Captured.Id.package_namespace (Captured.id declaration)))
        in
        let%bind () =
          budget
            origin
            (Budget.reserve_ordering counters ~count:capture_count ~key_bytes ~passes:16)
        in
        let%bind () =
          budget
            origin
            (Budget.reserve_scan counters ~bytes:(Captured.byte_length declaration))
        in
        let sources =
          Map.fold captured ~init:[] ~f:(fun ~key:_ ~data sources ->
            checkpoint ();
            data :: sources)
        in
        let%map catalog =
          Catalog.create plan ~sources ~checkpoint |> Result.map_error ~f:catalog_error
        in
        Chatml_local_capture_catalog.create catalog ~accounting:(Budget.snapshot counters))
  with
  | Ok result -> Ok result
  | Error (`Callback failure) -> Error failure
  | Error (`Filesystem failure) -> Error (filesystem origin failure)
;;
