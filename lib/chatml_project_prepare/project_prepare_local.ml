open Core
module Attempt = Project_prepare_attempt
module Accounting = Project_prepare_accounting
module Error = Project_prepare_error
module Local = Chatml_local_capture
module Native = Chatml_local_source
module Project = Chatml_project
module Captured = Chatml_source
module Path = Source.Portable_path

let charge_work attempt units =
  let open Result.Let_syntax in
  let%bind amount =
    List.fold_result units ~init:Accounting.zero ~f:(fun amount work_steps ->
      Accounting.add amount { Accounting.zero with work_steps }
      |> Result.map_error ~f:(fun _ -> Error.create Accounting_overflow))
  in
  Attempt.charge attempt amount
;;

let bootstrap attempt owner member namespace_override limits document =
  let checkpoint () = Attempt.poll attempt in
  let open Result.Let_syntax in
  let components = 1 + String.count (Path.to_string member) ~f:(Char.equal '/') in
  if
    components > Local.Limits.path_components limits
    || Local.Limits.path_components limits > Native.path_component_limit owner
    || Local.Limits.sources limits = 0
  then Error (Error.create Limit_exceeded)
  else (
    let allowance =
      Int.min
        (Document_schema.Limits.max_bytes document)
        (Int.min
           (Local.Limits.source_bytes limits)
           (Local.Limits.total_source_bytes limits))
    in
    (* Same worst-case one-byte read model as local capture: no assumption that
       a requested 4096-byte native read will return 4096 bytes. *)
    let%bind () = charge_work attempt [ allowance; (2 * components) + 6 ] in
    let%bind contents =
      Native.read owner ~member ~max_bytes:allowance ~checkpoint
      |> Result.map_error ~f:(fun failure -> Error.create (Source failure))
    in
    let bytes = String.length contents in
    let chunks = (bytes / 4096) + if bytes mod 4096 = 0 then 0 else 1 in
    let%bind () = charge_work attempt [ bytes; bytes; chunks + 1; chunks + 1 ] in
    let%bind authored_namespace =
      Project.Namespace.of_contents contents ~limits:document ~checkpoint
      |> Result.map_error ~f:(fun failure -> Error.create (Bootstrap failure))
    in
    let package_namespace = Option.value namespace_override ~default:authored_namespace in
    let%bind id =
      Captured.Id.create ~package_namespace ~relative_path:(Path.to_string member)
      |> Result.map_error ~f:(fun _ -> Error.create Conflicting_selection)
    in
    let%bind source =
      Captured.create
        id
        ~contents
        ~expected_digest:None
        ~max_bytes:(Some (Local.Limits.source_bytes limits))
        ~checkpoint
      |> Result.map_error ~f:(fun _ -> Error.create Conflicting_selection)
    in
    Project.decode source ~limits:document ~checkpoint
    |> Result.map_error ~f:(fun failure -> Error.create (Declaration failure)))
;;

let with_work_allowance limits ~work_steps =
  Local.Limits.create
    ~max_directory_entries:(Local.Limits.directory_entries limits)
    ~max_sources:(Local.Limits.sources limits)
    ~max_source_bytes:(Local.Limits.source_bytes limits)
    ~max_total_source_bytes:(Local.Limits.total_source_bytes limits)
    ~max_path_components:(Local.Limits.path_components limits)
    ~max_work_steps:work_steps
  |> Result.map_error ~f:(fun _ -> Error.create Limit_exceeded)
;;

let capture attempt ~owner ~project ~namespace_override ~selection =
  let result =
    let open Result.Let_syntax in
    let%bind limits = Attempt.local_limits attempt in
    let%bind composition = Attempt.composition_limits attempt in
    let%bind inventory =
      Attempt.reserve
        attempt
        { Accounting.zero with
          source_count = Local.Limits.sources limits
        ; source_bytes = Local.Limits.total_source_bytes limits
        ; local_directory_entries = Local.Limits.directory_entries limits
        }
    in
    let%bind project =
      bootstrap
        attempt
        owner
        project
        namespace_override
        limits
        (Chatml_project_build.Limits.document composition)
    in
    let work_steps =
      Int.min
        (Local.Limits.work_steps limits)
        (Accounting.work_steps (Attempt.remaining attempt))
    in
    let%bind limits = with_work_allowance limits ~work_steps in
    let%bind work = Attempt.reserve attempt { Accounting.zero with work_steps } in
    let%bind result =
      Local.capture_from_owner
        project
        ~owner
        ~request:selection
        ~limits
        ~checkpoint:(fun () -> Attempt.poll attempt)
      |> Result.map_error ~f:(fun failure ->
        Error.create ~declared_at:(Local.Error.declared_at failure) (Local failure))
    in
    let receipt = Local.accounting result in
    let%bind () =
      Attempt.finish
        attempt
        work
        ~actual:{ Accounting.zero with work_steps = Local.Accounting.work_steps receipt }
    in
    let%map () =
      Attempt.finish
        attempt
        inventory
        ~actual:
          { Accounting.zero with
            source_count = Local.Accounting.source_count receipt
          ; source_bytes = Local.Accounting.source_bytes receipt
          ; local_directory_entries = Local.Accounting.directory_entries receipt
          }
    in
    result
  in
  match result with
  | Ok _ -> result
  | Error error -> Attempt.reject attempt error
;;
