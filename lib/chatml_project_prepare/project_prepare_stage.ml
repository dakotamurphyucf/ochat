open Core
module Attempt = Project_prepare_attempt
module Accounting = Project_prepare_accounting
module Error = Project_prepare_error
module Build = Chatml_project_build
module Roots = Chatml_local_roots
module Git = Chatml_git_host
module Catalog = Chatml_git_catalog
module Reader = Catalog.Reader

let pure attempt ~f =
  let result =
    let open Result.Let_syntax in
    let%bind limits = Attempt.composition_limits attempt in
    let%bind reservation =
      Attempt.reserve
        attempt
        { Accounting.zero with work_steps = Build.Limits.max_work_steps limits }
    in
    let%bind result, receipt =
      f ~limits ~checkpoint:(fun () -> Attempt.poll attempt)
      |> Result.map_error ~f:(fun failure ->
        Error.create ?declared_at:(Build.Error.declared_at failure) (Composition failure))
    in
    let%map () =
      Attempt.finish
        attempt
        reservation
        ~actual:{ Accounting.zero with work_steps = Build.Accounting.work_steps receipt }
    in
    result
  in
  match result with
  | Ok _ -> result
  | Error error -> Attempt.reject attempt error
;;

let with_roots attempt ~anchors ~f =
  let result =
    let open Result.Let_syntax in
    let%bind limits = Attempt.roots_limits attempt in
    let%bind reservation =
      Attempt.reserve
        attempt
        { Accounting.zero with work_steps = Roots.Limits.work_steps limits }
    in
    let%bind result, receipt =
      Roots.with_anchors
        anchors
        ~limits
        ~checkpoint:(fun () -> Attempt.poll attempt)
        ~f:(fun roots ->
          let%map result = f roots in
          result, Roots.accounting roots)
      |> Result.map_error ~f:(function
        | `Roots failure -> Error.create (Roots failure)
        | `Callback failure -> failure)
    in
    let%map () =
      Attempt.finish
        attempt
        reservation
        ~actual:{ Accounting.zero with work_steps = Roots.Accounting.work_steps receipt }
    in
    result
  in
  match result with
  | Ok _ -> result
  | Error error -> Attempt.reject attempt error
;;

let with_reader attempt repository ~f =
  let result =
    let open Result.Let_syntax in
    let%bind limits = Attempt.git_object_limits attempt in
    let%bind reservation =
      Attempt.reserve
        attempt
        { Accounting.zero with
          source_count = Catalog.Limits.max_sources limits
        ; source_bytes = Catalog.Limits.max_total_source_bytes limits
        ; git_objects = Catalog.Limits.max_objects limits
        ; git_object_bytes = Catalog.Limits.max_total_object_bytes limits
        ; git_tree_entries = Catalog.Limits.max_tree_entries limits
        ; work_steps = Catalog.Limits.max_work_steps limits
        }
    in
    let%bind result, receipt =
      Reader.with_repository
        repository
        ~limits
        ~checkpoint:(fun () -> Attempt.poll attempt)
        ~f
      |> Result.map_error ~f:(function
        | `Reader failure ->
          Error.create
            ?declared_at:(Catalog.Error.declared_at failure)
            (Git_catalog failure)
        | `Callback failure -> failure)
    in
    let%map () =
      Attempt.finish
        attempt
        reservation
        ~actual:
          { Accounting.zero with
            source_count = Catalog.Accounting.source_count receipt
          ; source_bytes = Catalog.Accounting.source_bytes receipt
          ; git_objects = Catalog.Accounting.object_count receipt
          ; git_object_bytes = Catalog.Accounting.object_bytes receipt
          ; git_tree_entries = Catalog.Accounting.tree_entries receipt
          ; work_steps = Catalog.Accounting.work_steps receipt
          }
    in
    result
  in
  match result with
  | Ok _ -> result
  | Error error -> Attempt.reject attempt error
;;

let with_git attempt ~(host : Project_prepare_host.t) ~repository ~pin ~f =
  let result =
    Attempt.poll attempt;
    match host.git_store, Map.find host.git_grants repository with
    | None, _ | Some _, None -> Error (Error.create Authority_denied)
    | Some store, Some grant ->
      let open Result.Let_syntax in
      let%bind limits = Attempt.git_native_limits attempt in
      let%bind request =
        Git.Request.prepare store ~grant ~pin ~limits
        |> Result.map_error ~f:(fun failure -> Error.create (Git_host failure))
      in
      let%bind reservation =
        Attempt.reserve
          attempt
          { Accounting.zero with
            native_request_bytes = Git.Limits.maximum_request_bytes limits
          ; native_output_bytes = Git.Limits.maximum_output_bytes limits
          ; acquisition_payload_reserved = Git.Request.payload_bytes_reserved request
          ; work_steps = Git.Limits.maximum_work_steps limits
          }
      in
      let%bind result, receipt =
        Git.with_acquisition
          request
          ~checkpoint:(fun () -> Attempt.poll attempt)
          ~f:(fun repository -> with_reader attempt repository ~f)
        |> Result.map_error ~f:(function
          | `Host failure -> Error.create (Git_host failure)
          | `Callback failure -> failure)
      in
      let%map () =
        Attempt.finish
          attempt
          reservation
          ~actual:
            { Accounting.zero with
              native_request_bytes = Git.Receipt.request_bytes receipt
            ; native_output_bytes = Git.Receipt.output_bytes receipt
            ; acquisition_payload_reserved = Git.Receipt.payload_bytes_reserved receipt
            ; work_steps = Git.Receipt.work_steps receipt
            }
      in
      result
  in
  match result with
  | Ok _ -> result
  | Error error -> Attempt.reject attempt error
;;
