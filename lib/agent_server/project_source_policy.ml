open Core
module Configuration = Project_source_configuration
module Sources = Agent_session.Prompt_script_preparation
module Git = Chatml_git_host
module Project = Chatml_project_prepare

let kib = 1_024
let mib = kib * kib

let document =
  Document_schema.Limits.create
    ~max_bytes:262_144
    ~max_depth:128
    ~max_fields:100_000
    ~max_nodes:1_000_000
  |> Result.map_error ~f:(fun _ -> "Invalid fixed source document policy")
  |> Result.ok_or_failwith
;;

type t =
  { attempt_wall_time : Time_ns.Span.t
  ; max_semantic_work_steps : int
  ; max_native_work_steps : int
  ; max_acquisition_work_steps : int
  ; custody : Chatml_prompt_programs.Limits.t
  }

let create
      ~attempt_wall_time
      ~max_semantic_work_steps
      ~max_native_work_steps
      ~max_acquisition_work_steps
  =
  let open Or_error.Let_syntax in
  let seconds = Time_ns.Span.to_sec attempt_wall_time in
  if
    (not (Float.is_finite seconds))
    || Float.(seconds <= 0.)
    || max_semantic_work_steps <= 0
    || max_native_work_steps <= 0
    || max_acquisition_work_steps <= 0
  then Or_error.error_string "Invalid project source profile"
  else (
    let%map custody =
      Chatml_prompt_programs.Limits.create
        ~max_programs:512
        ~max_parser_inputs:512
        ~max_logical_sources:256
        ~max_total_input_bytes:(8 * mib)
        ~project_work_steps:max_semantic_work_steps
        ~document
    in
    { attempt_wall_time
    ; max_semantic_work_steps
    ; max_native_work_steps
    ; max_acquisition_work_steps
    ; custody
    })
;;

let default =
  create
    ~attempt_wall_time:(Time_ns.Span.of_sec 120.)
    ~max_semantic_work_steps:10_000_000
    ~max_native_work_steps:40_000_000
    ~max_acquisition_work_steps:160_000_000
  |> Or_error.ok_exn
;;

let attempt_wall_time t = t.attempt_wall_time
let custody_limits t = t.custody

module Configured = struct
  type t =
    { source_preparation : Sources.Policy.t
    ; git_probe_limits : Shell_access.Bounded_process.Limits.t option
    ; git_store_limits : Git.Store_limits.t
    }

  let source_preparation t = t.source_preparation
  let git_probe_limits t = t.git_probe_limits
  let git_store_limits t = t.git_store_limits
end

let configure t configuration =
  let open Or_error.Let_syntax in
  let%bind () = Configuration.validate configuration in
  let memory, enabled =
    match configuration.git with
    | None -> 1, false
    | Some git -> git.memory_bytes_per_process, true
  in
  let%bind native =
    Git.Limits.create
      ~metadata_file_bytes:(if enabled then 16 * kib else 1)
      ~pack_file_bytes:(if enabled then mib else 0)
      ~max_acquisition_payload_bytes:(if enabled then 16 * mib else 0)
      ~max_request_bytes:(4 * mib)
      ~max_output_bytes:(if enabled then 32 * mib else 0)
      ~max_work_steps:t.max_native_work_steps
      ~wall_time:(Time_ns.Span.of_sec 60.)
      ~cpu_seconds_per_process:30
      ~memory_bytes_per_process:memory
      ~open_files_per_process:128
  in
  let%bind git_probe_limits =
    if not enabled
    then Ok None
    else (
      let%map limits =
        Shell_access.Bounded_process.Limits.create
          ~maximum_request_bytes:65_536
          ~maximum_output_bytes:65_536
          ~maximum_wall_time:(Time_ns.Span.of_sec 10.)
          ~cpu_seconds_per_process:30
          ~memory_bytes_per_process:memory
          ~file_size_bytes:0
          ~open_files_per_process:128
      in
      Some limits)
  in
  let%bind roots =
    Chatml_local_roots.Limits.create
      ~max_locator_bytes:4_096
      ~max_locator_components:64
      ~max_anchors:33
      ~max_retained_roots:128
      ~max_work_steps:100_000
  in
  let%bind local =
    Chatml_local_capture.Limits.create
      ~max_directory_entries:4_096
      ~max_sources:256
      ~max_source_bytes:(256 * kib)
      ~max_total_source_bytes:(8 * mib)
      ~max_path_components:64
      ~max_work_steps:t.max_semantic_work_steps
  in
  let%bind objects =
    Chatml_git_catalog.Limits.create
      ~max_object_bytes:mib
      ~max_total_object_bytes:(16 * mib)
      ~max_objects:1_024
      ~max_tree_entries:8_192
      ~max_sources:256
      ~max_source_bytes:(256 * kib)
      ~max_total_source_bytes:(8 * mib)
      ~max_path_components:64
      ~max_work_steps:t.max_semantic_work_steps
      ~document
  in
  let%bind composition =
    Chatml_project_build.Limits.create
      ~max_work_steps:t.max_semantic_work_steps
      ~max_input_bytes:(Some (8 * mib))
      ~document
  in
  let policy =
    Project.Policy.create
      ~roots
      ~local
      ~git_objects:objects
      ~git_native:native
      ~composition
  in
  let%bind limits =
    Project.Limits.create
      ~max_packages:128
      ~max_dependency_visits:4_096
      ~max_source_count:256
      ~max_source_bytes:(8 * mib)
      ~max_local_directory_entries:4_096
      ~max_git_objects:1_024
      ~max_git_object_bytes:(16 * mib)
      ~max_git_tree_entries:8_192
      ~max_native_request_bytes:(4 * mib)
      ~max_native_output_bytes:(32 * mib)
      ~max_acquisition_payload_reserved:(128 * mib)
      ~max_work_steps:t.max_acquisition_work_steps
      ~max_prepared_input_bytes:(8 * mib)
      ~wall_time:t.attempt_wall_time
  in
  let%bind sources =
    Sources.Source_policy.create
      ~root_limits:roots
      ~max_grants:Configuration.maximum_git_grants
      ~max_grant_bytes:Configuration.maximum_authority_bytes
      ~max_request_bytes:65_536
      ~max_path_components:64
      ~max_input_bytes:(8 * mib)
      ~max_work_steps:t.max_semantic_work_steps
      ~policy
      ~limits
      ~custody_limits:t.custody
  in
  let%bind planning =
    Chat_response.Script_contract_plan.Limits.create
      ~max_programs:512
      ~max_metadata_bytes:262_144
      ~max_work_steps:t.max_semantic_work_steps
  in
  let%bind source_preparation =
    Sources.Policy.create
      ~compilation:
        { wall_seconds = Time_ns.Span.to_sec t.attempt_wall_time
        ; max_source_bytes = 256 * kib
        }
      ~planning
      ~sources
      ~max_context_roles:512
      ~max_reference_visits:4_096
      ~max_work_steps:t.max_semantic_work_steps
  in
  let%map git_store_limits =
    Git.Store_limits.create
      ~max_cache_repositories:8
      ~max_cache_payload_bytes:(64 * mib)
      ~max_managed_entries:1_024
      ~max_path_bytes:4_096
      ~max_work_steps:100_000
  in
  { Configured.source_preparation; git_probe_limits; git_store_limits }
;;
