open Core

(** Whole-attempt nonnegative counts, positive work/prepared bytes and deadline.
    Each stage also retains its independently configured policy ceiling. *)
type t =
  { packages : int
  ; dependency_visits : int
  ; source_count : int
  ; source_bytes : int
  ; local_directory_entries : int
  ; git_objects : int
  ; git_object_bytes : int
  ; git_tree_entries : int
  ; native_request_bytes : int
  ; native_output_bytes : int
  ; acquisition_payload_reserved : int
  ; work_steps : int
  ; prepared_input_bytes : int
  ; wall_time : Time_ns.Span.t
  }

let create
      ~max_packages
      ~max_dependency_visits
      ~max_source_count
      ~max_source_bytes
      ~max_local_directory_entries
      ~max_git_objects
      ~max_git_object_bytes
      ~max_git_tree_entries
      ~max_native_request_bytes
      ~max_native_output_bytes
      ~max_acquisition_payload_reserved
      ~max_work_steps
      ~max_prepared_input_bytes
      ~wall_time
  =
  if
    List.exists
      [ max_packages
      ; max_dependency_visits
      ; max_source_count
      ; max_source_bytes
      ; max_local_directory_entries
      ; max_git_objects
      ; max_git_object_bytes
      ; max_git_tree_entries
      ; max_native_request_bytes
      ; max_native_output_bytes
      ; max_acquisition_payload_reserved
      ; max_work_steps
      ; max_prepared_input_bytes
      ]
      ~f:(fun count -> count < 0)
    || max_work_steps = 0
    || max_prepared_input_bytes = 0
    || Time_ns.Span.(wall_time <= zero)
  then Or_error.error_string "invalid project preparation limits"
  else
    Ok
      { packages = max_packages
      ; dependency_visits = max_dependency_visits
      ; source_count = max_source_count
      ; source_bytes = max_source_bytes
      ; local_directory_entries = max_local_directory_entries
      ; git_objects = max_git_objects
      ; git_object_bytes = max_git_object_bytes
      ; git_tree_entries = max_git_tree_entries
      ; native_request_bytes = max_native_request_bytes
      ; native_output_bytes = max_native_output_bytes
      ; acquisition_payload_reserved = max_acquisition_payload_reserved
      ; work_steps = max_work_steps
      ; prepared_input_bytes = max_prepared_input_bytes
      ; wall_time
      }
;;
