open Core

(** Internal stage deltas are validated by the attempt before admission.
    Published receipts are immutable cumulative nonnegative counts. *)
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
  }

let zero =
  { packages = 0
  ; dependency_visits = 0
  ; source_count = 0
  ; source_bytes = 0
  ; local_directory_entries = 0
  ; git_objects = 0
  ; git_object_bytes = 0
  ; git_tree_entries = 0
  ; native_request_bytes = 0
  ; native_output_bytes = 0
  ; acquisition_payload_reserved = 0
  ; work_steps = 0
  ; prepared_input_bytes = 0
  }
;;

let packages t = t.packages
let dependency_visits t = t.dependency_visits
let source_count t = t.source_count
let source_bytes t = t.source_bytes
let local_directory_entries t = t.local_directory_entries
let git_objects t = t.git_objects
let git_object_bytes t = t.git_object_bytes
let git_tree_entries t = t.git_tree_entries
let native_request_bytes t = t.native_request_bytes
let native_output_bytes t = t.native_output_bytes
let acquisition_payload_reserved t = t.acquisition_payload_reserved
let work_steps t = t.work_steps
let prepared_input_bytes t = t.prepared_input_bytes

let add first second =
  if
    first.packages < 0
    || second.packages < 0
    || second.packages > Int.max_value - first.packages
    || first.dependency_visits < 0
    || second.dependency_visits < 0
    || second.dependency_visits > Int.max_value - first.dependency_visits
    || first.source_count < 0
    || second.source_count < 0
    || second.source_count > Int.max_value - first.source_count
    || first.source_bytes < 0
    || second.source_bytes < 0
    || second.source_bytes > Int.max_value - first.source_bytes
    || first.local_directory_entries < 0
    || second.local_directory_entries < 0
    || second.local_directory_entries > Int.max_value - first.local_directory_entries
    || first.git_objects < 0
    || second.git_objects < 0
    || second.git_objects > Int.max_value - first.git_objects
    || first.git_object_bytes < 0
    || second.git_object_bytes < 0
    || second.git_object_bytes > Int.max_value - first.git_object_bytes
    || first.git_tree_entries < 0
    || second.git_tree_entries < 0
    || second.git_tree_entries > Int.max_value - first.git_tree_entries
    || first.native_request_bytes < 0
    || second.native_request_bytes < 0
    || second.native_request_bytes > Int.max_value - first.native_request_bytes
    || first.native_output_bytes < 0
    || second.native_output_bytes < 0
    || second.native_output_bytes > Int.max_value - first.native_output_bytes
    || first.acquisition_payload_reserved < 0
    || second.acquisition_payload_reserved < 0
    || second.acquisition_payload_reserved
       > Int.max_value - first.acquisition_payload_reserved
    || first.prepared_input_bytes < 0
    || second.prepared_input_bytes < 0
    || second.prepared_input_bytes > Int.max_value - first.prepared_input_bytes
    || first.work_steps < 0
    || second.work_steps < 0
    || second.work_steps > Int.max_value - first.work_steps
  then Or_error.error_string "negative or overflowing project accounting"
  else
    Ok
      { packages = first.packages + second.packages
      ; dependency_visits = first.dependency_visits + second.dependency_visits
      ; source_count = first.source_count + second.source_count
      ; source_bytes = first.source_bytes + second.source_bytes
      ; local_directory_entries =
          first.local_directory_entries + second.local_directory_entries
      ; git_objects = first.git_objects + second.git_objects
      ; git_object_bytes = first.git_object_bytes + second.git_object_bytes
      ; git_tree_entries = first.git_tree_entries + second.git_tree_entries
      ; native_request_bytes = first.native_request_bytes + second.native_request_bytes
      ; native_output_bytes = first.native_output_bytes + second.native_output_bytes
      ; acquisition_payload_reserved =
          first.acquisition_payload_reserved + second.acquisition_payload_reserved
      ; work_steps = first.work_steps + second.work_steps
      ; prepared_input_bytes = first.prepared_input_bytes + second.prepared_input_bytes
      }
;;

let within t ~ceiling =
  t.packages <= ceiling.packages
  && t.dependency_visits <= ceiling.dependency_visits
  && t.source_count <= ceiling.source_count
  && t.source_bytes <= ceiling.source_bytes
  && t.local_directory_entries <= ceiling.local_directory_entries
  && t.git_objects <= ceiling.git_objects
  && t.git_object_bytes <= ceiling.git_object_bytes
  && t.git_tree_entries <= ceiling.git_tree_entries
  && t.native_request_bytes <= ceiling.native_request_bytes
  && t.native_output_bytes <= ceiling.native_output_bytes
  && t.acquisition_payload_reserved <= ceiling.acquisition_payload_reserved
  && t.work_steps <= ceiling.work_steps
  && t.prepared_input_bytes <= ceiling.prepared_input_bytes
;;

let subtract first second =
  if (not (within zero ~ceiling:second)) || not (within second ~ceiling:first)
  then Or_error.error_string "invalid project accounting subtraction"
  else
    Ok
      { packages = first.packages - second.packages
      ; dependency_visits = first.dependency_visits - second.dependency_visits
      ; source_count = first.source_count - second.source_count
      ; source_bytes = first.source_bytes - second.source_bytes
      ; local_directory_entries =
          first.local_directory_entries - second.local_directory_entries
      ; git_objects = first.git_objects - second.git_objects
      ; git_object_bytes = first.git_object_bytes - second.git_object_bytes
      ; git_tree_entries = first.git_tree_entries - second.git_tree_entries
      ; native_request_bytes = first.native_request_bytes - second.native_request_bytes
      ; native_output_bytes = first.native_output_bytes - second.native_output_bytes
      ; acquisition_payload_reserved =
          first.acquisition_payload_reserved - second.acquisition_payload_reserved
      ; work_steps = first.work_steps - second.work_steps
      ; prepared_input_bytes = first.prepared_input_bytes - second.prepared_input_bytes
      }
;;
