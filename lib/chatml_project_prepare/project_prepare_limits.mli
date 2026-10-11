open Core

(** Whole-attempt nonnegative counts, positive work/prepared bytes and deadline.
    Each stage also retains its independently configured policy ceiling. *)
type t = private
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

val create
  :  max_packages:int
  -> max_dependency_visits:int
  -> max_source_count:int
  -> max_source_bytes:int
  -> max_local_directory_entries:int
  -> max_git_objects:int
  -> max_git_object_bytes:int
  -> max_git_tree_entries:int
  -> max_native_request_bytes:int
  -> max_native_output_bytes:int
  -> max_acquisition_payload_reserved:int
  -> max_work_steps:int
  -> max_prepared_input_bytes:int
  -> wall_time:Time_ns.Span.t
  -> t Or_error.t
