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
  }

val zero : t
val packages : t -> int
val dependency_visits : t -> int
val source_count : t -> int
val source_bytes : t -> int
val local_directory_entries : t -> int
val git_objects : t -> int
val git_object_bytes : t -> int
val git_tree_entries : t -> int
val native_request_bytes : t -> int
val native_output_bytes : t -> int
val acquisition_payload_reserved : t -> int
val work_steps : t -> int

(** Checked coordinatewise addition; rejects any negative delta or overflow. *)
val add : t -> t -> t Or_error.t

(** Coordinatewise comparison, used only after nonnegative admission. *)
val within : t -> ceiling:t -> bool

(** Checked coordinatewise subtraction; no negative result is representable. *)
val subtract : t -> t -> t Or_error.t
