open Core
module Error = Git_host_profile.Error

(** One mutable ledger per exclusively owned bracket. No filesystem/process
    resources or retained caller callback. Arithmetic is checked before work. *)
module Limits : sig
  type t

  val create
    :  metadata_file_bytes:int
    -> pack_file_bytes:int
    -> max_acquisition_payload_bytes:int
    -> max_request_bytes:int
    -> max_output_bytes:int
    -> max_work_steps:int
    -> wall_time:Time_ns.Span.t
    -> cpu_seconds_per_process:int
    -> memory_bytes_per_process:int
    -> open_files_per_process:int
    -> t Or_error.t

  val metadata_file_bytes : t -> int
  val pack_file_bytes : t -> int
  val acquisition_payload_bytes : t -> int
  val maximum_request_bytes : t -> int
  val wall_time : t -> Time_ns.Span.t
end

type t

val create : Limits.t -> clock:_ Eio.Time.Mono.t -> t
val poison : t -> unit
val is_poisoned : t -> bool
val poll : t -> checkpoint:(unit -> unit) -> (unit, Error.t) Result.t
val reserve_work : t -> int -> (unit, Error.t) Result.t

val process_limits
  :  t
  -> file_size_bytes:int
  -> maximum_output_bytes:int
  -> (Shell_access.Bounded_process.Limits.t, Error.t) Result.t

val charge_request
  :  t
  -> executable:string
  -> arguments:string list
  -> environment:(string * string) list
  -> stdin:string
  -> (unit, Error.t) Result.t

val charge_output : t -> stdout:string -> stderr_bytes:int -> (unit, Error.t) Result.t
val output_bytes : t -> int
val work_steps : t -> int

(** Conservative fresh-stage peak including init, known fetch family and one
    full cache-copy overlap. No multiplication/addition overflow, no OS quota. *)
val acquisition_reservation : Limits.t -> (int, Error.t) Result.t

val acquisition_work_reservation : Limits.t -> (int, Error.t) Result.t
