open Core
module Pin = Chatml_project.Git_pin

(** Fixed source-audited stock Git command domain. Host declaration inputs are
    syntax-validated here; actual executable/helper and filesystem admission is
    owned by the public host owner before publishing Deployment or Store. *)
module Error : sig
  type code =
    | Unsupported_profile
    | Unsupported_provider
    | Invalid_input
    | Authority_denied
    | Limit_exceeded
    | Stale_request
    | Process_failed
    | Timed_out
    | Storage_failed
    | Cache_corrupt
    | Busy
    | Closed
  [@@deriving equal, sexp_of]

  type t

  val create : code -> t
  val code : t -> code
  val of_storage_error : Git_host_native.Code.t -> t
  val sexp_of_t : t -> Sexp.t
end

module Deployment : sig
  type t

  val inputs
    :  native_helper_directory:string
    -> native_git_executable:string
    -> native_ca_bundle:string option
    -> max_bytes:int
    -> (t, Error.t) Result.t

  val executable : t -> string
  val helper_directory : t -> string
  val ca_bundle : t -> string option
  val helper_names : string list
  val version_arguments : string list
  val admitted_version : string -> bool
end

module Grant : sig
  type t

  val https
    :  repository:string
    -> authorization:string option
    -> max_input_bytes:int
    -> t Or_error.t

  val local : repository:string -> max_input_bytes:int -> t Or_error.t
  val repository : t -> string
  val input_bytes : t -> int
  val authorization : t -> string option
  val is_https : t -> bool
end

module Object_id : sig
  type t [@@deriving equal, compare, sexp_of]

  val create : object_format:Pin.object_format -> string -> t Or_error.t
  val of_pin : Pin.t -> t
  val to_string : t -> string
  val object_format : t -> Pin.object_format
end

(** All commands selected from this finite variant. Native paths are host-selected
    stable private labels. The only variable wire data is validated route, hex ID
    and optional host-owned single authorization header; no authored argv/config. *)
type command =
  | Version
  | Init of Pin.object_format
  | Fetch of Grant.t * Pin.t
  | Raw_object of Object_id.t

val arguments : Deployment.t -> workspace:string -> command -> string list
val stdin : command -> string

(** Selected authorization exists only in HTTPS Fetch's explicit Git config
    environment, never argv or another command's environment. *)
val environment : Deployment.t -> workspace:string -> command -> (string * string) list

(** Canonical cache identity: bounded route bytes before scanning, 4096-byte
    hash chunks with caller-owned admission/deadline/cancellation polls. *)
val cache_key
  :  repository:string
  -> max_repository_bytes:int
  -> poll:(unit -> (unit, Error.t) Result.t)
  -> Pin.t
  -> (string, Error.t) Result.t

(** Validate the acquired fixed layout and metadata before any offline query.
    Receives exact admitted small config/HEAD bytes; no regex/config import. *)
val validate_metadata
  :  object_format:Pin.object_format
  -> config:string
  -> head:string
  -> bool

val permitted_directory : string list -> bool
val permitted_file : object_format:Pin.object_format -> string list -> bool
