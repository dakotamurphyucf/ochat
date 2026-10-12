open Core
module Error = Git_host_profile.Error
module Pin = Chatml_project.Git_pin

(** Retained private store, exclusive-operation lifecycle and immutable inventory.
    No Git commands, object semantics or caller verification callbacks. Resource
    accessors are borrowed only inside [with_operation], which joins before close. *)
module Limits : sig
  type t

  val create
    :  max_cache_repositories:int
    -> max_cache_payload_bytes:int
    -> max_managed_entries:int
    -> max_path_bytes:int
    -> max_work_steps:int
    -> t Or_error.t
end

type t

val open_root
  :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> sw:Eio.Switch.t
  -> deployment:Git_host_profile.Deployment.t
  -> clock:_ Eio.Time.Mono.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val root : t -> Git_host_storage.t
val cache_directory : t -> Git_host_storage.t
val stages_directory : t -> Git_host_storage.t
val native_root : t -> string
val native_workspace : t -> string
val deployment : t -> Git_host_profile.Deployment.t
val clock : t -> Eio.Time.Mono.ty Eio.Time.Mono.t
val generation : t -> int
val maximum_cache_payload_bytes : t -> int
val maximum_managed_entries : t -> int
val contains : t -> key:string -> bool

module Cache : sig
  type t

  val payload_bytes : t -> int
end

val cache : t -> key:string -> Cache.t option

module Request : sig
  type store = t
  type t

  val prepare
    :  store
    -> grant:Git_host_profile.Grant.t
    -> pin:Pin.t
    -> limits:Git_host_attempt.Limits.t
    -> (t, Error.t) Result.t

  val store : t -> store
  val grant : t -> Git_host_profile.Grant.t
  val pin : t -> Pin.t
  val limits : t -> Git_host_attempt.Limits.t
  val generation : t -> int
  val payload_bytes_reserved : t -> int
end

val with_operation
  :  t
  -> generation:int
  -> f:(unit -> ('a, [ `Host of Error.t | `Callback of 'error ]) Result.t)
  -> ('a, [ `Host of Error.t | `Callback of 'error ]) Result.t

(** Before creating the six workspace entries (active, home, xdg, tmp, empty,
    config), admit their overlap against the retained managed inventory. *)
val admit_workspace : t -> (unit, Error.t) Result.t

(** Before staging, check fixed selected-stage entry/payload overlap against the
    currently admitted inventory. No filesystem probe or budget refund. *)
val admit_acquisition : t -> Git_host_attempt.Limits.t -> (unit, Error.t) Result.t

(** After caller verification and final deadline check, atomically publish the
    bounded [copy] stage with create-only rename and update registry/generation.
    All accounting/result allocation precedes commit. Protected publication may
    finish after cancellation becomes pending; no selected lock is written here.
    Post-rename durability uncertainty returns storage failure without pretending
    rollback. Any publication failure invalidates this owner; release/reopen
    reconciles bounded cache inventory before reuse. *)
val publish_copy
  :  t
  -> key:string
  -> Git_host_storage.Inventory.t
  -> (unit, Error.t) Result.t
