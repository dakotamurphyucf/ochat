open Core

(** Test-owned monotonic clock. Only the host fiber advances time; real compiler
    domains read the atomic timestamp. Timer fibers await explicit transitions. *)
module Clock : sig
  type t

  val create : unit -> t
  val advance : t -> seconds:float -> unit
  val resource : t -> Eio.Time.Mono.ty Eio.Resource.t
end

(** Borrow the real environment/domain provider; observe admission and completion
    without replacing compiler execution. No resource is acquired or retained
    beyond the test. [on_domain_start] runs on the host before native admission. *)
val environment
  :  Clock.t
  -> Eio_unix.Stdenv.base
  -> on_domain_start:(unit -> unit)
  -> completed:int Atomic.t
  -> Eio_unix.Stdenv.base
