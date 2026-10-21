(** Test clock that keeps real monotonic progress and adds explicit elapsed time.
    [now] is safe for compiler-domain readers. Sleeps and advances belong to the
    creating Eio domain; advancing wakes existing sleeps to recheck deadlines.
    There is no pause or reset operation. *)
type t

val create : Eio.Time.Mono.ty Eio.Resource.t -> t
val resource : t -> Eio.Time.Mono.ty Eio.Resource.t

(** Add a nonnegative finite duration, in seconds. Reject invalid or overflowing
    advances; do not change the underlying clock or any configured allowance. *)
val advance : t -> seconds:float -> unit
