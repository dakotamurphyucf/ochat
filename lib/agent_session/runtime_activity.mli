(** Execution owned by a runtime switch and by its current caller. Cancelling
    either owner interrupts the activity; callers wait for cancellation cleanup
    before returning. This is resource lifetime, not invocation authority. *)
type t

(** New work cannot enter a runtime whose switch has finished. *)
exception Closed

(** The host keeps [sw] alive through the runtime's lifetime and joins it before
    releasing inherited resources. Does not start work or create a new domain. *)
val create : sw:Eio.Switch.t -> t

(** Run under the runtime switch, inheriting the invoking fiber's bindings for
    native borrows/moderation/coordination. Results and original exceptions are
    returned to the caller. A cancelled caller cancels and joins its own activity
    without cancelling sibling activities. Closing the runtime switch cancels
    every activity, even when their callers belong to other switches. The
    callback must join its own work and not close the runtime switch itself. *)
val run : t -> (unit -> 'a) -> 'a

(** Supply a switch for the activity's own child fibers/resources. Foreground
    workers must use this switch instead of their caller's outer operation switch
    so runtime revocation also cancels and joins provider/helper fibers. *)
val with_switch : t -> (sw:Eio.Switch.t -> 'a) -> 'a
