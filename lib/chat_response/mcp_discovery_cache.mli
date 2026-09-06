(** One discovery cache per connected MCP declaration, never process-global.
    Its loader closes over exactly one authenticated client and tool filter.
    Different declarations, credentials, transports or runtimes cannot share
    entries or invalidation. The owning runtime switch bounds client/listener
    lifetime. Expiry uses the host's Eio clock; failed/cancelled loads are not
    cached, and their exceptions propagate only after the mutex is unlocked so
    subsequent discovery and invalidation remain usable. *)
type 'a t

val create : now:(unit -> float) -> load:(unit -> 'a) -> 'a t
val get : 'a t -> 'a
val invalidate : 'a t -> unit
